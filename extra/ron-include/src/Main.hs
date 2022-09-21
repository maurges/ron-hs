module Main where

import Control.Applicative (optional, many)
import Data.Foldable (toList)
import Data.Map.Strict ((!))
import Data.Ron ( rustStyle )
import Data.Ron.Deserialize (loadFile, loadsLazy)
import Data.Ron.Serialize (dumps, dumpFile)
import Data.Set (Set, member)
import Data.Text (unpack)
import Options.Applicative (Parser, ParserInfo)
import System.Directory (doesFileExist, getCurrentDirectory)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Ron.Value as Ron
import qualified Data.Set as Set
import qualified Options.Applicative as P

data Options = Options
    { inputName :: !(Maybe FilePath)
    , outputName :: !(Maybe FilePath)
    , searchDirs :: ![FilePath]
    }
    deriving (Eq, Show)

optionsParser :: Parser Options
optionsParser = Options
    <$> inputName
    <*> outputName
    <*> searchDirs
    where
        inputName =
            let opts = P.long "input" <> P.short 'i' <> P.help "Main input file. Omit to read from stdin"
            in optional $ P.strOption opts
        outputName =
            let opts = P.long "output" <> P.short 'o' <> P.help "Output file. Omit to output to stdout"
            in optional $ P.strOption opts
        searchDirs =
            let opts = P.long "search" <> P.short 'I' <> P.help "Include dirs. Will search for relative paths here, in the specified order"
            in many $ P.strOption opts

optionsInfo :: ParserInfo Options
optionsInfo = P.info optionsParser $
    P.progDesc "RON file concatenator. Will replace all occurences of `ron'include(\"PATH\")` with the referenced file. Path can be absolute or relative to search dirs. CWD is searched last"

main :: IO ()
main = do
    Options{ inputName, outputName, searchDirs } <- P.execParser optionsInfo
    currentDir <- getCurrentDirectory

    let searchDirs' = searchDirs <> [currentDir]
    let readInput = case inputName of
            Nothing -> BSL.getContents
            Just fname -> BSL.readFile fname
    let writeOutput = case outputName of
            Nothing -> BSL.putStr . dumps rustStyle
            Just fname -> dumpFile rustStyle fname

    readInput >>= loadImpure >>= process searchDirs' mempty >>= writeOutput

loadImpure :: Applicative f => BSL.ByteString -> f Ron.Value
loadImpure bs = case loadsLazy bs of
    Left err -> error err
    Right x -> pure x

process :: [FilePath] -> Set FilePath -> Ron.Value -> IO Ron.Value
process dirs visited val = do
    refs <- pure . Map.fromList
        =<< traverse readDocument
        =<< traverse (goResolve dirs)
        =<< pure (goCollect val)
    pure $ goSet refs val
    where
        collectList :: Foldable t => t Ron.Value -> [FilePath]
        collectList = concat . map goCollect . toList
        goCollect = \case
            Ron.Tuple name vals
                | name == "ron'include" -> case toList vals of
                    [Ron.String arg] -> [unpack arg]
                    _other -> [] -- ignore incorrect arguments
                | otherwise -> collectList $ vals
            Ron.List vals -> collectList $ vals
            Ron.Map vals -> collectList $ vals
            Ron.Record _ vals -> collectList $ vals
            _other -> []

        goResolve [] name = error $ "Could not resolve in search dirs: " <> name
        goResolve (x:xs) name = do
            let fullName = x <> "/" <> name
            r <- doesFileExist fullName
            if r
            then pure (name, fullName)
            else goResolve xs name

        readDocument (originalName, fullName) =
            if fullName `member` visited
            then error $ "Recursive inclusion of " <> fullName
            else do
                value <- loadFile fullName
                let visited' = Set.insert fullName visited
                r <- process dirs visited' value
                pure (originalName, r)

        goSet refs = \case
            Ron.Tuple name vals
                | name == "ron'include" -> case toList vals of
                    [Ron.String arg] -> refs ! unpack arg
                    _other -> Ron.Tuple name $ fmap (goSet refs) vals -- ignore incorrect arguments
            Ron.Tuple name vals -> Ron.Tuple name $ fmap (goSet refs) vals
            Ron.List vals -> Ron.List $ fmap (goSet refs) vals
            Ron.Map vals -> Ron.Map $ fmap (goSet refs) vals
            Ron.Record name vals -> Ron.Record name $ fmap (goSet refs) vals
            other -> other
