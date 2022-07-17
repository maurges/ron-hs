module File
    ( fileTests
    ) where

import Control.Exception (bracket)
import Data.Ron.Deserialize (decodeFile, loadFile)
import Data.Ron.Serialize (encodeFile, haskellStyle)
import Data.Ron.Value.Internal (similar')
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase, (@?=), (@?))

withTempFile :: String -> (FilePath -> IO a) -> IO a
withTempFile template callback = do
    tempDir <- getTemporaryDirectory
    path <- bracket
        (openTempFile tempDir template)
        (\(p, h) -> hClose h >> removeFile p)
        (pure . fst)
    bracket
        (pure path)
        removeFile
        callback

writeAndRead = withTempFile "ron" $ \path -> do
    let value = Just ()
    encodeFile haskellStyle path value
    value' <- decodeFile path
    value @?= value'

readExample = do
    val <- loadFile "example.ron"
    val `similar'` val @? "Value is not similar to itself"

fileTests = testGroup "file tests"
    [ testCase "write and read to file" writeAndRead
    , testCase "read example file" readExample
    ]
