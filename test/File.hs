module File
    ( fileTests
    ) where

import Control.Exception (bracket)
import Data.Ron.Serialize (encodeFile, haskellStyle)
import Data.Ron.Deserialize (decodeFile)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty (testGroup)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)

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

fileTests = testGroup "file tests"
    [ testCase "write and read to file" writeAndRead
    ]
