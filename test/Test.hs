module Main where

import Data.ByteString.Lazy (toStrict)
import Data.Ron.Deserialize (loads)
import Data.Ron.Serialize (dumps, compactStyle)
import Data.Ron.Value.Internal (similar')
import Test.Tasty.QuickCheck (testProperty)

import ForExample (exampleTests)

import Test.Tasty

main = defaultMain $ testGroup "properties" [always_decodes, loads_dumps, exampleTests]

always_decodes = testProperty "isRight . loads . dumps $ x" $
    \value -> case loads . toStrict . dumps compactStyle $ value of
        Left _err -> False
        Right _val -> True

loads_dumps = testProperty "loads . dumps = id" $
    \value -> case loads . toStrict . dumps compactStyle $ value of
        Left err -> False
        Right value' -> value `similar'` value'
