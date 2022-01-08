module Main where

import Data.Ron.Deserialize (loads)
import Data.Ron.Serialize (dumpsCompact)
import Data.Ron.Value.Internal (similar)
import Data.Text.Lazy (toStrict)
import Test.Tasty.QuickCheck (testProperty)

import Test.Tasty

main = defaultMain $ testGroup "properties" [loads_dumps]

loads_dumps = testProperty "loads . dumps = id" $
    \value -> case loads . toStrict . dumpsCompact $ value of
        Left err -> False
        Right value' -> value `similar` value'
