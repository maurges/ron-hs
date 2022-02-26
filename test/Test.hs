module Main where

import Data.ByteString.Lazy (toStrict)
import Data.Ron.Deserialize (loads)
import Data.Ron.Serialize (dumps, compactStyle)
import Data.Ron.Value.Internal (similar')
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import ForExample (exampleTests)
import Generic (genericsTests)
import GenericOptions (genericOptionsTests)
import DerivingVia (derivingViaTests)

main = defaultMain $ testGroup "properties"
    [ always_decodes
    , loads_dumps
    , exampleTests
    , genericsTests
    , genericOptionsTests
    , derivingViaTests
    ]

always_decodes = testProperty "isRight . loads . dumps arbStyle $ x" $
    \style value -> case loads . toStrict . dumps style $ value of
        Left _err -> False
        Right _val -> True

loads_dumps = testProperty "loads . dumps compactStyle = id" $
    \value -> case loads . toStrict . dumps compactStyle $ value of
        Left err -> False
        Right value' -> value `similar'` value'
