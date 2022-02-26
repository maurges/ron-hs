{-# LANGUAGE DeriveGeneric, OverloadedLists #-}
module Generic
    ( genericsTests
    ) where

import GHC.Generics (Generic)
import Data.Map.Strict (Map)
import Data.Ron.Class (ParseResult, FromRon (fromRon), ToRon (toRon))
import Data.Ron.Value (Value)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word8)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty (testGroup)

-- Adapted from ron-rs
data Config = Config
    { float :: (Float, Float)
    , tuple :: TupleStruct
    , map' :: Map Word8 Char
    , nested :: Nested
    , var :: Variant
    , array :: Vector ()
    } deriving (Eq, Show, Generic)

data TupleStruct = TupleStruct () Bool
    deriving (Eq, Show, Generic)

data Variant = A Word8 | B String
    deriving (Eq, Show, Generic)

data Nested = Nested
    { a :: Text
    , b :: Char
    } deriving (Eq, Show, Generic)

instance ToRon Nested
instance FromRon Nested
instance ToRon Variant
instance FromRon Variant
instance ToRon TupleStruct
instance FromRon TupleStruct
instance ToRon Config
instance FromRon Config

config = Config
    { float = (2.18, -1.1)
    , tuple = TupleStruct () False
    , map' = [(0, '1'), (1, '2'), (3, '5'), (8, '1')]
    , nested = Nested
        { a = "Hello from \"RON\""
        , b =  'b'
        }
    , var = A 0
    , array = [(), (), ()]
    }

data BigTuple = BigTuple
    Int
    Float
    String
    Text
    Integer
    Nested
    ()
    Variant
    deriving (Eq, Show, Generic)

instance ToRon BigTuple
instance FromRon BigTuple

bigTuple = BigTuple
    1 2 "3" "4" 5 Nested { a = "6", b = '7' } () (B "8")

data BigEnum = C | D | E | F | G
    deriving (Eq, Show, Generic)

instance ToRon BigEnum
instance FromRon BigEnum

genericsTests = testGroup "generic instances"
    [ testCase "big record" $ roundtrip config @?= Right config
    , testCase "big tuple" $ roundtrip bigTuple @?= Right bigTuple
    , testCase "big enum" $ roundtrip C @?= Right C
    ]
    where
        roundtrip :: (ToRon a, FromRon a) => a -> ParseResult a
        roundtrip = fromRon . toRon
