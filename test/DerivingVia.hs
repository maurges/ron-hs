{-# LANGUAGE DeriveGeneric, DerivingVia, DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
module DerivingVia
    ( derivingViaTests
    ) where

import Data.Ron.Class (ToRon (..), FromRon (..), RonSettings (..))
import Data.Ron.Value (Value (..))
import GHC.Generics (Generic)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty (testGroup)

import Data.Ron.Class.Deriving

data Record1 = Record1
    { _rField :: Int
    , _rOtherField :: Maybe Int
    }
    deriving (Eq, Show, Generic)
    deriving (ToRon, FromRon) via RonWith
        '[ FieldsDropPrefix
         , FieldsToSnakeCase
         , ConvertWith ImplicitSome
         ] Record1

record1 = Record1 1 (Just 2)
record1ron = Record "Record1"
    [ ("field", Integral 1)
    , ("other_field", Integral 2)
    ]

data AppendA
data AppendB
instance ReifySettingsOptions AppendA where
    reifyS _ s@RonSettings {fieldModifier} = s
        { fieldModifier = (<> "aaa") . fieldModifier }
instance ReifySettingsOptions AppendB where
    reifyS _ s@RonSettings {fieldModifier} = s
        { fieldModifier = (<> "bbb") . fieldModifier }

newtype Record2 = Record2
    { field' :: Int
    }
    deriving (Eq, Show, Generic)
    deriving (ToRon, FromRon) via RonWith
        '[ AppendA
         , AppendB
         ] Record2

record2 = Record2 1337
record2ron = Record "Record2" [ ("field'aaabbb", Integral 1337) ]

derivingViaTests = testGroup "deriving via"
    [ testGroup "inbuilt field modifiers are correct"
        [ testCase "encode" $
            toRon record1 @?= record1ron
        , testCase "decode" $
            fromRon record1ron @?= Right record1
        ]
    , testGroup "options apply left to right"
        [ testCase "encode" $
            toRon record2 @?= record2ron
        , testCase "decode" $
            fromRon record2ron @?= Right record2
        ]
    ]
