{-# LANGUAGE DeriveGeneric, DerivingVia, DataKinds #-}
module GenericVia
    (
    ) where

import Data.Ron.Class (ToRon (toRon))
import GHC.Generics (Generic)

import Data.Ron.Class.Deriving

data MaybeFields = MaybeFields
    { f1 :: Int
    , f2 :: Maybe Int
    , f3 :: Maybe (Maybe Int)
    , re :: Maybe MaybeFields
    }
    deriving (Eq, Show, Generic)
    deriving ToRon via RonWith
        '[ EncodeWith ImplicitSome
         , DecodeWith ImplicitSome
         ] MaybeFields
