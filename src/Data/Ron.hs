-- | Most common ron definitions are collected in this module
module Data.Ron
    ( -- * Serialize and deserialize
      encode, encodeFile
    , decode, decodeLazy, decodeFile
    -- ** Style options
    , haskellStyle, rustStyle, compactStyle
    -- * Ron class and deriving
    , ToRon (..), FromRon (..)
    , RonWith (..)
    -- ** Settings for deriving
    , UseStrict
    , EncodeWith
    , DecodeWith
    , ConvertWith
    , FieldsToSnakeCase
    , FieldsDropPrefix
    , ImplicitSome
    , NoImplicitSome
    , SkipSingleConstructor
    , NoSkipSingleConstructor
    ) where

import Data.Ron.Class
import Data.Ron.Class.Deriving
import Data.Ron.Serialize
import Data.Ron.Deserialize
