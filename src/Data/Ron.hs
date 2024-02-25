-- | Most common ron definitions are collected in this module
--
-- == Quick start
--
-- 1. Define a datatype which supports Ron serialization:
--
-- @
--      data MyType
--          = Cons1
--              { _mtBool :: !Bool
--              , _mtList :: ![Text]
--              , _mtOtherValue :: !OtherType
--              }
--          | Cons2 Text Int
--          deriving (Generic)
--          deriving ('ToRon', 'FromRon')
--              via 'RonWith' '['FieldsDropPrefix'] MyType
-- @
--
-- For more options, see "Data.Ron.Class".
--
-- You will need to set @DerivingStrategies, DerivingVia, DeriveGeneric,
-- DataKinds@ for this to compile.
--
-- 2. Read a value of this type from a file with 'decodeFile':
--
-- @
--      myValue <- 'decodeFile' "path\/to\/file"
--      useMyValue myValue
-- @
--
-- 3. Write your value to a file with 'encodeFile'
--
-- @
--      'encodeFile' 'haskellStyle' "path\/to\/file" myValue
-- @
--
-- For a type defined above, you can expect the serialized form to look like this:
--
-- @
-- Cons1
-- ( bool: false
-- , list:
--     [ "list"
--     , "of"
--     , "strings"
--     ]
-- , otherValue: OtherType (/* something here */)
-- )
-- @
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
