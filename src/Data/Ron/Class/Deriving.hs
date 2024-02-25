{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE UndecidableInstances, FlexibleContexts #-} -- type family in instance head
{-# LANGUAGE PolyKinds #-}

-- | Allows deriving ron encoding-decoding instances with @DerivingVia@. See 'RonWith'
module Data.Ron.Class.Deriving
    ( RonWith (..)
    -- * Settings
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
    -- * Undelying typeclasses
    , ReflectSettingsOptions (..)
    , ReflectFlagOptions (..)
    ) where

import Data.Char (isUpper, toLower, isLower)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic, Rep)

import Data.Ron.Class


-- | Helper for deriving Ron instances. Use like this:
--
-- @
--      data MyType = MyType {...}
--          deriving (Eq, Show, Generic)
--          deriving (ToRon, FromRon)
--              via RonWith '[FieldsDropPrefix, EncodeWith SkipSingleConstructor] MyType
-- @
--
-- This is identical to
--
-- @
--      data MyType = MyType {...}
--          deriving (Eq, Show, Generic)
--      instance ToRon MyType where
--          toRon =
--              let settings
--                  = (\s{encodeFlags} -> s{encodeFlags = encodeFlags{skipSingleConstructor = True}})
--                  . (\s{fieldModifier} -> s{dropPrefix . fieldModifier})
--                  $ laxRonSettings
--              in toRonGeneric settings
-- @
--
-- The options are applied left to right, and the starting options are
-- 'laxRonSettings'
--
-- All of built-in settings are documented in 'Data.Ron.Class.Deriving' module.
-- You can define your own settings by creating new datatypes and creating
-- instances for 'ReflectSettingsOptions' or 'ReflectFlagOptions'
newtype RonWith s a = RonWith a

instance (Generic a, GToRon (Rep a), ReflectSettingsList s)
    => ToRon (RonWith s a) where
    toRon (RonWith x) = flip toRonGeneric x $ reflectL (Proxy @s) laxRonSettings

instance (Generic a, GFromRon (Rep a), ReflectSettingsList s)
    => FromRon (RonWith s a) where
    fromRon = fmap RonWith . fromRonGeneric (reflectL (Proxy @s) laxRonSettings)

class ReflectSettingsList a where
    reflectL :: Proxy a -> RonSettings -> RonSettings

instance ReflectSettingsList '[] where
    reflectL Proxy s = s
    {-# INLINE reflectL #-}
instance (ReflectSettingsOptions s, ReflectSettingsList ss)
    => ReflectSettingsList (s:ss) where
    reflectL Proxy
        = reflectL (Proxy @ss)
        . reflectS (Proxy @s)
    {-# INLINE reflectL #-}

-- | Replace the current settings with 'strictRonSettings'
data UseStrict
-- | Sets a flag in @encodeFlags@. Can set anything with 'ReflectFlagOptions'
-- instances, like 'ImplicitSome', 'SkipSingleConstructor'
data EncodeWith a
-- | Sets a flag in @decodeFlags@. Can set anything with 'ReflectFlagOptions'
-- instances
data DecodeWith a
-- | Same as setting both 'EncodeWith' and 'DecodeWith'
data ConvertWith a
-- | Convert fields to snake case. If your field also has prefix, you want to
-- have this option after 'FieldsDropPrefix', as options are applied left to
-- right
data FieldsToSnakeCase
-- | Drop the prefix that is usually used with lenses, that is an underscore
-- followed by several lowercase letters; regex for this prefix is
-- @s\/^_[[:lowercase:]]+([[:uppercase:]].*)\/\\1\/@. You probably want to use
-- this field modifier before other field modifiers, as they are applied left
-- to right
data FieldsDropPrefix

-- | Typeclass for you to implement your own options. Here's how
-- 'FieldsToSnakeCase' uses it:
--
-- @
--      instance ReflectSettingsOptions FieldsToSnakeCase where
--          reflectS Proxy s@RonSettings {fieldModifier} = s
--              { fieldModifier = toSnake . fieldModifier }
-- @
--
-- Be careful when composing functions to apply them after the already present,
-- to preserve the left-to-right semantics of adding the options
class ReflectSettingsOptions a where
    reflectS :: Proxy a -> RonSettings -> RonSettings

-- | Encode or decode flag that sets 'Data.Ron.Class.implicitSome' to @True@
data ImplicitSome
-- | Encode or decode flag that sets 'Data.Ron.Class.implicitSome' to @False@
data NoImplicitSome
-- | Encode or decode flag that sets 'Data.Ron.Class.skipSingleConstructor' to @True@
data SkipSingleConstructor
-- | Encode or decode flag that sets 'Data.Ron.Class.skipSingleConstructor' to @False@
data NoSkipSingleConstructor

-- | Typeclass for you to implement your own symmetric options. Here's how
-- 'ImplicitSome' uses it
--
-- @
--      instance ReflectFlagOptions ImplicitSome where
--          reflectF Proxy flags = flags { implicitSome = True }
-- @
class ReflectFlagOptions a where
    reflectF :: Proxy a -> RonFlags -> RonFlags

instance ReflectSettingsOptions UseStrict where
    reflectS Proxy _ = strictRonSettings
    {-# INLINE reflectS #-}

instance ReflectFlagOptions a => ReflectSettingsOptions (EncodeWith a) where
    reflectS Proxy s@RonSettings {encodeFlags} = s
        { encodeFlags = reflectF (Proxy @a) encodeFlags }
    {-# INLINE reflectS #-}
instance ReflectFlagOptions a => ReflectSettingsOptions (DecodeWith a) where
    reflectS Proxy s@RonSettings {decodeFlags} = s
        { decodeFlags = reflectF (Proxy @a) decodeFlags }
    {-# INLINE reflectS #-}
instance ReflectFlagOptions a => ReflectSettingsOptions (ConvertWith a) where
    reflectS Proxy s@RonSettings {encodeFlags, decodeFlags} = s
        { encodeFlags = reflectF (Proxy @a) encodeFlags
        , decodeFlags = reflectF (Proxy @a) decodeFlags
        }
    {-# INLINE reflectS #-}

instance ReflectSettingsOptions FieldsDropPrefix where
    reflectS Proxy conf@RonSettings {fieldModifier} = conf
        { fieldModifier = dropPrefix . fieldModifier }
        where
            dropPrefix ('_':s) =
                case dropWhile (not . isUpper) s of
                    "" -> '_':s
                    c:s' -> toLower c : s'
            dropPrefix s = s
    {-# INLINE reflectS #-}

instance ReflectSettingsOptions FieldsToSnakeCase where
    reflectS Proxy s@RonSettings {fieldModifier} = s
        { fieldModifier = toSnake . fieldModifier }
        where
         toSnake = camelTo2 '_'
    {-# INLINE reflectS #-}

-- | Better version of camelTo. Example where it works better:
--
--   > camelTo '_' "CamelAPICase" == "camel_apicase"
--   > camelTo2 '_' "CamelAPICase" == "camel_api_case"
--
-- Extracted from aeson at Feb 2022
camelTo2 :: Char -> String -> String
camelTo2 c = map toLower . go2 . go1
    where
        go1 "" = ""
        go1 (x:u:l:xs) | isUpper u && isLower l = x : c : u : l : go1 xs
        go1 (x:xs) = x : go1 xs
        go2 "" = ""
        go2 (l:u:xs) | isLower l && isUpper u = l : c : u : go2 xs
        go2 (x:xs) = x : go2 xs

instance ReflectFlagOptions ImplicitSome where
    reflectF Proxy f =
        f { implicitSome = True }
    {-# INLINE reflectF #-}

instance ReflectFlagOptions NoImplicitSome where
    reflectF Proxy f =
        f { implicitSome = False }
    {-# INLINE reflectF #-}

instance ReflectFlagOptions SkipSingleConstructor where
    reflectF Proxy f =
        f { skipSingleConstructor = True }
    {-# INLINE reflectF #-}

instance ReflectFlagOptions NoSkipSingleConstructor where
    reflectF Proxy f =
        f { skipSingleConstructor = False }
    {-# INLINE reflectF #-}
