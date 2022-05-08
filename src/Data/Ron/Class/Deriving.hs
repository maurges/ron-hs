{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE UndecidableInstances, FlexibleContexts #-} -- type family in instance head
{-# LANGUAGE PolyKinds #-}
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
    , ReifySettingsOptions (..)
    , ReifyFlagOptions (..)
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
-- The options are applied left to right, and the starting options are
-- 'laxRonSettings'
newtype RonWith s a = RonWith a

instance (Generic a, GToRon (Rep a), ReifySettingsList s)
    => ToRon (RonWith s a) where
    toRon (RonWith x) = flip toRonGeneric x $ reifyL (Proxy @s) laxRonSettings

instance (Generic a, GFromRon (Rep a), ReifySettingsList s)
    => FromRon (RonWith s a) where
    fromRon = fmap RonWith . fromRonGeneric (reifyL (Proxy @s) laxRonSettings)

class ReifySettingsList a where
    reifyL :: Proxy a -> RonSettings -> RonSettings

instance ReifySettingsList '[] where
    reifyL Proxy s = s
    {-# INLINE reifyL #-}
instance (ReifySettingsOptions s, ReifySettingsList ss)
    => ReifySettingsList (s:ss) where
    reifyL Proxy
        = reifyL (Proxy @ss)
        . reifyS (Proxy @s)
    {-# INLINE reifyL #-}

-- | Replace the current settings with 'strictRonSettings'
data UseStrict
-- | Sets a flag in @encodeFlags@. Can set anything with 'ReifyFlagOptions'
-- instances, like 'ImplicitSome', 'SkipSingleConstructor'
data EncodeWith a
-- | Same as above for @decodeFlags@
data DecodeWith a
-- | Same as setting both options above
data ConvertWith a
-- | Convert fields to snake case. If your field also has prefix, you want to
-- have it after 'FieldsDropPrefix', as options are applied left to right
data FieldsToSnakeCase
-- | Drop the usual lens prefix: underscore followed by several lowercase
-- letters; regex for this is @s\/^_[[:lowercase:]]+([[:uppercase:]].*)\/\\1\/@.
-- You probably want to use this field modifier before other field modifiers,
-- as they are applied left to right
data FieldsDropPrefix

-- | Typeclass for you to implement your own options. Here's how
-- 'FieldsToSnakeCase' uses it:
--
-- @
--      instance ReifySettingsOptions FieldsToSnakeCase where
--          reifyS Proxy s@RonSettings {fieldModifier} = s
--              { fieldModifier = toSnake . fieldModifier }
-- @
--
-- Be careful when composing functions to apply them after the already present,
-- to preserve the left-to-right semantics of adding the options
class ReifySettingsOptions a where
    reifyS :: Proxy a -> RonSettings -> RonSettings

-- | @implicitSome .~ True@
data ImplicitSome
-- | @implicitSome .~ False@
data NoImplicitSome
-- | @skipSingleConstructor .~ True@
data SkipSingleConstructor
-- | @skipSingleConstructor .~ False@
data NoSkipSingleConstructor

-- | Typeclass for you to implement your own symmetric options. Here's how
-- 'ImplicitSome' uses it
--
-- @
--      instance ReifyFlagOptions ImplicitSome where
--          reifyF Proxy flags = flags { implicitSome = True }
-- @
class ReifyFlagOptions a where
    reifyF :: Proxy a -> RonFlags -> RonFlags

instance ReifySettingsOptions UseStrict where
    reifyS Proxy _ = strictRonSettings
    {-# INLINE reifyS #-}

instance ReifyFlagOptions a => ReifySettingsOptions (EncodeWith a) where
    reifyS Proxy s@RonSettings {encodeFlags} = s
        { encodeFlags = reifyF (Proxy @a) encodeFlags }
    {-# INLINE reifyS #-}
instance ReifyFlagOptions a => ReifySettingsOptions (DecodeWith a) where
    reifyS Proxy s@RonSettings {decodeFlags} = s
        { decodeFlags = reifyF (Proxy @a) decodeFlags }
    {-# INLINE reifyS #-}
instance ReifyFlagOptions a => ReifySettingsOptions (ConvertWith a) where
    reifyS Proxy s@RonSettings {encodeFlags, decodeFlags} = s
        { encodeFlags = reifyF (Proxy @a) encodeFlags
        , decodeFlags = reifyF (Proxy @a) decodeFlags
        }
    {-# INLINE reifyS #-}

instance ReifySettingsOptions FieldsDropPrefix where
    reifyS Proxy conf@RonSettings {fieldModifier} = conf
        { fieldModifier = dropPrefix . fieldModifier }
        where
            dropPrefix ('_':s) =
                case dropWhile (not . isUpper) s of
                    "" -> '_':s
                    c:s' -> toLower c : s'
            dropPrefix s = s
    {-# INLINE reifyS #-}

instance ReifySettingsOptions FieldsToSnakeCase where
    reifyS Proxy s@RonSettings {fieldModifier} = s
        { fieldModifier = toSnake . fieldModifier }
        where
         toSnake = camelTo2 '_'
    {-# INLINE reifyS #-}

-- | Better version of 'camelTo'. Example where it works better:
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

instance ReifyFlagOptions ImplicitSome where
    reifyF Proxy f =
        f { implicitSome = True }
    {-# INLINE reifyF #-}

instance ReifyFlagOptions NoImplicitSome where
    reifyF Proxy f =
        f { implicitSome = False }
    {-# INLINE reifyF #-}

instance ReifyFlagOptions SkipSingleConstructor where
    reifyF Proxy f =
        f { skipSingleConstructor = True }
    {-# INLINE reifyF #-}

instance ReifyFlagOptions NoSkipSingleConstructor where
    reifyF Proxy f =
        f { skipSingleConstructor = False }
    {-# INLINE reifyF #-}
