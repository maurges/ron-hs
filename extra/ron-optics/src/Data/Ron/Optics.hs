module Data.Ron.Optics
    ( _Integral
    , _Floating
    , _Char
    , _String
    , _List
    , _Map
    , _Unit
    , _Tuple
    , _Record
    -- ** Reexports
    , at, ix, (%), (^?), preview
    ) where

import Data.Map.Strict (Map)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Vector (Vector)
import Optics.Core (ix, at, (%), (^?), preview)
import Optics.Prism (Prism', prism)

import Data.Ron.Value

_Integral :: Prism' Value Integer
_Integral = prism constructor matcher where
    constructor = Integral
    matcher = \case
        Integral x -> Right x
        other -> Left other

_Floating :: Prism' Value Scientific
_Floating = prism constructor matcher where
    constructor = Floating
    matcher = \case
        Floating x -> Right x
        other -> Left other

_Char :: Prism' Value Char
_Char = prism constructor matcher where
    constructor = Char
    matcher = \case
        Char x -> Right x
        other -> Left other

_String :: Prism' Value Text
_String = prism constructor matcher where
    constructor = String
    matcher = \case
        String x -> Right x
        other -> Left other

_List :: Prism' Value (Vector Value)
_List = prism constructor matcher where
    constructor = List
    matcher = \case
        List x -> Right x
        other -> Left other

_Map :: Prism' Value (Map Value Value)
_Map = prism constructor matcher where
    constructor = Map
    matcher = \case
        Map x -> Right x
        other -> Left other

_Unit :: Text -> Prism' Value ()
_Unit name = prism constructor matcher where
    constructor () = Unit name
    matcher = \case
        Unit name' | name' == name -> Right ()
        other -> Left other

_UnitName :: Prism' Value Text
_UnitName = prism constructor matcher where
    constructor = Unit
    matcher = \case
        Unit x -> Right x
        other -> Left other

_Tuple :: Text -> Prism' Value (Vector Value)
_Tuple name = prism constructor matcher where
    constructor = Tuple name
    matcher = \case
        Tuple name' vals | name' == name -> Right vals
        other -> Left other

_Record :: Text -> Prism' Value (Map Text Value)
_Record name = prism constructor matcher where
    constructor = Record name
    matcher = \case
        Record name' vals | name' == name -> Right vals
        other -> Left other
