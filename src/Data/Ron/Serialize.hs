module Data.Ron.Serialize
    where

import Data.Text.Lazy.Builder (fromText, toLazyText, singleton, fromString, Builder)

import qualified Data.Map as Map
import qualified Data.Text.Lazy as Lazy
import qualified Data.Vector as Vector

import Data.Ron.Value


dumpsCompact :: Value -> Lazy.Text
dumpsCompact = toLazyText . go where
    bshow :: Show a => a -> Builder
    bshow = fromString . show
    toListElem x = go x <> singleton ','
    toMapElem (k, v) = go k <> singleton ':' <> go v <> singleton ','
    toRecordElem (k, v) = fromText k <> singleton ':' <> go v <> singleton ','
    --
    go (Integral x) = bshow x
    go (Floating x) = bshow x
    go (String x) = bshow x
    go (Boolean x) = if x then "true" else "false"
    go (List xs)
        = singleton '['
       <> mconcat (map toListElem (Vector.toList xs))
       <> fromString "]"
    go (Map xs)
        = singleton '{'
       <> mconcat (map toMapElem (Map.toList xs))
       <> fromString "}"
    go (Unit constructor) = fromText constructor
    go (Tuple mbName xs)
        = fromText mbName
       <> singleton '('
       <> mconcat (map toListElem (Vector.toList xs))
       <> fromString ")"
    go (Record mbName xs)
        = fromText mbName
       <> singleton '('
       <> mconcat (map toRecordElem (Map.toList xs))
       <> fromString ")"
