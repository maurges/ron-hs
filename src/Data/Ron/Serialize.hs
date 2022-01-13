module Data.Ron.Serialize
    ( dumpsCompact
    ) where

import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Builder.Prim (condB, (>$<), (>*<), liftFixedToBounded, char7, word8, char8, word16HexFixed)
import Data.Char (ord)
import Data.Text.Encoding (encodeUtf8BuilderEscaped)
import Data.Text.Lazy.Builder (fromText, fromLazyText, toLazyText, singleton, fromString, Builder)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Word (Word8)

import qualified Data.Map as Map
import qualified Data.Text as Strict
import qualified Data.Text.Lazy as Lazy
import qualified Data.Vector as Vector

import Data.Ron.Value


-- | Dumps a RON 'Value' as a compact string with minimum whitespace, but a lot of commas
--
-- TODO: less commas. And also we need a non-compact dumper
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
    go (Char x) = singleton '\'' <> encodeString (Strict.singleton x) <> singleton '\''
    go (String x) = singleton '"' <> encodeString x <> singleton '"'
    go (List xs)
        = singleton '['
       <> mconcat (map toListElem (Vector.toList xs))
       <> fromString "]"
    go (Map xs)
        = singleton '{'
       <> mconcat (map toMapElem (Map.toList xs))
       <> fromString "}"
    go (Unit constructor)
        | constructor == ""  = fromText "()"
        | otherwise          = fromText constructor
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

encodeString :: Strict.Text -> Builder
encodeString = fromLazyText . decodeUtf8 . toLazyByteString . encodeUtf8BuilderEscaped escape
    where
        escape
            = condB (== c2w '\\' ) (ascii2 ('\\','\\'))
            $ condB (== c2w '\"' ) (ascii2 ('\\','"' ))
            $ condB (== c2w '\b' ) (ascii2 ('\\','b' ))
            $ condB (== c2w '\f' ) (ascii2 ('\\','f' ))
            $ condB (== c2w '\n' ) (ascii2 ('\\','n' ))
            $ condB (== c2w '\r' ) (ascii2 ('\\','r' ))
            $ condB (== c2w '\t' ) (ascii2 ('\\','t' ))
            $ condB (>= c2w ' '  ) (liftFixedToBounded word8)
            $ liftFixedToBounded hexEscape -- fallback for chars < 0x20
        hexEscape = (\c -> ('\\', ('u', fromIntegral c))) >$<
            char8 >*< char8 >*< word16HexFixed
        c2w :: Char -> Word8
        c2w = fromIntegral . ord
        ascii2 cs = liftFixedToBounded $ const cs >$< char7 >*< char7

