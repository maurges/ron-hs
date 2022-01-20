module Data.Ron.Serialize
    ( dumpsCompact
    ) where

import Data.ByteString.Builder (Builder, toLazyByteString, byteString, integerDec, doubleDec, char7)
import Data.ByteString.Builder.Prim (primBounded, condB, (>$<), (>*<), liftFixedToBounded, word16HexFixed)
import Data.Char (ord)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, encodeUtf8BuilderEscaped)
import Data.Word (Word8)

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Builder.Prim as Prim
import qualified Data.Vector as Vector

import Data.Ron.Value


-- | Dumps a RON 'Value' as a compact string with minimum whitespace, but a lot of commas
--
-- TODO: less commas. And also we need a non-compact dumper
dumpsCompact :: Value -> Lazy.ByteString
dumpsCompact = toLazyByteString . go where
    toListElem x = go x <> char7 ','
    toMapElem (k, v) = go k <> char7 ':' <> go v <> char7 ','
    toRecordElem (k, v) = fromText k <> char7 ':' <> go v <> char7 ','
    --
    go (Integral x) = integerDec x
    go (Floating x) = doubleDec x
    go (Char x) = char7 '\'' <> encodeChar x <> char7 '\''
    go (String x) = char7 '"' <> encodeString x <> char7 '"'
    go (List xs)
        = char7 '['
       <> mconcat (map toListElem (Vector.toList xs))
       <> char7 ']'
    go (Map xs)
        = char7 '{'
       <> mconcat (map toMapElem (Map.toList xs))
       <> char7 '}'
    go (Unit constructor)
        | constructor == ""  = byteString "()"
        | otherwise          = fromText constructor
    go (Tuple mbName xs)
        = fromText mbName
       <> char7 '('
       <> mconcat (map toListElem (Vector.toList xs))
       <> char7 ')'
    go (Record mbName xs)
        = fromText mbName
       <> char7 '('
       <> mconcat (map toRecordElem (Map.toList xs))
       <> char7 ')'

fromText :: Text -> Builder
fromText = byteString . encodeUtf8

encodeString :: Text -> Builder
encodeString = encodeUtf8BuilderEscaped escape
    where
        escape
            = condB (== c2w '\\' ) (ascii2 ('\\','\\'))
            . condB (== c2w '\b' ) (ascii2 ('\\','b' ))
            . condB (== c2w '\f' ) (ascii2 ('\\','f' ))
            . condB (== c2w '\n' ) (ascii2 ('\\','n' ))
            . condB (== c2w '\r' ) (ascii2 ('\\','r' ))
            . condB (== c2w '\t' ) (ascii2 ('\\','t' ))
            . condB (== c2w '\"' ) (ascii2 ('\\','\"' ))
            . condB (>= c2w ' '  ) (liftFixedToBounded Prim.word8)
            $ liftFixedToBounded hexEscape -- fallback for chars < 0x20
        hexEscape = (\c -> ('\\', ('u', fromIntegral c))) >$<
            Prim.char8 >*< Prim.char8 >*< word16HexFixed
        ascii2 cs = liftFixedToBounded $ const cs >$< Prim.char7 >*< Prim.char7
        c2w = fromIntegral . ord :: Char -> Word8

encodeChar :: Char -> Builder
encodeChar = primBounded escape
    where
        escape
            = condB (== '\\' ) (unicode2 ('\\','\\'))
            . condB (== '\b' ) (unicode2 ('\\','b' ))
            . condB (== '\f' ) (unicode2 ('\\','f' ))
            . condB (== '\n' ) (unicode2 ('\\','n' ))
            . condB (== '\r' ) (unicode2 ('\\','r' ))
            . condB (== '\t' ) (unicode2 ('\\','t' ))
            . condB (== '\'' ) (unicode2 ('\\','\'' ))
            . condB (>= ' '  ) Prim.charUtf8
            $ liftFixedToBounded hexEscape -- fallback for chars < 0x20
        hexEscape = (\c -> ('\\', ('u', fromIntegral . ord $ c))) >$<
            Prim.char8 >*< Prim.char8 >*< word16HexFixed
        unicode2 cs = const cs >$< Prim.charUtf8 >*< Prim.charUtf8
