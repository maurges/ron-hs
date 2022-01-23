{-# LANGUAGE RecordWildCards #-}
module Data.Ron.Serialize
    ( CommaStyle (..)
    , SerializeSettins (..)
    , haskellStyle, rustStyle, compactStyle
    , dumps
    ) where

import Data.ByteString.Builder (Builder, toLazyByteString, byteString, integerDec, doubleDec, char7, string7)
import Data.ByteString.Builder.Prim (primBounded, condB, (>$<), (>*<), liftFixedToBounded, word16HexFixed)
import Data.Char (ord)
import Data.List (intersperse)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, encodeUtf8BuilderEscaped)
import Data.Word (Word8)

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Builder.Prim as Prim
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Data.Ron.Value


data CommaStyle
    = CommaHistoric
    -- ^ Like in json, comma after value without trailing
    | CommaTrailing
    -- ^ Comma after value, including last element
    | CommaLeading
    -- ^ Haskell style, comma at line start
    deriving (Eq, Show)

data SerializeSettins = SerializeSettins
    { commaStyle :: !CommaStyle
    , indent :: !Int
    , singleElementSpecial :: !Bool
    -- ^ When a compound type only contains one element, this compound value is
    -- printed on one line
    , unpackToplevel :: !Bool
    -- ^ Toplevel struct and list are unpacked to not include the constructor
    -- and brackets
    , openBracketOnSameLine :: !Bool
    -- ^ For compound types, does the bracket go on a new line or stays on the
    -- same line as the constructor
    , closeBracketOnSameLine :: !Bool
    -- ^ For compound types, does the bracket go on a new line or stays on the
    -- same line as the last element. Some people in haskell like this one as
    -- True, and I think lispers do as well
    } deriving (Eq, Show)

haskellStyle, rustStyle, compactStyle :: SerializeSettins
haskellStyle = SerializeSettins
    { commaStyle = CommaLeading
    , indent = 4
    , singleElementSpecial = True
    , unpackToplevel = True
    , openBracketOnSameLine = False
    , closeBracketOnSameLine = False
    }
rustStyle = SerializeSettins
    { commaStyle = CommaTrailing
    , indent = 4
    , singleElementSpecial = True
    , unpackToplevel = False
    , openBracketOnSameLine = True
    , closeBracketOnSameLine = False
    }
compactStyle = SerializeSettins
    { commaStyle = CommaHistoric
    , indent = 0
    , singleElementSpecial = True
    , unpackToplevel = False
    , openBracketOnSameLine = True
    , closeBracketOnSameLine = True
    }

dumps :: SerializeSettins -> Value -> Lazy.ByteString
dumps SerializeSettins {..} = toLazyByteString . toplevel where
    deeper !lvl = lvl + indent
    nl  = if indent == 0 then mempty else char7 '\n'
    shift lvl = string7 $ replicate lvl ' '
    bracketOpen lvl c
        | indent == 0  = char7 c
        | openBracketOnSameLine  = char7 c <> nl <> shift (deeper lvl)
            <> if commaStyle == CommaLeading
                then char7 ' ' <> char7 ' '
                else mempty
        | commaStyle == CommaLeading  =
            nl <> shift (deeper lvl) <> char7 c <> char7 ' '
        | otherwise  = nl <> shift lvl <> char7 c <> nl <> shift (deeper lvl)
    bracketClose lvl c
        | indent == 0  = char7 c
        | closeBracketOnSameLine  = char7 ' ' <> char7 c
        | commaStyle == CommaLeading  = nl <> shift (deeper lvl) <> char7 c
        | otherwise  = nl <> shift lvl <> char7 c
    comma lvl = if commaStyle == CommaLeading
        then nl <> shift lvl <> string7 ", "
        else char7 ',' <> nl <> shift lvl
    trailingComma = if commaStyle == CommaTrailing
        then char7 ','
        else mempty
    toplevel = if unpackToplevel
        then \case
            -- Those two looks bad with leading comma, not sure what to do.
            -- Don't want to turn them into trailing/historic comma for
            -- toplevel only, since I personally always forget to put it after
            -- the previous element
            List xs -> listContent 0 xs
            Record _name xs -> recordContent 0 xs
            v -> go 0 v
        else go 0
    --
    go !lvl = \case
        Integral x -> integerDec x
        Floating x -> doubleDec x
        Char x -> char7 '\'' <> encodeChar x <> char7 '\''
        String x -> char7 '"' <> encodeString x <> char7 '"'
        Unit name -> if Text.null name
            then string7 "()"
            else fromText name
        List xs
            | null xs -> string7 "[]"
            | singleElementSpecial && isSimple (List xs)
                -> let !x = Vector.unsafeHead xs
                   in string7 "[ " <> go lvl x <> string7 " ]"
            | otherwise -> bracketOpen lvl '[' <> listContent (deeper lvl) xs <> bracketClose lvl ']'
        Map xs
            | null xs -> string7 "{}"
            | singleElementSpecial && isSimple (Map xs)
                -> let (!k, !v) = head . Map.toList $ xs
                   in string7 "{ " <> go lvl k <> string7 ": " <> go lvl v <> string7 " }"
            | otherwise -> bracketOpen lvl '{' <> mapContent (deeper lvl) xs <> bracketClose lvl '}'
        Tuple mbName xs ->
            let
              name = if Text.null mbName then mempty else fromText mbName <> char7 ' '
              content = if singleElementSpecial && isSimple (Tuple mbName xs)
                    then let !x = Vector.unsafeHead xs
                         in string7 "( " <> go lvl x <> string7 " )"
                    else bracketOpen lvl '(' <> listContent (deeper lvl) xs <> bracketClose lvl ')'
            in name <> content
        Record mbName xs ->
            let
              name = if Text.null mbName then mempty else fromText mbName <> char7 ' '
              content = if singleElementSpecial && isSimple (Record mbName xs)
                    then let (!k, !v) = head . Map.toList $ xs
                         in string7 "( " <> fromText k <> string7": " <> go lvl v <> string7 " )"
                    else bracketOpen lvl '(' <> recordContent (deeper lvl) xs <> bracketClose lvl ')'
            in name <> content
    --
    listContent lvl = (<> trailingComma) . mconcat . intersperse (comma lvl) . map (go lvl) . Vector.toList
    --
    mapContent lvl = (<> trailingComma) . mconcat . intersperse (comma lvl) . map toElem . Map.toList where
        toElem (k, v) = go lvl k <> string7 ": " <> go lvl v
    --
    recordContent lvl = (<> trailingComma) . mconcat . intersperse (comma lvl) . map toElem . Map.toList where
        toElem (k, v) = fromText k <> string7 ": " <> go lvl v

-- | Can the value be nicely rendered on a single line? True for non-compounds and single-value compounds
isSimple :: Value -> Bool
isSimple (Integral _) = True
isSimple (Floating _) = True
isSimple (Char _) = True
isSimple (String _) = True
isSimple (Unit _) = True
isSimple (List elems) = case Vector.uncons elems of
    Nothing -> True
    Just (x, xs)
        | null xs   -> isSimple x
        | otherwise -> False
isSimple (Map xs)
    | null xs = True
    | length xs == 1 = case Map.toList xs of
        [(k, v)] -> isSimple k && isSimple v
        _ -> False
    | otherwise = False
isSimple (Tuple _ elems) = case Vector.uncons elems of
    Nothing -> True
    Just (x, xs)
        | null xs   -> isSimple x
        | otherwise -> False
isSimple (Record _ xs)
    | null xs = True
    | length xs == 1 = case Map.toList xs of
        [(_, v)] -> isSimple v
        _ -> False
    | otherwise = False

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
