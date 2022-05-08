{-# LANGUAGE RecordWildCards #-}
module Data.Ron.Serialize
    ( CommaStyle (..)
    , SerializeSettings (..)
    , haskellStyle, rustStyle, compactStyle
    , encode, encodeFile
    , dumps, dumpFile
    , ronBuilder
    ) where

import Data.ByteString.Builder (Builder, toLazyByteString, byteString, integerDec, char7, string7, hPutBuilder)
import Data.ByteString.Builder.Prim (primBounded, condB, (>$<), (>*<), liftFixedToBounded, word16HexFixed)
import Data.ByteString.Builder.Scientific (scientificBuilder)
import Data.Char (ord)
import Data.List (intersperse)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, encodeUtf8BuilderEscaped)
import Data.Ron.Class (ToRon (toRon))
import Data.Word (Word8)
import Test.QuickCheck (Arbitrary, arbitrary, chooseEnum)
import System.IO (IOMode (WriteMode), hSetBinaryMode, hSetBuffering, BufferMode (BlockBuffering), withFile)

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
    deriving (Eq, Show, Bounded, Enum)

data SerializeSettings = SerializeSettings
    { commaStyle :: !CommaStyle
    , indent :: !Int
    -- ^ Setting this to zero also disables line breaks
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
    , spaceAfterColon :: !Bool
    -- ^ Useful for a compact representation
    } deriving (Eq, Show)

haskellStyle, rustStyle, compactStyle :: SerializeSettings
haskellStyle = SerializeSettings
    { commaStyle = CommaLeading
    , indent = 4
    , singleElementSpecial = True
    , unpackToplevel = True
    , openBracketOnSameLine = False
    , closeBracketOnSameLine = False
    , spaceAfterColon = True
    }
rustStyle = SerializeSettings
    { commaStyle = CommaTrailing
    , indent = 4
    , singleElementSpecial = True
    , unpackToplevel = False
    , openBracketOnSameLine = True
    , closeBracketOnSameLine = False
    , spaceAfterColon = True
    }
compactStyle = SerializeSettings
    { commaStyle = CommaHistoric
    , indent = 0
    , singleElementSpecial = True
    , unpackToplevel = False
    , openBracketOnSameLine = True
    , closeBracketOnSameLine = True
    , spaceAfterColon = False
    }

-- | Serialize a value to a lazy bytestring. For settings you can use
-- 'haskellStyle' or 'rustStyle' or 'compactStyle'
encode :: ToRon a => SerializeSettings -> a -> Lazy.ByteString
encode settings = dumps settings . toRon

-- | Serialize a value into a file. For settings you can use
-- 'haskellStyle' or 'rustStyle' or 'compactStyle'
encodeFile :: ToRon a => SerializeSettings -> FilePath -> a -> IO ()
encodeFile settings path = dumpFile settings path . toRon

-- | Serialize a RON value to a lazy bytestring. You probably want to use
-- 'encode' instead
dumps :: SerializeSettings -> Value -> Lazy.ByteString
dumps settings = toLazyByteString . ronBuilder settings

-- | Serialize a RON value into a file. You probably want to use 'encodeFile'
-- instead
dumpFile :: SerializeSettings -> FilePath -> Value -> IO ()
dumpFile settings path value = withFile path WriteMode $ \handle -> do
    -- recommended in builder package
    hSetBinaryMode handle True
    hSetBuffering handle $ BlockBuffering Nothing -- hmm
    hPutBuilder handle $ ronBuilder settings value

-- | The builder producing the serialized representation
ronBuilder :: SerializeSettings -> Value -> Builder
ronBuilder SerializeSettings {..} = toplevel where
    deeper !lvl = lvl + indent
    nl  = if indent == 0 then mempty else char7 '\n'
    shift lvl = string7 $ replicate lvl ' '
    bracketOpen afterColon lvl c
        | indent == 0  = char7 c
        | openBracketOnSameLine  = spc <> char7 c <> nl <> shift (deeper lvl)
            <> if commaStyle == CommaLeading
                then char7 ' ' <> char7 ' '
                else mempty
        | commaStyle == CommaLeading  =
            nl <> shift (deeper lvl) <> char7 c <> char7 ' '
        | otherwise  = nl <> shift lvl <> char7 c <> nl <> shift (deeper lvl)
            where spc = if afterColon && spaceAfterColon then char7 ' ' else mempty
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
            List xs | not (Vector.null xs) -> listContent 0 xs
            Record name xs | Text.null name -> recordContent 0 xs
            v -> go startIndent v
        else go startIndent
        -- special handling for haskell-style: don't indent the topmost block
        where startIndent = if commaStyle == CommaLeading
                then negate indent
                else 0
    --
    go = go' False
    go' !afterColon !lvl = \case
        Integral x -> spc <> integerDec x
        Floating x -> spc <> scientificBuilder x
        Char x -> spc <> char7 '\'' <> encodeChar x <> char7 '\''
        String x -> spc <> char7 '"' <> encodeString x <> char7 '"'
        Unit name -> spc <> if Text.null name
            then string7 "()"
            else fromText name
        List xs
            | null xs -> spc <> string7 "[]"
            | singleElementSpecial && isSimple (List xs)
                -> let !x = Vector.unsafeHead xs
                       open = if indent /= 0 then string7 "[ " else char7 '['
                       close = if indent /= 0 then string7 " ]" else char7 ']'
                   in spc <> open <> go lvl x <> close
            | otherwise -> bracketOpen afterColon lvl '[' <> listContent (deeper lvl) xs <> bracketClose lvl ']'
        Map xs
            | null xs -> spc <> string7 "{}"
            | singleElementSpecial && isSimple (Map xs)
                -> let (!k, !v) = head . Map.toList $ xs
                       open = if indent /= 0 then string7 "{ " else char7 '{'
                       close = if indent /= 0 then string7 " }" else char7 '}'
                   in spc <> open <> go lvl k <> char7 ':' <> go' True lvl v <> close
            | otherwise -> bracketOpen afterColon lvl '{' <> mapContent (deeper lvl) xs <> bracketClose lvl '}'
        Tuple name xs
            | singleElementSpecial && isSimple (Tuple name xs) ->
                let !x = Vector.unsafeHead xs
                    nameB = if Text.null name
                        then mempty
                        else fromText name <> char7 ' '
                    open = if indent /= 0 then string7 "( " else char7 '('
                    close = if indent /= 0 then string7 " )" else char7 ')'
                in spc <> nameB <> open <> go lvl x <> close
            | Text.null name ->
                bracketOpen afterColon lvl '(' <> listContent (deeper lvl) xs <> bracketClose lvl ')'
            | otherwise ->
                spc <> fromText name <> bracketOpen True lvl '(' <> listContent (deeper lvl) xs <> bracketClose lvl ')'
        Record name xs
            | singleElementSpecial && isSimple (Record name xs) ->
                let (!k, !v) = head . Map.toList $ xs
                    nameB = if Text.null name
                        then mempty
                        else fromText name <> char7 ' '
                    open = if indent /= 0 then string7 "( " else char7 '('
                    close = if indent /= 0 then string7 " )" else char7 ')'
                in spc <> nameB <> open <> fromText k <> char7 ':' <> go' True lvl v <> close
            | Text.null name ->
                bracketOpen afterColon lvl '(' <> recordContent (deeper lvl) xs <> bracketClose lvl ')'
            | otherwise ->
                spc <> fromText name <> bracketOpen True lvl '(' <> recordContent (deeper lvl) xs <> bracketClose lvl ')'
        where
            spc = if afterColon && spaceAfterColon then char7 ' ' else mempty
    --
    listContent lvl = (<> trailingComma) . mconcat . intersperse (comma lvl) . map (go lvl) . Vector.toList
    --
    mapContent lvl = (<> trailingComma) . mconcat . intersperse (comma lvl) . map toElem . Map.toList where
        toElem (k, v) = go lvl k <> char7 ':' <> go' True lvl v
    --
    recordContent lvl = (<> trailingComma) . mconcat . intersperse (comma lvl) . map toElem . Map.toList where
        toElem (k, v) = fromText k <> char7 ':' <> go' True lvl v

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

--- QuickCheck

instance Arbitrary CommaStyle where
    arbitrary = chooseEnum (minBound, maxBound)

instance Arbitrary SerializeSettings where
    arbitrary = SerializeSettings
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
