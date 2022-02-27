{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-do-bind #-}
module Data.Ron.Deserialize
--     ( decode, decodeLazy, decodeFile
--     , loads, loadsLazy, loadFile, loadFile'
--     , toplevel, value
--     , ParseError, DecodeError
--     ) where
    where

import Control.Applicative ((<|>), liftA2)
import Control.Exception (Exception, throwIO)
import Data.Attoparsec.ByteString (skip)
import Data.ByteString.Char8 (ByteString, cons)
import Data.ByteString.Lazy (toStrict)
import Data.Char (isAlpha, isAlphaNum, chr)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Ron.Class (FromRon, fromRon)
import Data.Scientific (Scientific, scientific)
import Data.Text (Text, uncons)
import Data.Text.Encoding (decodeUtf8, decodeUtf8')
import Data.Typeable (Typeable)
import Data.Vector (Vector)

import qualified Data.Map.Strict as Map
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Char8 as ByteString8
import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Data.Attoparsec.ByteString.Char8 hiding (feed, hexadecimal, decimal, isSpace, scientific, number)
import Data.Ron.Value
import Prelude hiding (takeWhile)


-- Each parser function assumes there is no whitespace before it, and must
-- consume all whitespace after it.
--
-- Parsers don't backtrack at all (except a few characters back sometimes
-- internally). It's mostly possible to understand what value is in front of us
-- by its first character, but sometimes we do have to parse the whole
-- identifier or number to see what character comes after it. The parsers xOrY
-- take care of that.
-- But just using those xOrY is not enough at times, since I didn't figure out
-- how to compose them properly: there are a lot of places where this ambiguity
-- arises. So i just duplicated that code. It's still not that bad, but could
-- be a lot better..
--
-- Also, fucking raw strings. Why not just start them with '#'?


--- Decode functions


-- | Parse a 'ByteString' to your type. The error is produced by attoparsec and is
-- not very useful.
decode :: FromRon a => ByteString -> Either String a
decode str = loads str >>= fromRon

-- | Parse a lazy 'Lazy.ByteString' to your type. The error is produced by
-- attoparsec and is not very useful.
decodeLazy :: FromRon a => Lazy.ByteString -> Either String a
decodeLazy str = loadsLazy str >>= fromRon

-- | Parse file content to your type. Throws 'ParseError' or 'DecodeError' on
-- errors.
decodeFile :: FromRon a => FilePath -> IO a
decodeFile path = loadFile path >>= pure . fromRon >>= \case
    Left err -> throwIO $! DecodeError err
    Right x -> pure x

-- | Parse a 'ByteString' to a 'Value'. You probably want 'decode' instead
loads :: ByteString -> Either String Value
loads = parseOnly (ws *> toplevel <* (takeWhile (const True) >>= \case {s | ByteString.null s -> pure (); s -> fail $ "Expected eof, got" <> show s}))

-- | Parse a lazy 'Lazy.ByteString' to a 'Value'. You probably want
-- 'decodeLazy' instead
loadsLazy :: Lazy.ByteString -> Either String Value
loadsLazy str = case Lazy.toChunks str of
    [] -> Left "Empty input"
    s:ss -> go ss $! parse (ws *> toplevel) s
  where
    -- since toplevel requires eof after end, _rest should always be nothing
    go _ (Fail _rest contexts message) = Left $
        "Parse error: " <> message
        <> "; context: " <> intercalate "; " contexts
    go [] (Done rest x) | ByteString.null rest = pure x
    go _ (Done rest _x) = Left $ "Unconsumed input after value: " <> show rest
    go [] (Partial _) = Left "Unexpected end of input"
    go (s:ss) (Partial feed) = go ss $! feed s

-- | Parse file. Throws 'ParseError'
loadFile :: FilePath -> IO Value
loadFile path = loadFile' path >>= \case
    Left err -> throwIO $! ParseError err
    Right x -> pure x

-- | Parse file and return the error in 'Either'
loadFile' :: FilePath -> IO (Either String Value)
loadFile' path = loadsLazy <$> Lazy.readFile path


--- Parsers


-- | Toplevel is either a toplevel 'list', toplevel 'record', or a regular ron
-- 'value'. The first two are hs-ron extensions
toplevel :: Parser Value
toplevel = toplevelRecord <|>  do
    v <- value
    toplevelList v <|> pure v

toplevelList :: Value -> Parser Value
toplevelList first = do
    char ','
    ws
    xs <- Vector.fromList . (first :) <$> sepBy value (char ',' *> ws)
    option () $ char ',' *> ws
    pure . List $ xs

toplevelRecord :: Parser Value
toplevelRecord = do
    firstField <- keyword
    char ':'
    ws
    firstValue <- value
    let initial = (firstField, firstValue)
    peekChar >>= \case
        Just ',' -> do
            skip1 -- ,
            ws
            let pair = do
                    k <- keyword
                    ws
                    char ':'
                    ws
                    v <- value
                    pure (k, v)
            xs <- sepBy pair (char ',' *> ws)
            option () $ char ',' *> ws
            pure . Record "" . Map.fromList $ initial:xs
        Nothing -> pure . Record "" . Map.fromList $ [initial]
        _ -> fail "Expecting , at toplevel record"

value :: Parser Value
value = intOrFloat
    <|> Char <$> character
    <|> String <$> (ronString <|> ronRawString)
    <|> List <$> list
    <|> Map <$> ronMap
    <|> Unit <$> unitWithBrackets
    <|> record
    <|> tuple
    <|> Unit <$> unit
    <|> fail "Incorrect value"


--- Numbers ---


number :: (Char -> Bool) -> Parser ByteString
number p = liftA2 cons (satisfy p) (takeWhile $ \c -> c == '_' || p c)

intOrFloat :: Parser Value
intOrFloat = do
    !positive <- peekChar' >>= \case
        '+' -> skip1 *> pure True
        '-' -> skip1 *> pure False
        _ -> pure True
    choice
        [ string "0x" *> fmap Integral (hexadecimal positive)
        , string "0o" *> fmap Integral (octal positive)
        , string "0b" *> fmap Integral (binary positive)
        , char '.' *> fmap Floating (floating positive "0")
        , do -- floating
            whole <- number decimalDigit
            char '.'
            Floating <$> floating positive whole
        , Integral <$> decimal positive
        ]

buildNumber :: Integer -> Bool -> ByteString -> Integer
buildNumber base positive digits = mbNegate . ByteString8.foldl' step 0 $ digits where
    mbNegate = if positive then id else negate
    step !a '_' = a
    step !a !d = a * base + toDigit d
    toDigit = \case
        '0' -> 0
        '1' -> 1
        '2' -> 2
        '3' -> 3
        '4' -> 4
        '5' -> 5
        '6' -> 6
        '7' -> 7
        '8' -> 8
        '9' -> 9
        'a' -> 10
        'b' -> 11
        'c' -> 12
        'd' -> 13
        'e' -> 14
        'f' -> 15
        'A' -> 10
        'B' -> 11
        'C' -> 12
        'D' -> 13
        'E' -> 14
        'F' -> 15
        _ -> error "Not a number"

hexadecimal positive
    = buildNumber 16 positive <$> number hexadecimalDigit <* ws
decimal positive
    = buildNumber 10 positive <$> number decimalDigit <* ws
octal positive
    = buildNumber 8 positive <$> number octalDigit <* ws
binary positive
    = buildNumber 2 positive <$> number binaryDigit <* ws

floating :: Bool -> ByteString -> Parser Scientific
floating positive !wholeStr = do
    -- dot is already skipped
    !fracStr <- takeWhile (\c -> c == '_' || decimalDigit c)
    let !fracPart = fromInteger $! buildNumber 10 positive fracStr
    let !wholePart = fromInteger $! buildNumber 10 positive wholeStr
    let !shift = fromIntegral $! ByteString.length fracStr
    !e <- (satisfy (\w -> w == 'e' || w == 'E') *> decimal') <|> pure 0
    let !mantissa = wholePart * 10^shift + fracPart
    let !power = e - shift
    ws
    pure $! scientific mantissa (fromIntegral power)
    where
        decimal' = peekChar' >>= \case
            '+' -> skip1 *> decimal True
            '-' -> skip1 *> decimal False
            c | isDigit c -> decimal True
            _ -> fail "Expected + or - or digit (scientific notation power)"


--- Strings ---


character :: Parser Char
character = char '\'' >> peekChar' >>= \case
    '\\' -> skip1 *> escapedChar <* char '\'' <* ws
    _ -> do
        chunk <- takeWhile1 (/= '\'')
        skip1
        ws
        text <- case decodeUtf8' chunk of
                Right x -> pure x
                Left _err -> fail "Incorrect utf8 in Char"
        case uncons text of
            Just (c, cs) | Text.length cs == 0 -> pure c
            _ -> fail "Incorrect length of Char content"

-- This is common for string and char. It seems by the spec @\'@ is incorrect
-- sequence for string, and @\"@ is incorrect sequence for char. I choose to
-- parse both for both for simplicity. Coming from C++ I want to call this
-- "undefined behaviour" in case of incorrect source RON file ;-)
escapedChar :: Parser Char
escapedChar = anyChar >>= \case
  '\\' -> pure '\\'
  '\"' -> pure '\"'
  '\'' -> pure '\''
  'b' -> pure '\b'
  'f' -> pure '\f'
  'n' -> pure '\n'
  'r' -> pure '\r'
  't' -> pure '\t'
  'u' -> do
      digits <- count 4 $ satisfy hexadecimalDigit
      let code = fromIntegral . buildNumber 16 True . ByteString8.pack $ digits
      pure $ chr code
  _ -> fail "Invalid escape sequence"

ronString :: Parser Text
ronString = char '\"' *> (decodeUtf8 . toStrict . Builder.toLazyByteString <$> go mempty) <* skip1 <* ws
  where
    go :: Builder.Builder -> Parser Builder.Builder
    go !builder = do
        chunk <- takeTill (\c -> c == '\"' || c == '\\')
        let !r = builder <> Builder.byteString chunk
        peekChar' >>= \case
            '\"' -> pure r
            '\\' -> do
              skip1
              c <- escapedChar
              go $ r <> Builder.charUtf8 c
            _ -> error "takeTill took till wrong character (not \" or \\)"

ronRawString :: Parser Text
ronRawString = do
    char 'r'
    delimeter <- takeWhile (== '#')
    char '\"'
    let go !builder = do
            chunk <- takeWhile (/= '\"')
            skip1
            let !r = builder <> Builder.byteString chunk
            (string delimeter *> pure r) <|> go (r <> Builder.char7 '\"')
    r <- decodeUtf8 . toStrict . Builder.toLazyByteString <$> go mempty
    ws
    pure r


--- List, Map ---


list :: Parser (Vector Value)
list = do
    char '['
    ws
    xs <- sepBy value (char ',' *> ws)
    option () $ char ',' *> ws
    char ']'
    ws
    pure . Vector.fromList $ xs

ronMap :: Parser (Map Value Value)
ronMap = do
    char '{'
    ws
    let pair = do
            k <- value
            char ':'
            ws
            v <- value
            pure (k, v)
    xs <- sepBy pair (char ',' *> ws)
    option () $ char ',' *> ws
    char '}'
    ws
    pure . Map.fromList $ xs


--- Algeraic types


keyword :: Parser Text
keyword = decodeUtf8 <$> liftA2 cons (satisfy startsIdentifier) (takeWhile isKeyword)

unit, unitWithBrackets :: Parser Text
unit = keyword <* ws
unitWithBrackets
      = (keyword <* ws <* char '(' <* ws <* char ')' <* ws)
    <|> (char '(' *> ws *> char ')' *> ws *> pure "")


record :: Parser Value
record = do
    name <- keyword <|> pure ""
    ws
    char '('
    ws
    let pair = do
            k <- keyword
            ws
            char ':'
            ws
            v <- value
            pure (k, v)
    xs <- sepBy pair (char ',' *> ws)
    option () $ char ',' *> ws
    char ')'
    ws
    pure . Record name . Map.fromList $ xs

tuple :: Parser Value
tuple = do
    name <- (keyword <* ws) <|> pure ""
    char '('
    ws
    xs <- sepBy value (char ',' *> ws)
    option () $ char ',' *> ws
    char ')'
    ws
    pure . Tuple name . Vector.fromList $ xs


--- Common ---

-- | Whitespace and comment skipper
ws :: Parser ()
ws = skipWhile isSpace >> peekChar >>= \case
    Nothing -> pure ()
    Just c | c == '/' -> skip1 >> anyChar >>= \case
        '/' -> skipWhile (/= '\n') >> endOr skip1 >> ws
        '*' -> goMultiline (1 :: Int)
        _ -> fail "Unexpected '/', not followed by a comment starting"
    _ -> pure () -- not a comment
  where
    goMultiline 0 = ws -- end of multiline comment, try taking some new whitespace
    goMultiline level = do
        skipWhile (\c -> c /= '*' && c /= '/')
        endOr $ anyChar >>= \case
            '*' -> endOr $ anyChar >>= \case
                '/' -> goMultiline $! level - 1
                _ -> goMultiline level
            '/' -> endOr $ anyChar >>= \case
                '*' -> goMultiline $! level + 1
                _ -> goMultiline level
            _ -> error "skipWhile skipped until unexpected character"

isSpace c = c == ' ' || c == '\n' || c == '\r' || c == '\t'
isKeyword c = isAlphaNum c || c == '_' || c == '\''
startsIdentifier c = isAlpha c || c == '_' -- this can also be a raw string

binaryDigit c = c == '0' || c == '1'
octalDigit c = binaryDigit c || c == '2' || c == '3' || c == '4'
                             || c == '5' || c == '6' || c == '7'
decimalDigit c = octalDigit c || c == '8' || c == '9'
hexadecimalDigit c = decimalDigit c
                  || c == 'a' || c == 'b' || c == 'c' || c == 'd' || c == 'e' || c == 'f'
                  || c == 'A' || c == 'B' || c == 'C' || c == 'D' || c == 'E' || c == 'F'

skip1 = skip (const True)
endOr parser = atEnd >>= \case {True -> pure (); False -> parser}


--- Exceptions


-- | Error parsing 'ByteString' to 'Value'
newtype ParseError = ParseError String
    deriving (Show, Typeable)

-- | Error parsing 'Value' to custom type
newtype DecodeError = DecodeError String
    deriving (Show, Typeable)

instance Exception ParseError
instance Exception DecodeError
