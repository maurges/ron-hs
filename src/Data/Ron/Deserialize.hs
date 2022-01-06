{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-do-bind #-}
module Data.Ron.Deserialize
    where

import Control.Applicative ((<|>), liftA2)
import Data.Char (isAlpha, isAlphaNum, chr)
import Data.Map (Map)
import Data.Text (Text, cons)
import Data.Vector (Vector)

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text (toStrict)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Vector as Vector

import Data.Attoparsec.Text hiding (hexadecimal, decimal)
import Data.Ron.Value
import Prelude hiding (takeWhile)


loads :: Text -> Either String Value
loads = parseOnly (ws *> toplevel <* endOfInput)


toplevel :: Parser Value
toplevel = value

value :: Parser Value
value = peekChar' >>= \case
    c | startsNumber c -> intOrFloat
      | startsString c -> String <$> ronString
      | startsList c -> List <$> list
      | startsMap c -> Map <$> ronMap
      | startsStruct c -> recordOrTuple ""
      | startsIdentifier c -> identifierLike c
      | otherwise -> fail $ "Unexpected symbol: " <> show c


--- Numbers ---


intOrFloat :: Parser Value
intOrFloat = go <* ws where
  go = do
    !positive <- ((== '+') <$> satisfy (\c -> c == '-' || c == '+'))
             <|> pure True
    let intOrFloatSimple = do
            whole <- takeWhile (\c -> decimalDigit c || c == '_')
            peekChar >>= \case
                Just '.' -> skip1 *> (Floating <$> floating positive whole)
                _ -> pure . Integral $ buildNumber 10 positive whole
    peekChar' >>= \case
        '.' -> skip1 *> (Floating <$> floating positive "0")
        '0' -> skip1 >> peekChar' >>= \case
            ('x') -> skip1 *> (Integral <$> hexadecimal positive)
            ('o') -> skip1 *> (Integral <$> octal positive)
            ('b') -> skip1 *> (Integral <$> binary positive)
            ('.') -> skip1 *> (Floating <$> floating positive "0")
            _ -> intOrFloatSimple
        _ -> intOrFloatSimple

buildNumber :: Integer -> Bool -> Text -> Integer
buildNumber base positive digits = mbNegate . Text.foldl' step 0 $ digits where
    mbNegate = if positive then id else negate
    step !a '_' = a
    step !a !d = a * base + (toDigit d)
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
    = buildNumber 16 positive <$> takeWhile (\c -> c == '_' || hexadecimalDigit c)
decimal positive
    = buildNumber 10 positive <$> takeWhile (\c -> c == '_' || decimalDigit c)
octal positive
    = buildNumber 8 positive <$> takeWhile (\c -> c == '_' || octalDigit c)
binary positive
    = buildNumber 2 positive <$> takeWhile (\c -> c == '_' || binaryDigit c)

floating :: Bool -> Text -> Parser Double
floating positive !wholeStr = do
    -- dot is already skipped
    !fracStr <- takeWhile (\c -> c == '_' || decimalDigit c)
    let !fracPart = fromInteger $! buildNumber 10 positive fracStr
    let !wholePart = fromInteger $! buildNumber 10 positive wholeStr
    let !shift = fromIntegral $! Text.length fracStr
    !e <- (satisfy (\w -> w == 'e' || w == 'E') *> decimal True) <|> pure 0
    let !mantissa = wholePart * 10^shift + fracPart
    let !power = e - shift
    ws
    pure $! mantissa * 10^^power

--- Strings ---

ronString :: Parser Text
ronString = skip1 *> (Text.toStrict . Builder.toLazyText <$> go mempty) <* skip1 <* ws
  where
    go :: Builder.Builder -> Parser Builder.Builder
    go !builder = do
        chunk <- takeTill (\c -> c == '\"' || c == '\\')
        let !r = builder <> Builder.fromText chunk
        peekChar' >>= \case
            '\"' -> pure r
            '\\' -> skip1 >> peekChar' >>= \case
                '\\' -> skip1 *> go (r <> Builder.singleton '\\')
                '\"' -> skip1 *> go (r <> Builder.singleton '\"')
                'b' -> skip1 *> go (r <> Builder.singleton '\b')
                'f' -> skip1 *> go (r <> Builder.singleton '\f')
                'n' -> skip1 *> go (r <> Builder.singleton '\n')
                'r' -> skip1 *> go (r <> Builder.singleton '\r')
                't' -> skip1 *> go (r <> Builder.singleton '\t')
                'u' -> skip1 *> do
                    digits <- count 4 $ satisfy hexadecimalDigit
                    let code = fromIntegral . buildNumber 16 True . Text.pack $ digits
                    go $ r <> Builder.singleton (chr code)
                _ -> fail "Invalid escape sequence"
            _ -> error "takeTill took till wrong character (not \" or \\)"

ronRawString :: Parser Text
ronRawString = do
    delimeter <- takeWhile (== '#')
    char '\"'
    let go !builder = do
            chunk <- takeWhile (/= '\"')
            skip1
            let !r = builder <> Builder.fromText chunk
            (string delimeter *> pure r) <|> go (r <> Builder.singleton '\"')
    r <- Text.toStrict . Builder.toLazyText <$> go mempty
    ws
    pure r

--- List, Map ---

list :: Parser (Vector Value)
list = do
    skip1 -- [
    ws
    xs <- sepBy value (char ',' *> ws)
    option () $ char ',' *> ws
    char ']'
    ws
    pure . Vector.fromList $ xs

ronMap :: Parser (Map Value Value)
ronMap = do
    skip1 -- {
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

recordOrTuple :: Text -> Parser Value
recordOrTuple name = skip1 >> ws >> peekChar' >>= \case
    -- either a value or an identifier (or end)
    -- identifier overlaps with record field
    ')' -> skip1 *> ws *> pure (Unit name)
    'r' -> skip1 >> peekChar' >>= \case
        c | c == '#' || c == '\"' -> do
            val <- String <$> ronRawString
            ws
            option () $ char ',' *> ws
            Tuple name <$> tuple [val]
          | otherwise -> common (Just 'r')
    c | startsIdentifier c -> common Nothing
        -- not starting an identifier means it's not a record field, so a tuple
      | otherwise -> Tuple name <$> tuple []
  where
    common mbHead = do
        ident <- case mbHead of {Nothing -> id; Just c -> cons c}
                 <$> takeWhile isKeyword
        ws
        peekChar' >>= \case
            ':' -> skip1 *> ws *> do
                v <- value
                peekChar' >>= \case
                    ',' -> skip1 *> ws
                    _ -> pure ()
                Record name <$> record [(ident, v)]
            '(' -> do -- a tuple with first element as a tuple or record
                val <- recordOrTuple ident
                peekChar' >>= \case
                    ',' -> skip1 *> ws
                    _ -> pure ()
                Tuple name <$> tuple [val]
            ',' -> skip1 *> ws *> (Tuple name <$> tuple [Unit ident])
            ')' -> skip1 *> ws *> pure (Tuple name (Vector.fromList [Unit ident]))
            _ -> fail "Expecting expecting ':', ',' or '('"

tuple :: [Value] -> Parser (Vector Value)
tuple initial = do
    xs <- sepBy value (char ',' *> ws)
    option () $ char ',' *> ws
    char ')'
    ws
    pure . Vector.fromList $ initial <> xs

record :: [(Text, Value)] -> Parser (Map Text Value)
record initial = do
    let pair = do
            k <- liftA2 cons (satisfy startsIdentifier) (takeWhile isKeyword)
            ws
            char ':'
            ws
            v <- value
            pure (k, v)
    xs <- sepBy pair (char ',' *> ws)
    option () $ char ',' *> ws
    char ')'
    ws
    pure . Map.fromList $ initial <> xs

-- Algebraic struct (named unit, record, tuple) or a raw string
identifierLike :: Char -> Parser Value
identifierLike 'r' = skip1 >> peekChar >>= \case
    Nothing -> pure $ Unit "r"
    Just '#' -> String <$> ronRawString
    Just '\"' -> String <$> ronRawString
    _ -> do
        name <- cons 'r' <$> takeWhile isKeyword
        ws
        peekChar >>= \case
            Just '(' -> recordOrTuple name
            _ -> pure $ Unit name
identifierLike _ = do
    name <- takeWhile isKeyword
    ws
    peekChar >>= \case
        Just '(' -> recordOrTuple name
        _ -> pure $ Unit name

ws :: Parser ()
ws = skipWhile isSpace >> peekChar >>= \case
    Nothing -> pure ()
    Just c | c == '/' -> skip1 >> anyChar >>= \case
        '/' -> skipWhile (/= '\n') >> endOr skip1 >> ws
        '*' -> goMultiline
        _ -> fail "Unexpected '/', not followed by a comment starting"
    _ -> pure () -- not a comment
  where
    goMultiline = do
        skipWhile (/= '*')
        endOr skip1
        endOr $ do
            c <- anyChar
            if c == '/'
                then ws -- end of multiline comment, try taking some new whitespace
                else goMultiline

isSpace c = c == ' ' || c == '\n' || c == '\r' || c == '\t'
isKeyword c = isAlphaNum c || c == '_' || c == '\''
startsNumber c = c == '+' || c == '-' || c == '.' || decimalDigit c
startsString c = c == '\"'
startsList c = c == '['
startsMap c = c == '{'
startsStruct c = c == '('
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
