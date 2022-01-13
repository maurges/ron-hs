module Data.Ron.Class
    ( ToRon (..), FromRon (..)
    , ParseResult
    ) where

import Control.Arrow ((***))
import Control.Applicative (liftA2)
import Data.Text (Text)

import qualified Data.Map as Map
import qualified Data.Vector as Vector

import Data.Ron.Value
import Prelude hiding (fail)


-- | In the future this might become more smart and report the error location.
-- In the present we don't even have the notion of "location" in RON. So this
-- sort-of MonadFail thing will do
type ParseResult = Either String
fail :: String -> ParseResult a
fail = Left

-- | A class of values that can be encoded to RON format
class ToRon a where
    toRon :: a -> Value

-- | A class of values that can be from RON format
class FromRon a where
    fromRon :: Value -> ParseResult a

instance ToRon Int where
    toRon = Integral . fromIntegral
instance FromRon Int where
    fromRon (Integral i) = pure . fromIntegral $ i
    fromRon _ = fail "Not an integer"
instance ToRon Integer where
    toRon = Integral
instance FromRon Integer where
    fromRon (Integral i) = pure i
    fromRon _ = fail "Not an integer"

instance ToRon Double where
    toRon = Floating
instance FromRon Double where
    fromRon (Floating x) = pure x
    fromRon _ = fail "Not a floating"

instance ToRon Char where
    toRon = Char
instance FromRon Char where
    fromRon (Char x) = pure x
    fromRon _ = fail "Expected Char"

instance ToRon Text where
    toRon = String
instance FromRon Text where
    fromRon (String x) = pure x
    fromRon _ = fail "Not text"

instance ToRon Bool where
    toRon True = Unit "true"
    toRon False = Unit "false"
instance FromRon Bool where
    fromRon (Unit name)
        | name == "True"  || name == "true"  = pure True
        | name == "False" || name == "false" = pure False
        | otherwise       = fail $ "Invalid enum value: " <> show name
    fromRon _ = fail "Not a boolean"

instance (ToRon a) => ToRon (Vector.Vector a) where
    toRon = List . Vector.map toRon
instance (FromRon a) => FromRon (Vector.Vector a) where
    fromRon (List xs) = traverse fromRon xs
    fromRon _ = fail "Not a list"
instance (ToRon a) => ToRon [a] where
    toRon = List . Vector.fromList . map toRon
instance (FromRon a) => FromRon [a] where
    fromRon (List xs) = Vector.toList <$> traverse fromRon xs
    fromRon _ = fail "Not a list"

instance (ToRon k, ToRon v) => ToRon (Map.Map k v) where
    toRon = Map . Map.fromList . map (toRon *** toRon) . Map.toAscList
instance (FromRon k, FromRon v, Ord k) => FromRon (Map.Map k v) where
    fromRon (Map xs) = Map.fromList <$> traverse tupleFromRon (Map.toAscList xs)
        where tupleFromRon (a, b) = liftA2 (,) (fromRon a) (fromRon b)
    fromRon _ = fail "Not a map"

instance ToRon () where
    toRon () = Unit "()"
instance FromRon () where
    fromRon (Unit name)
        | name == "()" = pure ()
        | name == ""   = pure ()
        | otherwise    = fail "Not a unit enum value"
    fromRon _ = fail "Not a unit"

instance (ToRon a) => ToRon (Maybe a) where
    toRon (Just x) = Tuple "Some" (Vector.singleton $ toRon x)
    toRon Nothing = Unit "None"
instance (FromRon a) => FromRon (Maybe a) where
    fromRon (Tuple name xs)
        | (name == "Just" || name == "Some") && Vector.length xs == 1
            = Just <$> fromRon (Vector.unsafeHead xs)
        | otherwise = fail "Not a Maybe enum"
    fromRon (Unit name)
        | name == "Nothing" || name == "None" = pure Nothing
        | otherwise = fail $ "Incorrect Maybe name: " <> show name
    fromRon _ = fail "Not a named tuple or unit"

instance (ToRon a, ToRon b) => ToRon (Either a b) where
    toRon (Left x) = Tuple "Left" (Vector.singleton $ toRon x)
    toRon (Right x) = Tuple "Right" (Vector.singleton $ toRon x)
instance (FromRon a, FromRon b) => FromRon (Either a b) where
    fromRon (Tuple name xs)
        | Vector.length xs == 1  =
            if | name == "Left"  -> Left <$> fromRon (Vector.unsafeHead xs)
               | name == "Right" -> Right <$> fromRon (Vector.unsafeHead xs)
               | otherwise -> fail "Not an Either enum"
        | otherwise = fail "Incorrect amount of arguments to Either"
    fromRon _ = fail "Not an Either value"

instance (ToRon a1, ToRon a2) => ToRon (a1, a2) where
    toRon (a1, a2) = Tuple "" . Vector.fromList $ [toRon a1, toRon a2]

instance (FromRon a1, FromRon a2) => FromRon (a1, a2) where
    fromRon (Tuple name xs)
        | name == "" = case Vector.toList xs of
            [a1, a2] -> liftA2 (,) (fromRon a1) (fromRon a2)
            _ -> fail "Incorrect amount of tuple values"
        | otherwise = fail "Incorrect tuple name"
    fromRon _ = fail "Not a tuple"
