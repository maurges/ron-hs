module Data.Ron.Value.Internal
    where

import Control.Applicative ((<|>))
import Data.Foldable (msum)
import Data.Maybe (isNothing)
import Data.Ron.Class (ToRon (toRon))
import Data.Text (pack)

import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector

import Data.Ron.Value

(!==!) :: (Eq a, ToRon a) => a -> a -> Maybe (Value, Value)
a !==! b = if a == b then Nothing else Just (toRon a, toRon b)

vshow :: Show a => a -> Value
vshow = String . pack . show

similar' :: Value -> Value -> Bool
similar' a b = isNothing $ similar a b

-- | Like '==' but not precise for floats
similar :: Value -> Value -> Maybe (Value, Value)
similar (Integral a) (Integral b) = a !==! b
similar (Floating a) (Floating b) = if abs (a - b) < 0.001 then Nothing else Just (Floating a, Floating b)
similar (Char a)     (Char b)     = a !==! b
similar (String a)   (String b)   = a !==! b
similar (List a)     (List b)     = vecSimilar a b
similar (Map a)      (Map b)      = mapSimilar a b
similar (Unit n)     (Unit m)     = n !==! m
similar (Tuple n a)  (Tuple m b)  = (n !==! m) <|> vecSimilar a b
similar (Record n a) (Record m b) = (n !==! m) <|> mapSimilar a b
similar _ _ = Just (String "different types", Unit "")

vecSimilar :: Vector.Vector Value -> Vector.Vector Value -> Maybe (Value, Value)
vecSimilar a b = msum $ Vector.zipWith similar a b

mapSimilar :: (Eq k, Show k) => Map.Map k Value -> Map.Map k Value -> Maybe (Value, Value)
mapSimilar a b = go (Map.toAscList a) (Map.toAscList b) where
    go [] [] = Nothing
    go ((ak, av):as) ((bk, bv):bs) = (if ak == bk then Nothing else Just (vshow ak, vshow bk)) <|> (av `similar` bv) <|> go as bs
    go x y = Just (toRon $ length x, toRon $ length y) -- different amount of values
