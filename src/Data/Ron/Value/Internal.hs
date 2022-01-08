module Data.Ron.Value.Internal
    where

import qualified Data.Map as Map
import qualified Data.Vector as Vector

import Data.Ron.Value

-- | Like '==' but not precise for floats
similar :: Value -> Value -> Bool
similar (Integral a) (Integral b) = a == b
similar (Floating a) (Floating b) = abs (a - b) < 0.001
similar (String a)   (String b)   = a == b
similar (List a)     (List b)     = vecSimilar a b
similar (Map a)      (Map b)      = mapSimilar a b
similar (Unit n)     (Unit m)     = n == m
similar (Tuple n a)  (Tuple m b)  = n == m && vecSimilar a b
similar (Record n a) (Record m b) = n == m && mapSimilar a b
similar _ _ = False

vecSimilar :: Vector.Vector Value -> Vector.Vector Value -> Bool
vecSimilar a b = foldr (&&) True $ Vector.zipWith similar a b

mapSimilar :: Eq k => Map.Map k Value -> Map.Map k Value -> Bool
mapSimilar a b = go (Map.toAscList a) (Map.toAscList b) where
    go [] [] = True
    go ((ak, av):as) ((bk, bv):bs) = ak == bk && av `similar` bv && go as bs
    go _ _ = False -- different amount of values
