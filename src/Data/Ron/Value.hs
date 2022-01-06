module Data.Ron.Value
    where

import Data.Hashable (Hashable, hashWithSalt)
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Map.Strict as Map


-- Alternatives:
-- Integer -> Int, Int64
-- Scientific -> Double
-- Map -> HashMap, List of tuples, Vector of tuples (especially for record)
-- Vector -> List (for tuples)
-- NonEmpty for tuple and record? Or unify unit with both of them
data Value
    = Integral !Integer
    | Floating !Double
    | String   !Text
    | Boolean  !Bool
    | List     !(Vector Value)
    | Map      !(Map Value Value)
    | Unit     !Text
    | Tuple    !Text !(Vector Value)
    | Record   !Text !(Map Text Value)
    deriving (Eq, Show, Ord)

hashValue :: Int -> Value -> Int
hashValue s (Integral x)  = s `hashWithSalt` (0::Int) `hashWithSalt` x
hashValue s (Floating x)  = s `hashWithSalt` (1::Int) `hashWithSalt` x
hashValue s (String x)    = s `hashWithSalt` (2::Int) `hashWithSalt` x
hashValue s (Boolean x)   = s `hashWithSalt` (3::Int) `hashWithSalt` x
hashValue s (List xs)     = foldl' hashWithSalt
                            (s `hashWithSalt` (4::Int))
                            xs
hashValue s (Map xs)      = Map.foldlWithKey'
                            (\acc k v -> acc `hashWithSalt` k `hashWithSalt` v)
                            (s `hashWithSalt` (5::Int))
                            xs
hashValue s (Unit n)      = s `hashWithSalt` (6::Int) `hashWithSalt` n
hashValue s (Tuple n xs)  = foldl' hashWithSalt
                            (s `hashWithSalt` (7::Int) `hashWithSalt` n)
                            xs
hashValue s (Record n xs) = Map.foldlWithKey'
                            (\acc k v -> acc `hashWithSalt` k `hashWithSalt` v)
                            (s `hashWithSalt` (8::Int) `hashWithSalt` n)
                            xs

instance Hashable Value where
    hashWithSalt = hashValue
