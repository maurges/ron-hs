{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 800
-- a) THQ works on cross-compilers and unregisterised GHCs
-- b) may make compilation faster as no dynamic loading is ever needed (not sure about this)
-- c) removes one hindrance to have code inferred as SafeHaskell safe
{-# LANGUAGE TemplateHaskellQuotes #-}
#else
{-# LANGUAGE TemplateHaskell #-}
#endif

module Data.Ron.Value
    where

import Control.Applicative (liftA2)
import Data.Hashable (Hashable, hashWithSalt)
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Test.QuickCheck (sized, Arbitrary, arbitrary, Gen, oneof, shuffle, chooseInt)

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Language.Haskell.TH.Syntax as TH


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

--- Hashable ---

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

--- Template Haskell ---

instance TH.Lift Value where
    lift (Integral x) = [| Integral x |]
    lift (Floating x) = [| Floating x |]
    lift (String x) = [| String (Text.pack s) |]
        where s = Text.unpack x
    lift (Boolean x) = [| Boolean  x |]
    lift (List x) = [| List (Vector.fromList a) |]
        where a = Vector.toList x
    lift (Map x) = [| Map (Map.fromAscList m) |]
        where m = Map.toAscList x
    lift (Unit x) = [| Unit (Text.pack s) |]
        where s = Text.unpack x
    lift (Tuple n x) = [| Tuple (Text.pack s) (Vector.fromList a)|]
        where s = Text.unpack n
              a = Vector.toList x
    lift (Record n x) = [| Record (Text.pack s) (Map.fromAscList m) |]
        where s = Text.unpack n
              m = Map.toAscList x

#if MIN_VERSION_template_haskell(2,17,0)
    liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
    liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

--- QuickCheck

instance Arbitrary Value where
    arbitrary = sized arbValue
    -- what the hell is 'shrink'?

arbValue :: Int -> Gen Value
arbValue size
    | size <= 0 = oneof
        [ Integral <$> arbitrary
        , Floating <$> arbitrary
        , String <$> arbText
        , Boolean <$> arbitrary
        , Unit <$> arbText
        ]
    | otherwise = oneof
        [ List <$> arbList size
        , Map <$> arbMap size
        , liftA2 Tuple arbText (arbList size)
        , liftA2 Record arbText (arbRecord size)
        ]

arbText :: Gen Text
arbText = Text.pack <$> arbitrary

arbList :: Int -> Gen (Vector Value)
arbList size = do
    sizes <- arbPartition (size - 1)
    Vector.fromList <$> traverse arbValue sizes

arbMap :: Int -> Gen (Map Value Value)
arbMap size = do
    sizes <- arbPartition (size - 1)
    let makeElem s = liftA2 (,) (arbValue 0) (arbValue s)
    Map.fromList <$> traverse makeElem sizes

arbRecord :: Int -> Gen (Map Text Value)
arbRecord size = do
    sizes <- arbPartition (size - 1)
    let makeElem s = liftA2 (,) arbText (arbValue s)
    Map.fromList <$> traverse makeElem sizes

-- | Produces new sizes for recursion
arbPartition :: Int -> Gen [Int]
arbPartition size = case compare size 1 of
    LT -> pure []
    EQ -> pure [1]
    GT -> shuffle =<< go size []
        where
            go 0 xs = pure xs
            go !s xs = do
                x <- chooseInt (1, s)
                go (s - 1) (x:xs)
