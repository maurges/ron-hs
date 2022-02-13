{-# LANGUAGE TypeApplications #-}
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
    ( Value (..)
    ) where

import Control.Applicative (liftA2)
import Data.Hashable (Hashable, hashWithSalt)
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Scientific (Scientific, fromFloatDigits)
import Data.Vector (Vector)
import Test.QuickCheck (sized, Arbitrary, arbitrary, Gen, oneof, shuffle, choose, elements, listOf)

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
-- | A RON value represented in haskell. This is the intermediate
-- representation between string and your own type.
--
-- @()@ is represented as @Unit ""@
--
-- 'Tuple' and 'Record' contents are not empty when parsed, since the empty
-- contents denote a 'Unit'
data Value
    = Integral !Integer
    | Floating !Scientific
    | Char     !Char
    | String   !Text
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
hashValue s (Char x)      = s `hashWithSalt` (2::Int) `hashWithSalt` x
hashValue s (String x)    = s `hashWithSalt` (3::Int) `hashWithSalt` x
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
    lift (Char x) = [| Char x |]
    lift (String x) = [| String (Text.pack s) |]
        where s = Text.unpack x
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
arbValue = arbValue' True

arbValue' :: Bool -> Int -> Gen Value
arbValue' allowFloat size
    | size <= 0 = oneof $
        [ Integral <$> arbitrary
        , Char <$> arbitrary
        , String <$> arbText
        , Unit <$> arbIdentifier
        ] <> if allowFloat then [Floating . fromFloatDigits <$> arbitrary @Double] else []
    | otherwise = oneof
        [ List <$> arbList size
        , Map <$> arbMap size
        , Tuple "" <$> arbTuple size
        , Record "" <$> arbRecord size
        , liftA2 Tuple arbIdentifier (arbTuple size)
        , liftA2 Record arbIdentifier (arbRecord size)
        ]

arbText :: Gen Text
arbText = Text.pack <$> arbitrary

arbIdentifier :: Gen Text
arbIdentifier = do
    h <- arbStartIdentifier
    t <- listOf arbKeyword
    pure . Text.pack $ h:t
    where
        starts = ['a'..'z'] <> ['A'..'Z'] <> ['_']
        keyword = starts <> ['0'..'9'] <> ['\'']
        arbStartIdentifier = elements starts
        arbKeyword = elements keyword

arbList :: Int -> Gen (Vector Value)
arbList size = do
    sizes <- arbPartition (size - 1)
    Vector.fromList <$> traverse arbValue sizes

arbMap :: Int -> Gen (Map Value Value)
arbMap size = do
    sizes <- arbPartition (size - 1)
    let makeElem s = liftA2 (,) (arbValue' False 0) (arbValue s)
    Map.fromList <$> traverse makeElem sizes

arbTuple :: Int -> Gen (Vector Value)
arbTuple size = do
    sizes <- arbPartition (size - 1)
    firstSize <- if size <= 1 then pure 0 else choose (1, size-1)
    first <- arbValue firstSize
    Vector.fromList . (first:) <$> traverse arbValue sizes

arbRecord :: Int -> Gen (Map Text Value)
arbRecord size = do
    sizes <- arbPartition (size - 1)
    let makeElem s = liftA2 (,) arbIdentifier (arbValue s)
    firstSize <- if size <= 1 then pure 0 else choose (1, size-1)
    first <- makeElem firstSize
    Map.fromList . (first:) <$> traverse makeElem sizes

-- | Produces new sizes for recursion
arbPartition :: Int -> Gen [Int]
arbPartition size = case compare size 1 of
    LT -> pure []
    EQ -> pure [1]
    GT -> shuffle =<< go size []
        where
            go 0 xs = pure xs
            go !s xs = do
                x <- choose (1, s)
                go (s - 1) (x:xs)
