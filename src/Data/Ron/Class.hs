{-# LANGUAGE DefaultSignatures, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE EmptyCase #-}
module Data.Ron.Class
    ( ToRon (..), FromRon (..)
    , ParseResult
    -- * Generic encoding
    , toRonGeneric
    , fromRonGeneric
    , RonSettings (..)
    , RonFlags (..)
    , strictRonSettings, laxRonSettings
    , GToRon, GFromRon
    ) where

import Control.Arrow ((***))
import Control.Applicative (liftA2)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Map.Strict (Map)
import Data.Proxy (Proxy (..))
import Data.Ron.Class.Internal (productSize, ProductSize)
import Data.Scientific (fromFloatDigits, toRealFloat, Scientific)
import Data.Text (Text, pack)
import Data.Vector (Vector)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics
    ( Generic (Rep, from, to), V1, U1 (..), (:+:)(..), (:*:)(..)
    , K1 (..), M1 (..)
    , C, S, D, R
    , Constructor (conName, conIsRecord), Selector (selName)
    )

import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector

import Data.Ron.Value
import Prelude hiding (fail)


-- | In the future this might become more smart and report the error location.
-- In the present we don't even have the notion of "location" in RON. So this
-- sort-of MonadFail thing will do
type ParseResult = Either String
fail :: String -> ParseResult a
fail = Left

-- | Like '(<|>)' but collect errors from both sides
(<<|>>) :: ParseResult a -> ParseResult a -> ParseResult a
pa <<|>> pb = case pa of
    Right a -> pure a
    Left ea -> case pb of
        Right b -> pure b
        Left eb -> fail $ ea <> "; "<> eb

-- | A class of values that can be encoded to RON format
class ToRon a where
    toRon :: a -> Value
    default toRon :: (Generic a, GToRon (Rep a)) => a -> Value
    toRon = toRonDefault

-- | A class of values that can be from RON format
class FromRon a where
    fromRon :: Value -> ParseResult a
    default fromRon :: (Generic a, GFromRon (Rep a)) => Value -> ParseResult a
    fromRon = fromRonDefault

instance ToRon Int where
    toRon = Integral . fromIntegral
instance FromRon Int where
    fromRon (Integral i) = pure . fromIntegral $ i
    fromRon _ = fail "Not an integer"
instance ToRon Int8 where
    toRon = Integral . fromIntegral
instance FromRon Int8 where
    fromRon (Integral i) = pure . fromIntegral $ i
    fromRon _ = fail "Not an integer"
instance ToRon Int16 where
    toRon = Integral . fromIntegral
instance FromRon Int16 where
    fromRon (Integral i) = pure . fromIntegral $ i
    fromRon _ = fail "Not an integer"
instance ToRon Int32 where
    toRon = Integral . fromIntegral
instance FromRon Int32 where
    fromRon (Integral i) = pure . fromIntegral $ i
    fromRon _ = fail "Not an integer"
instance ToRon Int64 where
    toRon = Integral . fromIntegral
instance FromRon Int64 where
    fromRon (Integral i) = pure . fromIntegral $ i
    fromRon _ = fail "Not an integer"
instance ToRon Word where
    toRon = Integral . fromIntegral
instance FromRon Word where
    fromRon (Integral i) = pure . fromIntegral $ i
    fromRon _ = fail "Not an integer"
instance ToRon Word8 where
    toRon = Integral . fromIntegral
instance FromRon Word8 where
    fromRon (Integral i) = pure . fromIntegral $ i
    fromRon _ = fail "Not an integer"
instance ToRon Word16 where
    toRon = Integral . fromIntegral
instance FromRon Word16 where
    fromRon (Integral i) = pure . fromIntegral $ i
    fromRon _ = fail "Not an integer"
instance ToRon Word32 where
    toRon = Integral . fromIntegral
instance FromRon Word32 where
    fromRon (Integral i) = pure . fromIntegral $ i
    fromRon _ = fail "Not an integer"
instance ToRon Word64 where
    toRon = Integral . fromIntegral
instance FromRon Word64 where
    fromRon (Integral i) = pure . fromIntegral $ i
    fromRon _ = fail "Not an integer"
instance ToRon Integer where
    toRon = Integral
instance FromRon Integer where
    fromRon (Integral i) = pure i
    fromRon _ = fail "Not an integer"

instance ToRon Scientific where
    toRon = Floating
instance FromRon Scientific where
    fromRon (Floating x) = pure x
    fromRon _ = fail "Not a floating"
instance ToRon Double where
    toRon x
        | isNaN x = Unit "NaN"
        | isInfinite x = Unit "inf"
        | otherwise = Floating . fromFloatDigits $ x
instance FromRon Double where
    fromRon (Floating x) = pure . toRealFloat $ x
    fromRon (Unit "inf") = pure $! read "Infinity"
    fromRon (Unit "NaN") = pure $! read "NaN"
    fromRon _ = fail "Not a floating"
instance ToRon Float where
    toRon x
        | isNaN x = Unit "NaN"
        | isInfinite x = Unit "inf"
        | otherwise = Floating . fromFloatDigits $ x
instance FromRon Float where
    fromRon (Floating x) = pure . toRealFloat $ x
    fromRon (Unit "inf") = pure $! read "Infinity"
    fromRon (Unit "NaN") = pure $! read "NaN"
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
    fromRon (Map xs) = Map.fromList <$>
        traverse tupleFromRon (Map.toAscList xs)
        where tupleFromRon (a, b) = liftA2 (,) (fromRon a) (fromRon b)
    fromRon _ = fail "Not a map"

instance ToRon () where
    toRon () = Unit ""
instance FromRon () where
    fromRon (Unit name)
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


--- Generic instance

-- | Part of 'RonSettings' that applies to both encoding and decoding, and
-- separately
data RonFlags = RonFlags
    { implicitSome :: !Bool
    -- ^ Like ron-rs's @implicit_some@: 'Nothing' in record fields is
    -- represented by omission of the field
    , skipSingleConstructor :: !Bool
    -- ^ When a datatype has a single constructor, encoding will omit it and
    -- decoding will ignore it missing
    } deriving (Eq, Show)

-- | Settings for use with 'Generic' RON encoding/decoding
data RonSettings = RonSettings
    { fieldModifier :: !(String -> String)
    -- ^ Field renamer in RON representation
    , constructorModifier :: !(String -> String)
    , decodeFlags :: !RonFlags
    , encodeFlags :: !RonFlags
    }

newtype Context = Context
    { isSingleConstructor :: Bool
    } deriving (Eq, Show)

-- | Encode ron using 'Generic' instance and provided 'RonSettings'
toRonGeneric :: (Generic a, GToRon (Rep a)) => RonSettings -> a -> Value
toRonGeneric conf = toRonG conf . from

-- | Values are expected to exactly conform: all fields should have the same
-- name, all constructors should be present, no @Some@ omission
strictRonSettings :: RonSettings
strictRonSettings = RonSettings
    { fieldModifier = id
    , constructorModifier = id
    , decodeFlags = RonFlags
        { implicitSome = False
        , skipSingleConstructor = False
        }
    , encodeFlags = RonFlags
        { implicitSome = False
        , skipSingleConstructor = False
        }
    }

-- | Relaxes 'strictRonSettings' on constructor omission and implicitSome when
-- decoding, but encodes in the same strict way
laxRonSettings :: RonSettings
laxRonSettings = RonSettings
    { fieldModifier = id
    , constructorModifier = id
    , decodeFlags = RonFlags
        { implicitSome = True
        , skipSingleConstructor = True
        }
    , encodeFlags = RonFlags
        { implicitSome = False
        , skipSingleConstructor = False
        }
    }

toRonDefault :: (Generic a, GToRon (Rep a)) => a -> Value
toRonDefault = toRonGeneric laxRonSettings

-- | Internal class for converting to Ron. You might need it if you're writing
-- you own generic combinators
class GToRon f where
    toRonG :: RonSettings -> f a -> Value

class GToRonSum f where
    toRonSum :: RonSettings -> Context -> f a -> Value
class GToRonProduct f where
    toRonProduct
        :: RonSettings -> f a -> Either (Vector Value) (Map Text Value)
class GToRonRec f where
    toRonRec :: RonSettings -> f a -> Value

instance GToRonSum f => GToRon (M1 D _d f) where
    toRonG conf (M1 x) = toRonSum conf cont x
        where cont = Context
                { isSingleConstructor = True
                }

instance GToRonSum V1 where
    toRonSum _ _ x = case x of {}

instance (Constructor c, GToRonProduct f) => GToRonSum (M1 C c f) where
    toRonSum conf cont (M1 x) =
        let con = undefined :: t c f a
            name = if isSingleConstructor cont
                        && skipSingleConstructor (encodeFlags conf)
                    then ""
                    else pack . conName $ con
            xs = toRonProduct conf x
        in case (xs, conIsRecord con) of
            (Right xs', True) -> Record name xs'
            (Left xs', False) -> Tuple name xs'
            (Right xs', _) | null xs' -> Unit name
            (Left xs', _) | Vector.null xs' -> Unit name
            _ -> error $ "Bad product: " <> take 128 (show xs)

instance (GToRonSum fl, GToRonSum fr) => GToRonSum (fl :+: fr) where
    toRonSum conf cont (L1 x) = toRonSum conf cont' x
        where cont' = cont
                {isSingleConstructor = False}
    toRonSum conf cont (R1 x) = toRonSum conf cont' x
        where cont' = cont
                {isSingleConstructor = False}

instance GToRonProduct U1 where
    toRonProduct _ U1 = Left Vector.empty

instance {-# OVERLAPPING #-} (Selector s, ToRon c)
    => GToRonProduct (M1 S s (K1 R (Maybe c))) where
    toRonProduct conf (M1 (K1 x)) =
        let field = selName (undefined :: t s (K1 R (Maybe c)) a)
        in case x of
            Nothing | implicitSome . encodeFlags $ conf ->
                if null field
                    then Left . Vector.singleton . toRon $ Nothing @()
                    else Right Map.empty
            Just x' | implicitSome . encodeFlags $ conf ->
                if null field
                    then Left . Vector.singleton . toRon $ x'
                    else
                        let field' = pack $ fieldModifier conf field
                            value = toRon x'
                        in Right $ Map.singleton field' value
            x'
                | null field -> Left . Vector.singleton . toRon $ x'
                | otherwise ->
                    let field' = pack $ fieldModifier conf field
                    in Right . Map.singleton field' $ toRon x'

instance (Selector s, GToRonRec f) => GToRonProduct (M1 S s f) where
    toRonProduct conf (M1 x) =
        let field = selName (undefined :: t s f a)
            value = toRonRec conf x
        in case field of
            "" -> Left . Vector.singleton $ value
            _field ->
                let field' = pack $ fieldModifier conf field
                in Right $ Map.singleton field' value

instance (GToRonProduct pl, GToRonProduct pr)
    => GToRonProduct (pl :*: pr) where
    toRonProduct conf (x :*: y) =
        case (toRonProduct conf x, toRonProduct conf y) of
            (Left xs, Left ys) -> Left $ xs <> ys
            (Right xs, Right ys) -> Right $ Map.union xs ys
            _ -> error "Incompatible product branches"

instance ToRon c => GToRonRec (K1 R c) where
    toRonRec _ (K1 x) = toRon x


-- | Decode ron using 'Generic' instance and provided 'RonSettings'
fromRonGeneric
    :: (Generic a, GFromRon (Rep a)) => RonSettings -> Value -> ParseResult a
fromRonGeneric conf = fmap to . fromRonG conf

fromRonDefault :: (Generic a, GFromRon (Rep a)) => Value -> ParseResult a
fromRonDefault = fromRonGeneric laxRonSettings

-- | Internal class for converting from Ron. You might need it if you're
-- writing you own generic combinators
class GFromRon f where
    fromRonG :: RonSettings -> Value -> ParseResult (f a)

class GFromRonSum f where
    fromRonSum :: RonSettings -> Context -> Value -> ParseResult (f a)
class GFromRonProduct f where
    fromRonProduct
        :: RonSettings
        -> Either (Vector Value) (Map Text Value)
        -> ParseResult (f a)
class GFromRonRec f where
    fromRonRec :: RonSettings -> Value -> ParseResult (f a)

instance GFromRonSum f => GFromRon (M1 D _d f) where
    fromRonG conf x = M1 <$> fromRonSum conf cont x
        where cont = Context
                { isSingleConstructor = True
                }

instance (Constructor c, GFromRonProduct f) => GFromRonSum (M1 C c f) where
    fromRonSum conf cont x =
        let con = undefined :: t c f a
            name = pack . conName $ con
        in M1 <$> case x of
            Unit n | name `matches` n
                        -> fromRonProduct conf $ Left Vector.empty
                   | otherwise -> fail "Incorrect name"
            Tuple n xs | name `matches` n -> fromRonProduct conf $ Left xs
                       | otherwise -> fail "Incorrect name"
            Record n xs | name `matches` n -> fromRonProduct conf $ Right xs
                        | otherwise -> fail "Incorrect name"
            _ -> fail "Incorrect value type"
        where
            matches target ron =
                ron == target
                || ron == ""
                    && isSingleConstructor cont
                    && skipSingleConstructor (decodeFlags conf)

instance (GFromRonSum fl, GFromRonSum fr) => GFromRonSum (fl :+: fr) where
    fromRonSum conf cont x =
        (L1 <$> fromRonSum conf cont' x) <<|>> (R1 <$> fromRonSum conf cont' x)
        where cont' = cont
                { isSingleConstructor = False }

instance GFromRonProduct U1 where
    fromRonProduct _ (Left xs) | Vector.null xs  = pure U1
    fromRonProduct _ (Right xs) | Map.null xs  = pure U1
    fromRonProduct _ _ = fail "Expected empty structure"

instance {-# OVERLAPPING #-} (Selector s, FromRon c)
    => GFromRonProduct (M1 S s (K1 R (Maybe c))) where
    fromRonProduct conf xs =
        let field =
                pack . fieldModifier conf $
                    selName (undefined :: t s (K1 R (Maybe c)) a)
        in case xs of
            Left xs' -> case Vector.uncons xs' of
                Nothing -> fail "Not enough elements in tuple"
                Just (x, xs'')
                    | Vector.null xs'' -> M1 <$>
                        let tryUnwrapped
                                | implicitSome . decodeFlags $ conf =
                                    K1 . Just <$> fromRon x
                                | otherwise =
                                    fail "Not using decodeImplicitSome"
                        in fromRonRec conf x <<|>> tryUnwrapped
                    | otherwise -> fail "Trailing members in tuple"
            Right xs'
                | implicitSome . decodeFlags $ conf ->
                    case Map.lookup field xs' of
                        Nothing -> pure . M1 . K1 $ Nothing
                        Just x ->
                            let unwrapped = M1 . K1 . Just <$> fromRon x
                                wrapped = M1 <$> fromRonRec conf x
                            in wrapped <<|>> unwrapped
                | otherwise -> case Map.lookup field xs' of
                    Nothing -> fail "Field not present in record"
                    Just x -> M1 <$> fromRonRec conf x

instance (Selector s, GFromRonRec f) => GFromRonProduct (M1 S s f) where
    fromRonProduct conf xs =
        let field = pack . fieldModifier conf $ selName (undefined :: t s f a)
        in case xs of
            Left xs' -> case Vector.uncons xs' of
                Nothing -> fail "Not enough elements in tuple"
                Just (x, xs'') | Vector.null xs'' -> M1 <$> fromRonRec conf x
                               | otherwise -> fail "Trailing members in tuple"
            Right xs' -> case Map.lookup field xs' of
                Nothing -> fail "Field not present in record"
                Just x -> M1 <$> fromRonRec conf x

instance (ProductSize pl, GFromRonProduct pl, GFromRonProduct pr)
    => GFromRonProduct (pl :*: pr) where
    fromRonProduct conf (Left xs) =
        let sizel = productSize (Proxy @pl)
            (xsl, xsr) = Vector.splitAt sizel xs
        in (:*:)
            <$> fromRonProduct conf (Left xsl)
            <*> fromRonProduct conf (Left xsr)
    fromRonProduct conf xs = (:*:)
        <$> fromRonProduct conf xs
        <*> fromRonProduct conf xs

instance FromRon c => GFromRonRec (K1 R c) where
    fromRonRec _ x = K1 <$> fromRon x
