{-# LANGUAGE DefaultSignatures, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE EmptyCase #-}

-- | Definition for ron conversion classes, and for using 'GHC.Generics' to
-- encode any datatype into RON.
module Data.Ron.Class
    ( ToRon (..), FromRon (..)
    , ParseResult
    -- * Settings for generic encoding
    , RonSettings (..)
    , RonFlags (..)
    , strictRonSettings, laxRonSettings
    -- * Generic encoding
    , toRonGeneric
    , fromRonGeneric
    , GToRon, GFromRon
    ) where

import Control.Applicative (liftA2)
import Control.Arrow ((***))
import Data.ByteString (ByteString)
import Data.Complex (Complex ((:+)))
import Data.Foldable (toList)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import Data.Proxy (Proxy (..))
import Data.Ron.Class.Internal (productSize, ProductSize)
import Data.Scientific (fromFloatDigits, toRealFloat, Scientific)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (Vector)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics
    ( Generic (Rep, from, to), V1, U1 (..), (:+:)(..), (:*:)(..)
    , K1 (..), M1 (..)
    , C, S, D, R
    , Constructor (conName, conIsRecord), Selector (selName), Datatype (datatypeName)
    )

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Vector as Vector

import Data.Ron.Value
import Prelude hiding (fail)


-- | When decoding from ron, this type is used to indicate decode failure.
-- During decoding, multiple failures may be concatenated with a semicolon, and
-- for some functions the string will be thrown with
-- 'Data.Ron.Deserialize.DecodeError'. When implementing your own decoding
-- functions, you should put short one-sentence error descriptions.
--
-- For the next major release, we plan to replace it with a better mechanism
-- that reports error locations and supports long-form content.
type ParseResult = Either String
fail :: String -> ParseResult a
fail = Left

guard :: String -> Bool -> ParseResult ()
guard _ True = pure ()
guard msg False = fail msg

-- | Like 'Control.Applicative.(<|>)' but collect errors from both sides
(<<|>>) :: ParseResult a -> ParseResult a -> ParseResult a
pa <<|>> pb = case pa of
    Right a -> pure a
    Left ea -> case pb of
        Right b -> pure b
        Left eb -> fail $ ea <> "; "<> eb

-- | A class of values that can be encoded to RON format.
--
-- There are several ways to define an instance:
--
--   1. By producing a 'Value' by hand
--   2. By using 'toRonGeneric'
--   3. By @DerivingVia@ extension and using 'Data.Ron.Class.Deriving.RonWith'
--
-- When using the second option, the encoding parameters are specified with
-- 'RonSettings'. With the third option, the same parameters are specified by a
-- list of settings found in 'Data.Ron.Class.Deriving'.
--
-- The default implementation uses generic encoding with 'laxRonSettings'. You
-- can use other settings like this:
--
-- @
--      instance ToRon MyType where
--          toRon = toRonGeneric strictRonSettings
--              { encodeFlags = RonFlags
--                  { implicitSome = True
--                  , skipSingleConstructor = True
--                  }
--              }
-- @
--
-- Or like this:
--
-- @
--      deriving via (RonWith '[UseStrict, EncodeWith SkipSingleConstructor, EncodeWith ImplicitSome])
--          instance ToRon MyType
-- @
class ToRon a where
    toRon :: a -> Value
    default toRon :: (Generic a, GToRon (Rep a)) => a -> Value
    toRon = toRonDefault

-- | A class of values that can be restored from RON format
--
-- There are several ways to define an instance:
--
--   1. By deconstructing a 'Value' by hand and producing a value of your type
--   2. By using 'fromRonGeneric'
--   3. By @DerivingVia@ extension and using 'Data.Ron.Class.Deriving.RonWith'
--
-- The default implementation uses generic decoding with 'laxRonSettings'. You
-- can use other settings like this:
--
-- @
--      instance FromRon MyType where
--          fromRon = fromRonGeneric strictRonSettings
--              { decodeFlags = RonFlags
--                  { implicitSome = True
--                  , skipSingleConstructor = True
--                  }
--              }
-- @
--
-- Or like this:
--
-- @
--      deriving via (RonWith '[UseStrict, DecodeWith SkipSingleConstructor, DecodeWith ImplicitSome])
--          instance FromRon MyType
-- @
class FromRon a where
    fromRon :: Value -> ParseResult a
    default fromRon :: (Generic a, GFromRon (Rep a)) => Value -> ParseResult a
    fromRon = fromRonDefault

intInRange :: forall a. (Bounded a, Num a, Integral a) => Integer -> ParseResult a
intInRange i
    | i >= minVal && i <= maxVal  = pure . fromInteger $ i
    | otherwise  = fail $ "Value " <> show i <> " is not in valid range"
    where
        minVal = toInteger (minBound @a)
        maxVal = toInteger (maxBound @a)

instance ToRon Int where
    toRon = Integral . toInteger
instance FromRon Int where
    fromRon (Integral i) = intInRange i
    fromRon _ = fail "Not an integer"
instance ToRon Int8 where
    toRon = Integral . toInteger
instance FromRon Int8 where
    fromRon (Integral i) = intInRange i
    fromRon _ = fail "Not an integer"
instance ToRon Int16 where
    toRon = Integral . toInteger
instance FromRon Int16 where
    fromRon (Integral i) = intInRange i
    fromRon _ = fail "Not an integer"
instance ToRon Int32 where
    toRon = Integral . toInteger
instance FromRon Int32 where
    fromRon (Integral i) = intInRange i
    fromRon _ = fail "Not an integer"
instance ToRon Int64 where
    toRon = Integral . toInteger
instance FromRon Int64 where
    fromRon (Integral i) = intInRange i
    fromRon _ = fail "Not an integer"
instance ToRon Word where
    toRon = Integral . toInteger
instance FromRon Word where
    fromRon (Integral i) = intInRange i
    fromRon _ = fail "Not an integer"
instance ToRon Word8 where
    toRon = Integral . toInteger
instance FromRon Word8 where
    fromRon (Integral i) = intInRange i
    fromRon _ = fail "Not an integer"
instance ToRon Word16 where
    toRon = Integral . toInteger
instance FromRon Word16 where
    fromRon (Integral i) = intInRange i
    fromRon _ = fail "Not an integer"
instance ToRon Word32 where
    toRon = Integral . toInteger
instance FromRon Word32 where
    fromRon (Integral i) = intInRange i
    fromRon _ = fail "Not an integer"
instance ToRon Word64 where
    toRon = Integral . toInteger
instance FromRon Word64 where
    fromRon (Integral i) = intInRange i
    fromRon _ = fail "Not an integer"
instance ToRon Integer where
    toRon = Integral
instance FromRon Integer where
    fromRon (Integral i) = pure i
    fromRon _ = fail "Not an integer"

instance ToRon Scientific where
    toRon = Floating
instance FromRon Scientific where
    fromRon (Integral x) = pure . fromInteger $ x
    fromRon (Floating x) = pure x
    fromRon _ = fail "Not a floating"
instance ToRon Double where
    toRon x
        | isNaN x = Unit "NaN"
        | isInfinite x = Unit "inf"
        | otherwise = Floating . fromFloatDigits $ x
instance FromRon Double where
    fromRon (Integral x) = pure . fromInteger $ x
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
    fromRon (Integral x) = pure . fromInteger $ x
    fromRon (Floating x) = pure . toRealFloat $ x
    fromRon (Unit "inf") = pure $! read "Infinity"
    fromRon (Unit "NaN") = pure $! read "NaN"
    fromRon _ = fail "Not a floating"

instance (ToRon a) => ToRon (Complex a) where
    toRon (r :+ im) = Tuple "Complex" (Vector.fromList [toRon r, toRon im])
instance (Num a, FromRon a) => FromRon (Complex a) where
    fromRon v@(Integral _) = (:+ 0) <$> fromRon v
    fromRon v@(Floating _) = (:+ 0) <$> fromRon v
    fromRon (Tuple name vals) = do
        guard "Incorrect Complex tuple name" $ name == "" || name == "Complex"
        case Vector.toList vals of
            [r, im] -> liftA2 (:+) (fromRon r) (fromRon im)
            _ -> fail "Incorrect amount of values in complex coordinates"
    fromRon _ = fail "Incorrect Complex value"

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

instance ToRon ByteString where
    toRon = List . Vector.fromList . map (Integral . toInteger) . BS.unpack
instance FromRon ByteString where
    fromRon (List bytes) = fmap BS.pack . traverse asWord8 . Vector.toList $ bytes
        where
            asWord8 (Integral x) = intInRange x
            asWord8 _ = fail "Not a byte in list"
    fromRon (String s) = pure . encodeUtf8 $ s
    fromRon _ = fail "Not a bytestring"

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
instance {-# OVERLAPPING #-} ToRon [Char] where
    toRon = String . pack
instance {-# OVERLAPPING #-} FromRon [Char] where
    fromRon = fmap unpack . fromRon
instance (ToRon a) => ToRon (NonEmpty a) where
    toRon (x:|xs) = toRon $ x:xs
instance (FromRon a) => FromRon (NonEmpty a) where
    fromRon v = fromRon v >>= \case
        [] -> fail "Expected non-empty list"
        x:xs -> pure $ x:|xs

instance (ToRon k, ToRon v) => ToRon (Map.Map k v) where
    toRon = Map . Map.fromList . map (toRon *** toRon) . Map.toAscList
instance (FromRon k, FromRon v, Ord k) => FromRon (Map.Map k v) where
    fromRon (Map xs) = Map.fromList <$>
        traverse tupleFromRon (Map.toAscList xs)
        where tupleFromRon (a, b) = liftA2 (,) (fromRon a) (fromRon b)
    fromRon _ = fail "Not a map"

instance (ToRon a) => ToRon (Set.Set a) where
    toRon = toRon . Set.toAscList
instance (FromRon a, Ord a) => FromRon (Set.Set a) where
    fromRon = fmap Set.fromList . fromRon
instance (ToRon a) => ToRon (Seq.Seq a) where
    toRon = toRon . toList
instance (FromRon a) => FromRon (Seq.Seq a) where
    fromRon = fmap Seq.fromList . fromRon

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
    -- ^ Like ron-rs's @implicit_some@. When set to True, 'Nothing' in record
    -- fields is represented by omission of the field
    , skipSingleConstructor :: !Bool
    -- ^ When a datatype has a single constructor, encoding will omit it and
    -- decoding will ignore it missing, turning the representation into a tuple
    -- or an anonymous record.
    } deriving (Eq, Show)

-- | Settings for use with 'Generic' RON encoding/decoding
data RonSettings = RonSettings
    { fieldModifier :: !(String -> String)
    -- ^ Every field will be renamed using the supplied function
    , constructorModifier :: !(String -> String)
    -- ^ Every constructor will be renamed using the supplied function
    , decodeFlags :: !RonFlags
    -- ^ Options that apply to encoding separately from decoding
    , encodeFlags :: !RonFlags
    -- ^ Options that apply to decoding separately from encoding
    }

data SumContext = SumContext
    { isSingleConstructor :: Bool
    , sumTypeName :: String
    } deriving (Eq, Show)

data ProductContext = ProductContext
    { prodTypeName :: String
    , constructorName :: String
    } deriving (Eq, Show)

-- | Encode ron using 'Generic' instance and provided 'RonSettings'.
--
-- With generic encoding sums are turned into sums, records into records, and
-- multi-param constructors into tuples.
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
    toRonSum :: RonSettings -> SumContext -> f a -> Value
class GToRonProduct f where
    toRonProduct
        :: RonSettings -> ProductContext -> f a -> Either (Vector Value) (Map Text Value)
class GToRonRec f where
    toRonRec :: RonSettings -> f a -> Value

instance (Datatype d, GToRonSum f) => GToRon (M1 D d f) where
    toRonG conf (M1 x) = toRonSum conf cont x
        where cont = SumContext
                { isSingleConstructor = True
                , sumTypeName = datatypeName (undefined :: t d f a)
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
            cont' = ProductContext
                { prodTypeName = sumTypeName cont
                , constructorName = unpack name
                }
            xs = toRonProduct conf cont' x
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
    toRonProduct _ _ U1 = Left Vector.empty

instance {-# OVERLAPPING #-} (Selector s, ToRon c)
    => GToRonProduct (M1 S s (K1 R (Maybe c))) where
    toRonProduct conf _cont (M1 (K1 x)) =
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
    toRonProduct conf _cont (M1 x) =
        let field = selName (undefined :: t s f a)
            value = toRonRec conf x
        in case field of
            "" -> Left . Vector.singleton $ value
            _field ->
                let field' = pack $ fieldModifier conf field
                in Right $ Map.singleton field' value

instance (GToRonProduct pl, GToRonProduct pr)
    => GToRonProduct (pl :*: pr) where
    toRonProduct conf cont (x :*: y) =
        case (toRonProduct conf cont x, toRonProduct conf cont y) of
            (Left xs, Left ys) -> Left $ xs <> ys
            (Right xs, Right ys) -> Right $ Map.union xs ys
            _ -> error "Incompatible product branches"

instance ToRon c => GToRonRec (K1 R c) where
    toRonRec _ (K1 x) = toRon x


-- | Decode ron using 'Generic' instance and provided 'RonSettings'
--
-- With generic encoding sums are turned into sums, records into records, and
-- multi-param constructors into tuples.
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
    fromRonSum :: RonSettings -> SumContext -> Value -> ParseResult (f a)
class GFromRonProduct f where
    fromRonProduct
        :: RonSettings
        -> ProductContext
        -> Either (Vector Value) (Map Text Value)
        -> ParseResult (f a)
class GFromRonRec f where
    fromRonRec :: RonSettings -> Value -> ParseResult (f a)

instance (Datatype d, GFromRonSum f) => GFromRon (M1 D d f) where
    fromRonG conf x = M1 <$> fromRonSum conf cont x
        where cont = SumContext
                { isSingleConstructor = True
                , sumTypeName = datatypeName (undefined :: t d f a)
                }

instance (Constructor c, GFromRonProduct f) => GFromRonSum (M1 C c f) where
    fromRonSum conf cont x =
        M1 <$> case x of
            Unit n | name `matches` n
                        -> fromRonProduct conf cont' $ Left Vector.empty
                   | otherwise -> fail incorrectConstructor
            Tuple n xs | name `matches` n -> fromRonProduct conf cont' $ Left xs
                       | otherwise -> fail incorrectConstructor
            Record n xs | name `matches` n -> fromRonProduct conf cont' $ Right xs
                        | otherwise -> fail incorrectConstructor
            _ -> fail $ "Incorrect value for type " <> sumTypeName cont
        where
            con = undefined :: t c f a
            name = pack . conName $ con
            matches target ron =
                ron == target
                || ron == ""
                    && isSingleConstructor cont
                    && skipSingleConstructor (decodeFlags conf)
            incorrectConstructor = "Incorrect constructor " <> constructorName <> " for type " <> sumTypeName cont
            constructorName = case unpack name of
                "" -> "<UNNAMED>"
                n -> n
            cont' = ProductContext
                { prodTypeName = sumTypeName cont
                , constructorName
                }

instance (GFromRonSum fl, GFromRonSum fr) => GFromRonSum (fl :+: fr) where
    fromRonSum conf cont x =
        (L1 <$> fromRonSum conf cont' x) <<|>> (R1 <$> fromRonSum conf cont' x)
        where cont' = cont
                { isSingleConstructor = False }

instance GFromRonProduct U1 where
    fromRonProduct _ _ (Left xs) | Vector.null xs  = pure U1
    fromRonProduct _ _ (Right xs) | Map.null xs  = pure U1
    fromRonProduct _ c _ = fail $ constructorName c <> ": expected empty structure"

instance {-# OVERLAPPING #-} (Selector s, FromRon c)
    => GFromRonProduct (M1 S s (K1 R (Maybe c))) where
    fromRonProduct conf cont xs =
        let field =
                pack . fieldModifier conf $
                    selName (undefined :: t s (K1 R (Maybe c)) a)
            ProductContext {constructorName} = cont
        in case xs of
            Left xs' -> case Vector.uncons xs' of
                Nothing -> fail $ "Not enough elements in tuple " <> constructorName
                Just (x, xs'')
                    | Vector.null xs'' -> M1 <$>
                        let tryUnwrapped
                                | implicitSome . decodeFlags $ conf =
                                    K1 . Just <$> fromRon x
                                | otherwise =
                                    fail $ constructorName <> ": not using decodeImplicitSome"
                        in fromRonRec conf x <<|>> tryUnwrapped
                    | otherwise -> fail $ "Trailing members in tuple " <> constructorName
            Right xs'
                | implicitSome . decodeFlags $ conf ->
                    case Map.lookup field xs' of
                        Nothing -> pure . M1 . K1 $ Nothing
                        Just x ->
                            let unwrapped = M1 . K1 . Just <$> fromRon x
                                wrapped = M1 <$> fromRonRec conf x
                            in wrapped <<|>> unwrapped
                | otherwise -> case Map.lookup field xs' of
                    Nothing -> fail $ "Field " <> unpack field <> " not present in record " <> constructorName
                    Just x -> M1 <$> fromRonRec conf x

instance (Selector s, GFromRonRec f) => GFromRonProduct (M1 S s f) where
    fromRonProduct conf cont xs =
        let field = pack . fieldModifier conf $ selName (undefined :: t s f a)
            ProductContext {constructorName} = cont
        in case xs of
            Left xs' -> case Vector.uncons xs' of
                Nothing -> fail $ "Not enough elements in tuple " <> constructorName
                Just (x, xs'') | Vector.null xs'' -> M1 <$> fromRonRec conf x
                               | otherwise -> fail $ "Trailing members in tuple " <> constructorName
            Right xs' -> case Map.lookup field xs' of
                Nothing -> fail $ "Field " <> unpack field <> " not present in record " <> constructorName
                Just x -> M1 <$> fromRonRec conf x

instance (ProductSize pl, GFromRonProduct pl, GFromRonProduct pr)
    => GFromRonProduct (pl :*: pr) where
    fromRonProduct conf cont (Left xs) =
        let sizel = productSize (Proxy @pl)
            (xsl, xsr) = Vector.splitAt sizel xs
        in (:*:)
            <$> fromRonProduct conf cont (Left xsl)
            <*> fromRonProduct conf cont (Left xsr)
    fromRonProduct conf cont xs = (:*:)
        <$> fromRonProduct conf cont xs
        <*> fromRonProduct conf cont xs

instance FromRon c => GFromRonRec (K1 R c) where
    fromRonRec _ x = K1 <$> fromRon x
