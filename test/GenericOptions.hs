{-# LANGUAGE DeriveGeneric, OverloadedLists #-}
{-# LANGUAGE OverloadedLabels #-}
module GenericOptions
    ( genericOptionsTests
    ) where

import GHC.Generics (Generic)
import Data.Map.Strict (Map)
import Data.Ron.Class (ToRon, FromRon, toRonGeneric, fromRonGeneric, RonSettings (..), strictRonSettings)
import Data.Function ((&))
import Data.Ron.Value (Value (..))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word8)
import Optics.Label ()
import Optics.Ron.Settings ()
import Optics.Operators ((.~))
import Optics.Optic ((%))
import Test.Tasty.HUnit (testCase, (@?=), (@=?))
import Test.Tasty (testGroup)

data MaybeFields = MaybeFields
    { f1 :: Int
    , f2 :: Maybe Int
    , f3 :: Maybe (Maybe Int)
    , rec :: Maybe MaybeFields
    } deriving (Eq, Show, Generic)

instance ToRon MaybeFields
instance FromRon MaybeFields

maybeFieldsNone = MaybeFields
    { f1 = 1
    , f2 = Nothing
    , f3 = Nothing
    , rec = Nothing
    }
maybeFieldsNoneRon = Record "MaybeFields"
    [ ("f1", Integral 1)
    ]

maybeFieldsSome = MaybeFields
    { f1 = 1
    , f2 = Just 2
    , f3 = Just $ Just 3
    , rec = Just MaybeFields
        { f1 = 11
        , f2 = Nothing
        , f3 = Nothing
        , rec = Nothing
        }
    }
maybeFieldsSomeRon = Record "MaybeFields"
    [ ("f1", Integral 1)
    , ("f2", Integral 2)
    , ("f3", Tuple "Some" [Integral 3])
    , ("rec", Record "MaybeFields"
        [ ("f1", Integral 11)
        , ("f2", Unit "None")
        , ("f3", Unit "None")
        , ("rec", Unit "None")
        ]
      )
    ]

encodeOptionalFieldsTests = testGroup "optional field encoding"
    [ testCase "all none" $
        enc maybeFieldsNone @?= maybeFieldsNoneRon
    , testCase "all present" $
        enc maybeFieldsSome @?= maybeFieldsSomeRon
    ]
    where
        enc = toRonGeneric settings
        settings = strictRonSettings &
            #encodeFlags % #implicitSome .~ True

decodeOptionalFieldsTests = testGroup "optional field decoding"
    [ testCase "all none" $
        Right maybeFieldsNone @=? dec maybeFieldsNoneRon
    , testCase "all none as None" $
        Right maybeFieldsNone @=? dec (Record "MaybeFields"
            [ ("f1", Integral 1)
            , ("f2", Unit "None")
            , ("f3", Unit "None")
            , ("rec", Unit "None")
            ]
        )
    , testCase "all present" $
        Right maybeFieldsSome @=? dec maybeFieldsSomeRon
    , testCase "all present as Some" $
        Right maybeFieldsSome @=? dec (Record "MaybeFields"
            [ ("f1", Integral 1)
            , ("f2", Tuple "Some" [Integral 2])
            , ("f3", Tuple "Some" [Tuple "Some" [Integral 3]])
            , ("rec", Tuple "Some" [Record "MaybeFields"
                [ ("f1", Integral 11)
                , ("f2", Unit "None")
                , ("f3", Unit "None")
                , ("rec", Unit "None")
                ]]
              )
            ]
        )
    ]
    where
        dec = fromRonGeneric settings
        settings = strictRonSettings &
            #decodeFlags % #implicitSome .~ True

data MaybeTuple = MaybeTuple
    Int
    (Maybe Int)
    (Maybe (Maybe Int))
    (Maybe MaybeTuple)
    deriving (Eq, Show, Generic)

instance ToRon MaybeTuple
instance FromRon MaybeTuple

maybeTupleNone = MaybeTuple
    1
    Nothing
    Nothing
    Nothing
maybeTupleNoneRon = Tuple "MaybeTuple"
    [ Integral 1
    , Unit "None"
    , Unit "None"
    , Unit "None"
    ]

maybeTupleSome = MaybeTuple
    1
    (Just 2)
    (Just $ Just 3)
    (Just $ MaybeTuple 1 Nothing Nothing Nothing)
maybeTupleSomeRon = Tuple "MaybeTuple"
    [ Integral 1
    , Integral 2
    , Tuple "Some" [Integral 3]
    , Tuple "MaybeTuple"
        [ Integral 1
        , Unit "None"
        , Unit "None"
        , Unit "None"
        ]
    ]

encodeOptionalTupleFieldsTests = testGroup "optional fields in tuple encoding"
    [ testCase "all none" $
        enc maybeTupleNone @?= maybeTupleNoneRon

    , testCase "all present" $
        enc maybeTupleSome @?= maybeTupleSomeRon
    ]
    where
        enc = toRonGeneric settings
        settings = strictRonSettings &
            #encodeFlags % #implicitSome .~ True

decodeOptionalTupleFieldsTests = testGroup "optional field in tuple decoding"
    [ testCase "all none" $
        Right maybeTupleNone @=? dec maybeTupleNoneRon
    , testCase "all present" $
        Right maybeTupleSome @=? dec maybeTupleSomeRon
    , testCase "all present as Some" $
        Right maybeTupleSome @=? dec (Tuple "MaybeTuple"
            [ Integral 1
            , Tuple "Some" [Integral 2]
            , Tuple "Some" [Tuple "Some" [Integral 3]]
            , Tuple "Some" [Tuple "MaybeTuple"
                [ Integral 1
                , Unit "None"
                , Unit "None"
                , Unit "None"
                ]]
            ]
        )
    ]
    where
        dec = fromRonGeneric settings
        settings = strictRonSettings &
            #decodeFlags % #implicitSome .~ True

singleConstructorTests = testGroup "omitting single constructor"
    [ testCase "record encoding" $
        toRonGeneric se maybeFieldsNone @?= Record "" [ ("f1", Integral 1) ]
    , testCase "tuple encoding" $
        toRonGeneric se maybeTupleNone @?= Tuple ""
            [ Integral 1, Unit "None", Unit "None", Unit "None" ]
    , testCase "record decoding" $
        Right maybeFieldsNone @=? fromRonGeneric sd
            (Record "" [ ("f1", Integral 1) ])
    , testCase "tuple decoding" $
        Right maybeTupleNone @=? fromRonGeneric sd
            (Tuple "" [ Integral 1, Unit "None", Unit "None", Unit "None" ])
    ]
    where
        se = strictRonSettings
            & ( #encodeFlags % #skipSingleConstructor .~ True )
            . ( #encodeFlags % #implicitSome .~ True )
        sd = strictRonSettings
            & ( #decodeFlags % #skipSingleConstructor .~ True )
            . ( #decodeFlags % #implicitSome .~ True )

genericOptionsTests = testGroup "generic options"
    [ encodeOptionalFieldsTests
    , decodeOptionalFieldsTests
    , encodeOptionalTupleFieldsTests
    , decodeOptionalTupleFieldsTests
    , singleConstructorTests
    ]
