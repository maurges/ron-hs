{-# LANGUAGE OverloadedLists #-}
module ForExample
    ( exampleTests
    ) where

import Data.Ron.Deserialize (loads)
import Data.Ron.Value (Value (..))
import Data.ByteString.Char8 (ByteString, unlines)
import Data.Text.Encoding (encodeUtf8)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty (testGroup)

import Prelude hiding (unlines)

example1, example2, example3 :: ByteString
example1_ron, example2_ron, example3_ron :: Value

example1 = unlines . map encodeUtf8 $
    [ "// RON supports comments"
    , "/* even multiline comments */"
    , "/* even /* embedded /* multiline */ comments */ /* */ */"
    , "TopLevelRecord ("
    , "    field: Compound (\"this\", \"is\", \"a\", \"compound\", 123),"

    , "    all_datatypes: ("
    , "        int: 1_337_322_228,"
    , "        float: 2.71828,"
    , "        char: 'ы',"
    , "        string: \"hello string\n\","
    , "        raw_string: r##\"raw \"string\", still unicode\"##,"
    , "        list: [\"homogenous\", \"elements\"],"
    , "        map: {"
    , "            \"value\": \"as keys\","
    , "            \"also\": \"homogenous\","
    , "        },"
    , "        tuple: SomeTuple(0, OtherTuple(\"with\", \"stuff\")),"
    , "        record: SomeRecord ("
    , "            like: \"this one you're looking at\","
    , "            not_homogenous: 2022,"
    , "        ),"
    , "        unit_values: ["
    , "            (), // regular unit"
    , "            NamedUnit,"
    , "            NamedUnitWithBraces(),"
    , "        ]"
    , "    )"
    , ")"
    ]

example2 = unlines
    [ "\"ron-rs document\","
    , "\"with a toplevel list\","
    , "\"denoted by just elements with commas\","
    ]

example3 = unlines
    [ "field1: SomeValue,"
    , "field2: \"other value\""
    ]

example1_ron = Record "TopLevelRecord"
    [ ( "field", Tuple "Compound"
        [ String "this"
        , String "is"
        , String "a"
        , String "compound"
        , Integral 123
        ]
      )
    , ("all_datatypes", Record ""
        [ ("int", Integral 1337322228)
        , ("float", Floating 2.71828)
        , ("char", Char 'ы')
        , ("string", String "hello string\n")
        , ("raw_string", String "raw \"string\", still unicode")
        , ("list", List [String "homogenous", String "elements"])
        , ( "map", Map
            [ (String "value", String "as keys")
            , (String "also", String "homogenous")
            ]
          )
        , ("tuple", Tuple "SomeTuple"
            [ Integral 0, Tuple "OtherTuple" [String "with", String "stuff"] ]
          )
        , ("record", Record "SomeRecord"
            [ ("like", String "this one you're looking at")
            , ("not_homogenous", Integral 2022)
            ]
          )
        , ("unit_values", List [Unit "", Unit "NamedUnit", Unit "NamedUnitWithBraces"])
        ]
      )
    ]

example2_ron = List
    [ String "ron-rs document"
    , String "with a toplevel list"
    , String "denoted by just elements with commas"
    ]

example3_ron = Record ""
    [ ("field1", Unit "SomeValue")
    , ("field2", String "other value")
    ]

exampleTests = testGroup "golden decodes"
    [ testCase "example1" $ loads example1 @?= Right example1_ron
    , testCase "example2" $ loads example2 @?= Right example2_ron
    , testCase "example3" $ loads example3 @?= Right example3_ron
    ]
