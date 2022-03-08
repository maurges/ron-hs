module Values
    ( valuesTests
    ) where

import Data.Text (Text)
import Data.Ron.Deserialize (decode)
import Data.Ron.Serialize (dumps)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty (testGroup)


valuesTests = testGroup "Various specific values"
    [ testGroup "floats"
        [ testCase "0.0" $ decode @Double " 0.0 " @?= Right 0
        , testCase "0.1" $ decode @Double " 0.1 " @?= Right 0.1
        , testCase "1.0e1" $ decode @Double " 1.0e1 " @?= Right 1e1
        , testCase "1.0e+1" $ decode @Double " 1.0e+1 " @?= Right 1e1
        , testCase "1.0e-1" $ decode @Double " 1.0e-1 " @?= Right 1e-1
        , testCase "1e1" $ decode @Double " 1e1 " @?= Right 1e1
        , testCase "1e+1" $ decode @Double " 1e+1 " @?= Right 1e1
        , testCase "1e-1" $ decode @Double " 1e-1 " @?= Right 1e-1
        , testCase "1__e1__" $ decode @Double " 1__e1__ " @?= Right 1e1
        , testCase "1_._0e1_" $ decode @Double " 1_._0e1_ " @?= Right 1e1
        ]
    , testCase "raw string" $ decode @Text
        " r##\" foo \\n bar \"baz\" \"## "
        @?= Right " foo \\n bar \"baz\" "
    ]
