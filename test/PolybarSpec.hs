module PolybarSpec where

import Polybar
import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec =
  testGroup
    "Polybar"
    [ testGroup
        ".font"
        [ testCase "wraps content into font tag" $
            font 3 "test" @?= "%{T3}test%{T-}"
        ],
      testGroup
        ".clickable"
        [ testCase "wraps content into action tag" $
            clickable "run" "test" @?= "%{A1:run:}test%{A}"
        ]
    ]
