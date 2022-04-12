module ScrollSpec where

import Default
import Scroll
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (length)

spec :: TestTree
spec =
  testGroup
    "Scroll"
    [ testGroup
        ".reset"
        [ testCase "changes position to -1" $
            reset (def {position = 4}) @?= def
        ],
      testGroup
        ".format"
        [ testCase "when content length < expected returns content filling space with ws" $
            format (def {content = "test", length = 5}) @?= "test ",
          testCase "when content length = expected returns content" $
            format (def {content = "test", length = 4}) @?= "test",
          testCase "when content length > expected and position = 0 trims content" $
            format (def {content = "longer text", length = 10, position = 0}) @?= "longer tex",
          testCase "when content length > expected and position < 0 trims content" $
            format (def {content = "longer text", length = 10, position = -1}) @?= "longer tex",
          testCase "when content length > expected and position > 0 cycles content until position reached" $
            format (def {content = "longer text", length = 10, position = 7}) @?= "text longe",
          testCase "when content length > expected and position > 0 joins cycled content with separator" $
            format (def {content = "longer text", length = 10, position = 8, separator = " - "}) @?= "ext - long"
        ],
      testGroup
        ".tick"
        [ testCase "increments position" $
            position (tick (def {position = 1, content = "test"})) @?= 2,
          testCase "when position is at last char changes position to 0" $
            position (tick (def {position = 3, content = "test"})) @?= 0
        ]
    ]
