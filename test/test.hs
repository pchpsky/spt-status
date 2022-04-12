import qualified PolybarSpec
import qualified ScrollSpec
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Tests" specs

specs :: [TestTree]
specs =
  [ ScrollSpec.spec,
    PolybarSpec.spec
  ]
