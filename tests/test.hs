import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests tsetGroup "Tests" [unitTests]

unitTests = testGroup "Unit Tests"
  [ testCase "Something" $ 1 @?= 2
  ]
