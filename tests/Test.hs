module Test where

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests  = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "Something" $ 1 @?= 2
  ]
