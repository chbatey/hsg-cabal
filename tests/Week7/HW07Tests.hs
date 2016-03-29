module Week7.HW07Tests where

import Test.Tasty
import Test.Tasty.HUnit

import Week7.HW07

fingersSuite :: TestTree
fingersSuite = testGroup "Various monadic methods"
    [
        testCase "liftM"      $ liftM (+1) (Just 5) @?= Just 6
    ]

