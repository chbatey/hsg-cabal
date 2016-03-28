module Week6.HW06Tests where

import Test.Tasty
import Test.Tasty.HUnit

import Week6.HW06

fibSuite :: TestTree
fibSuite = testGroup "Fib test"
    [
        testCase "0"       $ fib 0 @?= 1,
        testCase "1"       $ fib 1 @?= 1,
        testCase "n+1"     $ fib 2 @?= 2,
        testCase "first 5" $ (take 5 fibs1) @?= [1,1,2,3,5],
        testCase "first 5" $ (take 5 fibs2) @?= [1,1,2,3,5]
    ]

