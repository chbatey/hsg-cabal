module Week4.HW04Tests where

import Test.Tasty
import Test.Tasty.HUnit
import Week4.HW04

ex1Suite :: TestTree
ex1Suite = testGroup "Exercise 1"
    [
        testCase "x"      $ x @?=  P [0, 1]
    ]

equalitySuite :: TestTree
equalitySuite = testGroup "Testing equality"
    [
        testCase "Same length"    $ P [1, 2, 3] @?= P [1, 2, 3],
        testCase "Ending zeros"   $ P [1, 2, 0] @?= P [1, 2]

    ]

showSuite :: TestTree
showSuite = testGroup "Testing show"
    [
        testCase "If e is 0 only the coefficient is shown" $ show (P [5]) @?= "5",
        testCase "If e is 1 then only cx is shown"         $ show (P [0, 5]) @?= "5x",
        testCase "Terms are separated by +"                $ show (P [4, 5]) @?= "5x + 4",
        testCase "Display 0 only if the while poly is 0"   $ show (P [0]) @?= "0",
        testCase "Don't display 1"                         $ show (P [0, 1]) @?= "x",
        testCase "No special treatment for negative nums"  $ show (P [0, -1, 2]) @?= "2x^2 + -x"
    ]

plusSuite :: TestTree
plusSuite = testGroup "Testing addition"
    [
        testCase "Same length"      $ P [1, 0, 1] `plus` P [0, 1, 0] @?= P [1, 1, 1],
        testCase "First shorter"    $ P [1] `plus` P [1, 1, 1] @?= P [2, 1, 1],
        testCase "Second shorter"   $ P [1, 1, 1] `plus` P [1] @?= P [2, 1, 1]
    ]

timesSuite :: TestTree
timesSuite = testGroup "Testing times"
    [
        testCase "Example"          $ P [1, 1, 1] * P [2, 2] @?= P [2, 4, 4, 2]
    ]

applySuite :: TestTree
applySuite = testGroup "Testing application"
    [
        testCase "Simple" $ applyP x 1 @?= 1,
        testCase "Two"    $ applyP (5 * x^2) 10 @?= 500,
        testCase "Many"   $ applyP (x^2 + 2*x + 1) 2 @?= 9
    ]