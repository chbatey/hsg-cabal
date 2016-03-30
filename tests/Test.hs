module Main where

import Test.Tasty

import Week4.HW04Tests
import Week6.HW06Tests
import Week7.HW07Tests


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests  = testGroup "Tests" [week7Tests]

week4Tests :: TestTree
week4Tests = testGroup "Week 4 Tests"
    [
        ex1Suite,
        equalitySuite,
        showSuite,
        plusSuite,
        timesSuite,
        applySuite
    ]

week6Tests :: TestTree
week6Tests = testGroup "Week 6 Tests"
    [
        fibSuite
    ]

week7Tests :: TestTree
week7Tests = testGroup "Week 7 Tests"
    [
        fingersSuite,
        partitionSuite
    ]