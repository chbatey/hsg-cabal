module Main where

import Test.Tasty

import HW04Tests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests  = testGroup "Tests" [week4Tests]

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

