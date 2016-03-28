module Main where

import Test.Tasty

import HW04Tests
import Week6.HW06Tests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests  = testGroup "Tests" [week6Tests]

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
week6Tests = testGroup "Week 6 Tsets"
    [
        fibSuite
    ]
