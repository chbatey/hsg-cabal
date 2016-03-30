module Week7.HW07Tests where

import Data.Vector as V

import Test.Tasty
import Test.Tasty.HUnit

import Week7.HW07

fingersSuite :: TestTree
fingersSuite = testGroup "Various monadic methods"
    [
        testCase "liftM"      $ liftM (+1) (Just 5) @?= Just 6
    ]

-- Assumption is no duplicates for now
partitionSuite :: TestTree
partitionSuite = testGroup "Testing partitioning"
    [
        testCase "Single element"  $ partitionAt (V.fromList [1]) 0                      @?= (V.empty, 1, V.empty),
        testCase "Many elements"   $ partitionAt (V.fromList [10,11,54,1,2,34,4,5,10]) 0 @?= ((V.fromList [1,2,4,5]), 10, (V.fromList [11,54,34]))
    ]