module Main where

import System.Environment
import Text.Printf
import Data.Functor


-- main = print (lazySum [1..1000000])

main = do
    [d] <- map read <$> getArgs
    printf "%f\n" $ mean [1..d]


lazySum :: Num a => [a] -> a
lazySum = go 0
  where go acc []     = acc
        go acc (x:xs) = go (x + acc) xs

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)
