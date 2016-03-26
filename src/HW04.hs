{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List(intercalate)

newtype Poly a = P [a]

data Blah a = Blah [a] deriving (Show)
data Wah a = Wah Int deriving (Show)


-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [1, 0]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (P a) == (P b) = removeLeadingZeros (reverse a) == removeLeadingZeros (reverse b)

removeLeadingZeros :: (Num a, Eq a) => [a] -> [a]
removeLeadingZeros (0:xs) = xs
removeLeadingZeros xs = xs
 
-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P [0]) = "0"
    show (P p) = intercalate " + " $ reverse $ filter (/= "") $ zipWith string [0..] p


string :: (Num a, Show a, Eq a) => Integer -> a -> String
string 0 0 = ""
string 0 c = show c
string 1 1 = "x"
string 1 (-1) = "-x"
string 1 c = show c ++ "x"
string e 1 = "x^" ++ show e
string e (-1) = "-x^" ++ show e
string e c = show c ++ "x^" ++ show e

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
(P a) `plus` (P b) = P $ add a b
    where add [] b = b
          add a [] = a
          add (a:axs) (b:bxs) = (a+b) : add axs bxs


-- Exercise 5 -----------------------------------------

-- times :: Num a => Poly a -> Poly a -> Poly a
times :: Num a => Poly a -> Poly a -> Poly a
times (P a) b = sum $ map (\(val, s) -> mult val $ shift s b) zipped
    where zipped = zip a [0..]


shift :: Num a => Int -> Poly a -> Poly a
shift a (P xs) = P $ replicate a 0 ++ xs

mult :: Num a => a -> Poly a -> Poly a
mult a (P b) = P $ map (*a) b


-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger int = P [fromIntegral int]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined

