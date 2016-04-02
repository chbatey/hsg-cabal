{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module Week7.HW07 where

import Prelude hiding (mapM)
import Week7.Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random()

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = m >>= return . f

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV a b v = do
    aV <- v !? a
    bV <- v !? b
    return $ v // [(a,bV), (b, aV)]

-- Exercise 2 -----------------------------------------

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f a = sequence $ map f a

getElts :: [Int] -> Vector a -> Maybe [a]
getElts indices v = mapM' (v !?) indices

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

-- todo: pattern match against empty vector
randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = getRandomR (0, V.length v) >>= return .  (v !?)

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = replicateM n getRandom >>= (return . V.fromList)

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n bounds = replicateM n (getRandomR bounds) >>= (return . V.fromList)

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle v = swap (V.length v - 1) v

swap :: Int -> Vector a -> Rnd (Vector a)
swap 0 v = return v
swap n v = getRandomR (0, n) >>= (\r -> return $ v // [(r, current), (n, (toSwap r))]) >>= swap (n-1)
    where current  = v ! n
          toSwap r = v ! r

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v n =  (V.filter (< pivot) v, pivot, V.filter (> pivot) v)
        where pivot = v ! n

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort v
    | V.length v == 0 = V.empty
    | otherwise       = left V.++ V.cons pivot right
        where pivot = V.head v
              t  = V.tail v
              left  = qsort [ y | y <- t, y < pivot ]
              right = qsort [ y | y <- t, y >= pivot ]

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v
    | V.length v == 0 = return V.empty
    | otherwise = pivoted
            where pivoted = do index <- getRandomR (0, V.length v -1)
                               let (l, pivot, r) = partitionAt v index
                               left <- qsortR l
                               right <- qsortR r
                               return $ left V.++ (cons pivot right)

-- Exercise 9 -----------------------------------------

select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select i v
    | V.length v == 0 = return Nothing
    | otherwise = answer v 0
        where answer vector prefix
                  | V.length vector == 0 = return Nothing
                  | otherwise = do index <- getRandomR (0, V.length vector - 1)
                                   let (l, pivot, r) = partitionAt vector index
                                   let pivotNewIndex = prefix + V.length l
                                   if pivotNewIndex == i then return $ Just pivot
                                   else if pivotNewIndex > i then answer l prefix
                                   else answer r (pivotNewIndex + 1)

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [Card label suite | suite <- suits, label <- labels]

newDeck :: Rnd Deck
newDeck =  shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard deck
    | V.length deck == 0 = Nothing
    | otherwise = Just ((V.head deck), (V.tail deck))

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards n deck = draw n $ Just ([], deck)
    where draw 0 result = result
          draw n result = do
            (cards, deck) <- result
            (nextCard, nextDeck) <- nextCard deck
            draw (n-1) (Just (nextCard : cards, nextDeck))


-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty 

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
