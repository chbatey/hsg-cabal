module Week7.Lec07 where

import Control.Monad

sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (x:xs) = do
    next <- x
    rest <- sequence' xs
    return $ next : rest

replicateM' :: Monad m => Int -> m a -> m [a]
replicateM' n m = sequence' $ replicate n m


-- Typeclassopedia

-- instance Functor (Either e) where
--     fmap = undefined
--
-- instance Functor ((->) r) where
--     fmap = undefined

data Pair a = Pair a a

instance Functor Pair where
    fmap f (Pair a b) = Pair a $ f b