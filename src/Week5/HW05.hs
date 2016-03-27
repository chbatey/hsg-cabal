{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Week5.HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)
import System.FilePath.Posix (dropExtension)
import Data.Bits (xor)
import Data.List (filter, elem, sortBy)
import Control.Applicative


import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Week5.Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret original encoded = do
    o <- BS.readFile original
    e <- BS.readFile encoded
    let xored = xorbs o e
    return $ BS.filter (/= 0) xored

-- encrypt a b = BS.pack $ BS.zipWith xor a b

xorbs :: ByteString -> ByteString -> ByteString
xorbs a b = BS.pack $ BS.zipWith xor a b

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
-- decryptWithKey = undefined
decryptWithKey key path = do
    contents <- BS.readFile path
    let fullKey = BS.cycle key
    let output = xorbs contents fullKey
    let outputFile = dropExtension path
    BS.writeFile outputFile output



-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile path = do
    contents <- BS.readFile path
    return $ decode contents

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victims transactions = do
    victimTransactions <- parseFile victims :: IO (Maybe[TId])
    actualTransactions <- parseFile transactions :: IO (Maybe[Transaction])
    let common = pure filterTransactions <*> victimTransactions <*> actualTransactions
    return $ common

filterTransactions :: [TId] -> [Transaction] -> [Transaction]
filterTransactions tids transactions = Data.List.filter (\ t -> Data.List.elem (tid t) tids) transactions


-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow [] = Map.empty
getFlow (transaction:xs) = Map.filter (/=0) deduction
    where addition  = Map.insertWith (+) (to transaction) (amount transaction) (getFlow xs)
          deduction = Map.insertWith (+) (from transaction) (- (amount transaction)) addition

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal flow = name
    where (name, _) = find $ Map.toList flow
          find []                  = ("No one!", 0)
          find (x:[])              = x
          find ((n, amount):xs) = if amount > nextAmount then (n, amount) else (nextName, nextAmount)
            where (nextName, nextAmount) = find xs

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs flow tids = if payersTotal /= payeesTotal then error $ "Totals not equal. Payers: " ++ show payers ++ " Payees: " ++ show payees
                   else transact payers payees tids
    where
          transact :: [(String, Integer)] -> [(String, Integer)] -> [TId] -> [Transaction]
          transact [] [] _ = []
          transact [] payees _ = error $ "Run out of payers with payees left " ++ show payees
          transact payers [] _ = error $ "RUn out of payees with payers left " ++ show payers
          transact _ _ [] = error "Run out of transaction ids"
          transact ((payer, amountToPay):payers) ((payee, amountDown):payees) (tid: tids) = transaction : transact (newPayer ++ payers) (newPayee ++ payees) tids
            where transfer = min amountToPay amountDown
                  newPayer = if (amountToPay - transfer /= 0) then [(payer, amountToPay - transfer)] else []
                  newPayee = if (amountDown - transfer /= 0) then [(payee, amountDown - transfer)] else []
                  transaction = Transaction { from = payer, to = payee, amount = transfer, tid = tid}
          list = Map.toList flow
          -- list of (String, Integer) of accounts that need money taken from them
          payers = sortBy compareDebt $ Data.List.filter (\(_,amount) -> amount >= 0) list :: [(String, Integer)]
          -- List of (string, Integer) of accounts that money is owed
          payees = sortBy compareDebt $ map (\(n,a) -> (n, -a)) $ Data.List.filter (\(_,amount) -> amount < 0) list :: [(String, Integer)]
          -- Are they equal??
          payersTotal = foldr (\(_, amount) total -> amount + total) 0 payers
          payeesTotal = foldr (\(_, amount) total -> amount + total) 0 payees

compareDebt :: (String, Integer) -> (String, Integer) -> Ordering
compareDebt (_, a1) (_, a2) = compare a2 a1

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON path a = do
    BS.writeFile path $ encode a

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  putStrLn $ "Key is " ++ show key
  decryptWithKey key vict
  mts <- getBadTs vict trans
  putStrLn $ "Bad transactions " ++ show mts
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

