module Lib where

import Data.List 

--data types 

type Amount = Integer
type Description = String

data Account = Account { accountName :: String
                       , accountBalance :: Amount 
                       , accountTransaction :: [Transaction] } deriving Show
                       
data Transaction = Transaction { transactionAmount:: Amount
                               , transactionDescription :: Description} deriving Show

-- First functions to handle transactions 

makeTransaction :: Amount -> Description -> Transaction
makeTransaction = Transaction 

applyTransaction :: Transaction -> Account -> Account
applyTransaction transaction account = account{accountBalance = newAccountBalance, accountTransaction = transaction : accountTransaction account}
  where newAccountBalance = accountBalance account + transactionAmount transaction

deposit :: Amount ->  Description -> Account -> Account
deposit amount description  = applyTransaction (makeTransaction amount description) 

withdraw :: Amount -> Description -> Account -> Account
withdraw amount description = applyTransaction (makeTransaction (- amount) description) 

-- Make a new account

makeAccount :: String -> Amount -> Account
makeAccount n a = Account {accountName = n, accountBalance = a, accountTransaction = []}


doCreateAccounts :: [Account] -> IO [Account]
doCreateAccounts accounts = do
  putStrLn "Enter new account name"
  newName <- getLine
  putStrLn "Enter new account balance"
  balStr <- getLine
  let newBal      = read balStr :: Integer
      newAcct     = makeAccount newName newBal
      newAccounts = newAcct : accounts
  putStrLn $ "Account " ++ newName ++ " made with balance " ++ show newBal ++ "."
  return newAccounts

-- delete a account with FindIndex 


doDeleteAccounts :: [Account] -> IO [Account] 
doDeleteAccounts accounts = do 
  putStrLn "Which account to do want to remove?"
  accountNameToDelete <- getLine
  let newAccounts = removeAccountNamed accountNameToDelete accounts
  return newAccounts

isAccount :: String -> Account -> Bool
isAccount name acct = accountName acct == name

removeAccountNamed :: String -> [Account] -> [Account]
removeAccountNamed s accts = acctsAfter
  where maybeIndex = findIndex (isAccount s) accts
        acctsAfter = case maybeIndex of
                     Nothing  -> accts
                     Just idx -> take idx accts ++ drop (idx + 1) accts


-- Delete a account with a filter

doDeleteAccounts2 :: [Account] -> IO [Account]
doDeleteAccounts2 accounts = do
  putStrLn "Please enter an account name to delete?"
  acctName <- getLine
  let newAccounts = deleteAccount acctName accounts
  return newAccounts
                              
deleteAccount s accts = filter notNamed accts
  where notNamed acct = accountName acct /= s                             
        