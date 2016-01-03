module Lib where

type Amount = Integer
type Account = Integer
type Description = String

data Transaction = Transaction { transactionAmount :: Amount
                               , transactionDescription :: Description}
                               
applyTransaction :: Transaction -> Account -> Account
applyTransaction (Transaction amount _) account = account + amount

deposit :: Amount ->  Description -> Account -> Account
deposit amount description = applyTransaction (Transaction amount description)

withdraw :: Amount -> Description -> Account -> Account
withdraw amount description  = applyTransaction (Transaction (- amount ) description)

applyTransactions :: [Transaction] -> Account -> Account
applyTransactions [] account = account 
applyTransactions (x:xs) acc = applyTransactions xs newAcccount
  where newAcccount = applyTransaction x acc
