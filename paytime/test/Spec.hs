module Lib where

type Amount = Integer
type Account = Integer
type Description = String

data Transaction = Transaction Amount Description

applyTransaction :: Transaction -> Account -> Account
applyTransaction (Transaction amount _) account = account + amount

deposit :: Amount ->  Description -> Account -> Account
deposit amount description account = applyTransaction (Transaction amount description) account

withdraw :: Amount -> Description -> Account -> Account
withdraw amount description account = applyTransaction (Transaction (- amount ) description) account

applyTransactions :: [Transaction] -> Account -> Account
applyTransactions [] account = account 
applyTransactions (x:xs) acc = applyTransactions xs newAcccount
  where newAcccount = (applyTransaction x acc)
