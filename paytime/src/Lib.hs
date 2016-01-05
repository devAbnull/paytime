module Lib where

type Amount = Integer
type Description = String

data Account = Account { accountAmount :: Amount }
data Transaction = Transaction { transactionAmount :: Amount
                               , transactionDescription :: Description}

makeTransaction :: Amount -> Description -> Transaction
makeTransaction = Transaction 

applyTransaction :: Transaction -> Account -> Account
applyTransaction transaction account = Account {accountAmount = newAccountAmount}
  where newAccountAmount = accountAmount account + transactionAmount transaction

deposit :: Amount ->  Description -> Account -> Account
deposit amount description  = applyTransaction (makeTransaction amount description) 

withdraw :: Amount -> Description -> Account -> Account
withdraw amount description = applyTransaction (makeTransaction (- amount) description) 


