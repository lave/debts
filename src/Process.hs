module Process
where

import Transaction
import Debt
import qualified Result


data Operation =
      Balance
    | Log


process :: Operation -> Transactions -> Result.Result
process Balance transactions = Result.Balances $ zip balance' expenses'
    where
        balance' = calc balance transactions
        expenses' = calc expenses transactions

--  not implemented yet
process Log transactions = Result.Logs []
