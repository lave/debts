module Process (process)
where

import Debt
import Param
import qualified Result
import Transaction


data Operation =
      Balance
    | Log


getOperation :: Params -> Operation
getOperation params
    | hasParam "log" params = Log
    | otherwise = Balance


process :: Params -> Transactions -> Result.Result
process params transactions = process' (getOperation params) transactions
    

process' Balance transactions = Result.Balances $ zip balance' expenses'
    where
        balance' = calc balance transactions
        expenses' = calc expenses transactions

--  not implemented yet
process' Log transactions = Result.Logs []
