module Process
where

import qualified Data.List as List

import BasicTypes
import Balance
import CommonCalculationLog
import MoneyLog
import qualified Result
import Side
import Transaction


data Operation =
      Balance
    | CommonCalculationLog
    | CalculationLog
    | MoneyLog Name


process :: Operation -> Transactions -> Result.Result

process Balance transactions =
    Result.Balance $ zipWith zipper balance' expenses'
    where
        balance'  = List.sort $ balance transactions
        expenses' = List.sort $ expenses transactions

        zipper (Side name1 balance) (Side name2 expenses)
            | name1 == name2 = (name1, balance, expenses)


process CommonCalculationLog transactions =
    CommonCalculationLog.log transactions

process (MoneyLog name) transactions =
    MoneyLog.log name transactions
