module Process
where

import qualified Data.List as List

import BasicTypes
import Balance
import CommonCalculationLog
import Money
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
    Result.Balance $ zip balance' expenses'
    where
        balance'  = List.sort $ balance transactions
        expenses' = List.sort $ expenses transactions

        na = Moneys []

        zip [] [] = []
        zip [] (Side name expenses : sides) = (name, na, expenses) : zip [] sides
        zip (Side name balance : sides) [] = (name, balance, na) : zip sides []
        zip s1@(Side name1 balance : sides1) s2@(Side name2 expenses : sides2)
            | comparision == EQ = (name1, balance, expenses) : zip sides1 sides2
            | comparision == LT = (name1, balance, na) : zip sides1 s2
            | comparision == GT = (name2, na, expenses) : zip s1 sides2
            where
                comparision = compare name1 name2


process CommonCalculationLog transactions =
    CommonCalculationLog.log transactions

process (MoneyLog name) transactions =
    MoneyLog.log name transactions
