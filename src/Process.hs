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
import SpendingsBy
import Transaction


data Operation =
      Balance
    | CommonCalculationLog
    | CalculationLog
    | MoneyLog Name
    | SpendingsByCategory (Maybe Int)


process :: Operation -> Transactions -> Result.Result

process Balance transactions =
    Result.Balance $ outerJoin balance' expenses'
        (\(Side name balance) (Side _ expenses) -> (name, balance, expenses))
        (\(Side name balance) -> (name, balance, Moneys []))
        (\(Side name expenses) -> (name, Moneys [], expenses))
    where
        balance'  = List.sort $ balance transactions
        expenses' = List.sort $ expenses transactions


process CommonCalculationLog transactions =
    CommonCalculationLog.log transactions

process (MoneyLog name) transactions =
    MoneyLog.log name transactions

process (SpendingsByCategory depth) transactions =
    spendingsBy allCategories (\c t -> c == (truncCat depth $ category t)) transactions
    where
        allCategories = List.nub $ map ((truncCat depth) . category) transactions
        truncCat Nothing category = category
        truncCat (Just d) (Category cs) = Category (take d cs)


outerJoin :: Ord a => [a] -> [a] -> (a -> a -> b) -> (a -> b) -> (a -> b) -> [b]
outerJoin [] rs _ _ fromRight =
    map fromRight rs
outerJoin ls [] _ fromLeft _ =
    map fromLeft ls
outerJoin (l : ls) (r : rs) merge fromLeft fromRight
    | l < r = (fromLeft l) : (outerJoin ls (r : rs) merge fromLeft fromRight)
    | l > r = (fromRight r) : (outerJoin (l : ls) rs merge fromLeft fromRight)
    | otherwise = (merge l r) : (outerJoin ls rs merge fromLeft fromRight)
