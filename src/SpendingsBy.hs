module SpendingsBy
where

import Data.Function
import Data.List
import Data.Maybe

import BasicTypes
import Balance
import Money
import qualified Result
import Side
import Transaction


spendingsBy :: [Category] -> (Category -> Transaction -> Bool) -> Transactions -> Result.Result
spendingsBy allCategories isMatch transactions =
    Result.SpendingsBy $ sortBy (compare `on` fst) spendings'
    where
        spendings = map (\cat -> (show cat, spendingsFor cat transactions)) allCategories
        spendingsFor cat transactions =
            map (\(Side name exp) -> (name, exp)) $ expenses $ filter (isMatch cat) transactions
        allNames = sort $ nub $ concatMap (\e -> map fst $ snd e) spendings
        spendings' = map (\(cat, sides) -> (cat, map (\name -> fromMaybe (name, Moneys[]) $ find ((== name) . fst) sides) allNames)) spendings
