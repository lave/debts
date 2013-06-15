module CommonCalculationLog
where

import Data.Maybe

import BasicTypes
import Money
import Side
import Transaction
import qualified Result


log :: Transactions -> Result.Result
log transactions =
    Result.CommonCalculationLog names (map (makeLogEntry names) transactions)
    where
        names = getNames transactions

        makeLogEntry :: [Name] -> Transaction -> Result.CommonCalculationLogEntry
        makeLogEntry names transaction = (trname, trs)
            where
                Comment trname = fromMaybe (Comment "n/a") (comment transaction)
                trs = map makeP names

                makeP :: Name -> (Moneys, Moneys, Moneys)
                makeP name = (expenses, benefit, expenses `sub` benefit)
                    where
                        expenses = getMoneyFor name $ payers transaction
                        benefit = getMoneyFor name $ beneficators transaction
