module CommonCalculationLog
where

import Data.Maybe
import Data.String.Utils

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
        makeLogEntry names transaction = (transactionName transaction, trs)
            where
                trs = map makeP names

                makeP :: Name -> (Moneys, Moneys, Moneys)
                makeP name = (expenses, benefit, expenses `sub` benefit)
                    where
                        expenses = getMoneyFor name $ payers transaction
                        benefit = getMoneyFor name $ beneficators transaction

                transactionName t
                    | null name = "n/a"
                    | otherwise = name
                    where
                        name = date' ++ contragent' ++ category' ++ comment'
                        date' = maybe "" (\(Date d) -> d ++ ": ") $ date t
                        contragent' = maybe "" ((++ ", ") . getContragent) $ contragent t
                        Category categories = category t
                        category' = if null categories
                            then ""
                            else (join "." $ categories) ++ ": "
                        comment' = maybe "" (\(Comment c) -> c) $ comment t

                        getContragent Internal = "internal"
                        getContragent (Contragent contragent) = contragent
