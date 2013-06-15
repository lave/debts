module Result
where

import BasicTypes
import Money
import Side


data Result =
    --  name, balance, expenses
      Balance [(Name, Moneys, Moneys)]
    | CommonCalculationLog [Name] [CommonCalculationLogEntry]
    | MoneyLog Name [(Name, [MLTransaction])]
    deriving (Eq, Show)

--  name, sides (expenses, benefit, balance)
type CommonCalculationLogEntry = (String, [(Moneys, Moneys, Moneys)])

data MLTransaction = MLTransaction {
    --  for transfers to another account
    payer :: Name,
    beneficator :: Maybe Name,
    sum :: Moneys,
    date :: Maybe Date,
    contragent :: Maybe Contragent,
    category :: Category,
    tags :: Tags,
    comment :: Maybe Comment
} deriving (Eq, Show)
