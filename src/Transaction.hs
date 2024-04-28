module Transaction
where

import qualified Data.List as List
import qualified Data.Set as Set
import Data.Maybe

import BasicTypes
import Money
import Side
import Utils


data Transaction_ side = Transaction {
    payers :: [side],
    beneficators :: [side],
    sum :: Maybe Moneys,
    date :: Maybe Date,
    contragent :: Maybe Contragent,
    category :: Category,
    tags :: Tags,
    comment :: Maybe Comment
} deriving (Show)
    

type RawTransaction = Transaction_ RawSide
type Transaction = Transaction_ Side

type RawTransactions = [RawTransaction]
type Transactions = [Transaction]

data Group = Group String [RawSide]
    deriving (Show, Eq)


getNames :: Transactions -> [Name]
getNames transactions =
    Set.toList $ foldl addSideNames Set.empty transactions
    where
        addSideNames names transaction =
            addNames names $ (payers transaction) ++ (beneficators transaction)
        addNames names sides =
            foldl addName names sides
        addName names (Side name _) = Set.insert name names


getMoneyFor :: Name -> Sides -> Moneys
getMoneyFor name sides
    | found == Nothing = Moneys []
    | otherwise = moneys
    where
        found = List.find (\(Side n _) -> n == name) sides
        Side _ moneys = fromJust found

isInternal transaction = contragent transaction == Just Internal
