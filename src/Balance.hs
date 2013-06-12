module Debt (calc, balance, expenses, addSides, subSides)
where

import Money
import Side
import Transaction


calc f transactions =
    foldl f [] transactions

balance :: [Side] -> Transaction -> [Side]
balance sides transaction =
    sides
        `addSides` (payers transaction)
        `subSides` (beneficators transaction)

expenses :: [Side] -> Transaction -> [Side]
expenses sides transaction
    | isInternal transaction = sides
    | otherwise = sides `addSides` (beneficators transaction)
    where
        isInternal transaction =
            case (contragent transaction) of
                Just Internal -> True
                _ -> False


addSides = combineSides add
subSides = combineSides sub

combineSides f s1 s2 =
    foldl combineSide s1 s2
    where
        combineSide [] (Side name1 money1) =
            [Side name1 (f (Moneys []) money1)]
        combineSide (s@(Side name1 money1) : ss) s1@(Side name2 money2)
            | name1 == name2 = (Side name1 (f money1 money2)) : ss
            | otherwise = s : combineSide ss s1

