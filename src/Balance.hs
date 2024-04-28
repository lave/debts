module Balance --(balance, expenses)
where

import BasicTypes
import Money
import Side
import Transaction


balance :: Transactions -> Sides
balance transactions =
    foldl balance' [] transactions

expenses :: Transactions -> Sides
expenses transactions =
    foldl expenses' [] transactions


balance' sides transaction =
    sides
        `addSides` (payers transaction)
        `subSides` (beneficators transaction)

expenses' sides transaction
    | isInternal transaction = sides
    | otherwise = sides `addSides` (beneficators transaction)


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

