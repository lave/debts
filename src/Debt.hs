module Debt ( calc, balance, expenses )
where

import Money
import Transaction


calc :: ([Side] -> Transaction -> [Side]) -> [Transaction] -> [Side]
calc f transactions =
    foldl f [] transactions

balance :: [Side] -> Transaction -> [Side]
balance sides (Transaction payers beneficators _) =
    sides `addSides` payers `subSides` beneficators

expenses :: [Side] -> Transaction -> [Side]
expenses sides (Transaction _ beneficators _) =
    sides `addSides` beneficators

addSides s1 s2 = combineSides add s1 s2
subSides s1 s2 = combineSides sub s1 s2

combineSides f s1 s2 =
    foldl (combineSide f) s1 s2
    where
        combineSide f [] (Side name1 money1) =
            [Side name1 (f (Moneys []) money1)]
        combineSide f (s@(Side name1 money1) : ss) s1@(Side name2 money2)
            | name1 == name2 = (Side name1 (f money1 money2)) : ss
            | otherwise = s : combineSide f ss s1

