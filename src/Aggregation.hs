module Aggregation
where

import Data.List (find, groupBy, sort)
import Data.List.Utils (contains, split)
import Data.Maybe

import Money
import Transaction


type AggGroup = (String, [String])
type AggGroups = [AggGroup]

parseAggGroups :: [String] -> AggGroups
parseAggGroups = map parseGroup . filter (contains "+")
    where
        parseGroup s = (s, split "+" s)


aggregateTransaction :: AggGroups -> NormalizedTransaction -> NormalizedTransaction
aggregateTransaction groups (Transaction payers beneficators sum date contragent category tags comment) = Transaction payers' beneficators' sum date contragent category tags comment
    where
        payers' = aggregateSides groups payers
        beneficators' = aggregateSides groups beneficators

        aggregateSides groups sides
            = map (foldl1 addSides)
            $ groupBy (\s1 s2 -> compare s1 s2 == EQ)
            $ sort
            $ map (renameSide groups) sides
            where
                renameSide sides side@(Side name moneys) =
                    case findGroup groups name of
                        Just (groupName, _) -> Side groupName moneys
                        Nothing -> side
                findGroup groups name =
                    find (groupContains name) groups

                groupContains :: String -> AggGroup -> Bool
                groupContains name (groupName, names) =
                    contains [name] names
                addSides (Side name1 moneys1) (Side name2 moneys2)
                    | name1 == name2 = Side name1 $ add moneys1 moneys2
