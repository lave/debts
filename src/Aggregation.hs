module Aggregation
where

import Data.List (find, groupBy, sort, nub)
import Data.Maybe

import Money
import Transaction
import Utils


type AggGroup = (String, [String])
type AggGroups = [AggGroup]

parseAggGroups :: [String] -> AggGroups
parseAggGroups groups
    | isUnique r = r
    | otherwise = error "names are not unique"
    where
        r =  map parseGroup $ filter (contains "+") groups
        parseGroup s = (s, split '+' s)
        isUnique groups = (length names) == (length $ nub names)
            where
                names = concat $ map snd groups


aggregateTransaction :: AggGroups -> NormalizedTransaction -> NormalizedTransaction
aggregateTransaction groups transaction =
    transaction { payers = payers', beneficators = beneficators' }
    where
        payers' = aggregateSides groups $ payers transaction
        beneficators' = aggregateSides groups $ beneficators transaction

aggregateSides :: AggGroups -> [Side] -> [Side]
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
