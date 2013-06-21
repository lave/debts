module Aggregation
where

import Data.List (find, groupBy, sort, nub, (\\), intercalate)
import Data.Maybe

import Money
import Side
import Transaction
import Utils


data AggGroup = AggGroup String [String]
    deriving (Eq, Show)
type AggGroups = [AggGroup]

groupName (AggGroup name _) = name
groupNames (AggGroup _ names) = names

parseAggGroups :: [String] -> AggGroups
parseAggGroups groups
    | length duplicates == 0 = parsedGroups
    | otherwise = error $ "These names are included to multiple aggregation groups: " ++ intercalate ", " duplicates
    where
        parsedGroups = map parseGroup groups
        duplicates = nub $ names \\ (nub names)
        names = concatMap groupNames parsedGroups

        parseGroup s
            | length tokens <= 2 = AggGroup name $ split "+" groups
            | otherwise = error $ "More than one '=' sign in aggregation group definition: " ++ s
            where
                tokens = split "=" s
                name = tokens !! 0
                groups = tokens !! (min 1 $ length tokens - 1)
                


aggregateTransaction :: AggGroups -> Transaction -> Transaction
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
                Just (AggGroup groupName _) -> Side groupName moneys
                Nothing -> side
        findGroup groups name =
            find (groupContains name) groups

        groupContains :: String -> AggGroup -> Bool
        groupContains name (AggGroup groupName names) =
            contains [name] names
        addSides (Side name1 moneys1) (Side name2 moneys2)
            | name1 == name2 = Side name1 $ add moneys1 moneys2
