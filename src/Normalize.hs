module Normalize
where

import Data.List
import Data.Maybe

import Money
import Side
import Transaction


-- simply replace each group with sides it consists of:
--
-- group 1 = a, b, c
-- a, d, 1, e -> a, d, a, b, c, e
expandGroups :: [Group] -> [RawSide] -> [RawSide]
expandGroups groups sides =
    concat $ map expandGroup sides
    where
        expandGroup side@(RawSide name) =
            getSides $ getGroup name
            where
                getGroup name = find (groupHasName name) groups
                groupHasName n (Group name _) = n == name
                getSides Nothing = [side]
                getSides (Just (Group _ sides')) = expandGroups groups sides'
        expandGroup side = [side]


-- perform side operations - remove or override sides, check that every side is included only once
processSides :: [RawSide] -> [RawSide]
processSides sides =
    reverse $ foldl processSideOperations [] sides
    where
        processSideOperations sides (RawSideRemove name) =
            filter (\s -> getName s /= name) sides
        processSideOperations sides (RawSideOverride side) =
            side : filter (\s -> getName s /= getName side) sides
        processSideOperations sides side
            | find (\s -> haveSameName s side) sides == Nothing = side : sides
            | otherwise = error ("Side witn name " ++ (getName side) ++ " already exist in the list")
        haveSameName s1 s2 = getName s1 == getName s2
        getName (RawSide n) = n
        getName (RawSideWithFactor n _) = n
        getName (RawSideWithMoney n _) = n
        


-- determine effective sum and split it between sides according to factors, summands etc
normalizeSides :: Maybe Moneys -> [RawSide] -> [RawSide] -> (Moneys, [Side], [Side])
normalizeSides sum payers beneficators
    | length sums == 1 = (sum', payers', beneficators')
    | length sums == 0 = error $ "Failed to deduce transaction sum for " ++ transactionString
    | otherwise = error $ "Ambiguous transaction sum for " ++ transactionString ++ ": " ++ (show sums)
    where
        transactionString = "transaction {payers: " ++ (show payers)
            ++ ", beneficators: " ++ (show beneficators)
            ++ ", sum: " ++ (show sum) ++ "}"

        sums = nub $ catMaybes [sum,
            getMoneysOnlyIfAllHasMoneys payers,
            getMoneysOnlyIfAllHasMoneys beneficators]
        sum' = head sums
        payers' = splitSum payers sum'
        beneficators' = splitSum beneficators sum'

        getMoneysOnlyIfAllHasMoneys sides
            | all hasMoney sides = Just $ sumMoney sides
            | otherwise = Nothing

        sumFactors :: [RawSide] -> Double
        sumFactors sides =
            Data.List.sum $ map getFactor sides

        sumMoney:: [RawSide] -> Moneys
        sumMoney sides =
            foldl add (Moneys []) $ map getMoney sides

        splitSum :: [RawSide] -> Moneys -> [Side]
        splitSum sides sum = map makeSide sides
            where
                total_f = sumFactors sides
                extra_money = sub sum $ sumMoney sides
                makeSide side = Side (getName side) $
                    add (getMoney side) $ mul (getFactor side / total_f) extra_money


normalizeTransaction :: [Group] -> RawTransaction -> Transaction
normalizeTransaction groups (Transaction payers beneficators sum date contragent category tags comment) =
    Transaction payers' beneficators' (Just sum') date contragent category tags comment
    where
        (sum', payers', beneficators') = normalizeSides sum
            (processSides $ expandGroups groups payers)
            (processSides $ expandGroups groups beneficators)
