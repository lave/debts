module Normalize
where

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import Money
import Side
import Transaction


-- after expansion only possible sides are simple and with factor
-- simply replace each group with sides it consists of:
--
-- group G = a, -b, c*2, =d*3
-- a, d, G,   e -> a, d, a,   -b, c*2, =d*3, e
-- a, d, G*2, e -> a, d, a*2, -b, c*4, =d*6, e
--

--  replace groups with their definitions
expandGroups :: Map String RawSide -> RawSide -> MidSide
expandGroups groups side = expandGroup side
    where
        -- simple group - the only place where real replacement happens
        expandGroup side@(RawSide name)
            | isNothing group = MidSide name
            | otherwise = expandGroup side'
            where
                side' = Map.lookup name groups

        expandGroup (RawSideWithMoney side moneys) = MidSideM (expandGroup side) 0 moneys
        expandGroup (RawSideWithFactor side factor) = MidSideM (expandGroup side) factor (Moneys [])
        expandGroup (RawSideWithSummand side summand) = MidSideM (expandGroup side) 1 summand
        expandGroup (RawSides sides) = MidSides $ processOps $ map expandGroup sides
        --expandGroup (RawSideRemove side) = RawSideRemove $ expandGroup side
        --expandGroup (RawSideOverride side) = RawSideOverride $ expandGroup side
        --expandGroup (RawSideAdd side) = RawSideAdd $ expandGroup side

        processOps :: [RawSide] -> [MidSide]
        processOps sides = reverse $ foldl processOp [] sides

        processOp :: [RawSide] -> Side -> [RawSide]
        processOp sides side@(RawSideRemove name) = removeSides sides side
        processOp sides (RawSideOverride side) = side : removeSides sides side
        processOp sides (RawSideAdd side) = side : sides
        processOp sides side
            | Set.null commonNames = side : sides
            | otherwise = error $ "Sides witn name " ++ (show commonNames) ++ " already exist in the list: " ++ (show sides)
            where
                newNames = getSideNames side
                oldNames = Set.unions $ map getSideNames sides
                commonNames = Set.intersection oldNames newNames

        removeSides sides side = catMaybes $ map (removeSide removedNames) sides
            where
                removedNames = getSideNames side

                removeSide :: Set String -> RawSide -> Maybe RawSide
                removeSide names side@(RawSide name) = if Set.member name names then Nothing else Just side
                removeSide names (RawSideWithMoney side moneys) = fmap (s -> RawSideWithMoney s moneys) $ removeSide side
                removeSide names (RawSideWithFactor side factor) = fmap (s -> RawSideWithFactor s factor) $ removeSide side
                removeSide names (RawSideWithSummand side summand) = fmap (s -> RawSideWithSummand s summand) $ removeSide side
                removeSide names (RawSides sides) = fmap (s -> RawSideWithSummand s summand) $ removeSide side

                getSideNames :: RawSide -> Set String
                getSideNames (RawSide name) = Set.singleton name
                getSideNames (RawSideWithMoney side moneys) = getSideNames side
                getSideNames (RawSideWithFactor side factor) = getSideNames side
                getSideNames (RawSideWithSummand side summand) = getSideNames side
                getSideNames (RawSides sides) = Set.unions $ map getSideNames sides
                --  op sides should never be here
                --  remove does participate in detecting side names - they are removed from already built list
                --getSideNames (RawSideRemove side) = RawSideRemove $ expandGroup side
                --  override don't participate in detecting side names - we assume that overridden side(s) are already present in the list
                --getSideNames (RawSideOverride side) = []
                --  add don't participate in detecting side names - we assume that added side(s) are already present in the list
                --getSideNames (RawSideAdd side) = RawSideAdd $ expandGroup side

{-
-- perform side operations - remove or override sides, check that every side is included only once
processSides :: RawSide -> RawSide
processSides side = processSides side []
    processSide side@(RawSide _) [] = side
    processSide side@(RawSideWithMoney side money) [] = side
    processSide side@(RawSideWithFactor _ _) [] = side
    processSide side@(RawSideWithSummand _ _) [] = side
    processSide (RawSides sides) = RawSides $ reverse $ foldl processSideOperations [] sides
    where
        processSideOperations sides (RawSideRemove name) =
            filter (\s -> getName s /= name) sides
        processSideOperations sides (RawSideOverride side) =
            side : filter (\s -> getName s /= getName side) sides
        processSideOperations sides (RawSideAdd side) =
            side : sides
        processSideOperations sides side
            | find (\s -> haveSameName s side) sides == Nothing = side : sides
            | otherwise = error $ "Side witn name " ++ (getName side) ++ " already exist in the list: " ++ (show sides_)
        haveSameName s1 s2 = getName s1 == getName s2
-}


-- determine effective sum and split it between sides according to factors, summands etc
normalizeSides :: Maybe Moneys -> RawSide -> RawSide -> (Moneys, Side, Side)
normalizeSides sum payer beneficator
    | length sums == 1 = (sum', payer', beneficator')
    | length sums == 0 = error $ "Failed to deduce transaction sum for " ++ transactionString
    | otherwise = error $ "Ambiguous transaction sum for " ++ transactionString ++ ": " ++ (show sums)
    where
        transactionString = "transaction {payers: " ++ (show payers)
            ++ ", beneficators: " ++ (show beneficators)
            ++ ", sum: " ++ (show sum) ++ "}"

        sums = nub $ catMaybes [sum, getMoneys payer, getMoneys beneficator]
        sum' = head sums
        payer' = splitSum payer sum'
        beneficator' = splitSum beneficator sum'

        getMoneys :: RawSide -> Maybe Moneys
        getMoneys (RawSideWithMoney _ moneys) -> Just moneys
        getMoneys (RawSides sides)
            | all isJust moneys = Just $ foldl add (Moneys []) $ catMaybes moneys
            | otherwise = Nothing
            where
                moneys = map getMoneys sides
        getMoneys _ -> Nothing

{-
        getMoneysOnlyIfAllHasMoneys sides
            | all hasMoney sides = Just $ sumMoney sides
            | otherwise = Nothing
            where
                hasMoney (RawSideWithMoney _ _) = True
                hasMoney _ = False
-}

        sumFactors :: [RawSide] -> Double
        sumFactors sides =
            Data.List.sum $ map getFactor sides

        sumMoney:: [RawSide] -> Moneys
        sumMoney sides =
            foldl add (Moneys []) $ map getMoney sides

        splitSum :: RawSide -> Moneys -> [Side]
        splitSum (RawSide name) sum = [Side name sum]
        splitSum (RawSideWithMoney side moneys) sum
            | sum == moneys = splitSum side sum
            | otherwise = error $ "Ambiguous sum for side " ++ [show side] ++ ": should be " ++ (show moneys) ++ ", but it's " ++ (show sum)
        splitSum (RawSideWithFactor side factor) sum = splitSum (mul factor sum) side
        splitSum (RawSideWithSummand side summand) sum = splitSum (add summand sum) side

        splitSum (RawSides sides) sum = map makeSide sides
            where
                total_f = sumFactors sides
                extra_money = sub sum $ sumMoney sides
                makeSide side = Side (getName side) $
                    add (getMoney side) $ mul (getFactor side / total_f) extra_money


normalizeTransaction :: Map String RawSide -> RawTransaction -> Transaction
normalizeTransaction groups (RawTransaction payer beneficator sum date contragent category tags comment) =
    Transaction payer' beneficator' sum' date contragent category tags comment
    where
        (sum', payer', beneficator') = normalizeSides sum
            (expandGroups groups payer)
            (expandGroups groups beneficator)
