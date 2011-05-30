module Transaction ( Group(..), RawSide(..), RawTransaction(..), Side(..), Transaction(..),
    expandGroups, preprocessTransactions, unifyTransaction )

where

import List
import Maybe
import Utils
import Money



data RawSide = RawSide String
    | RawSideWithFactor String Double
    | RawSideWithMoney String Moneys
    | RawSideRemove String
    | RawSideOverride RawSide
    deriving (Show, Eq)

data Group = Group String [RawSide] deriving Show


data RawTransaction = RawTransaction [RawSide] [RawSide] (Maybe Moneys) String deriving (Show)


data Side = Side String Moneys deriving (Show, Eq)
data Transaction = Transaction [Side] [Side] String deriving Show

instance Eq Transaction where
    (Transaction p1 b1 c1) == (Transaction p2 b2 c2) =
        same p1 p2 && same b1 b2 && c1 == c2



haveSameName s1 s2 = getName s1 == getName s2

getName (RawSide n) = n
getName (RawSideWithFactor n _) = n
getName (RawSideWithMoney n _) = n


groupHasName n (Group name _) = n == name


expandGroups :: [Group] -> [RawSide] -> [RawSide]
expandGroups groups sides =
    concat $ map (expandGroup groups) sides
    where
        expandGroup groups s@(RawSide n) =
            getSides $ getGroup n
            where
                getGroup n = find (groupHasName n) groups
                getSides Nothing = [s]
                getSides (Just (Group _ sides')) = expandGroups groups sides'
        expandGroup groups s = [s]


processSidesOperations :: [RawSide] -> [RawSide]
processSidesOperations sides =
    reverse $ foldl processSideOperations [] sides
    where
        processSideOperations sides s@(RawSideRemove n) =
            deleteBy haveSameName (RawSide n) sides
        processSideOperations sides (RawSideOverride side) =
            side : (deleteBy haveSameName side sides)
        processSideOperations sides side
            | find (\s -> haveSameName s side) sides == Nothing = side : sides
            | otherwise = error ("Side witn name " ++ (getName side) ++ " already exist in the list")


preprocessTransactions groups transactions =
    map preprocessTransaction transactions
    where
        preprocessTransaction (RawTransaction payers beneficators sum comment) =
            RawTransaction (preprocess payers) (preprocess beneficators) sum comment
            where
                preprocess = processSidesOperations . (expandGroups groups)
    
    
 


unifyTransaction :: RawTransaction -> Transaction
unifyTransaction (RawTransaction payers beneficators sum comment) =
    Transaction (splitSum payers sum') (splitSum beneficators sum') comment
    where
        sum' = Maybe.fromJust $ allTheSame [payers_moneys, beneficators_moneys, sum]
        payers_moneys = getMoneysOnlyIfAllHasMoneys payers
        beneficators_moneys = getMoneysOnlyIfAllHasMoneys beneficators

        allTheSame l | length l' == 1 = head l'
            where
                l' = nub (filter Maybe.isJust l)
        getMoneysOnlyIfAllHasMoneys sides
            | all hasMoney sides = Just $ sumMoney sides
            | otherwise = Nothing
            where
                hasMoney (RawSideWithMoney _ _) = True
                hasMoney _ = False

sumFactors :: [RawSide] -> Double
sumFactors sides =
    sum $ map getFactor sides
    where
        getFactor (RawSide _) = 1
        getFactor (RawSideWithFactor _ factor) = factor
        getFactor (RawSideWithMoney _ _) = 0


sumMoney:: [RawSide] -> Moneys
sumMoney sides =
    foldl add (Moneys []) $ map getMoney sides
    where
        getMoney (RawSideWithMoney _ money) = money
        getMoney _ = Moneys []


splitSum :: [RawSide] -> Moneys -> [Side]
splitSum sides sum = map makeSide sides
    where
        total_f = sumFactors sides
        extra_money = sub sum $ sumMoney sides
        makeSide (RawSide name) = Side name (mul (1 / total_f) extra_money)
        makeSide (RawSideWithFactor name factor) = Side name (mul (factor / total_f) extra_money)
        makeSide (RawSideWithMoney name money) = Side name money

