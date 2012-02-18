module Transaction
where

import Date
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


newtype Contragent = Contragent String
    deriving (Show, Eq)

newtype Category = Category String
    deriving (Show, Eq)

newtype Tag = Tag String
    deriving (Show, Eq)

newtype Comment = Comment String
    deriving (Show, Eq)


data Transaction_ side = Transaction {
    payers :: [side],
    beneficators :: [side],
    sum :: Maybe Moneys,
    date :: Maybe Date,
    contragent :: Maybe Contragent,
    category :: [Category],
    tags :: [Tag],
    comment :: Maybe Comment
} deriving (Show)
    

type RawTransaction = Transaction_ RawSide


data Group = Group String [RawSide]
    deriving (Show, Eq)

data Side = Side String Moneys deriving (Show, Eq)
type NormalizedTransaction = Transaction_ Side



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
        


normalizeSides :: Maybe Moneys -> [RawSide] -> [RawSide] -> (Moneys, [Side], [Side])
normalizeSides sum payers beneficators = (sum', payers', beneficators')
    where
        sum' = fromJust $ allTheSame [sum,
            getMoneysOnlyIfAllHasMoneys payers,
            getMoneysOnlyIfAllHasMoneys beneficators]
        payers' = splitSum payers sum'
        beneficators' = splitSum beneficators sum'

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
            List.sum $ map getFactor sides
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


normalizeTransaction :: [Group] -> RawTransaction -> NormalizedTransaction
normalizeTransaction groups (Transaction payers beneficators sum date contragent category tags comment) =
    Transaction
        payers' beneficators' (Just sum') date contragent category tags comment
    where
        (sum', payers', beneficators') = normalizeSides sum
            (processSides $ expandGroups groups payers)
            (processSides $ expandGroups groups beneficators)


