module Postprocess (Postprocess.round)
where

import Data.Maybe
import Data.List

import Money
import Param
import Round
import Result
import Side
import Transaction
import Utils


round :: Params -> Result -> Result
round params result
    | base == Nothing = result
    | otherwise = round' (fromJust base) result
    where
        base = getNumberParam params "round.to"


round' base (Balance b) =
    Balance $ zip3
        names
        (roundMoneys smartRound base balances)
        (roundMoneys roundListTo base expences)
    where
        (names, balances, expences) = unzip3 b


--  no rounding for other result types
round' _ result = result


roundMoneys :: (Double -> [Double] -> [Double]) -> Double -> [Moneys] -> [Moneys]
roundMoneys rounder base moneys
    | length currencies == 1 =
        map makeMoneys $ rounder base $ map getMoneyValue moneys
    | otherwise = moneys
    where
        currencies = nub $ map getCurrency $ concat $ map (\(Moneys m) -> m) moneys
        currency = head currencies

        getCurrency (Sum _) = Nothing
        getCurrency (Money _ currency) = Just currency

        getMoneyValue m@(Moneys moneys) =
            case moneys of
                [] -> 0
                [Sum s] -> s
                [Money s _] -> s
                _ -> error ("different currencies: " ++ show (map getCurrency moneys))

        makeMoneys s
            | s == 0 = Moneys []
            | currency == Nothing = Moneys [Sum s]
            | otherwise = Moneys [Money s $ fromJust currency]
