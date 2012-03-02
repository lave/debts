module Postprocessing ( roundSides )
where

import Maybe

import Money
import Round
import Transaction
import Utils


roundSides :: (Double -> [Double] -> [Double]) -> Double -> [Side] -> [Side]
roundSides rounder base sides
    | allSidesHaveSameCurrency sides =
        zipWith setSideMoney sides (rounder base $ map getSideMoney sides)
    | otherwise = sides
    where
        allSidesHaveSameCurrency :: [Side] -> Bool
        allSidesHaveSameCurrency = allSame getSideCurrency

        currency = case sides of
            [] -> Nothing
            _ -> fromJust $ getSideCurrency $ head sides

        getSideCurrency (Side _ (Moneys moneys)) =
            case moneys of
                [] -> Just Nothing
                [Sum _] -> Just Nothing
                [Money _ c] -> Just $ Just c
                _ -> Nothing

        getSideMoney (Side _ (Moneys moneys)) =
            case moneys of
                [] -> 0
                [Sum s] -> s
                [Money s _] -> s
                _ -> error ("currencies: " ++ show (map getSideCurrency sides))

        setSideMoney (Side name _) s
            | s == 0 = Side name $ Moneys []
            | currency == Nothing = Side name $ Moneys [Sum s]
            | otherwise = Side name $ Moneys [Money s $ fromJust currency]
