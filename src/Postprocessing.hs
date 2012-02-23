module Postprocessing ( roundSides )
where

import Money
import Transaction
import Utils


roundTo :: (Integral b, RealFrac a) => b -> a -> b
roundTo base x = ((* base) . round . (/ (fromIntegral base))) x

roundSides :: (Integral b) => b -> [Side] -> [Side]
roundSides base sides
    | allSidesHaveSameCurrency sides = map (roundSide base) sides
    | otherwise = sides
    where
        allSidesHaveSameCurrency :: [Side] -> Bool
        allSidesHaveSameCurrency = allSame getSideCurrency
        getSideCurrency (Side _ (Moneys [m])) = Just (getCurrency m)
            where
                getCurrency (Sum _) = Nothing
                getCurrency (Money _ c) = Just c
        getSideCurrency _ = Nothing

        roundSide :: (Integral b) => b -> Side -> Side
        roundSide base (Side n (Moneys moneys)) = Side n $ Moneys $ map (roundMoney base) moneys

        roundMoney :: (Integral b) => b -> Money -> Money
        roundMoney base (Sum q) = Sum $ fromIntegral $ roundTo base q
        roundMoney base (Money q c) = Money (fromIntegral $ roundTo base q) c

