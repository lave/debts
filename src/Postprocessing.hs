module Postprocessing ( roundSides )
where

import Money
import Transaction


roundTo :: (Integral b, RealFrac a) => b -> a -> b
roundTo base x = ((* base) . round . (/ (fromIntegral base))) x

roundListTo :: (Integral b, RealFrac a) => b -> [a] -> [b]
roundListTo base = map (roundTo base)

roundSides :: (Integral b) => b -> [Side] -> [Side]
roundSides base = map (roundSide base)
    where
        roundSide :: (Integral b) => b -> Side -> Side
        roundSide base (Side n (Moneys moneys)) = Side n $ Moneys $ map (roundMoney base) moneys

        roundMoney :: (Integral b) => b -> Money -> Money
        roundMoney base (Sum q) = Sum $ fromIntegral $ roundTo base q
        roundMoney base (Money q c) = Money (fromIntegral $ roundTo base q) c

