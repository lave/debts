module Fx ( Fx(..), Fx.add, convert )
where

import Money
import Utils
import List

data Fx = Fx Money Money deriving (Eq, Show)


make m1@(Money q1 c1) m2@(Money q2 c2) = Fx m1 m2

add fxs fx
    | not (any (sameCurrencies fx) fxs) = fx : fxs
    where
        sameCurrencies
            (Fx (Money _ c11) (Money _ c21))
            (Fx (Money _ c12) (Money _ c22)) =
                same [c11, c21] [c12, c22]

convert :: [Fx] -> String -> Moneys -> Moneys
convert fxs c (Moneys moneys) = Money.add (Moneys []) (Moneys (map (convert' fxs c) moneys))
    where
        convert' [] c m = m
        convert' ((Fx (Money q1 c1) (Money q2 c2)) : fxs) c0 m@(Money q c)
            | c == c0 = m
            | c1 == c && c2 == c0 = Money (q * q2 / q1) c0
            | c2 == c && c1 == c0 = Money (q * q1 / q2) c0
            | otherwise = convert' fxs c0 m

