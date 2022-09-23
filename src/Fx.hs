module Fx (Fx(..), Fxs, make, convert)
where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe

import Money
import Utils

data Fx = Fx Money Money deriving (Eq, Show)

-- list of maps of currency to some "standard" currency (note that we can have
-- multiple maps because there's no requirement that all currencies must be
-- convertable to each other - e.g. we may have two disconnected groups of
-- connected currencies (use case for this is unknown yet :)
type Fxs = [Map.Map String Double]


make :: [Fx] -> Fxs
make = reverse . (foldl Fx.add [])

add :: Fxs -> Fx -> Fxs
add fxs (Fx (Money n1 c1) (Money n2 c2)) = add' g1 g2
    where
        (g1, gs1) = List.partition (Map.member c1) fxs
        (g2, gs2) = List.partition (Map.member c2) fxs

        add' :: [Map.Map String Double] -> [Map.Map String Double] -> Fxs
        --  both currencies are unknown yet - add new map with two currencies
        add' [] [] = (Map.fromList [(c1, n1), (c2, n2)]) : fxs
        --  one currency is known but another isn't - add it to the same map as known one
        add' [g1'] [] = (Map.insert c2 (n2 / n1 * (g1' Map.! c1)) g1') : gs1
        add' [] [g2'] = (Map.insert c1 (n1 / n2 * (g2' Map.! c2)) g2') : gs2
        --  both currencies are known
        add' [g1'] [g2']
            | isNothing n2' = (merge g1' g2') : (List.delete g2' gs1)
            -- two currencies are in same map - check that new FX rate is
            -- the same as existing one
            | (fromJust n1') * n2 == n1 * (fromJust n2') = fxs
            | otherwise = error $ "Inconsistent rates for converting " ++ c1 ++ " to " ++ c2 ++ ": existing rate is " ++ (show $ (fromJust n2') / (fromJust n1')) ++ ", new rate is " ++ (show $ n2 / n1)
            where
                 n2' = Map.lookup c2 g1'
                 n1' = Map.lookup c1 g1'

                 -- merge two lists adjusting rates in the second one
                 merge g1_ g2_ = Map.union (Map.map (*k1) g1_) (Map.map (*k2) g2_)
                    where
                        k1 = n1 * (g2_ Map.! c2)
                        k2 = n2 * (g1_ Map.! c1)

convert :: Fxs -> String -> Moneys -> Moneys
convert fxs c (Moneys moneys)
    | isNothing g = error $ "FX rate is unknown for currency " ++ c
    | otherwise = Money.add (Moneys []) (Moneys (map convert' moneys))
    where
        g = List.find (Map.member c) fxs
        n = (fromJust g) Map.! c

        convert' m@(Sum q) = m
        convert' m@(Money q c')
            | c == c' = m
            | isNothing n' = error $ "FX rate is unknown for converting " ++ c' ++ " to " ++ c
            | otherwise = Money q' c
            where
                n' = Map.lookup c' (fromJust g)
                q' = q * n / (fromJust n')
