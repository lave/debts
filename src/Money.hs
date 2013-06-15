module Money
where

import qualified Data.List as List
import Data.Maybe
import Text.Printf

import Utils


data Money = Money Double String
    | Sum Double

newtype Moneys = Moneys [Money]

instance Eq Money where
    Sum q1 == Sum q2 = numbersAreClose q1 q2
    Money q1 c1 == Money q2 c2 = c1 == c2 && numbersAreClose q1 q2

numbersAreClose q1 q2 = abs (q1 - q2) < 1e-10

instance Show Money where
    show (Money q c) = showNumber q ++ " " ++ c
    show (Sum q) = showNumber q

showNumber = printf "%.2f"

instance Show Moneys where
    show (Moneys []) = "0"
    show (Moneys [m]) = show m
    show (Moneys (m : ms)) = show m ++ ", " ++ show (Moneys ms)

instance Eq Moneys where
    (Moneys l1) == (Moneys l2) = same l1 l2


empty :: Moneys -> Bool
empty (Moneys []) = True
empty _ = False


add :: Moneys -> Moneys -> Moneys
add (Moneys l) (Moneys r) = Moneys $ foldl add' l r
    where
        add' [] m = [m]
        add' (m1@(Money q1 c1) : ms) m2@(Money q2 c2)
            | c1 == c2 && q1 + q2 == 0 = ms
            | c1 == c2 = (Money (q1 + q2) c1) : ms
            | otherwise = m1 : (add' ms m2)
        add' (Sum q1 : xs) (Sum q2)
            | q1 + q2 == 0 = xs
            | otherwise = Sum (q1 + q2) : xs
        add' (m : ms) m1 = m : (add' ms m1)


sum :: [Moneys] -> Moneys
sum moneys =
    foldl Money.add (Moneys []) moneys


mul :: Double -> Moneys -> Moneys
mul 0 _ = Moneys []
mul f (Moneys l) = Moneys $ map (mul' f) l
    where
        mul' f (Money q c) = Money (q * f) c
        mul' f (Sum q) = Sum (f * q)


sub :: Moneys -> Moneys -> Moneys
sub l r = add l $ mul (-1) r


intersect :: Moneys -> Moneys -> Moneys
intersect (Moneys l) (Moneys r) = Moneys $ mapMaybe (intersect' l) r
    where
        intersect' l (Sum q1)
            | found == Nothing = Nothing
            | otherwise = Just $ Sum $ min q1 q2
            where
                found = List.find isSum l
                Sum q2 = fromJust found

                isSum (Sum _) = True
                isSum _ = False

        intersect' l (Money q1 c1)
            | found == Nothing = Nothing
            | otherwise = Just $ Money (min q1 q2) c1
            where
                found = List.find (hasCurrency c1) l
                Money q2 c2 = fromJust found

                hasCurrency currency (Money _ c) = c == currency
                hasCurrency _ _ = False
