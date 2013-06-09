module Money
where

import Text.Printf

import Utils


data Money = Money Double String
    | Sum Double
    deriving Eq

newtype Moneys = Moneys [Money]

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


add :: Moneys -> Moneys -> Moneys
add (Moneys l) (Moneys r) = Moneys $ foldl add' l r where
    add' [] m = [m]
    add' (m1@(Money q1 c1) : ms) m2@(Money q2 c2)
        | c1 == c2 && q1 + q2 == 0 = ms
        | c1 == c2 = (Money (q1 + q2) c1) : ms
        | otherwise = m1 : (add' ms m2)
    add' (Sum q1 : xs) (Sum q2)
        | q1 + q2 == 0 = xs
        | otherwise = Sum (q1 + q2) : xs
    add' (m : ms) m1 = m : (add' ms m1)


mul :: Double -> Moneys -> Moneys
mul 0 _ = Moneys []
mul f (Moneys l) = Moneys $ map (mul' f) l where
    mul' f (Money q c) = Money (q * f) c
    mul' f (Sum q) = Sum (f * q)

sub :: Moneys -> Moneys -> Moneys
sub l r = add l $ mul (-1) r
