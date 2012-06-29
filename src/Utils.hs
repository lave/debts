module Utils (
    same, allSame,
    trimFirst, trimLast, trimBoth,
    contains, split
    )
where

import Data.List
--import Data.List.Utils (contains, split)


same :: Eq a => [a] -> [a] -> Bool
same l1 l2 =
    length l1 == length l2 &&
    length l1 == length (intersect l1 l2)


allSame :: Eq b => (a -> b) -> [a] -> Bool
allSame f l = length (nub $ map f l) < 2


trimFirst [] = []
trimFirst l = tail l

trimLast [] = []
trimLast [x] = []
trimLast (x : xs) = x : trimLast xs

trimBoth = trimLast . trimFirst

--contains = Data.List.Utils.contains
--split = Data.List.Utils.split
contains [x] xs = find (== x) xs /= Nothing

split :: Eq a => a -> [a] -> [[a]]
split _ [] = [[]]
split s (x : xs)
    | x == s = [] : r
    | otherwise = (x : (head r)) : (tail r)
    where
        r = split s xs
