module Utils ( same, allSame, trimBoth )
where

import List


same :: Eq a => [a] -> [a] -> Bool
same l1 l2 =
    length l1 == length l2 &&
    length l1 == length (List.intersect l1 l2)


allSame :: Eq b => (a -> b) -> [a] -> Bool
allSame f l = length (nub $ map f l) < 2


trimFirst [] = []
trimFirst l = tail l

trimLast [] = []
trimLast [x] = []
trimLast (x : xs) = x : trimLast xs

trimBoth = trimLast . trimFirst

