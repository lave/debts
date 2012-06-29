module Utils (
    same, allSame,
    trimFirst, trimLast, trimBoth,
    contains, split
    )
where

import Data.List
import qualified Data.List.Utils (contains, split)


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

contains :: Eq a => [a] -> [a] -> Bool
contains = Data.List.Utils.contains

split :: Eq a => [a] -> [a] -> [[a]]
split = Data.List.Utils.split
