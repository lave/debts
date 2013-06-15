module Utils (
    same,
    trimFirst, trimLast, trimBoth,
    contains, split,
    (|>)
    )
where

import Data.List
import qualified Data.List.Utils (split)


same :: Eq a => [a] -> [a] -> Bool
same l1 l2 =
    length l1 == length l2 &&
    length l1 == length (intersect l1 l2)


trimFirst [] = []
trimFirst l = tail l

trimLast [] = []
trimLast [x] = []
trimLast (x : xs) = x : trimLast xs

trimBoth = trimLast . trimFirst

contains :: Eq a => [a] -> [a] -> Bool
contains = Data.List.isInfixOf

split :: Eq a => [a] -> [a] -> [[a]]
split = Data.List.Utils.split

--  fancy 'pipe' operator
--a |> f = f a
(|>) = flip ($)
