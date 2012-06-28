module CommandLine
where

import qualified Data.List (find, isPrefixOf)

import Param


containsKey args name =
    Data.List.find (== name) args /= Nothing


findParameters :: [String] -> [RawParam]
findParameters s =
    map (parseParameter . drop 2) (filter isParameter s)
    where
        isParameter s = and [
            length s > 2,
            Data.List.isPrefixOf "-D" s,
            not $ Data.List.isPrefixOf "-D=" s]
        parseParameter = splitBy (== '=')


splitBy predicate [] = ([], [])
splitBy predicate (x : xs)
    | predicate x = ([], xs)
    | otherwise = (x : fst, snd)
        where (fst, snd) = splitBy predicate xs
