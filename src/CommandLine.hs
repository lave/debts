module CommandLine
where

import Data.Maybe
import qualified Data.List as List

import Param


type Arguments = [String]


containsKey :: Arguments -> String -> Bool
containsKey args name =
    List.find (== name) args /= Nothing

getKey :: Arguments -> String -> Maybe String
getKey args name
    | keyIndex == Nothing = Nothing
    | fromJust keyIndex + 1 == length args = Nothing
    | otherwise = Just $ args !! ((fromJust keyIndex) + 1)
    where
        keyIndex = List.elemIndex name args
    


findParameters :: Arguments -> [RawParam]
findParameters s =
    map (parseParameter . drop 2) (filter isParameter s)
    where
        isParameter s = and [
            length s > 2,
            List.isPrefixOf "-D" s,
            not $ List.isPrefixOf "-D=" s]
        parseParameter = splitBy (== '=')


splitBy predicate [] = ([], [])
splitBy predicate (x : xs)
    | predicate x = ([], xs)
    | otherwise = (x : fst, snd)
        where (fst, snd) = splitBy predicate xs
