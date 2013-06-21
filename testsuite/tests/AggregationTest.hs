module AggregationTest
where

import Test.HUnit

import Aggregation
import Money
import Side
import Transaction


parseAggGroupTest = test [
    "group of one member" ~:
        []
        ~=? parseAggGroups ["a"],
    "group of several members" ~:
        [AggGroup "a+b+c" ["a", "b", "c"]]
        ~=? parseAggGroups ["a+b+c"],
    "several groups" ~:
        [AggGroup "a+b" ["a", "b"], AggGroup "c+d" ["c", "d"]]
        ~=? parseAggGroups ["a+b", "c+d"],
    {- must generate error
    "one memeber in several groups" ~:
        []
        ~=? parseAggGroups ["a", "a+b"]
    -}
    "group with name" ~:
        [AggGroup "c" ["a", "b"]]
        ~=? parseAggGroups ["c=a+b"],
    "name alial" ~:
        [AggGroup "b" ["a"]]
        ~=? parseAggGroups ["b=a"]
    {- must generate error
    "group with several names" ~:
        []
        ~=? parseAggGroups ["c=d=a+b"]
    -}
    ]


aggregateSidesTest = test [
    [   Side "g1" (Moneys [Sum 3]),
        Side "g2" (Moneys [Sum 4]),
        Side "g3" (Moneys [Sum 8]),
        Side "h" (Moneys [Sum 16])]
    ~=? aggregateSides aggGroups [
        Side "a" (Moneys [Sum 1]),
        Side "b" (Moneys [Sum 2]),
        Side "c" (Moneys [Sum 4]),
        Side "e" (Moneys [Sum 8]),
        Side "h" (Moneys [Sum 16])]
    ]
    where
        aggGroups = [
            AggGroup "g1" ["a", "b"],
            AggGroup "g2" ["c", "d"],
            AggGroup "g3" ["e"],
            AggGroup "g4" ["f", "g"]]


tests = test [parseAggGroupTest, aggregateSidesTest]
