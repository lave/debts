module AggregationTest
where

import Test.HUnit
import Aggregation
import Transaction


parseAggGroupTest = test [
    "group of one member" ~:
        []
        ~=? parseAggGroups ["a"],
    "group of several members" ~:
        [("a+b+c", ["a", "b", "c"])]
        ~=? parseAggGroups ["a+b+c"],
    "several groups" ~:
        [("a+b", ["a", "b"]), ("c+d", ["c", "d"])]
        ~=? parseAggGroups ["a+b", "c+d"]
    {- must generate error
    "one memeber in several groups" ~:
        parseAggGroups ["a", "a+b"]
    -}
    ]


aggregationTest = test [
    "a" ~: 0 ~=? 0
    ]
    where
        aggGroups = [
            ("g1", ["a", "b"]),
            ("g2", ["c", "d"])]


tests = test [parseAggGroupTest, aggregationTest]
