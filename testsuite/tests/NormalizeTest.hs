module NormalizeTest
where

import Test.HUnit

import Utils
import Money
import Side
import Transaction
import Normalize


expandGropusTest = test [
    "only group" ~:
        [RawSide "stan", RawSide "kyle", RawSide "eric", RawSide "kenny"] ~=?
        (expandGroups groups [RawSide "all"]),
    "group and side" ~:
        [RawSideWithFactor "stan" 2, RawSide "kyle", RawSideWithFactor "eric" 3] ~=?
        (expandGroups groups [RawSide "g1", RawSideWithFactor "eric" 3]),
    "complex group" ~:
        [RawSideWithFactor "stan" 2, RawSide "kyle", RawSideWithFactor "eric" 3] ~=?
        (expandGroups groups [RawSide "g2"]),
    "complex group with override" ~:
        [RawSideWithFactor "eric" 3, RawSideWithFactor "kyle" 4] ~=?
        (expandGroups groups [RawSide "g3"]),

    "group with factor" ~:
        [RawSideWithFactor "stan" 4, RawSideWithFactor "kyle" 2] ~=?
        (expandGroups groups [RawSideWithFactor "g1" 2]),
    "complex group with factor" ~:
        [RawSideWithFactor "eric" 6, RawSideWithFactor "kyle" 8] ~=?
        (expandGroups groups [RawSideWithFactor "g3" 2]),

    "remove group" ~:
        [RawSide "eric"] ~=?
        (expandGroups groups [RawSide "stan", RawSide "kyle", RawSide "eric", RawSideRemove "g1"]),
    "remove group from group" ~:
        [RawSide "eric", RawSide "kenny"] ~=?
        (expandGroups groups [RawSide "all", RawSideRemove "g1"]),
    "remove complex group" ~:
        [RawSide "stan", RawSide "kenny"] ~=?
        (expandGroups groups [RawSide "all", RawSideRemove "g3"]),

    "override group" ~:
        [RawSide "eric", RawSideWithFactor "stan" 2, RawSide "kyle"] ~=?
        (expandGroups groups [RawSide "stan", RawSide "kyle", RawSide "eric", RawSideOverride(RawSide "g1")]),
    "override group with factor" ~:
        [RawSide "eric", RawSide "kenny", RawSideWithFactor "stan" 6, RawSideWithFactor "kyle" 3] ~=?
        (expandGroups groups [RawSide "all", RawSideOverride(RawSideWithFactor "g1" 3)]),
    "override complex group" ~:
        [RawSide "stan", RawSide "kenny", RawSideWithFactor "eric" 3, RawSideWithFactor "kyle" 4] ~=?
        (expandGroups groups [RawSide "all", RawSideOverride(RawSide "g3")]),

    "add group" ~:
        [RawSide "stan", RawSide "kyle", RawSide "eric", RawSideWithFactor "stan" 2, RawSide "kyle"] ~=?
        (expandGroups groups [RawSide "stan", RawSide "kyle", RawSide "eric", RawSideAdd(RawSide "g1")]),
    "add group with factor" ~:
        [RawSide "stan", RawSide "kyle", RawSide "eric", RawSide "kenny", RawSideWithFactor "stan" 4, RawSideWithFactor "kyle" 2] ~=?
        (expandGroups groups [RawSide "all", RawSideAdd(RawSideWithFactor "g1" 2)]),
    "add complex group" ~:
        [RawSide "stan", RawSide "kyle", RawSide "eric", RawSide "kenny", RawSideWithFactor "eric" 3, RawSideWithFactor "kyle" 4] ~=?
        (expandGroups groups [RawSide "all", RawSideAdd(RawSide "g3")])
    ]
    where
        groups = [
            Group "all" [RawSide "stan", RawSide "kyle", RawSide "eric", RawSide "kenny"],
            Group "g1" [RawSideWithFactor "stan" 2, RawSide "kyle"],
            Group "g2" [RawSide "g1", RawSideWithFactor "eric" 3],
            Group "g3" [RawSide "g2", RawSideRemove "stan", RawSideOverride(RawSideWithFactor "kyle" 4)]
            ]


processSidesTest = test [
    "simple accumulating" ~:
        [RawSide "stan",
         RawSideWithFactor "eric" 2,
         RawSideWithMoney "kyle" (Moneys [Sum 10]),
         RawSide "kenny"] ~=?
        (processSides
            [RawSide "stan",
             RawSideWithFactor "eric" 2,
             RawSideWithMoney "kyle" (Moneys [Sum 10]),
             RawSide "kenny"]),
    "removing" ~:
        [RawSide "stan",
         RawSideWithFactor "eric" 2] ~=?
        (processSides
            [RawSide "stan",
             RawSideWithFactor "eric" 2,
             RawSideWithMoney "kyle" (Moneys [Sum 10]),
             RawSideRemove "kenny",
             RawSideRemove "kyle"]),
    "overriding" ~:
        [RawSide "stan",
         RawSideWithFactor "eric" 2,
         RawSide "kyle"] ~=?
        (processSides
            [RawSide "stan",
             RawSideWithFactor "eric" 2,
             RawSideWithMoney "kyle" (Moneys [Sum 10]),
             RawSideOverride (RawSide "kyle")])
    ]


normalizeSidesTest = test [
    "split equally" ~: compareResults
        (   Moneys [Money 30 "RUR"],
            [Side "stan" (Moneys [Money 15 "RUR"]),
             Side "kenny" (Moneys [Money 15 "RUR"])],
            [Side "stan" (Moneys [Money 10 "RUR"]),
             Side "eric" (Moneys [Money 10 "RUR"]),
             Side "kyle" (Moneys [Money 10 "RUR"])]
        )
        (normalizeSides
            (Just (Moneys [Money 30 "RUR"]))
            [RawSide "stan", RawSide "kenny"]
            [RawSide "stan", RawSide "kyle", RawSide "eric"]
        ),

    "split by factors" ~: compareResults
        (   Moneys [Money 30 "RUR"],
            [Side "stan" (Moneys [Money 10 "RUR"]),
             Side "kenny" (Moneys [Money 20 "RUR"])],
            [Side "stan" (Moneys [Money 5 "RUR"]),
             Side "eric" (Moneys [Money 15 "RUR"]),
             Side "kyle" (Moneys [Money 10 "RUR"])]
        )
        (normalizeSides
            (Just (Moneys [Money 30 "RUR"]))
            [RawSide "stan", RawSideWithFactor "kenny" 2]
            [RawSide "stan", RawSideWithFactor "kyle" 2, RawSideWithFactor "eric" 3]
        ),

    "split by moneys" ~: compareResults
        (   Moneys [Money 30 "RUR"],
            [Side "stan" (Moneys [Money 10 "RUR"]),
             Side "kenny" (Moneys [Money 20 "RUR"])],
            [Side "stan" (Moneys [Money 5 "RUR"]),
             Side "eric" (Moneys [Money 20 "RUR"]),
             Side "kyle" (Moneys [Money 5 "RUR"])]
        )
        (normalizeSides
            Nothing
            [RawSideWithMoney "stan" (Moneys [Money 10 "RUR"]),
             RawSideWithMoney "kenny" (Moneys [Money 20 "RUR"])]
            [RawSideWithMoney "stan" (Moneys [Money 5 "RUR"]),
             RawSideWithMoney "kyle" (Moneys [Money 5 "RUR"]),
             RawSideWithMoney "eric" (Moneys [Money 20 "RUR"])]
        ),

    "split by plus moneys" ~: compareResults
        (   Moneys [Money 40 "RUR"],
            [Side "stan" (Moneys [Money 15 "RUR"]),
             Side "kenny" (Moneys [Money 25 "RUR"])],
            [Side "stan" (Moneys [Money 10 "RUR"]),
             Side "eric" (Moneys [Money 5 "RUR"]),
             Side "kyle" (Moneys [Money 25 "RUR"])]
        )
        (normalizeSides
            (Just (Moneys [Money 40 "RUR"]))
            [RawSide "stan",
             RawSideWithSummand "kenny" (Moneys [Money 10 "RUR"])]
            [RawSide "stan",
             RawSideWithSummand "eric" (Moneys [Money (-5) "RUR"]),
             RawSideWithSummand "kyle" (Moneys [Money 15 "RUR"])]
        ),

{-
    "no moneys" ~: compareResults (Moneys[], [], [])
        (normalizeSides
            Nothing
            [RawSide "stan"]
            [RawSide "eric"]
        ),

    "ambiguous moneys 1" ~: compareResults (Moneys[], [], [])
        (normalizeSides
            Nothing
            [RawSideWithMoney "stan" (Moneys [Money 10 "RUR"])]
            [RawSideWithMoney "eric" (Moneys [Money 20 "RUR"])]
        ),

    "ambiguous moneys 2" ~: compareResults (Moneys[], [], [])
        (normalizeSides
            (Just (Moneys [Money 30 "RUR"]))
            [RawSideWithMoney "stan" (Moneys [Money 10 "RUR"])]
            [RawSide "eric"]
        ),
-}

    -- stan, kenny 30 > 40 > stan*2, kyle 15, eric+10
    "complex split" ~: compareResults
        (   Moneys [Money 40 "RUR"],
            [Side "stan" (Moneys [Money 10 "RUR"]),
             Side "kenny" (Moneys [Money 30 "RUR"])],
            [Side "stan" (Moneys [Money 10 "RUR"]),
             Side "kyle" (Moneys [Money 15 "RUR"]),
             Side "eric" (Moneys [Money 15 "RUR"])]
        )
        (normalizeSides
            (Just (Moneys [Money 40 "RUR"]))
            [RawSide "stan",
             RawSideWithMoney "kenny" (Moneys [Money 30 "RUR"])]
            [RawSideWithFactor "stan" 2,
             RawSideWithMoney "kyle" (Moneys [Money 15 "RUR"]),
             RawSideWithSummand "eric" (Moneys [Money 10 "RUR"])]
        )
    ]
    where
        compareResults (s1, p1, b1) (s2, p2, b2) =
            assertBool "" $ s1 == s2 && same p1 p2 && same b1 b2

tests = test [expandGropusTest, processSidesTest, normalizeSidesTest]
