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
        [RawSide "kenny", RawSideWithFactor "stan" 2, RawSide "kyle", RawSideWithFactor "eric" 3] ~=?
        (expandGroups groups [RawSide "kenny", RawSide "g2"])
    ]
    where
        groups = [
            Group "all" [RawSide "stan", RawSide "kyle", RawSide "eric", RawSide "kenny"],
            Group "g1" [RawSideWithFactor "stan" 2, RawSide "kyle"],
            Group "g2" [RawSide "g1", RawSideWithFactor "eric" 3]
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
