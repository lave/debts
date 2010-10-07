module TransactionTest
where

import Test.HUnit
import Money
import Transaction


getNameTest = test [
    "raw side" ~: "stan" ~=? getName (RawSide "stan"),
    "raw side with factor" ~: "stan" ~=? getName (RawSideWithFactor "stan" 1),
    "raw side with money" ~: "stan" ~=? getName (RawSideWithMoney "stan" (Moneys [Sum 1]))
    ]


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


processSidesOperationsTest = test [
    "simple accumulating" ~:
        [RawSide "stan",
         RawSideWithFactor "eric" 2,
         RawSideWithMoney "kyle" (Moneys [Sum 10]),
         RawSide "kenny"] ~=?
        (processSidesOperations
            [RawSide "stan",
             RawSideWithFactor "eric" 2,
             RawSideWithMoney "kyle" (Moneys [Sum 10]),
             RawSide "kenny"]),
    "removing" ~:
        [RawSide "stan",
         RawSideWithFactor "eric" 2] ~=?
        (processSidesOperations
            [RawSide "stan",
             RawSideWithFactor "eric" 2,
             RawSideWithMoney "kyle" (Moneys [Sum 10]),
             RawSideRemove "kenny",
             RawSideRemove "kyle"]),
    "overriding" ~:
        [RawSide "stan",
         RawSideWithFactor "eric" 2,
         RawSide "kyle"] ~=?
        (processSidesOperations
            [RawSide "stan",
             RawSideWithFactor "eric" 2,
             RawSideWithMoney "kyle" (Moneys [Sum 10]),
             RawSideOverride (RawSide "kyle")])
    ]


unifyTests = test [
    "split equally" ~:
        (Transaction
            [Side "stan" (Moneys [Money 15 "RUR"]),
             Side "kenny" (Moneys [Money 15 "RUR"])]
            [Side "stan" (Moneys [Money 10 "RUR"]),
             Side "eric" (Moneys [Money 10 "RUR"]),
             Side "kyle" (Moneys [Money 10 "RUR"])]
            "beer") ~=?
        (unifyTransaction (RawTransaction
            [RawSide "stan", RawSide "kenny"]
            [RawSide "stan", RawSide "kyle", RawSide "eric"]
            (Just (Moneys [Money 30 "RUR"]))
            "beer")),

    "split by factors" ~:
        (Transaction
            [Side "stan" (Moneys [Money 10 "RUR"]),
             Side "kenny" (Moneys [Money 20 "RUR"])]
            [Side "stan" (Moneys [Money 5 "RUR"]),
             Side "eric" (Moneys [Money 15 "RUR"]),
             Side "kyle" (Moneys [Money 10 "RUR"])]
            "beer") ~=?
        (unifyTransaction (RawTransaction
            [RawSide "stan", RawSideWithFactor "kenny" 2]
            [RawSide "stan", RawSideWithFactor "kyle" 2, RawSideWithFactor "eric" 3]
            (Just (Moneys [Money 30 "RUR"]))
            "beer")),

    "split by moneys" ~:
        (Transaction
            [Side "stan" (Moneys [Money 10 "RUR"]),
             Side "kenny" (Moneys [Money 20 "RUR"])]
            [Side "stan" (Moneys [Money 5 "RUR"]),
             Side "eric" (Moneys [Money 20 "RUR"]),
             Side "kyle" (Moneys [Money 5 "RUR"])]
            "beer") ~=?
        (unifyTransaction (RawTransaction
            [RawSideWithMoney "stan" (Moneys [Money 10 "RUR"]),
             RawSideWithMoney "kenny" (Moneys [Money 20 "RUR"])]
            [RawSideWithMoney "stan" (Moneys [Money 5 "RUR"]),
             RawSideWithMoney "kyle" (Moneys [Money 5 "RUR"]),
             RawSideWithMoney "eric" (Moneys [Money 20 "RUR"])]
            Nothing
            "beer")),

{-
    "split by moneys - different moneys 1" ~:
        (unifyTransaction (RawTransaction
            [RawSideWithMoney "stan" (Moneys [Money 10 "RUR"])]
            [RawSideWithMoney "eric" (Moneys [Money 20 "RUR"])]
            Nothing
            "beer")),

    "split by moneys - different moneys 2" ~:
        (unifyTransaction (RawTransaction
            [RawSideWithMoney "stan" (Moneys [Money 10 "RUR"])]
            [RawSide "eric"]
            (Just (Moneys [Money 30 "RUR"]))
            "beer")),
-}

    "split by factors and moneys" ~:
        (Transaction
            [Side "stan" (Moneys [Money 10 "RUR"]),
             Side "kenny" (Moneys [Money 20 "RUR"])]
            [Side "stan" (Moneys [Money 5 "RUR"]),
             Side "eric" (Moneys [Money 15 "RUR"]),
             Side "kyle" (Moneys [Money 10 "RUR"])]
            "beer") ~=?
        (unifyTransaction (RawTransaction
            [RawSide "stan",
             RawSideWithMoney "kenny" (Moneys [Money 20 "RUR"])]
            [RawSide "stan",
             RawSideWithFactor "kyle" 2,
             RawSideWithMoney "eric" (Moneys [Money 15 "RUR"])]
            (Just (Moneys [Money 30 "RUR"]))
            "beer"))
    ]

tests = test [getNameTest, expandGropusTest, processSidesOperationsTest, unifyTests]

