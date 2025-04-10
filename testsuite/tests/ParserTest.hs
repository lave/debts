module ParserTest
where

import Test.HUnit
import TestUtils

import Data.Maybe

import BasicTypes
import Fx
import InputBuilder
import Money
import Param
import Parser
import ParserMonad
import Transaction
import Side
import Utils


s1 +++ s2 = s1 ++ "\n" ++ s2

parseParametersTest = test [
    "srting parameter" ~:
        Just [("s", "a")]
        ~=? parseParameters "param s=a",
    "multiword srting parameter" ~:
        Just [("s", "a a")]
        ~=? parseParameters "param s=\"a a\"",

    "number parameter" ~:
        Just [("s", "1.3")]
        ~=? parseParameters "param s=1.3",
    "number parameter with arithmetics" ~:
        Just [("s", "17.5")]
        ~=? parseParameters "param s=2 + 4*5 - (3+6)/2"
    ]


parseGroupsTest = test [
    "group" ~:
        Just [Group "g" [RawSide "A", RawSide "B", RawSide "C"]]
        ~=? parseGroups "group g = A, B, C",
    "group with factor" ~:
        Just [Group "g" [RawSide "A", RawSideWithFactor "B" 2, RawSide "C"]]
        ~=? parseGroups "group g = A, B*2, C",
    "group minus side" ~:
        Just [Group "g" [RawSide "A", RawSide "B", RawSide "C", RawSideRemove "B"]]
        ~=? parseGroups "group g = A, B, C, -B",
    "group with side overrides" ~:
        Just [Group "g" [RawSide "A", RawSideWithFactor "B" 2, RawSide "C",
                    RawSideOverride $ RawSideWithFactor "A" 2,
                    RawSideOverride $ RawSide "B",
                    RawSideOverride $ RawSideRemove "C"]]
        ~=? parseGroups "group g = A, B*2, C, =A*2, =B, =-C",
    "group with side adds" ~:
        Just [Group "g" [RawSide "A", RawSideWithFactor "B" 2, RawSide "C",
                    RawSideAdd $ RawSideWithFactor "A" 2,
                    RawSideAdd $ RawSide "B",
                    RawSideAdd $ RawSideRemove "C"]]
        ~=? parseGroups "group g = A, B*2, C, +A*2, +B, +-C",
    "group with money - error" ~:
        Nothing
        ~=? parseGroups "group g = A, B, C 1 EUR"
    ]


parseFxesTest = test [
    "srtaight fx" ~:
        Just [Fx (Money 1 "USD") (Money 30 "RUR")]
        ~=? parseFxes "fx 1 USD = 30 RUR",
    "complex fx" ~:
        Just [Fx (Money 10 "USD") (Money 300 "RUR")]
        ~=? parseFxes "fx 10 USD = 300 RUR"
    ]


parseMoneysTest = test [
    "sum" ~:
        Just (Moneys [Sum 100])
        ~=? parseMoneys "100",
    "sum with currency" ~:
        Just (Moneys [Money 100 "USD"])
        ~=? parseMoneys "100 USD",
    "sum with several currencies" ~:
        Just (Moneys [Money 100 "USD", Sum 10, Money 50 "EUR"])
        ~=? parseMoneys "50 EUR 10 100 USD",

    "sum in parenthesis" ~:
        Just (Moneys [Sum 10])
        ~=? parseMoneys "(10)",
    "sum with addition" ~:
        Just (Moneys [Sum 25])
        ~=? parseMoneys "10 + 20 - 5",
    "sum with multiplication" ~:
        Just (Moneys [Sum 40])
        ~=? parseMoneys "10 * 20 / 5",
    "sum with expression" ~:
        Just (Moneys [Sum 205])
        ~=? parseMoneys "10 * 20 + 5",
    "sum with expression - multiplication takes precedence" ~:
        Just (Moneys [Sum 110])
        ~=? parseMoneys "10 + 20 * 5",
    "sum with expression with parenthesis" ~:
        Just (Moneys [Sum 150])
        ~=? parseMoneys "(10 + 20) * 5",
    "sum with expression and currency" ~:
        Just (Moneys [Money 150 "USD"])
        ~=? parseMoneys "(10 + 20) * 5 USD",
    "sum with expression and multiple currencies" ~:
        Just (Moneys [Money 30 "USD", Sum 46, Money 25 "RUB"])
        ~=? parseMoneys "10 + 20 USD 5 * 10 - 4 30 / 2 + 10 RUB",

    -- errors
    "no sum" ~:
        Nothing
        ~=? parseMoneys "()",
    "currency can only be in the end of expression" ~:
        Nothing
        ~=? parseMoneys "10 RUB + 20 USD"
    ]


parseTransactionsTest = test [
    "minimal" ~:
        Just [transaction]
        ~=? parseTransactions "A > 50 > B",

    "no sum" ~:
        Just [transaction { Transaction.sum = Nothing }]
        ~=? parseTransactions "A > B",
    "auto sum" ~:
        Just [transaction { Transaction.sum = Nothing }]
        ~=? parseTransactions "A > _ > B",

    "no beneficators" ~:
        Just [transaction { beneficators = payers transaction }]
        ~=? parseTransactions "A > 50",
    "auto beneficators" ~:
        Just [transaction { beneficators = payers transaction }]
        ~=? parseTransactions "A > 50 > _",

    "no sum nor beneficators" ~:
        Just [transaction { beneficators = payers transaction, Transaction.sum = Nothing }]
        ~=? parseTransactions "A",
    "auto sum and beneficators" ~:
        Just [transaction { beneficators = payers transaction, Transaction.sum = Nothing }]
        ~=? parseTransactions "A > _ > _",

    "sum with currency" ~:
        Just [transaction { Transaction.sum = Just $ Moneys [Money 50 "EUR"] }]
        ~=? parseTransactions "A > 50 EUR > B",
    "sum with several currencies" ~:
        Just [transaction { Transaction.sum = Just $ Moneys [Money 100 "USD", Sum 10, Money 50 "EUR"] }]
        ~=? parseTransactions "A > 50 EUR 10 100 USD > B",

    "minus side" ~:
        Just [transaction { payers = [RawSideRemove "A"] }]
        ~=? parseTransactions "-A > 50 > B",
    "side with factor" ~:
        Just [transaction { payers = [RawSideWithFactor "A" 2] }]
        ~=? parseTransactions "A * 2 > 50 > B",
    "side with factor arithmetics" ~:
        Just [transaction { payers = [RawSideWithFactor "A" 8] }]
        ~=? parseTransactions "A * 2+3*2 > 50 > B",
    "side with money" ~:
        Just [transaction { payers = [RawSideWithMoney "A" $ Moneys [Sum 50]] }]
        ~=? parseTransactions "A 50 > 50 > B",
    "side with money with currency" ~:
        Just [transaction { payers = [RawSideWithMoney "A" $ Moneys [Money 50 "EUR"]] }]
        ~=? parseTransactions "A 50 EUR > 50 > B",
    "side with plus money" ~:
        Just [transaction { payers = [RawSideWithSummand "A" $ Moneys [Sum 10]] }]
        ~=? parseTransactions "A + 10 > 50 > B",
    "side with minus money" ~:
        Just [transaction { payers = [RawSideWithSummand "A" $ Moneys [Sum (-10)]] }]
        ~=? parseTransactions "A + -10 > 50 > B",

    "side override" ~:
        Just [transaction { payers = [RawSideOverride $ RawSide "A"] }]
        ~=? parseTransactions "=A > 50 > B",
    "side override with minus" ~:
        Just [transaction { payers = [RawSideOverride $ RawSideRemove "A"] }]
        ~=? parseTransactions "=-A > 50 > B",
    "side override with factor" ~:
        Just [transaction { payers = [RawSideOverride $ RawSideWithFactor "A" 2] }]
        ~=? parseTransactions "=A * 2 > 50 > B",
    "side override with money" ~:
        Just [transaction { payers = [RawSideOverride $ RawSideWithMoney "A" $ Moneys [Sum 50]] }]
        ~=? parseTransactions "=A 50 > 50 > B",
    "side override with plus money" ~:
        Just [transaction { payers = [RawSideOverride $ RawSideWithSummand "A" $ Moneys [Sum 10]] }]
        ~=? parseTransactions "=A + 10 > 50 > B",

    "several sides" ~:
        Just [transaction {
            payers = [RawSide "A", RawSide "B"],
            beneficators = [RawSide "C", RawSide "D"] }]
        ~=? parseTransactions "A, B > 50 > C, D",

    "comment" ~:
        Just [transaction { comment = Just $ Comment "comment" }]
        ~=? parseTransactions "A > 50 > B : comment",
    "multiword comment" ~:
        Just [transaction { comment = Just $ Comment "long comment" }]
        ~=? parseTransactions "A > 50 > B : \"long comment\"",

    "internal" ~:
        Just [transaction { contragent = Just Internal }]
        ~=? parseTransactions "A > 50 > B @ internal",
    "contragent" ~:
        Just [transaction { contragent = Just $ Contragent "shop" }]
        ~=? parseTransactions "A > 50 > B @ shop",
    "multiword contragent" ~:
        Just [transaction { contragent = Just $ Contragent "long shop" }]
        ~=? parseTransactions "A > 50 > B @ \"long shop\"",
    
    "empty category" ~:
        Just [transaction]
        ~=? parseTransactions "A > 50 > B {}",
    "category" ~:
        Just [transaction { category = Category ["category"] }]
        ~=? parseTransactions "A > 50 > B {category}",
    "multiword category" ~:
        Just [transaction { category = Category ["long category"] }]
        ~=? parseTransactions "A > 50 > B {\"long category\"}",
    "hieracical category" ~:
        Just [transaction { category = Category ["category", "sub category"] }]
        ~=? parseTransactions "A > 50 > B {category, \"sub category\"}",

    "empty tags" ~:
        Just [transaction]
        ~=? parseTransactions "A > 50 > B []",
    "tag" ~:
        Just [transaction { tags = [Tag "tag"] }]
        ~=? parseTransactions "A > 50 > B [tag]",
    "multiword tag" ~:
        Just [transaction { tags = [Tag "long tag"] }]
        ~=? parseTransactions "A > 50 > B [\"long tag\"]",
    "tags" ~:
        Just [transaction { tags = [Tag "tag", Tag "long tag"] }]
        ~=? parseTransactions "A > 50 > B [tag, \"long tag\"]",
    "same tags" ~:
        Just [transaction { tags = [Tag "tag"] }]
        ~=? parseTransactions "A > 50 > B [tag, tag]",

    "all together - different orders" ~:
        Just (replicate 6 transaction {
            contragent = Just $ Contragent "shop",
            category = Category ["food"],
            tags = [Tag "austria"],
            comment = Just $ Comment "comment" })
        ~=? parseTransactions
               ("A > 50 > B @ shop [austria] {food} : comment"
            +++ "A > 50 > B @ shop {food} [austria] : comment"
            +++ "A > 50 > B [austria] @ shop {food} : comment"
            +++ "A > 50 > B [austria] {food} @ shop : comment"
            +++ "A > 50 > B {food} [austria] @ shop : comment"
            +++ "A > 50 > B {food} @ shop [austria] : comment"),

    "with dates" ~:
        Just [
            transaction { comment = Just $ Comment "a", date = Nothing },
            transaction { comment = Just $ Comment "b", date = Just $ StringDate "23.05.12" },
            transaction { comment = Just $ Comment "c", date = Just $ StringDate "23.05.12" },
            transaction { comment = Just $ Comment "d", date = Just $ StringDate "25.05.12" },
            transaction { comment = Just $ Comment "e", date = Nothing }]
        ~=? parseTransactions
               ("A > 50 > B : a"
            +++ "date \"23.05.12\""
            +++ "A > 50 > B : b"
            +++ "A > 50 > B : c"
            +++ "date \"25.05.12\""
            +++ "A > 50 > B : d"
            +++ "date _"
            +++ "A > 50 > B : e"),
    
    "with dates - different formats" ~:
        Just [
            transaction { date = Just $ StructDate 2020 1 22 },
            transaction { date = Just $ StructDate 2021 2  3 },
            transaction { date = Just $ StructDate 2022 3 24 },
            transaction { date = Just $ StructDate 2023 4  5 },
            transaction { date = Just $ StructDate 2024 5 26 }]
        ~=? parseTransactions (
               ("date 2020-01-22 A > 50 > B"
            +++ "date     2/3/21 A > 50 > B"
            +++ "date 03/24/2022 A > 50 > B"
            +++ "date     5.4.23 A > 50 > B")
            +++ "date 26.05.2024 A > 50 > B")
    ]
    where
        transaction = Transaction
            [RawSide "A"] [RawSide "B"] (Just $ Moneys [Sum 50])
            Nothing Nothing (Category []) [] Nothing


parseParameters s = do
    Input parameters _ _ _ <- build s
    return parameters

parseGroups s = do
    Input _ groups _ _ <- build s
    return groups

parseFxes s = do
    Input _ _ fxes _ <- build s
    return fxes

parseMoneys s = do
    Input _ _ _ transactions <- build $ "A > " ++ s ++ " > A"
    return $ fromJust $ Transaction.sum $ head transactions

parseTransactions s = do
    Input _ _ _ transactions <- build s
    return transactions

build s =
    case parseString s of
        Ok parsed -> Just $ buildInputData parsed
        Error _ -> Nothing



tests = test [
    parseParametersTest,
    parseGroupsTest,
    parseFxesTest,
    parseMoneysTest,
    parseTransactionsTest
    ]
