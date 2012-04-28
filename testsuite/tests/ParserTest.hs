module ParserTest
where

import Test.HUnit

import Maybe

import Date
import Fx
import InputBuilder
import Money
import Param
import Parser
import ParserMonad
import Transaction
import Utils



s1 +++ s2 = s1 ++ "\n" ++ s2

parseParametersTest = test [
    "srting parameter" ~:
        Just [StringParam "s" "a"]
        ~=? parseParameters "param s=a",
    "multiword srting parameter" ~:
        Just [StringParam "s" "a a"]
        ~=? parseParameters "param s=\"a a\"",

    "number parameter" ~:
        Just [NumberParam "s" 1.3]
        ~=? parseParameters "param s=1.3"
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


instance (Eq group) => Eq (Transaction_ group) where
    t1 == t2 =
        same (payers t1) (payers t2) &&
        same (beneficators t1) (beneficators t2) &&
        Transaction.sum t1 == Transaction.sum t2 &&
        date t1 == date t2 &&
        contragent t1 == contragent t2 &&
        category t1 == category t2 &&
        same (tags t1) (tags t2) &&
        comment t1 == comment t2
        
parseTransactionsTest = test [
    "minimal" ~:
        Just [transaction]
        ~=? parseTransactions "A > 50 > B",

    "no sum" ~:
        Just [transaction { Transaction.sum = Nothing }]
        ~=? parseTransactions "A > _ > B",
    "sum with currency" ~:
        Just [transaction { Transaction.sum = Just $ Moneys [Money 50 "EUR"] }]
        ~=? parseTransactions "A > 50 EUR > B",

    "no beneficators" ~:
        Just [transaction { beneficators = payers transaction }]
        ~=? parseTransactions "A > 50 > _",

    "side with factor" ~:
        Just [transaction { payers = [RawSideWithFactor "A" 2] }]
        ~=? parseTransactions "A * 2 > 50 > B",
    "side with money" ~:
        Just [transaction { payers = [RawSideWithMoney "A" $ Moneys [Sum 50]] }]
        ~=? parseTransactions "A 50 > 50 > B",
    "side with money with currency" ~:
        Just [transaction { payers = [RawSideWithMoney "A" $ Moneys [Money 50 "EUR"]] }]
        ~=? parseTransactions "A 50 EUR > 50 > B",
    "minus side" ~:
        Just [transaction { payers = [RawSideRemove "A"] }]
        ~=? parseTransactions "-A > 50 > B",
    "side override" ~:
        Just [transaction { payers = [RawSideOverride $ RawSide "A"] }]
        ~=? parseTransactions "=A > 50 > B",
    "side override with factor" ~:
        Just [transaction { payers = [RawSideOverride $ RawSideWithFactor "A" 2] }]
        ~=? parseTransactions "=A * 2 > 50 > B",
    "side override with money" ~:
        Just [transaction { payers = [RawSideOverride $ RawSideWithMoney "A" $ Moneys [Sum 50]] }]
        ~=? parseTransactions "=A 50 > 50 > B",
    "side override with minus" ~:
        Just [transaction { payers = [RawSideOverride $ RawSideRemove "A"] }]
        ~=? parseTransactions "=-A > 50 > B",

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
        ~=? parseTransactions "A > 50 > B ()",
    "category" ~:
        Just [transaction { category = [Category "category"] }]
        ~=? parseTransactions "A > 50 > B (category)",
    "multiword category" ~:
        Just [transaction { category = [Category "long category"] }]
        ~=? parseTransactions "A > 50 > B (\"long category\")",
    "hieracical category" ~:
        Just [transaction { category = [Category "category", Category "sub category"] }]
        ~=? parseTransactions "A > 50 > B (category, \"sub category\")",

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
            category = [Category "food"],
            tags = [Tag "austria"],
            comment = Just $ Comment "comment" })
        ~=? parseTransactions
               ("A > 50 > B @ shop [austria] (food) : comment"
            +++ "A > 50 > B @ shop (food) [austria] : comment"
            +++ "A > 50 > B [austria] @ shop (food) : comment"
            +++ "A > 50 > B [austria] (food) @ shop : comment"
            +++ "A > 50 > B (food) [austria] @ shop : comment"
            +++ "A > 50 > B (food) @ shop [austria] : comment"),

    "with dates" ~:
        Just [
            transaction { comment = Just $ Comment "a", date = Nothing },
            transaction { comment = Just $ Comment "b", date = Just $ Date "23.05.12" },
            transaction { comment = Just $ Comment "c", date = Just $ Date "23.05.12" },
            transaction { comment = Just $ Comment "d", date = Just $ Date "25.05.12" },
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
            transaction { date = Just $ Date "23.05.12" },
            transaction { date = Just $ Date "23.05.2012" },
            transaction { date = Just $ Date "23/05/12" },
            transaction { date = Just $ Date "23/05/2012" },
            transaction { date = Just $ Date "2012-05-03" }]
        ~=? parseTransactions
               ("date \"23.05.12\" A > 50 > B"
            +++ "date \"23.05.2012\" A > 50 > B"
            +++ "date \"23/05/12\" A > 50 > B"
            +++ "date \"23/05/2012\" A > 50 > B"
            +++ "date \"2012-05-03\" A > 50 > B")
    ]
    where
        transaction = Transaction
            [RawSide "A"] [RawSide "B"] (Just $ Moneys [Sum 50])
            Nothing Nothing [] [] Nothing


parseParameters s = do
    Input parameters _ _ _ <- build s
    return parameters

parseGroups s = do
    Input _ groups _ _ <- build s
    return groups

parseFxes s = do
    Input _ _ fxes _ <- build s
    return fxes

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
    parseTransactionsTest
    ]
