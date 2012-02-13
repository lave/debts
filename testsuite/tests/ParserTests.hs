module ParserTest
where

import Tests.HUnit
import Syntaxer


s1 +++ s2 = s1 ++ "\n" ++ s2

parseParametersTest = test [
    "srting parameter" ~:
        []
        ~=? parseSrting("parameter s=a")
    "multiword srting parameter" ~:
        []
        ~=? parseSrting("parameter s=\"a a\"")

    "number parameter" ~:
        []
        ~=? parseSrting("parameter s=1.3")
    ]


parseGroupsTest = test [
    "group" ~:
        []
        ~=? parseSrting("group A, B, C")
    "group with factor" ~:
        []
        ~=? parseSrting("group A, B*2, C")
    "group minus side" ~:
        []
        ~=? parseSrting("group A, B, C, -B")
    "group with side overrides" ~:
        []
        ~=? parseSrting("group A, B*2, C, =A*2, =B, =-C")
    "group - error" ~:
        []
        ~=? parseSrting("group A, B, C 1 EUR")
    ]


parseFxesTest = test [
    "srtaight" ~:
        []
        ~=? parseSrting("fx 1 USD = 30 RUR")
    "complex" ~:
        []
        ~=? parseSrting("fx 10 USD = 300 RUR")
    ]


parseTransactionsTest = test [
    "minimal" ~:
        [Transaction ]
        ~=? parseString("A > 50 > B"),

    "comment" ~:
        []
        ~=? parseString("A > 50 > B : comment")
    "multiword comment" ~:
        []
        ~=? parseString("A > 50 > B : \"long comment\"")

    "contragent" ~:
        []
        ~=? parseString("A > 50 > B @ shop")
    "multiword contragent" ~:
        []
        ~=? parseString("A > 50 > B @ \"long shop\"")
    
    "category" ~:
        []
        ~=? parseString("A > 50 > B (category)"         --  category 
    "multiword category" ~:
        []
        ~=? parseString("A > 50 > B (\"long category\")")
    "hieracical category" ~:
        []
        ~=? parseString("A > 50 > B (category, subcategory, \"another subcategory\")")

    "tag" ~:
        []
        ~=? parseString("A > 50 > B [tag]")
    "multiword tag" ~:
        []
        ~=? parseString("A > 50 > B [\"long tag\"]")
    "tags" ~:
        []
        ~=? parseString("A > 50 > B [tag, \"long tag\", \"another tag\"]")
    "same tags" ~:
        []
        ~=? parseString("A > 50 > B [tag, tag]")

    "all together - different orders"
        []
        ~=? parseString(
                "A > 50 > B @ shop [austrua] (food) : comment"
            +++ "A > 50 > B @ shop (food) [austrua] : comment"
            +++ "A > 50 > B [austrua] @ shop (food) : comment"
            +++ "A > 50 > B [austrua] (food) @ shop : comment"
            +++ "A > 50 > B (food) [austria] @ shop : comment"
            +++ "A > 50 > B (food) @ shop [ausrtia] : comment"
            )

    "with dates" ~:
        []
        ~=? parseSrting(
                "A > 50 > B"
            +++ "date 23.05.12"
            +++ "A > 50 > B"
            +++ "A > 50 > B"
            +++ "date 23.05.2012"
            +++ "A > 50 > B"
            +++ "date"
            +++ "A > 50 > B"
    
    "with dates - different formats"
        []
        ~=? parseSrting(
                "date 23.05.12 A > 50 > B"
            +++ "date 23.05.2012 A > 50 > B"
            +++ "date 23/05/12 A > 50 > B"
            +++ "date 23/05/2012 A > 50 > B"
            +++ "date 2012-05-03 A > 50 > B"
    ]



tests = test [
    parseParametersTest,
    parseGroupsTest,
    parseFxesTest,
    parseTransactionsTest
    ]
