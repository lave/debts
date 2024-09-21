module LexerTest
where

import Test.HUnit
import Lexer


parseTests = test [
    "symbols" ~:
        map TokenSym allowedSymbols
        ~=? scan allowedSymbols,

    "symbols" ~:
        map TokenKeyword allowedKeywords
        ~=? scan (unwords allowedKeywords),

    "numbers" ~:
        [TokenInteger 0,
         TokenInteger 0,
         TokenInteger 12,
         TokenInteger (-34),
         TokenNumber 0.0,
         TokenNumber 0.0,
         TokenNumber 12.3,
         TokenNumber (-45.6)]
        ~=? scan "0 -0 12 -34 0.0 -0.0 12.3 -45.6",

    "strings" ~:
        [TokenString "abc",
         TokenString "def ghi"]
        ~=? scan "abc \"def ghi\"",

    "dates" ~:
        [TokenDate 2020 1 22,
         TokenDate   21 2  3,
         TokenDate 2022 3 24,
         TokenDate   23 4  5,
         TokenDate 2024 5 26
        ]
        ~=? scan "2020-01-22 2/3/21 03/24/2022 5.4.23 26.05.2024",

    "whitespaces" ~:
        [TokenString "a"]
        ~=? scan " a  ",

    "comments" ~:
        [TokenString "a"]
        ~=? scan "a#bc"

    ]
    where
        scan = (map token_data) . alexScanTokens
        allowedSymbols = ">:,_*=-@()[]"
        allowedKeywords = ["param", "fx", "group", "date", "internal"]

tests = test [parseTests]

