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
        [TokenNumber 0,
         TokenNumber 0,
         TokenNumber 12,
         TokenNumber (-34),
         TokenNumber 0.0,
         TokenNumber 0.0,
         TokenNumber 12.3,
         TokenNumber (-45.6)]
        ~=? scan "0 -0 12 -34 0.0 -0.0 12.3 -45.6",

    "strings" ~:
        [TokenString "abc",
         TokenString "def ghi"]
        ~=? scan "abc \"def ghi\"",

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

