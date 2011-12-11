module LexerTest
where

import Test.HUnit
import Lexer


parseTests = test [
    "whitespaces" ~: [] ~=? alexScanTokens " a  ",
    ]

tests = test [parseTests]

