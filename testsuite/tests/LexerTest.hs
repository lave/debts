module LexerTest
where

import Test.HUnit
import Lexer


parseTests = test [
    "whitespaces" ~:
        [TokenString "a"]
        ~=? alexScanTokens " a  "
    ]

tests = test [parseTests]

