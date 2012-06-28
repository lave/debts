module RunTests
where

import Test.HUnit

import CommandLineTest
import DebtTest
import FxTest
import LexerTest
import MoneyTest
import ParamTest
import ParserTest
import RoundTest
import TransactionTest
import UtilsTest


allTests = TestList [
        CommandLineTest.tests,
        DebtTest.tests,
        FxTest.tests,
        LexerTest.tests,
        MoneyTest.tests,
        ParserTest.tests,
        ParamTest.tests,
        RoundTest.tests,
        TransactionTest.tests,
        UtilsTest.tests
    ]
    
main = runTestTT allTests
