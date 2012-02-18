module RunTests
where

import Test.HUnit

import DebtTest
import FxTest
import LexerTest
import MoneyTest
import ParserTest
import TransactionTest
import UtilsTest


allTests = TestList [
        DebtTest.tests,
        FxTest.tests,
        LexerTest.tests,
        MoneyTest.tests,
        ParserTest.tests,
        TransactionTest.tests,
        UtilsTest.tests
    ]
    
main = runTestTT allTests
