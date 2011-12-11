module RunTests
where

import Test.HUnit

import DebtTest
import FxTest
import LexerTest
import MoneyTest
import TransactionTest
import UtilsTest

allTests = TestList [
        DebtTest.tests,
        FxTest.tests,
        LexerTest.tests,
        MoneyTest.tests,
        TransactionTest.tests,
        UtilsTest.tests
    ]
    
main = runTestTT allTests
