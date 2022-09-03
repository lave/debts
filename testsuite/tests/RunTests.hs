module Main
where

import System.Exit
import Test.HUnit

import AggregationTest
import BalanceTest
import CommandLineTest
import FxTest
import LexerTest
import MoneyTest
import MoneyLogTest
import NormalizeTest
import ParamTest
import ParserTest
import PreprocessTest
import PrintTableTest
import RoundTest
import UtilsTest


allTests = TestList [
        AggregationTest.tests,
        BalanceTest.tests,
        CommandLineTest.tests,
        FxTest.tests,
        LexerTest.tests,
        MoneyTest.tests,
        MoneyLogTest.tests,
        NormalizeTest.tests,
        ParserTest.tests,
        ParamTest.tests,
        PreprocessTest.tests,
        PrintTableTest.tests,
        RoundTest.tests,
        UtilsTest.tests
    ]
    
main = do
    counters <- runTestTT allTests
    let failuresCount = errors counters + failures counters

    if failuresCount == 0
        then exitSuccess
        else exitWith $ ExitFailure failuresCount
