module CommandLineTest
where

import Test.HUnit
import CommandLine


optionsTest = test [
    "help" ~:
        True ~=? (optHelp $ options $ getOptions ["-h"]),
    "verbose" ~:
        True ~=? (optVerbose $ options $ getOptions ["-v"]),
    "parameters" ~:
        reverse [("a", "1"), ("b", "a"), ("c", ""), ("d", "0=1")]
        ~=? (optParams $ options $ getOptions ["-Da=1", "-D", "b=a", "-Dc", "-Dd=0=1"]),
    "balance" ~:
        True ~=? (optBalance $ options $ getOptions ["-b"]),
    "log" ~:
        True ~=? (optLog $ options $ getOptions ["-l"]),
    "money log" ~:
        Just "Eric" ~=? (optMoneyLog $ options $ getOptions ["--ml", "Eric"]),
    "spending by category" ~:
        Just Nothing ~=? (optSpendingByCategory $ options $ getOptions ["--by-cat"]),
    "spending by category with limit" ~:
        Just (Just 2) ~=? (optSpendingByCategory $ options $ getOptions ["--by-cat=2"])
    ]

filesTest = test [
    "files" ~:
        ["file1", "file2"] ~=? (files $ getOptions ["-v", "file1", "--ml", "Eric", "file2"])
    ]

options (ops, _, _) = ops
files (_, fs, _) = fs

tests = test [optionsTest, filesTest]
