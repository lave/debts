module PrintTableTest
where

import Test.HUnit

import PrintTable
import Utils


padTest = test [
    "pad right" ~:
        ["AA  ", "AAAA", "A..."] ~=? map (padRight 4) ["AA", "AAAA", "AAAAA"],

    "pad left" ~:
        ["  AA", "AAAA", "A..."] ~=? map (padLeft 4) ["AA", "AAAA", "AAAAA"],

    "pad center" ~:
        [" A  ", " AA ", "AAAA", "A..."] ~=? map (padBoth 4) ["A", "AA", "AAAA", "AAAAA"]
    ]


makeTableTest = test [
    "test fill" ~:
        Table [
            Row [Cell AlignCenter "A", Cell AlignCenter "B"],
            Separator,
            Row [Cell AlignLeft "a", Cell AlignLeft "b"],
            Row [Cell AlignLeft "c", Cell AlignLeft "d"],
            Separator,
            Row [Cell AlignLeft "e", Cell AlignLeft "f"]]
        ~=? tableBuilder
            |> addHeader ["A", "B"]
            |> addSeparator
            |> addRows [["a", "b"],["c", "d"]]
            |> addSeparator
            |> addRow ["e", "f"]
            |> build,

    "test align" ~:
        Table [
            Row [Cell AlignLeft "a", Cell AlignLeft "b"],
            Row [Cell AlignRight "0", Cell AlignRight "1"],
            Row [Cell AlignLeft "2", Cell AlignRight "3"]]
        ~=? tableBuilder
            |> addRow ["a", "b"]
            |> setGlobalAlign AlignRight
            |> addRow ["0", "1"]
            |> setAlign 0 AlignLeft
            |> addRow ["2", "3"]
            |> build
    ]


toStringsTest = test [
    "test width" ~:
        ["AA B  C ",
         "--------",
         "a  bb c ",
         "a  b  cc"]
        ~=? toStrings " " (Table
            [Row [Cell AlignCenter "AA", Cell AlignCenter "B", Cell AlignCenter "C"],
             Separator,
             Row [Cell AlignLeft "a", Cell AlignLeft "bb", Cell AlignLeft "c"],
             Row [Cell AlignLeft "a", Cell AlignLeft "b", Cell AlignLeft "cc"]
            ]),

    "test align" ~:
        ["A | B ",
         "------",
         "a |bb ",
         " a| bb",
         "aa|bbb"]
        ~=? toStrings "|" (Table
            [Row [Cell AlignCenter "A", Cell AlignCenter "B"],
             Separator,
             Row [Cell AlignLeft "a", Cell AlignLeft "bb"],
             Row [Cell AlignRight "a", Cell AlignRight "bb"],
             Row [Cell AlignCenter "aa", Cell AlignCenter "bbb"]
            ])
    ]


tests = test [padTest, makeTableTest, toStringsTest]
