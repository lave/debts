module UtilsTest
where

import Test.HUnit
import Utils


testSame = test [
    "empty" ~:
        same ([] :: [Int]) [] ~? "",
    "equal" ~:
        same [1, 2, 3] [1, 2, 3] ~? "",
    "equal different order" ~:
        same [1, 2, 3] [1, 3, 2] ~? "",
    "not equal" ~:
        not (same [1, 2, 3] [1, 3]) ~? "",
    "not equal" ~:
        not (same [1, 2, 3] [1, 2, 4]) ~? ""
    ]


testTrim = test [
    "trim first empty" ~: "" ~=? trimFirst "",
    "trim first one" ~: "" ~=? trimFirst "a",
    "trim first many" ~: "bc" ~=? trimFirst "abc",

    "trim last empty" ~: "" ~=? trimLast "",
    "trim last one" ~: "" ~=? trimLast "a",
    "trim last many" ~: "ab" ~=? trimLast "abc",

    "trim both empty" ~: "" ~=? trimBoth "",
    "trim both one" ~: "" ~=? trimBoth "a",
    "trim both two" ~: "" ~=? trimBoth "ab",
    "trim both many" ~: "b" ~=? trimBoth "abc"
    ]


testContains = test [
    "contsins" ~: contains [1] [1, 2, 3] ~? "",
    "does not contsin" ~: not (contains [1] [2, 3]) ~? ""
    ]
        

testSplit = test [
    "empty string" ~: [""] ~=? split ',' "",
    "empty string" ~: ["a"] ~=? split ',' "a",
    "empty string" ~: ["", "a"] ~=? split ',' ",a",
    "empty string" ~: ["a", ""] ~=? split ',' "a,",
    "empty string" ~: ["a", "", "b"] ~=? split ',' "a,,b",
    "empty string" ~: ["a", "b", "c"] ~=? split ',' "a,b,c"
    ]


tests = test [testSame, testTrim, testContains, testSplit]

