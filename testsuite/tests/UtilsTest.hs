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

testSetNth = test [
    "set in the middle" ~:
        [1, 4, 3] ~=? setNth 1 4 [1, 2, 3],
    "set last" ~:
        [1, 2, 4] ~=? setNth 2 4 [1, 2, 3],
    "set after last" ~:
        [1, 2, 3] ~=? setNth 3 4 [1, 2, 3]
    ]


tests = test [testSame, testTrim, testSetNth]

