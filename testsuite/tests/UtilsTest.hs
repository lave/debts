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
        

tests = test [testSame, testTrim]

