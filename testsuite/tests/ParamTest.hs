module ParamTest
where

import Test.HUnit
import Param


descriptors = [
    Param "string" StringParameter Override,
    Param "number" NumberParameter Override,
    Param "number1" NumberParameter NoOverride,
    Param "strings" StringParameter (Concatenate ";")
    ]
    

addTests = test [
    "test string" ~:
        Just "a" ~=? getStringParam params "string",
    "test number override" ~:
        Just 2 ~=? getNumberParam params "number",
    "test number no override" ~:
        Just 1 ~=? getNumberParam params "number1",
    "test string concatenate" ~:
        Just "a;b;c" ~=? getStringParam params "strings"
--    "test strings" ~:
--        ["a", "b", "c"] ~?= getStringsParam params "strings"
    ]
    where
        params = makeParams descriptors [
            ("string", "a"),
            ("number", "1"),
            ("number", "2"),
            ("number1", "1"),
            ("number1", "2"),
            ("strings", "a"),
            ("strings", "b;c")
            ]

tests = test [addTests]

