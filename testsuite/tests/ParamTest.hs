module ParamTest
where

import Test.HUnit
import Param


descriptors = [
    Param "string" StringParameter Override,
    Param "number" NumberParameter Override,
    Param "number1" NumberParameter NoOverride,
    Param "strings" StringParameter (Concatenate ";"),
    Param "bool" BoolParameter Override
    ]
    

makeAndGetTests = test [
    "test string" ~:
        Just "a" ~=? getStringParam params "string",
    "test number override" ~:
        Just 2 ~=? getNumberParam params "number",
    "test number no override" ~:
        Just 1 ~=? getNumberParam params "number1",
    "test string concatenate" ~:
        Just "a;b;c" ~=? getStringParam params "strings",
    "test strings" ~:
        ["a", "b", "c"] ~?= getStringsParam params "strings"
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


boolTest = test [
    "not defined" ~:
        False ~=? getBoolFrom [],
    "defined empty" ~:
        True  ~=? getBoolFrom [("bool", "")],
    "defined 'true'" ~:
        True  ~=? getBoolFrom [("bool", "true")],
    "defined 'TRUE'" ~:
        True  ~=? getBoolFrom [("bool", "TRUE")],
    "defined 'false'" ~:
        False ~=? getBoolFrom [("bool", "false")],
    "defined 'FALSE'" ~:
        False ~=? getBoolFrom [("bool", "FALSE")],
    "defined something else" ~:
        False ~=? getBoolFrom [("bool", "some")]
    ]
    where
        getBoolFrom rawParams =
            getBoolParam (makeParams descriptors rawParams) "bool"


tests = test [makeAndGetTests, boolTest]

