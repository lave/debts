module ParamTest
where

import Test.HUnit
import Param


descriptors = [
    Param "d1" NumberParameter NoOverride,
    Param "d2" NumberParameter Override,
    Param "d3" NumberParameter Multiple,
    Param "s1" StringParameter NoOverride,
    Param "s2" StringParameter Override,
    Param "s3" StringParameter Multiple
    ]

addParamTests = test [
    "just add" ~:
        Params descriptors [
            StringParam "s1" "a",
            NumberParam "d1" 1,
            StringParam "s2" "b",
            NumberParam "d2" 2,
            StringParam "s3" "c",
            NumberParam "d3" 3]
        ~=? foldl (\params (name, value) -> addParam (Params descriptors []) name value) [
            ("s1", "a"),
            ("d1", 1),
            ("s2", "b"),
            ("d2", 2),
            ("s3", "c"),
            ("d3", 3)]
    ]

tests = test [addParamTests]

