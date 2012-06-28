module CommandLineTest
where

import Test.HUnit
import CommandLine


containsKeyTest = test [
    "contains key" ~:
        containsKey ["-a", "-b", "-c"] "-a" ~? "",
    "contains non-key" ~:
        containsKey ["a", "b", "c"] "a" ~? "",
    "doesn't contain ley" ~:
        not (containsKey ["-a", "-b", "-c"] "-d") ~? ""
    ]


findParametersTest = test [
    "empty" ~:
        [] ~=? findParameters [],
    "empty string" ~:
        [] ~=? findParameters [""],
    "non-parameters" ~:
        [] ~=? findParameters ["a", "-a", "-d"],
    "empty parameter" ~:
        [] ~=? findParameters ["-D"],
    "empty parameter name" ~:
        [] ~=? findParameters ["-D=a"],
    "empty parameter value" ~:
        [("a", ""), ("b", "")] ~=? findParameters ["-Da=", "-Db"],
    "parameter value with '=' char" ~:
        [("a", "b=c")] ~=? findParameters ["-Da=b=c"],
    "parameters" ~:
        [("a", "1"), ("b", "2")] ~=? findParameters ["-Da=1", "-Db=2"],
    "same parameters" ~:
        [("a", "1"), ("a", "2")] ~=? findParameters ["-Da=1", "-Da=2"],
    "mixed parameters and non-parameters" ~:
        [("a", "1"), ("a", "2")] ~=? findParameters ["-Da=1", "a", "-Da=2"]
    ]


tests = test [containsKeyTest, findParametersTest]
