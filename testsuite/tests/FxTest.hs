module FxTest
where

import Test.HUnit
import Money
import Fx
import Utils


addTests = test [
    "empty + fx" ~: same
        [Fx (Money 1 "RUR") (Money 5 "KZT")]
        (Fx.add [] (Fx (Money 1 "RUR") (Money 5 "KZT"))) ~? "",

    "fx1 + fx2" ~: same
        [Fx (Money 30 "RUR") (Money 1 "USD"), Fx (Money 1 "RUR") (Money 5 "KZT")]
        (Fx.add [Fx (Money 30 "RUR") (Money 1 "USD")] (Fx (Money 1 "RUR") (Money 5 "KZT"))) ~? ""
{-
    "fx1 + fx1" ~: fail
        (Fx.add [Fx (Money 30 "RUR") (Money 1 "USD")] (Fx (Money 30 "RUR") (Money 1 "USD"))) ~? "",

    "fx1 + fx1" ~: fail
        (Fx.add [Fx (Money 30 "RUR") (Money 1 "USD")] (Fx (Money 1 "USD") (Money 30 "RUR"))) ~? ""
-}
    ]

testFxs =
    [Fx (Money 30 "RUR") (Money 1 "USD"),
     Fx (Money 1 "RUR") (Money 5 "KZT")]


convertTests = test [
    "" ~:
        (Moneys [Money 50 "RUR"]) ~=?
        convert testFxs "RUR" (Moneys [Money 10 "RUR", Money 50 "KZT", Money 1 "USD"])
    ]

tests = test [addTests, convertTests]

