module FxTest
where

import Test.HUnit

import qualified Data.Map as Map

import Money
import Fx
import Utils


makeTests = test [
    "one fx" ~:
        [
            Map.fromList [("USD", 1), ("RUR", 30)]
        ]
        ~=?
        (make [
            (Fx (Money 1 "USD") (Money 30 "RUR"))
        ]),
    "disconnected fxes" ~:
        [
            Map.fromList [("USD", 1), ("RUR", 30)],
            Map.fromList [("EUR", 10), ("GBP", 8)]
        ]
        ~=?
        (make [
            (Fx (Money 1 "USD") (Money 30 "RUR")),
            (Fx (Money 10 "EUR") (Money 8 "GBP"))
        ]),
    "connected fxes - add one by one" ~:
        [
            Map.fromList [("USD", 1), ("RUR", 30), ("KZT", 150), ("AMD", 180)]
        ]
        ~=?
        (make [
            (Fx (Money 1 "USD") (Money 30 "RUR")),
            (Fx (Money 1 "RUR") (Money 5 "KZT")),
            (Fx (Money 6 "AMD") (Money 1 "RUR"))
        ]),
    "connected fxes - merge two disconnected" ~:
        [
            Map.fromList [("USD", 6), ("RUR", 180), ("KZT", 900), ("AMD", 1080)]
        ]
        ~=?
        (make [
            (Fx (Money 1 "USD") (Money 30 "RUR")),
            (Fx (Money 6 "AMD") (Money 5 "KZT")),
            (Fx (Money 1 "RUR") (Money 6 "AMD"))
        ]),
    "connected fxes - consistent duplicate" ~:
        [
            Map.fromList [("USD", 1), ("RUR", 30), ("KZT", 150)]
        ]
        ~=?
        (make [
            (Fx (Money 1 "USD") (Money 30 "RUR")),
            (Fx (Money 1 "RUR") (Money 5 "KZT")),
            (Fx (Money 1 "USD") (Money 150 "KZT"))
        ])
{-
    "connected fxes - inconsistent duplicate" ~:
        [
            Map.fromList [("USD", 1), ("RUR", 30), ("KZT", 150)]
        ]
        ~=?
        (make [
            (Fx (Money 1 "USD") (Money 30 "RUR")),
            (Fx (Money 1 "RUR") (Money 5 "KZT")),
            (Fx (Money 1 "USD") (Money 100 "KZT"))
        ])
-}
    ]

convertTests = test [
    "normal" ~:
        (Moneys [Money 50 "RUR"]) ~=?
        convert testFxs "RUR" (Moneys [Money 10 "RUR", Money 50 "KZT", Money 1 "USD"])
{-
    "no target currency" ~:
        (Moneys [Money 61 "RUR"]) ~=?
        convert testFxs "EUR" (Moneys [Money 10 "RUR", Money 50 "KZT", Money 1 "USD"]),
    "no source currency" ~:
        (Moneys [Money 61 "RUR"]) ~=?
        convert testFxs "RUR" (Moneys [Money 10 "EUR", Money 50 "KZT", Money 1 "USD"])
-}
    ]
    where
        testFxs = [Map.fromList [("USD", 1), ("RUR", 30), ("KZT", 150)]]


tests = test [makeTests, convertTests]
