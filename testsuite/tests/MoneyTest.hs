module MoneyTest
where

import Test.HUnit
import Money


addTests = test [
    "empty + empty" ~:
        (Moneys []) ~=?
        (add (Moneys []) (Moneys [])),

    "sum + empty" ~:
        (Moneys [Sum 100]) ~=?
        (add (Moneys [Sum 100]) (Moneys [])),
    "empty + sum" ~:
        (Moneys [Sum 50]) ~=?
        (add (Moneys []) (Moneys [Sum 50])),
    "sum + sum" ~:
        (Moneys [Sum 150]) ~=?
        (add (Moneys [Sum 100]) (Moneys [Sum 50])),
    "sum + -sum" ~:
        (Moneys []) ~=?
        (add (Moneys [Sum 100]) (Moneys [Sum (-100)])),

    "empty + money" ~:
        (Moneys [Money 100 "RUR"]) ~=?
        (add (Moneys [])
             (Moneys [Money 100 "RUR"])),
    "money + empty" ~:
        (Moneys [Money 100 "RUR"]) ~=?
        (add (Moneys [Money 100 "RUR"])
             (Moneys [])),
    "sum + money" ~:
        (Moneys [Sum 100, Money 100 "RUR"]) ~=?
        (add (Moneys [Sum 100])
             (Moneys [Money 100 "RUR"])),
    "money + sum" ~:
        (Moneys [Sum 100, Money 100 "RUR"]) ~=?
        (add (Moneys [Money 100 "RUR"])
             (Moneys [Sum 100])),
    "RUR + UAH" ~:
        (Moneys [Money 100 "RUR", Money 50 "UAH"]) ~=?
        (add (Moneys [Money 100 "RUR"])
             (Moneys [Money 50 "UAH"])),
    "RUR + RUR" ~:
        (Moneys [Money 150 "RUR"]) ~=?
        (add (Moneys [Money 100 "RUR"])
             (Moneys [Money 50 "RUR"])),
    "RUR + -RUR" ~:
        (Moneys []) ~=?
        (add (Moneys [Money 100 "RUR"])
             (Moneys [Money (-100) "RUR"])),

    "complex" ~:
        (Moneys [Sum 150, Money 250 "RUR", Money 200 "EUR"]) ~=?
        (add (Moneys [Money 100 "RUR", Sum 100, Money 50 "USD"])
             (Moneys [Money 150 "RUR", Money (-50) "USD", Money 200 "EUR", Sum 50]))
    ]


mulTests = TestList [
    "f * empty" ~:
        (Moneys []) ~=?
        (mul 2 (Moneys [])),

    "f * sum" ~:
        (Moneys [Sum 100]) ~=?
        (mul 2 (Moneys [Sum 50])),
    "0 * sum" ~:
        (Moneys []) ~=?
        (mul 0 (Moneys [Sum 50])),

    "f * sum" ~:
        (Moneys [Money 100 "RUR"]) ~=?
        (mul 2 (Moneys [Money 50 "RUR"])),
    "0 * sum" ~:
        (Moneys []) ~=?
        (mul 0 (Moneys [Money 50 "RUR"])),

    "f * complex" ~:
        (Moneys [Money 100 "RUR", Sum 200, Money 40 "USD"]) ~=?
        (mul 2 (Moneys [Money 50 "RUR", Sum 100, Money 20 "USD"])),
    "0 * complex" ~:
        (Moneys []) ~=?
        (mul 0 (Moneys [Money 50 "RUR", Sum 100, Money 20 "USD"]))
    ]


-- sub is a composition if add and mul, so we don't test it so thoroughtly - only common cases
subTests = TestList [
    "complex" ~:
        (Moneys [Sum 50, Money (-50) "RUR", Money 100 "USD", Money (-200) "EUR"]) ~=?
        (sub (Moneys [Money 100 "RUR", Sum 100, Money 50 "USD"])
             (Moneys [Money 150 "RUR", Money (-50) "USD", Money 200 "EUR", Sum 50])),
    "complex  - complex" ~:
        (Moneys []) ~=?
        (sub (Moneys [Money 100 "RUR", Sum 100, Money 50 "USD"])
             (Moneys [Money 100 "RUR", Sum 100, Money 50 "USD"]))
    ]



tests = test [eqTest, addTests, mulTests, subTests]

