module MoneyTest
where

import Test.HUnit
import Money


emptyTests = TestList [
    "empty" ~:
        empty (Moneys []) ~? "",
    "not empty" ~:
        not (empty (Moneys [Sum 10])) ~? "",
    "not empty" ~:
        not (empty (Moneys [Money 10 "RUR"])) ~? ""
    ]


equalTests = TestList[
    "both empty" ~:
        Moneys [] == Moneys [] ~? "",
    "both same" ~:
        Moneys [Sum 10, Money 10 "RUR"] == Moneys [Sum 10, Money 10 "RUR"] ~? "",
    "both same but have different order" ~:
        Moneys [Sum 10, Money 10 "RUR"] == Moneys [Money 10 "RUR", Sum 10] ~? "",
    "empty and non-empty" ~:
        Moneys [] /= Moneys [Sum 10] ~? "",
    "with currency and without currency" ~:
        Moneys [Money 10 "RUR"] /= Moneys [Sum 10] ~? "",
    "different quantity" ~:
        Moneys [Money 10 "RUR"] /= Moneys [Money 5 "RUR"] ~? "",
    "different currency" ~:
        Moneys [Money 10 "RUR"] /= Moneys [Money 10 "EUR"] ~? ""
    ]


addTests = TestList [
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
    "RUR + USD" ~:
        (Moneys [Money 100 "RUR", Money 50 "USD"]) ~=?
        (add (Moneys [Money 100 "RUR"])
             (Moneys [Money 50 "USD"])),
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


sumTests = TestList [
    "empty sum" ~:
        (Moneys []) ~=?
        (Money.sum []),
    "non-empty sum" ~:
        (Moneys [Sum 100, Money 50 "RUR", Money 100 "EUR", Money 50 "USD"]) ~=?
        (Money.sum [
            Moneys [Sum 100, Money 50 "EUR"],
            Moneys [Money 50 "RUR"],
            Moneys [Money 50 "USD", Money 50 "EUR"]])
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
    "subtract complex values" ~:
        (Moneys [Sum 50, Money (-50) "RUR", Money 100 "USD", Money (-200) "EUR"]) ~=?
        (sub (Moneys [Money 100 "RUR", Sum 100, Money 50 "USD"])
             (Moneys [Money 150 "RUR", Money (-50) "USD", Money 200 "EUR", Sum 50])),
    "subtract complex value from iteself" ~:
        (Moneys []) ~=?
        (sub (Moneys [Money 100 "RUR", Sum 100, Money 50 "USD"])
             (Moneys [Money 100 "RUR", Sum 100, Money 50 "USD"]))
    ]


intersectTests = TestList [
    "no intersection" ~:
        (Moneys []) ~=?
        (intersect (Moneys [Money 100 "RUR", Sum 100])
                   (Moneys [Money 100 "USD", Money 100 "EUR"])),
    "general intersection" ~:
        (Moneys [Money 50 "EUR", Sum 50]) ~=?
        (intersect (Moneys [Money 100 "RUR", Money 50 "EUR", Sum 100])
                   (Moneys [Money 100 "USD", Money 100 "EUR", Sum 50])),
    "intersect with subset" ~:
        (Moneys [Money 100 "RUR", Sum 100]) ~=?
        (intersect (Moneys [Money 100 "RUR", Money 50 "EUR", Sum 100])
                   (Moneys [Money 100 "RUR", Sum 100])),
    "intersect same values" ~:
        (Moneys [Money 100 "RUR", Sum 100]) ~=?
        (intersect (Moneys [Money 100 "RUR", Sum 100])
                   (Moneys [Money 100 "RUR", Sum 100]))
    ]


tests = TestList [emptyTests, equalTests, addTests, sumTests, mulTests, subTests, intersectTests]

