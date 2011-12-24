module DebtTest
where

import Test.HUnit
import Debt
import Utils
import Money
import Transaction


addSidesTests = test [
    "add sides" ~: same
        [Side "stan" (Moneys []),
         Side "eric" (Moneys [Sum 10]),
         Side "kyle" (Moneys [Sum 20]),
         Side "kenny" (Moneys [])]
        (addSides
            [Side "eric" (Moneys [Sum 10]),
             Side "kenny" (Moneys [Sum 10])]
            [Side "stan" (Moneys []),
             Side "kenny" (Moneys [Sum (-10)]),
             Side "kyle" (Moneys [Sum 20])]) ~? ""
    ]


subSidesTests = test [
    "subtract sides" ~: same
        [Side "stan" (Moneys []),
         Side "eric" (Moneys [Sum 10]),
         Side "kyle" (Moneys [Sum (-20)]),
         Side "kenny" (Moneys [Sum 20])]
        (subSides
            [Side "eric" (Moneys [Sum 10]),
             Side "kenny" (Moneys [Sum 10])]
            [Side "stan" (Moneys []),
             Side "kenny" (Moneys [Sum (-10)]),
             Side "kyle" (Moneys [Sum 20])]) ~? ""
    ]


calcTest = test [
    "" ~: same
        [Side "stan" (Moneys [Sum 20]),
         Side "eric" (Moneys [Sum (-10)]),
         Side "kyle" (Moneys [Sum (-20)]),
         Side "kenny" (Moneys [Sum 10])]
        (calc balance [
            Transaction
                [Side "stan" (Moneys [Sum 30])] 
                [Side "eric" (Moneys [Sum 10]),
                 Side "kyle" (Moneys [Sum 10]),
                 Side "stan" (Moneys [Sum 10])]
                "stan bought beer for him, eric and kyle",
            Transaction
                [Side "eric" (Moneys [Sum 10]),
                 Side "stan" (Moneys [Sum 20])]
                [Side "eric" (Moneys [Sum 10]),
                 Side "kyle" (Moneys [Sum 10]),
                 Side "stan" (Moneys [Sum 10])]
                "stans paid for kyle",
            Transaction
                [Side "kenny" (Moneys [Sum 10])]
                [Side "stan" (Moneys [Sum 10])]
                "kenny gave money to stan"
        ]) ~? ""
    ]


tests = test [addSidesTests, subSidesTests, calcTest]

