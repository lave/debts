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
    "balance" ~: same
        [Side "stan" (Moneys [Sum 20]),
         Side "eric" (Moneys [Sum (-10)]),
         Side "kyle" (Moneys [Sum (-20)]),
         Side "kenny" (Moneys [Sum 10])]
        (calc balance transactions) ~? "",

    "balance" ~: same
        [Side "stan" (Moneys [Sum 20]),
         Side "eric" (Moneys [Sum 20]),
         Side "kyle" (Moneys [Sum 20])]
        (calc expenses transactions) ~? ""
    ]
    where
        transactions = [
            transaction {
                payers =
                    [Side "stan" (Moneys [Sum 30])],
                beneficators =
                    [Side "eric" (Moneys [Sum 10]),
                     Side "kyle" (Moneys [Sum 10]),
                     Side "stan" (Moneys [Sum 10])],
                contragent = Just $ Contragent "pub"
            },
            transaction {
                payers =
                    [Side "eric" (Moneys [Sum 10]),
                     Side "stan" (Moneys [Sum 20])],
                beneficators =
                    [Side "eric" (Moneys [Sum 10]),
                     Side "kyle" (Moneys [Sum 10]),
                     Side "stan" (Moneys [Sum 10])]
            },
            transaction {
                payers =
                    [Side "kenny" (Moneys [Sum 10])],
                beneficators =
                    [Side "stan" (Moneys [Sum 10])],
                contragent = Just Internal  --  intenral transaction - must not be taken into account during expenses calculation
            }]
        transaction = Transaction [] [] Nothing Nothing Nothing [] [] Nothing


tests = test [addSidesTests, subSidesTests, calcTest]

