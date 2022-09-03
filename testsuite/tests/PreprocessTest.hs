module PreprocessTest
where

import Test.HUnit
import TestUtils

import BasicTypes
import Money
import Param
import Preprocess
import Side
import Transaction


assignDefaultCurrencyTest = test [
    "with both sides and no sum" ~:
        [transaction {
            payers = [Side "a" $ Moneys [Money 10 "USD", Money 40 "USD"]],
            beneficators = [Side "b" $ Moneys [Money 20 "USD", Money 30 "EUR"]]
        }]
        ~=?
        assignDefaultCurrency (defaultCurrencyParam "USD") [transaction {
            payers = [Side "a" $ Moneys [Money 10 "USD", Sum 40]],
            beneficators = [Side "b" $ Moneys [Sum 20, Money 30 "EUR"]]
        }],
    "with both sides and sum" ~:
        [transaction {
            payers = [Side "a" $ Moneys [Money 10 "USD", Money 40 "USD"]],
            beneficators = [Side "b" $ Moneys [Money 20 "USD", Money 30 "EUR"]],
            Transaction.sum = Just $ Moneys [Money 50 "USD"]
        }]
        ~=?
        assignDefaultCurrency (defaultCurrencyParam "USD") [transaction {
            payers = [Side "a" $ Moneys [Money 10 "USD", Sum 40]],
            beneficators = [Side "b" $ Moneys [Sum 20, Money 30 "EUR"]],
            Transaction.sum = Just $ Moneys [Sum 50]
        }]
    ]
    where
        defaultCurrencyParam currency =
            makeParams [Param "default.currency" StringParameter Override] [("default.currency", currency)]
        transaction = Transaction [] [] Nothing Nothing Nothing (Category []) [] Nothing


tests = test [assignDefaultCurrencyTest]
