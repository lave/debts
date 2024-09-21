module MoneyLogTest
where

import Test.HUnit

import BasicTypes
import Money
import MoneyLog
import Result
import Side
import qualified Transaction
import Utils


intersectSidesTests = TestList [
    "common case" ~:
        [Side "stan" (Moneys [Sum 5]),
         Side "kyle" (Moneys [Sum 10])] ~=?
        (intersectSides
            [Side "stan" (Moneys [Sum 10]),
             Side "eric" (Moneys [Sum 10]),
             Side "kyle" (Moneys [Sum 10])]
            [Side "stan" (Moneys [Sum 5]),
             Side "kyle" (Moneys [Sum 10]),
             Side "kenny" (Moneys [Sum 5])])
    ]


splitTransactionsTests = TestList [

    -- simple cases

    "me pay for myself" ~: same
        [mlt "stan" Nothing 10]
        (splitTransaction "stan" $ transaction
            [("stan", 10)]
            [("stan", 10)]) ~? "",

    "me pay for myself - internal" ~: same
        []
        (splitTransaction "stan" $ internalTransaction
            [("stan", 10)]
            [("stan", 10)]) ~? "",

    "me pay for another" ~: same
        [mlt "stan" (Just "kyle") 10]
        (splitTransaction "stan" $ transaction
            [("stan", 10)]
            [("kyle", 10)]) ~? "",

    "me pay for another - internal" ~: same
        [internalMlt "stan" (Just "kyle") 10]
        (splitTransaction "stan" $ internalTransaction
            [("stan", 10)]
            [("kyle", 10)]) ~? "",

    "another pays for me" ~: same
        [mlt "kyle" Nothing 10]
        (splitTransaction "stan" $ transaction
            [("kyle", 10)]
            [("stan", 10)]) ~? "",

    "another pays for me - internal" ~: same
        [internalMlt "kyle" (Just "stan") 10]
        (splitTransaction "stan" $ internalTransaction
            [("kyle", 10)]
            [("stan", 10)]) ~? "",

    "another pays for another" ~: same
        []
        (splitTransaction "stan" $ transaction
            [("kyle", 10)]
            [("eric", 10)]) ~? "",

    "another pays for another - internal" ~: same
        []
        (splitTransaction "stan" $ internalTransaction
            [("kyle", 10)]
            [("eric", 10)]) ~? "",

    -- splitting

    "me pay for us" ~: same
        [mlt "stan" Nothing 10,
         mlt "stan" (Just "kyle") 10,
         mlt "stan" (Just "eric") 10]
        (splitTransaction "stan" $ transaction
            [("stan", 30)]
            [("stan", 10), ("kyle", 10), ("eric", 10)]) ~? "",

    "we pay for another" ~: same
        [mlt "stan" (Just "eric") 10]
        (splitTransaction "stan" $ transaction
            [("stan", 10), ("kyle", 10), ("eric", 10)]
            [("eric", 30)]) ~? "",

    "we pay for us" ~: same
        [mlt "stan" Nothing 10]
        (splitTransaction "stan" $ transaction
            [("stan", 10), ("kyle", 10)]
            [("stan", 10), ("eric", 10)]) ~? "",

    "we pay for others" ~: same
        [mlt "stan" (Just "eric") 10]
        (splitTransaction "stan" $ transaction
            [("stan", 10), ("kyle", 10)]
            [("kyle", 10), ("eric", 10)]) ~? "",

    "we pay for others 2" ~: same
        [mlt "stan" Nothing 10,
         mlt "stan" (Just "eric") 10]
        (splitTransaction "stan" $ transaction
            [("stan", 20), ("kyle", 10)]
            [("stan", 10), ("kyle", 10), ("eric", 10)]) ~? "",

    "another pay for us" ~: same
        [mlt "kyle" Nothing 10]
        (splitTransaction "stan" $ transaction
            [("kyle", 30)]
            [("stan", 10), ("kyle", 10), ("eric", 10)]) ~? "",

    "we with others pay for me" ~: same
        [mlt "stan" Nothing 10,
         mlt "kyle" Nothing 10,
         mlt "eric" Nothing 10]
        (splitTransaction "stan" $ transaction
            [("stan", 10), ("kyle", 10), ("eric", 10)]
            [("stan", 30)]) ~? ""

    {- must generate error
    "we pay for others" ~:
        [] ~=?
        (splitTransaction "stan" $ transaction
            [("stan", 10), ("kyle", 10)]
            [("eric", 10), ("kenny", 10)]),
    -}

    {- must generate error
    "others pay for us" ~:
        [] ~=?
        (splitTransaction "stan" $ transaction
            [("eric", 10), ("kenny", 10)]
            [("stan", 10), ("kyle", 10)])
    -}
    ]


transaction payers beneficators = Transaction.Transaction
    (map makeSide payers)
    (map makeSide beneficators)
    (Just $ Moneys [Sum $ Prelude.sum $ snd $ unzip payers])
    (Just $ StructDate 2000 1 1)
    (Just $ Contragent "contragent")
    (Category ["category"])
    [Tag "tag"]
    (Just $ Comment "comment")

    where
        makeSide (name, quantity) = Side name (Moneys [Sum quantity])

internalTransaction payers beneficators =
    (transaction payers beneficators) { Transaction.contragent = Just Internal }

mlt payer beneficator sum = MLTransaction
    payer
    beneficator
    (Moneys [Sum sum])
    (Just $ StructDate 2000 1 1)
    (Just $ Contragent "contragent")
    (Category ["category"])
    [Tag "tag"]
    (Just $ Comment "comment")

internalMlt payer beneficator sum =
    (mlt payer beneficator sum) { contragent = Just Internal }


tests = TestList [intersectSidesTests, splitTransactionsTests]
