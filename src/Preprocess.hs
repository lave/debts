module Preprocess
where

import Data.Maybe

import Aggregation
import Fx
import Money
import Normalize
import Round
import Side
import SplitSide
import Transaction
import Param


normalize :: [Group] -> RawTransactions -> Transactions
normalize groups transactions =
    map (normalizeTransaction groups) transactions


--  isn't implemented yet
filter :: Params -> Transactions -> Transactions
filter _ = id


aggregate :: Params -> Transactions -> Transactions
aggregate params transactions =
    map (aggregateTransaction aggGroups) transactions
    where
        aggGroups = parseAggGroups $ getStringsParam params "aggregate"


splitGroups :: Params -> Transactions -> Transactions
splitGroups params transactions
    | getBoolParam params "split" = map split transactions
    | otherwise = transactions
    

assignDefaultCurrency :: Params -> Transactions -> Transactions
assignDefaultCurrency params transactions
    | isNothing currency = transactions
    | otherwise = map convertTransaction transactions
    where
        currency = getStringParam params "default.currency"
        convertTransaction t = t {
            payers = map convertSide $ payers t,
            beneficators = map convertSide $ beneficators t,
            Transaction.sum = fmap convertMoneys $ Transaction.sum t
        }

        convertSide (Side name moneys) =
            Side name $ convertMoneys moneys
        convertMoneys (Moneys moneys) =
            Moneys $ map convertMoney moneys
        convertMoney (Sum q) = Money q $ fromJust currency
        convertMoney m = m

convert :: Params -> Fxs -> Transactions -> Transactions
convert params fxs transactions
    | isNothing currency = transactions
    | otherwise = map convertTransaction transactions
    where
        currency = getStringParam params "target.currency"
        convertTransaction t = t {
            payers = map convertSide $ payers t,
            beneficators = map convertSide $ beneficators t,
            Transaction.sum = Just $ Fx.convert fxs (fromJust currency) $ fromJust $ Transaction.sum t
        }

        convertSide (Side name money) =
            Side name $ Fx.convert fxs (fromJust currency) money
