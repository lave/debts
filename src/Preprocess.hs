module Preprocess
where

import Data.Maybe

import Aggregation
import Fx
import Normalize
import Round
import Side
import Transaction


normalize :: [Group] -> RawTransactions -> Transactions
normalize groups transactions =
    map (normalizeTransaction groups) transactions


--  isn't implemented yet
filter :: Transactions -> Transactions
filter = id


aggregate :: AggGroups -> Transactions -> Transactions
aggregate aggGroups transactions =
    map (aggregateTransaction aggGroups) transactions


convert :: Fxs -> Maybe String -> Transactions -> Transactions
convert fxs currency transactions
    | isNothing currency = transactions
    | otherwise = map convertTransaction transactions
    where
        convertTransaction t = t {
            payers = map convertSide $ payers t,
            beneficators = map convertSide $ beneficators t
        }

        convertSide (Side name money) =
            Side name $ Fx.convert fxs (fromJust currency) money
