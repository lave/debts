module Preprocess ( roundSides )
where

import Data.Maybe

import Aggregation
import Round
import Transaction
import Utils

data NormalizedInput =
    NormalizedInput [RawParam] [Group] [Fx] [NormalizedTransaction]

normalize :: Input -> NormalizedInput
normalize Input params groups fxs transactions =
    Input params fxs groups $ map (normalizeTransaction groups) transactions


aggregate :: NormalizedInput -> NormalizedInput
aggregate Input params groups fxs transactions =
    Input params fxs groups $ map (aggregateTransaction outGroups) transactions
    where
        outGroups = parseAggGroups $ getStringsParam params "aggregate"

filter :: NormalizedInput -> NormalizedInput
filter = id

convert :: NormalizedInput -> NormalizedInput
convert Input params groups fxs transactions =
    applyIfParamIsSet (convertSides fxs) targetCurrency raw
    where
        targetCurrency = getStringParam params "target.currency"
    Input params fxs groups $ map (convertTransaction fxs) transactions
