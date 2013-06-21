module SplitSide
where

import Money
import Side
import Transaction


split :: Transaction -> Transaction
split transaction =
    transaction { payers = payers', beneficators = beneficators' }
    where
        payers' = concatMap splitSide $ payers transaction
        beneficators' = concatMap splitSide $ beneficators transaction


splitSide :: Side -> Sides
splitSide (Side name (Moneys money)) = map makeSide money
    where
        makeSide money@(Sum _) =
            Side name $ Moneys [money]
        makeSide money@(Money _ currency) =
            Side (name ++ "-" ++ currency) $ Moneys [money]
