module Side
where

import BasicTypes
import Money


data RawSide =
      RawSide String
    | RawSideRemove String
    | RawSideWithFactor String Double
    | RawSideWithMoney String Moneys
    | RawSideWithSummand String Moneys
    | RawSideOverride RawSide
    deriving (Show, Eq)


data Side = Side Name Moneys
    deriving (Show, Eq)
instance Ord Side where
    compare (Side name1 _) (Side name2 _) = compare name1 name2

type Sides = [Side]


hasMoney (RawSideWithMoney _ _) = True
hasMoney _ = False

getName (RawSide name) = name
getName (RawSideWithFactor name _) = name
getName (RawSideWithMoney name _) = name
getName (RawSideWithSummand name _) = name

getFactor (RawSide _) = 1
getFactor (RawSideWithFactor _ factor) = factor
getFactor (RawSideWithMoney _ _) = 0
getFactor (RawSideWithSummand _ _) = 1

getMoney (RawSideWithMoney _ money) = money
getMoney (RawSideWithSummand _ money) = money
getMoney _ = Moneys []
