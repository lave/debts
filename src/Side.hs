module Side
where

import BasicTypes
import Money


data RawSide =
      RawSide String
    | RawSideWithFactor String Double
    | RawSideWithMoney String Moneys
    | RawSideRemove String
    | RawSideOverride RawSide
    deriving (Show, Eq)


data Side = Side Name Moneys
    deriving (Show, Eq)
instance Ord Side where
    compare (Side name1 _) (Side name2 _) = compare name1 name2

type Sides = [Side]


hasMoney (RawSideWithMoney _ _) = True
hasMoney _ = False

getFactor (RawSide _) = 1
getFactor (RawSideWithFactor _ factor) = factor
getFactor (RawSideWithMoney _ _) = 0

getMoney (RawSideWithMoney _ money) = money
getMoney _ = Moneys []
