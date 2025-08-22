module Side
where

import BasicTypes
import Money


data RawSide =
      RawSide String
    | RawSideWithFactor String Double
    | RawSideWithMoney String Moneys
    | RawSideWithSummand String Moneys
    | RawSideRemove String
    | RawSideOverride RawSide
    | RawSideAdd RawSide
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

toSideWithFactor (RawSide name) = RawSideWithFactor name 1
toSideWithFactor s@(RawSideWithFactor _ _) = s
toSideWithFactor (RawSideWithMoney name moneys) = RawSideWithFactor name f
    where
        Moneys [Sum f] = moneys
toSideWithFactor s@(RawSideRemove _) = s
toSideWithFactor (RawSideOverride s) = RawSideOverride $ toSideWithFactor s
toSideWithFactor (RawSideAdd s) = RawSideAdd $ toSideWithFactor s
