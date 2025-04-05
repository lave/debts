module Side
where

import BasicTypes
import Money


data RawSide =
      RawSide String                        --  just side/group name
    | RawSides [RawSide]                    --  list of sides
    | RawSideWithMoney String Moneys        --  side with already assigned amount of money
    | RawSideWithFactor RawSide Double      --  side with factor (relative to other sides at this list)
    | RawSideWithSummand RawSide Moneys     --  side with summand (relative to other sides at this list)
    | RawSideRemove RawSide                 --  removes this side (from this level)
    | RawSideOverride RawSide               --  overrides side with given name (at this level, side must already exist at this level)
    | RawSideAdd RawSide                    --  adds to the side (at this level, side must already exist at this level)
    deriving (Show, Eq)

cdrSides lhs (RawSides rhs) = RawSides $ lhs : rhs
cdrSides lhs rhs = RawSides [lhs, rhs]

data MidSide =
      MidSide String
    | MidSideM MidSide Double Moneys
    | MidSizes [MidSize]
    deriving (Show, Eq)

data Side = Side Name Moneys
    deriving (Show, Eq)
instance Ord Side where
    compare (Side name1 _) (Side name2 _) = compare name1 name2

type Sides = [Side]


--hasMoney (RawSideWithMoney _ _) = True
--hasMoney _ = False

--getName (RawSide name) = name
--getName (RawSideWithFactor name _) = name
--getName (RawSideWithMoney name _) = name
--getName (RawSideWithSummand name _) = name

getFactor (RawSide _) = 1
getFactor (RawSideWithFactor _ factor) = factor
getFactor (RawSideWithMoney _ _) = 0
getFactor (RawSideWithSummand _ _) = 1

getMoney (RawSideWithMoney _ money) = money
getMoney (RawSideWithSummand _ money) = money
getMoney _ = Moneys []
