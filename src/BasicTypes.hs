module BasicTypes
where

import Data.String.Utils
import Data.Time
import Text.Printf

--  side/account name
type Name = String

--  date
data Date =
      StringDate String
    | StructDate Int Int Int    -- year, month, day
    deriving (Eq)

instance Show Date where
    show (StringDate str) = str
    show (StructDate year month day) = (printf "%04d" year) ++ "-" ++ (printf "%02d" month) ++ "-" ++ (printf "%02d" day)

makeDate year month day = StructDate year' month' day'
    where
        year' = if year < 100 then year + 2000 else year
        month' = if month < 1 || month > 12 then error ("Wrong month: " ++ show month) else month
        day' = if month < 1 || month > 31 then error ("Wrong day: " ++ show day) else day
    

--  contragent - who ultimately gets the money, e.g. a shop.
--  'Internal' means that transaction is just transfer of money
--  from one side/account to another - money aren't actually spent
data Contragent =
      Internal
    | Contragent String
    deriving (Show, Eq)

--  category of transaction
type CategoryComponent = String
newtype Category = Category [CategoryComponent]
    deriving (Eq)

instance Show Category where
    show (Category components) = join "." components


newtype Tag = Tag String
    deriving (Show, Eq)
type Tags = [Tag]

newtype Comment = Comment String
    deriving (Show, Eq)
