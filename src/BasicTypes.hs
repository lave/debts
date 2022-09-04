module BasicTypes
where

import Data.String.Utils

--  side/account name
type Name = String

--  date
data Date = Date String
    deriving (Eq, Show)

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
