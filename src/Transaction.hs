module Transaction
where

import Data.List
import Data.Maybe

import Date
import Utils
import Money


data RawSide =
      RawSide String
    | RawSideWithFactor String Double
    | RawSideWithMoney String Moneys
    | RawSideRemove String
    | RawSideOverride RawSide
    deriving (Show, Eq)


data Contragent =
      Internal
    | Contragent String
    deriving (Show, Eq)

newtype Category = Category String
    deriving (Show, Eq)

newtype Tag = Tag String
    deriving (Show, Eq)

newtype Comment = Comment String
    deriving (Show, Eq)


data Transaction_ side = Transaction {
    payers :: [side],
    beneficators :: [side],
    sum :: Maybe Moneys,
    date :: Maybe Date,
    contragent :: Maybe Contragent,
    category :: [Category],
    tags :: [Tag],
    comment :: Maybe Comment
} deriving (Show)
    

type RawTransaction = Transaction_ RawSide


data Group = Group String [RawSide]
    deriving (Show, Eq)

data Side = Side String Moneys
    deriving (Show, Eq)
instance Ord Side where
    compare (Side name1 _) (Side name2 _) = compare name1 name2

type NormalizedTransaction = Transaction_ Side
