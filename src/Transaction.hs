module Transaction
where

import Data.Maybe

import Date
import Money
import Side
import Utils


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
type NormalizedTransaction = Transaction_ Side

data Group = Group String [RawSide]
    deriving (Show, Eq)
