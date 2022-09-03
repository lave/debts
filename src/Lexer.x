{
module Lexer
where

import Utils
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$quote = \"

tokens :-
    $white+ ;
    "#".*   ;
    [>:\,_\*\/=\-\+@\(\)\[\]\{\}] { \p s -> Token p $ TokenSym $ head s }
    "param"                       { \p s -> Token p $ TokenKeyword "param" }
    "fx"                          { \p s -> Token p $ TokenKeyword "fx" }
    "group"                       { \p s -> Token p $ TokenKeyword "group" }
    "date"                        { \p s -> Token p $ TokenKeyword "date" }
    "internal"                    { \p s -> Token p $ TokenKeyword "internal" }
    "-"? $digit+ (\. $digit+)?    { \p s -> Token p $ TokenNumber $ read s }
    $alpha [$alpha $digit _\.]*   { \p s -> Token p $ TokenString s }
    $quote [^$quote]* $quote      { \p s -> Token p $ TokenString $ trimBoth s }

{

data Token = Token AlexPosn TokenData
    deriving (Eq, Show)

data TokenData =
      TokenSym Char
    | TokenKeyword String
    | TokenString String
    | TokenNumber Double
    deriving (Eq)

instance Show TokenData
    where
        show (TokenSym c) = show c
        show (TokenKeyword w) = "'" ++ w ++ "'"
        show (TokenNumber n) = "'" ++ show n ++ "'"
        show (TokenString s) = "'" ++ s ++ "'"


token_position (Token p _) = p

token_data (Token _ d) = d

}
