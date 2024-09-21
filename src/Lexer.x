{
module Lexer
where

import Utils
}

%wrapper "posn"

$d = 0-9
$a = [a-zA-Z]
$q = \"

tokens :-
    $white+ ;
    "#".*   ;
    [>:\,\._\*\/=\-\+@\(\)\[\]\{\}]         { \p s -> Token p $ TokenSym $ head s }
    "param"                                 { \p s -> Token p $ TokenKeyword "param" }
    "fx"                                    { \p s -> Token p $ TokenKeyword "fx" }
    "group"                                 { \p s -> Token p $ TokenKeyword "group" }
    "date"                                  { \p s -> Token p $ TokenKeyword "date" }
    "internal"                              { \p s -> Token p $ TokenKeyword "internal" }
    "-"? $d+                                { \p s -> Token p $ TokenInteger $ read s }
    "-"? $d+ (\. $d+)?                      { \p s -> Token p $ TokenNumber $ read s }
    $a [$a $d _\.]*                         { \p s -> Token p $ TokenString s }
    $q [^$q]* $q                            { \p s -> Token p $ TokenString $ trimBoth s }
    $d{4} \- $d{2} \- $d{2}                 { \p s -> Token p $ (\[y, m, d] -> TokenDate y m d) $ map read $ split "-" s }
    $d{1,2} \. $d{1,2} \. ($d{2} | $d{4})   { \p s -> Token p $ (\[d, m, y] -> TokenDate y m d) $ map read $ split "." s }
    $d{1,2} \/ $d{1,2} \/ ($d{2} | $d{4})   { \p s -> Token p $ (\[m, d, y] -> TokenDate y m d) $ map read $ split "/" s }

{

data Token = Token AlexPosn TokenData
    deriving (Eq)

data TokenData =
      TokenSym Char
    | TokenKeyword String
    | TokenString String
    | TokenNumber Double
    | TokenInteger Integer
    | TokenDate Int Int Int
    deriving (Eq)

instance Show TokenData
    where
        show (TokenSym c) = show c
        show (TokenKeyword w) = w
        show (TokenInteger n) = show n ++ "i"
        show (TokenNumber n) = show n ++ "f"
        show (TokenString s) = "'" ++ s ++ "'"
        show (TokenDate y m d) = show y ++ "-" ++ show m ++ "-" ++ show d

instance Show Token
    where
        show (Token (AlexPn _ line column) tokenData) =
            show tokenData ++ "(" ++ show line ++ ":" ++ show column ++ ")"

token_position (Token p _) = p

token_data (Token _ d) = d

}
