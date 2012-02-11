{
module Lexer
where

import Utils
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$quote = \"

tokens :-
    $white+ ;
    "#".*   ;
    ">"     { \s -> TokenArrow }
    ":"     { \s -> TokenColumn }
    ","     { \s -> TokenComma }
    "_"     { \s -> TokenUnderscore }
    "*"     { \s -> TokenAsterisk }
    "="     { \s -> TokenEqual }
    "-"     { \s -> TokenHyphen }
    "param" { \s -> TokenParameter }
    "fx"    { \s -> TokenFx }
    "group" { \s -> TokenGroup }
    "date"  { \s -> TokenDate }
    $alpha [$alpha $digit _\.]* { \s -> TokenString s }
    "-"? $digit+ (\. $digit+)? { \s -> TokenNumber (read s) }
    $quote [^$quote]* $quote { \s -> TokenString (trimBoth s) }

{

data Token =
      TokenArrow
    | TokenColumn
    | TokenUnderscore
    | TokenComma
    | TokenAsterisk
    | TokenEqual
    | TokenHyphen
    | TokenParameter
    | TokenFx
    | TokenGroup
    | TokenDate
    | TokenString String
    | TokenNumber Double
    deriving (Eq, Show)
}

