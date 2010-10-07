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
    ">"    { \s -> Arrow }
    ":"     { \s -> Column }
    ","     { \s -> Comma }
    "_"     { \s -> Underscore }
    "*"     { \s -> Asterisk }
    "="     { \s -> Equal }
    "-"     { \s -> Hyphen }
    $alpha [$alpha $digit _\.]* { \s -> String s }
    "-"? $digit+ (\. $digit+)? { \s -> Number (read s) }
    $quote [^$quote]* $quote { \s -> String $ trimBoth s }

{

data Token =
    Arrow
    | Column
    | Underscore
    | Comma
    | Asterisk
    | Equal
    | Hyphen
    | String String
    | Number Double
    deriving Show
}

