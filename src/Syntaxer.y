{
module Syntaxer ( parseString, parse )
where

import Lexer

import Date
import Fx
import InputBuilder
import Money
import Option
import Transaction

}


%name parse
%tokentype { Token }
%error { parseError }


%token
    '>'     { TokenArrow }
    ':'     { TokenColumn }
    '_'     { TokenUnderscore }
    ','     { TokenComma }
    '*'     { TokenAsterisk }
    '='     { TokenEqual }
    '-'     { TokenHyphen }
    param   { TokenParameter }
    fx      { TokenFx }
    group   { TokenGroup }
    string  { TokenString $$ }
    number  { TokenNumber $$ }
    date    { TokenDate }

%%

All :: { ([Builder]) }
    : Builders { (reverse $1) }

Builders :: { [Builder] }
    : {- empty -} { [] }
    | Builders Option { $2 : $1 }
    | Builders Group { $2 : $1 }
    | Builders Fx { $2 : $1 }
    | Builders Date { $2 : $1 }
    | Builders Transaction { $2 : $1 }

Option :: { Builder }
    : param string '=' string
        { ParameterBuilder $ StringOption $2 $4 }
    | param string '=' number
        { ParameterBuilder $ NumberOption $2 $4 }

Group :: { Builder }
    : group string '=' GroupSides
        { GroupBuilder $ Group $2 (reverse $4) }

GroupSides :: { [RawSide] }
    : GroupSide { [$1] }
    | GroupSides ',' GroupSide { $3 : $1 }

GroupSide :: { RawSide }
    : Side { $1 }
    | SideWithFactor { $1 }
    | SideRemove { $1 }
    | '=' GroupSide { RawSideOverride $2 }


Fx :: { Builder }
    : fx MoneyWithCurrency '=' MoneyWithCurrency
        { FxBuilder $ Fx $2 $4 }


Date :: { Builder }
    : date string { DateBuilder $ Date $2 }


Transaction :: { Builder }
    : RawTransaction { TransactionBuilder $1 }

RawTransaction :: { RawTransaction }
    : TSides '>' MaybeMoneys '>' TSides { RawTransaction (reverse $1) (reverse $5) $3 "" }
    | TSides '>' MaybeMoneys '>' TSides ':' Comment { RawTransaction (reverse $1) (reverse $5) $3 $7 }
    | TSides '>' MaybeMoneys '>' '_' { RawTransaction (reverse $1) (reverse $1) $3 "" }
    | TSides '>' MaybeMoneys '>' '_' ':' Comment { RawTransaction (reverse $1) (reverse $1) $3 $7 }

Comment :: { String }
    : string { $1 }

MaybeMoneys :: { Maybe Moneys }
    : '_' { Nothing }
    | Moneys { Just $1 }

Moneys :: { Moneys }
    : MoneyList { Moneys $1 }

MoneyList :: { [Money] }
    : Money { [$1] }
    | MoneyList Money { $2 : $1 }

Money :: { Money }
    : MoneyWithoutCurrency { $1 }
    | MoneyWithCurrency { $1 }

MoneyWithoutCurrency :: { Money }
    : number { Sum $1 }

MoneyWithCurrency :: { Money }
    : number string { Money $1 $2 }

TSides :: { [RawSide] }
    : TSide { [$1] }
    | TSides ',' TSide { $3 : $1 }

TSide :: { RawSide }
    : Side { $1 }
    | SideWithFactor { $1 }
    | SideWithMoney { $1 }
    | SideRemove { $1 }
    | '=' TSide { RawSideOverride $2 }

Side :: { RawSide }
    : string { RawSide $1 }

SideWithFactor :: { RawSide }
    : string '*' number { RawSideWithFactor $1 $3 }

SideWithMoney :: { RawSide }
    : string Moneys { RawSideWithMoney $1 $2 }

SideRemove :: { RawSide }
    : '-' string { RawSideRemove $2 }

{
parseError :: [Token] -> a
parseError token = error ("Parse error: " ++ show token)

parseString :: String -> ([Builder])
parseString = parse . alexScanTokens
}

