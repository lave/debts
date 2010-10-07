{
module Syntaxer ( parseString, parse )
where

import Lexer
import Option
import Money
import Fx
import Transaction

}


%name parse
%tokentype { Token }
%error { parseError }


%token
    '>' { Arrow }
    ':'   { Column }
    '_'   { Underscore }
    ','   { Comma }
    '*'   { Asterisk }
    '='   { Equal }
    '-'   { Hyphen }
    string { String $$ }
    number { Number $$ }

%%

All :: { ([Option], [Group], [Fx], [RawTransaction]) }
    : Options Groups Fxs Transactions { ($1, $2, $3, $4) }

Options :: { [Option] }
    : {- empty -} { [] }
    | Options Option { $2 : $1 }

Option :: { Option }
    : string '*' string { StringOption $1 $3 }
    | string '*' number { NumberOption $1 $3 }

Groups :: { [Group] }
    : {- empty -} { [] }
    | Groups Group { $2 : $1 }

Group :: { Group }
    : string '=' GroupSides { Group $1 (reverse $3) }

GroupSides :: { [RawSide] }
    : GroupSide { [$1] }
    | GroupSides ',' GroupSide { $3 : $1 }

GroupSide :: { RawSide }
    : Side { $1 }
    | SideWithFactor { $1 }
    | SideRemove { $1 }
    | '=' GroupSide { RawSideOverride $2 }


Fxs :: { [Fx] }
    : {- empty -} { [] }
    | Fxs Fx { $2 : $1 }

Fx :: { Fx }
    : MoneyWithCurrency '=' MoneyWithCurrency { Fx $1 $3 }


Transactions :: { [RawTransaction] }
    : {- empty -} { [] }
    | Transactions Transaction { $2 : $1 }

Transaction :: { RawTransaction }
    : TSides '>' MaybeMoneys '>' TSides ':' Comment { RawTransaction (reverse $1) (reverse $5) $3 $7 }

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

parseString :: String -> ([Option], [Group], [Fx], [RawTransaction])
parseString = parse . alexScanTokens
}

