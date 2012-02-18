{
module Syntaxer ( parseString, parse )
where

import Maybe

import Lexer
import ParserMonad

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
    '@'     { TokenAt }
    '('     { TokenOpenParenthesis }
    ')'     { TokenCloseParenthesis }
    '['     { TokenOpenBracket }
    ']'     { TokenCloseBracket }
    param   { TokenParameter }
    fx      { TokenFx }
    group   { TokenGroup }
    string  { TokenString $$ }
    number  { TokenNumber $$ }
    date    { TokenDate }

%monad {ParserError} {thenE} {returnE}

%%

All :: { ([Builder]) }
    : Builders { (reverse $1) }

Builders :: { [Builder] }
    : {- empty -} { [] }
    | Builders Builder { $2 : $1 }

Builder :: { Builder }
    : ParameterBuilder { $1 }
    | GroupBuilder { $1 }
    | FxBuilder { $1 }
    | DateBuilder { $1 }
    | TransactionBuilder { $1 }



ParameterBuilder :: { Builder }
    : param string '=' string
        { ParameterBuilder $ StringOption $2 $4 }
    | param string '=' number
        { ParameterBuilder $ NumberOption $2 $4 }



GroupBuilder :: { Builder }
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



FxBuilder :: { Builder }
    : fx MoneyWithCurrency '=' MoneyWithCurrency
        { FxBuilder $ Fx $2 $4 }



DateBuilder :: { Builder }
    : date '_' { DateBuilder Nothing }
    | date string { DateBuilder $ Just $ Date $2 }



TransactionBuilder :: { Builder }
    : TSides '>' MaybeMoneys '>' MaybeTSides TransactionAttributeBuilders CommentBuilder
        { TransactionBuilder
            (reverse $1)
            (reverse $ fromMaybe $1 $5)
            $3
            ($6 ++ $7) }


TransactionAttributeBuilders :: { [TransactionAttributeBuilder] }
    : {- empty -} { [] }
    | TransactionAttributeBuilders TransactionAttributeBuilder { $2 : $1 }

TransactionAttributeBuilder :: { TransactionAttributeBuilder }
    : '@' string { ContragentBuilder $ Contragent $2 }
    | '(' Categories ')' { CategoryBuilder $ reverse $2 }
    | '[' Tags ']' { TagsBuilder $ reverse $2 }

Categories :: { [Category] }
    : {- empty -} { [] }
    | Category { [$1] }
    | Categories ',' Category { $3 : $1 }

Category :: { Category }
    : string { Category $1 }

Tags :: { [Tag] }
    : {- empty -} { [] }
    | Tag { [$1] }
    | Tags ',' Tag { $3 : $1 }

Tag :: { Tag }
    : string { Tag $1 }


CommentBuilder :: { [TransactionAttributeBuilder] }
    : {- empty -} { [] }
    | ':' string { [CommentBuilder $ Comment $2] }



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


MaybeTSides :: { Maybe [RawSide] }
    : '_' { Nothing }
    | TSides { Just $1 }

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
parseError :: [Token] -> ParserError a
parseError tokens = Error ("Parse error: " ++ show (head tokens))

parseString :: String -> ParserError [Builder]
parseString = parse . alexScanTokens
}

