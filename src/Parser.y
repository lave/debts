{
module Parser ( parseString, parse )
where

import Data.Maybe

import Lexer
import ParserMonad

import BasicTypes
import Fx
import InputBuilder
import Money
import Param
import Side
import Transaction

}


%name parse
%tokentype { Token }
%error { parseError }


%token
    '>'         { Token _ (TokenSym '>') }
    ':'         { Token _ (TokenSym ':') }
    '_'         { Token _ (TokenSym '_') }
    ','         { Token _ (TokenSym ',') }
    '*'         { Token _ (TokenSym '*') }
    '/'         { Token _ (TokenSym '/') }
    '='         { Token _ (TokenSym '=') }
    '-'         { Token _ (TokenSym '-') }
    '+'         { Token _ (TokenSym '+') }
    '@'         { Token _ (TokenSym '@') }
    '('         { Token _ (TokenSym '(') }
    ')'         { Token _ (TokenSym ')') }
    '['         { Token _ (TokenSym '[') }
    ']'         { Token _ (TokenSym ']') }
    '{'         { Token _ (TokenSym '{') }
    '}'         { Token _ (TokenSym '}') }
    param       { Token _ (TokenKeyword "param") }
    fx          { Token _ (TokenKeyword "fx") }
    group       { Token _ (TokenKeyword "group") }
    date        { Token _ (TokenKeyword "date") }
    internal    { Token _ (TokenKeyword "internal") }
    string      { Token _ (TokenString $$) }
    integer     { Token _ (TokenInteger $$) }
    number      { Token _ (TokenNumber $$) }
    date_l      { Token _ (TokenDate _ _ _) }

%monad {ParserError} {thenE} {returnE}

%nonassoc FACTOR
%left '+' '-'
%left '*' '/'

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
        { ParameterBuilder ($2, $4) }
    | param string '=' NumericExp
        { ParameterBuilder ($2, show $4) }



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
    | '+' GroupSide { RawSideAdd $2 }



FxBuilder :: { Builder }
    : fx MoneyWithCurrency '=' MoneyWithCurrency
        { FxBuilder $ Fx $2 $4 }



DateBuilder :: { Builder }
    : date '_' { DateBuilder Nothing }
    | date string { DateBuilder $ Just $ StringDate $2 }
    | date date_l { DateBuilder $ Just $ (\(Token _ (TokenDate y m d)) -> makeDate y m d) $2 }



TransactionBuilder :: { Builder }
    : TSides '>' MaybeMoneys '>' MaybeTSides TransactionAttributeBuilders CommentBuilder
        { TransactionBuilder
            (reverse $1)
            (reverse $ fromMaybe $1 $5)
            $3
            ($6 ++ $7) }
    | TSides '>' TSides TransactionAttributeBuilders CommentBuilder
        { TransactionBuilder
            (reverse $1)
            (reverse $3)
            Nothing
            ($4 ++ $5) }
    | TSides '>' MaybeMoneys TransactionAttributeBuilders CommentBuilder
        { TransactionBuilder
            (reverse $1)
            (reverse $1)
            $3
            ($4 ++ $5) }
    | TSides TransactionAttributeBuilders CommentBuilder
        { TransactionBuilder
            (reverse $1)
            (reverse $1)
            Nothing
            ($2 ++ $3) }


TransactionAttributeBuilders :: { [TransactionAttributeBuilder] }
    : {- empty -} { [] }
    | TransactionAttributeBuilders TransactionAttributeBuilder { $2 : $1 }

TransactionAttributeBuilder :: { TransactionAttributeBuilder }
    : Contragent { ContragentBuilder $1 }
    | '{' Categories '}' { CategoryBuilder $ Category $ reverse $2 }
    | '[' Tags ']' { TagsBuilder $ reverse $2 }

Contragent :: { Contragent }
    : '@' internal { Internal }
    | '@' string { Contragent $2 }

Categories :: { [CategoryComponent] }
    : {- empty -} { [] }
    | Category { [$1] }
    | Categories ',' Category { $3 : $1 }

Category :: { CategoryComponent }
    : string { $1 }

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
    : NumericExp { Sum $1 }

MoneyWithCurrency :: { Money }
    : NumericExp string { Money $1 $2 }

NumericExp :: { Double }
    : number { $1 }
    | integer { fromIntegral $1 }
    | '(' NumericExp ')' { $2 }
    | NumericExp '+' NumericExp { $1 + $3 }
    | NumericExp '-' NumericExp { $1 - $3 }
    | NumericExp '*' NumericExp { $1 * $3 }
    | NumericExp '/' NumericExp { $1 / $3 }
    | NumericExp '/' string '/' { $1 }


MaybeTSides :: { Maybe [RawSide] }
    : '_' { Nothing }
    | TSides { Just $1 }

TSides :: { [RawSide] }
    : TSide { [$1] }
    | TSides ',' TSide { $3 : $1 }

TSide :: { RawSide }
    : Side { $1 }
    | SideRemove { $1 }
    | SideWithFactor { $1 }
    | SideWithMoney { $1 }
    | SideWithSummand { $1 }
    | '=' TSide { RawSideOverride $2 }
    | '+' TSide { RawSideAdd $2 }

Side :: { RawSide }
    : string { RawSide $1 }

SideRemove :: { RawSide }
    : '-' string { RawSideRemove $2 }

SideWithFactor :: { RawSide }
    : string '*' NumericExp %prec FACTOR { RawSideWithFactor $1 $3 }

SideWithMoney :: { RawSide }
    : string Moneys { RawSideWithMoney $1 $2 }

SideWithSummand :: { RawSide }
    : string '+' Moneys { RawSideWithSummand $1 $3 }


{
parseError :: [Token] -> ParserError a
parseError tokens =
    Error $ "Parse error at " ++ errorPosition tokens ++ ": "
        ++ show (token_data $ head tokens)

errorPosition:: [Token] -> String
errorPosition tokens =
    case tokens of
        [] -> "end of script"
        token:_ -> show line ++ ":" ++ show column
            where
                AlexPn _ line column = token_position token

parseString :: String -> ParserError [Builder]
parseString = parse . alexScanTokens
}

