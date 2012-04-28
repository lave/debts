module Param
where

data Param
    = StringParam String String 
    | NumberParam String Double
    deriving (Eq, Show)

data ParamType
    = StringParameter
    | NumberParameter
    deriving (Eq, Show)

type Concatenator = String -> String -> String

data ParamAggregateType
    = NoOverride
    | Override
    | Multiple Concatenator



newtype ParamName = ParamName String
    deriving (Eq, Show)
newtype ParamValue = ParamValue String
    deriving (Eq, Show)

data ParamDescription
    = Param ParamName ParamType ParamAggregateType 

data Param_ = Param_ ParamName ParamValue

concatWith separator =
    \s1 s2 -> s1 ++ separator ++ s2

optionDescriptors = [
    Param (ParamName "round.to") NumberParameter Override,
    Param (ParamName "target.currency") StringParameter Override,
    Param (ParamName "aggregate") StringParameter (Multiple (concatWith ";"))
    ]

data Params = Params [Param]

getStringParam n opts =
    find opts
    where
        find [] = Nothing
        find ((StringParam n1 v) : opts)
            | n == n1 = Just v
            | otherwise = find opts
        find (_ : opts) = find opts

getNumberParam n opts =
    find opts
    where
        find [] = Nothing
        find ((NumberParam n1 v) : opts)
            | n == n1 = Just v
            | otherwise = find opts
        find (_ : opts) = find opts

