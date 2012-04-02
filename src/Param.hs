module Param
where

data Param =
      StringParam String String 
    | NumberParam String Double
    deriving (Eq, Show)

data ParamType =
    = StringParameter
    | NumberParameter
    deriving (Eq, Show)

data ParamAggregateType =
    = NoOverride
    | Override
    | Multiple
    deriving (Eq, Show)

data ParamDescription
    = Param String ParamType AccumulateType 
    deriving (Eq, Show)


optionDescriptors = [
    Param "round.to" NumberParameter Override,
    Param "target.currency" StringParameter Override,
    Param "aggregate" StringParameter Multiple
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

