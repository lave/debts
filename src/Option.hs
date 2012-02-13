module Option
where

data Option =
      StringOption String String 
    | NumberOption String Double
    deriving (Eq, Show)

{-
data OptionType =
    String
    | Number

data OptionAggregateType =
    SingleDefined
    | Overridable
    | Multiple

data OptionDescription =
      Option String OptionType AccumulateType 


optionDescriptors = [
    Option "round.to" Number Overridable,
    Option "round.to" Number Overridable Default 1
    Option "target.currency" String Overridable,
    Option "aggregate" String Multiple
    ]

data Options = Options [Option]
-}

getStringOption n opts =
    find opts
    where
        find [] = Nothing
        find ((StringOption n1 v) : opts)
            | n == n1 = Just v
            | otherwise = find opts
        find (_ : opts) = find opts

getNumberOption n opts =
    find opts
    where
        find [] = Nothing
        find ((NumberOption n1 v) : opts)
            | n == n1 = Just v
            | otherwise = find opts
        find (_ : opts) = find opts

