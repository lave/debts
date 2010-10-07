module Option
where

data Option = StringOption String String 
    | NumberOption String Double
    deriving (Eq, Show)


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

