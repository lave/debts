module ParserMonad
where

data ParserError a = Ok a | Error String

thenE :: ParserError a -> (a -> ParserError b) -> ParserError b
thenE (Ok a) f = f a
thenE (Error s) _ = Error s

returnE :: a -> ParserError a
returnE a = Ok a

failE :: String -> ParserError a
failE s = Error s

catchE :: ParserError a -> (String -> ParserError a) -> ParserError a
catchE (Ok a) f = Ok a
catchE (Error s) f = f s

