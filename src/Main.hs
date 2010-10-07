module Main ( main )
where

import Option
import Money
import Debt
import Syntaxer
import Lexer
import Transaction
import Fx
import Postprocessing

import System
import List


main = do
    args <- getArgs
    s <- readFile $ head args
    let tokens = alexScanTokens s
    if (find (\s -> s == "-d") args /= Nothing)
        then do
            putStrLn "Tokens:"
            putStrLn $ show tokens
        else do
            putStrLn ""

    --let parsed = parseString s
    let parsed = parse tokens
    if (find (\s -> s == "-d") args /= Nothing)
        then do
            putStrLn "Syntax tree:"
            putStrLn $ show parsed
        else do
            putStrLn ""

    let result = process $ parsed
    printResults result





process (options, groups, fxs, transactions) = rounded
    where
        rawResults = calc $ map unifyTransaction $ preprocessTransactions groups transactions
        converted = applyIfOptionIsSet (convertSides fxs) (getStringOption "target.currency" options) rawResults
        rounded = applyIfOptionIsSet (\b l -> roundSides (round b) l) (getNumberOption "round.to" options) converted

        applyIfOptionIsSet f (Just x) a = f x a
        applyIfOptionIsSet _ Nothing a = a


convertSides fxs c sides = map (convertSide fxs c) sides
    where
        convertSide fxs c (Side n m) = Side n $ convert fxs c m


printResults sides = do
    sequence_ $ map printResult sides
    putStrLn "----------------"
    let sum = foldl Money.add (Moneys []) $ [m | (Side _ m) <- sides]
    printResult $ Side "Total" $ sum
    where
        printResult (Side n m) = do
            putStrLn $ n ++ ":\t " ++ (show m)

