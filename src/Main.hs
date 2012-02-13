module Main ( main )
where

import InputBuilder
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

    let parsed = parse tokens
    if (find (\s -> s == "-d") args /= Nothing)
        then do
            putStrLn "Syntax tree:"
            putStrLn $ show parsed
        else do
            putStrLn ""

    let input = buildInputData parsed
    let result = process input
    printResults result




process :: Input -> ([Side], [Side])

process (Input options groups fxs transactions) = (balance', expenses')
    where
        targetCurrency = getStringOption "target.currency" options
        roundTo = getNumberOption "round.to" options

        balance' = process' balance
        expenses' = process' expenses

        process' f = sortBy compareSides rounded
            where
                raw = calc f $ map (normalizeTransaction groups) transactions
                converted = applyIfOptionIsSet (convertSides fxs) targetCurrency raw
                rounded = applyIfOptionIsSet (\b l -> roundSides (round b) l) roundTo converted

                applyIfOptionIsSet f (Just x) a = f x a
                applyIfOptionIsSet _ Nothing a = a
                
                compareSides (Side n1 _) (Side n2 _) = compare n1 n2


convertSides fxs c sides = map (convertSide fxs c) sides
    where
        convertSide fxs c (Side n m) = Side n $ convert fxs c m


printResults (balance, expenses) = do
    putStrLn $ show balance
    putStrLn $ show expenses
    let sides = balance
    sequence_ $ map printResult sides
    putStrLn "----------------"
    let sum = foldl Money.add (Moneys []) $ [m | (Side _ m) <- sides]
    printResult $ Side "Total" $ sum
    where
        printResult (Side n m) = do
            putStrLn $ n ++ ":\t " ++ (show m)

