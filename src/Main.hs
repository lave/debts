module Main (main)
where

import qualified System.Environment (getArgs)
import qualified Data.List (sort)

import Aggregation
import qualified CommandLine
import InputBuilder
import Param
import Money
import Debt
import Parser
import ParserMonad
import Lexer
import Transaction
import Fx
import Postprocessing
import Round


main = do
    args <- System.Environment.getArgs
    let fileName = head args
    let isVerbose = CommandLine.containsKey args "-v"
    let parameters = CommandLine.findParameters args

    s <- readFile fileName
    let tokens = alexScanTokens s
    putStrLn $ if isVerbose then "Tokens:\n" ++ show tokens else ""

    case parse tokens of
        Ok parsed -> do
            putStrLn $ if isVerbose then "Syntax tree:\n" ++ show parsed else ""
            printResults $ process parameters parsed
        Error s ->
            putStrLn ("Parse error:" ++ s)


process paramOverrides parsed =
    process' $ Input (params ++ paramOverrides) groups fxs transactions
    where
        Input params groups fxs transactions = buildInputData parsed



parameterDescriptors = [
    Param "round.to" NumberParameter Override,
    Param "target.currency" StringParameter Override,
    Param "aggregate" StringParameter (Concatenate ",")
    ]


process' :: Input -> ([Side], [Side])

process' (Input rawParams groups fxs transactions) = (balance', expenses')
    where
        params = makeParams parameterDescriptors rawParams
        targetCurrency = getStringParam params "target.currency"
        roundTo = getNumberParam params "round.to"
        outGroups = parseAggGroups $ getStringsParam params "aggregate"

        balance' = process' balance smartRound
        expenses' = process' expenses roundListTo

        process' calculator rounder = Data.List.sort rounded
            where
                raw = calc calculator
                    $ map (aggregateTransaction outGroups)
                    $ map (normalizeTransaction groups) transactions
                converted = applyIfParamIsSet (convertSides fxs) targetCurrency raw
                rounded = applyIfParamIsSet (roundSides rounder) roundTo converted

                applyIfParamIsSet f (Just x) a = f x a
                applyIfParamIsSet _ Nothing a = a


convertSides fxs c sides = map (convertSide fxs c) sides
    where
        convertSide fxs c (Side n m) = Side n $ convert fxs c m


padRight n s
    | l > n = (take (n - 3) s) ++ "..."
    | otherwise = s ++ (replicate (n - l) ' ')
    where
        l = length s

padLeft n s
    | l > n = padRight n s
    | otherwise = (replicate (n - l) ' ') ++ s
    where
        l = length s


printResults (balance, expenses) = do
    let sides = zip balance expenses
    let total = sums balance expenses
    putStrLn $ (padRight 20 "name") ++ (padRight 20 "balance") ++ (padRight 20 "expenses")
    putStrLn (replicate 60 '-')
    sequence_ $ map printResult sides
    putStrLn (replicate 60 '-')
    printResult total
    where
        printResult (Side name balance, Side name1 expenses)
            | name == name1 = do
                putStrLn $ (padRight 20 name) ++ (padLeft 20 $ show balance) ++ (padLeft 20 $ show expenses)
        sums balance expenses =
            (Side "Total" (sum balance), Side "Total" (sum expenses))
        sum sides =
            foldl Money.add (Moneys []) $ [m | (Side _ m) <- sides]

