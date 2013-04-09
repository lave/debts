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
import Result
import Round


parameterDescriptors = [
    Param "round.to" NumberParameter Override,
    Param "target.currency" StringParameter Override,
    Param "aggregate" StringParameter (Concatenate ",")
    ]

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


process paramOverrides (Input rawParams, groups, fxs, transactions) =
    postprocessed
    where
        params = makeParams parameterDescriptors $ rawParams ++ paramOverrides
        preprocessed = filter params
            $ aggregate (parseAggGroups $ getStringsParam params "aggregate")
            $ convert (getStringParam params "target.currency") fxs
            $ normalize groups transactions
        processed = process' params preprocessed
        --postprocessed = round (getNumberParam params "round.to") processed
        postprocessed = processed
        
        normalize groups =
            map (normalizeTransaction groups)

        convert targetCurrency fxs transactions = 
            applyIfParamIsSet (convertSides fxs) 

        aggregate outGroups =
            map (aggregateTransaction outGroups)
    

process' :: Params -> [NormalizedTransaction] -> Result
process' params transactions =
    if hasParam params "log"
        then Log ""
        else Balance $ process'' params transactions
    


process'' params transactions = zip balance', expenses'
    where
        balance' = process' balance smartRound
        expenses' = process' expenses roundListTo

        process' calculator rounder = Data.List.sort rounded
            where
                raw = calc calculator transactions
                rounded = applyIfParamIsSet (roundSides rounder) (getNumberParam params "round.to") converted

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


printResults Result sides = do
    let total = sums $ unzip balance expenses
    putStrLn $ (padRight 20 "name") ++ (padRight 20 "balance") ++ (padRight 20 "expenses")
    putStrLn (replicate 60 '-')
    sequence_ $ map printResult sides
    putStrLn (replicate 60 '-')
    printResult total
    where
        printResult (Side name balance, Side name1 expenses)
            | name == name1 = do
                putStrLn $ (padRight 20 name) ++ (padLeft 20 $ show balance) ++ (padLeft 20 $ show expenses)

        sums (balance, expenses) =
            (Side "Total" (sum balance), Side "Total" (sum expenses))
        sum sides =
            foldl Money.add (Moneys []) $ [m | (Side _ m) <- sides]

