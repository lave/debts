module Main (main)
where

import qualified System.Environment (getArgs)
import qualified Data.List (sort)

import Aggregation
import qualified CommandLine
import InputBuilder
import Param
import Parser
import ParserMonad
import Lexer
import Transaction
import Side
import qualified Preprocess
import qualified Process
import qualified Postprocess
import Print
import Result
import Utils


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
            printResults $ process parameters $ buildInputData parsed
        Error s ->
            putStrLn ("Parse error:" ++ s)


process paramOverrides (Input rawParams groups fxs transactions) =
    postprocessed
    where
        params = makeParams parameterDescriptors $ rawParams ++ paramOverrides

        preprocessed = transactions
            Utils.>> Preprocess.normalize groups
            Utils.>> Preprocess.filter
            Utils.>> Preprocess.aggregate (parseAggGroups $ getStringsParam params "aggregate")
            Utils.>> Preprocess.convert fxs (getStringParam params "target.currency")

        processed = Process.process (getOperation params) preprocessed

        --postprocessed = round (getNumberParam params "round.to") processed
        postprocessed = processed
        

getOperation :: Params -> Process.Operation
getOperation params
    | hasParam "log" params = Process.Log
    | otherwise = Process.Balance
