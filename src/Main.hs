module Main (main)
where

import qualified System.Environment (getArgs)

import qualified CommandLine
import InputBuilder
import Param
import Parser
import ParserMonad
import Lexer
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
            |> Preprocess.normalize groups
            |> Preprocess.filter params
            |> Preprocess.aggregate params
            |> Preprocess.convert params fxs 

        processed = preprocessed
            |> Process.process params

        postprocessed = processed
            |> Postprocess.round params
