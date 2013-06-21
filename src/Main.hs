{-# LANGUAGE PatternGuards #-}

module Main (main)
where

import qualified System.Environment (getArgs)
import Control.Monad

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
    let mode = determineMode args

    s <- readFile fileName
    let tokens = alexScanTokens s
    when isVerbose $ putStrLn $ "Tokens:\n" ++ show tokens

    case parse tokens of
        Ok parsed -> do
            when isVerbose $ putStrLn $ "Syntax tree:\n" ++ show parsed
            printResults $ process mode parameters $ buildInputData parsed
        Error s ->
            putStrLn ("Parse error:" ++ s)


process mode paramOverrides (Input rawParams groups fxs transactions) =
    postprocessed
    where
        params = makeParams parameterDescriptors $ rawParams ++ paramOverrides

        preprocessed = transactions
            |> Preprocess.normalize groups
            |> Preprocess.filter params
            |> Preprocess.aggregate params
            |> Preprocess.convert params fxs 

        processed = preprocessed
            |> Process.process mode

        postprocessed = processed
            |> Postprocess.round params


determineMode args
    | CommandLine.containsKey args "--log" = Process.CommonCalculationLog
    | Just name <- CommandLine.getKey args "--ml" = Process.MoneyLog name
    | otherwise = Process.Balance

