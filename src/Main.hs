{-# LANGUAGE PatternGuards #-}

module Main (main)
where

import Control.Monad
import qualified Data.Map as Map
import qualified System.Environment (getArgs)
import qualified System.Exit

import qualified CommandLine
import qualified Fx
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
    Param "default.currency" StringParameter Override,
    Param "target.currency" StringParameter Override,
    Param "aggregate" StringParameter (Concatenate ","),
    Param "split" BoolParameter Override
    ]

main = do
    args <- System.Environment.getArgs
    let (options, files, errors) = CommandLine.getOptions args

    when (errors /= []) $ mapM_ putStrLn errors >> System.Exit.exitFailure

    let isHelp = CommandLine.optHelp options
    when isHelp $ putStrLn CommandLine.getUsageInfo

    let isVerbose = CommandLine.optVerbose options
    let parameters = CommandLine.optParams options
    let mode = determineMode options
    let fileName = head files

    s <- readFile fileName
    let tokens = alexScanTokens s
    when isVerbose $ putStrLn $ "Tokens:\n" ++ show tokens

    case parse tokens of
        Ok parsed -> do
            when isVerbose $ putStrLn $ "Syntax tree:\n" ++ show parsed
            printResults $ process mode parameters $ buildInputData parsed
        Error s ->
            putStrLn ("Parse error:" ++ s)


process mode paramOverrides (Input rawParams groups fxs transactions) = postprocessed
    where
        params = makeParams parameterDescriptors $ rawParams ++ paramOverrides
        fxs_ = Fx.make fxs
        groups_ = Map.fromList $ map (\(Group name side) -> (name, side)) groups

        preprocessed = transactions
            |> Preprocess.normalize groups_
            |> Preprocess.assignDefaultCurrency params
            |> Preprocess.filter params
            |> Preprocess.aggregate params
            |> Preprocess.splitGroups params
            |> Preprocess.convert params fxs_

        processed = preprocessed
            |> Process.process mode

        postprocessed = processed
            |> Postprocess.round params


determineMode options
    | CommandLine.optLog options = Process.CommonCalculationLog
    | Just name <- CommandLine.optMoneyLog options = Process.MoneyLog name
    | Just depth <- CommandLine.optSpendingByCategory options = Process.SpendingsByCategory depth
    | otherwise = Process.Balance
