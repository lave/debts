module CommandLine
where

import Data.Maybe
import System.Console.GetOpt

import Param
import Utils


data Options = Options {
    optHelp :: Bool,
    optVerbose :: Bool,
    optParams :: [RawParam],
    optBalance :: Bool,
    optLog :: Bool,
    optMoneyLog :: Maybe String,
    optSpendingByCategory :: Maybe (Maybe Int)
    } deriving (Show)

startOptions = Options {
    optHelp = False,
    optVerbose = False,
    optParams = [],
    optBalance = False,
    optLog = False,
    optMoneyLog = Nothing,
    optSpendingByCategory = Nothing
    }

cmdOptionDescriptors = [
    Option "h" ["help"]
        (NoArg (\opt -> opt { optHelp = True }))
        "print help",
    Option "v" ["verbose"]
        (NoArg (\opt -> opt { optVerbose = True }))
        "be verbose",
    Option "D" []
        (ReqArg (\arg opt -> opt { optParams = (splitBy (== '=') arg) : (optParams opt) }) "parameter")
        "define calculation parameter",
    Option "b" ["balance"]
        (NoArg (\opt -> opt { optBalance = True }))
        "calculate balance (default)",
    Option "l" ["log"]
        (NoArg (\opt -> opt { optLog = True }))
        "print log of calculating balance",
    Option "" ["ml"]
        (ReqArg (\arg opt -> opt { optMoneyLog = Just arg }) "participant name")
        "generate files for importing into AceMoney",
    Option "C" ["by-cat"]
        (OptArg (\arg opt -> opt { optSpendingByCategory = Just $ fmap (read :: String -> Int) arg }) "depth")
        "print spendings by category"
    ]


splitBy predicate [] = ([], [])
splitBy predicate (x : xs)
    | predicate x = ([], xs)
    | otherwise = (x : fst, snd)
        where (fst, snd) = splitBy predicate xs

getOptions args = (options, files, errors)
    where
        (optionActions, files, errors) = getOpt Permute cmdOptionDescriptors args
        options = foldl (|>) startOptions optionActions

getUsageInfo = usageInfo "Usage: debts FILE [OPTION...]" cmdOptionDescriptors
