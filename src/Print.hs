module Print (printResults)
where

import Data.Maybe
import Data.String.Utils
import System.IO

import BasicTypes
import Money
import PrintTable
import Result
import Round
import Utils


printResults (Balance b) = do
    sequence_ $ map putStrLn $ toStrings " | " makeTable

    where
        (names, balance, expenses) = unzip3 b

        makeTable = tableBuilder
            |> addHeader ["Name", "Balance", "Expenses"]
            |> addSeparator
            |> setGlobalAlign AlignRight
            |> addRows (map toRow b)
            |> addSeparator
            |> addRow (toRow ("Total", Money.sum balance, Money.sum expenses))
            |> build

        toRow (name, balance, expenses) =
            [name, show balance, show expenses]


printResults (CommonCalculationLog names log) = do
    sequence_ $ map (\name -> putStr $ padRight 40 name) (names ++ ["Transaction"])
    putStrLn ""
    putStrLn (replicate 60 '-')

    sequence_ $ map printLogEntry log
    putStrLn (replicate 60 '-')

    let total = ("Total", totals log)
    printLogEntry total

    where
        printLogEntry (name, sides) = do
            sequence_ $ map printSide sides
            putStrLn $ "| " ++ name

        printSide (expenses, benefit, balance) =
            --putStr $ padLeft 40 $ (show expenses) ++ " - " ++ (show benefit) ++ " = " ++ (show balance)
            putStr $ padLeft 40 $ show balance

        totals :: [CommonCalculationLogEntry] -> [(Moneys, Moneys, Moneys)]
        totals log = foldl
            (\r (_, sides) -> zipWith add' r sides)
            (repeat (Moneys [], Moneys [], Moneys []))
            log
        add' (expenses, benefit, balance) (e, b, ba) = (add expenses e, add benefit b, add balance ba)


printResults (MoneyLog name logs) = do
    putStrLn $ "Money log for " ++ name
    sequence_ $ map printLog logs

    where
        printLog (sideName, transactions) = do
            let lines = "\"Num\",\"Date\",\"Payee\",\"Category\",\"S\",\"Withdrawal\",\"Deposit\",\"Total\",\"Comment\"" : map makeLine transactions
            let fileName = "moneylog_" ++ name ++ "_" ++ sideName ++ ".csv"
            putStrLn $ "Writing log for side " ++ sideName ++ " to file " ++ fileName
            file <- openFile fileName WriteMode
            sequence_ $ map (hPutStrLn file) lines
            hClose file
            where
                makeLine t =
                    "\"\",\"" ++ date' ++ "\",\"" ++ contragent' ++ "\",\"" ++ category' ++ "\",\"\",\"" ++ (show sum') ++ "\",\"\",\"\",\"" ++ comment'' ++ "\""
                    where
                        Date date' = fromMaybe (Date "") $ date t
                        Contragent contragent' = fromMaybe (Contragent "") $ contragent t
                        Category categories = category t
                        category' = join ":" $ categories
                        Moneys sum' = Result.sum t
                        Comment comment' = fromMaybe (Comment "") $ comment t
                        comment'' = if isNothing $ beneficator t
                            then comment'
                            else "<transfer to " ++ (fromJust $ beneficator t) ++ ">: " ++ comment'
