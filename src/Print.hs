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


putLines lines = do
    sequence_ $ map putStrLn lines


printResults (Balance b) = do
    putLines $ toStrings " | " makeTable

    where
        (names, balance, expenses) = unzip3 b

        makeTable = tableBuilder
            |> addHeader ["Name", "Balance", "Expenses"]
            |> addSeparator
            |> setGlobalAlign AlignRight
            |> setAlign 0 AlignLeft
            |> addRows (map toRow b)
            |> addSeparator
            |> addRow (toRow ("Total", Money.sum balance, Money.sum expenses))
            |> build

        toRow (name, balance, expenses) =
            [name, show balance, show expenses]


printResults (CommonCalculationLog names log) = do
    putLines $ toStrings " | " makeTable

    where
        makeTable = tableBuilder
            |> addHeader (names ++ ["Transaction"])
            |> addSeparator
            |> setGlobalAlign AlignRight
            |> setAlign (length names) AlignLeft
            |> addRows (map (toRow True) log)
            |> addSeparator
            |> addRow (toRow False ("Total", totals log))
            |> build

        toRow verbose (name, sides) =
            (map (printSide verbose) sides) ++ [name]

        printSide verbose (expenses, benefit, balance) =
            if empty expenses && empty benefit
                then ""
                else if verbose
                    then (show expenses) ++ " - " ++ (show benefit) ++ " = " ++ (show balance)
                    else show balance

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
                    "\"\",\"" ++ date' ++ "\",\"" ++ contragent'' ++ "\",\"" ++ category' ++ "\",\"\",\"" ++ (show sum') ++ "\",\"\",\"\",\"" ++ comment'' ++ "\""
                    where
                        isTransfer = not $ isNothing $ beneficator t
                        account = fromJust $ beneficator t
                        date' = maybe "" show $ date t
                        contragent' = getContragent $ contragent t
                        contragent'' = if isTransfer then account else contragent'
                        Category categories = category t
                        category' = join ":" $ categories
                        Moneys sum' = Result.sum t
                        Comment comment' = fromMaybe (Comment "") $ comment t
                        comment'' = comment'
                            |> prependIf (not $ null contragent') ("<payee:" ++ contragent' ++ "> ")
                            |> prependIf isTransfer ("<transfer to " ++ (fromJust $ beneficator t) ++ ">: ")

                        getContragent Nothing = ""
                        getContragent (Just Internal) = ""
                        getContragent (Just (Contragent contragent)) = contragent

                        prependIf :: Bool -> String -> String -> String
                        prependIf False _ string = string
                        prependIf True prefix string = prefix ++ string

printResults (SpendingsBy spendings) = do
    putLines $ toStrings " | " makeTable
    where
        makeTable = tableBuilder
            |> addHeader ("Category" : allNames)
            |> addSeparator
            |> setGlobalAlign AlignRight
            |> setAlign 0 AlignLeft
            |> addRows (map toRow spendings)
            |> addSeparator
            |> addRow (toRow $ totals spendings)
            |> build

        allNames = map fst $ snd $ head spendings

        toRow :: (Name, [(Name, Moneys)]) -> [String]
        toRow (cat, sides) =
            cat : (map ((showMoneys "") . snd) sides)

        totals :: [(Name, [(Name, Moneys)])] -> (Name, [(Name, Moneys)])
        totals spendings = ("Total", map (\m -> ("", m)) $ foldl (\r sides -> zipWith add r (map snd sides)) (repeat $ Moneys []) (map snd spendings))

showMoneys emptyString moneys
    | empty moneys = emptyString
    | otherwise = show moneys
