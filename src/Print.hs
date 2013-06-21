module Print (printResults)
where

import Data.Maybe
import Data.String.Utils

import BasicTypes
import Money
import Side
import Result
import Round


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


printResults (Balance b) = do
    putStrLn $ (padRight 20 "Name") ++ (padRight 20 "Balance") ++ (padRight 20 "Expenses")
    putStrLn (replicate 60 '-')

    sequence_ $ map printResult b
    putStrLn (replicate 60 '-')

    let (names, balance, expenses) = unzip3 b
    printResult ("Total", Money.sum balance, Money.sum expenses)

    where
        printResult (name, balance, expenses) = do
            putStrLn $ (padRight 20 name) ++ (padLeft 20 $ show balance) ++ (padLeft 20 $ show expenses)



{-
    A   B   C   comment
    +4  -3  -1  beer
    -4  -4  +8  smth else
-}
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
    putStrLn $ "### Money log for " ++ name
    sequence_ $ map printLog logs

    where
        printLog (sideName, transactions) = do
            putStrLn $ "### Side " ++ sideName
            putStrLn "\"Num\",\"Date\",\"Payee\",\"Category\",\"S\",\"Withdrawal\",\"Deposit\",\"Total\",\"Comment\""
            sequence_ $ map printTransaction transactions
            putStrLn $ "### Side End"

            where
                printTransaction t = do
                    putStr $ "\"\","
                    putStr $ "\"" ++ date' ++ "\","
                    putStr $ "\"" ++ contragent' ++ "\","
                    putStr $ "\"" ++ category' ++ "\","
                    putStr $ "\"\","
                    putStr $ "\"" ++ (show sum') ++ "\","
                    putStr $ "\"\","
                    putStr $ "\"\","
                    putStr $ "\"" ++ comment' ++ "\","
                    putStrLn ""
                    where
                        Date date' = fromMaybe (Date "") $ date t
                        Contragent contragent' = fromMaybe (Contragent "") $ contragent t
                        Category categories = category t
                        category' = join ":" $ take 2 categories
                        Moneys sum' = Result.sum t
                        Comment comment' = fromMaybe (Comment "") $ comment t
