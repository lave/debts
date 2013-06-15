module Print (printResults)
where

import Data.Maybe

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
            sequence_ $ map printTransaction transactions
            putStrLn $ "### Side End"

            where
                printTransaction t = do
                    putStr $ payer t
                    putStr ", "
                    putStr $ fromMaybe "" $ beneficator t
                    putStr ", "
                    putStr $ show $ Result.sum t
                    putStr ", "
                    putStr $ show $ date t
                    putStr ", "
                    putStr $ show $ contragent t
                    putStr ", "
                    putStr $ show $ category t
                    putStr ", "
                    putStr $ show $ tags t
                    putStr ", "
                    putStr $ show $ comment t
                    putStrLn ""
