module Print (printResults)
where


import Money
import Transaction
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


printResults (Balances sides) = do
    let total = sums $ unzip sides
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

