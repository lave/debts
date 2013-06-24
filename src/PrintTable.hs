module PrintTable
where

import Data.String.Utils

import Utils


data Align = AlignLeft | AlignRight | AlignCenter
    deriving (Show, Eq)

data Cell = Cell Align String
    deriving (Show, Eq)

type RowText = [String]

data Row =
      Row [Cell]
    | Separator
    deriving (Show, Eq)


data Table = Table [Row]
    deriving (Show, Eq)

data TableBuilder = TableBuilder [Align] [Row]




tableBuilder = setGlobalAlign AlignLeft $ TableBuilder [] []


addHeader :: RowText -> TableBuilder -> TableBuilder
addHeader header (TableBuilder format rows) =
    TableBuilder format $ Row (map (Cell AlignCenter) header) : rows

addRow :: RowText -> TableBuilder -> TableBuilder
addRow row (TableBuilder format rows) =
    TableBuilder format $ Row (zipWith Cell format row) : rows

addRows :: [RowText] -> TableBuilder -> TableBuilder
addRows rows tableBuilder = foldr addRow tableBuilder rows

addSeparator :: TableBuilder -> TableBuilder
addSeparator (TableBuilder format rows) =
    TableBuilder format $ Separator : rows

setGlobalAlign :: Align -> TableBuilder -> TableBuilder
setGlobalAlign align (TableBuilder _ rows) =
    TableBuilder (repeat align) rows

setAlign :: Int -> Align -> TableBuilder -> TableBuilder
setAlign n align (TableBuilder format rows) =
    TableBuilder (setNth n align format) rows

build :: TableBuilder -> Table
build (TableBuilder _ rows) = Table $ reverse rows


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

padBoth n s = padRight n $ padLeft l s
    where
        l = (n + length s) `div` 2


toStrings :: String -> Table -> [String]
toStrings delimeter (Table rows) = map toString rows
    where
        getCellsLength (Row cells) = map (\(Cell _ s) -> length s) cells
        getCellsLength Separator = repeat 0

        lengths = foldl (\a x -> zipWith max a $ getCellsLength x) (repeat 0) rows

        l = sum lengths + (length lengths - 1) * (length delimeter)

        toString Separator = replicate l '-'
        toString (Row cells) = toStringCells cells
        toStringCells cells = join delimeter $ zipWith toStringN lengths cells

        toStringN n (Cell AlignLeft text) = padRight n text
        toStringN n (Cell AlignRight text) = padLeft n text
        toStringN n (Cell AlignCenter text) = padBoth n text
