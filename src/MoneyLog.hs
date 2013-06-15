module MoneyLog
where

import qualified Data.List as List
import qualified Data.Set as Set
import Data.Function
import Data.Maybe

import BasicTypes
import Balance (subSides)
import Money
import Side
import Transaction
import qualified Result
import Utils


log :: Name -> Transaction.Transactions -> Result.Result
log name transactions = Result.MoneyLog name $
    transactions
        |> concatMap (splitTransaction name)
        |> List.sortBy (compare `on` Result.payer)
        |> List.groupBy ((==) `on` Result.payer)
        |> map (\t -> (Result.payer $ head t, t))


intersectSides :: Sides -> Sides -> Sides
intersectSides l r = mapMaybe (intersect' l) r
    where
        intersect' l (Side n1 m1)
            | found == Nothing = Nothing
            | otherwise = Just $ Side n1 (intersect m1 m2)
            where
                found = List.find (\(Side n _) -> n == n1) l
                Side n2 m2 = fromJust found


splitTransaction :: Name -> Transaction -> [Result.MLTransaction]
splitTransaction name transaction = me2me ++ me2them ++ them2me
    where
        payers = Transaction.payers transaction
        beneficators = Transaction.beneficators transaction

        selfExpenses = intersectSides payers beneficators
        payers' = subSides payers selfExpenses
        beneficators' = subSides beneficators selfExpenses

        me2me'   = getMoneyFor name selfExpenses
        me2them' = getMoneyFor name payers'
        them2me' = getMoneyFor name beneficators'

        -- internal transaction from me to me doesn't count at all
        -- external transaction form me to me is transaction with contragent
        me2me = if isInternal
            then []
            else catMaybes [makeTs me2me' name Nothing transaction]

        -- internal transaction from me to them is internal transfer between accounts
        -- external transaction form me to them is transfer between accounts with contragent
        possible_me2them = map ((intersect me2them') . getMoney) beneficators'
        me2them = if Money.sum possible_me2them == me2them'
            then mapMaybe (\(Side them money) ->
                makeTs (intersect me2them' money) name (Just them) transaction) beneficators'
            else error $ "Splitting payer '" ++ name
                ++ "' to beneficators is ambiguous for transaction "
                ++ (show transaction)
                {-
                ++ "\nself' = " ++ (show selfExpenses)
                ++ "\npayers' = " ++ (show payers')
                ++ "\nbeneficators' = " ++ (show beneficators')
                ++ "\nme2them' = " ++ (show me2them')
                ++ "\npot. me2them' = " ++ (show possible_me2them)
                ++ "\npot. sum = " ++ show(Money.sum possible_me2them)
                ++ "\nequals = " ++ show (Money.sum possible_me2them == me2them')
                -}
        
        -- internal transaction from them to me is internal transfer between accounts
        -- external transaction form them to me is transaction with contragent
        possible_them2me = map ((intersect them2me') . getMoney) payers'
        them2me = if Money.sum possible_them2me == them2me'
            then mapMaybe (\(Side them money) ->
                makeTs (intersect them2me' money) them (if isInternal then Just name else Nothing) transaction) payers'
            else error $ "Splitting beneficator '" ++ name
                ++ "' to payers is ambiguous for transaction "
                ++ (show transaction)

        isInternal = contragent transaction == Just Internal

        getMoney (Side _ money) = money

        makeTs sum payer beneficator t
            | empty sum = Nothing
            | otherwise = Just $ Result.MLTransaction
                payer beneficator sum (date t) (contragent t) (category t) (tags t) (comment t)
