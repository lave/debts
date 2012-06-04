module InputBuilder
where

import Data.List

import Date
import Fx
import Money
import Param
import Transaction


data Builder =
      ParameterBuilder RawParam
    | GroupBuilder Group
    | FxBuilder Fx
    | DateBuilder (Maybe Date)
    | TransactionBuilder
        [RawSide] [RawSide] (Maybe Moneys) [TransactionAttributeBuilder]
    deriving Show

data TransactionAttributeBuilder =
      ContragentBuilder Contragent
    | CategoryBuilder [Category]
    | TagsBuilder [Tag]
    | CommentBuilder Comment
    deriving Show
    
    
data Input = Input [RawParam] [Group] [Fx] [RawTransaction]

data Context = Context (Maybe Date)


buildInputData :: [Builder] -> Input
buildInputData builders =
    Input
        (reverse parameters)
        (reverse groups)
        (reverse fxs)
        (reverse transactions)
    where
        (_, Input parameters groups fxs transactions) =
            foldl build (Context Nothing, Input [] [] [] []) $ builders

        build :: (Context, Input) -> Builder -> (Context, Input)
        build (context, Input parameters groups fxs transactions) (ParameterBuilder parameter) =
            (context, Input (parameter : parameters) groups fxs transactions)
        build (context, Input parameters groups fxs transactions) (GroupBuilder group) =
            (context, Input parameters (group : groups) fxs transactions)
        build (context, Input parameters groups fxs transactions) (FxBuilder fx) =
            (context, Input parameters groups (fx : fxs) transactions)
        build (context, input) (DateBuilder date) =
            (Context date, input)
        build (context@(Context date), Input parameters groups fxs transactions) transaction@(TransactionBuilder _ _ _ _) =
            (context, Input parameters groups fxs ((buildTransaction date transaction) : transactions))
       

buildTransaction :: Maybe Date -> Builder -> RawTransaction
buildTransaction date (TransactionBuilder payers beneficators sum attributes) =
    foldl
        applyAttribute
        (Transaction payers beneficators sum date Nothing [] [] Nothing)
        attributes
    where
        applyAttribute transaction (ContragentBuilder contragent) =
            transaction { contragent = Just contragent }
        applyAttribute transaction (CategoryBuilder category) =
            transaction { category = category }
        applyAttribute transaction (TagsBuilder tags) =
            transaction { tags = nub tags }
        applyAttribute transaction (CommentBuilder comment) =
            transaction { comment = Just comment }

