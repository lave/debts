module InputBuilder
where

import Date
import Fx
import Money
import Option
import Transaction


data Builder =
      ParameterBuilder Option
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
    
    
data Input = Input [Option] [Group] [Fx] [RawTransaction]

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
            foldr build (Context Nothing, Input [] [] [] []) builders

        build :: Builder -> (Context, Input) -> (Context, Input)
        build (ParameterBuilder parameter) (context, Input parameters groups fxs transactions) =
            (context, Input (parameter : parameters) groups fxs transactions)
        build (GroupBuilder group) (context, Input parameters groups fxs transactions) =
            (context, Input parameters (group : groups) fxs transactions)
        build (FxBuilder fx) (context, Input parameters groups fxs transactions) =
            (context, Input parameters groups (fx : fxs) transactions)
        build (DateBuilder date) (context, input) =
            (Context date, input)
        build transaction@(TransactionBuilder _ _ _ _) (context@(Context date), Input parameters groups fxs transactions) =
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
            transaction { tags = tags }
        applyAttribute transaction (CommentBuilder comment) =
            transaction { comment = Just comment }

