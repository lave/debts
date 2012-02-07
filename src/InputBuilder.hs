module InputBuilder
where

import Date
import Fx
import Option
import Transaction

data Builder =
      ParameterBuilder Option
    | GroupBuilder Group
    | FxBuilder Fx
    | DateBuilder Date
    | TransactionBuilder RawTransaction
    deriving Show
    
data Input = Input [Option] [Group] [Fx] [RawTransaction]

data Context = Context (Maybe Date)


buildInputData :: [Builder] -> Input
buildInputData builders =
    Input (reverse parameters) (reverse groups) (reverse fxs) (reverse transactions)
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
            (Context (Just date), input)
        build (TransactionBuilder transaction) (context@(Context date), Input parameters groups fxs transactions) =
            (context, Input parameters groups fxs ((setDate date transaction) : transactions))

        setDate _ transaction = transaction
       
        
        
        
