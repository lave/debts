module Param
where

import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map


-- parameter type
data ParamType =
      StringParameter
    | NumberParameter
    deriving (Eq, Show)

-- parameter aggregation type
data ParamAggType =
      NoOverride
    | Override
    | Multiple
    deriving (Eq, Show)

-- parameter descriptor
data ParamDescriptor = Param {
    paramName :: String,
    paramType :: ParamType,
    paramAggType :: ParamAggType 
} deriving (Eq, Show)


combineStrings separator = \s1 s2 -> s1 ++ separator ++ s2

parameterDescriptors = [
    Param "round.to" NumberParameter Override,
    Param "target.currency" StringParameter Override,
    Param "aggregate" StringParameter Multiple
    ]

data Param =
      StringParam String String
    | NumberParam String Double
    deriving (Eq, Show)

type Params = Map String Param


--data Params = Params [ParamDescriptor] [Param]


getDescriptor :: [ParamDescriptor] -> String -> ParamDescriptor
getDescriptor descriptors name =
    case find ((== name) . paramName) descriptors of
        Just descriptor -> descriptor
        Nothing -> error $ "Unknown parameter " ++ name


type RawParam = (String, String)

addParam :: [ParamDescriptor] -> Params -> RawParam -> Params
addParam descriptors params rawParam@(name, value) =
    case paramAggType descriptor of
        NoOverride -> Map.insertWith (flip const) name param params
        Override   -> Map.insert name param params
        Multiple   -> Map.insertWith (combineStrings name [param] params
    where
        descriptor = getDescriptor descriptors name
        param = makeParam (paramType descriptor) name value

        makeParam StringParameter name value = StringParam name value
        makeParam NumberParameter name value = NumberParam name $ read value
    

getStringParam :: Params -> String -> Maybe String
getStringParam params name =
    case Map.lookup params name of
        Just (StringParameter _ value) -> Just value
        Nothing -> Nothing
        _ -> error "Parameter " ++ name ++ " is not a number"


getNumberParam :: Params -> String -> Maybe Double
getNumberParam params name =
    case Map.lookup params name of
        Just (NumberParameter _ value) -> Just value
        Nothing -> Nothing
        _ -> error "Parameter " ++ name ++ " is not a number"
