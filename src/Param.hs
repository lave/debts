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
    | Concatenate String
    deriving (Eq, Show)

-- parameter descriptor
data ParamDescriptor = Param {
    paramName :: String,
    paramType :: ParamType,
    paramAggType :: ParamAggType 
} deriving (Eq, Show)


concatStrings separator = \s1 s2 -> s1 ++ separator ++ s2

type RawParam = (String, String)
type RawParams = Map String String


getDescriptor :: [ParamDescriptor] -> String -> ParamDescriptor
getDescriptor descriptors name =
    case find ((== name) . paramName) descriptors of
        Just descriptor -> descriptor
        Nothing -> error $ "Unknown parameter " ++ name


addParam :: [ParamDescriptor] -> RawParams -> RawParam -> RawParams
addParam descriptors params (name, value) =
    case paramAggType descriptor of
        NoOverride ->
            Map.insertWith (flip const) name value params
        Override ->
            Map.insert name value params
        Concatenate separator ->
            Map.insertWith (flip $ concatStrings separator) name value params
    where
        descriptor = getDescriptor descriptors name


data Param =
      StringParam String
    | NumberParam Double
    deriving (Eq, Show)


type Params = Map String Param
parseParams :: [ParamDescriptor] -> RawParams -> Params
parseParams descriptors params =
    Map.mapWithKey (makeParam descriptors) params
    where
        makeParam descriptors name value =
            makeParam' (paramType $ getDescriptor descriptors name) value
        makeParam' StringParameter value = StringParam value
        makeParam' NumberParameter value = NumberParam $ read value


makeParams descriptors rawParams =
    parseParams descriptors $
        foldl (addParam descriptors) Map.empty rawParams


getStringParam :: Params -> String -> Maybe String
getStringParam params name =
    case Map.lookup name params of
        Just (StringParam value) -> Just value
        Nothing -> Nothing
        _ -> error $ "Parameter " ++ name ++ " is not a string"


getStringsParam :: Params -> String -> [String]
getStringsParam params name =
    case Map.lookup name params of
        Just (StringParam value) -> [value]
        Nothing -> []
        _ -> error $ "Parameter " ++ name ++ " is not a string"


getNumberParam :: Params -> String -> Maybe Double
getNumberParam params name =
    case Map.lookup name params of
        Just (NumberParam value) -> Just value
        Nothing -> Nothing
        _ -> error $ "Parameter " ++ name ++ " is not a number"

