module Param (
    ParamType(..), ParamAggType(..), ParamDescriptor(..),
    RawParam,
    Params,
    makeParams,
    hasParam,
    getStringParam, getStringsParam, getNumberParam, getBoolParam)
where

import Data.List
import Data.Maybe
import qualified Data.Map as Map

import Utils


-- parameter type
data ParamType =
      StringParameter
    | NumberParameter
    | BoolParameter
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
type RawParams = Map.Map String String


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
      StringParam ParamDescriptor String
    | NumberParam ParamDescriptor Double
    | BoolParam ParamDescriptor Bool
    deriving (Eq, Show)
type Params = Map.Map String Param

parseParams :: [ParamDescriptor] -> RawParams -> Params
parseParams descriptors params =
    Map.mapWithKey (makeParam descriptors) params
    where
        makeParam descriptors name value =
            makeParam' (paramType descriptor) value
            where
                descriptor = getDescriptor descriptors name
                makeParam' StringParameter value = StringParam descriptor value
                makeParam' NumberParameter value = NumberParam descriptor $ read value
                makeParam' BoolParameter value = BoolParam descriptor $ parseBool value
                
                parseBool s
                    | s == "" || s == "true" = True
                    | otherwise = False


makeParams :: [ParamDescriptor] -> [RawParam] -> Params
makeParams descriptors rawParams =
    parseParams descriptors $
        foldl (addParam descriptors) Map.empty rawParams


getStringParam :: Params -> String -> Maybe String
getStringParam params name =
    case Map.lookup name params of
        Just (StringParam _ value) -> Just value
        Nothing -> Nothing
        _ -> error $ "Parameter " ++ name ++ " is not a string"


getStringsParam :: Params -> String -> [String]
getStringsParam params name =
    case Map.lookup name params of
        Just (StringParam descriptor value) ->
            case descriptor of
                Param _ _ (Concatenate separator) -> split separator value
                _ -> [value]
        Nothing -> []
        _ -> error $ "Parameter " ++ name ++ " is not a string"


getNumberParam :: Params -> String -> Maybe Double
getNumberParam params name =
    case Map.lookup name params of
        Just (NumberParam _ value) -> Just value
        Nothing -> Nothing
        _ -> error $ "Parameter " ++ name ++ " is not a number"

getBoolParam :: Params -> String -> Bool
getBoolParam params name =
    case Map.lookup name params of
        Just (BoolParam _ value) -> value
        Nothing -> False
        _ -> error $ "Parameter " ++ name ++ " is not a boolean value"

hasParam :: String -> Params -> Bool
hasParam name params =
    Map.member name params
