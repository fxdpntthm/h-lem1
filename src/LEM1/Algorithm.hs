module LEM1.Algorithm ( convertToTuple
                      , computeLEM1
                      , computeAllLEM1
                      , getGlobalCovering
                      , computeRuleSet
                      , dropConditions
                      , isRuleConsistent
                      , preprocess
                      , isDataSetConsistent
                      , CPType (UA, LA)
                      , getUpperApproxDF
                      , getLowerApproxDF
                      ) where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.List

import LEM1.RoughSet (leq
                     , (<=*)
                     , upper_approx
                     , lower_approx
                     )

import Model.Rules ( Rule
                   , ruleCoverage
                   , ruleSetCoverage
                   , showRule
                   , ruleCoverageHelper
                   )

import Model.DataSet (DataFrame
                     , Rows
                     , ColumnType (Attribute, Decision)
                     , getAStar
                     , getDStar
                     , stripColumn
                     , get1, get2, get3
                     , getAttributes
                     )

import Model.Util (deleteN)
import LERS.Parser (binarizeDataFrame)

-- should compute using lower or upper?
data CPType = UA | LA

-- preprocess the dataframe into correct format
-- this will not touch any concept/decision variables
-- it will work only on attributes
preprocess :: DataFrame -> DataFrame
preprocess = id

computeAllLEM1 :: CPType -> DataFrame -> [Set Rule]
computeAllLEM1 UA df = map (uncurry (computeLEM1 odf)) frames
  where
    odf = preprocess df
    frames :: [((String, String), DataFrame)]
    frames = map (uncurry getUpperApproxDF) (binarizeDataFrame odf)
computeAllLEM1 LA df = map (uncurry (computeLEM1 odf)) frames
  where
    odf = preprocess df
    frames :: [((String, String), DataFrame)]
    frames = map (uncurry getLowerApproxDF) (binarizeDataFrame odf)

-- This will work only for symbolic attributes
-- make sure you do all the preprocessing of the attributes before hand
computeLEM1 :: DataFrame -> (String, String) -> DataFrame -> Set Rule
computeLEM1 odf target df =  Set.fromList $ map ((flip dropConditions) odf) rs
  where
    gc_df = getGlobalCovering df
    rs = Set.toList (computeRuleSet target gc_df)

-- checks if original data is consistent
isDataSetConsistent :: DataFrame -> Bool
isDataSetConsistent = isAStrLeqDStr

-- returns a subset of the dataframe
-- it will be used to compute lower approx and upper approx of a dataset
dataSubsetSelector :: DataFrame -> Set Integer -> DataFrame
dataSubsetSelector df s = (get1 df, get2 df, newRows)
  where
    originalRows = (Map.toList (get3 df))
    newRows = Map.fromList [x | x <- originalRows, ((fst x) `elem` s) ]

isAStrLeqDStr :: DataFrame -> Bool
isAStrLeqDStr df = (getAStar df) <=*  (getDStar df)

getUpperApproxDF :: (String, String) -> DataFrame -> ((String, String), DataFrame)
getUpperApproxDF (attr, val) df = ((attr, val), dataSubsetSelector df indices)
  where
    foriginalRows = filter (\(i, v) -> (snd v == val)) (Map.toList (get3 df))
    f_original_indices = Set.fromList (map (fst) foriginalRows)
    indices = upper_approx f_original_indices (getAStar df)

getLowerApproxDF :: (String, String) -> DataFrame -> ((String, String), DataFrame)
getLowerApproxDF (attr, val) df = ((attr, val), dataSubsetSelector df indices)
  where
    foriginalRows = filter (\(i, v) -> (snd v == val)) (Map.toList (get3 df))
    f_original_indices = Set.fromList (map (fst) foriginalRows)
    indices = lower_approx f_original_indices (getAStar df)


-- computes global covering for a dataset
-- keep dropping attributes till a* </= d*
getGlobalCovering :: DataFrame -> DataFrame
getGlobalCovering df =
  if(isAStrLeqDStr df)
  then getGlobalCoveringHelper 0 df
  else df

getGlobalCoveringHelper :: Integer -> DataFrame -> DataFrame
getGlobalCoveringHelper i df =
  if (i >= toInteger(length (get1 df)) || isAStrLeqDStr df)
  then df
  else
    if (isAStrLeqDStr new_df)
    then getGlobalCoveringHelper i new_df
    else getGlobalCoveringHelper (i+1) df
  where
    -- strip the ith column of df
    new_df = stripColumn (genericIndex (get1 df) i) df


-- computes the ruleset given a global covering and a target decision value
computeRuleSet :: (String, String) -> DataFrame -> Set Rule
computeRuleSet t df = Set.fromList $ zip ct (repeat t)
  where
    ct :: [[(String, String)]]
    ct = convertToTuple new_df
    new_df = ((get1 df), (get2 df), rows)
    rows = getTargetCover t df

-- convert from minimalistic rows to (a, v) pair
convertToTuple :: DataFrame -> [[(String, String)]]
convertToTuple df = map (zip columnNames) attrs
  where
    columnNames :: [String]
    columnNames = get1 df
    attrs:: [[String]]
    attrs = map snd $ Map.assocs (getAttributes df)

-- get only the rows that satisfy the decision
getTargetCover :: (String, String) -> DataFrame -> Rows
getTargetCover t df = (Map.filter (\(_, d) -> d == snd t) rs)
  where
    rs = get3 df

-- drops conditions from the rules to make them simpler
dropConditions :: Rule -> DataFrame -> Rule
dropConditions r df = dropConditionsHelper 0  r df

dropConditionsHelper :: Integer -> Rule  -> DataFrame ->  Rule
dropConditionsHelper i rule df=
  if (i == (toInteger (length (fst (rule)))))
  then rule
  else
    if (isRuleConsistent new_rule df)
    then (dropConditionsHelper i new_rule df)
    else (dropConditionsHelper (i+1) rule df)
  where
    new_rule = ruleDropNthCondition rule i


isRuleConsistent :: Rule -> DataFrame -> Bool
isRuleConsistent r df = (ruleCoverage r df
  `Set.difference`  ruleCoverageHelper Decision (snd r) df) == Set.empty

ruleDropNthCondition :: Rule -> Integer -> Rule
ruleDropNthCondition (attrs, des) i = ((deleteN i attrs), des)
