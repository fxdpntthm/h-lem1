module LEM1.Algorithm ( convertToTuple
                      , computeLEM1
                      , getGlobalCovering
                      , computeRuleSet
                      , dropConditions
                      , isRuleConsistent
                      ) where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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

computeLEM1 :: (String, String) -> DataFrame -> Set Rule
computeLEM1 target df = undefined

-- computes global covering for a dataset
-- keep dropping attributes till a* </= d*
getGlobalCovering :: DataFrame -> DataFrame
getGlobalCovering = id

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
dropConditions :: Set Int -> DataFrame -> Rule -> Rule
dropConditions targetCover df r = undefined

isRuleConsistent :: Rule -> DataFrame -> Bool
isRuleConsistent r df = (ruleCoverage r df
  `Set.difference`  ruleCoverageHelper Decision (snd r) df) == Set.empty

ruleDropNthCondition :: Rule -> Integer -> Rule
ruleDropNthCondition (attrs, des) i = ((deleteN i attrs), des)
