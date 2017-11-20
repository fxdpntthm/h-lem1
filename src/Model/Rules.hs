module Model.Rules ( Rule
                   , checkRule
                   , ruleSetCoverage
                   , showRule
                   ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.List (elemIndex, genericIndex, intercalate)

import Model.DataSet (DataFrame
                     , Rows
                     , Column (SC, NC)
                     , get1, get2, get3
                     , ColumnType (Attribute, Decision)
                     )

type Rule = ([(String, String)], (String, String))

showRule :: Rule -> String
showRule r =
  (filter (/= '\"') (partAttrs ++ " -> " ++ partDec))
  where
    partAttrs = intercalate " & " $ map show (fst r)
    partDec = show (snd r)

ruleSetCoverage :: Set Rule -> DataFrame -> Set Integer
ruleSetCoverage rs df =
  Set.foldl (Set.union) (Set.empty) (Set.map ((flip checkRule) df) rs)

-- returns the cases satisfying the rule
checkRule :: Rule -> DataFrame -> Set Integer
checkRule rule df =
  (checkRuleHelper Decision (snd rule) df) `Set.intersection`
  (foldl (Set.intersection) (Map.keysSet rs)
  $ map ((flip (checkRuleHelper Attribute)) df) (fst rule))
  where
    rs = get3 df

checkAttributeRules:: (String, String) -> DataFrame -> Set Integer
checkAttributeRules t df =
  case columnIndex of
    Just i  -> Map.keysSet
      (Map.filter (\(attrs, _) -> (genericIndex attrs i) == snd t) rs)
    Nothing -> Set.empty
  where
    columnName = fst t
    columnIndex = elemIndex columnName  (get1 df)
    rs = get3 df

checkDecisionRules :: (String, String) -> DataFrame -> Set Integer
checkDecisionRules t df =  Map.keysSet (Map.filter (\(_, d) -> d == snd t) rs)
  where
    rs = get3 df

checkRuleHelper :: ColumnType -> (String, String) -> DataFrame -> Set Integer
checkRuleHelper ct t df =
  case ct of
    Attribute -> checkAttributeRules t df
    Decision  -> checkDecisionRules t df

