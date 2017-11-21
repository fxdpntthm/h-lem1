module Main where

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
                   )

import Model.DataSet (DataFrame
                     , Rows
                     -- , Column (SC, NC) 
                     , getAStar
                     , getDStar
                     , stripColumn
                     , showDF
                     )

import LEM1.Algorithm (convertToTuple
                      , computeLEM1
                      , getGlobalCovering
                      , computeRuleSet
                      , dropConditions
                      , isRuleConsistent
                      )

main  :: IO()
main = putStrLn "Hello World!"
