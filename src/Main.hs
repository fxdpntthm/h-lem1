module Main where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import System.Directory (doesFileExist)

import LEM1.RoughSet ( leq
                     , (<=*)
                     , upper_approx
                     , lower_approx
                     )

import Model.Rules ( Rule
                   , ruleCoverage
                   , ruleSetCoverage
                   , showRule
                   , showRuleSet
                   )

import Model.DataSet ( DataFrame
                     , Rows
                     , getAStar
                     , getDStar
                     , stripColumn
                     , showDF
                     )

import LEM1.Algorithm ( convertToTuple
                      , computeLEM1
                      , computeAllLEM1
                      , getGlobalCovering
                      , computeRuleSet
                      , dropConditions
                      , isRuleConsistent
                      , isDataSetConsistent
                      , preprocess
                      , CPType (UA, LA)
                      )

import LERS.Parser ( dataFrameParser
                   , replaceDecisions
                   , getAllConcepts
                   )

main  :: IO()
main = do
  putStrLn "Please enter file path of dataset: "
  line <- getLine
  exists <- doesFileExist line
  if (exists)
    then do
         -- putStrLn "file exists"
         f <- readFile line
         let dfp = preprocess (dataFrameParser f)
         -- discretize the numerical attributes
         -- check for inconsistency in dataframe
         if (isDataSetConsistent dfp)
           then do
               putStrLn "Data set is consistent"
               putStrLn $ "writing certain rules to file: " ++ line ++ ".rules.certain"
               writeFile (line ++ ".rules.certain")
                 (unlines $ map showRuleSet (computeAllLEM1 LA dfp))
               writeFile (line ++ ".rules.possible") "! Possible rule set is not shown since it is identical with the certain rule set"
           else do
               putStrLn "Data set is not consistent"
               putStrLn $ "writing certain rules to file: " ++ line ++ ".rules.certain"
               writeFile (line ++ ".rules.certain")
                 (unlines $ map showRuleSet (computeAllLEM1 LA dfp))
               putStrLn $ "writing possible rules to file: " ++ line ++ ".rules.possible"
               writeFile (line ++ ".rules.possible")
                 (unlines $ map showRuleSet (computeAllLEM1 UA dfp))
    else do
         putStrLn "File does not exists please enter correct file path\n"
         main
