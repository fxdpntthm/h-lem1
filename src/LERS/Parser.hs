{-# LANGUAGE ScopedTypeVariables #-}
module LERS.Parser ( dataFrameParser
                   , replaceDecisions
                   , getAllConcepts
                   , binarizeDataFrame
                   ) where

import Model.DataSet (DataFrame
                     , Rows
                     , showDF
                     , get1, get2, get3
                     )

import Data.Char ( isAlphaNum
                 , isSpace
                 , isPunctuation
                 )

import Text.ParserCombinators.ReadP
import Control.Monad
import Data.Maybe
import Data.List

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


dataFrameParser :: String -> DataFrame
dataFrameParser file = (init columnNames, last columnNames, rws)
  where
    ls = tail $ lines file
    columnNames = words $ sanitize (head ls)
    dataRows :: [([String], String)]
    dataRows = map (convertToDataRow) (map (words) (tail ls))
    rws::Rows
    rws = Map.fromList (zip [0..] dataRows)

sanitize :: String -> String
sanitize xs = [ x | x <- xs, not (x `elem` "<>[]") ]

convertToDataRow :: [String] -> ([String], String)
convertToDataRow lst = (init lst, last lst)

getAllConcepts :: DataFrame -> [(String, String)]
getAllConcepts df = zip (repeat $ get2 df) (nub decisions)
  where
    dataRows :: [(Integer, ([String], String))]
    dataRows = Map.toList $ get3 df
    decisions = map (snd.snd) dataRows

binarizeDataFrame :: DataFrame -> [((String, String), DataFrame)]
binarizeDataFrame df = map (\(des::(String, String), df::(DataFrame)) -> (des, replaceDecisions df (snd des))) ps
  where
    concepts :: [(String, String)]
    concepts = getAllConcepts df
    ps :: [((String, String), DataFrame)]
    ps = zip concepts (repeatDF (length concepts) df)

-- binarizes the decision table
-- takes a dataframe and replaces all the occurances of
-- data not equal to st to SPECIAL
replaceDecisions :: DataFrame -> String -> DataFrame
replaceDecisions df st = (get1 df, get2 df, newRows)
  where
    dataRows :: [(Integer, ([String], String))]
    dataRows = Map.toList $ get3 df
    newRows :: Rows
    newRows = Map.fromList (map (replaceDecisionHelper st) dataRows)

replaceDecisionHelper :: String -> (Integer, ([String], String)) -> (Integer, ([String], String))
replaceDecisionHelper st r = if (decision == st)
                             then r
                             else (fst r, ((fst . snd) r, "SPECIAL"))
  where
    decision = (snd . snd) r

-- TODO
-- discretizes data frame using all cutpoints method
discretizeDataFrame :: DataFrame -> DataFrame
discretizeDataFrame = id

repeatDF :: Int -> DataFrame -> [DataFrame]
repeatDF 0 df = []
repeatDF i df = df : repeatDF (i-1) df
