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
                      , findNumericalColumns
                      , addColumnToDF
                      , findCutPoints
                      , generateCutPointColumn
                      , discretizeDataFrame
                      ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Char (isDigit)
import Data.List
import Text.Read
import Data.Foldable (concatMap, toList)

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


------------------------------------------------------------------------
-- Some helper functions

flatMap f = concatMap (Data.Foldable.toList . f)
windows n xs = transpose (take n (tails xs))

------------------------------------------------

-- preprocess the dataframe into correct format
-- this will not touch any concept/decision variables
-- it will work only on attributes
preprocess :: DataFrame -> DataFrame
preprocess = discretizeDataFrame

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
dropConditionsHelper i rule df =
  if (i >= (toInteger (length (fst (rule)))))
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


-- foldl (b -> a -> b) -> [a] -> b
-- a = columns
-- b = dataframe

-- discretizes data frame using all cutpoints method
discretizeDataFrame :: DataFrame -> DataFrame
discretizeDataFrame df = foldl (addColumnToDF) (numericalColumsRemovedDF) newColumns
  where
    colNames :: [String]
    colNames = findNumericalColumns df
    cutPoints :: [(String,[Float])]
    cutPoints = zip colNames $ map (findCutPoints df) colNames
    newColumnCutPoints = flatMap (flattenColumn) cutPoints
    newColumns :: [(String, [String])]
    newColumns = reverse $ map (uncurry (generateCutPointColumn df)) newColumnCutPoints
    numericalColumsRemovedDF :: DataFrame
    numericalColumsRemovedDF = foldl (flip stripColumn) df colNames

flattenColumn :: (String, [Float]) -> [(String, Float)]
flattenColumn (n, vals) = zip (repeat n) vals

-- returns a list of all the columns that need to be discretized
findNumericalColumns :: DataFrame -> [String]
findNumericalColumns df = map (fst) $ filter (isNumber . snd) z
  where
    z :: [(String, String)]
    z = zip (get1 df) (snd $ head (Map.toList (getAttributes df)))

-- converts the value into "lower..avg" or "avg..higher"
convertToSymbolic :: (Float, Float, Float) -> Float -> String
convertToSymbolic (minV, avgV, maxV) num =
  if (num <  avgV)
  then (show minV ++ ".." ++ show avgV)
  else (show avgV ++ ".." ++ show maxV)

-- given column name and cut point it generates the correct column name and values
generateCutPointColumn :: DataFrame -> String -> Float -> (String, [String])
generateCutPointColumn df colName cutPt = (colName ++ "_" ++ show cutPt, newVals)
  where
    col = findAttributeValues df colName
    numList :: [Float]
    numList = sort $ flatMap (readMaybe) col -- I am assuming all Maybe are Just here
    minVal = minimum numList
    maxVal = maximum numList
    newVals = map (convertToSymbolic (minVal, cutPt, maxVal)) numList

-- find all cut points for a column columnName -> [cutpoints]
findCutPoints :: DataFrame -> String -> [Float]
findCutPoints df colName = cutPoints
  where
    col = findAttributeValues df colName
    numList :: [Float]
    numList = nub $ sort $ flatMap (readMaybe) col
    xs = filter (\xs -> length xs >= 2 ) $ windows 2 numList
    cutPoints = map (\x -> sum x / genericLength x) xs

-- find all the values for that column
findAttributeValues :: DataFrame -> String -> [String]
findAttributeValues df cn =
    case idxM of
      Just idx -> map (head . (drop idx) . (fst. snd)) (Map.toList (get3 df))
      Nothing -> replicate (Map.size (get3 df)) "error" -- this shouldn't happen
    where
      idxM = elemIndex cn (get1 df)

-- adds a column to the dataset
-- assumes that the size of new column confirms with the size of existing columns
addColumnToDF :: DataFrame -> (String, [String]) -> DataFrame
addColumnToDF df (columnName, attrs) = (new_meta, get2 df, new_rows)
  where
    new_meta = columnName : (get1 df)
    new_rows = Map.fromList $ map (uncurry addInFront) (zip attrs old_rows)
    old_rows =  Map.toList $ get3 df

addInFront :: String -> (Integer, ([String], String)) -> (Integer, ([String], String))
addInFront na (i, (attrs, des)) = (i, (na:attrs, des))

isNumber :: String -> Bool
isNumber ""  = False
isNumber "." = False
isNumber xs  =
  case dropWhile isDigit xs of
    ""       -> True
    ('.':ys) -> all isDigit ys
    _        -> False
