module Model.DataSet (DataFrame
                     , Rows
                     , Column (SC, NC)
                     , getAStar
                     , getDStar
                     , get1, get2, get3
                     , ColumnType (Attribute, Decision)
                     , getAttributes
                     , stripColumn
                     , showDF
                     ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Tuple (swap)
import Control.Arrow (second)

import Data.List (elemIndex)

get1:: ([String], String, Rows) -> [String]
get1 (a, _, _) = a

get2:: ([String], String, Rows) -> String
get2 (_, b, _) = b

get3:: ([String], String, Rows) -> Rows
get3 (_, _, c) = c



-- DataFrame is where all the data is stored
-- It is a tuple of metadata and rows
-- metadata is a list of column (attribute, decision) names
-- Rows is a map of index and the values of attributes
-- Index specifies the row number, the list of
-- strings specifies attribute values

transMap :: Ord a => Map Integer a -> Map a (Set Integer)
transMap = Map.fromListWith Set.union . map (second Set.singleton . swap) . Map.toList

data Column = SC String [(Integer, String)]
            | NC String [(Integer, Float)]
              deriving (Show, Eq)

type Rows = Map Integer ([String], String)
type DataFrame = ([String], String, Rows)
data ColumnType = Attribute | Decision
                  deriving (Show, Eq)

--------------------------------------------------------------------------

-- given a column name, takes it out of the dataframe
stripColumn :: String -> DataFrame -> DataFrame
stripColumn col df = (nMeta, des, nRows)
  where
    nMeta = filter (/= col) (get1 df)
    des = get2 df
    nRows = case (elemIndex col (get1 df)) of
      Just i -> rowMod (toInteger i) (get3 df)
      Nothing -> get3 df

rowMod :: Integer -> Rows -> Rows
rowMod i rs = Map.map (\(attrs, des) -> (deleteN i attrs, des)) rs

deleteN :: Integer -> [a] -> [a]
deleteN _ []     = []
deleteN i (a:as)
   | i == 0    = as
        | otherwise = a : deleteN (i-1) as

showDF :: DataFrame -> String
showDF df = show (get1 df) ++ " -> " ++ show (get2 df) ++ "\n"
  ++ showRows (get3 df)

showRows :: Rows -> String
showRows rs = unlines $ map show (Map.toList rs)

--------------------------------------------------------------------------

getAttributes :: DataFrame -> Map Integer [String]
getAttributes df = Map.map (fst) rows
  where rows :: Rows
        rows = get3 df

getAttributeStarHelper :: DataFrame -> Map [String] (Set Integer)
getAttributeStarHelper =  transMap . getAttributes

getAStar :: DataFrame -> Set (Set Integer)
getAStar df = Set.fromList $ map snd (Map.assocs $ getAttributeStarHelper df)
--------------------------------------------------------------------------

getDecisions :: DataFrame -> Map Integer String
getDecisions df = Map.map (snd) rows
  where rows :: Rows
        rows = get3 df

getDecisionStarHelper :: DataFrame -> Map String (Set Integer)
getDecisionStarHelper = transMap . getDecisions

getDStar :: DataFrame -> Set (Set Integer)
getDStar df = Set.fromList $ map snd (Map.assocs $ getDecisionStarHelper df)
