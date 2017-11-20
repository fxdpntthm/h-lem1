module Model.DataSet (DataFrame
                     , Rows
                     , Column (SC, NC)
                     , getAttributes
                     , getAttributeStarHelper
                     , getAStar
                     ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Tuple (swap)
import Control.Arrow (second)

transMap = Map.fromListWith Set.union . map (second Set.singleton . swap) . Map.toList

-- DataFrame is where all the data is stored
-- It is a tuple of metadata and rows
-- metadata is a list of column (attribute, decision) names
-- Rows is a map of index and the values of attributes
-- Index specifies the row number, the list of
-- strings specifies attribute values

get1:: ([String], String, Rows) -> [String]
get1 (a, _, _) = a

get2:: ([String], String, Rows) -> String
get2 (_, b, _) = b

get3:: ([String], String, Rows) -> Rows
get3 (_, _, c) = c

data Column = SC String [(Integer, String)]
            | NC String [(Integer, Float)]
              deriving (Show, Eq)

type Rows = Map Integer ([String], String)
type DataFrame = ([String], String, Rows)

getAttributes :: DataFrame -> Map Integer [String]
getAttributes df = Map.map (fst) rows
  where rows :: Rows
        rows = get3 df

getAttributeStarHelper :: DataFrame -> Map [String] (Set Integer)
getAttributeStarHelper =  transMap . getAttributes

getAStar :: DataFrame -> Set (Set Integer)
getAStar df = Set.fromList $ map snd as
  where
    ash :: Map [String] (Set Integer)
    ash = getAttributeStarHelper df
    as :: [([String], Set Integer)]
    as = Map.assocs ash
