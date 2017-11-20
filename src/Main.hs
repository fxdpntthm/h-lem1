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
                   , RuleSet
                   )

import Model.DataSet (DataFrame
                     , Rows
                     , Column (SC, NC)
                     , getAttributes
                     , getAttributeStarHelper
                     , getAStar
                     )

r_star = Set.fromList [ Set.fromList [1]
                      , Set.fromList [2,3]
                      , Set.fromList [4,5]
                      , Set.fromList [6,7]
                      , Set.fromList [8]
                      ]

x = Set.fromList [1, 2, 3, 5, 7]

a_star :: Set (Set Int)
a_star = Set.fromList [ Set.fromList[1,2,3]
                      , Set.fromList[4]
                      , Set.fromList[5,6] ]

d_star :: Set (Set Int)
d_star = Set.fromList [ Set.fromList[1]
                      , Set.fromList[2]
                      , Set.fromList[3]
                      , Set.fromList[4]
                      , Set.fromList[5,6] ]

d_star' :: Set (Set Int)
d_star' = Set.fromList [ Set.fromList[1]
                      , Set.fromList[2]
                      , Set.fromList[3, 4]
                      , Set.fromList[5,6] ]

rs :: Rows
rs = Map.fromList [ (0, (["0.8", "0.3", "7.2"], "very-small"))
                  , (1, (["0.8", "1.1", "7.2"], "small"))
                  , (2, (["0.8", "1.1", "10.2"], "medium"))
                  , (3, (["1.2", "0.3", "10.2"], "medium"))
                  , (4, (["1.2", "2.3", "10.2"], "medium"))
                  , (5, (["2.0", "2.3", "10.2"], "high"))
                  , (6, (["2.0", "2.3", "15.2"], "very-high"))
                  ]

clms :: [Column]
clms = [ NC "A" [ (0, 0.8)
                , (1, 0.8)
                , (2, 0.8)
                , (3, 1.2)
                , (4, 1.2)
                , (5, 2.0)
                , (6, 2.0) ] 
       , NC "B" [ (0, 0.3)
                , (1, 1.1)
                , (2, 1.1)
                , (3, 0.3)
                , (4, 2.3)
                , (5, 2.3)
                , (6, 2.3) ]
       , NC "C" [ (0, 7.2)
                , (1, 7.2)
                , (2, 10.2)
                , (3, 10.2)
                , (4, 10.2)
                , (5, 10.2)
                , (6, 15.2) ]
       , SC "D" [ (0, "very-small")
                , (1, "small")
                , (2, "medium")
                , (3, "medium")
                , (4, "medium")
                , (5, "high")
                , (6, "very-high")
                ]
       ]

d :: DataFrame
d = (["A", "B", "C"], "D", rs)


main :: IO ()
main = do
  print "Hello World!"
  -- print a_star
  -- print d_star
  -- print d_star'
  -- print $ (Set.elemAt 2 d_star) `Set.isSubsetOf` (Set.elemAt 1 a_star)
  -- print $ map (Set.isSubsetOf (Set.elemAt 0 d_star)) (Set.toList a_star)
  --  print $ map (((flip Set.isSubsetOf) (Set.toList a_star))) (Set.toList d_star)
  -- print $ leq a_star d_star
  -- print $ a_star <=* d_star'
  -- print $ upper_approx x r_star
  -- print $ lower_approx x r_star
  -- print d
  -- print clms
  print $ getAStar d
