module LEM1.RoughSet (leq
                     , (<=*)
                     , upper_approx
                     , lower_approx)  where

-- All the functions related to rough sets
import Data.Set (Set)
import qualified Data.Set as Set

-- Calculates if a set is less than or equal to another set
leq :: Set (Set Int) -> Set (Set Int) -> Bool
leq ast dst =  foldl (&&) True $ map ((flip leq_helper) ast) (Set.toList dst)

(<=*) :: Set (Set Int) -> Set (Set Int) -> Bool
(<=*) = leq

leq_helper :: Set Int -> Set (Set Int) -> Bool
leq_helper s ast = foldl (||) False $ map (Set.isSubsetOf s) (Set.toList ast)

subset_finder :: Int -> Set (Set Int) -> Set Int
subset_finder e ss = head $ filter (Set.member e) (Set.toList ss)
  
-- Calculates the upper approximation of the set wrt to an equivalence relation
upper_approx :: Set Int -> Set (Set Int) -> Set Int
upper_approx ss eq_rel = foldl (Set.union) (Set.empty)
  $ map ((flip subset_finder) eq_rel) (Set.toList ss)

-- Calculates the lower approximtion of a set wrt an equivalence relation
lower_approx :: Set Int -> Set (Set Int) -> Set Int
lower_approx ss eq_rel = foldl (Set.union) (Set.empty)
  $ filter ((flip Set.isSubsetOf) ss) (Set.toList eq_rel)
