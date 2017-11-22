module LEM1.RoughSet (leq
                     , (<=*)
                     , upper_approx
                     , lower_approx)  where

-- All the functions related to rough sets
import Data.Set (Set)
import qualified Data.Set as Set

-- Calculates if a set is less than or equal to another set
leq :: Set (Set Integer) -> Set (Set Integer) -> Bool
leq ast dst =  foldl (&&) True $ map ((flip leq_helper) dst) (Set.toList ast)

(<=*) :: Set (Set Integer) -> Set (Set Integer) -> Bool
(<=*) = leq

leq_helper :: Set Integer -> Set (Set Integer) -> Bool
leq_helper s dst = foldl (||) False $ map (Set.isSubsetOf s) (Set.toList dst)

subset_finder :: Integer -> Set (Set Integer) -> Set Integer
subset_finder e ss = head $ filter (Set.member e) (Set.toList ss)

-- Calculates the upper approximation of the set wrt to an equivalence relation
upper_approx :: Set Integer -> Set (Set Integer) -> Set Integer
upper_approx ss eq_rel = foldl (Set.union) (Set.empty)
  $ map ((flip subset_finder) eq_rel) (Set.toList ss)

-- Calculates the lower approximation of a set wrt an equivalence relation
lower_approx :: Set Integer -> Set (Set Integer) -> Set Integer
lower_approx ss eq_rel = foldl (Set.union) (Set.empty)
  $ filter ((flip Set.isSubsetOf) ss) (Set.toList eq_rel)
