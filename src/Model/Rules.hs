module Model.Rules ( Rule
                   , RuleSet
                   ) where

import Data.Set (Set)
import qualified Data.Set as Set

type Rule = ([(String, String)], (String, String))
type RuleSet = Set Rule


