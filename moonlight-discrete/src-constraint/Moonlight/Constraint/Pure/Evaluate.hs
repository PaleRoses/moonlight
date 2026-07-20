module Moonlight.Constraint.Pure.Evaluate
  ( evaluate,
    satisfiable,
    unsatisfiable,
    tautology,
    implies,
    equivalent,
    atoms,
  )
where

import qualified Data.Set as Set
import Moonlight.Constraint.Pure.CNF (toCNF)
import Moonlight.Constraint.Pure.DPLL (dpll)
import Moonlight.Constraint.Pure.Recursion
  ( ConstraintExprF (..),
    cataConstraintExpr,
  )
import Moonlight.Constraint.Pure.Types (ConstraintExpr (..))

evaluate :: (a -> Bool) -> ConstraintExpr a -> Bool
evaluate resolver = cataConstraintExpr algebra
  where
    algebra expressionLayer =
      case expressionLayer of
        AtomF variable -> resolver variable
        AndF children -> all id children
        OrF children -> any id children
        NotF inner -> not inner

satisfiable :: Ord a => ConstraintExpr a -> Bool
satisfiable expression =
  case dpll (toCNF expression) of
    Just _ -> True
    Nothing -> False

unsatisfiable :: Ord a => ConstraintExpr a -> Bool
unsatisfiable = not . satisfiable

tautology :: Ord a => ConstraintExpr a -> Bool
tautology = unsatisfiable . Not

implies :: Ord a => ConstraintExpr a -> ConstraintExpr a -> Bool
implies premise conclusion =
  unsatisfiable (And [premise, Not conclusion])

equivalent :: Ord a => ConstraintExpr a -> ConstraintExpr a -> Bool
equivalent left right =
  implies left right && implies right left

atoms :: Ord a => ConstraintExpr a -> Set.Set a
atoms = cataConstraintExpr algebra
  where
    algebra :: Ord a => ConstraintExprF a (Set.Set a) -> Set.Set a
    algebra expressionLayer =
      case expressionLayer of
        AtomF variable -> Set.singleton variable
        AndF children -> foldMap id children
        OrF children -> foldMap id children
        NotF inner -> inner
