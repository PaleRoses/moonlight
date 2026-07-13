module Moonlight.Constraint.Pure.Recursion
  ( ConstraintExprF (..),
    projectConstraintExpr,
    embedConstraintExpr,
    cataConstraintExpr,
    paraConstraintExpr,
  )
where

import Moonlight.Constraint.Pure.Types (ConstraintExpr, ConstraintExprF (..))
import Data.Functor.Foldable (Corecursive (embed), Recursive (project))
import Data.Functor.Foldable (cata, para)

projectConstraintExpr :: ConstraintExpr a -> ConstraintExprF a (ConstraintExpr a)
projectConstraintExpr = project

embedConstraintExpr :: ConstraintExprF a (ConstraintExpr a) -> ConstraintExpr a
embedConstraintExpr = embed

cataConstraintExpr :: (ConstraintExprF a b -> b) -> ConstraintExpr a -> b
cataConstraintExpr = cata

paraConstraintExpr :: (ConstraintExprF a (ConstraintExpr a, b) -> b) -> ConstraintExpr a -> b
paraConstraintExpr = para
