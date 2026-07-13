{-# LANGUAGE DerivingStrategies #-}

-- | Projection\/restriction run outcomes and the per-iteration section report.
module Moonlight.Pale.Diagnostic.Section.Propagation
  ( ProjectionRunOutcome (..),
    foldProjectionOutcome,
    RestrictionRunOutcome (..),
    RestrictionOutcomeStat (..),
    IterationReport (..),
  )
where

import Data.Kind (Type)
import Data.Set (Set)
import Prelude (Double, Eq, Int, Show, String)

type ProjectionRunOutcome :: Type -> Type -> Type -> Type -> Type -> Type
data ProjectionRunOutcome cell key outcome failure diagnostic
  = ProjectionApplied key (Set cell) outcome Double [diagnostic]
  | ProjectionSkipped key String
  | ProjectionFailed key failure
  deriving stock (Eq, Show)

foldProjectionOutcome ::
  (key -> Set cell -> outcome -> Double -> [diagnostic] -> r) ->
  (key -> String -> r) ->
  (key -> failure -> r) ->
  ProjectionRunOutcome cell key outcome failure diagnostic ->
  r
foldProjectionOutcome applied skipped failed outcome =
  case outcome of
    ProjectionApplied key changedCells result residual diagnostics ->
      applied key changedCells result residual diagnostics
    ProjectionSkipped key reason ->
      skipped key reason
    ProjectionFailed key failure ->
      failed key failure

type RestrictionRunOutcome :: Type -> Type -> Type
data RestrictionRunOutcome cell mismatch
  = RestrictionMismatch cell cell [mismatch]
  deriving stock (Eq, Show)

type RestrictionOutcomeStat :: Type -> Type -> Type
data RestrictionOutcomeStat cell mismatch = RestrictionOutcomeStat
  { rosSourceCell :: cell,
    rosTargetCell :: cell,
    rosMismatch :: mismatch,
    rosOccurrences :: Int
  }
  deriving stock (Eq, Show)

type IterationReport :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data IterationReport cell mismatch key outcome failure diagnostic = IterationReport
  { irIterationIndex :: Int,
    irFrontierSize :: Int,
    irChangedCells :: Set cell,
    irResidualEnergy :: Double,
    irProjectionOutcomes :: [ProjectionRunOutcome cell key outcome failure diagnostic],
    irRestrictionOutcomes :: [RestrictionRunOutcome cell mismatch],
    irRestrictionOutcomeStats :: [RestrictionOutcomeStat cell mismatch]
  }
  deriving stock (Eq, Show)
