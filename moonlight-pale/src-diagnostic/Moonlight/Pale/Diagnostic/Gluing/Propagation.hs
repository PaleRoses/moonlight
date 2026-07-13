{-# LANGUAGE DerivingStrategies #-}

-- | Gluing sections into outcome summaries, propagation failures, and reports.
module Moonlight.Pale.Diagnostic.Gluing.Propagation
  ( OutcomeSummary (..),
    PropagationFailure (..),
    PropagationReport (..),
  )
where

import Data.Kind (Type)
import Data.Set (Set)
import Moonlight.Pale.Diagnostic.Section.Propagation
  ( IterationReport,
    ProjectionRunOutcome,
    RestrictionOutcomeStat,
    RestrictionRunOutcome,
  )
import Prelude (Bool, Double, Eq, Int, Monoid (mempty), Semigroup ((<>)), Show, String)

type OutcomeSummary :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data OutcomeSummary cell mismatch key outcome failure diagnostic = OutcomeSummary
  { outcomeSummaryDiagnostics :: [diagnostic],
    outcomeSummaryProjectionOutcomes :: [ProjectionRunOutcome cell key outcome failure diagnostic],
    outcomeSummaryRestrictionOutcomes :: [RestrictionRunOutcome cell mismatch]
  }
  deriving stock (Eq, Show)

instance Semigroup (OutcomeSummary cell mismatch key outcome failure diagnostic) where
  leftSummary <> rightSummary =
    OutcomeSummary
      { outcomeSummaryDiagnostics =
          outcomeSummaryDiagnostics leftSummary
            <> outcomeSummaryDiagnostics rightSummary,
        outcomeSummaryProjectionOutcomes =
          outcomeSummaryProjectionOutcomes leftSummary
            <> outcomeSummaryProjectionOutcomes rightSummary,
        outcomeSummaryRestrictionOutcomes =
          outcomeSummaryRestrictionOutcomes leftSummary
            <> outcomeSummaryRestrictionOutcomes rightSummary
      }

instance Monoid (OutcomeSummary cell mismatch key outcome failure diagnostic) where
  mempty =
    OutcomeSummary
      { outcomeSummaryDiagnostics = [],
        outcomeSummaryProjectionOutcomes = [],
        outcomeSummaryRestrictionOutcomes = []
      }

type PropagationFailure :: Type -> Type -> Type
data PropagationFailure key failure
  = PropagationIterationExceeded Int
  | PropagationInvariantViolation String
  | PropagationProjectionFailure key failure
  deriving stock (Eq, Show)

type PropagationReport :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data PropagationReport cell mismatch key outcome failure diagnostic = PropagationReport
  { prChangedCells :: Set cell,
    prIterationCount :: Int,
    prConverged :: Bool,
    prTotalCellsProcessed :: Int,
    prResidualEnergy :: Double,
    prDiagnostics :: [diagnostic],
    prProjectionOutcomes :: [ProjectionRunOutcome cell key outcome failure diagnostic],
    prRestrictionOutcomes :: [RestrictionRunOutcome cell mismatch],
    prRestrictionOutcomeStats :: [RestrictionOutcomeStat cell mismatch],
    prIterationReports :: [IterationReport cell mismatch key outcome failure diagnostic]
  }
  deriving stock (Eq, Show)
