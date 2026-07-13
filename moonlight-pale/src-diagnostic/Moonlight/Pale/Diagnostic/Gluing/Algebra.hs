-- | Folding run outcomes and propagation reports into hotspot statistics.
module Moonlight.Pale.Diagnostic.Gluing.Algebra
  ( projectionOutcomeChangedCells,
    projectionOutcomeResidual,
    projectionOutcomeDiagnostics,
    outcomeSummaryFromProjectionOutcome,
    outcomeSummaryFromRestrictionOutcome,
    outcomeSummaryChangedCells,
    outcomeSummaryResidual,
    summarizeRestrictionOutcomes,
    statsByMismatch,
    statsByCell,
    topRestrictionHotspots,
    filterReportDiagnostics,
    reportTotalMismatches,
  )
where

import Data.Function ((&))
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Pale.Diagnostic.Gluing.Propagation
  ( OutcomeSummary (..),
    PropagationReport (..),
  )
import Moonlight.Pale.Diagnostic.Section.Propagation
  ( ProjectionRunOutcome (..),
    RestrictionOutcomeStat (..),
    RestrictionRunOutcome (..),
    foldProjectionOutcome,
  )
import Prelude
  ( Bool,
    Double,
    Eq ((==)),
    Int,
    Maybe (Just, Nothing),
    Ord,
    filter,
    fmap,
    foldMap,
    max,
    maybe,
    pure,
    sum,
    take,
    (+),
    (.),
    (>>=),
  )

projectionOutcomeChangedCells :: ProjectionRunOutcome cell key outcome failure diagnostic -> Set cell
projectionOutcomeChangedCells =
  foldProjectionOutcome (\_ cells _ _ _ -> cells) (\_ _ -> Set.empty) (\_ _ -> Set.empty)

projectionOutcomeResidual :: ProjectionRunOutcome cell key outcome failure diagnostic -> Maybe Double
projectionOutcomeResidual =
  foldProjectionOutcome (\_ _ _ residual _ -> Just residual) (\_ _ -> Nothing) (\_ _ -> Nothing)

projectionOutcomeDiagnostics :: ProjectionRunOutcome cell key outcome failure diagnostic -> [diagnostic]
projectionOutcomeDiagnostics =
  foldProjectionOutcome (\_ _ _ _ diagnostics -> diagnostics) (\_ _ -> []) (\_ _ -> [])

outcomeSummaryFromProjectionOutcome :: ProjectionRunOutcome cell key outcome failure diagnostic -> OutcomeSummary cell mismatch key outcome failure diagnostic
outcomeSummaryFromProjectionOutcome outcome =
  OutcomeSummary
    { outcomeSummaryDiagnostics = projectionOutcomeDiagnostics outcome,
      outcomeSummaryProjectionOutcomes = [outcome],
      outcomeSummaryRestrictionOutcomes = []
    }

outcomeSummaryFromRestrictionOutcome :: RestrictionRunOutcome cell mismatch -> OutcomeSummary cell mismatch key outcome failure diagnostic
outcomeSummaryFromRestrictionOutcome outcome =
  OutcomeSummary
    { outcomeSummaryDiagnostics = [],
      outcomeSummaryProjectionOutcomes = [],
      outcomeSummaryRestrictionOutcomes = [outcome]
    }

outcomeSummaryChangedCells :: Ord cell => OutcomeSummary cell mismatch key outcome failure diagnostic -> Set cell
outcomeSummaryChangedCells summary =
  outcomeSummaryProjectionOutcomes summary
    & foldMap projectionOutcomeChangedCells

outcomeSummaryResidual :: OutcomeSummary cell mismatch key outcome failure diagnostic -> Double
outcomeSummaryResidual summary =
  outcomeSummaryProjectionOutcomes summary
    & foldMap (maybe [] pure . projectionOutcomeResidual)
    & sum

summarizeRestrictionOutcomes :: (Ord cell, Ord mismatch) => [RestrictionRunOutcome cell mismatch] -> [RestrictionOutcomeStat cell mismatch]
summarizeRestrictionOutcomes outcomes =
  outcomes
    & foldMap restrictionOutcomeAtoms
    & Map.fromListWith (+)
    & Map.toList
    & fmap
      ( \((sourceCell, targetCell, mismatch), occurrences) ->
          RestrictionOutcomeStat
            { rosSourceCell = sourceCell,
              rosTargetCell = targetCell,
              rosMismatch = mismatch,
              rosOccurrences = occurrences
            }
      )

statsByMismatch :: Ord mismatch => [RestrictionOutcomeStat cell mismatch] -> Map mismatch Int
statsByMismatch stats =
  stats
    & fmap (\stat -> (rosMismatch stat, rosOccurrences stat))
    & Map.fromListWith (+)

statsByCell :: Ord cell => [RestrictionOutcomeStat cell mismatch] -> Map cell Int
statsByCell stats =
  stats
    >>= statCells
    & Map.fromListWith (+)
  where
    statCells :: Eq cell => RestrictionOutcomeStat cell mismatch -> [(cell, Int)]
    statCells stat =
      if rosSourceCell stat == rosTargetCell stat
        then [(rosSourceCell stat, rosOccurrences stat)]
        else
          [ (rosSourceCell stat, rosOccurrences stat),
            (rosTargetCell stat, rosOccurrences stat)
          ]

topRestrictionHotspots :: Int -> [RestrictionOutcomeStat cell mismatch] -> [RestrictionOutcomeStat cell mismatch]
topRestrictionHotspots limitValue stats =
  stats
    & sortOn (Down . rosOccurrences)
    & take (max 0 limitValue)

filterReportDiagnostics ::
  (diagnostic -> Bool) ->
  PropagationReport cell mismatch key outcome failure diagnostic ->
  PropagationReport cell mismatch key outcome failure diagnostic
filterReportDiagnostics predicate report =
  report {prDiagnostics = filter predicate (prDiagnostics report)}

reportTotalMismatches :: PropagationReport cell mismatch key outcome failure diagnostic -> Int
reportTotalMismatches report =
  prRestrictionOutcomeStats report
    & fmap rosOccurrences
    & sum

restrictionOutcomeAtoms :: RestrictionRunOutcome cell mismatch -> [((cell, cell, mismatch), Int)]
restrictionOutcomeAtoms outcome =
  case outcome of
    RestrictionMismatch sourceCell targetCell mismatches ->
      mismatches
        & fmap (\mismatch -> ((sourceCell, targetCell, mismatch), 1))
