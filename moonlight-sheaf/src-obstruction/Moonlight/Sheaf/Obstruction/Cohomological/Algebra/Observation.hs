{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Cohomology observations: first-class projection verbs over a
-- 'CoverCohomologyReport' — the sheaf-side analog of
-- "Moonlight.EGraph.Pure.Saturation.Logic.RunObservation". Each verb names a
-- /defining quantity/ of the descent cohomology (the H¹ gluing obstructions,
-- their supports, and the local C1 conflicts) so boundary specs assert them
-- declaratively instead of reaching into report fields by hand.
--
-- These are total, pure projections of an already-built report — no flow, no
-- carrier, no failure mode.
module Moonlight.Sheaf.Obstruction.Cohomological.Algebra.Observation
  ( CohomologyObservation (..),
    runCohomologyObservation,
  )
where

import Data.Kind (Type)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Sheaf.Obstruction.Cohomological.Types
  ( AnalysisCompleteness,
    C1LocalConflict,
    CoverCohomologyReport (..),
    H1Class (..),
  )

-- | A projection of a completed 'CoverCohomologyReport', indexed by its result
-- type.
type CohomologyObservation :: Type -> Type -> Type -> Type -> Type
data CohomologyObservation ctx obstruction witness result where
  -- | Supports of the H¹ gluing obstructions — the context set each non-exact
  -- fundamental cycle is carried on.
  ObserveH1Supports :: CohomologyObservation ctx obstruction witness (Set (Set ctx))
  -- | The H¹ gluing obstructions themselves (one per non-exact fundamental
  -- cycle).
  ObserveH1Obstructions :: CohomologyObservation ctx obstruction witness [H1Class ctx witness]
  -- | The local C1 conflicts — descent failures already visible on a single
  -- context or pairwise overlap (pre-cohomological, distinct from a global
  -- gluing obstruction).
  ObserveLocalC1Conflicts :: CohomologyObservation ctx obstruction witness [C1LocalConflict ctx obstruction witness]
  ObserveCompleteness :: CohomologyObservation ctx obstruction witness AnalysisCompleteness

runCohomologyObservation ::
  Ord ctx =>
  CohomologyObservation ctx obstruction witness result ->
  CoverCohomologyReport ctx report obstruction witness ->
  result
runCohomologyObservation observation report =
  case observation of
    ObserveH1Supports ->
      Set.fromList (fmap h1cSupport (corH1Obstructions report))
    ObserveH1Obstructions ->
      corH1Obstructions report
    ObserveLocalC1Conflicts ->
      corLocalC1Conflicts report
    ObserveCompleteness ->
      corCompleteness report
