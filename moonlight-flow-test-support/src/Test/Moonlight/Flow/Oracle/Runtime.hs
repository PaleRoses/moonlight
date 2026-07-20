{-# LANGUAGE DerivingStrategies #-}

module Test.Moonlight.Flow.Oracle.Runtime
  ( RuntimeInvariant (..),
    runtimeInvariantName,
    migratedRuntimeInvariants,
  )
where

-- A closed index of runtime invariants migrated out of fixture-local specs.
-- The executable witnesses live in Property.Runtime.* and the constructors in
-- Oracle.Runtime.*; this type prevents the migration from dissolving into prose.
data RuntimeInvariant
  = ExactByCoverCompleteCoverExact
  | ExactByCoverIncompleteRejects
  | ExactByCoverStaleTopologyRejects
  | ExactByCoverGeneratedSiteNoReuseEdges
  | ExactByCoverRuntimeReuseFromPlan
  | ExactByCoverDigestStableUnderRegisteredReuse
  | PlanReuseValidationCorruptionMatrix
  | FactorActionExactEquivalentUsesReuse
  | FactorReuseRejectionFallsBackToRepair
  | BranchSharingUnmodifiedCarrierSnapshotsShared
  | StructuralCutoffPredictionsHold
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

runtimeInvariantName :: RuntimeInvariant -> String
runtimeInvariantName invariant =
  case invariant of
    ExactByCoverCompleteCoverExact -> "complete cover emits exact target coverage"
    ExactByCoverIncompleteRejects -> "incomplete cover does not produce proof"
    ExactByCoverStaleTopologyRejects -> "stale topology invalidates proof and reuse"
    ExactByCoverGeneratedSiteNoReuseEdges -> "generated site structural graph has no reuse edges"
    ExactByCoverRuntimeReuseFromPlan -> "runtime graph derives reuse edges from plan reuse"
    ExactByCoverDigestStableUnderRegisteredReuse -> "registered reuse does not perturb cover-proof digest"
    PlanReuseValidationCorruptionMatrix -> "plan reuse validation rejects exact and lower-bound corruption"
    FactorActionExactEquivalentUsesReuse -> "exact-equivalent query uses reuse"
    FactorReuseRejectionFallsBackToRepair -> "exact-equivalent rejection falls back to exact repair"
    BranchSharingUnmodifiedCarrierSnapshotsShared -> "branch-local carrier deltas physically share unmodified current snapshots"
    StructuralCutoffPredictionsHold -> "per-edit structural cutoff predictions hold under sustained adversarial edits"

migratedRuntimeInvariants :: [RuntimeInvariant]
migratedRuntimeInvariants =
  [minBound .. maxBound]
