{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Moonlight.Flow.Bench.Runtime.RbacEntitlement.Types
  ( RbacContext (..),
    RbacProp (..),
    RbacAtomName (..),
    RbacAtoms (..),
    RbacTruth (..),
    RbacRelationPatchSummary (..),
    RbacPatchSummary (..),
    RbacRowsDigest (..),
    RbacPlans (..),
    RbacModel (..),
    RbacFixtureError (..),
    RbacResourceScopeReproducerPlanSet (..),
    RbacResourceScopeReproducerCase (..),
    Rng (..),
    RbacWorkloadConfig (..),
    RbacSize (..),
    RbacSeedCounts (..),
    RbacPatchShape (..),
    RbacLocalityScenario (..),
    RbacLocalityMatrixReport (..),
    RbacLocalityScenarioReport (..),
    RbacTargetedScenario (..),
    RbacTargetedTimingReport (..),
    RbacTargetedScenarioReport (..),
    RbacRunSummary (..),
    RbacBatchReport (..),
    RbacBenchError (..),
    RbacResourceScopeReproducerReport (..),
    RbacResourceScopeReproducerCaseReport (..),
    RbacResourceScopeReproducerOutcome (..),
    RbacSnapshot (..),
    RbacSnapshotDigest (..),
    RbacAdversarialReport (..),
    RbacRuntimeStats (..),
    RbacRuntimeStatsSample (..),
    RbacTruthProbe (..),
    RbacLoopState (..),
    RbacVisibleRead (..),
    fromRbacFixture,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Word
  ( Word64,
  )
import Moonlight.Flow.Patch qualified as R
import Moonlight.Flow.Read qualified as R
import Moonlight.Flow.Runtime.Types qualified as R
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )
import Moonlight.Flow.Runtime.RbacFixture.Types
  ( RbacAtomName (..),
    RbacAtoms (..),
    RbacContext (..),
    RbacFixtureError (..),
    RbacLocalityScenario (..),
    RbacModel (..),
    RbacPatchShape (..),
    RbacPatchSummary (..),
    RbacPlans (..),
    RbacProp (..),
    RbacRelationPatchSummary (..),
    RbacResourceScopeReproducerCase (..),
    RbacResourceScopeReproducerPlanSet (..),
    RbacRowsDigest (..),
    RbacSeedCounts (..),
    RbacSize (..),
    RbacTruth (..),
    Rng (..),
  )

data RbacTargetedScenario
  = RbacTargetMemberOnly
  | RbacTargetUserAttrOnly
  | RbacTargetRoleActionOnly
  | RbacTargetResourceScopeOnly
  | RbacTargetDenyOnly
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

data RbacWorkloadConfig = RbacWorkloadConfig
  { rwcPatchSeed :: !Word64,
    rwcSize :: !RbacSize,
    rwcSeedCounts :: !RbacSeedCounts,
    rwcPatchShape :: !RbacPatchShape,
    rwcBatches :: !Int,
    rwcFreshCheckEvery :: !Int,
    rwcAdversarialEvery :: !Int,
    rwcSemanticCheckEvery :: !Int,
    rwcReadInitialOutputs :: !Bool,
    rwcReadFinalOutputs :: !Bool
  }
  deriving stock (Eq, Show, Read)

data RbacSnapshot = RbacSnapshot
  { rsGrant :: !R.Rows,
    rsConditionalGrant :: !R.Rows,
    rsDenied :: !R.Rows,
    rsGrantUserAction :: !R.Rows,
    rsGrantResourceSubject :: !R.Rows,
    rsGrantScopeAction :: !R.Rows
  }
  deriving stock (Eq, Show)

data RbacSnapshotDigest = RbacSnapshotDigest
  { rsdGrant :: !RbacRowsDigest,
    rsdConditionalGrant :: !RbacRowsDigest,
    rsdDenied :: !RbacRowsDigest,
    rsdGrantUserAction :: !RbacRowsDigest,
    rsdGrantResourceSubject :: !RbacRowsDigest,
    rsdGrantScopeAction :: !RbacRowsDigest,
    rsdEffectiveCount :: !Int
  }
  deriving stock (Eq, Ord, Show, Read)

data RbacAdversarialReport = RbacAdversarialReport
  { rarCancellationPreservedDigest :: !Bool,
    rarInvalidDeleteRejected :: !Bool
  }
  deriving stock (Eq, Ord, Show, Read)

data RbacRuntimeStats = RbacRuntimeStats
  { rrsAllocatedBytesDelta :: !Word64,
    rrsMaxLiveBytes :: !Word64,
    rrsGcCpuNsDelta :: !Word64
  }
  deriving stock (Eq, Ord, Show, Read)

data RbacTargetedTimingReport = RbacTargetedTimingReport
  { rttrConfig :: !RbacWorkloadConfig,
    rttrWarmupPatch :: !RbacPatchSummary,
    rttrWarmupApplyNs :: !Word64,
    rttrWarmupRepairStats :: !R.RuntimeRepairStats,
    rttrWarmupDiagnostics :: !R.RuntimeReuseDiagnostics,
    rttrScenarios :: ![RbacTargetedScenarioReport]
  }
  deriving stock (Show)

data RbacTargetedScenarioReport = RbacTargetedScenarioReport
  { rtsrScenario :: !RbacTargetedScenario,
    rtsrPatchShape :: !RbacPatchShape,
    rtsrPatch :: !RbacPatchSummary,
    rtsrApplyNs :: !Word64,
    rtsrRepairStats :: !R.RuntimeRepairStats,
    rtsrRuntimeStats :: !(Maybe RbacRuntimeStats),
    rtsrReuseStats :: !R.RuntimeReuseStats,
    rtsrReuseDiagnosticsBefore :: !R.RuntimeReuseDiagnostics,
    rtsrReuseDiagnosticsAfter :: !R.RuntimeReuseDiagnostics,
    rtsrStaleRejectedDelta :: !Int,
    rtsrRegisteredNewDelta :: !Int
  }
  deriving stock (Show)

data RbacRuntimeStatsSample = RbacRuntimeStatsSample
  { rrssAllocatedBytes :: !Word64,
    rrssMaxLiveBytes :: !Word64,
    rrssGcCpuNs :: !Word64
  }
  deriving stock (Eq, Ord, Show, Read)

data RbacTruthProbe = RbacTruthProbe
  { rtpTotalRows :: !Int,
    rtpRelationRows :: !(Map RbacAtomName Int),
    rtpChecksum :: !Word64
  }
  deriving stock (Eq, Ord, Show, Read)

data RbacBatchReport = RbacBatchReport
  { rbrBatch :: !Int,
    rbrPatch :: !RbacPatchSummary,
    rbrApplyNs :: !Word64,
    rbrReadNs :: !(Maybe Word64),
    rbrFreshCheckNs :: !(Maybe Word64),
    rbrSemanticCheckNs :: !(Maybe Word64),
    rbrDigest :: !(Maybe RbacSnapshotDigest),
    rbrFreshMatched :: !(Maybe Bool),
    rbrAdversarial :: !(Maybe RbacAdversarialReport),
    rbrReuseStats :: !R.RuntimeReuseStats,
    rbrReuseDiagnostics :: !R.RuntimeReuseDiagnostics,
    rbrRepairStats :: !R.RuntimeRepairStats,
    rbrRuntimeStats :: !(Maybe RbacRuntimeStats)
  }
  deriving stock (Show)

data RbacRunSummary = RbacRunSummary
  { rrsConfig :: !RbacWorkloadConfig,
    rrsInitialDigest :: !(Maybe RbacSnapshotDigest),
    rrsLastObservedDigest :: !(Maybe RbacSnapshotDigest),
    rrsReports :: ![RbacBatchReport],
    rrsFinalDigest :: !(Maybe RbacSnapshotDigest)
  }
  deriving stock (Show)

data RbacLocalityMatrixReport = RbacLocalityMatrixReport
  { rlmrConfig :: !RbacWorkloadConfig,
    rlmrWarmupPatch :: !RbacPatchSummary,
    rlmrWarmupApplyNs :: !Word64,
    rlmrWarmupDiagnostics :: !R.RuntimeReuseDiagnostics,
    rlmrScenarios :: ![RbacLocalityScenarioReport]
  }
  deriving stock (Show)

data RbacLocalityScenarioReport = RbacLocalityScenarioReport
  { rlsrScenario :: !RbacLocalityScenario,
    rlsrPatchShape :: !RbacPatchShape,
    rlsrPatch :: !RbacPatchSummary,
    rlsrApplyNs :: !Word64,
    rlsrFreshCheckNs :: !Word64,
    rlsrRegisteredFactorShapesBefore :: !Int,
    rlsrRegisteredFactorShapesAfter :: !Int,
    rlsrStaleRejectedDelta :: !Int,
    rlsrRegisteredNewDelta :: !Int,
    rlsrDiagnosticsBefore :: !R.RuntimeReuseDiagnostics,
    rlsrDiagnosticsAfter :: !R.RuntimeReuseDiagnostics
  }
  deriving stock (Show)

data RbacBenchError
  = RbacFixtureSupportError !RbacFixtureError
  | RbacPatchError !R.PatchError
  | RbacCreateError !(R.RuntimeCreateError RbacContext RbacProp)
  | RbacApplyError !(R.RuntimeApplyError RbacContext RbacProp)
  | RbacReadError !(R.ReadError RbacContext RbacProp)
  | RbacFreshMismatch !Int !RbacSnapshotDigest !RbacSnapshotDigest
  | RbacProjectionMismatch !String
  | RbacCancellationChangedOutput !Int !RbacSnapshotDigest !RbacSnapshotDigest
  | RbacInvalidDeleteAccepted !Int
  | RbacVisibleReadRequired !Int !String
  | RbacLocalityWarmupFailed !String !R.RuntimeReuseDiagnostics
  | RbacConditionalReferenceMismatch !Int !RbacRowsDigest !RbacRowsDigest
  deriving stock (Show)

data RbacResourceScopeReproducerReport = RbacResourceScopeReproducerReport
  { rrsrConfig :: !RbacWorkloadConfig,
    rrsrDeletedResourceScopeRows :: ![RowTupleKey],
    rrsrInsertedResourceScopeRows :: ![RowTupleKey],
    rrsrPatch :: !RbacRelationPatchSummary,
    rrsrCases :: ![RbacResourceScopeReproducerCaseReport]
  }
  deriving stock (Show)

data RbacResourceScopeReproducerCaseReport = RbacResourceScopeReproducerCaseReport
  { rrscrPlanSet :: !RbacResourceScopeReproducerPlanSet,
    rrscrSeedAtoms :: ![RbacAtomName],
    rrscrOutcome :: !RbacResourceScopeReproducerOutcome
  }
  deriving stock (Show)

data RbacResourceScopeReproducerOutcome
  = RbacResourceScopeReproducerApplied !R.RuntimeReuseStats
  | RbacResourceScopeReproducerRejected !(R.RuntimeApplyError RbacContext RbacProp)
  deriving stock (Show)

data RbacLoopState = RbacLoopState
  { rlsTruth :: !RbacTruth,
    rlsRuntime :: !(R.Runtime RbacContext RbacProp),
    rlsRng :: !Rng,
    rlsInitialDigest :: !(Maybe RbacSnapshotDigest),
    rlsLastDigest :: !(Maybe RbacSnapshotDigest),
    rlsRuntimeStatsSample :: !(Maybe RbacRuntimeStatsSample),
    rlsReportsReversed :: ![RbacBatchReport]
  }

data RbacVisibleRead = RbacVisibleRead
  { rvrReadNs :: !Word64,
    rvrSnapshot :: !RbacSnapshot,
    rvrDigest :: !RbacSnapshotDigest
  }

fromRbacFixture :: Either RbacFixtureError value -> Either RbacBenchError value
fromRbacFixture =
  first RbacFixtureSupportError
{-# INLINE fromRbacFixture #-}
