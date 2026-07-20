{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Moonlight.Flow.Runtime.RbacDataflowFixture
  ( RbacDataflowPatchStage (..),
    RbacDataflowFixtureError (..),
    RbacDataflowLiveState,
    RuntimeDataflowSnapshot,
    RbacDataflowWorkloadCapture (..),
    initialRbacDataflowLiveState,
    initialRbacDataflowLiveStateWith,
    rbacDataflowWorkloadPatchShape,
    rbacDataflowWorkloadCapture,
    stepRbacDataflowLiveState,
    rbacDataflowLiveSnapshots,
    rbacDataflowSnapshot,
    runtimeDataflowSnapshotHex,
    writeRbacDataflowCBOR,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.ByteString.Lazy qualified as BSL
import Data.Word
  ( Word64,
    Word8,
  )
import Moonlight.Flow.Runtime.Apply qualified as R
import Moonlight.Flow.Runtime.Types qualified as R
import Moonlight.Flow.Runtime.Engine.Dataflow
  ( RuntimeDataflowSnapshot,
    encodeRuntimeDataflowCBOR,
    runtimeDataflowStepForPatch,
    runtimeDataflowSnapshotForPatch,
    writeRuntimeDataflowCBOR,
  )
import Numeric
  ( showHex,
  )
import Moonlight.Flow.Runtime.RbacFixture.Config
  ( testRbacSeedCounts,
    testRbacSize,
  )
import Moonlight.Flow.Runtime.RbacFixture.Patch
  ( generatePatchBatch,
  )
import Moonlight.Flow.Runtime.RbacFixture.Plans
  ( fullSoakPlans,
    rbacAtoms,
  )
import Moonlight.Flow.Runtime.RbacFixture.Truth
  ( buildRuntimeFromTruth,
    seedTruth,
  )
import Moonlight.Flow.Runtime.RbacFixture.Types
  ( RbacContext,
    RbacFixtureError,
    RbacPatchShape (..),
    RbacPatchSummary,
    RbacProp,
    RbacSize,
    RbacTruth,
    Rng,
  )

data RbacDataflowPatchStage
  = RbacDataflowWarmupPatch
  | RbacDataflowCapturePatch
  | RbacDataflowLivePatch !Int
  deriving stock (Eq, Ord, Show, Read)

data RbacDataflowFixtureError
  = RbacDataflowFixtureBuildFailed !RbacFixtureError
  | RbacDataflowPatchApplyFailed !RbacDataflowPatchStage !(R.RuntimeApplyError RbacContext RbacProp)
  deriving stock (Show)

data RbacDataflowWorkloadCapture = RbacDataflowWorkloadCapture
  { rdwcSnapshot :: !RuntimeDataflowSnapshot,
    rdwcWarmupSummary :: !RbacPatchSummary,
    rdwcCaptureSummary :: !RbacPatchSummary
  }
  deriving stock (Eq, Show, Read)

data RbacDataflowLiveState = RbacDataflowLiveState
  { rdlsSize :: !RbacSize,
    rdlsTruth :: !RbacTruth,
    rdlsRuntime :: !(R.Runtime RbacContext RbacProp),
    rdlsRng :: !Rng,
    rdlsPatchShape :: !RbacPatchShape,
    rdlsStepIndex :: !Int
  }

rbacDataflowWorkloadSeed :: Word64
rbacDataflowWorkloadSeed =
  0x5E1E51DA7A000001
{-# INLINE rbacDataflowWorkloadSeed #-}

rbacDataflowWorkloadPatchShape :: RbacPatchShape
rbacDataflowWorkloadPatchShape =
  RbacPatchShape
    { rpsMemberMoves = 18,
      rpsUserAttrMoves = 12,
      rpsResourceScopeMoves = 8,
      rpsRoleActionMoves = 4,
      rpsGroupRoleMoves = 5,
      rpsDenyMoves = 6,
      rpsGroupScopeMoves = 3
    }
{-# INLINE rbacDataflowWorkloadPatchShape #-}

rbacDataflowWorkloadCapture :: Either RbacDataflowFixtureError RbacDataflowWorkloadCapture
rbacDataflowWorkloadCapture = do
  let atomsValue = rbacAtoms
      sizeValue = testRbacSize
      (truth0, rng0) = seedTruth sizeValue testRbacSeedCounts rbacDataflowWorkloadSeed
  plansValue <- first RbacDataflowFixtureBuildFailed (fullSoakPlans atomsValue)
  runtime0 <- first RbacDataflowFixtureBuildFailed (buildRuntimeFromTruth atomsValue plansValue truth0)
  (truthWarm, runtimeWarm, rngWarm, warmupSummary) <-
    applyRbacDataflowPatch
      sizeValue
      RbacDataflowWarmupPatch
      rbacDataflowWorkloadPatchShape
      truth0
      runtime0
      rng0
  (_truthCapture, snapshot, _rngCapture, captureSummary) <-
    previewRbacDataflowPatch
      sizeValue
      RbacDataflowCapturePatch
      rbacDataflowWorkloadPatchShape
      truthWarm
      runtimeWarm
      rngWarm
  pure
    RbacDataflowWorkloadCapture
      { rdwcSnapshot = snapshot,
        rdwcWarmupSummary = warmupSummary,
        rdwcCaptureSummary = captureSummary
      }

applyRbacDataflowPatch ::
  RbacSize ->
  RbacDataflowPatchStage ->
  RbacPatchShape ->
  RbacTruth ->
  R.Runtime RbacContext RbacProp ->
  Rng ->
  Either
    RbacDataflowFixtureError
    (RbacTruth, R.Runtime RbacContext RbacProp, Rng, RbacPatchSummary)
applyRbacDataflowPatch sizeValue stage shape truth0 runtime0 rng0 = do
  (truth1, patchValue, rng1, summary) <-
    first RbacDataflowFixtureBuildFailed $
      generatePatchBatch rbacAtoms sizeValue shape truth0 rng0
  runtime1 <-
    first (RbacDataflowPatchApplyFailed stage) $
      R.applyPatch patchValue runtime0
  pure (truth1, runtime1, rng1, summary)

previewRbacDataflowPatch ::
  RbacSize ->
  RbacDataflowPatchStage ->
  RbacPatchShape ->
  RbacTruth ->
  R.Runtime RbacContext RbacProp ->
  Rng ->
  Either
    RbacDataflowFixtureError
    (RbacTruth, RuntimeDataflowSnapshot, Rng, RbacPatchSummary)
previewRbacDataflowPatch sizeValue stage shape truth0 runtime0 rng0 = do
  (truth1, patchValue, rng1, summary) <-
    first RbacDataflowFixtureBuildFailed $
      generatePatchBatch rbacAtoms sizeValue shape truth0 rng0
  snapshot <-
    first (RbacDataflowPatchApplyFailed stage) $
      runtimeDataflowSnapshotForPatch patchValue runtime0
  pure (truth1, snapshot, rng1, summary)

rbacDataflowSnapshot :: Either RbacDataflowFixtureError RuntimeDataflowSnapshot
rbacDataflowSnapshot =
  rdwcSnapshot <$> rbacDataflowWorkloadCapture

writeRbacDataflowCBOR :: FilePath -> IO (Either RbacDataflowFixtureError ())
writeRbacDataflowCBOR path =
  traverse (writeRuntimeDataflowCBOR path) rbacDataflowSnapshot

initialRbacDataflowLiveState :: Either RbacDataflowFixtureError RbacDataflowLiveState
initialRbacDataflowLiveState =
  initialRbacDataflowLiveStateWith rbacDataflowWorkloadPatchShape

initialRbacDataflowLiveStateWith ::
  RbacPatchShape ->
  Either RbacDataflowFixtureError RbacDataflowLiveState
initialRbacDataflowLiveStateWith patchShape = do
  let atomsValue = rbacAtoms
      sizeValue = testRbacSize
      (truth0, rng0) = seedTruth sizeValue testRbacSeedCounts rbacDataflowWorkloadSeed
  plansValue <- first RbacDataflowFixtureBuildFailed (fullSoakPlans atomsValue)
  runtime0 <- first RbacDataflowFixtureBuildFailed (buildRuntimeFromTruth atomsValue plansValue truth0)
  (truthWarm, runtimeWarm, rngWarm, _warmupSummary) <-
    applyRbacDataflowPatch
      sizeValue
      RbacDataflowWarmupPatch
      rbacDataflowWorkloadPatchShape
      truth0
      runtime0
      rng0
  pure
    RbacDataflowLiveState
      { rdlsSize = sizeValue,
        rdlsTruth = truthWarm,
        rdlsRuntime = runtimeWarm,
        rdlsRng = rngWarm,
        rdlsPatchShape = patchShape,
        rdlsStepIndex = 0
      }

stepRbacDataflowLiveState ::
  RbacDataflowLiveState ->
  Either RbacDataflowFixtureError (RuntimeDataflowSnapshot, RbacDataflowLiveState)
stepRbacDataflowLiveState state = do
  (truthNext, patchValue, rngNext, _summary) <-
    first RbacDataflowFixtureBuildFailed $
      generatePatchBatch
        rbacAtoms
        (rdlsSize state)
        (rdlsPatchShape state)
        (rdlsTruth state)
        (rdlsRng state)
  (runtimeNext, snapshot) <-
    first (RbacDataflowPatchApplyFailed (RbacDataflowLivePatch (rdlsStepIndex state))) $
      runtimeDataflowStepForPatch patchValue (rdlsRuntime state)
  pure
    ( snapshot,
      state
        { rdlsTruth = truthNext,
          rdlsRuntime = runtimeNext,
          rdlsRng = rngNext,
          rdlsStepIndex = rdlsStepIndex state + 1
        }
    )

rbacDataflowLiveSnapshots :: Int -> Either RbacDataflowFixtureError [RuntimeDataflowSnapshot]
rbacDataflowLiveSnapshots requested = do
  state0 <- initialRbacDataflowLiveState
  go (max 0 requested) state0
  where
    go :: Int -> RbacDataflowLiveState -> Either RbacDataflowFixtureError [RuntimeDataflowSnapshot]
    go remaining state
      | remaining <= 0 =
          Right []
      | otherwise = do
          (snapshot, nextState) <- stepRbacDataflowLiveState state
          (snapshot :) <$> go (remaining - 1) nextState

runtimeDataflowSnapshotHex :: RuntimeDataflowSnapshot -> String
runtimeDataflowSnapshotHex =
  foldMap hexWord8 . BSL.unpack . encodeRuntimeDataflowCBOR
{-# INLINE runtimeDataflowSnapshotHex #-}

hexWord8 :: Word8 -> String
hexWord8 byte =
  prefix <> showHex byte ""
  where
    prefix =
      if byte < 16 then "0" else ""
{-# INLINE hexWord8 #-}
