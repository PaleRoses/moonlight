{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Moonlight.Flow.Runtime.Core.Hydrate
  ( RuntimeSeedChunk (..),
    RuntimeSeedProgress (..),
    RuntimePatchPlanError (..),
    RuntimePatchPlan (..),
    RuntimeSeedHydrationPlan (..),
    RuntimeSeedHydrationStep (..),
    planRuntimePatch,
    planRuntimeSeedHydration,
    pendingSeedProgress,
    splitRuntimeSeedPatch,
  )
where

import Moonlight.Core
  ( QuotientEpoch,
  )
import Moonlight.Flow.Runtime.Core.Patch
  ( emptyPatch,
  )
import Moonlight.Flow.Runtime.Core.Patch.Internal
  ( Patch,
    QuotientPatch,
    patchAtomCount,
    patchNull,
    patchToQuotientPatch,
    splitPatchAtomEvents,
  )
import Moonlight.Flow.Runtime.Core.State
  ( RuntimeSeedState (..),
    runtimeSeedStateFromPatch,
    runtimeSeedStateSettled,
  )

data RuntimeSeedChunk
  = RuntimeSeedAll
  | RuntimeSeedAtoms {-# UNPACK #-} !Int
  deriving stock (Eq, Ord, Show, Read)

data RuntimeSeedProgress = RuntimeSeedProgress
  { rspAppliedAtoms :: {-# UNPACK #-} !Int,
    rspPendingAtoms :: {-# UNPACK #-} !Int,
    rspSettled :: !Bool
  }
  deriving stock (Eq, Ord, Show, Read)

data RuntimePatchPlanError
  = RuntimePatchInvalidSeedChunk !RuntimeSeedChunk
  | RuntimePatchSeedPending !RuntimeSeedProgress
  deriving stock (Eq, Ord, Show, Read)

data RuntimePatchPlan
  = RuntimePatchNoop
  | RuntimePatchSubmit !QuotientPatch
  deriving stock (Eq, Show)

data RuntimeSeedHydrationPlan
  = RuntimeSeedAlreadySettled !RuntimeSeedProgress
  | RuntimeSeedHydrationStepPlan !RuntimeSeedHydrationStep
  deriving stock (Eq, Show)

data RuntimeSeedHydrationStep = RuntimeSeedHydrationStep
  { rshpSelectedPatch :: !Patch,
    rshpRemainingPatch :: !Patch,
    rshpNextSeedState :: !RuntimeSeedState,
    rshpProgress :: !RuntimeSeedProgress
  }
  deriving stock (Eq, Show)

planRuntimePatch ::
  QuotientEpoch ->
  RuntimeSeedState ->
  Patch ->
  Either RuntimePatchPlanError RuntimePatchPlan
planRuntimePatch quotientEpoch seedState patch
  | patchNull patch =
      Right RuntimePatchNoop
  | otherwise =
      case seedState of
        RuntimeSeedSettled ->
          Right (RuntimePatchSubmit (patchToQuotientPatch quotientEpoch patch))
        RuntimeSeedPending pendingPatch ->
          Left (RuntimePatchSeedPending (pendingSeedProgress pendingPatch))
{-# INLINE planRuntimePatch #-}

planRuntimeSeedHydration ::
  RuntimeSeedChunk ->
  RuntimeSeedState ->
  Either RuntimePatchPlanError RuntimeSeedHydrationPlan
planRuntimeSeedHydration chunk seedState =
  case seedState of
    RuntimeSeedSettled ->
      Right
        ( RuntimeSeedAlreadySettled
            RuntimeSeedProgress
              { rspAppliedAtoms = 0,
                rspPendingAtoms = 0,
                rspSettled = True
              }
        )
    RuntimeSeedPending pendingPatch -> do
      (selectedPatch, remainingPatch) <-
        splitRuntimeSeedPatch chunk pendingPatch
      let nextSeedState =
            runtimeSeedStateFromPatch remainingPatch
          progress =
            RuntimeSeedProgress
              { rspAppliedAtoms = patchAtomCount selectedPatch,
                rspPendingAtoms = patchAtomCount remainingPatch,
                rspSettled = runtimeSeedStateSettled nextSeedState
              }
      Right
        ( RuntimeSeedHydrationStepPlan
            RuntimeSeedHydrationStep
              { rshpSelectedPatch = selectedPatch,
                rshpRemainingPatch = remainingPatch,
                rshpNextSeedState = nextSeedState,
                rshpProgress = progress
              }
        )
{-# INLINE planRuntimeSeedHydration #-}

pendingSeedProgress :: Patch -> RuntimeSeedProgress
pendingSeedProgress pendingPatch =
  RuntimeSeedProgress
    { rspAppliedAtoms = 0,
      rspPendingAtoms = patchAtomCount pendingPatch,
      rspSettled = False
    }
{-# INLINE pendingSeedProgress #-}

splitRuntimeSeedPatch ::
  RuntimeSeedChunk ->
  Patch ->
  Either RuntimePatchPlanError (Patch, Patch)
splitRuntimeSeedPatch chunk patch =
  case chunk of
    RuntimeSeedAll ->
      Right (patch, emptyPatch)
    RuntimeSeedAtoms atomCount ->
      case splitPatchAtomEvents atomCount patch of
        Just pair ->
          Right pair
        Nothing ->
          Left (RuntimePatchInvalidSeedChunk chunk)
{-# INLINE splitRuntimeSeedPatch #-}
