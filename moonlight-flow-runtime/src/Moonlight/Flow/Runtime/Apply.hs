module Moonlight.Flow.Runtime.Apply
  ( applyPatch,
    hydrateRuntimeSeedChunk,
    hydrateRuntimeSeedFully,
  )
where

import Data.Bifunctor
  ( first,
  )
import Moonlight.Flow.Runtime.Core.Hydrate
  ( RuntimePatchPlan (..),
    RuntimePatchPlanError (..),
    RuntimeSeedHydrationPlan (..),
    RuntimeSeedHydrationStep (..),
    planRuntimePatch,
    planRuntimeSeedHydration,
  )
import Moonlight.Flow.Runtime.Core.Patch.Internal
  ( Patch,
    patchNull,
    patchToQuotientPatch,
  )
import Moonlight.Flow.Runtime.Core.State qualified as Core
import Moonlight.Flow.Runtime.Engine.Patch.Apply qualified as EnginePatch
import Moonlight.Flow.Runtime.Kernel
  ( RuntimeEnvelope (..),
  )
import Moonlight.Flow.Runtime.Types
  ( Runtime (..),
    RuntimeApplyError (..),
    RuntimeSeedChunk (..),
    RuntimeSeedProgress,
  )

applyPatch ::
  Patch ->
  Runtime ctx prop ->
  Either (RuntimeApplyError ctx prop) (Runtime ctx prop)
applyPatch patch runtime@(Runtime kernel) =
  case
    planRuntimePatch
      (Core.rsQuotientEpoch state)
      (Core.rsSeedState state)
      patch
    of
      Left err ->
        Left (runtimeApplyErrorFromPlanError err)
      Right RuntimePatchNoop ->
        Right runtime
      Right (RuntimePatchSubmit quotientPatch) ->
        Runtime
          <$> first
            RuntimeApplyRejected
            (EnginePatch.applyQuotientPatch quotientPatch kernel)
  where
    state =
      rdrState kernel
{-# INLINE applyPatch #-}

hydrateRuntimeSeedFully ::
  Runtime ctx prop ->
  Either (RuntimeApplyError ctx prop) (Runtime ctx prop)
hydrateRuntimeSeedFully runtime = do
  (runtime', _progress) <-
    hydrateRuntimeSeedChunk RuntimeSeedAll runtime
  pure runtime'
{-# INLINE hydrateRuntimeSeedFully #-}

hydrateRuntimeSeedChunk ::
  RuntimeSeedChunk ->
  Runtime ctx prop ->
  Either
    (RuntimeApplyError ctx prop)
    (Runtime ctx prop, RuntimeSeedProgress)
hydrateRuntimeSeedChunk chunk runtime@(Runtime kernel) =
  case planRuntimeSeedHydration chunk (Core.rsSeedState (rdrState kernel)) of
    Left err ->
      Left (runtimeApplyErrorFromPlanError err)
    Right (RuntimeSeedAlreadySettled progress) ->
      Right (runtime, progress)
    Right (RuntimeSeedHydrationStepPlan step)
      | patchNull (rshpSelectedPatch step) ->
          Right
            ( setRuntimeSeedState
                (rshpNextSeedState step)
                runtime,
              rshpProgress step
            )
      | otherwise -> do
          let quotientPatch =
                patchToQuotientPatch
                  (Core.rsQuotientEpoch (rdrState kernel))
                  (rshpSelectedPatch step)
          appliedKernel <-
            first
              RuntimeApplyRejected
              (EnginePatch.applyInitialQuotientPatch quotientPatch kernel)
          Right
            ( setRuntimeSeedState
                (rshpNextSeedState step)
                (Runtime appliedKernel),
              rshpProgress step
            )
{-# INLINE hydrateRuntimeSeedChunk #-}

runtimeApplyErrorFromPlanError ::
  RuntimePatchPlanError ->
  RuntimeApplyError ctx prop
runtimeApplyErrorFromPlanError planError =
  case planError of
    RuntimePatchInvalidSeedChunk chunk ->
      RuntimeApplyInvalidSeedChunk chunk
    RuntimePatchSeedPending progress ->
      RuntimeApplySeedPending progress
{-# INLINE runtimeApplyErrorFromPlanError #-}

setRuntimeSeedState ::
  Core.RuntimeSeedState ->
  Runtime ctx prop ->
  Runtime ctx prop
setRuntimeSeedState seedState (Runtime kernel) =
  Runtime
    kernel
      { rdrState =
          Core.setRuntimeSeedState seedState (rdrState kernel)
      }
{-# INLINE setRuntimeSeedState #-}
