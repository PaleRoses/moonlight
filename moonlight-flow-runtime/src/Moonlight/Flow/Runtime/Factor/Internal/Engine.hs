{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Runtime.Factor.Internal.Engine
  ( selectFactorReuseAction,
    selectFactorRepairAction,
  )
where

import Control.Monad
  ( foldM,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
  )
import Moonlight.Flow.Carrier.Reuse
  ( CarrierReuseStrategy (..),
    carrierReuseStrategiesForMode,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.Flow.Runtime.Factor.Internal.Exact
  ( ExactFactorRepairResult (..),
    runExactFactorRepair,
  )
import Moonlight.Flow.Runtime.Factor.Internal.Reuse.SelectedMaterialization
  ( tryExactByCoverContainmentReuse,
    tryExactEquivalentReuse,
    tryLowerBoundContainmentReuse,
  )
import Moonlight.Flow.Runtime.Factor.Internal.Reuse.Result
  ( ExactByCoverReuseResult (..),
    ExactReuseResult (..),
    FactorReuseMaterialization (..),
    LowerBoundReuseResult (..),
  )
import Moonlight.Flow.Runtime.Factor.Program
  ( FactorProgram,
  )
import Moonlight.Flow.Runtime.Factor.Request
  ( FactorRepairRequest (..),
  )
import Moonlight.Flow.Runtime.Factor.Reuse
  ( FactorRepairAction (..),
    FactorReuseAction (..),
    FactorReuseKind (..),
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
    RuntimeEnv (..),
  )
import Moonlight.Flow.Runtime.Carrier.Reuse.CoverMaterialization
  ( CoverMaterializationPlan (..),
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )

data FactorRepairSelection ctx prop boundary evidence joinState joinErr = FactorRepairSelection
  { frsRuntime :: !(RelDiffRuntime ctx prop boundary evidence joinState joinErr),
    frsAction :: !(Maybe (FactorReuseAction ctx prop boundary evidence))
  }

selectFactorReuseAction ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  RelationalCarrierTime ctx ->
  FactorRepairRequest ctx prop ->
  FactorProgram ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      Maybe (FactorReuseAction ctx prop boundary evidence)
    )
selectFactorReuseAction eventTime request program runtime0 = do
  selection <-
    foldM
      (selectWithStrategy eventTime request program)
      FactorRepairSelection
        { frsRuntime = runtime0,
          frsAction = Nothing
        }
      (carrierReuseStrategiesForMode (reReuseMode (rdrEnv runtime0)))
  pure (frsRuntime selection, frsAction selection)

selectFactorRepairAction ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  RelationalCarrierTime ctx ->
  FactorRepairRequest ctx prop ->
  FactorProgram ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      FactorRepairAction ctx prop boundary evidence joinState joinErr
    )
selectFactorRepairAction eventTime request program runtime0 = do
  (runtime1, maybeReuseAction) <-
    selectFactorReuseAction eventTime request program runtime0
  case maybeReuseAction of
    Just reuseAction ->
      pure (runtime1, FactorActionReuse reuseAction)
    Nothing -> do
      exactResult <-
        runExactFactorRepair
          (reFactorCarrierEmitSpec (rdrEnv runtime1))
          eventTime
          request
          program
          runtime1
      pure (efrrRuntime exactResult, FactorActionExact exactResult)

selectWithStrategy ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  RelationalCarrierTime ctx ->
  FactorRepairRequest ctx prop ->
  FactorProgram ->
  FactorRepairSelection ctx prop boundary evidence joinState joinErr ->
  CarrierReuseStrategy ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (FactorRepairSelection ctx prop boundary evidence joinState joinErr)
selectWithStrategy eventTime request program selection strategy =
  case frsAction selection of
    Just _ ->
      pure selection
    Nothing -> do
      (runtime1, maybeAction) <-
        tryReuseStrategy eventTime request program strategy (frsRuntime selection)
      pure
        FactorRepairSelection
          { frsRuntime = runtime1,
            frsAction = maybeAction
          }

tryReuseStrategy ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  RelationalCarrierTime ctx ->
  FactorRepairRequest ctx prop ->
  FactorProgram ->
  CarrierReuseStrategy ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      Maybe (FactorReuseAction ctx prop boundary evidence)
    )
tryReuseStrategy eventTime request program strategy runtime =
  case strategy of
    ReuseExactEquivalent -> do
      (runtime1, maybeReuse) <-
        tryExactEquivalentReuse
          (reFactorCarrierEmitSpec (rdrEnv runtime))
          eventTime
          (frrQueryId request)
          program
          runtime
      pure
        ( runtime1,
          fmap
            ( \reuse ->
                FactorReuseAction
                  { fruaKind = FactorReuseExactEquivalent,
                    fruaMaterializations = [],
                    fruaCoverPlans = [],
                    fruaSnapshots = errSnapshots reuse,
                    fruaDeltas = errDeltas reuse
                  }
            )
            maybeReuse
        )
    ReuseExactByCover -> do
      (runtime1, maybeReuse) <-
        tryExactByCoverContainmentReuse
          (reFactorCarrierEmitSpec (rdrEnv runtime))
          eventTime
          (frrQueryId request)
          program
          runtime
      pure
        ( runtime1,
          fmap
            ( \reuse ->
                FactorReuseAction
                  { fruaKind = FactorReuseExactByCover,
                    fruaMaterializations = [],
                    fruaCoverPlans = ebcrPlans reuse,
                    fruaSnapshots = fmap cmpProjectedSnapshot (ebcrPlans reuse),
                    fruaDeltas = fmap cmpProjectedDelta (ebcrPlans reuse)
                  }
            )
            maybeReuse
        )
    ReuseLowerBound -> do
      (runtime1, maybeReuse) <-
        tryLowerBoundContainmentReuse
          (reFactorCarrierEmitSpec (rdrEnv runtime))
          eventTime
          (frrQueryId request)
          program
          runtime
      pure
        ( runtime1,
          fmap
            ( \reuse ->
                FactorReuseAction
                  { fruaKind = FactorReuseLowerBound,
                    fruaMaterializations = lbrrMaterializations reuse,
                    fruaCoverPlans = [],
                    fruaSnapshots = fmap frumProjectedSnapshot (lbrrMaterializations reuse),
                    fruaDeltas = fmap frumProjectedDelta (lbrrMaterializations reuse)
                  }
            )
            maybeReuse
        )
