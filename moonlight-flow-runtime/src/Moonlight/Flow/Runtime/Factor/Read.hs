{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Runtime.Factor.Read
  ( readFactorRows,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.IntMap.Strict qualified as IntMap
import Moonlight.Core
  ( QueryId,
  )
import Moonlight.Flow.Execution.Factor.Enumerate
  ( foldBagRows,
  )
import Moonlight.Flow.Execution.Factor.Run
  ( runFactor,
  )
import Moonlight.Flow.Execution.Factor.Types
  ( FactorDemand (..),
    FactorRunResult (..),
    FactorRunSpec (..),
  )
import Moonlight.Flow.Execution.Observe.Provenance.GC
  ( defaultProvGCConfig,
  )
import Moonlight.Delta.Signed
  ( Multiplicity (..)
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )
import Moonlight.Flow.Runtime.Factor.Input
  ( FactorInputFrame (..),
    factorInputFrameRuntime,
  )
import Moonlight.Flow.Runtime.Factor.Program
  ( factorProgramQueryId,
    factorProgramDecompPlan,
  )
import Moonlight.Flow.Runtime.Spec.FactorProgram
  ( RepairProgramKey,
  )
import Moonlight.Flow.Runtime.Factor.Request
  ( FactorRepairRequest,
    FactorRepairScope (..),
    patchRepair,
    repairRequest,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
    RuntimeEnv (..),
  )
import Moonlight.Flow.Runtime.Factor.State
  ( lookupFactorProgramByKey,
    lookupQueryBinding,
  )
import Moonlight.Flow.Runtime.Factor.State.Types
  ( RuntimeQueryBinding (..),
  )
import Moonlight.Flow.Runtime.Types
  ( RuntimeReadError (..),
  )
import Moonlight.Flow.Runtime.Spec.Schema
  ( RuntimePlan (..),
    RuntimePlanProjection (..),
    runtimePlanQueryId,
  )

readFactorRows ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  RuntimePlan ctx prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  r ->
  (RowTupleKey -> Multiplicity -> r -> r) ->
  Either (RuntimeReadError ctx prop) r
readFactorRows plan runtime initial step =
  case lookupQueryBinding queryId runtime of
    Nothing ->
      Left (RuntimeReadPlanMissing queryId)
    Just binding ->
      case lookupFactorProgramByKey (rqbRepairKey binding) runtime of
        Nothing ->
          Left (RuntimeReadPlanMissing queryId)
        Just program -> do
          let !representativeQueryId =
                factorProgramQueryId program
          inputFrame <-
            first
              (const (RuntimeReadFactorRowsUnavailable queryId))
              ( factorInputFrameRuntime
                  (reAtomCarrierEmitSpec (rdrEnv runtime))
                  representativeQueryId
                  (readRowsRepairRequest plan (rqbRepairKey binding) representativeQueryId)
                  program
                  runtime
              )
          result <-
            first
              (RuntimeReadFactorRowsObstructed queryId)
              ( runFactor
                  FactorRunSpec
                    { frsDecomp = factorProgramDecompPlan program,
                      frsInput = fifInput inputFrame,
                      frsCache = fifCache inputFrame,
                      frsGc = defaultProvGCConfig,
                      frsRepairTelemetry = reRepairTelemetry (rdrEnv (fifRuntime inputFrame)),
                      frsDemand = FactorDemandRows
                    }
              )
          pure $
            foldBagRows
              (rppFullSchema (rpProjection plan))
              (factorProgramDecompPlan program)
              (frrPreSealCache result)
              initial
              ( \rowValue acc0 ->
                  let !acc1 =
                        step rowValue (Multiplicity 1) acc0
                   in acc1
              )
  where
    queryId =
      runtimePlanQueryId plan
{-# INLINE readFactorRows #-}

readRowsRepairRequest ::
  RuntimePlan ctx prop ->
  RepairProgramKey ->
  QueryId ->
  FactorRepairRequest ctx prop
readRowsRepairRequest plan repairKey queryId =
  repairRequest
    (rpContext plan)
    (rpProp plan)
    repairKey
    queryId
    ( patchRepair
        FactorRepairScope
          { frsRelationalScope = mempty,
            frsAtomDeltas = IntMap.empty
          }
    )
{-# INLINE readRowsRepairRequest #-}
