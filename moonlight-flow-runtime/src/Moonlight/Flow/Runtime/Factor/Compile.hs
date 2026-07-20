{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Moonlight.Flow.Runtime.Factor.Compile
  ( CompiledRuntimeFactorPrograms (..),
    compileRuntimeFactorPrograms,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.Bifunctor
  ( first,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( QueryId,
  )
import Moonlight.Flow.Runtime.Factor.Program
  ( FactorProgram (..),
    factorProgramFromSpec,
    factorProgramRepairKey,
    factorProgramSpec,
  )
import Moonlight.Flow.Runtime.Spec.FactorProgram
  ( FactorProgramError (..),
    RepairProgramKey,
    factorProgramSpecShapeCompatible,
  )
import Moonlight.Flow.Runtime.Factor.State.Types
  ( RuntimeQueryBinding (..),
  )
import Moonlight.Flow.Runtime.Spec.Schema
  ( RuntimePlan (..),
    RuntimePlanProjection (..),
    runtimePlanQueryId,
  )

data CompiledRuntimeFactorPrograms = CompiledRuntimeFactorPrograms
  { crfpPrograms :: !(Map RepairProgramKey FactorProgram),
    crfpQueryBindings :: !(Map QueryId RuntimeQueryBinding)
  }
  deriving stock (Eq, Show)

emptyCompiledRuntimeFactorPrograms :: CompiledRuntimeFactorPrograms
emptyCompiledRuntimeFactorPrograms =
  CompiledRuntimeFactorPrograms
    { crfpPrograms = Map.empty,
      crfpQueryBindings = Map.empty
    }
{-# INLINE emptyCompiledRuntimeFactorPrograms #-}

compileRuntimeFactorPrograms ::
  [RuntimePlan ctx prop] ->
  Either (QueryId, FactorProgramError) CompiledRuntimeFactorPrograms
compileRuntimeFactorPrograms =
  foldM compileOne emptyCompiledRuntimeFactorPrograms
  where
    compileOne ::
      CompiledRuntimeFactorPrograms ->
      RuntimePlan ctx prop ->
      Either (QueryId, FactorProgramError) CompiledRuntimeFactorPrograms
    compileOne compiled plan = do
      let !queryId =
            runtimePlanQueryId plan
          !projection =
            rpProjection plan
      program <-
        first
          (queryId,)
          (factorProgramFromSpec (rpProgram plan))
      let !repairKey =
            factorProgramRepairKey program
      programs' <-
        first
          (queryId,)
          (insertCanonicalProgram repairKey program (crfpPrograms compiled))
      let !binding =
            RuntimeQueryBinding
              { rqbRepairKey = repairKey,
                rqbFullSchema = rppFullSchema projection,
                rqbOutputSlots = rppOutputSlots projection
              }
      pure
        compiled
          { crfpPrograms = programs',
            crfpQueryBindings =
              Map.insert queryId binding (crfpQueryBindings compiled)
          }

insertCanonicalProgram ::
  RepairProgramKey ->
  FactorProgram ->
  Map RepairProgramKey FactorProgram ->
  Either FactorProgramError (Map RepairProgramKey FactorProgram)
insertCanonicalProgram repairKey program programs =
  case Map.lookup repairKey programs of
    Nothing ->
      Right (Map.insert repairKey program programs)
    Just existing
      | compatibleRepairProgram existing program ->
          Right programs
      | otherwise ->
          Left
            ( FactorProgramRepairKeyCollision
                repairKey
                (factorProgramSpec existing)
                (factorProgramSpec program)
            )
{-# INLINE insertCanonicalProgram #-}

compatibleRepairProgram ::
  FactorProgram ->
  FactorProgram ->
  Bool
compatibleRepairProgram existing candidate =
  factorProgramSpecShapeCompatible
    (factorProgramSpec existing)
    (factorProgramSpec candidate)
{-# INLINE compatibleRepairProgram #-}
