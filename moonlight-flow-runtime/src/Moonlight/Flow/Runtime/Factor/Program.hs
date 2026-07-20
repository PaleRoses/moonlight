{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Runtime.Factor.Program
  ( FactorProgram (..),
    FactorAtomReadStamp (..),
    FactorCacheReadiness (..),
    FactorCacheState (..),
    FactorNodeCache (..),
    clearFactorCacheState,
    emptyFactorCacheState,
    factorProgramFromSpec,
    compileFactorProgram,
    validateFactorProgram,
    factorProgramSpec,
    factorProgramRepairKey,
    factorProgramQueryId,
    factorProgramQueryPlan,
    factorProgramCanonical,
    factorProgramFactorShapeManifest,
    factorProgramDecompPlan,
    factorProgramAtomKeys,
    factorProgramFactorNodes,
    factorProgramMaintenanceNodes,
    factorProgramCacheState,
    factorProgramCacheCold,
    factorProgramWithCacheState,
    clearFactorProgramCache,
  )
where

import Data.IntSet qualified as IntSet
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Flow.Execution.Subsumption.FactorShape
  ( FactorShapeManifest,
  )
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Flow.Plan.Shape
  ( CanonicalizationResult,
  )
import Moonlight.Flow.Runtime.Factor.Internal.Cache
  ( FactorAtomReadStamp (..),
    FactorCacheReadiness (..),
    FactorCacheState (..),
    FactorNodeCache (..),
    clearFactorCacheState,
    emptyFactorCacheState,
    factorCacheReadiness,
  )
import Moonlight.Flow.Runtime.Spec.FactorProgram
  ( ErasedQueryPlanShape,
    FactorProgramError,
    FactorProgramSpec (..),
    RepairProgramKey,
    compileFactorProgramSpec,
    factorProgramSpecAtomKeys,
    factorProgramSpecFactorNodes,
    factorProgramSpecQueryId,
    factorProgramSpecRepairKey,
    validateFactorProgramSpec,
  )

data FactorProgram = FactorProgram
  { fpSpec :: !FactorProgramSpec,
    fpCacheState :: !FactorCacheState
  }
  deriving stock (Eq, Show)

factorProgramFromSpec ::
  FactorProgramSpec ->
  Either FactorProgramError FactorProgram
factorProgramFromSpec spec = do
  validateFactorProgramSpec spec
  pure
    FactorProgram
      { fpSpec = spec,
        fpCacheState = emptyFactorCacheState
      }
{-# INLINE factorProgramFromSpec #-}

compileFactorProgram ::
  QueryPlan compiled output guard tag tuple key ->
  DecompPlan ->
  Either FactorProgramError FactorProgram
compileFactorProgram plan decomp =
  compileFactorProgramSpec plan decomp >>= factorProgramFromSpec
{-# INLINE compileFactorProgram #-}

validateFactorProgram ::
  FactorProgram ->
  Either FactorProgramError ()
validateFactorProgram =
  validateFactorProgramSpec . factorProgramSpec
{-# INLINE validateFactorProgram #-}

factorProgramSpec :: FactorProgram -> FactorProgramSpec
factorProgramSpec =
  fpSpec
{-# INLINE factorProgramSpec #-}

factorProgramRepairKey ::
  FactorProgram ->
  RepairProgramKey
factorProgramRepairKey =
  factorProgramSpecRepairKey . factorProgramSpec
{-# INLINE factorProgramRepairKey #-}

factorProgramQueryId :: FactorProgram -> QueryId
factorProgramQueryId =
  factorProgramSpecQueryId . factorProgramSpec
{-# INLINE factorProgramQueryId #-}

factorProgramQueryPlan :: FactorProgram -> ErasedQueryPlanShape
factorProgramQueryPlan =
  fpsQueryPlan . factorProgramSpec
{-# INLINE factorProgramQueryPlan #-}

factorProgramCanonical :: FactorProgram -> CanonicalizationResult
factorProgramCanonical =
  fpsCanonical . factorProgramSpec
{-# INLINE factorProgramCanonical #-}

factorProgramFactorShapeManifest :: FactorProgram -> FactorShapeManifest
factorProgramFactorShapeManifest =
  fpsFactorShapeManifest . factorProgramSpec
{-# INLINE factorProgramFactorShapeManifest #-}

factorProgramDecompPlan :: FactorProgram -> DecompPlan
factorProgramDecompPlan =
  fpsDecompPlan . factorProgramSpec
{-# INLINE factorProgramDecompPlan #-}

factorProgramAtomKeys :: FactorProgram -> IntSet.IntSet
factorProgramAtomKeys =
  factorProgramSpecAtomKeys . factorProgramSpec
{-# INLINE factorProgramAtomKeys #-}

factorProgramFactorNodes :: FactorProgram -> [FactorNode]
factorProgramFactorNodes =
  factorProgramSpecFactorNodes . factorProgramSpec
{-# INLINE factorProgramFactorNodes #-}

factorProgramMaintenanceNodes :: FactorProgram -> Set FactorNode
factorProgramMaintenanceNodes =
  Set.fromList . filter (not . factorNodeIsBagBelief) . factorProgramFactorNodes
{-# INLINE factorProgramMaintenanceNodes #-}

factorProgramCacheState :: FactorProgram -> FactorCacheState
factorProgramCacheState =
  fpCacheState
{-# INLINE factorProgramCacheState #-}

factorProgramCacheCold :: FactorProgram -> Bool
factorProgramCacheCold program =
  case factorCacheReadiness (factorProgramAtomKeys program) (fpCacheState program) of
    FactorCacheReady _ ->
      False
    FactorCacheCold ->
      True
    FactorCacheIncoherent ->
      True
{-# INLINE factorProgramCacheCold #-}

factorProgramWithCacheState ::
  FactorCacheState ->
  FactorProgram ->
  FactorProgram
factorProgramWithCacheState cacheState program =
  program {fpCacheState = cacheState}
{-# INLINE factorProgramWithCacheState #-}

clearFactorProgramCache :: FactorProgram -> FactorProgram
clearFactorProgramCache program =
  program {fpCacheState = clearFactorCacheState (fpCacheState program)}
{-# INLINE clearFactorProgramCache #-}
