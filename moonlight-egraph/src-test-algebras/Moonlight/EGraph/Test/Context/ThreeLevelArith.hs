{-# LANGUAGE RankNTypes #-}

module Moonlight.EGraph.Test.Context.ThreeLevelArith
  ( moduleContextGraph,
    fixtureContextGraph,
    fixtureModuleMergedContextGraph,
    fixtureProofEGraph,
  )
where

import Moonlight.EGraph.Test.Arith.Core
  ( ArithF,
    NodeCount,
    addTermNode,
    analysisSpec,
    numTerm,
  )
import Moonlight.EGraph.Test.Context.ThreeLevel (Scope (ModuleCtx))

import Data.Bifunctor (first)
import Moonlight.EGraph.Pure.Context (ContextDeltaError (..), ContextEGraph, contextMerge, globalMerge, withEmptyContextEGraph)
import Moonlight.EGraph.Pure.Context (cegBase)
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.Core (emptySubstitution)
import Moonlight.EGraph.Pure.Context.Proof (ProofEGraph, emptyProofEGraph, recordProofStepWith)
import Moonlight.EGraph.Pure.Types (ClassId (..), EGraph, canonicalizeClassId, emptyEGraph, RewriteRuleId (..))
import Moonlight.Rewrite.ProofContext (defaultProofStepInput)
import Moonlight.FiniteLattice (ContextLattice, latticeContext)

moduleContextGraph ::
  EGraph ArithF NodeCount ->
  (forall owner. ContextEGraph owner ArithF NodeCount Scope -> result) ->
  result
moduleContextGraph baseGraph =
  withEmptyContextEGraph moduleLattice baseGraph

moduleLattice :: ContextLattice Scope
moduleLattice =
  case latticeContext of
    Right latticeValue -> latticeValue
    Left compileError ->
      error ("invalid Scope lattice fixture: " <> show compileError)

fixtureContextGraph ::
  (forall owner. (ClassId, ClassId, ContextEGraph owner ArithF NodeCount Scope) -> result) ->
  Either (ContextDeltaError ArithF Scope) result
fixtureContextGraph useFixture = do
  let graph0 = emptyEGraph analysisSpec
  (oneClassId, graph1) <- first ContextClassIdAllocationFailed (addTerm (numTerm 1) graph0)
  (_, graph2) <- first ContextClassIdAllocationFailed (addTerm (numTerm 0) graph1)
  (sumClassId, graph3) <- first ContextClassIdAllocationFailed (addTerm (addTermNode (numTerm 1) (numTerm 0)) graph2)
  pure
    ( moduleContextGraph graph3 $ \contextGraph ->
        useFixture (sumClassId, oneClassId, contextGraph)
    )

fixtureModuleMergedContextGraph ::
  (forall owner. (ClassId, ClassId, ContextEGraph owner ArithF NodeCount Scope) -> result) ->
  Either (ContextDeltaError ArithF Scope) result
fixtureModuleMergedContextGraph useFixture =
  either Left id $
    fixtureContextGraph $ \(sumClassId, oneClassId, contextGraph) -> do
      mergedContextGraph <- contextMerge ModuleCtx sumClassId oneClassId contextGraph
      pure (useFixture (sumClassId, oneClassId, mergedContextGraph))

fixtureProofEGraph ::
  (forall owner. ProofEGraph owner ArithF NodeCount Scope () -> result) ->
  Either (ContextDeltaError ArithF Scope) result
fixtureProofEGraph useFixture =
  either Left id $
    fixtureContextGraph $ \(sumClassId, oneClassId, contextGraph) -> do
      proofContextGraph <- globalMerge sumClassId oneClassId contextGraph
      pure
        ( useFixture
            ( recordProofStepWith
                (canonicalizeClassId (cegBase proofContextGraph))
                (defaultProofStepInput (RewriteRuleId 0) sumClassId oneClassId emptySubstitution ())
                (emptyProofEGraph proofContextGraph)
            )
          )
