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
import Moonlight.EGraph.Pure.Context (ContextDeltaError (..), ContextEGraph, contextMerge, emptyContextEGraph, globalMerge)
import Moonlight.EGraph.Pure.Context.Core (cegBase)
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.Core (emptySubstitution)
import Moonlight.EGraph.Pure.Context.Proof (ProofEGraph, emptyProofEGraph, recordProofStepWith)
import Moonlight.EGraph.Pure.Types (ClassId (..), EGraph, canonicalizeClassId, emptyEGraph, RewriteRuleId (..))
import Moonlight.Rewrite.ProofContext (defaultProofStepInput)
import Moonlight.FiniteLattice (ContextLattice, latticeContext)

moduleContextGraph :: EGraph ArithF NodeCount -> ContextEGraph ArithF NodeCount Scope
moduleContextGraph =
  emptyContextEGraph moduleLattice

moduleLattice :: ContextLattice Scope
moduleLattice =
  case latticeContext of
    Right latticeValue -> latticeValue
    Left compileError ->
      error ("invalid Scope lattice fixture: " <> show compileError)

fixtureContextGraph :: Either (ContextDeltaError ArithF Scope) (ClassId, ClassId, ContextEGraph ArithF NodeCount Scope)
fixtureContextGraph = do
  let graph0 = emptyEGraph analysisSpec
  (oneClassId, graph1) <- first ContextClassIdAllocationFailed (addTerm (numTerm 1) graph0)
  (_, graph2) <- first ContextClassIdAllocationFailed (addTerm (numTerm 0) graph1)
  (sumClassId, graph3) <- first ContextClassIdAllocationFailed (addTerm (addTermNode (numTerm 1) (numTerm 0)) graph2)
  pure (sumClassId, oneClassId, moduleContextGraph graph3)

fixtureModuleMergedContextGraph ::
  Either
    (ContextDeltaError ArithF Scope)
    (ClassId, ClassId, ContextEGraph ArithF NodeCount Scope)
fixtureModuleMergedContextGraph =
  fixtureContextGraph >>= \(sumClassId, oneClassId, contextGraph) ->
    fmap
      (\mergedContextGraph -> (sumClassId, oneClassId, mergedContextGraph))
      (contextMerge ModuleCtx sumClassId oneClassId contextGraph)

fixtureProofEGraph :: Either (ContextDeltaError ArithF Scope) (ProofEGraph ArithF NodeCount Scope ())
fixtureProofEGraph =
  fixtureContextGraph >>= \(sumClassId, oneClassId, contextGraph) ->
    fmap
      ( \proofContextGraph ->
          recordProofStepWith
            (canonicalizeClassId (cegBase proofContextGraph))
            (defaultProofStepInput (RewriteRuleId 0) sumClassId oneClassId emptySubstitution ())
            (emptyProofEGraph proofContextGraph)
      )
      (globalMerge sumClassId oneClassId contextGraph)
