{-# LANGUAGE RankNTypes #-}

-- | Reusable powerset context fixture for dense-vs-symbolic e-graph workloads.
module Moonlight.EGraph.Test.Context.Powerset
  ( PowersetContext,
    PowersetTwinGraph,
    PowersetTwinObstruction,
    PowersetTwinFixtureError (..),
    PowersetTwinWorkload (..),
    powersetTwinAtoms,
    powersetContextOf,
    powersetSubsets,
    powersetProbeContexts,
    powersetProbePairs,
    powersetTwinProbeContexts,
    powersetTwinProbePairs,
    densePowersetLattice,
    densePowersetSite,
    symbolicPowersetSite,
    powersetTwinFixture,
    powersetTwinWorkload,
  )
where

import Data.Bifunctor (first)
import Data.List (subsequences)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.EGraph.Pure.Context
  ( ContextDeltaError (..),
    ContextEGraph,
    contextMerge,
    emptyContextEGraphFromSite,
  )
import Moonlight.EGraph.Pure.Context
  ( ContextEGraphObstructionFailure,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.EGraph.Pure.Types (ClassId, ENode, emptyEGraph)
import Moonlight.EGraph.Test.Context.SimpleArith
  ( ArithF,
    Depth,
    depthSpec,
    lit,
    plus,
  )
import Moonlight.FiniteLattice
  ( ContextLattice,
    ContextLatticeCompileError,
    compileContextLattice,
    contextOrderDecl,
  )
import Moonlight.Sheaf.Context.Core (SectionMismatch)
import Moonlight.Sheaf.Context.Site
  ( PowersetSitePreparationError,
    PreparedContextSite,
    withPreparedContextSiteFromFiniteLattice,
    withPreparedContextSiteFromPowersetAtoms,
  )
import Moonlight.Sheaf.Obstruction (Obstruction)

type PowersetContext = Set Char

type PowersetTwinGraph owner = ContextEGraph owner ArithF Depth PowersetContext

type PowersetTwinObstruction =
  Obstruction
    ClassId
    (ENode ArithF)
    ()
    PowersetContext
    ()
    (SectionMismatch ClassId Depth)
    (ContextEGraphObstructionFailure PowersetContext)

data PowersetTwinFixtureError
  = PowersetDenseLatticeFailed !(ContextLatticeCompileError PowersetContext)
  | PowersetSymbolicSiteFailed !(PowersetSitePreparationError Char)
  | PowersetDenseMergeFailed !(ContextDeltaError ArithF PowersetContext)
  | PowersetSymbolicMergeFailed !(ContextDeltaError ArithF PowersetContext)
  deriving stock (Eq, Show)

data PowersetTwinWorkload denseOwner symbolicOwner = PowersetTwinWorkload
  { ptwClassA :: !ClassId,
    ptwClassB :: !ClassId,
    ptwDenseGraph :: !(PowersetTwinGraph denseOwner),
    ptwSymbolicGraph :: !(PowersetTwinGraph symbolicOwner),
    ptwProbeContexts :: ![PowersetContext],
    ptwProbePairs :: ![(PowersetContext, PowersetContext)]
  }

powersetTwinAtoms :: [Char]
powersetTwinAtoms =
  "abc"

powersetContextOf :: [Char] -> PowersetContext
powersetContextOf =
  Set.fromList

powersetSubsets :: [Char] -> [PowersetContext]
powersetSubsets =
  fmap Set.fromList . subsequences

powersetProbeContexts :: [Char] -> [PowersetContext]
powersetProbeContexts =
  powersetSubsets

powersetProbePairs :: [Char] -> [(PowersetContext, PowersetContext)]
powersetProbePairs atomValues =
  liftA2 (,) probeContexts probeContexts
  where
    probeContexts =
      powersetProbeContexts atomValues

powersetTwinProbeContexts :: [PowersetContext]
powersetTwinProbeContexts =
  fmap powersetContextOf ["", "a", "b", "ab", "abc"]

powersetTwinProbePairs :: [(PowersetContext, PowersetContext)]
powersetTwinProbePairs =
  liftA2 (,) powersetTwinProbeContexts powersetTwinProbeContexts

densePowersetLattice :: [Char] -> Either PowersetTwinFixtureError (ContextLattice PowersetContext)
densePowersetLattice atomValues =
  first PowersetDenseLatticeFailed
    ( compileContextLattice
        (Set.fromList subsets)
        ( contextOrderDecl
            (Set.fromList atomValues)
            Set.empty
            (powersetCoverEdges atomValues subsets)
        )
    )
  where
    subsets =
      powersetSubsets atomValues

densePowersetSite ::
  [Char] ->
  (forall owner. PreparedContextSite owner PowersetContext -> result) ->
  Either PowersetTwinFixtureError result
densePowersetSite atomValues useSite =
  fmap
    (\latticeValue -> withPreparedContextSiteFromFiniteLattice latticeValue useSite)
    (densePowersetLattice atomValues)

symbolicPowersetSite ::
  [Char] ->
  (forall owner. PreparedContextSite owner PowersetContext -> result) ->
  Either PowersetTwinFixtureError result
symbolicPowersetSite atomValues useSite =
  first PowersetSymbolicSiteFailed
    (withPreparedContextSiteFromPowersetAtoms atomValues useSite)

powersetTwinFixture ::
  PreparedContextSite owner PowersetContext ->
  Either (ContextDeltaError ArithF PowersetContext) (ClassId, ClassId, PowersetTwinGraph owner)
powersetTwinFixture site =
  let graph0 = emptyEGraph depthSpec
   in do
        (classA, graph1) <- first ContextClassIdAllocationFailed (addTerm (lit 1) graph0)
        (classB, graph2) <- first ContextClassIdAllocationFailed (addTerm (plus (lit 2) (lit 3)) graph1)
        fmap
          ((,,) classA classB)
          (contextMerge (powersetContextOf "a") classA classB (emptyContextEGraphFromSite site graph2))

powersetTwinWorkload ::
  [Char] ->
  (forall denseOwner symbolicOwner. PowersetTwinWorkload denseOwner symbolicOwner -> result) ->
  Either PowersetTwinFixtureError result
powersetTwinWorkload atomValues useWorkload =
  either Left id $
    densePowersetSite atomValues $ \denseSite ->
      either Left id $
        symbolicPowersetSite atomValues $ \symbolicSite -> do
          (classA, classB, denseGraph) <-
            first PowersetDenseMergeFailed (powersetTwinFixture denseSite)
          (_, _, symbolicGraph) <-
            first PowersetSymbolicMergeFailed (powersetTwinFixture symbolicSite)
          pure
            ( useWorkload
                PowersetTwinWorkload
                  { ptwClassA = classA,
                    ptwClassB = classB,
                    ptwDenseGraph = denseGraph,
                    ptwSymbolicGraph = symbolicGraph,
                    ptwProbeContexts = powersetProbeContexts atomValues,
                    ptwProbePairs = powersetProbePairs atomValues
                  }
            )

powersetCoverEdges :: [Char] -> [PowersetContext] -> [(PowersetContext, PowersetContext)]
powersetCoverEdges atomValues =
  concatMap
    ( \subset ->
        fmap
          (\atomValue -> (subset, Set.insert atomValue subset))
          (filter (not . (`Set.member` subset)) atomValues)
    )
