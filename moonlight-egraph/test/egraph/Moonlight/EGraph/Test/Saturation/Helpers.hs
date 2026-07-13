{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

module Moonlight.EGraph.Test.Saturation.Helpers
  ( addXYPattern,
    compileRingPatternQuery,
    buildGraph,
    mkRequest,
    rootsOf,
    mkConfig,
  )
where

import Data.IntSet (IntSet)
import Moonlight.EGraph.Effect.CoveringSurface
  ( SurfaceKind,
  )
import Moonlight.Core (Pattern (..), UnionFindAllocationError)
import Moonlight.Core qualified as EGraph
import Moonlight.EGraph.Pure.Change (EGraphMutationResult (..))
import Moonlight.EGraph.Pure.Kernel.HashCons (insertTermsTracked)
import Moonlight.EGraph.Pure.Saturation.Matching
    ( AnnotatedContextSource,
      MatchingAlgebra,
      MatchingRequest,
      MatchingStrategy )
import Moonlight.EGraph.Pure.Saturation.Substrate (EGraphU)
import Moonlight.EGraph.Pure.Types
    ( classIdKey, ClassId, EGraph, RewriteRuleId, emptyEGraph )
import Moonlight.EGraph.Test.Ring.Core
    ( NodeCount, RingF(..), ringAnalysis )
import Data.Fix (Fix)
import Moonlight.Rewrite.System
    ( CompiledGuard, combineCompiledGuards, compileGuard )
import Moonlight.Rewrite.Algebra
    ( CompiledPatternQuery, compilePatternQuery, singlePatternQuery )
import Moonlight.EGraph.Test.Saturation
    ( SaturationConfig, data SaturationConfig, scBudget, scMatchingStrategy, scSchedulerConfig, deterministicSchedulerConfig )
import Moonlight.Saturation.Core ( SaturationBudget(..) )
import Moonlight.Saturation.Matching qualified as GenericMatching
    ( MatchSite,
      QueryRequest (..),
      SaturationPurpose(RawMatchPurpose) )
import Data.IntSet qualified as IntSet ( fromList )

addXYPattern :: Pattern RingF
addXYPattern =
  PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1)))

compileRingPatternQuery :: Pattern RingF -> Either [EGraph.PatternVar] (CompiledPatternQuery (CompiledGuard SurfaceKind RingF) RingF)
compileRingPatternQuery patternValue =
  compilePatternQuery combineCompiledGuards compileGuard (singlePatternQuery patternValue)

buildGraph :: [Fix RingF] -> Either UnionFindAllocationError (EGraph RingF NodeCount, [ClassId])
buildGraph terms =
  fmap
    ( \EGraphMutationResult
        { emrResult = classIds,
          emrGraph = graph
        } -> (graph, classIds)
    )
    (insertTermsTracked terms (emptyEGraph ringAnalysis))

mkRequest ::
  MatchingAlgebra state String SurfaceKind RingF NodeCount ->
  GenericMatching.MatchSite String ->
  Maybe (AnnotatedContextSource RingF) ->
  CompiledPatternQuery (CompiledGuard SurfaceKind RingF) RingF ->
  EGraph RingF NodeCount ->
  MatchingRequest String SurfaceKind RingF NodeCount
mkRequest _matchingAlgebra site snapshot compiled _graph =
  GenericMatching.QueryRequest
    { GenericMatching.qrSite = site,
      GenericMatching.qrSnapshot = snapshot,
      GenericMatching.qrQuery = compiled,
      GenericMatching.qrPurpose = GenericMatching.RawMatchPurpose
    }

rootsOf :: [(ClassId, subst)] -> IntSet
rootsOf =
  IntSet.fromList . fmap (classIdKey . fst)

mkConfig :: MatchingStrategy String SurfaceKind RingF NodeCount -> SaturationConfig (EGraphU SurfaceKind RingF NodeCount String) RewriteRuleId
mkConfig strategy =
  SaturationConfig
    { scBudget =
        SaturationBudget
          { sbMaxIterations = 4,
            sbMaxNodes = 256
          },
      scMatchingStrategy = strategy,
      scSchedulerConfig = deterministicSchedulerConfig
    }
