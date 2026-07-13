{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.EGraph.Pure.Saturation.Substrate.Compile.Query
  ( registerCompiledPatternQueries,
    preparedRuleRequest,
  )
where

import Data.Bifunctor (first)
import Moonlight.Core
  ( Language,
  )
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingContextEGraph,
    cssQueryRegistry,
    sceSaturationState,
  )
import Moonlight.EGraph.Pure.Relational
  ( compiledPatternQueryFingerprint,
  )
import Moonlight.EGraph.Pure.Saturation.Matching
  ( AnnotatedContextSource,
    EGraphMatchingObstruction (..),
    MatchSite,
    MatchingRequest,
  )
import Moonlight.Rewrite.System
  ( CompiledGuard,
  )
import Moonlight.Rewrite.Algebra
  ( CompiledPatternQuery,
  )
import Moonlight.Rewrite.Runtime (RulePlan (..))
import Moonlight.Saturation.Matching qualified as GenericMatching
import Moonlight.Saturation.Context.Match.State.Registry qualified as SaturationMatch

registerCompiledPatternQueries ::
  (Language f, Show (f ()), Show capability) =>
  [CompiledPatternQuery (CompiledGuard capability f) f] ->
  SaturatingContextEGraph capability f a c ->
  Either EGraphMatchingObstruction (SaturatingContextEGraph capability f a c)
registerCompiledPatternQueries [] saturatingGraph =
  Right saturatingGraph
registerCompiledPatternQueries compiledQueries saturatingGraph =
  fmap
    ( \fingerprints ->
        let saturationState =
              sceSaturationState saturatingGraph
            registeredRegistry =
              SaturationMatch.registerQueryFingerprints
                fingerprints
                (cssQueryRegistry saturationState)
         in saturatingGraph
              { sceSaturationState =
                  saturationState
                    { cssQueryRegistry = registeredRegistry
                    }
              }
    )
    ( traverse
        ( fmap GenericMatching.QueryFingerprint
            . first EGraphMatchingPatternAtomizeObstruction
            . compiledPatternQueryFingerprint
        )
        compiledQueries
    )

preparedRuleRequest ::
  MatchSite c ->
  Maybe (AnnotatedContextSource f) ->
  RulePlan (CompiledGuard capability f) f ->
  MatchingRequest c capability f a
preparedRuleRequest site annotatedSource compiledRule =
  GenericMatching.QueryRequest
    { GenericMatching.qrSite = site,
      GenericMatching.qrSnapshot = annotatedSource,
      GenericMatching.qrQuery = rpQuery compiledRule,
      GenericMatching.qrPurpose = GenericMatching.RewritePurpose (rpId compiledRule)
    }
