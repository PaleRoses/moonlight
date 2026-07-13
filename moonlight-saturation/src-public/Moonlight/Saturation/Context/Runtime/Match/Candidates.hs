{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Runtime.Match.Candidates
  ( enumerateProjectedBaseSiteMatches,
    enumerateContextSiteMatches,
    materializeRawMatchesAtContext,
  )
where

import Data.Map.Strict
  ( Map,
  )
import Data.Map.Lazy qualified as LazyMap
import Data.Map.Strict qualified as Map
import Data.Maybe
  ( mapMaybe,
  )
import Data.Set qualified as Set
import Moonlight.Core
  ( accumByKey,
  )
import Moonlight.Core
  ( MatchActivationIndex (..),
    SiteIndex (..),
    SiteProgram (..),
    SupportIndexedRule (..),
  )
import Moonlight.FiniteLattice
  ( supportGenerators,
  )
import Moonlight.Sheaf.Context.Site
  ( SupportCarrier,
  )
import Moonlight.Saturation.Context.Match.Algebra.Aggregate
  ( aggregateSupportedMatches,
  )
import Moonlight.Saturation.Context.Program.Plan
  ( Program,
    ProgramStage (CompiledProgramStage),
  )
import Moonlight.Saturation.Context.Runtime.Match.Facts
  ( effectiveContextMatchFactsAt,
  )
import Moonlight.Saturation.Context.Runtime.Round.Input
  ( RoundInput (..),
  )
import Moonlight.Saturation.Context.Runtime.State
  ( RuntimeCore (..),
    RuntimeState (..),
  )
import Moonlight.Saturation.Substrate

enumerateProjectedBaseSiteMatches ::
  forall u carrier schedulerGroup.
  ( MatchingBackend u,
    Ord (SatRuleKey u),
    Ord (SatContext u),
    Ord (SatClassId u),
    Semigroup (SatFactIndex u)
  ) =>
  RoundInput u carrier schedulerGroup ->
  Int ->
  SatMatchState u ->
  Program 'CompiledProgramStage u ->
  Either (SatObstruction u) (SatMatchState u, [SatSupportedMatch u])
enumerateProjectedBaseSiteMatches input iterationIndex startingMatchState siteProgram =
  let coreState =
        rsCore (riState input)

      rewriteUniverse =
        siBase (spRewriteRules siteProgram)

      rewriteActivation =
        spRewriteActivation siteProgram

      ruleIdToEngineKey =
        Map.fromList
          [ (rewriteRuleId @u rule, rewriteRuleKey @u rule)
          | rule <- rewriteUniverse
          ]

      rawBaseMatchesResult =
        rawBaseMatchesPrepared @u
          (riRewriteContext input)
          iterationIndex
          (rcMatchingDelta coreState)
          (riGraph input)
          (emptyFactStore @u)
          rewriteUniverse
          startingMatchState
   in do
        (nextMatchState, rawBaseMatches) <-
          rawBaseMatchesResult

        let rawMatchesByEngineKey =
              accumByKey
                (rawMatchRuleKey @u)
                (: [])
                rawBaseMatches

            projectForContext contextValue activeRuleIds =
              let (factStore, factDerivations) =
                    effectiveContextMatchFactsAt @u
                      (riBaseContext input)
                      (riBaseFacts input)
                      (riBaseFactDerivations input)
                      contextValue
                      coreState

                  activeEngineKeys =
                    mapMaybe
                      (`Map.lookup` ruleIdToEngineKey)
                      (Set.toAscList activeRuleIds)

                  rawMatchesForContext =
                    foldMap
                      ( \engineKey ->
                          Map.findWithDefault
                            []
                            engineKey
                            rawMatchesByEngineKey
                      )
                      activeEngineKeys
               in materializeRawMatchesAtContext @u
                    (riRewriteContext input)
                    (riCapabilityResolver input)
                    contextValue
                    factStore
                    factDerivations
                    (riBaseGraph input)
                    rawMatchesForContext

            projectedBaseMatches =
              projectForContext
                (riBaseContext input)
                (maiBase rewriteActivation)

            projectedContextMatches =
              foldMap
                (uncurry projectForContext)
                (Map.toAscList (maiContexts rewriteActivation))

        aggregatedMatches <-
          aggregateSupportedMatches @u
            (riGraph input)
            (projectedBaseMatches <> projectedContextMatches)

        pure
          ( nextMatchState,
            aggregatedMatches
          )
{-# INLINE enumerateProjectedBaseSiteMatches #-}

enumerateContextSiteMatches ::
  forall u carrier schedulerGroup.
  ( MatchingBackend u,
    Ord (SatRuleKey u),
    Ord (SatContext u),
    Ord (SatClassId u),
    Semigroup (SatFactIndex u)
  ) =>
  RoundInput u carrier schedulerGroup ->
  Int ->
  SatMatchState u ->
  Map (SatContext u) [SatRule u] ->
  [(SupportIndexedRule (SupportBasis (SatContext u)) (SatRule u), SupportCarrier (SatContext u))] ->
  Either (SatObstruction u) (SatMatchState u, [SatSupportedMatch u])
enumerateContextSiteMatches input iterationIndex startingMatchState contextRules supportedRules =
  let coreState =
        rsCore (riState input)

      activeContexts =
        Set.unions
          [ Set.fromList (graphExecutionContexts @u (riGraph input)),
            Map.keysSet contextRules,
            Map.keysSet (rcContextFactInputs coreState),
            Map.keysSet (rcContextFacts coreState),
            Map.keysSet (rcContextFactDerivations coreState),
            Map.keysSet (rcFactViewKeys coreState),
            Map.keysSet (rcFactRoundsByContext coreState),
            foldMap
              (Set.fromList . supportGenerators . sirSupport . fst)
              supportedRules
          ]

      contextInputs =
        LazyMap.fromSet
          ( \contextValue ->
              let (facts, derivations) =
                    effectiveContextMatchFactsAt @u
                      (riBaseContext input)
                      (riBaseFacts input)
                      (riBaseFactDerivations input)
                      contextValue
                      coreState
               in (facts, derivations, Map.findWithDefault [] contextValue contextRules)
          )
          activeContexts
   in contextSupportedMatchesPrepared @u
        (riRewriteContext input)
        (riCapabilityResolver input)
        iterationIndex
        (rcMatchingDelta coreState)
        (riGraph input)
        contextInputs
        supportedRules
        startingMatchState
{-# INLINE enumerateContextSiteMatches #-}

materializeRawMatchesAtContext ::
  forall u.
  MatchingBackend u =>
  SatRewriteContext u ->
  SatCapabilityResolver u ->
  SatContext u ->
  SatFactStore u ->
  SatFactIndex u ->
  SatBaseGraph u ->
  [SatRawMatch u] ->
  [SatSupportedMatch u]
materializeRawMatchesAtContext rewriteContext capabilityResolver contextValue factStore factDerivations baseGraph =
  mapMaybe
    ( \rawMatch ->
        either
          (const Nothing)
          Just
          ( materializeRawMatch @u
              rewriteContext
              capabilityResolver
              contextValue
              canonicalFactStore
              factDerivations
              baseGraph
              rawMatch
          )
    )
  where
    canonicalFactStore =
      canonicalizeFactStoreBase @u baseGraph factStore
{-# INLINE materializeRawMatchesAtContext #-}
