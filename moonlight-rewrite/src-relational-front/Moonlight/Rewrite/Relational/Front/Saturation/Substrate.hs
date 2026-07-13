{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Rewrite.Relational.Front.Saturation.Substrate
  ( RelationalFrontSaturation,
    relationalSaturationMatchScheduleKey,
    relationalSaturationMatchSubstitution,
    relationalSaturationSupportedScheduleKey,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.Foldable
  ( foldlM,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Kind
  ( Type,
  )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Void
  ( Void,
    absurd,
  )
import Moonlight.Sheaf.Context.Site
  ( unionPreparedSupport,
  )
import Moonlight.Core
  ( ClassId,
    RewriteRuleId (..),
    classIdKey,
  )
import Moonlight.Core
  ( Substitution,
  )
import Moonlight.Core
  ( mkQueryId,
  )
import Moonlight.Rewrite.Runtime
  ( ExecutedRewrite,
  )
import Moonlight.Rewrite.DSL
  ( NodeTag,
    RewriteSignature,
  )
import Moonlight.Rewrite.DSL
  ( ContextName,
  )
import Moonlight.Rewrite.Relational
  ( RelationalPlanSet (..),
  )
import Moonlight.Rewrite.Relational.Front.Host
  ( Host,
    hostBackend,
    hostCanonicalClass,
    hostClassCount,
    hostNodeClasses,
    hostRevision,
  )
import Moonlight.Rewrite.Relational.Front.Error
  ( RelationalProgramError,
  )
import Moonlight.Rewrite.Relational.Front.Saturation.Types
import Moonlight.Rewrite.Relational
  ( RewriteRunStats,
    appendRewriteRunStats,
    emptyRewriteRunStats,
    statsForRounds,
  )
import Moonlight.Rewrite.Relational
  ( RelationalRewriteMatch (..),
  )
import Moonlight.Rewrite.Relational
  ( RewriteRunError,
    RewriteRunResult (..),
    prepareRelationalSystem,
    runMatchRule,
    runMatchRuleWithContextHost,
  )
import Moonlight.Rewrite.System
  ( RuleName,
  )
import Moonlight.Rewrite.System
  ( RuleSupportIndex,
    baseRuleSupportIndex,
    contextRuleSupportIndex,
  )
import Moonlight.Saturation.Matching
  ( QueryFingerprint (..),
  )
import Moonlight.Saturation.Substrate
import GHC.TypeLits
  ( Symbol,
  )
import Moonlight.FiniteLattice
  ( principalSupport
  )

type RelationalFrontSaturation :: (Symbol -> (Symbol -> Type) -> Type) -> RewriteGuardAtomKind -> Type -> Type
data RelationalFrontSaturation sig atom projection

type instance SatGraph (RelationalFrontSaturation sig atom projection) = RelationalSaturationCarrier sig atom

type instance SatBaseGraph (RelationalFrontSaturation sig atom projection) = Host sig

type instance SatClassId (RelationalFrontSaturation sig atom projection) = ClassId

type instance SatContext (RelationalFrontSaturation sig atom projection) = RelationalSaturationContext

type instance SatObstruction (RelationalFrontSaturation sig atom projection) = RelationalSaturationObstruction sig

type instance SatCapabilityResolver (RelationalFrontSaturation sig atom projection) = ()

type instance SatFactStore (RelationalFrontSaturation sig atom projection) = ()

type instance SatFactIndex (RelationalFrontSaturation sig atom projection) = ()

type instance SatFactSource (RelationalFrontSaturation sig atom projection) = Void

type instance SatFactRule (RelationalFrontSaturation sig atom projection) = Void

type instance SatFactCompileError (RelationalFrontSaturation sig atom projection) = Void

type instance SatFactRound (RelationalFrontSaturation sig atom projection) = ()

type instance SatQuery (RelationalFrontSaturation sig atom projection) = RewriteRuleId

type instance SatMatchSnapshot (RelationalFrontSaturation sig atom projection) = QueryFingerprint

type instance SatMatchSection (RelationalFrontSaturation sig atom projection) = ()

type instance SatMatchingDelta (RelationalFrontSaturation sig atom projection) = ()

type instance SatChangeSummary (RelationalFrontSaturation sig atom projection) = ()

type instance SatRuleSource (RelationalFrontSaturation sig atom projection) = RelationalSaturationRule sig atom

type instance SatRule (RelationalFrontSaturation sig atom projection) = RelationalSaturationRule sig atom

type instance SatRuleKey (RelationalFrontSaturation sig atom projection) = RewriteRuleId

type instance SatRewriteContext (RelationalFrontSaturation sig atom projection) = SaturationConfig sig

type instance SatRuleCompileError (RelationalFrontSaturation sig atom projection) = RelationalSaturationObstruction sig

type instance SatRawMatch (RelationalFrontSaturation sig atom projection) = RelationalSaturationMatch sig atom
type instance SatRawMatchRejection (RelationalFrontSaturation sig atom projection) = Void

type instance SatRequestMatch (RelationalFrontSaturation sig atom projection) = RelationalSaturationMatch sig atom

type instance SatMatchWorld (RelationalFrontSaturation sig atom projection) = ()

type instance SatMatchingRequest (RelationalFrontSaturation sig atom projection) = ()

type instance SatMatch (RelationalFrontSaturation sig atom projection) = RelationalSaturationMatch sig atom

type instance SatSupportedMatch (RelationalFrontSaturation sig atom projection) = RelationalSaturationSupportedMatch sig atom

type instance SatSupportWitness (RelationalFrontSaturation sig atom projection) = ()

type instance SatMatchState (RelationalFrontSaturation sig atom projection) = RelationalSaturationMatchState sig projection

type instance SatMatchStrategy (RelationalFrontSaturation sig atom projection) = ()

type instance SatRebuild (RelationalFrontSaturation sig atom projection) = RelationalSaturationRebuild

type instance SatApplicationError (RelationalFrontSaturation sig atom projection) = RelationalProgramError sig

type instance SatApplicationResult (RelationalFrontSaturation sig atom projection) = RelationalSaturationApplicationResult

instance (RewriteSignature sig, Ord (NodeTag sig)) => SaturationGraph (RelationalFrontSaturation sig atom projection) where
  graphCanonicalizeClass classId graph =
    maybe classId id (hostCanonicalClass (rscLiveHost graph) classId)

  graphClassCount =
    hostClassCount . rscLiveHost

  graphNodeCount =
    hostNodeCount . rscLiveHost

  graphBase =
    rscBaseHost

  baseGraphEquals =
    hostStateEquals

  graphPreparedSite =
    rscPreparedSite

  graphContextLattice =
    rscContextLattice

  graphPendingMerges _graph =
    0

  graphConvergenceStateEquals leftGraph rightGraph =
    hostStateEquals (rscLiveHost leftGraph) (rscLiveHost rightGraph)

  graphContextClassProjection contextValue graph =
    Right
      ( IntMap.fromList
          [ (classIdKey classId, canonicalClass)
          | (classId, _nodes) <- hostNodeClasses (hostForContext contextValue graph),
            Just canonicalClass <- [hostCanonicalClass (hostForContext contextValue graph) classId]
          ]
      )

  graphContextClasses contextValue graph =
    Right
      ( Set.fromList
          [ classId
          | (classId, _nodes) <- hostNodeClasses (hostForContext contextValue graph)
          ]
      )

instance (RewriteSignature sig, Ord (NodeTag sig)) => CapabilitySystem (RelationalFrontSaturation sig atom projection) where
  emptyCapabilityResolver =
    ()

instance (RewriteSignature sig, Ord (NodeTag sig)) => QueryIndex (RelationalFrontSaturation sig atom projection) where
  queryFingerprint (RewriteRuleId ruleKey) =
    Right (QueryFingerprint ruleKey)

  matchSnapshotKey =
    id

  fullMatchingDelta =
    ()

  registerQueries _queries graph =
    Right graph

  contextMatchSections _graph =
    Map.empty

  lookupQueryId (QueryFingerprint queryKey) _graph =
    Just (mkQueryId queryKey)

instance (RewriteSignature sig, Ord (NodeTag sig)) => FactSystem (RelationalFrontSaturation sig atom projection) where
  type SatFactRuleIdentity (RelationalFrontSaturation sig atom projection) = Void

  emptyFactStore =
    ()

  emptyFactIndex =
    ()

  canonicalizeFactStore _graph =
    id

  canonicalizeFactIndex _graph =
    id

  canonicalizeFactStoreBase _baseGraph =
    id

  canonicalizeFactIndexBase _baseGraph =
    id

  canonicalizeFactStoreAtContext _contextValue _graph =
    Right

  canonicalizeFactIndexAtContext _contextValue _graph =
    Right

  unionFactStores _left _right =
    ()

  factChangeMatchingDelta _graph _before _after =
    ()

  compileFactRules =
    traverse absurd

  factRuleQuery =
    absurd

  factRuleId =
    absurd

  factRuleIdentity =
    absurd

  factSourceId =
    absurd

  deriveFactClosure _resolver _initialFacts compiledFactRules _baseGraph _facts _index =
    (,,) () () <$> traverse absurd compiledFactRules

  deriveFactClosureAtContext _resolver _initialFacts compiledFactRules _graph _contextValue _facts _index =
    (,,) () () <$> traverse absurd compiledFactRules

instance (RewriteSignature sig, Ord (NodeTag sig)) => RewriteSystem (RelationalFrontSaturation sig atom projection) where
  type SatRewriteRuleIdentity (RelationalFrontSaturation sig atom projection) =
    RelationalSaturationRuleIdentity sig atom

  compileRewriteRules =
    Right

  rewriteRuleSourceId =
    rsrRuleId

  rewriteRuleId =
    rsrRuleId

  rewriteRuleIdentity =
    Right . rsrRulePlan

  rewriteRuleKey =
    rsrRuleId

  rewriteRuleQuery =
    rsrRuleId

  defaultRewriteContext =
    defaultSaturationConfig

  rewriteCapabilityResolver _rewriteContext _graph =
    ()

instance (RewriteSignature sig, Ord (NodeTag sig)) => MatchView (RelationalFrontSaturation sig atom projection) where
  matchKey matchValue =
    ( rsrRuleId (rsmRule matchValue),
      rrmRoot (rsmMatch matchValue),
      relationalSaturationMatchSubstitution matchValue
    )

  matchRuleKey =
    rsrRuleId . rsmRule

  supportedMatchInner =
    rssmMatch

  setSupportedMatchInner matchValue supportedMatch =
    supportedMatch {rssmMatch = matchValue}

  supportedMatchBasis =
    rssmSupport

  supportedMatchWitnesses =
    rssmWitnesses

  mergeSupportedMatch graph leftMatch rightMatch =
    fmap
      ( \mergedSupport ->
          leftMatch
            { rssmSupport = mergedSupport,
              rssmWitnesses = Map.union (rssmWitnesses leftMatch) (rssmWitnesses rightMatch)
            }
      )
      ( first
          RelationalSaturationPreparedSupportFailed
          (unionPreparedSupport (graphPreparedSite @(RelationalFrontSaturation sig atom projection) graph) (rssmSupport leftMatch) (rssmSupport rightMatch))
      )

instance (RewriteSignature sig, Ord (NodeTag sig)) => MatchingBackend (RelationalFrontSaturation sig atom projection) where
  initialMatchState _matchingStrategy _rewriteContext =
    emptyRelationalSaturationMatchState

  runMatchingRequests _matchingDelta _matchWorld requests state =
    ( state,
      if null requests
        then Right []
        else Left RelationalSaturationUnsupportedMatchingRequest
    )

  materializeRawMatch _rewriteContext _resolver contextValue _facts _derivations _baseGraph rawMatch =
    Right (supportMatchAt contextValue rawMatch)

  materializeRawMatchesAtContextView _rewriteContext _resolver contextValue _facts _derivations _graph =
    Right . fmap (supportMatchAt contextValue)

  rawBaseMatchesPrepared rewriteContext iterationIndex _matchingDelta graph _facts rewriteRules state =
    fmap
      ( \(matches, stats) ->
          ( recordCollectedMatches iterationIndex (length matches) stats state,
            matches
          )
      )
      (collectRuleMatchesBase rewriteContext (rscLiveHost graph) rewriteRules)

  rawContextMatchesPrepared rewriteContext contextValue iterationIndex _matchingDelta graph _facts _derivations rewriteRules state =
    fmap
      ( \(matches, stats) ->
          ( recordCollectedMatches iterationIndex (length matches) stats state,
            matches
          )
      )
      (collectRuleMatchesContext rewriteContext contextValue graph rewriteRules)

  consumedDerivations _supportedMatch =
    ()

  rawMatchRuleKey =
    rsrRuleId . rsmRule

  filterSupportedMatches _rewriteContext _factStore _matchState matches _graph =
    matches

  advanceMatchStateForRound _matchingDelta _graph state =
    state {rsmsPendingRound = Nothing}

  advanceMatchStateAfterRebuild _rebuild =
    id

  recordScheduledMatches scheduledMatches state =
    case rsmsPendingRound state of
      Nothing ->
        state
      Just pendingRound
        | null scheduledMatches ->
            commitPendingRound [] emptyRewriteRunStats state
        | otherwise ->
            state {rsmsPendingRound = Just pendingRound}

  recordApplicationResult _graph applicationResult state =
    commitPendingRound
      (rsarExecuted applicationResult)
      (rsarStats applicationResult)
      state

instance (RewriteSignature sig, Ord (NodeTag sig)) => ApplicationResultSystem (RelationalFrontSaturation sig atom projection) where
  applicationResultCount =
    length . rsarExecuted

instance (RewriteSignature sig, Ord (NodeTag sig)) => RebuildSystem (RelationalFrontSaturation sig atom projection) where
  rebuildGraph graph _facts _derivations =
    Right (graph, RelationalSaturationRebuild (hostRevision (rscLiveHost graph)))

  rebuildEpoch =
    rsrEpoch

  rebuildMatchingDelta _rebuild =
    ()

  factViewGraphChanges _summary =
    FactViewGraphChanges
      { fvgcBaseChanged = True,
        fvgcChangedFiberAuthors = Set.empty
      }

  postApplyMatchingDelta _matchState _scheduledMatches _applicationResult _rebuild =
    ()

  postApplyChangeSummary _matchState _scheduledMatches _applicationResult _rebuild =
    ()

hostForContext :: RelationalSaturationContext -> RelationalSaturationCarrier sig atom -> Host sig
hostForContext contextValue graph =
  case contextValue of
    RelationalBaseContext ->
      rscBaseHost graph
    RelationalNamedContext {} ->
      rscLiveHost graph

hostStateEquals :: (RewriteSignature sig, Ord (NodeTag sig)) => Host sig -> Host sig -> Bool
hostStateEquals leftHost rightHost =
  hostRevision leftHost == hostRevision rightHost
    && hostNodeClasses leftHost == hostNodeClasses rightHost

collectRuleMatchesBase ::
  Ord (NodeTag sig) =>
  SaturationConfig sig ->
  Host sig ->
  [RelationalSaturationRule sig atom] ->
  Either (RelationalSaturationObstruction sig) ([RelationalSaturationMatch sig atom], RewriteRunStats)
collectRuleMatchesBase config host rewriteRules =
  collectRuleMatchesWith
    (\ruleNameValue prepared -> runMatchRule (scRunConfig config) ruleNameValue prepared)
    host
    (baseSaturationRuleSupportIndex rewriteRules)
    rewriteRules

collectRuleMatchesContext ::
  Ord (NodeTag sig) =>
  SaturationConfig sig ->
  RelationalSaturationContext ->
  RelationalSaturationCarrier sig atom ->
  [RelationalSaturationRule sig atom] ->
  Either (RelationalSaturationObstruction sig) ([RelationalSaturationMatch sig atom], RewriteRunStats)
collectRuleMatchesContext config contextValue graph rewriteRules =
  case contextValue of
    RelationalBaseContext ->
      collectRuleMatchesBase config (rscLiveHost graph) rewriteRules
    RelationalNamedContext contextName ->
      collectRuleMatchesWith
        ( \ruleNameValue prepared ->
            runMatchRuleWithContextHost
              (scRunConfig config)
              contextName
              (hostBackend (rscLiveHost graph))
              ruleNameValue
              prepared
        )
        (rscBaseHost graph)
        (contextSaturationRuleSupportIndex contextName rewriteRules)
        rewriteRules

collectRuleMatchesWith ::
  ( RuleName ->
    PreparedCache sig atom ->
    Either
      (RewriteRunError ContextName)
      (PreparedCache sig atom, RewriteRunResult ContextName [RawMatch])
  ) ->
  Host sig ->
  RuleSupportIndex ContextName ->
  [RelationalSaturationRule sig atom] ->
  Either (RelationalSaturationObstruction sig) ([RelationalSaturationMatch sig atom], RewriteRunStats)
collectRuleMatchesWith runRuleMatch preparedHost supportIndex rewriteRules =
  fmap projectCollected $
    foldlM
      collectRule
      (prepared0, [], emptyRewriteRunStats)
      rewriteRules
  where
    prepared0 =
      prepareRelationalSystem
        (hostBackend preparedHost)
        supportIndex
        (RelationalPlanSet (Map.fromList [(rsrRuleName ruleValue, rsrPlan ruleValue) | ruleValue <- rewriteRules]))

    collectRule (prepared, matchChunks, stats) ruleValue = do
      (prepared', result) <-
        first
          RelationalSaturationRunFailed
          (runRuleMatch (rsrRuleName ruleValue) prepared)
      Right
        ( prepared',
          fmap (RelationalSaturationMatch ruleValue) (rrrValue result) : matchChunks,
          appendRewriteRunStats stats (rrrStats result)
        )

    projectCollected ::
      (prepared, [[RelationalSaturationMatch sig atom]], RewriteRunStats) ->
      ([RelationalSaturationMatch sig atom], RewriteRunStats)
    projectCollected (_prepared, matchChunks, stats) =
      (concat (reverse matchChunks), stats)

baseSaturationRuleSupportIndex :: [RelationalSaturationRule sig atom] -> RuleSupportIndex ContextName
baseSaturationRuleSupportIndex =
  baseRuleSupportIndex . saturationRuleNames

contextSaturationRuleSupportIndex :: ContextName -> [RelationalSaturationRule sig atom] -> RuleSupportIndex ContextName
contextSaturationRuleSupportIndex contextName =
  contextRuleSupportIndex contextName . saturationRuleNames

saturationRuleNames :: [RelationalSaturationRule sig atom] -> Set.Set RuleName
saturationRuleNames =
  Set.fromList . fmap rsrRuleName

hostNodeCount :: Host sig -> Int
hostNodeCount =
  sum . fmap (length . snd) . hostNodeClasses

supportMatchAt :: RelationalSaturationContext -> RelationalSaturationMatch sig atom -> RelationalSaturationSupportedMatch sig atom
supportMatchAt contextValue matchValue =
  RelationalSaturationSupportedMatch
    { rssmMatch = matchValue,
      rssmSupport = principalSupport contextValue,
      rssmWitnesses = Map.singleton contextValue ()
    }

recordCollectedMatches ::
  Int ->
  Int ->
  RewriteRunStats ->
  RelationalSaturationMatchState sig projection ->
  RelationalSaturationMatchState sig projection
recordCollectedMatches iterationIndex matchCount stats state =
  state
    { rsmsPendingRound =
        Just
          ( case rsmsPendingRound state of
              Nothing ->
                RelationalSaturationPendingRound
                  { rspRoundIndex = iterationIndex,
                    rspMatchedCount = matchCount,
                    rspMatchStats = stats
                  }
              Just pending ->
                pending
                  { rspMatchedCount = rspMatchedCount pending + matchCount,
                    rspMatchStats = appendRewriteRunStats (rspMatchStats pending) stats
                  }
          )
    }

commitPendingRound ::
  [ExecutedRewrite] ->
  RewriteRunStats ->
  RelationalSaturationMatchState sig projection ->
  RelationalSaturationMatchState sig projection
commitPendingRound executed applicationStats state =
  case rsmsPendingRound state of
    Nothing ->
      state
    Just pending ->
      let roundStats =
            appendRewriteRunStats
              (statsForRounds 1)
              (appendRewriteRunStats (rspMatchStats pending) applicationStats)
          roundValue =
            SaturationRound
              { saturationRoundIndex = rspRoundIndex pending,
                saturationRoundMatches = rspMatchedCount pending,
                saturationRoundExecuted = executed,
                saturationRoundStats = roundStats
              }
       in state
            { rsmsPendingRound = Nothing,
              rsmsRounds = roundValue : rsmsRounds state,
              rsmsStats = appendRewriteRunStats (rsmsStats state) roundStats
            }

relationalSaturationMatchSubstitution :: RelationalSaturationMatch sig atom -> Substitution
relationalSaturationMatchSubstitution =
  rawMatchSubstitution . rsmMatch

relationalSaturationMatchScheduleKey :: RelationalSaturationMatch sig atom -> RewriteScheduleKey
relationalSaturationMatchScheduleKey matchValue =
  ( rsrRuleId (rsmRule matchValue),
    rrmRoot (rsmMatch matchValue),
    relationalSaturationMatchSubstitution matchValue
  )

relationalSaturationSupportedScheduleKey :: RelationalSaturationSupportedMatch sig atom -> RewriteScheduleKey
relationalSaturationSupportedScheduleKey =
  relationalSaturationMatchScheduleKey . rssmMatch
