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
import Data.IntSet qualified as IntSet
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
    Substitution,
    classIdKey,
    mkQueryId,
  )
import Moonlight.Rewrite.Runtime
  ( ExecutedRewrite,
    RulePlan,
  )
import Moonlight.Rewrite.DSL
  ( ContextName,
    Node,
    NodeTag,
    RewriteGuardAtom (..),
    RewriteSignature,
  )
import Moonlight.Rewrite.Relational
  ( RelationalPlanSet (..),
    RelationalRewriteMatch (..),
    RewriteRunError,
    RewriteRunResult (..),
    RewriteRunStats,
    appendRewriteRunStats,
    emptyRewriteRunStats,
    evictRelationalSystemContext,
    invalidateRelationalSystemHost,
    prepareRelationalSystem,
    runMatchRule,
    runMatchRuleWithContextHost,
    statsForRounds,
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
import Moonlight.Rewrite.System
  ( CompiledGuard,
    RuleSupportIndex,
    RuleName,
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

type RelationalFrontSaturation :: Type -> (Symbol -> (Symbol -> Type) -> Type) -> RewriteGuardAtomKind -> Type -> Type
data RelationalFrontSaturation owner sig atom projection

type instance SatGraph (RelationalFrontSaturation owner sig atom projection) = RelationalSaturationCarrier owner sig atom

type instance SatBaseGraph (RelationalFrontSaturation owner sig atom projection) = Host sig

type instance SatClassId (RelationalFrontSaturation owner sig atom projection) = ClassId

type instance SatContext (RelationalFrontSaturation owner sig atom projection) = RelationalSaturationContext

type instance SatContextOwner (RelationalFrontSaturation owner sig atom projection) = owner

type instance SatObstruction (RelationalFrontSaturation owner sig atom projection) = RelationalSaturationObstruction sig

type instance SatCapabilityResolver (RelationalFrontSaturation owner sig atom projection) = ()

type instance SatFactStore (RelationalFrontSaturation owner sig atom projection) = ()

type instance SatFactIndex (RelationalFrontSaturation owner sig atom projection) = ()

type instance SatFactSource (RelationalFrontSaturation owner sig atom projection) = Void

type instance SatFactRule (RelationalFrontSaturation owner sig atom projection) = Void

type instance SatFactCompileError (RelationalFrontSaturation owner sig atom projection) = Void

type instance SatFactRound (RelationalFrontSaturation owner sig atom projection) = ()

type instance SatQuery (RelationalFrontSaturation owner sig atom projection) = RewriteRuleId

type instance SatMatchSnapshot (RelationalFrontSaturation owner sig atom projection) = QueryFingerprint

type instance SatMatchSection (RelationalFrontSaturation owner sig atom projection) = ()

type instance SatMatchingDelta (RelationalFrontSaturation owner sig atom projection) = ()

type instance SatChangeSummary (RelationalFrontSaturation owner sig atom projection) = ()

type instance SatRuleSource (RelationalFrontSaturation owner sig atom projection) = RelationalSaturationRule sig atom

type instance SatRule (RelationalFrontSaturation owner sig atom projection) = RelationalSaturationRule sig atom

type instance SatRuleKey (RelationalFrontSaturation owner sig atom projection) = RewriteRuleId

type instance SatRewriteContext (RelationalFrontSaturation owner sig atom projection) = RelationalSaturationRuntimeContext sig atom

type instance SatRuleCompileError (RelationalFrontSaturation owner sig atom projection) = RelationalSaturationObstruction sig

type instance SatRawMatch (RelationalFrontSaturation owner sig atom projection) = RelationalSaturationMatch sig atom
type instance SatRawMatchRejection (RelationalFrontSaturation owner sig atom projection) = Void

type instance SatRequestMatch (RelationalFrontSaturation owner sig atom projection) = RelationalSaturationMatch sig atom

type instance SatMatchWorld (RelationalFrontSaturation owner sig atom projection) = ()

type instance SatMatchingRequest (RelationalFrontSaturation owner sig atom projection) = ()

type instance SatMatch (RelationalFrontSaturation owner sig atom projection) = RelationalSaturationMatch sig atom

type instance SatSupportedMatch (RelationalFrontSaturation owner sig atom projection) = RelationalSaturationSupportedMatch sig atom

type instance SatSupportWitness (RelationalFrontSaturation owner sig atom projection) = ()

type instance SatMatchState (RelationalFrontSaturation owner sig atom projection) = RelationalSaturationMatchState sig atom projection

type instance SatMatchStrategy (RelationalFrontSaturation owner sig atom projection) = ()

type instance SatRebuild (RelationalFrontSaturation owner sig atom projection) = RelationalSaturationRebuild sig

type instance SatApplicationError (RelationalFrontSaturation owner sig atom projection) = RelationalProgramError sig

type instance SatApplicationResult (RelationalFrontSaturation owner sig atom projection) = RelationalSaturationApplicationResult

instance (RewriteSignature sig, Ord (NodeTag sig)) => SaturationGraph (RelationalFrontSaturation owner sig atom projection) where
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

instance (RewriteSignature sig, Ord (NodeTag sig)) => CapabilitySystem (RelationalFrontSaturation owner sig atom projection) where
  emptyCapabilityResolver =
    ()

instance (RewriteSignature sig, Ord (NodeTag sig)) => QueryIndex (RelationalFrontSaturation owner sig atom projection) where
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

instance (RewriteSignature sig, Ord (NodeTag sig)) => FactSystem (RelationalFrontSaturation owner sig atom projection) where
  type SatFactRuleIdentity (RelationalFrontSaturation owner sig atom projection) = Void

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

instance (RewriteSignature sig, Ord (NodeTag sig)) => RewriteSystem (RelationalFrontSaturation owner sig atom projection) where
  type SatRewriteRuleIdentity (RelationalFrontSaturation owner sig atom projection) =
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
    RelationalSaturationRuntimeContext
      { rsrcConfig = defaultSaturationConfig,
        rsrcInitialPreparedSystem = Nothing
      }

  rewriteCapabilityResolver _rewriteContext _graph =
    ()

instance (RewriteSignature sig, Ord (NodeTag sig)) => MatchView (RelationalFrontSaturation owner sig atom projection) where
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
          (unionPreparedSupport (graphPreparedSite @(RelationalFrontSaturation owner sig atom projection) graph) (rssmSupport leftMatch) (rssmSupport rightMatch))
      )

instance (RewriteSignature sig, Ord (NodeTag sig)) => MatchingBackend (RelationalFrontSaturation owner sig atom projection) where
  initialMatchState _matchingStrategy rewriteContext =
    emptyRelationalSaturationMatchState
      { rsmsPreparedSystem = rsrcInitialPreparedSystem rewriteContext
      }

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
      ( \(preparedSystem, matches, stats) ->
          ( recordCollectedMatches
              iterationIndex
              (length matches)
              stats
              state {rsmsPreparedSystem = Just preparedSystem},
            matches
          )
      )
      ( collectRuleMatchesBase
          (rsrcConfig rewriteContext)
          (saturationPreparedSystemBase graph rewriteRules state)
          rewriteRules
      )

  rawContextMatchesPrepared rewriteContext contextValue iterationIndex _matchingDelta graph _facts _derivations rewriteRules state =
    fmap
      ( \(preparedSystem, matches, stats) ->
          ( recordCollectedMatches
              iterationIndex
              (length matches)
              stats
              state {rsmsPreparedSystem = Just preparedSystem},
            matches
          )
      )
      ( collectRuleMatchesContext
          (rsrcConfig rewriteContext)
          contextValue
          graph
          (saturationPreparedSystemContext contextValue graph rewriteRules state)
          rewriteRules
      )

  consumedDerivations _supportedMatch =
    ()

  rawMatchRuleKey =
    rsrRuleId . rsmRule

  filterSupportedMatches _rewriteContext _factStore _matchState matches _graph =
    matches

  advanceMatchStateForRound _matchingDelta _graph state =
    state {rsmsPendingRound = Nothing}

  advanceMatchStateAfterRebuild =
    advanceRelationalSaturationPreparedSystem

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

instance (RewriteSignature sig, Ord (NodeTag sig)) => ApplicationResultSystem (RelationalFrontSaturation owner sig atom projection) where
  applicationResultCount =
    length . rsarExecuted

instance (RewriteSignature sig, Ord (NodeTag sig)) => RebuildSystem (RelationalFrontSaturation owner sig atom projection) where
  rebuildGraph graph _facts _derivations =
    Right
      ( graph,
        RelationalSaturationRebuild
          { rsrEpoch = hostRevision (rscLiveHost graph),
            rsrActiveContext = rscActiveContext graph,
            rsrRebuiltHost = rscLiveHost graph,
            rsrDirtyResults = IntSet.empty
          }
      )

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

advanceRelationalSaturationPreparedSystem ::
  RelationalSaturationRebuild sig ->
  RelationalSaturationMatchState sig atom projection ->
  RelationalSaturationMatchState sig atom projection
advanceRelationalSaturationPreparedSystem rebuild state =
  state
    { rsmsPreparedSystem =
        fmap
          advancePreparedSystem
          (rsmsPreparedSystem state)
    }
  where
    advancePreparedSystem =
      case rsrActiveContext rebuild of
        RelationalBaseContext ->
          if IntSet.null (rsrDirtyResults rebuild)
            then id
            else
              invalidateRelationalSystemHost
                (hostBackend (rsrRebuiltHost rebuild))

        RelationalNamedContext contextName
          | IntSet.null (rsrDirtyResults rebuild) ->
              id
          | otherwise ->
              evictRelationalSystemContext contextName

hostForContext :: RelationalSaturationContext -> RelationalSaturationCarrier owner sig atom -> Host sig
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
  PreparedCache sig atom ->
  [RelationalSaturationRule sig atom] ->
  Either
    (RelationalSaturationObstruction sig)
    (PreparedCache sig atom, [RelationalSaturationMatch sig atom], RewriteRunStats)
collectRuleMatchesBase config preparedSystem rewriteRules =
  collectRuleMatchesWith
    (\ruleNameValue prepared -> runMatchRule (scRunConfig config) ruleNameValue prepared)
    preparedSystem
    rewriteRules

collectRuleMatchesContext ::
  Ord (NodeTag sig) =>
  SaturationConfig sig ->
  RelationalSaturationContext ->
  RelationalSaturationCarrier owner sig atom ->
  PreparedCache sig atom ->
  [RelationalSaturationRule sig atom] ->
  Either
    (RelationalSaturationObstruction sig)
    (PreparedCache sig atom, [RelationalSaturationMatch sig atom], RewriteRunStats)
collectRuleMatchesContext config contextValue graph preparedSystem rewriteRules =
  case contextValue of
    RelationalBaseContext ->
      collectRuleMatchesBase config preparedSystem rewriteRules
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
        preparedSystem
        rewriteRules

collectRuleMatchesWith ::
  ( RuleName ->
    PreparedCache sig atom ->
    Either
      (RewriteRunError ContextName)
      (PreparedCache sig atom, RewriteRunResult ContextName [RawMatch])
  ) ->
  PreparedCache sig atom ->
  [RelationalSaturationRule sig atom] ->
  Either
    (RelationalSaturationObstruction sig)
    (PreparedCache sig atom, [RelationalSaturationMatch sig atom], RewriteRunStats)
collectRuleMatchesWith runRuleMatch prepared0 rewriteRules =
  fmap projectCollected $
    foldlM
      collectRule
      (prepared0, [], emptyRewriteRunStats)
      rewriteRules
  where
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
      (PreparedCache sig atom, [[RelationalSaturationMatch sig atom]], RewriteRunStats) ->
      (PreparedCache sig atom, [RelationalSaturationMatch sig atom], RewriteRunStats)
    projectCollected (prepared, matchChunks, stats) =
      (prepared, concat (reverse matchChunks), stats)

saturationPreparedSystemBase ::
  RelationalSaturationCarrier owner sig atom ->
  [RelationalSaturationRule sig atom] ->
  RelationalSaturationMatchState sig atom projection ->
  PreparedCache sig atom
saturationPreparedSystemBase graph rewriteRules state =
  case rsmsPreparedSystem state of
    Just preparedSystem ->
      preparedSystem

    Nothing ->
      prepareRelationalSystem
        (hostBackend (rscLiveHost graph))
        (baseSaturationRuleSupportIndex rewriteRules)
        (saturationRelationalPlanSet rewriteRules)

saturationPreparedSystemContext ::
  RelationalSaturationContext ->
  RelationalSaturationCarrier owner sig atom ->
  [RelationalSaturationRule sig atom] ->
  RelationalSaturationMatchState sig atom projection ->
  PreparedCache sig atom
saturationPreparedSystemContext contextValue graph rewriteRules state =
  case rsmsPreparedSystem state of
    Just preparedSystem ->
      preparedSystem

    Nothing ->
      prepareRelationalSystem
        (hostBackend (rscBaseHost graph))
        supportIndex
        (saturationRelationalPlanSet rewriteRules)
  where
    supportIndex =
      case contextValue of
        RelationalBaseContext ->
          baseSaturationRuleSupportIndex rewriteRules

        RelationalNamedContext contextName ->
          contextSaturationRuleSupportIndex contextName rewriteRules

saturationRelationalPlanSet ::
  [RelationalSaturationRule sig atom] ->
  RelationalPlanSet
    (RulePlan (CompiledGuard (GuardCapabilityKey atom) (Node sig)) (Node sig))
    MatchVar
    ClassId
    (CompiledGuard (GuardCapabilityKey atom) (Node sig))
    (NodeTag sig)
    (Node sig ClassId)
saturationRelationalPlanSet =
  RelationalPlanSet
    . Map.fromList
    . fmap (\ruleValue -> (rsrRuleName ruleValue, rsrPlan ruleValue))

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
  RelationalSaturationMatchState sig atom projection ->
  RelationalSaturationMatchState sig atom projection
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
  RelationalSaturationMatchState sig atom projection ->
  RelationalSaturationMatchState sig atom projection
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
