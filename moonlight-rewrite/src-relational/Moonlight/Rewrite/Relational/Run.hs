{-# LANGUAGE GHC2024 #-}

module Moonlight.Rewrite.Relational.Run
  ( RewriteRestriction (..),
    RewriteContext (..),
    RewriteRunConfig (..),
    defaultRewriteRunConfig,
    RewritePreparedOp (..),
    RewriteRunError (..),
    RewriteRunResult (..),
    RelationalPreparedSystem,
    prepareRelationalSystem,
    advanceRelationalSystemHost,
    invalidateRelationalSystemHost,
    evictRelationalSystemContext,
    preparedRelationalSystemSupport,
    preparedRelationalSystemRevision,
    preparedRelationalSystemCachedBaseRevisions,
    runRewrite,
    runMatchRule,
    runMatchRuleWithContextHost,
    runRuleSupport,
    runExistsMatch,
  )
where

import Data.Bifunctor (first)
import Data.HashSet qualified as HashSet
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Constraint, Type)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core
  ( DenseKey,
    QuerySnapshot (..),
    emptyFootprint,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta,
    dropEmptyRowDeltas,
  )
import Moonlight.Differential.Row.Patch
  ( composePlainRowPatch
  )
import Moonlight.Flow.Execution.Engine
  ( EngineTelemetry,
    FactorCacheKey (..),
    RelationalEngineState (..),
    RelationalRequest (..),
    RelationalResult (..),
    RelationalRunObstruction (..),
    emptyRelationalEngineState,
    runRelational,
  )
import Moonlight.Flow.Execution.Factor.Run
  ( FactorRunError,
  )
import Moonlight.Flow.Execution.Observe.Provenance.Types
  ( ProvenanceObstruction,
  )
import Moonlight.Flow.Execution.Prepared.Run
  ( PreparedOp (..),
    supportIds,
  )
import Moonlight.Differential.Row.Tuple
  ( RepKey (..),
  )
import Moonlight.Flow.Execution.Prepared.Cache
  ( PreparedCacheEntry (..),
    PreparedCacheKey (..),
    advanceJoinCacheStateWith,
    contextKeysOnly,
    evictPreparedKeys,
    jcsByResult,
    jcsPrepared,
    lookupAffected,
  )
import Moonlight.Flow.Execution.Prepared.Backend
  ( PreparedBackend (pbPatchBase),
  )
import Moonlight.Flow.Execution.Prepared.Base
  ( BuildBasePreparedDBError,
  )
import Moonlight.Flow.Execution.Prepared.Request
  ( PreparedExecutionKey (..),
    PreparedRequestView (..),
    frontierRestriction,
    preparedExecutionKey,
  )
import Moonlight.Flow.Model.Scope
  ( relationalScopeDelta,
    relationalScopeFromSets,
  )
import Moonlight.Flow.Plan.Query.Core
  ( OutputProjectionObstruction,
    qpId,
    projectQueryPlanOutputs,
  )
import Moonlight.Flow.Storage.Relation
  ( Relation,
  )
import Moonlight.Flow.Storage.Restriction
  ( Restriction,
    emptyRestriction,
    restrictionFromSlots,
  )
import Moonlight.Flow.Storage.View
  ( SupportIds,
  )
import Moonlight.Rewrite.Relational.Backend
  ( RewriteRelationalBackend,
    RewriteRelationalHost,
    RewriteRelationalPatch (..),
    RewriteRelationalPreparedObstruction (..),
    rewriteBasePreparedRevision,
    rewriteRelationalHostPreparedRelationsForPlan,
    rewritePreparedBackend,
    rewriteRelationalHostRevision,
  )
import Moonlight.Rewrite.Relational.Compile
  ( RelationalPlanSet (..),
    RewritePlan,
  )
import Moonlight.Rewrite.Relational.Limits
  ( RewriteRunLimit,
    RewriteRunLimits,
    RewriteRunStats,
    checkRewriteRunLimits,
    defaultRewriteRunLimits,
    limitToOverflowSentinel,
    rrmResultRows,
    statsForExists,
    statsForMatches,
    statsForSupport,
  )
import Moonlight.Rewrite.Relational.Output
  ( MatchKey,
    RelationalRewriteMatch,
    matchKeyAtomId,
    matchKeyPinnedRow,
  )
import Moonlight.Rewrite.System
  ( RuleName,
    RuleSupportIndex,
    baseSupportsRule,
    contextSupportsRule,
  )

type RewriteRestriction :: Type
data RewriteRestriction
  = RewriteUnrestricted
  | RewriteRootFrontier !IntSet
  | RewriteSlots !(IntMap.IntMap IntSet)
  deriving stock (Eq, Ord, Show, Read)

type RewriteContext :: Type -> Type -> Type -> Type
data RewriteContext context projection relation = RewriteContext
  { rcContextId :: !context,
    rcSnapshot :: !(QuerySnapshot projection relation)
  }

type RewriteRunConfig :: Type -> Type -> Type
data RewriteRunConfig context projection = RewriteRunConfig
  { rrcContext :: !(Maybe (RewriteContext context projection Relation)),
    rrcRestriction :: !RewriteRestriction,
    rrcLimits :: !RewriteRunLimits
  }

defaultRewriteRunConfig :: RewriteRunConfig context projection
defaultRewriteRunConfig =
  RewriteRunConfig
    { rrcContext = Nothing,
      rrcRestriction = RewriteUnrestricted,
      rrcLimits = defaultRewriteRunLimits
    }

type RewritePreparedOp :: Type -> Type -> Type -> Type
data RewritePreparedOp var key result where
  MatchRule ::
    RuleName ->
    RewritePreparedOp var key [RelationalRewriteMatch var key]

  RuleSupport ::
    RuleName ->
    RewritePreparedOp var key SupportIds

  ExistsMatch ::
    RuleName ->
    MatchKey ->
    RewritePreparedOp var key Bool

  ExistsRule ::
    RuleName ->
    RewritePreparedOp var key Bool

type RewriteRunError :: Type -> Type
data RewriteRunError context
  = RewriteRuleNotFound !RuleName
  | RewriteRuleUnsupported !context !RuleName
  | RewriteBaseRuleUnsupported !RuleName
  | RewritePreparedObstruction !RewriteRelationalPreparedObstruction
  | RewriteRelationalObstruction !ProvenanceObstruction
  | RewriteFactorRunObstruction !FactorRunError
  | RewriteOutputProjectionError !OutputProjectionObstruction
  | RewriteRunLimitExceeded !RewriteRunLimit !RewriteRunStats
  deriving stock (Show)

type RewriteRunResult :: Type -> Type -> Type
data RewriteRunResult context result = RewriteRunResult
  { rrrValue :: !result,
    rrrTelemetry :: !(EngineTelemetry context),
    rrrStats :: !RewriteRunStats
  }
  deriving stock (Show)

type RewriteEngineState ::
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type
type RewriteEngineState context compiled var key guard tag tuple =
  RelationalEngineState
    context
    (RewriteRelationalBackend compiled var key guard tag tuple)

type RelationalPreparedSystem ::
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type
data RelationalPreparedSystem context projection compiled var key guard tag tuple =
  RelationalPreparedSystem
    { rpsSupportIndex :: !(RuleSupportIndex context),
      rpsPlanSet :: !(RelationalPlanSet compiled var key guard tag tuple),
      rpsHost :: !(RewriteRelationalHost tag),
      rpsEngine :: !(RewriteEngineState context compiled var key guard tag tuple),
      rpsPendingPatches :: !(Map.Map (PreparedCacheKey context) RewriteRelationalPatch)
    }

prepareRelationalSystem ::
  RewriteRelationalHost tag ->
  RuleSupportIndex context ->
  RelationalPlanSet compiled var key guard tag tuple ->
  RelationalPreparedSystem context projection compiled var key guard tag tuple
prepareRelationalSystem host supportIndex planSet =
  RelationalPreparedSystem
    { rpsSupportIndex = supportIndex,
      rpsPlanSet = planSet,
      rpsHost = host,
      rpsEngine = emptyRelationalEngineState,
      rpsPendingPatches = Map.empty
    }

advanceRelationalSystemHost ::
  (Ord context, Ord tag) =>
  RewriteRelationalHost tag ->
  IntSet ->
  RelationalPreparedSystem context projection compiled var key guard tag tuple ->
  RelationalPreparedSystem context projection compiled var key guard tag tuple
advanceRelationalSystemHost repairHost dirtyResults system =
  system
    { rpsHost = repairHost,
      rpsEngine = engineWithRetainedFactors {resJoinCache = joinCache'},
      rpsPendingPatches = pendingPatches'
    }
  where
    engine0 =
      rpsEngine system
    affectedPreparedKeys =
      contextKeysOnly (lookupAffected (jcsByResult (resJoinCache engine0)) dirtyResults)
    (joinCache', patches) =
      advanceJoinCacheStateWith
        (\_payload joinCache -> contextKeysOnly (lookupAffected (jcsByResult joinCache) dirtyResults))
        (pbPatchBase rewritePreparedBackend)
        ( relationalScopeDelta
            (relationalScopeFromSets IntSet.empty IntSet.empty IntSet.empty dirtyResults IntSet.empty)
            Nothing
        )
        (rpsHost system)
        (Just repairHost)
        (resJoinCache engine0)
    patchedPreparedKeys =
      Map.keysSet patches
    evictedPreparedKeys =
      Set.union
        patchedPreparedKeys
        ( Set.union
            affectedPreparedKeys
            (unpatchedBasePreparedKeys dirtyResults patchedPreparedKeys engine0)
        )
    engineWithRetainedFactors =
      evictFactorCachesForPreparedKeys evictedPreparedKeys engine0
    pendingPatches' =
      Map.unionWith
        composeRewriteRelationalPatch
        patches
        (rpsPendingPatches system)

-- | Replace the base host and invalidate its derived relational state.
invalidateRelationalSystemHost ::
  RewriteRelationalHost tag ->
  RelationalPreparedSystem context projection compiled var key guard tag tuple ->
  RelationalPreparedSystem context projection compiled var key guard tag tuple
invalidateRelationalSystemHost replacementHost system =
  system
    { rpsHost = replacementHost,
      rpsEngine = emptyRelationalEngineState,
      rpsPendingPatches = Map.empty
    }

evictRelationalSystemContext ::
  Ord context =>
  context ->
  RelationalPreparedSystem context projection compiled var key guard tag tuple ->
  RelationalPreparedSystem context projection compiled var key guard tag tuple
evictRelationalSystemContext contextValue system =
  system
    { rpsEngine =
        engineWithoutFactors
          { resJoinCache = evictPreparedKeys contextPreparedKeys (resJoinCache engineWithoutFactors)
          },
      rpsPendingPatches = Map.withoutKeys (rpsPendingPatches system) contextPreparedKeys
    }
  where
    engine =
      rpsEngine system
    contextPreparedKeys =
      Set.filter belongsToContext (Map.keysSet (jcsPrepared (resJoinCache engine)))
    engineWithoutFactors =
      evictFactorCachesForPreparedKeys contextPreparedKeys engine
    belongsToContext preparedKey =
      case preparedKey of
        ContextPreparedKey preparedContext _queryId _liveEpoch ->
          preparedContext == contextValue

        BasePreparedKey {} ->
          False

unpatchedBasePreparedKeys ::
  Ord context =>
  IntSet ->
  Set.Set (PreparedCacheKey context) ->
  RewriteEngineState context compiled var key guard tag tuple ->
  Set.Set (PreparedCacheKey context)
unpatchedBasePreparedKeys dirtyResults patchedPreparedKeys engine
  | IntSet.null dirtyResults =
      Set.empty
  | otherwise =
      Set.difference
        (Set.filter basePreparedKey (preparedKeysForFactorCaches engine))
        patchedPreparedKeys
{-# INLINE unpatchedBasePreparedKeys #-}

evictFactorCachesForPreparedKeys ::
  Ord context =>
  Set.Set (PreparedCacheKey context) ->
  RewriteEngineState context compiled var key guard tag tuple ->
  RewriteEngineState context compiled var key guard tag tuple
evictFactorCachesForPreparedKeys preparedKeys engine
  | Set.null preparedKeys =
      engine
  | otherwise =
      engine
        { resFactorCaches =
            Map.filterWithKey
              ( \factorKey _entry ->
                  not
                    ( Set.member
                        (preparedCacheKeyForExecutionKey (fckPreparedExecutionKey factorKey))
                        preparedKeys
                    )
              )
              (resFactorCaches engine)
        }
{-# INLINE evictFactorCachesForPreparedKeys #-}

preparedKeysForFactorCaches ::
  Ord context =>
  RewriteEngineState context compiled var key guard tag tuple ->
  Set.Set (PreparedCacheKey context)
preparedKeysForFactorCaches =
  Set.map (preparedCacheKeyForExecutionKey . fckPreparedExecutionKey)
    . Map.keysSet
    . resFactorCaches
{-# INLINE preparedKeysForFactorCaches #-}

basePreparedKey :: PreparedCacheKey context -> Bool
basePreparedKey preparedKey =
  case preparedKey of
    BasePreparedKey {} ->
      True

    ContextPreparedKey {} ->
      False
{-# INLINE basePreparedKey #-}

preparedCacheKeyForExecutionKey ::
  PreparedExecutionKey context ->
  PreparedCacheKey context
preparedCacheKeyForExecutionKey executionKey =
  case pekPreparedScope executionKey of
    Nothing ->
      BasePreparedKey (pekPlanKey executionKey)

    Just (contextValue, queryIdValue, liveEpochValue) ->
      ContextPreparedKey contextValue queryIdValue liveEpochValue
{-# INLINE preparedCacheKeyForExecutionKey #-}

composeRewriteRelationalPatch ::
  RewriteRelationalPatch ->
  RewriteRelationalPatch ->
  RewriteRelationalPatch
composeRewriteRelationalPatch newer older =
  RewriteRelationalPatch
    { rrpDirtyResults = IntSet.union (rrpDirtyResults newer) (rrpDirtyResults older),
      rrpAtomDeltas = composeAtomDeltas (rrpAtomDeltas newer) (rrpAtomDeltas older)
    }
{-# INLINE composeRewriteRelationalPatch #-}

composeAtomDeltas ::
  IntMap.IntMap RowDelta ->
  IntMap.IntMap RowDelta ->
  IntMap.IntMap RowDelta
composeAtomDeltas newer older =
  dropEmptyRowDeltas (IntMap.unionWith composePlainRowPatch newer older)
{-# INLINE composeAtomDeltas #-}

preparedRelationalSystemSupport ::
  RelationalPreparedSystem context projection compiled var key guard tag tuple ->
  RuleSupportIndex context
preparedRelationalSystemSupport =
  rpsSupportIndex

preparedRelationalSystemRevision ::
  RelationalPreparedSystem context projection compiled var key guard tag tuple ->
  Int
preparedRelationalSystemRevision =
  rewriteRelationalHostRevision . rpsHost

preparedRelationalSystemCachedBaseRevisions ::
  RelationalPreparedSystem context projection compiled var key guard tag tuple ->
  [Int]
preparedRelationalSystemCachedBaseRevisions =
  fmap rewriteBasePreparedRevision
    . Map.elems
    . Map.mapMaybe
      ( \entry ->
          case entry of
            BasePreparedEntry basePrepared _touchedAt ->
              Just basePrepared

            ContextPreparedEntry {} ->
              Nothing
      )
    . jcsPrepared
    . resJoinCache
    . rpsEngine
{-# INLINE preparedRelationalSystemCachedBaseRevisions #-}

type RewriteRelationalRunConstraints :: Type -> Type -> Type -> Type -> Type -> Constraint
type RewriteRelationalRunConstraints context var key tag tuple =
  ( Ord context,
    Ord var,
    Ord tag,
    DenseKey key
  )

runMatchRule ::
  RewriteRelationalRunConstraints context var key tag tuple =>
  RewriteRunConfig context projection ->
  RuleName ->
  RelationalPreparedSystem context projection compiled var key guard tag tuple ->
  Either
    (RewriteRunError context)
    ( RelationalPreparedSystem context projection compiled var key guard tag tuple,
      RewriteRunResult context [RelationalRewriteMatch var key]
    )
runMatchRule config = runRewrite config . MatchRule

runMatchRuleWithContextHost ::
  RewriteRelationalRunConstraints context var key tag tuple =>
  RewriteRunConfig context projection ->
  context ->
  RewriteRelationalHost tag ->
  RuleName ->
  RelationalPreparedSystem context projection compiled var key guard tag tuple ->
  Either
    (RewriteRunError context)
    ( RelationalPreparedSystem context projection compiled var key guard tag tuple,
      RewriteRunResult context [RelationalRewriteMatch var key]
    )
runMatchRuleWithContextHost config contextId contextHost ruleNameValue system =
  case Map.lookup ruleNameValue (rpsPlans (rpsPlanSet system)) of
    Nothing ->
      Left (RewriteRuleNotFound ruleNameValue)

    Just plan -> do
      snapshot <-
        first (RewritePreparedObstruction . RewritePreparedBuildObstruction) $
          contextSnapshotForPlan system plan contextHost
      let config' =
            config
              { rrcContext =
                  Just
                    RewriteContext
                      { rcContextId = contextId,
                        rcSnapshot = snapshot
                      }
              }
      requireSupported config' ruleNameValue (rpsSupportIndex system)
      runRelationalPrepared config' plan (PreparedRows (resultRowOverflowSentinel config'))
        (first RewriteOutputProjectionError . projectQueryPlanOutputs plan) statsForMatches system

runRuleSupport ::
  RewriteRelationalRunConstraints context var key tag tuple =>
  RewriteRunConfig context projection ->
  RuleName ->
  RelationalPreparedSystem context projection compiled var key guard tag tuple ->
  Either
    (RewriteRunError context)
    ( RelationalPreparedSystem context projection compiled var key guard tag tuple,
      RewriteRunResult context SupportIds
    )
runRuleSupport config = runRewrite config . RuleSupport

runExistsMatch ::
  RewriteRelationalRunConstraints context var key tag tuple =>
  RewriteRunConfig context projection ->
  RuleName ->
  MatchKey ->
  RelationalPreparedSystem context projection compiled var key guard tag tuple ->
  Either
    (RewriteRunError context)
    ( RelationalPreparedSystem context projection compiled var key guard tag tuple,
      RewriteRunResult context Bool
    )
runExistsMatch config ruleNameValue matchKey =
  runRewrite config (ExistsMatch ruleNameValue matchKey)

runRewrite ::
  RewriteRelationalRunConstraints context var key tag tuple =>
  RewriteRunConfig context projection ->
  RewritePreparedOp var key result ->
  RelationalPreparedSystem context projection compiled var key guard tag tuple ->
  Either
    (RewriteRunError context)
    ( RelationalPreparedSystem context projection compiled var key guard tag tuple,
      RewriteRunResult context result
    )
runRewrite config op system =
  case op of
    MatchRule ruleNameValue ->
      withRulePlan config ruleNameValue system $ \plan ->
        runRelationalPrepared config plan (PreparedRows (resultRowOverflowSentinel config))
          (first RewriteOutputProjectionError . projectQueryPlanOutputs plan) statsForMatches system

    RuleSupport ruleNameValue ->
      withRulePlan config ruleNameValue system $ \plan ->
        runRelationalPrepared config plan PreparedSupport (Right . supportIds) statsForSupport system

    ExistsMatch ruleNameValue matchKey ->
      withRulePlan config ruleNameValue system $ \plan ->
        runRelationalPrepared config plan
          (PreparedExistsPinned (matchKeyAtomId matchKey) (matchKeyPinnedRow matchKey))
          Right statsForExists system

    ExistsRule ruleNameValue ->
      withRulePlan config ruleNameValue system $ \plan ->
        runRelationalPrepared config plan PreparedExists Right statsForExists system

withRulePlan ::
  Ord context =>
  RewriteRunConfig context projection ->
  RuleName ->
  RelationalPreparedSystem context projection compiled var key guard tag tuple ->
  ( RewritePlan compiled var key guard tag tuple ->
    Either
      (RewriteRunError context)
      ( RelationalPreparedSystem context projection compiled var key guard tag tuple,
        RewriteRunResult context result
      )
  ) ->
  Either
    (RewriteRunError context)
    ( RelationalPreparedSystem context projection compiled var key guard tag tuple,
      RewriteRunResult context result
    )
withRulePlan config ruleNameValue system continue =
  case Map.lookup ruleNameValue (rpsPlans (rpsPlanSet system)) of
    Nothing ->
      Left (RewriteRuleNotFound ruleNameValue)

    Just plan -> do
      requireSupported config ruleNameValue (rpsSupportIndex system)
      continue plan

requireSupported ::
  Ord context =>
  RewriteRunConfig context projection ->
  RuleName ->
  RuleSupportIndex context ->
  Either (RewriteRunError context) ()
requireSupported config ruleNameValue supportIndex =
  case rrcContext config of
    Nothing
      | baseSupportsRule ruleNameValue supportIndex ->
          Right ()
      | otherwise ->
          Left (RewriteBaseRuleUnsupported ruleNameValue)

    Just rewriteContext
      | baseSupportsRule ruleNameValue supportIndex
          || contextSupportsRule (rcContextId rewriteContext) ruleNameValue supportIndex ->
          Right ()
      | otherwise ->
          Left (RewriteRuleUnsupported (rcContextId rewriteContext) ruleNameValue)

runRelationalPrepared ::
  RewriteRelationalRunConstraints context var key tag tuple =>
  RewriteRunConfig context projection ->
  RewritePlan compiled var key guard tag tuple ->
  PreparedOp raw ->
  (raw -> Either (RewriteRunError context) result) ->
  (result -> RewriteRunStats) ->
  RelationalPreparedSystem context projection compiled var key guard tag tuple ->
  Either
    (RewriteRunError context)
    ( RelationalPreparedSystem context projection compiled var key guard tag tuple,
      RewriteRunResult context result
    )
runRelationalPrepared config plan preparedOp projectRaw measureResult system =
  case runRelational rewritePreparedBackend request (rpsEngine system) of
    Left (RelationalPreparedObstruction obstruction) ->
      Left (RewritePreparedObstruction obstruction)

    Left (RelationalProvenanceObstruction obstruction) ->
      Left (RewriteRelationalObstruction obstruction)

    Left (RelationalFactorRunObstruction obstruction) ->
      Left (RewriteFactorRunObstruction obstruction)

    Right (engine', relationalResult) -> do
      projected <-
        projectRaw (relResultValue relationalResult)

      let stats =
            measureResult projected

      case checkRewriteRunLimits (rrcLimits config) stats of
        Left (limit, limitStats) ->
          Left (RewriteRunLimitExceeded limit limitStats)

        Right () ->
          Right
            ( system
                { rpsEngine = engine',
                  rpsPendingPatches = Map.delete preparedKey (rpsPendingPatches system)
                },
              RewriteRunResult
                { rrrValue = projected,
                  rrrTelemetry = relResultTelemetry relationalResult,
                  rrrStats = stats
                }
            )
  where
    request =
      RelationalRequest
        { relRequestPlan = plan,
          relRequestView = requestView,
          relRequestRestriction = lowerRewriteRestriction config plan,
          relRequestAtomDeltas =
            pendingAtomDeltasForRequest
              preparedKey
              system,
          relRequestOp = preparedOp
        }

    requestView =
      requestViewFor config system

    executionKey =
      preparedExecutionKey plan requestView

    preparedKey =
      preparedCacheKeyForExecutionKey executionKey

pendingAtomDeltasForRequest ::
  Ord context =>
  PreparedCacheKey context ->
  RelationalPreparedSystem context projection compiled var key guard tag tuple ->
  IntMap.IntMap RowDelta
pendingAtomDeltasForRequest preparedKey system =
  case Map.lookup preparedKey (rpsPendingPatches system) of
    Just patch ->
      rrpAtomDeltas patch

    _ ->
      IntMap.empty
{-# INLINE pendingAtomDeltasForRequest #-}

resultRowOverflowSentinel :: RewriteRunConfig context projection -> Maybe Int
resultRowOverflowSentinel =
  limitToOverflowSentinel . rrmResultRows . rrcLimits
{-# INLINE resultRowOverflowSentinel #-}

requestViewFor ::
  RewriteRunConfig context projection ->
  RelationalPreparedSystem context projection compiled var key guard tag tuple ->
  PreparedRequestView context (RewriteRelationalBackend compiled var key guard tag tuple) projection
requestViewFor config system =
  PreparedRequestView
    { prvHost = rpsHost system,
      prvContext =
        fmap
          (\rewriteContext -> (rcContextId rewriteContext, rcSnapshot rewriteContext))
          (rrcContext config)
    }

contextSnapshotForPlan ::
  RewriteRelationalRunConstraints context var key tag tuple =>
  RelationalPreparedSystem context projection compiled var key guard tag tuple ->
  RewritePlan compiled var key guard tag tuple ->
  RewriteRelationalHost tag ->
  Either BuildBasePreparedDBError (QuerySnapshot projection Relation)
contextSnapshotForPlan system plan contextHost =
  fmap
    ( \liveRelations ->
        QuerySnapshot
          { baseRevision = preparedRelationalSystemRevision system,
            queryId = qpId plan,
            liveEpoch = rewriteRelationalHostRevision contextHost,
            liveRelations = liveRelations,
            projection = IntMap.empty,
            footprint = emptyFootprint
          }
    )
    ( rewriteRelationalHostPreparedRelationsForPlan
        plan
        contextHost
    )

lowerRewriteRestriction ::
  RewriteRunConfig context projection ->
  RewritePlan compiled var key guard tag tuple ->
  Restriction
lowerRewriteRestriction config plan =
  case rrcRestriction config of
    RewriteUnrestricted ->
      emptyRestriction

    RewriteRootFrontier roots ->
      frontierRestriction plan (Just roots)

    RewriteSlots slotValues ->
      restrictionFromSlotValues slotValues
{-# INLINE lowerRewriteRestriction #-}

restrictionFromSlotValues :: IntMap.IntMap IntSet -> Restriction
restrictionFromSlotValues =
  restrictionFromSlots . fmap repKeySet
{-# INLINE restrictionFromSlotValues #-}

repKeySet :: IntSet -> HashSet.HashSet RepKey
repKeySet =
  HashSet.fromList . fmap RepKey . IntSet.toList
{-# INLINE repKeySet #-}
