{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Runtime.State
  ( RuntimeCore (..),
    FactViewKey (..),
    RuntimePlanIdentity (..),
    RuntimePlanFingerprint (..),
    RuntimeRuleIdentity (..),
    RuntimeCoreSnapshot (..),
    RuntimeState (..),
    RuntimeReportWindow (..),
    PlainRuntimeState,
    ProofRuntimeState,
    FactDerivationResult (..),
    runtimeCoreFactsAt,
    runtimeCoreFactInputsAt,
    runtimeCoreFactDerivationsAt,
    runtimeCoreFactsWithBase,
    runtimeCoreFactInputsWithBase,
    runtimeCoreFactDerivationsWithBase,
    runtimeCoreFactViewKeyAt,
    advanceRuntimeCoreFactViewGraphChanges,
    invalidateRuntimeCoreFactViews,
    DerivedFactArtifacts (..),
    EligibleMatchArtifacts (..),
    GuidedMatchArtifacts (..),
    ScheduledMatchArtifacts (..),
    RoundRebuildDelta (..),
    RoundArtifacts (..),
    roundArtifactsFrontierComplete,
    roundArtifactsNextMatchingDelta,
    roundViewFromParts,
    roundViewFromArtifacts,
    seedRuntimeCoreFacts,
    seedRuntimeStateFacts,
    initialRuntimeCore,
    initialRuntimeState,
    initialPlainRuntimeState,
    initialBasePlainRuntimeState,
    initialProofRuntimeState,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Vector (Vector)
import Numeric.Natural (Natural)
import Moonlight.Core (RewriteRuleId)
import Moonlight.Core
  ( MatchActivationIndex,
    SupportIndexedRule,
  )
import Moonlight.Saturation.Context.Program.View
  ( SaturationRoundView (..),
  )
import Moonlight.Saturation.Context.Runtime.Match.Batch
  ( MatchBatch,
    matchBatchLength,
  )
import Moonlight.Saturation.Context.Runtime.Match.Pipeline
  ( CandidatePipelineCounts,
  )
import Moonlight.Control.Diagnostics.Trace
  ( TraceLog,
    emptyTraceLog,
  )
import Moonlight.Control.Gate
  ( GuideRoundTrace,
  )
import Moonlight.Control.Schedule
  ( SchedulerConfig,
    TracePolicy,
  )
import Moonlight.Control.Schedule.Round
  ( ScheduleTrace,
    SchedulerState,
    emptySchedulerState,
  )
import Moonlight.Saturation.Matching
  ( QueryFingerprint,
  )
import Moonlight.Saturation.Substrate
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite,
    PreparedContextSupportError,
    preparedContextRestrictsTo,
  )

type RuntimeRuleIdentity :: Type -> Type
data RuntimeRuleIdentity ruleIdentity = RuntimeRuleIdentity
  { rriRuleId :: !RewriteRuleId,
    rriQueryFingerprint :: !QueryFingerprint,
    rriRuleIdentity :: !ruleIdentity
  }
  deriving stock (Eq)

type RuntimePlanFingerprint :: Type -> Type -> Type
data RuntimePlanFingerprint u schedulerGroup = RuntimePlanFingerprint
  { rpfMatchingStrategy :: !(SatMatchStrategy u),
    rpfSchedulerConfig :: !(SchedulerConfig schedulerGroup),
    rpfBaseFactRules :: ![RuntimeRuleIdentity (SatFactRuleIdentity u)],
    rpfContextFactRules :: !(Map (SatContext u) [RuntimeRuleIdentity (SatFactRuleIdentity u)]),
    rpfSupportedFactRules :: ![SupportIndexedRule (SupportBasis (SatContext u)) (RuntimeRuleIdentity (SatFactRuleIdentity u))],
    rpfBaseRewriteRules :: ![RuntimeRuleIdentity (SatRewriteRuleIdentity u)],
    rpfContextRewriteRules :: !(Map (SatContext u) [RuntimeRuleIdentity (SatRewriteRuleIdentity u)]),
    rpfSupportedRewriteRules :: !(Map RewriteRuleId (SupportIndexedRule (SupportBasis (SatContext u)) (RuntimeRuleIdentity (SatRewriteRuleIdentity u)))),
    rpfRewriteActivation :: !(MatchActivationIndex (SatContext u) RewriteRuleId),
    rpfBaseRewriteSupport :: !(Map RewriteRuleId (SupportBasis (SatContext u)))
  }

type RuntimePlanIdentity :: Type -> Type -> Type
newtype RuntimePlanIdentity u schedulerGroup = RuntimePlanIdentity
  { rpiPlanFingerprint :: RuntimePlanFingerprint u schedulerGroup
  }

type RuntimeCore :: Type -> Type -> Type
data RuntimeCore u schedulerGroup = RuntimeCore
  { rcIterationCount :: !Int,
    rcTotalMatches :: !Int,
    rcTrace :: !(TraceLog (SatRuleKey u) schedulerGroup),
    rcContextFactInputs :: !(Map (SatContext u) (SatFactStore u)),
    rcContextFacts :: !(Map (SatContext u) (SatFactStore u)),
    rcContextFactDerivations :: !(Map (SatContext u) (SatFactIndex u)),
    rcFactViewBaseGeneration :: !Natural,
    rcFactViewFiberGenerations :: !(Map (SatContext u) Natural),
    rcFactViewInputGenerations :: !(Map (SatContext u) Natural),
    rcCurrentFactRuleIdsByContext :: !(Map (SatContext u) [RewriteRuleId]),
    rcCurrentFactCapabilityGeneration :: !Natural,
    rcFactViewKeys :: !(Map (SatContext u) FactViewKey),
    rcFactRoundsByContext :: !(Map (SatContext u) (Seq (SatFactRound u))),
    rcFactRoundCount :: !Int,
    rcGuideTrace :: !(Seq GuideRoundTrace),
    rcGuideRoundCount :: !Int,
    rcMatchingDelta :: !(SatMatchingDelta u),
    rcChangeSummary :: !(SatChangeSummary u),
    rcContextRevision :: !Int,
    rcPlanIdentity :: !(Maybe (RuntimePlanIdentity u schedulerGroup))
  }

type FactViewKey :: Type
data FactViewKey = FactViewKey
  { fvkBaseGeneration :: !Natural,
    fvkFiberGeneration :: !Natural,
    fvkInputGeneration :: !Natural,
    fvkFactRuleIds :: ![RewriteRuleId],
    fvkCapabilityGeneration :: !Natural
  }
  deriving stock (Eq, Ord, Show)

type RuntimeCoreSnapshot :: Type -> Type -> Type
newtype RuntimeCoreSnapshot u schedulerGroup = RuntimeCoreSnapshot
  { unRuntimeCoreSnapshot :: RuntimeCore u schedulerGroup
  }

type RuntimeState :: Type -> Type -> Type -> Type
data RuntimeState u carrier schedulerGroup = RuntimeState
  { rsCore :: !(RuntimeCore u schedulerGroup),
    rsCarrier :: !carrier,
    rsMatchState :: !(SatMatchState u),
    rsScheduler :: !(SchedulerState schedulerGroup)
  }

type RuntimeReportWindow :: Type -> Type -> Type -> Type
data RuntimeReportWindow u carrier schedulerGroup = RuntimeReportWindow
  { rrwInitialState :: !(RuntimeState u carrier schedulerGroup),
    rrwFinalState :: !(RuntimeState u carrier schedulerGroup)
  }

type PlainRuntimeState :: Type -> Type
type PlainRuntimeState u =
  RuntimeState
    u
    (SatGraph u)
    (SatRuleKey u)

type ProofRuntimeState :: Type -> Type -> Type
type ProofRuntimeState u proofGraph =
  RuntimeState
    u
    proofGraph
    (SatRuleKey u)

type FactDerivationResult :: Type -> Type
data FactDerivationResult u = FactDerivationResult
  { fdrFactsByContext :: !(Map (SatContext u) (SatFactStore u)),
    fdrFactDerivationsByContext :: !(Map (SatContext u) (SatFactIndex u)),
    fdrFactViewKeysByContext :: !(Map (SatContext u) FactViewKey),
    fdrFactRoundsByContext :: !(Map (SatContext u) (Seq (SatFactRound u))),
    fdrFactRoundCount :: !Int
  }

runtimeCoreFactsAt ::
  forall u schedulerGroup.
  (FactSystem u, Ord (SatContext u)) =>
  SatContext u ->
  RuntimeCore u schedulerGroup ->
  SatFactStore u
runtimeCoreFactsAt contextValue core =
  Map.findWithDefault
    (emptyFactStore @u)
    contextValue
    (rcContextFacts core)
{-# INLINE runtimeCoreFactsAt #-}

runtimeCoreFactInputsAt ::
  forall u schedulerGroup.
  (FactSystem u, Ord (SatContext u)) =>
  SatContext u ->
  RuntimeCore u schedulerGroup ->
  SatFactStore u
runtimeCoreFactInputsAt contextValue core =
  Map.findWithDefault
    (emptyFactStore @u)
    contextValue
    (rcContextFactInputs core)
{-# INLINE runtimeCoreFactInputsAt #-}

runtimeCoreFactDerivationsAt ::
  forall u schedulerGroup.
  (FactSystem u, Ord (SatContext u)) =>
  SatContext u ->
  RuntimeCore u schedulerGroup ->
  SatFactIndex u
runtimeCoreFactDerivationsAt contextValue core =
  Map.findWithDefault
    (emptyFactIndex @u)
    contextValue
    (rcContextFactDerivations core)
{-# INLINE runtimeCoreFactDerivationsAt #-}

runtimeCoreFactsWithBase ::
  forall u schedulerGroup.
  (FactSystem u, Ord (SatContext u)) =>
  SatContext u ->
  RuntimeCore u schedulerGroup ->
  Map (SatContext u) (SatFactStore u)
runtimeCoreFactsWithBase baseContext core =
  Map.insert
    baseContext
    (runtimeCoreFactsAt @u baseContext core)
    (rcContextFacts core)
{-# INLINE runtimeCoreFactsWithBase #-}

runtimeCoreFactInputsWithBase ::
  forall u schedulerGroup.
  (FactSystem u, Ord (SatContext u)) =>
  SatContext u ->
  RuntimeCore u schedulerGroup ->
  Map (SatContext u) (SatFactStore u)
runtimeCoreFactInputsWithBase baseContext core =
  Map.insert
    baseContext
    (runtimeCoreFactInputsAt @u baseContext core)
    (rcContextFactInputs core)
{-# INLINE runtimeCoreFactInputsWithBase #-}

runtimeCoreFactDerivationsWithBase ::
  forall u schedulerGroup.
  (FactSystem u, Ord (SatContext u)) =>
  SatContext u ->
  RuntimeCore u schedulerGroup ->
  Map (SatContext u) (SatFactIndex u)
runtimeCoreFactDerivationsWithBase baseContext core =
  Map.insert
    baseContext
    (runtimeCoreFactDerivationsAt @u baseContext core)
    (rcContextFactDerivations core)
{-# INLINE runtimeCoreFactDerivationsWithBase #-}

runtimeCoreFactViewKeyAt ::
  forall u schedulerGroup.
  Ord (SatContext u) =>
  SatContext u ->
  [RewriteRuleId] ->
  Natural ->
  RuntimeCore u schedulerGroup ->
  FactViewKey
runtimeCoreFactViewKeyAt contextValue factRuleIds capabilityGeneration core =
  FactViewKey
    { fvkBaseGeneration = rcFactViewBaseGeneration core,
      fvkFiberGeneration =
        Map.findWithDefault
          0
          contextValue
          (rcFactViewFiberGenerations core),
      fvkInputGeneration =
        Map.findWithDefault
          0
          contextValue
          (rcFactViewInputGenerations core),
      fvkFactRuleIds = factRuleIds,
      fvkCapabilityGeneration = capabilityGeneration
    }
{-# INLINE runtimeCoreFactViewKeyAt #-}

advanceRuntimeCoreFactViewGraphChanges ::
  forall u schedulerGroup.
  Ord (SatContext u) =>
  PreparedContextSite (SatContext u) ->
  FactViewGraphChanges (SatContext u) ->
  RuntimeCore u schedulerGroup ->
  Either
    (PreparedContextSupportError (SatContext u))
    (RuntimeCore u schedulerGroup)
advanceRuntimeCoreFactViewGraphChanges site graphChanges core
  | fvgcBaseChanged graphChanges =
      Right
        core
          { rcContextFactDerivations = Map.empty,
            rcCurrentFactRuleIdsByContext = Map.empty,
            rcFactViewBaseGeneration = rcFactViewBaseGeneration core + 1
          }
  | Set.null (fvgcChangedFiberAuthors graphChanges) =
      Right core
  | otherwise = do
      visibilityByContext <-
        Map.traverseWithKey
          ( \contextValue _cacheKey ->
              fmap or
                ( traverse
                    (preparedContextRestrictsTo site contextValue)
                    (Set.toAscList (fvgcChangedFiberAuthors graphChanges))
                )
          )
          (rcFactViewKeys core)
      let dirtyContexts =
            Map.keysSet (Map.filter id visibilityByContext)
          generationIncrements =
            Map.fromSet (const 1) dirtyContexts
      Right
        core
          { rcContextFactDerivations =
              Map.withoutKeys
                (rcContextFactDerivations core)
                dirtyContexts,
            rcFactViewFiberGenerations =
              Map.unionWith
                (+)
                (rcFactViewFiberGenerations core)
                generationIncrements,
            rcCurrentFactRuleIdsByContext =
              Map.withoutKeys
                (rcCurrentFactRuleIdsByContext core)
                dirtyContexts
          }
{-# INLINE advanceRuntimeCoreFactViewGraphChanges #-}

invalidateRuntimeCoreFactViews ::
  RuntimeCore u schedulerGroup ->
  RuntimeCore u schedulerGroup
invalidateRuntimeCoreFactViews core =
  core
    { rcContextFactDerivations = Map.empty,
      rcCurrentFactRuleIdsByContext = Map.empty,
      rcFactViewBaseGeneration = rcFactViewBaseGeneration core + 1
    }
{-# INLINE invalidateRuntimeCoreFactViews #-}

type DerivedFactArtifacts :: Type -> Type
data DerivedFactArtifacts u = DerivedFactArtifacts
  { dfaFactDerivationResult :: !(FactDerivationResult u),
    dfaFactsChanged :: !Bool
  }

type EligibleMatchArtifacts :: Type -> Type
data EligibleMatchArtifacts u = EligibleMatchArtifacts
  { emaBaseMatches :: !(MatchBatch (SatSupportedMatch u)),
    emaContextMatches :: !(MatchBatch (SatSupportedMatch u)),
    emaAggregatedMatches :: !(MatchBatch (SatSupportedMatch u))
  }

type GuidedMatchArtifacts :: Type -> Type
data GuidedMatchArtifacts u = GuidedMatchArtifacts
  { gmaMatches :: !(MatchBatch (SatSupportedMatch u)),
    gmaTraceDelta :: !(Vector GuideRoundTrace),
    gmaAllCandidatesAccepted :: !Bool
  }

type ScheduledMatchArtifacts :: Type -> Type -> Type
data ScheduledMatchArtifacts u schedulerGroup = ScheduledMatchArtifacts
  { smaMatches :: !(MatchBatch (SatSupportedMatch u)),
    smaTracePolicy :: !TracePolicy,
    smaTraceDelta :: !(Vector (ScheduleTrace schedulerGroup)),
    smaAllCandidatesScheduled :: !Bool,
    smaPipelineCounts :: !(CandidatePipelineCounts schedulerGroup)
  }

type RoundRebuildDelta :: Type -> Type
data RoundRebuildDelta u = RoundRebuildDelta
  { rrdMatchingDelta :: !(SatMatchingDelta u),
    rrdContextRevision :: !Int
  }

type RoundArtifacts :: Type -> Type -> Type
data RoundArtifacts u schedulerGroup = RoundArtifacts
  { raInitialCore :: !(RuntimeCore u schedulerGroup),
    raGraphBefore :: !(SatGraph u),
    raBaseGraphBefore :: !(SatBaseGraph u),
    raBaseContext :: !(SatContext u),
    raDerivedFacts :: !(DerivedFactArtifacts u),
    raEligibleMatches :: !(EligibleMatchArtifacts u),
    raGuidance :: !(GuidedMatchArtifacts u),
    raSchedule :: !(ScheduledMatchArtifacts u schedulerGroup),
    raTraceDelta :: !(TraceLog (SatRuleKey u) schedulerGroup),
    raNoApplyMatchingDelta :: !(SatMatchingDelta u),
    raRebuildDelta :: !(Maybe (RoundRebuildDelta u))
  }

roundArtifactsFrontierComplete ::
  RoundArtifacts u schedulerGroup ->
  Bool
roundArtifactsFrontierComplete artifacts =
  gmaAllCandidatesAccepted (raGuidance artifacts)
    && smaAllCandidatesScheduled (raSchedule artifacts)
{-# INLINE roundArtifactsFrontierComplete #-}

roundArtifactsNextMatchingDelta ::
  RoundArtifacts u schedulerGroup ->
  SatMatchingDelta u
roundArtifactsNextMatchingDelta artifacts =
  maybe
    (raNoApplyMatchingDelta artifacts)
    rrdMatchingDelta
    (raRebuildDelta artifacts)
{-# INLINE roundArtifactsNextMatchingDelta #-}

roundViewFromParts ::
  forall u schedulerGroup.
  (FactSystem u, Ord (SatContext u)) =>
  RuntimeCore u schedulerGroup ->
  SatGraph u ->
  SatBaseGraph u ->
  SatContext u ->
  DerivedFactArtifacts u ->
  EligibleMatchArtifacts u ->
  SaturationRoundView u
roundViewFromParts initialCore graph baseGraph baseContext derivedFacts eligibleMatches =
  let factDerivationResult =
        dfaFactDerivationResult derivedFacts
      facts =
        Map.findWithDefault
          (emptyFactStore @u)
          baseContext
          (fdrFactsByContext factDerivationResult)
      factDerivations =
        Map.findWithDefault
          (emptyFactIndex @u)
          baseContext
          (fdrFactDerivationsByContext factDerivationResult)
   in SaturationRoundView
        { srvIteration = rcIterationCount initialCore,
          srvGraph = graph,
          srvBaseGraph = baseGraph,
          srvFacts = facts,
          srvFactDerivations = factDerivations,
          srvFactsChanged = dfaFactsChanged derivedFacts,
          srvFactRoundCount = fdrFactRoundCount factDerivationResult,
          srvBaseEligibleMatchCount = matchBatchLength (emaBaseMatches eligibleMatches),
          srvContextEligibleMatchCount = matchBatchLength (emaContextMatches eligibleMatches),
          srvAggregatedEligibleMatchCount = matchBatchLength (emaAggregatedMatches eligibleMatches),
          srvContextRevision = rcContextRevision initialCore
        }
{-# INLINE roundViewFromParts #-}

roundViewFromArtifacts ::
  forall u schedulerGroup.
  (FactSystem u, Ord (SatContext u)) =>
  RoundArtifacts u schedulerGroup ->
  SaturationRoundView u
roundViewFromArtifacts artifacts =
  roundViewFromParts @u
    (raInitialCore artifacts)
    (raGraphBefore artifacts)
    (raBaseGraphBefore artifacts)
    (raBaseContext artifacts)
    (raDerivedFacts artifacts)
    (raEligibleMatches artifacts)
{-# INLINE roundViewFromArtifacts #-}

seedRuntimeCoreFacts ::
  forall u schedulerGroup.
  (FactSystem u, Ord (SatContext u), Eq (SatFactStore u)) =>
  Map (SatContext u) (SatFactStore u) ->
  RuntimeCore u schedulerGroup ->
  RuntimeCore u schedulerGroup
seedRuntimeCoreFacts seedFacts core =
  let previousInputs =
        rcContextFactInputs core
      nextInputs =
        Map.unionWith
          (unionFactStores @u)
          seedFacts
          previousInputs
      changedContexts =
        Map.keysSet
          ( Map.filterWithKey
              ( \contextValue seedInput ->
                  ( unionFactStores @u
                      seedInput
                      ( Map.findWithDefault
                          (emptyFactStore @u)
                          contextValue
                          previousInputs
                      )
                  )
                    /= Map.findWithDefault
                      (emptyFactStore @u)
                      contextValue
                      previousInputs
              )
              seedFacts
          )
      generationIncrements =
        Map.fromSet (const 1) changedContexts
   in core
        { rcContextFactInputs = nextInputs,
          rcContextFactDerivations =
            Map.withoutKeys
              (rcContextFactDerivations core)
              changedContexts,
          rcCurrentFactRuleIdsByContext =
            Map.withoutKeys
              (rcCurrentFactRuleIdsByContext core)
              changedContexts,
          rcFactViewInputGenerations =
            Map.unionWith
              (+)
              (rcFactViewInputGenerations core)
              generationIncrements
        }
{-# INLINE seedRuntimeCoreFacts #-}

seedRuntimeStateFacts ::
  forall u carrier schedulerGroup.
  (FactSystem u, Ord (SatContext u), Eq (SatFactStore u)) =>
  Map (SatContext u) (SatFactStore u) ->
  RuntimeState u carrier schedulerGroup ->
  RuntimeState u carrier schedulerGroup
seedRuntimeStateFacts seedFacts state =
  state
    { rsCore = seedRuntimeCoreFacts @u seedFacts (rsCore state)
    }
{-# INLINE seedRuntimeStateFacts #-}

initialRuntimeCore ::
  forall u schedulerGroup.
  (FactSystem u, Monoid (SatChangeSummary u)) =>
  RuntimeCore u schedulerGroup
initialRuntimeCore =
  RuntimeCore
    { rcIterationCount = 0,
      rcTotalMatches = 0,
      rcTrace = emptyTraceLog,
      rcContextFactInputs = Map.empty,
      rcContextFacts = Map.empty,
      rcContextFactDerivations = Map.empty,
      rcFactViewBaseGeneration = 0,
      rcFactViewFiberGenerations = Map.empty,
      rcFactViewInputGenerations = Map.empty,
      rcCurrentFactRuleIdsByContext = Map.empty,
      rcCurrentFactCapabilityGeneration = 0,
      rcFactViewKeys = Map.empty,
      rcFactRoundsByContext = Map.empty,
      rcFactRoundCount = 0,
      rcGuideTrace = Seq.empty,
      rcGuideRoundCount = 0,
      rcMatchingDelta = fullMatchingDelta @u,
      rcChangeSummary = mempty,
      rcContextRevision = 0,
      rcPlanIdentity = Nothing
    }

initialRuntimeState ::
  forall u carrier schedulerGroup.
  (FactSystem u, Monoid (SatChangeSummary u)) =>
  SatMatchState u ->
  SchedulerState schedulerGroup ->
  carrier ->
  RuntimeState u carrier schedulerGroup
initialRuntimeState matchState schedulerState carrier =
  RuntimeState
    { rsCore = initialRuntimeCore @u,
      rsCarrier = carrier,
      rsMatchState = matchState,
      rsScheduler = schedulerState
    }

initialPlainRuntimeState ::
  forall u.
  (FactSystem u, Monoid (SatChangeSummary u)) =>
  SatMatchState u ->
  SatGraph u ->
  PlainRuntimeState u
initialPlainRuntimeState matchState =
  initialRuntimeState @u matchState emptySchedulerState

initialBasePlainRuntimeState ::
  forall u.
  (FactSystem u, BaseGraphEmbedding u (SatGraph u), Monoid (SatChangeSummary u)) =>
  SatMatchState u ->
  SatBaseGraph u ->
  PlainRuntimeState u
initialBasePlainRuntimeState matchState =
  initialPlainRuntimeState @u matchState . embedBaseGraph @u @(SatGraph u)

initialProofRuntimeState ::
  forall u proofGraph.
  (FactSystem u, Monoid (SatChangeSummary u)) =>
  SatMatchState u ->
  proofGraph ->
  ProofRuntimeState u proofGraph
initialProofRuntimeState matchState =
  initialRuntimeState @u matchState emptySchedulerState
