{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Test.ContextFixture
  ( TestSubstrate,
    TestContext (..),
    TestGraph (..),
    TestEffect (..),
    TestRule (..),
    TestFactRule (..),
    TestRound (..),
    TestMatch (..),
    TestSupportedMatch (..),
    TestMatchState (..),
    TestDeltaView (..),
    TestSiteProgram,
    TestContextState,
    TestReport,
    TestGoal,
    TestGuidance,
    testRuntimePolicy,
    emptyTestGraph,
    graphFromClasses,
    noEffect,
    addClassEffect,
    factViewFiberChangeEffect,
    makeBaseRule,
    makeContextRule,
    makeFactRule,
    makeObservedFactRule,
    emptySiteProgram,
    siteProgramWith,
    emptyTestMatchState,
    deltaView,
    supportedFor,
    primeBaseContextState,
    passThroughGuidance,
    runSaturation,
    classCountGoal,
    testContextLatticeValidation,
    testContextLattice,
    testPreparedSite,
    principalSupportOf,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Void (Void)
import Moonlight.Algebra
  ( BoundedJoinSemilattice (..),
    BoundedMeetSemilattice (..),
    JoinSemilattice (..),
    Lattice,
    MeetSemilattice (..)
  )
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite,
    fromFiniteLattice,
    unionPreparedSupport,
  )
import Moonlight.Core (RewriteRuleId (..))
import Moonlight.Delta.Scope
  ( Scoped,
    dirtyScope,
    foldScope,
    fullDelta,
    scopedDelta,
    scopedDeltaPayload,
    scopedDeltaSupport,
  )
import Moonlight.Core
  ( MatchActivationIndex (..),
    SiteIndex (..),
    SiteProgram (..),
  )
import Moonlight.Core (emptySubstitution)
import Moonlight.Saturation.Context.Error
  ( SaturationError (..),
  )
import Moonlight.Saturation.Context.Program.Compile
  ( planFromCompiledProgram,
  )
import Moonlight.Saturation.Context.Program.Plan
  ( Program,
    ProgramStage (CompiledProgramStage),
  )
import Moonlight.Saturation.Context.Program.Spec
  ( SaturationGuidanceView,
    planSpec,
    withGuidance,
    withSchedulerConfig,
  )
import Moonlight.Saturation.Context.Runtime.Carrier.Plain
  ( plainRuntimePolicy,
  )
import Moonlight.Saturation.Context.Runtime.Report
  ( SaturationReport,
  )
import Moonlight.Saturation.Context.Runtime.Policy
  ( RuntimePolicy,
  )
import Moonlight.Saturation.Context.Runtime.Engine
  ( runRuntime,
  )
import Moonlight.Saturation.Context.Runtime.State
  ( PlainRuntimeState,
    RuntimeState (..),
    rcContextFactDerivations,
    rcContextFacts,
    rsCarrier,
    rsCore,
    rsMatchState,
  )
import Moonlight.Saturation.Core (ApplyOutcome (..), SaturationBudget, TerminationGoal (..))
import Moonlight.Control.Gate (Gate, GuideRoundTrace, noGate)
import Moonlight.Control.Schedule (SchedulerConfig)
import Moonlight.Saturation.Matching (QueryFingerprint (..))
import Moonlight.Saturation.Substrate
import Moonlight.FiniteLattice
  ( ContextLatticeCompileError,
    latticeContext,
  )
import Moonlight.FiniteLattice
  ( principalSupport
  )

type TestQuery :: Type
newtype TestQuery = TestQuery
  { unTestQuery :: Int
  }
  deriving stock (Eq, Ord, Show)

type TestSnapshot :: Type
newtype TestSnapshot = TestSnapshot
  { unTestSnapshot :: Int
  }
  deriving stock (Eq, Ord, Show)

type TestContext :: Type
data TestContext
  = BaseContext
  | LeftContext
  | RightContext
  | TopContext
  deriving stock (Eq, Ord, Show, Enum, Bounded)

testContextLattice :: ContextLattice TestContext
testContextLattice =
  case latticeContext @TestContext of
    Right latticeValue -> latticeValue
    Left compileError ->
      error ("invalid TestContext lattice fixture: " <> show compileError)

testContextLatticeValidation ::
  Either (ContextLatticeCompileError TestContext) ()
testContextLatticeValidation =
  () <$ latticeContext @TestContext

testPreparedSite :: PreparedContextSite TestContext
testPreparedSite =
  fromFiniteLattice testContextLattice

instance JoinSemilattice TestContext where
  join =
    joinTestContext

instance MeetSemilattice TestContext where
  meet =
    meetTestContext

instance BoundedJoinSemilattice TestContext where
  bottom =
    BaseContext

instance BoundedMeetSemilattice TestContext where
  top =
    TopContext

instance Lattice TestContext

joinTestContext :: TestContext -> TestContext -> TestContext
joinTestContext leftContext rightContext =
  case (leftContext, rightContext) of
    (BaseContext, contextValue) ->
      contextValue
    (contextValue, BaseContext) ->
      contextValue
    (TopContext, _) ->
      TopContext
    (_, TopContext) ->
      TopContext
    (LeftContext, LeftContext) ->
      LeftContext
    (RightContext, RightContext) ->
      RightContext
    (LeftContext, RightContext) ->
      TopContext
    (RightContext, LeftContext) ->
      TopContext

meetTestContext :: TestContext -> TestContext -> TestContext
meetTestContext leftContext rightContext =
  case (leftContext, rightContext) of
    (TopContext, contextValue) ->
      contextValue
    (contextValue, TopContext) ->
      contextValue
    (BaseContext, _) ->
      BaseContext
    (_, BaseContext) ->
      BaseContext
    (LeftContext, LeftContext) ->
      LeftContext
    (RightContext, RightContext) ->
      RightContext
    (LeftContext, RightContext) ->
      BaseContext
    (RightContext, LeftContext) ->
      BaseContext

type TestGraph :: Type
data TestGraph = TestGraph
  { tgClasses :: !(IntMap IntSet),
    tgNodeCount :: !Int,
    tgPendingMerges :: !Int,
    tgRevision :: !Int,
    tgCapabilityGeneration :: !Int,
    tgDirtyImpacted :: !IntSet,
    tgDirtyKeys :: !IntSet,
    tgPayload :: !IntSet,
    tgUnionChanged :: !Bool,
    tgDisabledMatches :: !(IntMap IntSet)
  }
  deriving stock (Eq, Show)

type TestEffect :: Type
data TestEffect = TestEffect
  { teAddsFreshClass :: !Bool,
    teImpactedKeys :: !IntSet,
    teDirtyKeys :: !IntSet,
    tePayloadKeys :: !IntSet,
    teUnionChanged :: !Bool,
    teFactViewGraphChanges :: !(FactViewGraphChanges TestContext)
  }
  deriving stock (Eq, Show)

type TestRule :: Type
data TestRule = TestRule
  { trId :: !RewriteRuleId,
    trQuery :: !TestQuery,
    trBaseRoots :: ![Int],
    trContextRoots :: !(Map TestContext [Int]),
    trOneShot :: !Bool,
    trEffect :: !TestEffect
  }
  deriving stock (Eq, Show)

type TestFactRule :: Type
data TestFactRule = TestFactRule
  { tfrId :: !RewriteRuleId,
    tfrQuery :: !TestQuery,
    tfrProducedFact :: !Int,
    tfrReportsDerivation :: !Bool
  }
  deriving stock (Eq, Show)

type TestRound :: Type
newtype TestRound = TestRound
  { trDeltaFacts :: IntSet
  }
  deriving stock (Eq, Show)

type TestMatch :: Type
data TestMatch = TestMatch
  { tmRule :: !TestRule,
    tmRootClass :: !Int
  }
  deriving stock (Eq, Show)

type TestSupportedMatch :: Type
data TestSupportedMatch = TestSupportedMatch
  { tsmInner :: !TestMatch,
    tsmBasis :: !(SupportBasis TestContext),
    tsmWitnesses :: !(Map TestContext IntSet)
  }
  deriving stock (Eq, Show)

type TestRebuild :: Type
data TestRebuild = TestRebuild
  { trbEpoch :: !Int,
    trbDelta :: !(Scoped IntSet IntSet)
  }
  deriving stock (Eq, Show)

type TestDeltaView :: Type
data TestDeltaView
  = SawClean
  | SawFull
  | SawDirty !IntSet !(Maybe IntSet)
  deriving stock (Eq, Show)

type TestMatchState :: Type
data TestMatchState = TestMatchState
  { tmsBaseCalls :: !Int,
    tmsContextCalls :: !(Map TestContext Int),
    tmsRoundAdvances :: !Int,
    tmsSeenDeltas :: ![TestDeltaView],
    tmsRecordedSchedules :: !Int,
    tmsRebuildEpochs :: ![Int],
    tmsObservedSaturatedMatches :: !(Set.Set (RewriteRuleId, Int, SupportBasis TestContext))
  }
  deriving stock (Eq, Show)

type TestApplicationResult :: Type
data TestApplicationResult = TestApplicationResult
  { tarAppliedCount :: !Int,
    tarAppliedMatches :: ![TestSupportedMatch]
  }
  deriving stock (Eq, Show)

type TestSubstrate :: Type
data TestSubstrate

type TestSiteProgram :: Type
type TestSiteProgram =
  Program 'CompiledProgramStage TestSubstrate

type TestContextState :: Type
type TestContextState = PlainRuntimeState TestSubstrate

type TestReport :: Type
type TestReport = SaturationReport TestSubstrate

type TestGoal :: Type
type TestGoal = TerminationGoal TestContextState

type TestGuidance :: Type
type TestGuidance =
  Gate
    (SaturationGuidanceView TestSubstrate)
    ()
    (SatSupportedMatch TestSubstrate)
    GuideRoundTrace
    (SatRuleKey TestSubstrate)

emptyTestGraph :: TestGraph
emptyTestGraph = graphFromClasses []

graphFromClasses :: [Int] -> TestGraph
graphFromClasses classIds =
  TestGraph
    { tgClasses = IntMap.fromList [(classId, IntSet.singleton classId) | classId <- classIds],
      tgNodeCount = length classIds,
      tgPendingMerges = 0,
      tgRevision = 0,
      tgCapabilityGeneration = 0,
      tgDirtyImpacted = IntSet.empty,
      tgDirtyKeys = IntSet.empty,
      tgPayload = IntSet.empty,
      tgUnionChanged = False,
      tgDisabledMatches = IntMap.empty
    }

noEffect :: TestEffect
noEffect =
  TestEffect
    { teAddsFreshClass = False,
      teImpactedKeys = IntSet.empty,
      teDirtyKeys = IntSet.empty,
      tePayloadKeys = IntSet.empty,
      teUnionChanged = False,
      teFactViewGraphChanges = mempty
    }

addClassEffect :: IntSet -> IntSet -> IntSet -> TestEffect
addClassEffect impactedKeys dirtyKeys payloadKeys =
  TestEffect
    { teAddsFreshClass = True,
      teImpactedKeys = impactedKeys,
      teDirtyKeys = dirtyKeys,
      tePayloadKeys = payloadKeys,
      teUnionChanged = False,
      teFactViewGraphChanges =
        FactViewGraphChanges
          { fvgcBaseChanged = True,
            fvgcChangedFiberAuthors = Set.empty
          }
    }

factViewFiberChangeEffect :: TestContext -> TestEffect
factViewFiberChangeEffect contextValue =
  noEffect
    { teUnionChanged = True,
      teFactViewGraphChanges =
        FactViewGraphChanges
          { fvgcBaseChanged = False,
            fvgcChangedFiberAuthors = Set.singleton contextValue
          }
    }

makeBaseRule :: Int -> [Int] -> Bool -> TestEffect -> TestRule
makeBaseRule ruleKey roots oneShot effect =
  TestRule
    { trId = RewriteRuleId ruleKey,
      trQuery = TestQuery ruleKey,
      trBaseRoots = roots,
      trContextRoots = Map.empty,
      trOneShot = oneShot,
      trEffect = effect
    }

makeContextRule :: Int -> TestContext -> [Int] -> Bool -> TestEffect -> TestRule
makeContextRule ruleKey contextValue roots oneShot effect =
  TestRule
    { trId = RewriteRuleId ruleKey,
      trQuery = TestQuery ruleKey,
      trBaseRoots = [],
      trContextRoots = Map.singleton contextValue roots,
      trOneShot = oneShot,
      trEffect = effect
    }

makeFactRule :: Int -> Int -> TestFactRule
makeFactRule ruleKey producedFact =
  TestFactRule
    { tfrId = RewriteRuleId ruleKey,
      tfrQuery = TestQuery ruleKey,
      tfrProducedFact = producedFact,
      tfrReportsDerivation = False
    }

makeObservedFactRule :: Int -> Int -> TestFactRule
makeObservedFactRule ruleKey producedFact =
  (makeFactRule ruleKey producedFact)
    { tfrReportsDerivation = True
    }

emptySiteProgram :: TestSiteProgram
emptySiteProgram = siteProgramWith [] Map.empty [] Map.empty

siteProgramWith ::
  [TestRule] ->
  Map TestContext [TestRule] ->
  [TestFactRule] ->
  Map TestContext [TestFactRule] ->
  TestSiteProgram
siteProgramWith baseRules contextRules baseFactRules contextFactRules =
  SiteProgram
    { spFactRules =
        SiteIndex
          { siBase = baseFactRules,
            siContexts = contextFactRules
          },
      spRewriteRules =
        SiteIndex
          { siBase = baseRules,
            siContexts = contextRules
          },
      spSupportedFactRules = [],
      spSupportedRewriteRules = Map.empty,
      spRewriteActivation =
        MatchActivationIndex
          { maiBase = Set.fromList (fmap trId baseRules),
            maiContexts = fmap (Set.fromList . fmap trId) contextRules
          },
      spBaseRewriteSupport =
        Map.fromList
          [(trId rule, principalSupport BaseContext) | rule <- baseRules]
    }

emptyTestMatchState :: TestMatchState
emptyTestMatchState =
  TestMatchState
    { tmsBaseCalls = 0,
      tmsContextCalls = Map.empty,
      tmsRoundAdvances = 0,
      tmsSeenDeltas = [],
      tmsRecordedSchedules = 0,
      tmsRebuildEpochs = [],
      tmsObservedSaturatedMatches = Set.empty
    }

deltaView :: Scoped IntSet IntSet -> TestDeltaView
deltaView matchingDelta =
  foldScope
    SawClean
    (\keys -> SawDirty keys (scopedDeltaPayload matchingDelta))
    SawFull
    (scopedDeltaSupport matchingDelta)

supportedFor :: TestContext -> TestMatch -> TestSupportedMatch
supportedFor contextValue matchValue =
  TestSupportedMatch
    { tsmInner = matchValue,
      tsmBasis = principalSupport contextValue,
      tsmWitnesses = Map.singleton contextValue (IntSet.singleton (tmRootClass matchValue))
    }

primeBaseContextState :: TestContextState -> TestContextState
primeBaseContextState state =
  state
    { rsCore =
        (rsCore state)
          { rcContextFacts = Map.singleton BaseContext IntSet.empty,
            rcContextFactDerivations = Map.singleton BaseContext IntSet.empty
          }
    }

passThroughGuidance :: TestGuidance
passThroughGuidance =
  noGate

testRuntimePolicy ::
  RuntimePolicy
    TestSubstrate
    (SatGraph TestSubstrate)
    (SatRuleKey TestSubstrate)
    (SaturationReport TestSubstrate)
testRuntimePolicy =
  plainRuntimePolicy @TestSubstrate

runSaturation ::
  SaturationBudget ->
  SchedulerConfig RewriteRuleId ->
  TestSiteProgram ->
  TestGoal ->
  TestMatchState ->
  TestContextState ->
  Either (SaturationError TestSubstrate (SatRuleKey TestSubstrate)) (TestMatchState, TestReport)
runSaturation budget schedulerConfig siteProgram terminationGoal seedMatchState state0 = do
  planValue <-
    first SaturationCompileFailure $
      planFromCompiledProgram @TestSubstrate
        (withGuidance passThroughGuidance (withSchedulerConfig schedulerConfig (planSpec budget () ())))
        siteProgram
  case first SaturationRunFailure $
    runRuntime @TestSubstrate
      testRuntimePolicy
      planValue
      terminationGoal
      (state0 { rsMatchState = seedMatchState }) of
      Left err -> Left err
      Right (finalState, finalReport) ->
        Right (rsMatchState finalState, finalReport)

classCountGoal :: Int -> TestGoal
classCountGoal requiredClasses =
  TerminationGoal
    (\state ->
       graphClassCount @TestSubstrate (rsCarrier state) >= requiredClasses
    )

principalSupportOf :: TestContext -> SupportBasis TestContext
principalSupportOf =
  principalSupport

ruleIdKey :: RewriteRuleId -> Int
ruleIdKey (RewriteRuleId ruleKey) =
  ruleKey

nextFreshClassId :: TestGraph -> Int
nextFreshClassId graph =
  maybe 1 ((+ 1) . fst) (IntMap.lookupMax (tgClasses graph))

disableMatch :: RewriteRuleId -> Int -> TestGraph -> TestGraph
disableMatch ruleId rootClass graph =
  graph
    { tgDisabledMatches =
        IntMap.insertWith
          IntSet.union
          (ruleIdKey ruleId)
          (IntSet.singleton rootClass)
          (tgDisabledMatches graph)
    }

isDisabled :: TestGraph -> TestRule -> Int -> Bool
isDisabled graph ruleValue rootClass =
  IntSet.member
    rootClass
    (IntMap.findWithDefault IntSet.empty (ruleIdKey (trId ruleValue)) (tgDisabledMatches graph))

baseMatchesFor :: TestGraph -> TestRule -> [TestMatch]
baseMatchesFor graph ruleValue =
  [ TestMatch ruleValue rootClass
  | rootClass <- trBaseRoots ruleValue,
    IntMap.member rootClass (tgClasses graph),
    not (isDisabled graph ruleValue rootClass)
  ]

contextMatchesFor :: TestGraph -> TestContext -> TestRule -> [TestMatch]
contextMatchesFor graph contextValue ruleValue =
  [ TestMatch ruleValue rootClass
  | rootClass <- Map.findWithDefault [] contextValue (trContextRoots ruleValue),
    IntMap.member rootClass (tgClasses graph),
    not (isDisabled graph ruleValue rootClass)
  ]

applySupportedMatch :: TestGraph -> TestSupportedMatch -> TestGraph
applySupportedMatch graph supportedMatch =
  let matchValue = tsmInner supportedMatch
      ruleValue = tmRule matchValue
      effect = trEffect ruleValue
      disabledGraph =
        if trOneShot ruleValue
          then disableMatch (trId ruleValue) (tmRootClass matchValue) graph
          else graph
      graphWithFreshClass =
        if teAddsFreshClass effect
          then
            let freshClassId = nextFreshClassId disabledGraph
             in disabledGraph
                  { tgClasses = IntMap.insert freshClassId (IntSet.singleton freshClassId) (tgClasses disabledGraph),
                    tgNodeCount = tgNodeCount disabledGraph + 1
                  }
          else disabledGraph
   in graphWithFreshClass
        { tgPendingMerges =
            tgPendingMerges graphWithFreshClass
              + if teUnionChanged effect then 1 else 0,
          tgDirtyImpacted =
            IntSet.union (tgDirtyImpacted graphWithFreshClass) (teImpactedKeys effect),
          tgDirtyKeys =
            IntSet.union (tgDirtyKeys graphWithFreshClass) (teDirtyKeys effect),
          tgPayload =
            IntSet.union (tgPayload graphWithFreshClass) (tePayloadKeys effect),
          tgUnionChanged =
            tgUnionChanged graphWithFreshClass || teUnionChanged effect
        }

testSaturationKey :: TestSupportedMatch -> (RewriteRuleId, Int, SupportBasis TestContext)
testSaturationKey supportedMatch =
  let matchValue =
        tsmInner supportedMatch
   in (trId (tmRule matchValue), tmRootClass matchValue, tsmBasis supportedMatch)

testApplyOutcome :: [TestSupportedMatch] -> TestGraph -> ApplyOutcome TestApplicationResult TestGraph
testApplyOutcome scheduledMatches carrier =
  ApplyOutcome
    { aoState = foldl' applySupportedMatch carrier scheduledMatches,
      aoEffect =
        TestApplicationResult
          { tarAppliedCount = length scheduledMatches,
            tarAppliedMatches = scheduledMatches
          }
    }

type instance SatGraph TestSubstrate = TestGraph
type instance SatBaseGraph TestSubstrate = TestGraph
type instance SatClassId TestSubstrate = Int
type instance SatContext TestSubstrate = TestContext

type instance SatObstruction TestSubstrate = String
type instance SatCapabilityResolver TestSubstrate = ()

type instance SatFactStore TestSubstrate = IntSet
type instance SatFactIndex TestSubstrate = IntSet
type instance SatFactSource TestSubstrate = TestFactRule
type instance SatFactRule TestSubstrate = TestFactRule
type instance SatFactCompileError TestSubstrate = String
type instance SatFactRound TestSubstrate = TestRound

type instance SatQuery TestSubstrate = TestQuery
type instance SatMatchSnapshot TestSubstrate = TestSnapshot
type instance SatMatchSection TestSubstrate = ()
type instance SatMatchingDelta TestSubstrate = Scoped IntSet IntSet
type instance SatChangeSummary TestSubstrate = FactViewGraphChanges TestContext

type instance SatRuleSource TestSubstrate = TestRule
type instance SatRule TestSubstrate = TestRule
type instance SatRuleKey TestSubstrate = RewriteRuleId
type instance SatRewriteContext TestSubstrate = ()
type instance SatRuleCompileError TestSubstrate = String

type instance SatRawMatch TestSubstrate = TestMatch
type instance SatRawMatchRejection TestSubstrate = Void
type instance SatRequestMatch TestSubstrate = ()
type instance SatMatchWorld TestSubstrate = ()
type instance SatMatchingRequest TestSubstrate = ()
type instance SatMatch TestSubstrate = TestMatch
type instance SatSupportedMatch TestSubstrate = TestSupportedMatch
type instance SatSupportWitness TestSubstrate = IntSet
type instance SatMatchState TestSubstrate = TestMatchState
type instance SatMatchStrategy TestSubstrate = ()

type instance SatRebuild TestSubstrate = TestRebuild
type instance SatApplicationError TestSubstrate = String
type instance SatApplicationResult TestSubstrate = TestApplicationResult

type instance SatProofGraph TestSubstrate p = TestGraph
type instance SatProofBuilder TestSubstrate p = ()

instance SaturationGraph TestSubstrate where
  graphCanonicalizeClass =
    canonicalizeTestClass

  graphClassCount =
    IntMap.size . tgClasses

  graphNodeCount =
    tgNodeCount

  graphBase =
    id

  baseGraphEquals leftGraph rightGraph =
    tgClasses leftGraph == tgClasses rightGraph
      && tgNodeCount leftGraph == tgNodeCount rightGraph

  graphContextLattice _graph =
    testContextLattice

  graphPreparedSite _graph =
    testPreparedSite

  graphPendingMerges =
    tgPendingMerges

  graphConvergenceStateEquals =
    \leftGraph rightGraph ->
      baseGraphEquals @TestSubstrate leftGraph rightGraph
        && tgRevision leftGraph == tgRevision rightGraph

  graphContextClassProjection _contextValue graph =
    Right
      ( IntMap.fromList
          [ (classId, classId)
          | classId <- IntMap.keys (tgClasses graph)
          ]
      )

  graphContextClasses _contextValue graph =
    Right (Set.fromList (IntMap.keys (tgClasses graph)))

instance CapabilitySystem TestSubstrate where
  emptyCapabilityResolver = ()

instance QueryIndex TestSubstrate where
  queryFingerprint = Right . QueryFingerprint . unTestQuery
  matchSnapshotKey = QueryFingerprint . unTestSnapshot

  fullMatchingDelta =
    fullDelta

  registerQueries _queries = Right

  contextMatchSections _graph = Map.empty

  lookupQueryId _fingerprint _graph = Nothing

instance FactSystem TestSubstrate where
  type SatFactRuleIdentity TestSubstrate = QueryFingerprint

  emptyFactStore = IntSet.empty
  emptyFactIndex = IntSet.empty

  canonicalizeFactStore = canonicalizeTestFacts
  canonicalizeFactIndex = canonicalizeTestFacts
  canonicalizeFactStoreBase = canonicalizeTestFacts
  canonicalizeFactIndexBase = canonicalizeTestFacts
  canonicalizeFactStoreAtContext _contextValue graph =
    Right . canonicalizeTestFacts graph
  canonicalizeFactIndexAtContext _contextValue graph =
    Right . canonicalizeTestFacts graph

  unionFactStores = IntSet.union
  factChangeMatchingDelta _graph oldFacts newFacts =
    dirtyDeltaFromKeys (changedTestFactKeys oldFacts newFacts)
  compileFactRules factRules =
    if any ((< 0) . tfrProducedFact) factRules
      then Left "invalid fact rule"
      else Right factRules

  factRuleQuery = tfrQuery
  factRuleId = tfrId
  factRuleIdentity factRule =
    Right
      ( testFingerprint
          0x213
          [ rewriteRuleIdInt (tfrId factRule),
            unTestQuery (tfrQuery factRule),
            tfrProducedFact factRule,
            boolFingerprint (tfrReportsDerivation factRule)
          ]
      )
  factSourceId = tfrId

  deriveFactClosure _capResolver _currentFacts factRules _baseGraph seedFacts seedIndex =
    let newFacts =
          IntSet.fromList
            [ tfrProducedFact factRule
            | factRule <- factRules,
              IntSet.notMember (tfrProducedFact factRule) seedFacts
            ]
        factRounds
          | not (IntSet.null newFacts) =
              [TestRound newFacts]
          | any tfrReportsDerivation factRules =
              [TestRound IntSet.empty]
          | otherwise =
              []
     in Right
          ( IntSet.union seedFacts newFacts,
            IntSet.union seedIndex newFacts,
            factRounds
          )

  deriveFactClosureAtContext capabilityResolver currentFacts factRules graph _contextValue seedFacts seedIndex =
    deriveFactClosure @TestSubstrate
      capabilityResolver
      currentFacts
      factRules
      graph
      seedFacts
      seedIndex

canonicalizeTestClass :: Int -> TestGraph -> Int
canonicalizeTestClass classId graph =
  maybe
    classId
    id
    ( IntMap.foldrWithKey
        ( \representative members canonicalRepresentative ->
            if IntSet.member classId members
              then Just representative
              else canonicalRepresentative
        )
        Nothing
        (tgClasses graph)
    )

canonicalizeTestFacts :: TestGraph -> IntSet -> IntSet
canonicalizeTestFacts graph =
  IntSet.map (\classId -> canonicalizeTestClass classId graph)

changedTestFactKeys ::
  Map TestContext IntSet ->
  Map TestContext IntSet ->
  IntSet
changedTestFactKeys oldFacts newFacts =
  foldMap changedContextKeys allContexts
  where
    allContexts =
      Set.union (Map.keysSet oldFacts) (Map.keysSet newFacts)

    changedContextKeys contextValue =
      let oldStore =
            Map.findWithDefault IntSet.empty contextValue oldFacts
          newStore =
            Map.findWithDefault IntSet.empty contextValue newFacts
       in IntSet.difference oldStore newStore
            <> IntSet.difference newStore oldStore

dirtyDeltaFromKeys :: IntSet -> Scoped IntSet IntSet
dirtyDeltaFromKeys keys =
  scopedDelta (dirtyScope keys) Nothing

instance RewriteSystem TestSubstrate where
  type SatRewriteRuleIdentity TestSubstrate = QueryFingerprint

  compileRewriteRules rules =
    if any ((< 0) . unTestQuery . trQuery) rules
      then Left "invalid rewrite rule"
      else Right rules

  rewriteRuleSourceId = trId

  rewriteRuleId = trId
  rewriteRuleIdentity rule =
    Right
      ( testFingerprint
          0x377
          ( [ rewriteRuleIdInt (trId rule),
              unTestQuery (trQuery rule),
              boolFingerprint (trOneShot rule)
            ]
              <> trBaseRoots rule
              <> foldMap contextRootsFingerprint (Map.toAscList (trContextRoots rule))
              <> testEffectFingerprint (trEffect rule)
          )
      )
  rewriteRuleKey = trId
  rewriteRuleQuery = trQuery

  defaultRewriteContext = ()

  rewriteCapabilityResolver _rewriteContext _graph =
    emptyCapabilityResolver @TestSubstrate

testFingerprint :: Int -> [Int] -> QueryFingerprint
testFingerprint seed =
  QueryFingerprint . foldl' mixFingerprint seed
  where
    mixFingerprint :: Int -> Int -> Int
    mixFingerprint acc nextValue =
      acc * 16777619 + nextValue
{-# INLINE testFingerprint #-}

rewriteRuleIdInt :: RewriteRuleId -> Int
rewriteRuleIdInt (RewriteRuleId ruleKey) =
  ruleKey
{-# INLINE rewriteRuleIdInt #-}

boolFingerprint :: Bool -> Int
boolFingerprint flag =
  if flag then 1 else 0
{-# INLINE boolFingerprint #-}

contextRootsFingerprint :: (TestContext, [Int]) -> [Int]
contextRootsFingerprint (contextValue, roots) =
  fromEnum contextValue : length roots : roots
{-# INLINE contextRootsFingerprint #-}

testEffectFingerprint :: TestEffect -> [Int]
testEffectFingerprint effect =
  [ boolFingerprint (teAddsFreshClass effect),
    boolFingerprint (teUnionChanged effect),
    boolFingerprint (fvgcBaseChanged (teFactViewGraphChanges effect))
  ]
    <> taggedIntSetFingerprint 0x41 (teImpactedKeys effect)
    <> taggedIntSetFingerprint 0x43 (teDirtyKeys effect)
    <> taggedIntSetFingerprint 0x47 (tePayloadKeys effect)
    <> ( 0x53
           : Set.size changedFiberAuthors
           : fmap fromEnum (Set.toAscList changedFiberAuthors)
       )
  where
    changedFiberAuthors =
      fvgcChangedFiberAuthors (teFactViewGraphChanges effect)
{-# INLINE testEffectFingerprint #-}

taggedIntSetFingerprint :: Int -> IntSet -> [Int]
taggedIntSetFingerprint tag keys =
  tag : IntSet.size keys : IntSet.toAscList keys
{-# INLINE taggedIntSetFingerprint #-}

instance MatchView TestSubstrate where
  matchKey matchValue =
    (trId (tmRule matchValue), tmRootClass matchValue, emptySubstitution)

  matchRuleKey =
    trId . tmRule

  supportedMatchInner = tsmInner

  setSupportedMatchInner innerMatch supportedMatch =
    supportedMatch {tsmInner = innerMatch}

  supportedMatchBasis = tsmBasis

  supportedMatchWitnesses = tsmWitnesses

  mergeSupportedMatch graph leftMatch rightMatch =
    fmap
      ( \mergedSupport ->
          TestSupportedMatch
            { tsmInner = tsmInner leftMatch,
              tsmBasis = mergedSupport,
              tsmWitnesses =
                Map.unionWith
                  IntSet.union
                  (tsmWitnesses leftMatch)
                  (tsmWitnesses rightMatch)
            }
      )
      (first show (unionPreparedSupport (graphPreparedSite @TestSubstrate graph) (tsmBasis leftMatch) (tsmBasis rightMatch)))

instance MatchingBackend TestSubstrate where
  initialMatchState _strategy _rewriteContext =
    emptyTestMatchState

  runMatchingRequests _matchingDelta _matchWorld _requests matchState =
    (matchState, Right [])

  materializeRawMatch _rewriteContext _capResolver contextValue _facts _derivations _baseGraph rawMatch =
    Right (supportedFor contextValue rawMatch)

  materializeRawMatchesAtContextView _rewriteContext _capResolver contextValue _facts _derivations _graph =
    Right . fmap (supportedFor contextValue)

  rawBaseMatchesPrepared _rewriteContext _iteration matchingDelta graph _facts rewriteRules matchState =
    Right
      ( matchState
          { tmsBaseCalls = tmsBaseCalls matchState + 1,
            tmsSeenDeltas = tmsSeenDeltas matchState <> [deltaView matchingDelta]
          },
        concatMap (baseMatchesFor graph) rewriteRules
      )

  rawContextMatchesPrepared _rewriteContext contextValue _iteration matchingDelta graph _facts _derivations rewriteRules matchState =
    Right
      ( matchState
          { tmsContextCalls = Map.insertWith (+) contextValue 1 (tmsContextCalls matchState),
            tmsSeenDeltas = tmsSeenDeltas matchState <> [deltaView matchingDelta]
          },
        concatMap (contextMatchesFor graph contextValue) rewriteRules
      )

  consumedDerivations _supportedMatch =
    IntSet.empty

  rawMatchRuleKey = trId . tmRule

  filterSupportedMatches _rewriteContext _factStore matchState matches _graph =
    filter
      ( \(_, supportedMatch) ->
          Set.notMember
            (testSaturationKey supportedMatch)
            (tmsObservedSaturatedMatches matchState)
      )
      matches

  advanceMatchStateForRound _matchingDelta _graph matchState =
    matchState {tmsRoundAdvances = tmsRoundAdvances matchState + 1}

  advanceMatchStateAfterRebuild rebuildReport matchState =
    matchState {tmsRebuildEpochs = tmsRebuildEpochs matchState <> [trbEpoch rebuildReport]}

  recordScheduledMatches scheduledMatches matchState =
    matchState
      { tmsRecordedSchedules =
          tmsRecordedSchedules matchState + length scheduledMatches
      }

  recordApplicationResult _graph applicationResult matchState =
    matchState
      { tmsObservedSaturatedMatches =
          tmsObservedSaturatedMatches matchState
            <> Set.fromList (fmap testSaturationKey (tarAppliedMatches applicationResult))
      }

instance ApplicationResultSystem TestSubstrate where
  applicationResultCount =
    tarAppliedCount

instance GraphApply TestSubstrate where
  applyBaseMatches _rewriteContext _factStore scheduledMatches carrier =
    Right (testApplyOutcome scheduledMatches carrier)

  applyContextualMatches _rewriteContext scheduledMatches carrier =
    Right (testApplyOutcome scheduledMatches carrier)

instance RebuildSystem TestSubstrate where
  rebuildGraph graph _facts _derivations =
    let scopeKeys =
          IntSet.union (tgDirtyImpacted graph) (tgDirtyKeys graph)
        rebuildDelta =
          scopedDelta
            (dirtyScope scopeKeys)
            (if IntSet.null (tgPayload graph) then Nothing else Just (tgPayload graph))
        hasPendingWork =
          tgPendingMerges graph > 0
            || not (IntSet.null (tgDirtyImpacted graph))
            || not (IntSet.null (tgDirtyKeys graph))
            || not (IntSet.null (tgPayload graph))
            || tgUnionChanged graph
        nextEpoch =
          if hasPendingWork
            then tgRevision graph + 1
            else tgRevision graph
        rebuiltGraph =
          graph
            { tgRevision = nextEpoch,
              tgDirtyImpacted = IntSet.empty,
              tgDirtyKeys = IntSet.empty,
              tgPayload = IntSet.empty,
              tgPendingMerges = 0,
              tgUnionChanged = False
            }
     in Right (rebuiltGraph, TestRebuild nextEpoch rebuildDelta)

  rebuildEpoch = trbEpoch

  rebuildMatchingDelta = trbDelta

  factViewGraphChanges = id

  postApplyMatchingDelta _matchState _scheduledMatches _applicationResult =
    rebuildMatchingDelta @TestSubstrate

  postApplyChangeSummary _matchState _scheduledMatches applicationResult _rebuildReport =
    foldMap
      (teFactViewGraphChanges . trEffect . tmRule . tsmInner)
      (tarAppliedMatches applicationResult)

instance ProofCarrier TestSubstrate p where
  proofGraphContext = id

  setProofGraphContext = const

  applyProofMatches _rewriteContext _builder _ctx scheduledMatches carrier =
    Right (testApplyOutcome scheduledMatches carrier)
