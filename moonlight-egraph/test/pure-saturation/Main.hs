{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Exception (Exception, evaluate, throw)
import Data.Fix (Fix (..))
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Map.Lazy qualified as LazyMap
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Kind (Type)
import Moonlight.Algebra
  ( JoinSemilattice (join),
  )
import Moonlight.Core
  ( ClassId (..),
    ConstructorTag,
    HasConstructorTag (..),
    Pattern (..),
    PatternVar,
    RewriteRuleId (..),
    Substitution,
    UnionFindAllocationError,
    ZipMatch (..),
    mkPatternVar,
    zipSameNodeShape,
  )
import Moonlight.Delta.Scope qualified as Delta
import Moonlight.EGraph.Effect.CoveringSurface (SurfaceKind)
import Moonlight.EGraph.Pure.Change (EGraphMutationResult (..))
import Moonlight.EGraph.Pure.Context
  ( contextMerge,
    emptyContextMutationTrace,
    withEmptyContextEGraph,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons
  ( addTerm,
    insertTermsTracked,
  )
import Moonlight.EGraph.Pure.Analysis
  ( AnalysisSpec,
    semilatticeAnalysis,
  )
import Moonlight.EGraph.Pure.Rebuild
  ( merge,
    rebuildWithDelta,
  )
import Moonlight.EGraph.Pure.Saturation.Apply
  ( EGraphApplicationResult (..),
  )
import Moonlight.EGraph.Pure.Saturation.Matching
  ( MatchingStrategy (..),
    matchingDeltaFromTouchedKeys,
  )
import Moonlight.EGraph.Pure.Saturation.Substrate
  ( EGraphU,
    RawRewriteMatch (..),
  )
import Moonlight.EGraph.Pure.Types
  ( EGraph,
    canonicalizeClassId,
    classIdKey,
    eGraphAnalysis,
    emptyEGraph,
  )
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingContextEGraph,
    emptySaturatingContextEGraph,
    sceContextGraph,
  )
import Moonlight.FiniteLattice
  ( ContextLattice,
    SupportBasis,
    contextLatticeFromClosedOrder,
    supportBasis,
  )
import Moonlight.Rewrite.ProofContext
  ( SupportedRewriteMatch (..),
  )
import Moonlight.Rewrite.Runtime
  ( ExecutableRewriteMatch (..),
  )
import Moonlight.Rewrite.Runtime
  ( RulePlan,
    rpId,
  )
import Moonlight.Rewrite.System
  ( CompiledGuard,
    RewriteCondition,
  )
import Moonlight.Rewrite.System
  ( emptyFactDerivationIndex,
  )
import Moonlight.Rewrite.System qualified as LogicStore
import Moonlight.Rewrite.System
  ( RawRewriteRule (..),
  )
import Moonlight.Saturation.Substrate
  ( SatMatchState,
    compileRewriteRules,
    contextSupportedMatchesPrepared,
    defaultRewriteContext,
    emptyCapabilityResolver,
    filterSupportedMatches,
    graphBase,
    initialMatchState,
    materializeRawMatch,
    rawBaseMatchesPrepared,
    recordApplicationResult,
    advanceMatchStateForRound,
    trivialLattice,
  )
import System.Timeout (timeout)
import Test.Tasty
  ( TestTree,
    defaultMain,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertFailure,
    testCase,
    (@?=),
  )

type TestU owner context =
  EGraphU owner SurfaceKind TestF NodeCount context

type TestGraph owner context =
  SaturatingContextEGraph owner SurfaceKind TestF NodeCount context

type TestRule =
  RulePlan (CompiledGuard SurfaceKind TestF) TestF

type TestF :: Type -> Type
data TestF child
  = Var String
  | Num Int
  | Add child child
  | Mul child child
  | Zero
  | One
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type TestTag :: Type
data TestTag
  = VarTag String
  | NumTag Int
  | AddTag
  | MulTag
  | ZeroTag
  | OneTag
  deriving stock (Eq, Ord, Show)

instance HasConstructorTag TestF where
  type ConstructorTag TestF = TestTag

  constructorTag nodeValue =
    case nodeValue of
      Var name -> VarTag name
      Num value -> NumTag value
      Add {} -> AddTag
      Mul {} -> MulTag
      Zero -> ZeroTag
      One -> OneTag

instance ZipMatch TestF where
  zipMatch =
    zipSameNodeShape

type NodeCount :: Type
newtype NodeCount = NodeCount Int
  deriving stock (Eq, Ord, Show)

instance JoinSemilattice NodeCount where
  join (NodeCount left) (NodeCount right) =
    NodeCount (max left right)

testAnalysis :: AnalysisSpec TestF NodeCount
testAnalysis =
  semilatticeAnalysis testNodeCount

testNodeCount :: TestF NodeCount -> NodeCount
testNodeCount nodeValue =
  case nodeValue of
    Var _ -> NodeCount 1
    Num _ -> NodeCount 1
    Add (NodeCount left) (NodeCount right) -> NodeCount (left + right + 1)
    Mul (NodeCount left) (NodeCount right) -> NodeCount (left + right + 1)
    Zero -> NodeCount 1
    One -> NodeCount 1

testVar :: String -> Fix TestF
testVar name =
  Fix (Var name)

testAdd :: Fix TestF -> Fix TestF -> Fix TestF
testAdd left right =
  Fix (Add left right)

testMul :: Fix TestF -> Fix TestF -> Fix TestF
testMul left right =
  Fix (Mul left right)

testNum :: Int -> Fix TestF
testNum value =
  Fix (Num value)

testZero :: Fix TestF
testZero =
  Fix Zero

testOne :: Fix TestF
testOne =
  Fix One

data SupportContext
  = SupportBottom
  | SupportLeft
  | SupportRight
  | SupportTop
  deriving stock (Eq, Ord, Show)

main :: IO ()
main =
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "moonlight-egraph:pure-saturation termination regressions"
    [ testCase "NO-OP PRODUCTIVITY" noOpProductivityLaw,
      testCase "SATURATED-KEY RETENTION" saturatedKeyRetentionLaw,
      testCase "CYCLIC-REPAIR TERMINATION" cyclicRepairTerminationLaw,
      testCase "WCOJ SIBLING COMPLETENESS" wcojSiblingCompletenessLaw,
      testCase "REGIONAL WCOJ LAZY VIEW AGREES WITH EAGER AND PER-CONTEXT ORACLES" regionalWcojOracleLaw,
      testCase "REGIONAL WCOJ DEMANDS FACT VIEWS ONLY AT SUPPORT GENERATORS" regionalWcojDemandLaw,
      testCase "REGIONAL WCOJ DELTA AGREES WITH REPEATED-VARIABLE ORACLE" regionalWcojDeltaOracleLaw
    ]

noOpProductivityLaw :: Assertion
noOpProductivityLaw = do
  lattice <- supportContextLattice
  topSupport <- supportAcrossGenerators lattice
  (baseGraph, _) <- buildTestGraph [testAdd (testVar "x") testZero]
  withTestGraphFromLattice lattice baseGraph $ \(ownerProxy :: Proxy owner) graph -> do
    rule <- compileSingleTestRule @owner @SupportContext ownerProxy noOpAddZeroRule
    supportedMatch <-
      requireSingle "expected one no-op match"
        =<< supportedMatchesForRule @owner SupportBottom graph rule
    let supportedAcrossCover =
          supportedMatch
            { srmSupport = topSupport,
              srmWitnesses = Map.empty
            }
        filteredMatches =
          filterSupportedMatches @(TestU owner SupportContext)
            (defaultRewriteContext @(TestU owner SupportContext))
            LogicStore.emptyFactStore
            (initialTestMatchState @owner @SupportContext)
            [((), supportedAcrossCover)]
            graph
    length filteredMatches @?= 0

saturatedKeyRetentionLaw :: Assertion
saturatedKeyRetentionLaw = do
  (baseGraph, _) <- buildTestGraph [testAdd (testVar "x") testZero]
  withTestGraphFromLattice trivialLattice baseGraph $ \(ownerProxy :: Proxy owner) graph -> do
    rule <- compileSingleTestRule @owner @() ownerProxy addZeroProductiveRule
    supportedMatch <-
      requireSingle "expected one productive match"
        =<< supportedMatchesForRule @owner () graph rule
    let applicationResult =
          EGraphApplicationResult
            { egarTrace = emptyContextMutationTrace (graphBase @(TestU owner ()) graph),
              egarAppliedMatches = [supportedMatch],
              egarProofRestrictionRegistryConstructions = 0,
              egarProofExtractionTableConstructions = 0
            }
        recordedState =
          recordApplicationResult @(TestU owner ())
            graph
            applicationResult
            (initialTestMatchState @owner @())
        localDelta =
          matchingDeltaFromTouchedKeys (IntSet.singleton 9001)
        advancedState =
          advanceMatchStateForRound @(TestU owner ())
            localDelta
            graph
            recordedState
        filteredMatches =
          filterSupportedMatches @(TestU owner ())
            (defaultRewriteContext @(TestU owner ()))
            LogicStore.emptyFactStore
            advancedState
            [((), supportedMatch)]
            graph
    length filteredMatches @?= 0

cyclicRepairTerminationLaw :: Assertion
cyclicRepairTerminationLaw = do
  repairResult <- timeout 1000000 (evaluate cyclicRepairAnalysis)
  case repairResult of
    Nothing ->
      assertFailure "cyclic repair did not terminate within 1000000 microseconds"
    Just (Right analysisValue) ->
      analysisValue @?= Just (NodeCount 3)
    Just (Left allocationError) ->
      assertFailure ("cyclic repair fixture allocation failed: " <> show allocationError)

wcojSiblingCompletenessLaw :: Assertion
wcojSiblingCompletenessLaw = do
  let addOne =
        testAdd (testVar "x") testZero
      addTwo =
        testAdd testOne testZero
  (baseGraph, rootClasses) <- buildTestGraph [addOne, addTwo]
  withTestGraphFromLattice trivialLattice baseGraph $ \(ownerProxy :: Proxy owner) graph -> do
    compiledRules <- compileTestRules @owner @() ownerProxy [sameAddLhsLeftRule, sameAddLhsRightRule]
    let expectedRoots =
          Set.fromList (fmap (canonicalizeClassId baseGraph) rootClasses)
    rawMatches <- rawMatchesForRules @owner @() graph compiledRules
    rootsByRule rawMatches (RewriteRuleId 201) @?= expectedRoots
    rootsByRule rawMatches (RewriteRuleId 202) @?= expectedRoots

regionalWcojOracleLaw :: Assertion
regionalWcojOracleLaw = do
  lattice <- supportContextLattice
  (baseGraph, classIds) <-
    buildTestGraph
      [ testVar "x",
        testOne,
        testAdd (testVar "x") testZero,
        testAdd testOne testZero
      ]
  (variableClass, oneClass) <-
    case classIds of
      variableClass : oneClass : _ ->
        pure (variableClass, oneClass)
      _ ->
        assertFailure "regional WCOJ fixture did not build its child classes"
  withTestGraphFromLattice lattice baseGraph $ \(ownerProxy :: Proxy owner) emptyGraph -> do
    rule <- compileSingleTestRule @owner @SupportContext ownerProxy addZeroProductiveRule
    contextGraph <-
      expectRight "failed to install contextual quotient" $
        contextMerge
          SupportLeft
          variableClass
          oneClass
          (sceContextGraph emptyGraph)
    let graph =
          emptySaturatingContextEGraph contextGraph
        eagerContextInputs =
          Map.fromList
            [ (SupportLeft, (LogicStore.emptyFactStore, emptyFactDerivationIndex, [rule])),
              (SupportRight, (LogicStore.emptyFactStore, emptyFactDerivationIndex, [rule]))
            ]
        lazyContextInputs =
          LazyMap.fromList
            [ (SupportLeft, (LogicStore.emptyFactStore, emptyFactDerivationIndex, [rule])),
              (SupportRight, (LogicStore.emptyFactStore, emptyFactDerivationIndex, [rule]))
            ]
        runWith contextInputs strategy =
          contextSupportedMatchesPrepared @(TestU owner SupportContext)
            (defaultRewriteContext @(TestU owner SupportContext))
            (emptyCapabilityResolver @(TestU owner SupportContext))
            0
            Delta.fullDelta
            graph
            contextInputs
            []
            ( initialMatchState @(TestU owner SupportContext)
                strategy
                (defaultRewriteContext @(TestU owner SupportContext))
            )
    regionalMatches <-
      fmap snd (expectRight "lazy regional WCOJ failed" (runWith lazyContextInputs GenericJoinMatching))
    eagerRegionalMatches <-
      fmap snd (expectRight "eager regional WCOJ failed" (runWith eagerContextInputs GenericJoinMatching))
    oracleMatches <-
      fmap snd (expectRight "per-context oracle failed" (runWith eagerContextInputs GenericJoinPerContextMatching))
    supportedMatchDigest regionalMatches @?= supportedMatchDigest eagerRegionalMatches
    supportedMatchDigest regionalMatches @?= supportedMatchDigest oracleMatches

data UnexpectedContextFactDemand = UnexpectedContextFactDemand
  deriving stock (Show)

instance Exception UnexpectedContextFactDemand

regionalWcojDemandLaw :: Assertion
regionalWcojDemandLaw = do
  lattice <- supportContextLattice
  leftSupport <-
    expectRight "failed to build demand-law left support" $
      supportBasis lattice [SupportLeft]
  (baseGraph, _) <-
    buildTestGraph
      [ testVar "x",
        testAdd (testVar "x") testZero
      ]
  withTestGraphFromLattice lattice baseGraph $ \(ownerProxy :: Proxy owner) graph -> do
    rule <- compileSingleTestRule @owner @SupportContext ownerProxy addZeroProductiveRule
    let contextInputs =
          LazyMap.fromList
            [ (SupportLeft, (LogicStore.emptyFactStore, emptyFactDerivationIndex, [rule])),
              ( SupportRight,
                ( throw UnexpectedContextFactDemand,
                  throw UnexpectedContextFactDemand,
                  []
                )
              )
            ]
    matches <-
      fmap snd $
        expectRight "regional demand-law WCOJ failed" $
          contextSupportedMatchesPrepared @(TestU owner SupportContext)
            (defaultRewriteContext @(TestU owner SupportContext))
            (emptyCapabilityResolver @(TestU owner SupportContext))
            0
            Delta.fullDelta
            graph
            contextInputs
            []
            ( initialMatchState @(TestU owner SupportContext)
                GenericJoinMatching
                (defaultRewriteContext @(TestU owner SupportContext))
            )
    Set.map (\(_, _, supportValue) -> supportValue) (supportedMatchDigest matches)
      @?= Set.singleton leftSupport

regionalWcojDeltaOracleLaw :: Assertion
regionalWcojDeltaOracleLaw = do
  lattice <- supportContextLattice
  leftSupport <-
    expectRight "failed to build left support basis" $
      supportBasis lattice [SupportLeft]
  topSupport <- supportAcrossGenerators lattice
  (baseGraph, classIds) <-
    buildTestGraph
      [ testVar "x",
        testOne,
        testAdd (testVar "x") testOne
      ]
  (variableClass, oneClass) <-
    case classIds of
      variableClass : oneClass : _ ->
        pure (variableClass, oneClass)
      _ ->
        assertFailure "regional delta fixture did not build its child classes"
  withTestGraphFromLattice lattice baseGraph $ \(ownerProxy :: Proxy owner) emptyGraph -> do
    rule <- compileSingleTestRule @owner @SupportContext ownerProxy sameAddChildrenRule
    leftContextGraph <-
      expectRight "failed to install left contextual quotient" $
        contextMerge
          SupportLeft
          variableClass
          oneClass
          (sceContextGraph emptyGraph)
    let leftGraph =
          emptySaturatingContextEGraph leftContextGraph
        contextInputs =
          Map.fromList
            [ (SupportLeft, (LogicStore.emptyFactStore, emptyFactDerivationIndex, [rule])),
              (SupportRight, (LogicStore.emptyFactStore, emptyFactDerivationIndex, [rule]))
            ]
        runWith graph matchingDelta matchState =
          contextSupportedMatchesPrepared @(TestU owner SupportContext)
            (defaultRewriteContext @(TestU owner SupportContext))
            (emptyCapabilityResolver @(TestU owner SupportContext))
            0
            matchingDelta
            graph
            contextInputs
            []
            matchState
    (leftState, leftMatches) <-
      expectRight
        "initial regional WCOJ failed"
        ( runWith
            leftGraph
            Delta.fullDelta
            ( initialMatchState @(TestU owner SupportContext)
                GenericJoinMatching
                (defaultRewriteContext @(TestU owner SupportContext))
            )
        )
    Set.map (\(_, _, supportValue) -> supportValue) (supportedMatchDigest leftMatches)
      @?= Set.singleton leftSupport
    bothContextGraph <-
      expectRight "failed to install right contextual quotient" $
        contextMerge SupportRight variableClass oneClass leftContextGraph
    let bothGraph =
          emptySaturatingContextEGraph bothContextGraph
        matchingDelta =
          matchingDeltaFromTouchedKeys
            (IntSet.fromList [classIdKey variableClass, classIdKey oneClass])
        advancedState =
          advanceMatchStateForRound @(TestU owner SupportContext)
            matchingDelta
            bothGraph
            leftState
    (_deltaState, deltaMatches) <-
      expectRight
        "incremental regional WCOJ failed"
        (runWith bothGraph matchingDelta advancedState)
    (_oracleState, oracleMatches) <-
      expectRight
        "fresh per-context oracle failed"
        ( runWith
            bothGraph
            Delta.fullDelta
            ( initialMatchState @(TestU owner SupportContext)
                GenericJoinPerContextMatching
                (defaultRewriteContext @(TestU owner SupportContext))
            )
        )
    supportedMatchDigest deltaMatches @?= supportedMatchDigest oracleMatches
    Set.map (\(_, _, supportValue) -> supportValue) (supportedMatchDigest deltaMatches)
      @?= Set.singleton topSupport

supportedMatchDigest ::
  [SupportedRewriteMatch SupportContext SurfaceKind TestF] ->
  Set.Set (ClassId, Substitution, SupportBasis SupportContext)
supportedMatchDigest =
  Set.fromList
    . fmap
      (\supportedMatch ->
          let rewriteMatch = srmMatch supportedMatch
           in (ermRootClass rewriteMatch, ermSubstitution rewriteMatch, srmSupport supportedMatch)
      )

supportContextLattice :: IO (ContextLattice SupportContext)
supportContextLattice =
  expectRight "failed to compile support context lattice" $
    contextLatticeFromClosedOrder
      SupportTop
      SupportBottom
      [SupportBottom, SupportLeft, SupportRight, SupportTop]
      supportContextLeq
      supportContextJoin
      supportContextMeet

supportAcrossGenerators :: ContextLattice SupportContext -> IO (SupportBasis SupportContext)
supportAcrossGenerators lattice =
  expectRight "failed to build support generator basis" $
    supportBasis lattice [SupportLeft, SupportRight]

supportContextLeq :: SupportContext -> SupportContext -> Bool
supportContextLeq left right =
  left == right
    || left == SupportBottom
    || right == SupportTop

supportContextJoin :: SupportContext -> SupportContext -> SupportContext
supportContextJoin left right =
  case (left, right) of
    (SupportBottom, value) -> value
    (value, SupportBottom) -> value
    (SupportTop, _) -> SupportTop
    (_, SupportTop) -> SupportTop
    (SupportLeft, SupportLeft) -> SupportLeft
    (SupportRight, SupportRight) -> SupportRight
    _ -> SupportTop

supportContextMeet :: SupportContext -> SupportContext -> SupportContext
supportContextMeet left right =
  case (left, right) of
    (SupportTop, value) -> value
    (value, SupportTop) -> value
    (SupportBottom, _) -> SupportBottom
    (_, SupportBottom) -> SupportBottom
    (SupportLeft, SupportLeft) -> SupportLeft
    (SupportRight, SupportRight) -> SupportRight
    _ -> SupportBottom

cyclicRepairAnalysis :: Either UnionFindAllocationError (Maybe NodeCount)
cyclicRepairAnalysis =
  addTerm (testVar "x") (emptyEGraph testAnalysis) >>= \(xClass, graphWithX) ->
    addTerm (testNum 0) graphWithX >>= \(_, graphWithZero) ->
      addTerm (testMul (testVar "x") (testNum 0)) graphWithZero >>= \(mulClass, cyclicBaseGraph) ->
        let mergedGraph =
              merge xClass mulClass cyclicBaseGraph
            (_rebuildDelta, repairedGraph) =
              rebuildWithDelta mergedGraph
            repairedKey =
              classIdKey (canonicalizeClassId repairedGraph xClass)
         in pure (IntMap.lookup repairedKey (eGraphAnalysis repairedGraph))

rawMatchesForRules ::
  forall owner context.
  (Ord context, Show context) =>
  TestGraph owner context ->
  [TestRule] ->
  IO [RawRewriteMatch SurfaceKind TestF]
rawMatchesForRules graph rules =
  case
    rawBaseMatchesPrepared @(TestU owner context)
      (defaultRewriteContext @(TestU owner context))
      0
      Delta.fullDelta
      graph
      LogicStore.emptyFactStore
      rules
      (initialTestMatchState @owner @context)
    of
      Left obstruction ->
        assertFailure ("raw matching failed: " <> show obstruction)
      Right (_nextState, rawMatches) ->
        pure rawMatches

supportedMatchesForRule ::
  forall owner context.
  (Ord context, Show context) =>
  context ->
  TestGraph owner context ->
  TestRule ->
  IO [SupportedRewriteMatch context SurfaceKind TestF]
supportedMatchesForRule contextValue graph rule = do
  rawMatches <- rawMatchesForRules @owner @context graph [rule]
  expectRight "failed to materialize raw match" $
    traverse
      ( materializeRawMatch @(TestU owner context)
          (defaultRewriteContext @(TestU owner context))
          (emptyCapabilityResolver @(TestU owner context))
          contextValue
          LogicStore.emptyFactStore
          emptyFactDerivationIndex
          (graphBase @(TestU owner context) graph)
      )
      rawMatches

initialTestMatchState ::
  forall owner context.
  Ord context =>
  SatMatchState (TestU owner context)
initialTestMatchState =
  initialMatchState @(TestU owner context)
    GenericJoinMatching
    (defaultRewriteContext @(TestU owner context))

compileSingleTestRule ::
  forall (owner :: Type) context.
  Ord context =>
  Proxy owner ->
  RawRewriteRule (RewriteCondition SurfaceKind TestF) TestF ->
  IO TestRule
compileSingleTestRule ownerProxy rawRule =
  requireSingle "expected one compiled rewrite rule"
    =<< compileTestRules @owner @context ownerProxy [rawRule]

compileTestRules ::
  forall (owner :: Type) context.
  Ord context =>
  Proxy owner ->
  [RawRewriteRule (RewriteCondition SurfaceKind TestF) TestF] ->
  IO [TestRule]
compileTestRules _ rawRules =
  expectRight "failed to compile rewrite rules" $
    compileRewriteRules @(TestU owner context) rawRules

buildTestGraph :: [Fix TestF] -> IO (EGraph TestF NodeCount, [ClassId])
buildTestGraph terms = do
  EGraphMutationResult
    { emrResult = classIds,
      emrGraph = graph
    } <- expectRight "failed to allocate test graph classes" (insertTermsTracked terms (emptyEGraph testAnalysis))
  pure (graph, classIds)

withTestGraphFromLattice ::
  Ord context =>
  ContextLattice context ->
  EGraph TestF NodeCount ->
  (forall owner. Proxy owner -> TestGraph owner context -> result) ->
  result
withTestGraphFromLattice lattice baseGraph useGraph =
  withEmptyContextEGraph lattice baseGraph $ \contextGraph ->
    useGraph Proxy (emptySaturatingContextEGraph contextGraph)

rootsByRule ::
  [RawRewriteMatch SurfaceKind TestF] ->
  RewriteRuleId ->
  Set.Set ClassId
rootsByRule rawMatches ruleId =
  Set.fromList
    [ rrmRootClass rawMatch
    | rawMatch <- rawMatches,
      rpId (rrmRule rawMatch) == ruleId
    ]

expectRight :: Show err => String -> Either err value -> IO value
expectRight _ (Right value) =
  pure value
expectRight label (Left err) =
  assertFailure (label <> ": " <> show err)

requireSingle :: String -> [value] -> IO value
requireSingle _ [value] =
  pure value
requireSingle label values =
  assertFailure (label <> ", saw " <> show (length values))

noOpAddZeroRule :: RawRewriteRule (RewriteCondition SurfaceKind TestF) TestF
noOpAddZeroRule =
  testRewrite 101 addZeroPattern addZeroPattern

addZeroProductiveRule :: RawRewriteRule (RewriteCondition SurfaceKind TestF) TestF
addZeroProductiveRule =
  testRewrite 102 addZeroPattern (PatternVar xVar)

sameAddLhsLeftRule :: RawRewriteRule (RewriteCondition SurfaceKind TestF) TestF
sameAddLhsLeftRule =
  testRewrite 201 addXYPattern (PatternVar xVar)

sameAddLhsRightRule :: RawRewriteRule (RewriteCondition SurfaceKind TestF) TestF
sameAddLhsRightRule =
  testRewrite 202 addXYPattern (PatternVar yVar)

sameAddChildrenRule :: RawRewriteRule (RewriteCondition SurfaceKind TestF) TestF
sameAddChildrenRule =
  testRewrite 301 sameAddChildrenPattern (PatternVar xVar)

testRewrite ::
  Int ->
  Pattern TestF ->
  Pattern TestF ->
  RawRewriteRule (RewriteCondition SurfaceKind TestF) TestF
testRewrite ruleId lhsPattern rhsPattern =
  RawRewriteRule
    { rrId = RewriteRuleId ruleId,
      rrLhs = lhsPattern,
      rrRhs = rhsPattern,
      rrCondition = Nothing,
      rrApplicationCondition = Nothing,
      rrPostSubst = Nothing
    }

addZeroPattern :: Pattern TestF
addZeroPattern =
  PatternNode (Add (PatternVar xVar) (PatternNode Zero))

addXYPattern :: Pattern TestF
addXYPattern =
  PatternNode (Add (PatternVar xVar) (PatternVar yVar))

sameAddChildrenPattern :: Pattern TestF
sameAddChildrenPattern =
  PatternNode (Add (PatternVar xVar) (PatternVar xVar))

xVar :: PatternVar
xVar =
  mkPatternVar 0

yVar :: PatternVar
yVar =
  mkPatternVar 1
