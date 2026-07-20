module Moonlight.EGraph.Rewrite.PresheafPathSpec
  ( tests,
  )
where

import Data.List ( sort )
import Moonlight.EGraph.Effect.CoveringSurface
  ( SurfaceKind,
  )
import Moonlight.EGraph.Pure.Query.RootFilter
    ( RootClassFilter(RestrictedRootClasses, AllRootClasses) )
import Moonlight.EGraph.Pure.Kernel.HashCons ( addTerm )
import Moonlight.EGraph.Pure.Rebuild ( merge, rebuild )
import Moonlight.EGraph.Pure.Relational
    ( atomizeCompiledPatternQuery,
      compiledPatternQueryFingerprint,
      emptyEGraphPreparedMatchState,
      wcojPreparedMatchCompiledWithRootFilter,
      wcojMatchCompiledWithRootFilter )
import Moonlight.EGraph.Pure.Types ( emptyEGraph )
import Moonlight.EGraph.Test.Arith.Core
    ( ArithF(..), addTermNode, analysisSpec, mulTermNode, numTerm )
import Moonlight.EGraph.Saturation.Context.State
    ( ContextSaturationState,
      cssQueryRegistry,
      emptyContextSaturationState )
import Moonlight.Rewrite.System
    ( combineCompiledGuards, compileGuard )
import Moonlight.Core
import Moonlight.Core qualified as EGraph
import Moonlight.Rewrite.Algebra
    ( compilePatternQuery, singlePatternQuery )
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit
    ( (@?=), assertBool, assertFailure, testCase )
import Data.IntSet qualified as IntSet ( singleton )
import Moonlight.Flow.Plan.Query.Core qualified as RelPlan
    ( qpAtoms, qpFingerprint )
import Data.Set qualified as Set ( singleton, fromList, toList )
import Moonlight.Saturation.Context.Match.State.Registry qualified as SaturationMatch
    ( lookupQueryIdByFingerprint,
      registerQueryFingerprint,
      registeredQueryIds )
import Moonlight.Saturation.Matching ( QueryFingerprint(..) )
import Data.Vector qualified as Vector ( length )
import Moonlight.Pale.Test.Site.Assertion (expectRight)

tests :: TestTree
tests =
  testGroup
    "PresheafPath"
    [ testCase "shared engine finds Add matches" $ do
        (_, graph1) <- expectRight (addTerm (addTermNode (numTerm 1) (numTerm 0)) (emptyEGraph analysisSpec))
        (_, graph2) <- expectRight (addTerm (mulTermNode (numTerm 2) (numTerm 3)) graph1)
        let graph3 = rebuild graph2
            patAdd = PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1)))
        case compilePatternQuery (combineCompiledGuards @SurfaceKind) compileGuard (singlePatternQuery patAdd) of
              Left unbound -> assertFailure ("pattern compilation failed: " <> show unbound)
              Right compiledQuery -> do
                matches <- expectRight (wcojMatchCompiledWithRootFilter AllRootClasses compiledQuery graph3)
                let matchRoots = sort (fmap (\(cid, _) -> classIdKey cid) matches)
                assertBool "should find at least one Add match" (not (null matchRoots)),
      testCase "shared engine finds Mul matches" $ do
        (_, graph1) <- expectRight (addTerm (mulTermNode (numTerm 2) (numTerm 3)) (emptyEGraph analysisSpec))
        let graph2 = rebuild graph1
            patMul = PatternNode (Mul (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1)))
        case compilePatternQuery (combineCompiledGuards @SurfaceKind) compileGuard (singlePatternQuery patMul) of
              Left unbound -> assertFailure ("pattern compilation failed: " <> show unbound)
              Right compiledQuery -> do
                matches <- expectRight (wcojMatchCompiledWithRootFilter AllRootClasses compiledQuery graph2)
                assertBool "should find Mul match" (not (null matches)),
      testCase "shared engine respects root class filter" $ do
        (c1, graph1) <- expectRight (addTerm (addTermNode (numTerm 1) (numTerm 0)) (emptyEGraph analysisSpec))
        (_, graph2) <- expectRight (addTerm (addTermNode (numTerm 2) (numTerm 3)) graph1)
        let graph3 = rebuild graph2
            patAdd = PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1)))
        case compilePatternQuery (combineCompiledGuards @SurfaceKind) compileGuard (singlePatternQuery patAdd) of
              Left unbound -> assertFailure ("pattern compilation failed: " <> show unbound)
              Right compiledQuery -> do
                allMatches <- expectRight (wcojMatchCompiledWithRootFilter AllRootClasses compiledQuery graph3)
                restrictedMatches <-
                  expectRight
                    ( wcojMatchCompiledWithRootFilter
                        (RestrictedRootClasses (IntSet.singleton (classIdKey c1)))
                        compiledQuery
                        graph3
                    )
                assertBool "all matches should find two" (length allMatches >= 2)
                assertBool "restricted should find fewer" (length restrictedMatches < length allMatches),
      testCase "compiled variable query enumerates roots even without structural atoms" $ do
        (c1, graph1) <- expectRight (addTerm (numTerm 1) (emptyEGraph analysisSpec))
        (c2, graph2) <- expectRight (addTerm (numTerm 2) graph1)
        let graph3 = rebuild graph2
            patVar :: Pattern ArithF
            patVar = PatternVar (EGraph.mkPatternVar 0)
        case compilePatternQuery (combineCompiledGuards @SurfaceKind) compileGuard (singlePatternQuery patVar) of
              Left unbound -> assertFailure ("pattern compilation failed: " <> show unbound)
              Right compiledQuery -> do
                allMatches <- expectRight (wcojMatchCompiledWithRootFilter AllRootClasses compiledQuery graph3)
                restrictedMatches <-
                  expectRight
                    ( wcojMatchCompiledWithRootFilter
                        (RestrictedRootClasses (IntSet.singleton (classIdKey c1)))
                        compiledQuery
                        graph3
                    )
                let allRoots = Set.fromList (fmap fst allMatches)
                    restrictedRoots = Set.fromList (fmap fst restrictedMatches)
                allRoots @?= Set.fromList [c1, c2]
                restrictedRoots @?= Set.singleton c1,
      testCase "atomize produces correct atom count for binary node" $
        let patAdd = PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1)))
         in case compilePatternQuery (combineCompiledGuards @SurfaceKind) compileGuard (singlePatternQuery patAdd) of
              Left unbound -> assertFailure ("pattern compilation failed: " <> show unbound)
              Right compiledQuery -> do
                plan <- expectRight (atomizeCompiledPatternQuery compiledQuery)
                Vector.length (RelPlan.qpAtoms plan) @?= 1,
      testCase "atomize produces correct atom count for nested pattern" $
        let patNested = PatternNode (Add (PatternNode (Mul (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1)))) (PatternVar (EGraph.mkPatternVar 2)))
         in case compilePatternQuery (combineCompiledGuards @SurfaceKind) compileGuard (singlePatternQuery patNested) of
              Left unbound -> assertFailure ("pattern compilation failed: " <> show unbound)
              Right compiledQuery -> do
                plan <- expectRight (atomizeCompiledPatternQuery compiledQuery)
                Vector.length (RelPlan.qpAtoms plan) @?= 2,
      testCase "query registry preserves direct fingerprint identity" $
        let saturationState :: ContextSaturationState SurfaceKind ArithF
            saturationState = emptyContextSaturationState
            patAdd = PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1)))
            patMul = PatternNode (Mul (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1)))
            compile = compilePatternQuery (combineCompiledGuards @SurfaceKind) compileGuard . singlePatternQuery
         in case traverse compile [patAdd, patMul] of
              Left unbound -> assertFailure ("pattern compilation failed: " <> show unbound)
              Right [compiledAdd, compiledMul] -> do
                addPlan <- expectRight (atomizeCompiledPatternQuery compiledAdd)
                addFingerprint <- fmap QueryFingerprint (expectRight (compiledPatternQueryFingerprint compiledAdd))
                mulFingerprint <- fmap QueryFingerprint (expectRight (compiledPatternQueryFingerprint compiledMul))
                addFingerprint @?= QueryFingerprint (RelPlan.qpFingerprint addPlan)
                let initialRegistry = cssQueryRegistry saturationState
                    (addId, addRegistry) = SaturationMatch.registerQueryFingerprint addFingerprint initialRegistry
                    (duplicateAddId, duplicateRegistry) = SaturationMatch.registerQueryFingerprint addFingerprint addRegistry
                    (mulId, registeredRegistry) = SaturationMatch.registerQueryFingerprint mulFingerprint duplicateRegistry
                duplicateAddId @?= addId
                addId @?= mkQueryId 0
                mulId @?= mkQueryId 1
                SaturationMatch.registeredQueryIds registeredRegistry @?= [addId, mulId]
                SaturationMatch.lookupQueryIdByFingerprint addFingerprint registeredRegistry @?= Just addId
                SaturationMatch.lookupQueryIdByFingerprint mulFingerprint registeredRegistry @?= Just mulId
              Right compiledQueries ->
                assertFailure ("expected exactly two compiled queries, got " <> show (length compiledQueries)),
      testCase "shared engine equivalence after merge" $ do
        (c1, graph1) <- expectRight (addTerm (numTerm 1) (emptyEGraph analysisSpec))
        (c2, graph2) <- expectRight (addTerm (numTerm 2) graph1)
        (_, graph3) <- expectRight (addTerm (addTermNode (numTerm 1) (numTerm 2)) graph2)
        let graph4 = rebuild (merge c1 c2 graph3)
            patAdd = PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1)))
        case compilePatternQuery (combineCompiledGuards @SurfaceKind) compileGuard (singlePatternQuery patAdd) of
              Left unbound -> assertFailure ("pattern compilation failed: " <> show unbound)
              Right compiledQuery -> do
                matches <- expectRight (wcojMatchCompiledWithRootFilter AllRootClasses compiledQuery graph4)
                assertBool "should find matches after merge" (not (null matches))
    , testCase "cold relational matcher agrees with prepared relational matcher on shared-prefix nested query" $ do
        let sharedMul =
              mulTermNode
                (numTerm 1)
                (numTerm 2)
        (_, graph1) <- expectRight (addTerm sharedMul (emptyEGraph analysisSpec))
        (leftRoot, graph2) <- expectRight (addTerm (addTermNode sharedMul (numTerm 0)) graph1)
        (rightRoot, graph3) <- expectRight (addTerm (addTermNode sharedMul (numTerm 1)) graph2)
        (_, graph4) <- expectRight (addTerm (addTermNode (numTerm 4) (numTerm 5)) graph3)
        let graph5 = rebuild graph4
            nestedPattern =
              PatternNode
                ( Add
                    (PatternNode (Mul (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1))))
                    (PatternVar (EGraph.mkPatternVar 2))
                )
        case compilePatternQuery (combineCompiledGuards @SurfaceKind) compileGuard (singlePatternQuery nestedPattern) of
              Left unbound -> assertFailure ("pattern compilation failed: " <> show unbound)
              Right compiledQuery -> do
                relationalMatches <- expectRight (wcojMatchCompiledWithRootFilter AllRootClasses compiledQuery graph5)
                (_preparedState, preparedMatches) <-
                  expectRight
                    ( wcojPreparedMatchCompiledWithRootFilter
                        AllRootClasses
                        compiledQuery
                        graph5
                        emptyEGraphPreparedMatchState
                    )
                let sharedMatches =
                      Set.fromList relationalMatches
                    preparedMatchSet =
                      Set.fromList preparedMatches
                    expectedRoots =
                      Set.fromList [leftRoot, rightRoot]
                    actualRoots =
                      Set.fromList (fmap fst (Set.toList sharedMatches))
                sharedMatches @?= preparedMatchSet
                actualRoots @?= expectedRoots
    , testCase "shared relational matcher respects restricted roots on shared-prefix nested query" $ do
        let sharedMul =
              mulTermNode
                (numTerm 1)
                (numTerm 2)
        (_, graph1) <- expectRight (addTerm sharedMul (emptyEGraph analysisSpec))
        (leftRoot, graph2) <- expectRight (addTerm (addTermNode sharedMul (numTerm 0)) graph1)
        (_, graph3) <- expectRight (addTerm (addTermNode sharedMul (numTerm 1)) graph2)
        let graph4 = rebuild graph3
            nestedPattern =
              PatternNode
                ( Add
                    (PatternNode (Mul (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1))))
                    (PatternVar (EGraph.mkPatternVar 2))
                )
            rootFilter =
              RestrictedRootClasses (IntSet.singleton (classIdKey leftRoot))
        case compilePatternQuery (combineCompiledGuards @SurfaceKind) compileGuard (singlePatternQuery nestedPattern) of
              Left unbound -> assertFailure ("pattern compilation failed: " <> show unbound)
              Right compiledQuery -> do
                relationalMatches <- expectRight (wcojMatchCompiledWithRootFilter rootFilter compiledQuery graph4)
                (_preparedState, preparedMatches) <-
                  expectRight
                    ( wcojPreparedMatchCompiledWithRootFilter
                        rootFilter
                        compiledQuery
                        graph4
                        emptyEGraphPreparedMatchState
                    )
                Set.fromList relationalMatches
                  @?= Set.fromList preparedMatches
    ]
