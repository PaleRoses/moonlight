{-# LANGUAGE ExplicitNamespaces #-}
module Moonlight.EGraph.Rewrite.RewriteSpec
  ( tests,
  )
where

import Moonlight.EGraph.Effect.CoveringSurface
    ( SurfaceKind(Matching) )
import Moonlight.EGraph.Pure.Context
    ( ContextEGraph,
      ContextMutationTrace (..),
      contextMutationTraceEffect,
      emptyContextEGraph )
import Moonlight.EGraph.Pure.Change
    ( EGraphMutationResult (..),
      eGraphMutationTraceEffect,
      emtObservedClassUnions,
      emtTouchedClassKeys,
      observedClassUnionPairs )
import Moonlight.EGraph.Pure.Context.Proof
    ( serializeProofLog )
import Moonlight.EGraph.Saturation.Context.State
    ( SaturatingProofEGraph,
      emptySaturatingProofEGraph,
      emptySaturatingProofEGraphWithRetention )
import Moonlight.EGraph.Pure.Kernel.HashCons ( addTerm )
import Moonlight.Core ( find )
import Moonlight.EGraph.Pure.Rebuild ( rebuild )
import Moonlight.EGraph.Pure.Relational
    ( wcojMatchCompiledWithRoots )
import Moonlight.EGraph.Pure.Rewrite.Env
    ( EGraphRewriteEnv(..),
      emptyEGraphRewriteEnv )
import Moonlight.EGraph.Pure.Rewrite.Program
    ( RewriteProgramPreview (..),
      commitRewriteProgramPreview,
      runRewriteProgramEGraphPreview,
      runExecutableRewriteMatchEGraphPreview,
      runExecutableRewriteMatchesEGraphCommitted )
import Moonlight.EGraph.Pure.Types
    ( EGraph,
      eGraphPendingClassUnions,
      eGraphUnionFind,
      emptyEGraph )
import Moonlight.EGraph.Test.Arith.Core
    ( ArithF(..), NodeCount, addTermNode, analysisSpec, numTerm )
import Moonlight.EGraph.Pure.Saturation.Apply
    ( EGraphApplicationResult (..),
      EGraphRewriteApplicationError,
      ProofTraceProjectionError (..),
      proofUpdateFromTrace,
      applySupportedProofRewritesReported )
import Moonlight.Rewrite.Runtime
    ( ExecutableRewriteMatch(..) )
import Moonlight.Rewrite.ProofContext
    ( SupportMatchWitness(..),
      SupportedRewriteMatch(..) )
import Moonlight.Rewrite.System
  ( CompiledGuard,
    emptyGuardCapabilityResolver,
    GuardCapabilityResolver (..),
    GuardEvidence(..),
    RewriteCondition(..),
    data GuardRoot,
    data GuardVar,
    guardHasCapability )
import Moonlight.Rewrite.System
    ( emptyFactStore )
import Moonlight.Rewrite.System
    ( emptyFactDerivationIndex )
import Moonlight.Core hiding
    ( addCanonicalNode,
      canonicalizeClass,
      eGraphProgramEffectCount,
      mergeClasses )
import Moonlight.Core.EGraph.Program
    ( addCanonicalNode,
      canonicalizeClass,
      eGraphProgramEffectCount,
      mergeClasses )
import Moonlight.Core qualified as EGraph
import Moonlight.EGraph.Pure.Saturation.Substrate
    ( EGraphU,
      RawRewriteMatch (..) )
import Moonlight.Rewrite.Runtime
  ( RulePlan (..),
    RewriteApplicationError (..)
  )
import Moonlight.Rewrite.System (RawRewriteRule (..))
import Moonlight.Saturation.Substrate
    ( compileRewriteRules,
      materializeRawMatch )
import Moonlight.Rewrite.Runtime
    ( emptyRewriteRuntimeCapabilities,
      withRuntimeGuardCapabilityResolver )
import Moonlight.Rewrite.ProofContext
    ( ProofAnnotationBuilder(..),
      ProofKind(..),
      ProofRetention(KeepNoProof),
      ProofStep(..) )
import Moonlight.Core (Substitution)
import Moonlight.EGraph.Test.Context.ThreeLevel
    ( Scope(GlobalCtx) )
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( (@?=), assertBool, assertFailure, testCase )
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Moonlight.FiniteLattice
  ( principalSupport
  )
import Moonlight.Pale.Test.Site.Assertion (expectRight)
import Moonlight.FiniteLattice
  ( ContextLattice,
    latticeContext
  )

tests :: TestTree
tests =
  testGroup
    "rewrite"
    [ testCase "applyRewrite honors capability guards" $ do
        (oneClassId, graph1) <- expectRight (addTerm (numTerm 1) (emptyEGraph analysisSpec))
        (_, graph2) <- expectRight (addTerm (numTerm 0) graph1)
        (sumClassId, graph3) <- expectRight (addTerm (addTermNode (numTerm 1) (numTerm 0)) graph2)
        let capabilityResolver =
              GuardCapabilityResolver
                (\surfaceKind classIds -> surfaceKind == Matching && not (null classIds))
            rawRewrite :: RawRewriteRule (RewriteCondition SurfaceKind ArithF) ArithF
            rawRewrite =
              RawRewriteRule
                { rrId = RewriteRuleId 3,
                  rrLhs = PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternNode (Num 0))),
                  rrRhs = PatternVar (EGraph.mkPatternVar 0),
                  rrCondition = Just (RewriteCondition (guardHasCapability Matching [GuardRoot, GuardVar (EGraph.mkPatternVar 0)])),
                  rrApplicationCondition = Nothing,
                  rrPostSubst = Nothing
                }
        compiledRewrite <- expectCompiledRewrite rawRewrite
        (_, substitution) <- expectSingletonMatch =<< expectRelationalMatches compiledRewrite graph3
        rewriteProgramPreview <-
                expectAppliedRewrite
                  ( runExecutableRewriteMatchEGraphPreview
                      ( emptyEGraphRewriteEnv
                          { ereRuntimeCapabilities =
                              withRuntimeGuardCapabilityResolver
                                capabilityResolver
                                (ereRuntimeCapabilities emptyEGraphRewriteEnv)
                          }
                      )
                      (ExecutableRewriteMatch compiledRewrite sumClassId Nothing Nothing substitution)
                      graph3
                  )
        length (observedClassUnionPairs (rppPlannedClassUnions rewriteProgramPreview)) @?= 1
        length (eGraphPendingClassUnions (rppPreviewGraph rewriteProgramPreview)) @?= 0
        let rewriteCommit =
                    commitRewriteProgramPreview rewriteProgramPreview
        length (observedClassUnionPairs (emtObservedClassUnions (emrTrace rewriteCommit))) @?= 1
        length (eGraphPendingClassUnions (emrGraph rewriteCommit)) @?= 1
        let rebuiltGraph = rebuild (emrGraph rewriteCommit)
            (sumRootClassId, unionFindAfterSum) = find sumClassId (eGraphUnionFind rebuiltGraph)
            (oneRootClassId, _) = find oneClassId unionFindAfterSum
        sumRootClassId @?= oneRootClassId
    , testCase "applyRewrite rejects stale guard evidence without validation" $ do
        (_, graph1) <- expectRight (addTerm (numTerm 1) (emptyEGraph analysisSpec))
        (_, graph2) <- expectRight (addTerm (numTerm 0) graph1)
        (sumClassId, graph3) <- expectRight (addTerm (addTermNode (numTerm 1) (numTerm 0)) graph2)
        let rawRewrite :: RawRewriteRule (RewriteCondition SurfaceKind ArithF) ArithF
            rawRewrite =
              RawRewriteRule
                { rrId = RewriteRuleId 5,
                  rrLhs = PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternNode (Num 0))),
                  rrRhs = PatternVar (EGraph.mkPatternVar 0),
                  rrCondition = Just (RewriteCondition (guardHasCapability Matching [GuardRoot, GuardVar (EGraph.mkPatternVar 0)])),
                  rrApplicationCondition = Nothing,
                  rrPostSubst = Nothing
                }
        compiledRewrite <- expectCompiledRewrite rawRewrite
        (_, substitution) <- expectSingletonMatch =<< expectRelationalMatches compiledRewrite graph3
        case
                runExecutableRewriteMatchEGraphPreview
                  emptyEGraphRewriteEnv
                  (ExecutableRewriteMatch compiledRewrite sumClassId (Just (GuardEvidence [] Set.empty)) Nothing substitution)
                  graph3 of
                Left (RewriteConditionRejected (RewriteRuleId 5)) -> pure ()
                Left rewriteError -> assertFailure ("expected guard rejection, got " <> show rewriteError)
                Right _ -> assertFailure "stale guard evidence must not bypass validation"
    , testCase "raw rewrite materialization returns typed guard rejection" $ do
        (_, graph1) <- expectRight (addTerm (numTerm 1) (emptyEGraph analysisSpec))
        (_, graph2) <- expectRight (addTerm (numTerm 0) graph1)
        (sumClassId, graph3) <- expectRight (addTerm (addTermNode (numTerm 1) (numTerm 0)) graph2)
        let rawRewrite :: RawRewriteRule (RewriteCondition SurfaceKind ArithF) ArithF
            rawRewrite =
              RawRewriteRule
                { rrId = RewriteRuleId 7,
                  rrLhs = PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternNode (Num 0))),
                  rrRhs = PatternVar (EGraph.mkPatternVar 0),
                  rrCondition = Just (RewriteCondition (guardHasCapability Matching [GuardRoot, GuardVar (EGraph.mkPatternVar 0)])),
                  rrApplicationCondition = Nothing,
                  rrPostSubst = Nothing
                }
        compiledRewrite <- expectCompiledRewrite rawRewrite
        (_, substitution) <- expectSingletonMatch =<< expectRelationalMatches compiledRewrite graph3
        case
                materializeRawMatch @(EGraphU SurfaceKind ArithF NodeCount ())
                  emptyRewriteRuntimeCapabilities
                  emptyGuardCapabilityResolver
                  ()
                  emptyFactStore
                  emptyFactDerivationIndex
                  graph3
                  RawRewriteMatch
                    { rrmRule = compiledRewrite,
                      rrmRootClass = sumClassId,
                      rrmSubstitution = substitution
                    } of
                Left (RewriteConditionRejected (RewriteRuleId 7)) -> pure ()
                Left rewriteError -> assertFailure ("expected typed guard rejection, got " <> show rewriteError)
                Right _ -> assertFailure "guard rejection must not materialize a supported match"
    , testCase "raw rewrite materialization preserves unguarded support witness" $ do
        (_, graph1) <- expectRight (addTerm (numTerm 1) (emptyEGraph analysisSpec))
        (_, graph2) <- expectRight (addTerm (numTerm 0) graph1)
        (sumClassId, graph3) <- expectRight (addTerm (addTermNode (numTerm 1) (numTerm 0)) graph2)
        let rawRewrite :: RawRewriteRule (RewriteCondition SurfaceKind ArithF) ArithF
            rawRewrite =
              RawRewriteRule
                { rrId = RewriteRuleId 8,
                  rrLhs = PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternNode (Num 0))),
                  rrRhs = PatternVar (EGraph.mkPatternVar 0),
                  rrCondition = Nothing,
                  rrApplicationCondition = Nothing,
                  rrPostSubst = Nothing
                }
        compiledRewrite <- expectCompiledRewrite rawRewrite
        (_, substitution) <- expectSingletonMatch =<< expectRelationalMatches compiledRewrite graph3
        supportedMatch <-
                expectRawMaterialized
                  ( materializeRawMatch @(EGraphU SurfaceKind ArithF NodeCount ())
                      emptyRewriteRuntimeCapabilities
                      emptyGuardCapabilityResolver
                      ()
                      emptyFactStore
                      emptyFactDerivationIndex
                      graph3
                      RawRewriteMatch
                        { rrmRule = compiledRewrite,
                          rrmRootClass = sumClassId,
                          rrmSubstitution = substitution
                        }
                  )
        ermRootClass (srmMatch supportedMatch) @?= sumClassId
        ermGuardEvidence (srmMatch supportedMatch) @?= Nothing
        Map.keys (srmWitnesses supportedMatch) @?= [()]
    , testCase "preview rejects graph reads after planned merge" $ do
        (oneClassId, graph1) <- expectRight (addTerm (numTerm 1) (emptyEGraph analysisSpec))
        (zeroClassId, graph2) <- expectRight (addTerm (numTerm 0) graph1)
        case runRewriteProgramEGraphPreview (mergeClasses oneClassId zeroClassId *> canonicalizeClass oneClassId) graph2 of
                Left RewriteProgramReadAfterMerge -> pure ()
                Left rewriteError -> assertFailure ("expected read-after-merge rejection, got " <> show rewriteError)
                Right _ -> assertFailure "canonicalize after merge must not preview as if the merge were visible"
        case runRewriteProgramEGraphPreview (mergeClasses oneClassId zeroClassId *> addCanonicalNode (Add oneClassId zeroClassId)) graph2 of
                Left RewriteProgramReadAfterMerge -> pure ()
                Left rewriteError -> assertFailure ("expected add-node read-after-merge rejection, got " <> show rewriteError)
                Right _ -> assertFailure "add-node after merge must not preview as if the merge were visible"
    , testCase "preview accepts merge-only suffixes after construction closes" $ do
        (oneClassId, graph1) <- expectRight (addTerm (numTerm 1) (emptyEGraph analysisSpec))
        (twoClassId, graph2) <- expectRight (addTerm (numTerm 2) graph1)
        (_, graph3) <- expectRight (addTerm (numTerm 0) graph2)
        (oneSumClassId, graph4) <- expectRight (addTerm (addTermNode (numTerm 1) (numTerm 0)) graph3)
        (twoSumClassId, graph5) <- expectRight (addTerm (addTermNode (numTerm 2) (numTerm 0)) graph4)
        rewriteProgramPreview <-
                expectAppliedRewrite
                  ( runRewriteProgramEGraphPreview
                      (mergeClasses oneSumClassId oneClassId *> mergeClasses twoSumClassId twoClassId)
                      graph5
                  )
        length (observedClassUnionPairs (rppPlannedClassUnions rewriteProgramPreview)) @?= 2
        length (eGraphPendingClassUnions (rppPreviewGraph rewriteProgramPreview)) @?= 0
        let rewriteCommit =
                    commitRewriteProgramPreview rewriteProgramPreview
        length (observedClassUnionPairs (emtObservedClassUnions (emrTrace rewriteCommit))) @?= 2
        length (eGraphPendingClassUnions (emrGraph rewriteCommit)) @?= 2
    , testCase "core executable rewrite programs accumulate RHS-variable rewrite merges" $ do
        (oneClassId, graph1) <- expectRight (addTerm (numTerm 1) (emptyEGraph analysisSpec))
        (twoClassId, graph2) <- expectRight (addTerm (numTerm 2) graph1)
        (_, graph3) <- expectRight (addTerm (numTerm 0) graph2)
        (oneSumClassId, graph4) <- expectRight (addTerm (addTermNode (numTerm 1) (numTerm 0)) graph3)
        (twoSumClassId, graph5) <- expectRight (addTerm (addTermNode (numTerm 2) (numTerm 0)) graph4)
        let rawRewrite :: RawRewriteRule (RewriteCondition SurfaceKind ArithF) ArithF
            rawRewrite =
              RawRewriteRule
                { rrId = RewriteRuleId 4,
                  rrLhs = PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternNode (Num 0))),
                  rrRhs = PatternVar (EGraph.mkPatternVar 0),
                  rrCondition = Nothing,
                  rrApplicationCondition = Nothing,
                  rrPostSubst = Nothing
                }
        compiledRewrite <- expectCompiledRewrite rawRewrite
        relationalMatches <- expectRelationalMatches compiledRewrite graph5
        let rewriteMatches =
              fmap
                ( \(rootClassId, substitution) ->
                    ExecutableRewriteMatch compiledRewrite rootClassId Nothing Nothing substitution
                )
                relationalMatches
        length rewriteMatches @?= 2
        EGraphMutationResult
          { emrTrace = applicationTrace,
            emrGraph = graphWithDelta
          } <-
          expectAppliedRewrite
            ( runExecutableRewriteMatchesEGraphCommitted
                emptyEGraphRewriteEnv
                rewriteMatches
                graph5
            )
        eGraphProgramEffectCount (eGraphMutationTraceEffect applicationTrace) @?= 2
        emtTouchedClassKeys applicationTrace
          @?= IntSet.fromList
            [ classIdKey oneClassId,
              classIdKey oneSumClassId,
              classIdKey twoClassId,
              classIdKey twoSumClassId
            ]
        length (observedClassUnionPairs (emtObservedClassUnions applicationTrace)) @?= 2
        length (eGraphPendingClassUnions graphWithDelta) @?= 2
        let rebuiltGraph = rebuild graphWithDelta
            (oneRootClassId, unionFindAfterOne) = find oneClassId (eGraphUnionFind rebuiltGraph)
            (oneSumRootClassId, unionFindAfterOneSum) = find oneSumClassId unionFindAfterOne
            (twoRootClassId, unionFindAfterTwo) = find twoClassId unionFindAfterOneSum
            (twoSumRootClassId, _) = find twoSumClassId unionFindAfterTwo
        oneSumRootClassId @?= oneRootClassId
        twoSumRootClassId @?= twoRootClassId
    , testCase "proof-supported apply reports trace effect, proof projection, and witnesses" $ do
        (_, graph1) <- expectRight (addTerm (numTerm 1) (emptyEGraph analysisSpec))
        (_, graph2) <- expectRight (addTerm (numTerm 0) graph1)
        (sumClassId, graph3) <- expectRight (addTerm (addTermNode (numTerm 1) (numTerm 0)) graph2)
        let rawRewrite :: RawRewriteRule (RewriteCondition SurfaceKind ArithF) ArithF
            rawRewrite =
              RawRewriteRule
                { rrId = RewriteRuleId 6,
                  rrLhs = PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternNode (Num 0))),
                  rrRhs = PatternVar (EGraph.mkPatternVar 0),
                  rrCondition = Nothing,
                  rrApplicationCondition = Nothing,
                  rrPostSubst = Nothing
                }
        compiledRewrite <- expectCompiledRewrite rawRewrite
        (_, substitution) <- expectSingletonMatch =<< expectRelationalMatches compiledRewrite graph3
        let rewriteMatch =
              ExecutableRewriteMatch compiledRewrite sumClassId Nothing Nothing substitution
            supportedRewriteMatch =
              SupportedRewriteMatch
                { srmMatch = rewriteMatch,
                  srmSupport = principalSupport GlobalCtx,
                  srmWitnesses =
                    Map.singleton
                      GlobalCtx
                      SupportMatchWitness
                        { smwFactStore = emptyFactStore,
                          smwFactDerivations = Set.empty,
                          smwGuardEvidence = Nothing,
                          smwGuideEvidence = Nothing
                        }
                }
            contextGraph =
              emptyContextEGraph rewriteScopeLattice graph3 :: ContextEGraph ArithF NodeCount Scope
            proofGraph =
              emptySaturatingProofEGraph contextGraph :: SaturatingProofEGraph SurfaceKind ArithF NodeCount Scope ()
            proofAnnotationBuilder :: ProofAnnotationBuilder Scope ()
            proofAnnotationBuilder =
              ProofAnnotationBuilder (const ())
        (updatedProofGraph, applicationResult) <-
          expectProofApply
            ( applySupportedProofRewritesReported
                (Just GlobalCtx)
                emptyRewriteRuntimeCapabilities
                proofAnnotationBuilder
                [supportedRewriteMatch]
                proofGraph
            )
        let EGraphApplicationResult
              { egarTrace = applicationTrace
              } = applicationResult
        eGraphProgramEffectCount (contextMutationTraceEffect applicationTrace) @?= 1
        length (observedClassUnionPairs (emtObservedClassUnions (cmtBaseTrace applicationTrace))) @?= 1
        egarProofRestrictionRegistryConstructions applicationResult @?= 1
        egarProofExtractionTableConstructions applicationResult @?= 1
        (sharedWitnessProofGraph, sharedWitnessApplicationResult) <-
          expectProofApply
            ( applySupportedProofRewritesReported
                (Just GlobalCtx)
                emptyRewriteRuntimeCapabilities
                proofAnnotationBuilder
                [supportedRewriteMatch, supportedRewriteMatch]
                proofGraph
            )
        egarProofRestrictionRegistryConstructions sharedWitnessApplicationResult @?= 1
        egarProofExtractionTableConstructions sharedWitnessApplicationResult @?= 1
        fmap psKind (serializeProofLog sharedWitnessProofGraph)
          @?= replicate 2 (ProofRewrite (RewriteRuleId 6))
        (noProofGraph, noProofApplicationResult) <-
          expectProofApply
            ( applySupportedProofRewritesReported
                (Just GlobalCtx)
                emptyRewriteRuntimeCapabilities
                proofAnnotationBuilder
                [supportedRewriteMatch]
                (emptySaturatingProofEGraphWithRetention KeepNoProof contextGraph)
            )
        egarProofRestrictionRegistryConstructions noProofApplicationResult @?= 0
        egarProofExtractionTableConstructions noProofApplicationResult @?= 0
        assertBool "KeepNoProof retains no proof steps" (null (serializeProofLog noProofGraph))
        case proofUpdateFromTrace applicationTrace proofGraph of
          Left (ProofTraceProjectionMissingJustification unionKeys) ->
            assertBool "proofless trace obstruction carries union keys" (not (IntSet.null unionKeys))
          Right _ ->
            assertFailure "bare committed union trace must not fabricate proof"
        case serializeProofLog updatedProofGraph of
          [proofStep] -> do
            psKind proofStep @?= ProofRewrite (RewriteRuleId 6)
            assertBool "proof step records the instantiated LHS witness" (isJust (psLhsWitness proofStep))
            assertBool "proof step records the extracted RHS witness" (isJust (psRhsWitness proofStep))
            psLhsClass proofStep @?= psRhsClass proofStep
          proofSteps ->
            assertFailure ("expected exactly one proof step, got " <> show (length proofSteps))
    ]

rewriteScopeLattice :: ContextLattice Scope
rewriteScopeLattice =
  case latticeContext of
    Right latticeValue -> latticeValue
    Left compileError ->
      error ("invalid rewrite Scope lattice fixture: " <> show compileError)

expectCompiledRewrite :: RawRewriteRule (RewriteCondition SurfaceKind ArithF) ArithF -> IO (RulePlan (CompiledGuard SurfaceKind ArithF) ArithF)
expectCompiledRewrite rewriteRule =
  case compileRewriteRules @(EGraphU SurfaceKind ArithF () ()) [rewriteRule] of
    Right [compiledRewrite] -> pure compiledRewrite
    Right compiledRewrites ->
      assertFailure ("expected exactly one compiled rewrite, got " <> show (length compiledRewrites))
    Left compileError -> assertFailure ("expected rewrite to compile, got " <> show compileError)

expectSingletonMatch :: [substitution] -> IO substitution
expectSingletonMatch matches =
  case matches of
    [singleMatch] -> pure singleMatch
    _ -> assertFailure ("expected exactly one match, found " <> show (length matches))

expectRelationalMatches ::
  RulePlan (CompiledGuard SurfaceKind ArithF) ArithF ->
  EGraph ArithF a ->
  IO [(ClassId, Substitution)]
expectRelationalMatches compiledRewrite graph =
  case wcojMatchCompiledWithRoots (rpQuery compiledRewrite) graph of
    Right matches -> pure matches
    Left obstruction -> assertFailure ("expected relational matcher to succeed, got " <> show obstruction)

expectAppliedRewrite :: Either RewriteApplicationError graph -> IO graph
expectAppliedRewrite rewriteResult =
  case rewriteResult of
    Right graph -> pure graph
    Left applicationError -> assertFailure ("expected rewrite to apply, got " <> show applicationError)

expectRawMaterialized :: Either RewriteApplicationError match -> IO match
expectRawMaterialized materializationResult =
  case materializationResult of
    Right match -> pure match
    Left applicationError -> assertFailure ("expected raw match to materialize, got " <> show applicationError)

expectProofApply :: Either (EGraphRewriteApplicationError ArithF Scope) result -> IO result
expectProofApply applicationResult =
  case applicationResult of
    Right result -> pure result
    Left applicationError -> assertFailure ("expected proof-supported rewrite to apply, got " <> show applicationError)
