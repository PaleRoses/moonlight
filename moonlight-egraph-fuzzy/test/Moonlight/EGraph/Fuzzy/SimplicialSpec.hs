{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PackageImports #-}
module Moonlight.EGraph.Fuzzy.SimplicialSpec
  ( tests,
  )
where

import Moonlight.EGraph.Effect.CoveringSurface (SurfaceKind)
import Data.IntSet qualified as IntSet
import Moonlight.Rewrite.Algebra
  ( compilePatternQuery,
    singlePatternQuery,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.Rewrite.System
  ( combineCompiledGuards,
    compileGuard,
    emptyGuardCapabilityResolver,
  )
import Moonlight.Rewrite.System (emptyFactDerivationIndex)
import Moonlight.Rewrite.System (emptyFactStore)
import Moonlight.Core
  ( Pattern (..)
  )
import Moonlight.Core qualified as EGraph
import Moonlight.Core (lookupSubst)
import Moonlight.EGraph.Pure.Rebuild (merge, rebuild)
import "moonlight-saturation" Moonlight.Saturation.Context.Program.Spec
  ( PlanSpec,
    planSpecMatchingStrategy,
  )
import Moonlight.EGraph.Pure.Saturation.Matching
  ( rootFilterMatchingAlgebra,
    MatchingStrategy (CustomMatchingAlgebra),
  )
import Moonlight.EGraph.Pure.Saturation.Substrate
  ( EGraphU,
  )
import Moonlight.Saturation.Matching qualified as GenericMatching
import Moonlight.Saturation.Substrate (SatGraph)
import Moonlight.Delta.Scope qualified as Delta
import Moonlight.EGraph.Pure.Types (ClassId, RewriteRuleId, emptyEGraph)
import Moonlight.EGraph.Test.Config (testConfigWith)
import "moonlight-egraph-fuzzy" Moonlight.EGraph.Fuzzy.Simplicial
import Moonlight.Pale.Test.Site.Assertion (expectRight)
import Moonlight.Pale.Test.Site.Core (TestBudget (..))
import Moonlight.EGraph.Test.Arith.Core
  ( ArithF (..),
    ArithTag (..),
    addTermNode,
    analysisSpec,
    NodeCount,
    numTerm,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "simplicial"
    [ testCase "smart constructor rejects truncation bounds below 2" $ do
        parallelBlockRegistry <- expectParallelBlockRegistry (mkParallelBlockRegistry arithParallelBlockDeclarations)
        case mkSimplicialBackend 1 (arithParallelShapeAlgebra parallelBlockRegistry) False of
          Left validationError ->
            validationError @?= SimplicialUpperBoundTooSmall 1
          Right _ ->
            assertFailure "expected simplicial backend validation to reject upper bounds below 2",
      testCase "parallel shape validation rejects canonicalizations that change membership" $
        let badPatternNode :: ArithF (Pattern ArithF)
            badPatternNode = Add (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1))
         in do
              parallelBlockRegistry <- expectParallelBlockRegistry (mkParallelBlockRegistry arithParallelBlockDeclarations)
              validateParallelPatternShape (badArithParallelShapeAlgebra parallelBlockRegistry) badPatternNode
                @?= [CanonicalizationChangesMembership [0, 1] [0, 2]],
      testCase "parallel block registry rejects overlapping blocks" $
        case mkParallelBlockRegistry badArithParallelBlockDeclarations of
          Left validationErrors ->
            validationErrors
              @?= [RegistryBlocksOverlap AddTag 2 (IntSet.fromList [0, 1]) (IntSet.fromList [1])]
          Right _ ->
            assertFailure "expected parallel block registry validation to reject overlapping blocks",
      testCase "backend collapses commutative enode permutations inside one e-class" $
        let patternValue = PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1)))
         in do
              (_, graph1) <- expectRight (addTerm (numTerm 1) (emptyEGraph analysisSpec))
              (_, graph2) <- expectRight (addTerm (numTerm 2) graph1)
              (leftRootClass, graph3) <- expectRight (addTerm (addTermNode (numTerm 1) (numTerm 2)) graph2)
              (rightRootClass, graph4) <- expectRight (addTerm (addTermNode (numTerm 2) (numTerm 1)) graph3)
              let graph5 = rebuild (merge leftRootClass rightRootClass graph4)
              parallelBlockRegistry <- expectParallelBlockRegistry (mkParallelBlockRegistry arithParallelBlockDeclarations)
              simplicialBackend <-
                expectSimplicialBackend
                  (mkSimplicialBackend 2 (arithParallelShapeAlgebra parallelBlockRegistry) False)
              let rootedMatches = homotopicMatch simplicialBackend patternValue graph5
              length rootedMatches @?= 1
              case rootedMatches of
                [(_, homotopy)] -> do
                  (leftChild, rightChild) <- expectBinaryBindings homotopy
                  case hsParallelEvidence homotopy of
                    [parallelEvidence] -> do
                      pfkRootClass (peFaceKey parallelEvidence) @?= hsRootClass homotopy
                      pfkTagFingerprint (peFaceKey parallelEvidence) @?= ParallelTagFingerprint 0
                      pfkSlots (peFaceKey parallelEvidence) @?= (0, 1)
                      pfkChildren (peFaceKey parallelEvidence) @?= orderedChildren leftChild rightChild
                    _ ->
                      assertFailure "expected exactly one piece of parallel evidence"
                _ ->
                  assertFailure "expected a single homotopic match after commutative collapse",
      testCase "custom matching strategy dispatches through the saturation seam" $
        let patternValue = PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1)))
            configOf :: SimplicialBackend ArithF -> PlanSpec (EGraphU SurfaceKind ArithF NodeCount ()) (SatGraph (EGraphU SurfaceKind ArithF NodeCount ())) RewriteRuleId
            configOf simplicialBackend =
              testConfigWith
                (TestBudget {testBudgetMaxIterations = 1, testBudgetMaxNodes = 32})
                ( CustomMatchingAlgebra
                    ( rootFilterMatchingAlgebra
                        emptyGuardCapabilityResolver
                        (\rootFilter compiledQuery graph ->
                           Right (homotopicMatchCompiledWithRootFilter simplicialBackend rootFilter compiledQuery graph))
                    )
                )
         in do
              (_, graph1) <- expectRight (addTerm (numTerm 1) (emptyEGraph analysisSpec))
              (_, graph2) <- expectRight (addTerm (numTerm 2) graph1)
              (leftRootClass, graph3) <- expectRight (addTerm (addTermNode (numTerm 1) (numTerm 2)) graph2)
              (rightRootClass, graph4) <- expectRight (addTerm (addTermNode (numTerm 2) (numTerm 1)) graph3)
              let graph5 = rebuild (merge leftRootClass rightRootClass graph4)
              parallelBlockRegistry <- expectParallelBlockRegistry (mkParallelBlockRegistry arithParallelBlockDeclarations)
              simplicialBackend <-
                expectSimplicialBackend
                  (mkSimplicialBackend 2 (arithParallelShapeAlgebra parallelBlockRegistry) True)
              case compilePatternQuery combineCompiledGuards compileGuard (singlePatternQuery patternValue) of
                Left _ ->
                  assertFailure "expected simplicial pattern compilation to succeed"
                Right compiledQuery ->
                  case planSpecMatchingStrategy (configOf simplicialBackend) of
                    CustomMatchingAlgebra matchingAlgebra ->
                      let matchingRequest =
                            GenericMatching.QueryRequest
                              { GenericMatching.qrSite = GenericMatching.BaseSite,
                                GenericMatching.qrSnapshot = Nothing,
                                GenericMatching.qrQuery = compiledQuery,
                                GenericMatching.qrPurpose = GenericMatching.RawMatchPurpose
                              }
                          matchingWorld =
                            GenericMatching.MatchWorld
                              { GenericMatching.mwGraph = graph5,
                                GenericMatching.mwFacts = emptyFactStore,
                                GenericMatching.mwFactDerivations = emptyFactDerivationIndex,
                                GenericMatching.mwCapabilities = GenericMatching.maEnvironment matchingAlgebra,
                                GenericMatching.mwProofContext = Nothing,
                                GenericMatching.mwIteration = 0
                              }
                          (preparedState, frontier) =
                            GenericMatching.prepareSingleQuery
                              matchingAlgebra
                              (GenericMatching.maInitialState matchingAlgebra)
                              Delta.fullDelta
                              matchingWorld
                              matchingRequest
                          (_, matches) =
                            GenericMatching.runSingleQuery
                              matchingAlgebra
                              preparedState
                              matchingWorld
                              frontier
                              matchingRequest
                       in case matches of
                            Right matchedSubstitutions ->
                              length matchedSubstitutions @?= 1
                            Left obstruction ->
                              assertFailure ("expected simplicial matching to succeed, got " <> show obstruction)
                    _ ->
                      assertFailure "expected custom matching algebra"
    ]

arithParallelShapeAlgebra :: ParallelBlockRegistry ArithTag -> ParallelShapeAlgebra ArithF
arithParallelShapeAlgebra parallelBlockRegistry =
  mkParallelShapeAlgebra
    parallelBlockRegistry
    arithCanonicalizationRegistry
    arithTagFingerprint

badArithParallelShapeAlgebra :: ParallelBlockRegistry ArithTag -> ParallelShapeAlgebra ArithF
badArithParallelShapeAlgebra parallelBlockRegistry =
  mkParallelShapeAlgebra
    parallelBlockRegistry
    badArithCanonicalizationRegistry
    arithTagFingerprint

arithParallelBlockDeclarations :: [(ArithTag, Int, [IntSet.IntSet])]
arithParallelBlockDeclarations =
  [(AddTag, 2, [IntSet.fromList [0, 1]])]

badArithParallelBlockDeclarations :: [(ArithTag, Int, [IntSet.IntSet])]
badArithParallelBlockDeclarations =
  [(AddTag, 2, [IntSet.fromList [0, 1], IntSet.fromList [1]])]

arithCanonicalizationRegistry :: CanonicalizationRegistry ArithTag
arithCanonicalizationRegistry =
  mkCanonicalizationRegistry
    [(AddTag, SortBlockAscending)]

badArithCanonicalizationRegistry :: CanonicalizationRegistry ArithTag
badArithCanonicalizationRegistry =
  mkCanonicalizationRegistry
    [(AddTag, ExplicitBlockOrder [0, 2])]

arithTagFingerprint :: ArithTag -> ParallelTagFingerprint
arithTagFingerprint tag =
  ParallelTagFingerprint
    (case tag of
       AddTag -> 0
       NumTag number -> number + 1
       VarTag variable -> variable + 1000
       MulTag -> 2000
       NegTag -> 2001)

expectBinaryBindings :: HomotopicSubstitution -> IO (ClassId, ClassId)
expectBinaryBindings homotopy =
  case (lookupSubst (EGraph.mkPatternVar 0) (hsSubstitution homotopy), lookupSubst (EGraph.mkPatternVar 1) (hsSubstitution homotopy)) of
    (Just leftChild, Just rightChild) -> pure (leftChild, rightChild)
    _ -> assertFailure "expected both pattern variables to be bound"

expectSimplicialBackend :: Either SimplicialBackendValidationError (SimplicialBackend f) -> IO (SimplicialBackend f)
expectSimplicialBackend backendResult =
  case backendResult of
    Right backend -> pure backend
    Left validationError ->
      assertFailure ("expected simplicial backend to validate, got " <> show validationError)

expectParallelBlockRegistry ::
  Either [ParallelBlockRegistryValidationError tag] (ParallelBlockRegistry tag) ->
  IO (ParallelBlockRegistry tag)
expectParallelBlockRegistry registryResult =
  case registryResult of
    Right parallelBlockRegistry -> pure parallelBlockRegistry
    Left _ ->
      assertFailure "expected parallel block registry to validate"

orderedChildren :: ClassId -> ClassId -> (ClassId, ClassId)
orderedChildren leftChild rightChild =
  if leftChild <= rightChild
    then (leftChild, rightChild)
    else (rightChild, leftChild)
