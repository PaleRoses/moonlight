{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.IntSet qualified as IntSet
import Moonlight.Core (ClassId (..), RewriteRuleId (..))
import Moonlight.Core (emptySubstitution)
import Moonlight.Rewrite.ProofContext
  ( ProofBoundaryDischarge (..),
    ProofBoundaryObstruction (..),
    InvalidProofTheoremName (..),
    ProofTheoremNameRejection (..),
    ProofTheoremName (..),
    checkProofBoundary,
    parseTheoremManifest,
    proofObligationRuntimeLawId,
    proofTheoremManifestFromIdentifiers,
    proofTheoremManifestIdentifiers,
    requiredProofObligations,
    requiredProofTheoremManifest,
    requiredRestrictionManifestTheoremManifest,
    requiredRestrictionRuntimeLawIdentifiers,
    requiredRuntimeLawObligationIdentifiers,
  )
import Moonlight.Rewrite.ProofContext
  ( ProofQueryError (..),
    ProofRegistry,
    ProofRetention (..),
    ProofStep (..),
    emptyProofRegistry,
    emptyProofRegistryWithRetention,
    proofBetween,
    proofClassesReachableFrom,
    proofReachability,
    proofRelated,
    recordProofStepWith,
    defaultProofStepInput,
  )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-rewrite-proof-context"
        [ proofRegistryTests,
          proofBoundaryTests
        ]
    )

proofRegistryTests :: TestTree
proofRegistryTests =
  testGroup
    "proof registry indexed queries"
    [ testCase "proofBetween finds a reverse-oriented retained edge" $ do
        let registry =
              recordTestProofStep
                (RewriteRuleId 7)
                (ClassId 2)
                (ClassId 1)
                "reverse-edge"
                emptyProofRegistry

        assertProofAnnotation
          "reverse-edge"
          (proofBetween (ClassId 1) (ClassId 2) registry),
      testCase "proofBetween resolves retained step ids after recent-proof pruning" $ do
        let registry =
              recordTestProofStep
                (RewriteRuleId 8)
                (ClassId 2)
                (ClassId 1)
                "retained-edge"
                ( recordTestProofStep
                    (RewriteRuleId 7)
                    (ClassId 0)
                    (ClassId 1)
                    "dropped-edge"
                    (emptyProofRegistryWithRetention (KeepRecentProofSteps 1))
                )

        assertProofAnnotation
          "retained-edge"
          (proofBetween (ClassId 1) (ClassId 2) registry)
        assertProofQueryError ProofPruned (proofBetween (ClassId 0) (ClassId 1) registry)
        assertProofQueryError ProofNotRecorded (proofBetween (ClassId 4) (ClassId 5) registry),
      testCase "proof reachability remaps sparse observed classes without changing public keys" $ do
        let sourceClass = ClassId 1000000
            targetClass = ClassId 1000001
            registry =
              recordTestProofStep
                (RewriteRuleId 9)
                sourceClass
                targetClass
                "sparse-edge"
                emptyProofRegistry

        reachability <- expectRight (proofReachability registry)
        proofClassesReachableFrom sourceClass reachability
          @?= IntSet.fromList [1000000, 1000001]
        proofClassesReachableFrom (ClassId 7) reachability
          @?= IntSet.singleton 7
        proofRelated sourceClass targetClass registry @?= Right True,
      testCase "proof reachability preserves contiguous identity keys" $ do
        let registry =
              recordTestProofStep
                (RewriteRuleId 12)
                (ClassId 1)
                (ClassId 2)
                "contiguous-second-edge"
                ( recordTestProofStep
                    (RewriteRuleId 11)
                    (ClassId 0)
                    (ClassId 1)
                    "contiguous-first-edge"
                    emptyProofRegistry
                )

        reachability <- expectRight (proofReachability registry)
        proofClassesReachableFrom (ClassId 0) reachability
          @?= IntSet.fromList [0, 1, 2],
      testCase "negative proof endpoints remain outside the observed dense graph" $ do
        let registry =
              recordTestProofStep
                (RewriteRuleId 10)
                (ClassId (-1))
                (ClassId 2)
                "negative-edge"
                emptyProofRegistry

        reachability <- expectRight (proofReachability registry)
        proofClassesReachableFrom (ClassId (-1)) reachability
          @?= IntSet.singleton (-1)
        proofClassesReachableFrom (ClassId 2) reachability
          @?= IntSet.singleton 2
    ]

proofBoundaryTests :: TestTree
proofBoundaryTests =
  testGroup
    "proof boundary discharge"
    [ testCase "exact manifest discharges the typed proof boundary" $ do
        requiredManifest <-
          expectRight requiredProofTheoremManifest
        case checkProofBoundary requiredManifest of
          Right discharge ->
            proofTheoremManifestIdentifiers (pbdManifestTheorems discharge)
              @?= proofTheoremManifestIdentifiers requiredManifest
          Left obstruction ->
            assertFailure ("expected proof boundary discharge, got " <> show obstruction),
      testCase "missing theorem is a typed obstruction" $ do
        requiredManifest <-
          expectRight requiredProofTheoremManifest
        manifestWithoutFirstTheorem <-
          expectRight
            (proofTheoremManifestFromIdentifiers (drop 1 (proofTheoremManifestIdentifiers requiredManifest)))
        case checkProofBoundary manifestWithoutFirstTheorem of
          Left (ProofBoundaryMissingTheorems (_ : _)) ->
            pure ()
          other ->
            assertFailure ("expected missing-theorem obstruction, got " <> show other),
      testCase "unexpected theorem is a typed obstruction" $ do
        requiredManifest <-
          expectRight requiredProofTheoremManifest
        manifestWithUnexpectedTheorem <-
          expectRight
            (proofTheoremManifestFromIdentifiers (proofTheoremManifestIdentifiers requiredManifest <> ["unexpected_theorem"]))
        case checkProofBoundary manifestWithUnexpectedTheorem of
          Left (ProofBoundaryUnexpectedTheorems [ProofTheoremName "unexpected_theorem"]) ->
            pure ()
          other ->
            assertFailure ("expected unexpected-theorem obstruction, got " <> show other),
      testCase "invalid manifest theorem names are rejected before discharge" $
        case parseTheoremManifest "{\"theorems\":[\"bad theorem\"]}" of
          Left
            ( ProofBoundaryInvalidManifestTheorems
                [ InvalidProofTheoremName
                    "bad theorem"
                    ProofTheoremNameContainsWhitespace
                  ]
              ) ->
            pure ()
          other ->
            assertFailure ("expected invalid-manifest obstruction, got " <> show other),
      testCase "runtime-law proof obligations point at runtime law ids" $ do
        runtimeLawObligationIdentifiers <-
          expectRight requiredRuntimeLawObligationIdentifiers
        assertBool
          "runtime law obligations must not be empty perfume"
          (not (null runtimeLawObligationIdentifiers))
        assertBool
          "runtime law obligations must be backed by the restriction runtime law registry"
          (all (`elem` requiredRestrictionRuntimeLawIdentifiers) runtimeLawObligationIdentifiers),
      testCase "restriction-kernel theorem names are valid before inclusion" $ do
        restrictionManifest <-
          expectRight requiredRestrictionManifestTheoremManifest
        case checkProofBoundary restrictionManifest of
          Left (ProofBoundaryMissingTheorems _) ->
            pure ()
          other ->
            assertFailure ("expected only missing non-restriction obligations after valid restriction import, got " <> show other),
      testCase "obligations expose classification instead of boolean name checking" $
        case requiredProofObligations of
          Left obstruction ->
            assertFailure ("expected typed obligations, got " <> show obstruction)
          Right obligations ->
            do
              runtimeLawObligationIdentifiers <-
                expectRight requiredRuntimeLawObligationIdentifiers
              runtimeLawObligationIdentifiers @?= foldMap (maybe [] (: []) . proofObligationRuntimeLawId) obligations
    ]

data TestNode child

type TestProofRegistry = ProofRegistry TestNode () String

recordTestProofStep ::
  RewriteRuleId ->
  ClassId ->
  ClassId ->
  String ->
  TestProofRegistry ->
  TestProofRegistry
recordTestProofStep rewriteRuleId leftClassId rightClassId annotation =
  recordProofStepWith (defaultProofStepInput rewriteRuleId leftClassId rightClassId emptySubstitution annotation)

assertProofAnnotation :: String -> Either ProofQueryError (ProofStep TestNode () String) -> IO ()
assertProofAnnotation expectedAnnotation =
  \case
    Left queryError ->
      assertFailure ("expected retained proof step, got " <> show queryError)
    Right proofStep ->
      psAnnotation proofStep @?= expectedAnnotation

assertProofQueryError :: ProofQueryError -> Either ProofQueryError (ProofStep TestNode () String) -> IO ()
assertProofQueryError expectedError =
  \case
    Left queryError ->
      queryError @?= expectedError
    Right proofStep ->
      assertFailure ("expected proof query error, got " <> show proofStep)

expectRight :: Show errorValue => Either errorValue value -> IO value
expectRight =
  \case
    Left errorValue ->
      assertFailure ("unexpected Left: " <> show errorValue)
    Right value ->
      pure value
