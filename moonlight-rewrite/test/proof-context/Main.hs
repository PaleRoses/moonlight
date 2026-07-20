{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.IntSet qualified as IntSet
import Data.Bits (finiteBitSize)
import Data.Int (Int32)
import Moonlight.Core (ClassId (..), RewriteRuleId (..), emptySubstitution)
import Moonlight.Pale.Test.Site.Assertion (expectRight)
import Moonlight.Rewrite.ProofContext
  ( ProofBoundaryDischarge (..),
    ProofBoundaryObstruction (..),
    InvalidProofTheoremName (..),
    ProofTheoremNameRejection (..),
    checkProofBoundary,
    parseTheoremManifest,
    proofObligationRuntimeLawId,
    proofTheoremManifestFromIdentifiers,
    proofTheoremManifestIdentifiers,
    proofTheoremNameString,
    requiredProofObligations,
    requiredProofTheoremManifest,
    requiredRestrictionManifestTheoremManifest,
    requiredRestrictionRuntimeLawIdentifiers,
    requiredRuntimeLawObligationIdentifiers,
    ProofQueryError (..),
    ProofCompressionSummary (..),
    ProofRegistry,
    ProofRetention (..),
    ProofStep (..),
    emptyProofRegistry,
    emptyProofRegistryWithRetention,
    proofBetween,
    proofClassesReachableFrom,
    proofReachability,
    proofRegistryRecordedStepCount,
    proofRegistryRetainedStepCount,
    proofRelated,
    recordProofStepWith,
    defaultProofStepInput,
    summarizeProofLog,
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
      testCase "recent proof retention evicts endpoint indexes incrementally" $ do
        let registry =
              recordTestProofSteps
                (emptyProofRegistryWithRetention (KeepRecentProofSteps 2))
                [ (RewriteRuleId 20, ClassId 0, ClassId 1, "first"),
                  (RewriteRuleId 21, ClassId 1, ClassId 2, "second"),
                  (RewriteRuleId 22, ClassId 2, ClassId 3, "third"),
                  (RewriteRuleId 23, ClassId 3, ClassId 4, "fourth")
                ]

        proofRegistryRetainedStepCount registry @?= 2
        assertProofAnnotation "fourth" (proofBetween (ClassId 4) (ClassId 3) registry)
        assertProofAnnotation "third" (proofBetween (ClassId 2) (ClassId 3) registry)
        assertProofQueryError ProofPruned (proofBetween (ClassId 0) (ClassId 1) registry),
      testCase "no-proof retention records no proof summary or log" $ do
        let registry =
              recordTestProofStep
                (RewriteRuleId 30)
                (ClassId 0)
                (ClassId 1)
                "not-retained"
                (emptyProofRegistryWithRetention KeepNoProof)

        proofRegistryRecordedStepCount registry @?= 0
        proofRegistryRetainedStepCount registry @?= 0,
      testCase "summary retention preserves the full-mode observable summary" $ do
        let recordOne retention =
              recordTestProofStep
                (RewriteRuleId 31)
                (ClassId 2)
                (ClassId 3)
                "summary"
                (emptyProofRegistryWithRetention retention)

        summarizeProofLog (recordOne KeepProofSummary)
          @?= summarizeProofLog (recordOne KeepFullProof),
      testCase "summary indexes preserve signed, wide, and reversed identities" $ do
        let wideClassKey =
              fromIntegral (maxBound :: Int32) + 1
            fallbackPairs
              | finiteBitSize (0 :: Int) >= 64 =
                  [ (wideClassKey, wideClassKey + 1),
                    (wideClassKey + 1, wideClassKey),
                    (wideClassKey + 2, wideClassKey + 3)
                  ]
              | otherwise =
                  [(10, 11), (11, 10), (12, 13)]
            indexedSteps =
              zip3
                [RewriteRuleId 40, RewriteRuleId 40, RewriteRuleId 41, RewriteRuleId 41, RewriteRuleId 42]
                ((-2, -1) : (-1, -2) : fallbackPairs)
                ["signed", "signed-reversed", "wide", "wide-reversed", "wide-distinct"]
            registry =
              recordTestProofSteps
                (emptyProofRegistryWithRetention KeepProofSummary)
                [ (ruleId, ClassId leftKey, ClassId rightKey, annotation)
                  | (ruleId, (leftKey, rightKey), annotation) <- indexedSteps
                ]
            summary =
              summarizeProofLog registry

        pcsUniqueClassPairs summary @?= 3
        pcsUniqueRewriteRules summary @?= 3,
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
          Left (ProofBoundaryUnexpectedTheorems [theoremName])
            | proofTheoremNameString theoremName == "unexpected_theorem" ->
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

recordTestProofSteps ::
  TestProofRegistry ->
  [(RewriteRuleId, ClassId, ClassId, String)] ->
  TestProofRegistry
recordTestProofSteps =
  foldl'
    (\registry (rewriteRuleId, leftClassId, rightClassId, annotation) ->
      recordTestProofStep rewriteRuleId leftClassId rightClassId annotation registry
    )

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
