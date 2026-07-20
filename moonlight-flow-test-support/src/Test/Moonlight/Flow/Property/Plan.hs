{-# LANGUAGE DataKinds #-}

module Test.Moonlight.Flow.Property.Plan
  ( saturationIdempotentWithState,
    canonicalKeyAtomOrderInvariant,
    planLiteralRoundTripExamples,
    planLiteralProofChecks,
    planEqualityLawReachability,
    planEquivalenceProofChecksValid,
    planEquivalenceProofRejectsTampered,
    planProperties,
  )
where

import Data.Bifunctor (first)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Foldable (fold)
import Data.Set qualified as Set
import Data.Word (Word64)
import Data.Fix (Fix)
import Moonlight.Flow.Plan.Shape.Build qualified as ShapeBuild
import Moonlight.Flow.Model.Schema.Digest (StableDigest128 (..))
import Moonlight.Flow.Plan.Rewrite
  ( PlanEGraphResult (..),
    PlanEqualityLaw (..),
    PlanEquivalenceStep (..),
    PlanNode,
    PlanSaturationState,
    SaturationBudget (..),
    amalgamationPlanTerm,
    canonicalPlanTerm,
    coverageTransformPlanTerm,
    emptyPlanSaturationState,
    extractCanonicalPlanKey,
    insertPlanTerm,
    mkPlanRewriteSystem,
    pepSteps,
    pegrCanonicalProof,
    pegrCanonicalPlanShape,
    pegrState,
    pepTargetDigest,
    plpLaw,
    saturatePlanShape,
    saturatePlanShapeWithState,
    semanticNormalizationPlanRewriteSystem,
    projectionPlanTerm,
    restrictionPlanTerm,
    rewritePlanSaturationState,
  )
import Moonlight.Flow.Plan.Shape.Term
  ( CanonSlot (..),
    CoverageTransformPayload (..),
    PlanShape,
    PlanStage (Canonical, Projection, RawLogical, Restriction),
    psDigest,
  )
import Test.Moonlight.Flow.Gen.Plan
  ( atomOrderPair,
    genPlanShape,
  )
import Test.Moonlight.Flow.Literal
  ( literalRoundTrip,
    planLiteral,
  )
import Test.Moonlight.Flow.Execution.RelProgram
  ( RelProgram,
    atom,
    program,
    programRawPlanShape,
  )
import Test.Moonlight.Flow.Verify
  ( verifyPlanEquivalenceProof,
  )
import Test.QuickCheck
  ( Property,
    conjoin,
    counterexample,
    forAll,
    property,
    (===),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

-- Proves semantic-surface invariant: saturation with the returned state is
-- idempotent over canonical key and state.
planPropertySaturationBudget :: SaturationBudget
planPropertySaturationBudget =
  SaturationBudget
    { sbMaxIterations = 8,
      sbMaxNodes = maxBound
    }
{-# INLINE planPropertySaturationBudget #-}

saturationIdempotentWithState :: Property
saturationIdempotentWithState =
  forAll genPlanShape $ \raw ->
    case saturatePlanShape planPropertySaturationBudget semanticNormalizationPlanRewriteSystem raw of
      Left err -> counterexample (show err) (property False)
      Right firstResult ->
        case saturatePlanShapeWithState planPropertySaturationBudget semanticNormalizationPlanRewriteSystem (pegrState firstResult) raw of
          Left err -> counterexample (show err) (property False)
          Right second ->
            conjoin
              [ extractCanonicalPlanKey firstResult === extractCanonicalPlanKey second,
                pegrState firstResult === pegrState second
              ]

-- Proves semantic-surface invariant: canonical key is insensitive to atom order.
canonicalKeyAtomOrderInvariant :: TestTree
canonicalKeyAtomOrderInvariant =
  testCase "canonical key atom-order invariant" $
    case atomOrderPair of
      Left err -> assertFailure err
      Right (leftShape, rightShape) -> do
        leftKey <- expectRight (fmap extractCanonicalPlanKey (saturatePlanShape planPropertySaturationBudget semanticNormalizationPlanRewriteSystem leftShape))
        rightKey <- expectRight (fmap extractCanonicalPlanKey (saturatePlanShape planPropertySaturationBudget semanticNormalizationPlanRewriteSystem rightShape))
        leftKey @?= rightKey

-- Proves literal-module invariant: parser-ready plan literals round-trip
-- through the pretty printer instead of smuggling hardcoded canonical keys.
planLiteralRoundTripExamples :: TestTree
planLiteralRoundTripExamples =
  testCase "plan literal parser round-trips examples" $
    case traverse literalRoundTrip planLiteralExamples of
      Left err -> assertFailure (show err)
      Right results -> results @?= fmap (const True) planLiteralExamples

-- Proves literal-driven plan tests still enter the same proof-checked
-- saturation path as generated plan shapes.
planLiteralProofChecks :: TestTree
planLiteralProofChecks =
  testCase "plan literal saturation proof verifies" $
    case planLiteral "(project [a] (atom R [a,b]))" of
      Left err -> assertFailure (show err)
      Right raw ->
        case saturatePlanShape planPropertySaturationBudget semanticNormalizationPlanRewriteSystem raw of
          Left err -> assertFailure (show err)
          Right result ->
            verifyPlanEquivalenceProof semanticNormalizationPlanRewriteSystem raw (pegrCanonicalPlanShape result) result @?= Right ()

-- Proves semantic-surface invariant: every active PlanEqualityLaw constructor is
-- reachable by a documented shape, not just present in a registry.
planEqualityLawReachability :: TestTree
planEqualityLawReachability =
  testCase "each active PlanEqualityLaw has a firing shape" $
    case reachablePlanEqualityLaws of
      Left err -> assertFailure err
      Right observed ->
        Set.fromList observed @?= Set.fromList allActivePlanEqualityLaws

-- Proves semantic-surface invariant: generated saturator proofs pass the explicit checker.
planEquivalenceProofChecksValid :: Property
planEquivalenceProofChecksValid =
  forAll genPlanShape $ \raw ->
    case saturatePlanShape planPropertySaturationBudget semanticNormalizationPlanRewriteSystem raw of
      Left err -> counterexample (show err) (property False)
      Right result ->
        verifyPlanEquivalenceProof semanticNormalizationPlanRewriteSystem raw (pegrCanonicalPlanShape result) result
          === Right ()

-- Proves semantic-surface invariant: the proof checker rejects a tampered chain.
planEquivalenceProofRejectsTampered :: Property
planEquivalenceProofRejectsTampered =
  forAll genPlanShape $ \raw ->
    case saturatePlanShape planPropertySaturationBudget semanticNormalizationPlanRewriteSystem raw of
      Left err -> counterexample (show err) (property False)
      Right result ->
        let proof = pegrCanonicalProof result
            tampered = result {pegrCanonicalProof = proof {pepTargetDigest = StableDigest128 0xdecaf 0xbad}}
         in counterexample "tampered proof unexpectedly accepted" $
              case verifyPlanEquivalenceProof semanticNormalizationPlanRewriteSystem raw (pegrCanonicalPlanShape result) tampered of
                Left _ -> property True
                Right () -> property False

planProperties :: TestTree
planProperties =
  testGroup
    "plan-EGraph"
    [ testProperty "saturation idempotent with returned state" saturationIdempotentWithState,
      canonicalKeyAtomOrderInvariant,
      planLiteralRoundTripExamples,
      planLiteralProofChecks,
      planEqualityLawReachability,
      testProperty "verifyPlanEquivalenceProof succeeds on saturator output" planEquivalenceProofChecksValid,
      testProperty "verifyPlanEquivalenceProof fails on tampered chain" planEquivalenceProofRejectsTampered
    ]

planLiteralExamples :: [String]
planLiteralExamples =
  [ "(atom R [a])",
    "(atom Rel [a,b,c])",
    "(project [a] (atom R [a,b]))",
    "(project [b,a] (atom R [a,b,c]))"
  ]

reachablePlanEqualityLaws :: Either String [PlanEqualityLaw]
reachablePlanEqualityLaws = do
  baseShape <- canonicalShapeFromProgram baseProgram
  otherShape <- canonicalShapeFromProgram otherProgram
  rawShape <- first show (programRawPlanShape atomOrderProgram)
  let baseTerm = canonicalPlanTerm baseShape
      rawLaw law =
        lawStepsFromSaturation law rawShape
      termLaw law term =
        lawsFromTerm law term
  foldMapA
    id
    [ rawLaw LawAlphaCanonical,
      rawLaw LawAtomOrder,
      projectionIdentityTerm baseShape >>= termLaw LawProjectionId,
      projectionComposeTerm baseShape >>= termLaw LawProjectionCompose,
      termLaw LawRestrictionId (restrictionIdentityTerm baseShape),
      termLaw LawRestrictionCompose (restrictionComposeTerm baseShape),
      projectionRestrictionCommuteTerm baseShape >>= termLaw LawProjectionRestrictionCommute,
      restrictionProjectionCommuteTerm baseShape >>= termLaw LawRestrictionProjectionCommute,
      projectionRestrictionFuseTerm baseShape >>= termLaw LawProjectionRestrictionFuse,
      coverMemberOrderState baseShape otherShape >>= lawsFromPreparedState LawCoverMemberOrder,
      termLaw LawCoverSingleton (coverSingletonTerm baseTerm baseShape),
      termLaw LawCoverageTransformId (coverageTransformIdentityTerm baseTerm),
      termLaw LawCoverageTransformCompose (coverageTransformComposeTerm baseTerm)
    ]

allActivePlanEqualityLaws :: [PlanEqualityLaw]
allActivePlanEqualityLaws =
  [ LawAtomOrder,
    LawAlphaCanonical,
    LawProjectionId,
    LawProjectionCompose,
    LawRestrictionId,
    LawRestrictionCompose,
    LawProjectionRestrictionCommute,
    LawRestrictionProjectionCommute,
    LawProjectionRestrictionFuse,
    LawCoverMemberOrder,
    LawCoverSingleton,
    LawCoverageTransformId,
    LawCoverageTransformCompose
  ]

lawStepsFromSaturation :: PlanEqualityLaw -> PlanShape 'RawLogical -> Either String [PlanEqualityLaw]
lawStepsFromSaturation law rawShape =
  first show $
    fmap
      (fmap stepLaw . pepSteps . pegrCanonicalProof)
      (saturatePlanShape planPropertySaturationBudget (mkPlanRewriteSystem (Set.singleton law)) rawShape)

lawsFromTerm :: PlanEqualityLaw -> Fix PlanNode -> Either String [PlanEqualityLaw]
lawsFromTerm law term = do
  (_, state) <- first show (insertPlanTerm term emptyPlanSaturationState)
  lawsFromPreparedState law state

lawsFromPreparedState :: PlanEqualityLaw -> PlanSaturationState -> Either String [PlanEqualityLaw]
lawsFromPreparedState law state =
  first show $
    fmap
      (fmap stepLaw . snd)
      (rewritePlanSaturationState planPropertySaturationBudget (mkPlanRewriteSystem (Set.singleton law)) state)

stepLaw :: PlanEquivalenceStep -> PlanEqualityLaw
stepLaw step =
  case step of
    EqStepAppliedLaw proof -> plpLaw proof

canonicalShapeFromProgram :: RelProgram -> Either String (PlanShape 'Canonical)
canonicalShapeFromProgram relProgram = do
  rawShape <- first show (programRawPlanShape relProgram)
  fmap pegrCanonicalPlanShape (first show (saturatePlanShape planPropertySaturationBudget semanticNormalizationPlanRewriteSystem rawShape))

projectionIdentityTerm :: PlanShape 'Canonical -> Either String (Fix PlanNode)
projectionIdentityTerm baseShape = do
  shape <-
    projectionShape (psDigestOf baseShape) (psDigestOf baseShape) schema01 schema01 [(0, 0), (1, 1)]
  pure
    ( projectionPlanTerm
        shape
        (canonicalPlanTerm baseShape)
    )

projectionComposeTerm :: PlanShape 'Canonical -> Either String (Fix PlanNode)
projectionComposeTerm baseShape = do
  outerShape <-
    projectionShape digestB digestC schema01 [slot 1] [(1, 1)]
  innerShape <-
    projectionShape (psDigestOf baseShape) digestB schema012 schema01 [(0, 0), (1, 1)]
  pure
    ( projectionPlanTerm
        outerShape
        ( projectionPlanTerm
            innerShape
            (canonicalPlanTerm baseShape)
        )
    )

restrictionIdentityTerm :: PlanShape 'Canonical -> Fix PlanNode
restrictionIdentityTerm baseShape =
  restrictionPlanTerm
    (restrictionShape (psDigestOf baseShape) (psDigestOf baseShape) [])
    (canonicalPlanTerm baseShape)

restrictionComposeTerm :: PlanShape 'Canonical -> Fix PlanNode
restrictionComposeTerm baseShape =
  restrictionPlanTerm
    (restrictionShape digestB digestC [(1, [20])])
    ( restrictionPlanTerm
        (restrictionShape (psDigestOf baseShape) digestB [(0, [10])])
        (canonicalPlanTerm baseShape)
    )

projectionRestrictionCommuteTerm :: PlanShape 'Canonical -> Either String (Fix PlanNode)
projectionRestrictionCommuteTerm baseShape = do
  shape <-
    projectionShape digestB digestC schema01 [slot 0] [(0, 0)]
  pure
    ( projectionPlanTerm
        shape
        ( restrictionPlanTerm
            (restrictionShape (psDigestOf baseShape) digestB [(0, [10])])
            (canonicalPlanTerm baseShape)
        )
    )

restrictionProjectionCommuteTerm :: PlanShape 'Canonical -> Either String (Fix PlanNode)
restrictionProjectionCommuteTerm baseShape = do
  shape <-
    projectionShape (psDigestOf baseShape) digestB schema01 [slot 0] [(0, 0)]
  pure
    ( restrictionPlanTerm
        (restrictionShape digestB digestC [(0, [10])])
        ( projectionPlanTerm
            shape
            (canonicalPlanTerm baseShape)
        )
    )

projectionRestrictionFuseTerm :: PlanShape 'Canonical -> Either String (Fix PlanNode)
projectionRestrictionFuseTerm baseShape = do
  outerShape <-
    projectionShape digestC digestD schema01 [slot 0] [(0, 0)]
  innerShape <-
    projectionShape (psDigestOf baseShape) digestB schema012 schema01 [(0, 0), (1, 1)]
  pure
    ( projectionPlanTerm
        outerShape
        ( restrictionPlanTerm
            (restrictionShape digestB digestC [(0, [10])])
            ( projectionPlanTerm
                innerShape
                (canonicalPlanTerm baseShape)
            )
        )
    )

coverMemberOrderState :: PlanShape 'Canonical -> PlanShape 'Canonical -> Either String PlanSaturationState
coverMemberOrderState baseShape otherShape = do
  let baseTerm = canonicalPlanTerm baseShape
      otherTerm = canonicalPlanTerm otherShape
  (_, stateWithBase) <- first show (insertPlanTerm baseTerm emptyPlanSaturationState)
  (_, stateWithMembers) <- first show (insertPlanTerm otherTerm stateWithBase)
  (_, stateWithCover) <-
    first show
      ( insertPlanTerm
          ( amalgamationPlanTerm
              (ShapeBuild.mkCoverShape digestFamily digestC (Set.fromList [psDigestOf baseShape, psDigestOf otherShape]))
              [otherTerm, baseTerm]
          )
          stateWithMembers
      )
  pure stateWithCover

coverSingletonTerm :: Fix PlanNode -> PlanShape 'Canonical -> Fix PlanNode
coverSingletonTerm baseTerm baseShape =
  amalgamationPlanTerm
    (ShapeBuild.mkCoverShape digestFamily (psDigestOf baseShape) (Set.singleton (psDigestOf baseShape)))
    [baseTerm]

coverageTransformIdentityTerm :: Fix PlanNode -> Fix PlanNode
coverageTransformIdentityTerm =
  coverageTransformPlanTerm (ShapeBuild.mkCoverageTransformShape CoveragePreserveExact)

coverageTransformComposeTerm :: Fix PlanNode -> Fix PlanNode
coverageTransformComposeTerm baseTerm =
  coverageTransformPlanTerm
    (ShapeBuild.mkCoverageTransformShape CoverageDowngradeLowerBound)
    (coverageTransformPlanTerm (ShapeBuild.mkCoverageTransformShape (CoverageExactByCover digestCoverProof)) baseTerm)

projectionShape ::
  StableDigest128 ->
  StableDigest128 ->
  [CanonSlot] ->
  [CanonSlot] ->
  [(Int, Int)] ->
  Either String (PlanShape 'Projection)
projectionShape sourceDigest targetDigest sourceSchema targetSchema slotPairs =
  first show $
    ShapeBuild.compileProjectionShape
      sourceDigest
      targetDigest
      sourceSchema
      targetSchema
      (IntMap.fromList (fmap (\(targetKey, sourceKey) -> (targetKey, slot sourceKey)) slotPairs))

restrictionShape ::
  StableDigest128 ->
  StableDigest128 ->
  [(Int, [Int])] ->
  PlanShape 'Restriction
restrictionShape sourceDigest targetDigest pinned =
  ShapeBuild.mkRestrictionShape
    sourceDigest
    targetDigest
    (IntMap.fromList (fmap (\(slotKey, values) -> (slotKey, IntSet.fromList values)) pinned))

foldMapA :: Applicative f => (value -> f [result]) -> [value] -> f [result]
foldMapA convert =
  fmap fold . traverse convert

psDigestOf :: PlanShape stage -> StableDigest128
psDigestOf =
  psDigest

slot :: Int -> CanonSlot
slot =
  CanonSlot

schema01 :: [CanonSlot]
schema01 =
  fmap slot [0, 1]

schema012 :: [CanonSlot]
schema012 =
  fmap slot [0, 1, 2]

digest :: Word64 -> StableDigest128
digest word =
  StableDigest128 word (word * 167 + 19)

digestB :: StableDigest128
digestB =
  digest 0x100

digestC :: StableDigest128
digestC =
  digest 0x101

digestD :: StableDigest128
digestD =
  digest 0x102

digestFamily :: StableDigest128
digestFamily =
  digest 0x200

digestCoverProof :: StableDigest128
digestCoverProof =
  digest 0x300

baseProgram :: RelProgram
baseProgram =
  program "law-base" 0 [atom 0 [0, 1, 2] [[1, 10, 100], [2, 20, 200]]] Nothing

otherProgram :: RelProgram
otherProgram =
  program "law-other" 0 [atom 1 [0] [[7], [8]]] Nothing

atomOrderProgram :: RelProgram
atomOrderProgram =
  program
    "law-atom-order"
    0
    [ atom 1 [1, 2] [[2, 4], [3, 5]],
      atom 0 [0, 1] [[1, 2], [2, 3]]
    ]
    Nothing

expectRight :: Show err => Either err value -> IO value
expectRight eitherValue =
  case eitherValue of
    Left err -> assertFailure (show err)
    Right value -> pure value
