module Moonlight.Sheaf.Core.Section.Congruence.EquivalenceSpec
  ( tests,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Moonlight.Algebra
  ( JoinSemilattice (..),
  )
import Moonlight.Sheaf.Section.Congruence.Equivalence.Relation
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck
  ( Gen,
    Property,
    chooseInt,
    counterexample,
    forAll,
    listOf,
    property,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "finite-equivalence-relation"
    [ testCase "partial rep map completes singleton blocks over the declared universe" testSingletonCompletion,
      testCase "domain merges normalize through existing representatives" testDomainMergeClosure,
      testCase "canonical union normalization drops duplicates and trivial pairs" testCanonicalUnionNormalization,
      testCase "image rejects a partial key map" testImageRejectsPartialMap,
      testCase "image closes all projected equalities under non-injective maps" testImageClosesNonInjectiveProjection,
      testCase "endomap constructor rejects partial maps" testEndomapRejectsPartialMap,
      testCase "checked endomap application rejects domain mismatch" testCheckedEndomapRejectsDomainMismatch,
      testCase "domain equivalence rejects relation domain mismatch" testDomainEquivalenceRejectsRelationDomainMismatch,
      testCase "domain endomap rejects invalid carrier maps at construction" testDomainEndomapRejectsInvalidMap,
      testCase "domain endomap application merges exactly the projected blocks" testDomainEndomapProjectsBlocks,
      testCase "equivalence rejects keys outside the declared domain" testEquivalenceRejectsOutOfDomainKeys,
      testProperty "join is idempotent" propJoinIdempotent,
      testProperty "join is commutative" propJoinCommutative,
      testProperty "join is associative" propJoinAssociative,
      testProperty "the discrete partition is the join unit" propDiscreteIsJoinUnit,
      testProperty "join is an upper bound in refinement order" propJoinUpperBound
    ]

testSingletonCompletion :: Assertion
testSingletonCompletion =
  assertRight "expected relation construction"
    (equivalenceFromPartialRepMap (IntSet.fromList [1, 2, 3, 10]) (IntMap.fromList [(1, 1 :: Int), (2, 1)])) $ \relationValue -> do
      equivalenceDomain relationValue @?= IntSet.fromList [1, 2, 3, 10]
      equivalenceRepOfBase relationValue @?= IntMap.fromList [(1, 1), (2, 1), (3, 3), (10, 10)]
      equivalenceMembersByRep relationValue @?= IntMap.fromList [(1, IntSet.fromList [1, 2]), (3, IntSet.singleton 3), (10, IntSet.singleton 10)]
      validateEquivalenceRelation relationValue @?= Right ()

testDomainMergeClosure :: Assertion
testDomainMergeClosure =
  case withEquivalenceDomain (IntSet.fromList [1, 2, 3, 4]) checkMergeClosure of
    Left failureValue ->
      assertFailure ("expected domain construction, got " <> show failureValue)
    Right assertion ->
      assertion
  where
    checkMergeClosure ::
      EquivalenceDomain carrier Int ->
      Assertion
    checkMergeClosure domain =
      assertRight "expected relation construction"
        (equivalenceFromPartialRepMap (IntSet.fromList [1, 2, 3, 4]) (IntMap.fromList [(1, 1 :: Int), (2, 1)])) $ \relationValue ->
          assertRight "expected domain equivalence"
            (mkDomainEquivalence domain relationValue) $ \domainRelation -> do
              assertRight "expected checked domain merges"
                (applyDomainEquivalenceMergesCounted [(3, 4), (2, 4)] domainRelation) $ \(merged, changed, mergeCount) -> do
                  let closed = domainEquivalenceRaw merged
                  equivalenceRepresentative closed 4 @?= Just 1
                  equivalenceMembersByRep closed @?= IntMap.singleton 1 (IntSet.fromList [1, 2, 3, 4])
                  mergeCount @?= 2
                  assertBool "merge should report touched members" (not (IntSet.null changed))
                  validateEquivalenceRelation closed @?= Right ()

testCanonicalUnionNormalization :: Assertion
testCanonicalUnionNormalization =
  assertRight "expected relation construction"
    (equivalenceFromPartialRepMap (IntSet.fromList [1, 2, 3]) (IntMap.fromList [(1, 1 :: Int), (3, 1)])) $ \relationValue ->
      canonicalizeEquivalenceUnions relationValue [(1, 2), (3, 1), (2, 1)] @?= Right [(1, 2)]

testImageRejectsPartialMap :: Assertion
testImageRejectsPartialMap =
  assertRight "expected relation construction"
    (equivalenceFromPartialRepMap (IntSet.fromList [1, 2]) (IntMap.fromList [(1, 1 :: Int), (2, 1)])) $ \relationValue ->
      equivalenceImage (IntMap.singleton 1 (10 :: Int)) (IntSet.fromList [10, 20]) relationValue
        @?= Left (EquivalenceImageMissingSourceKey 2)

testImageClosesNonInjectiveProjection :: Assertion
testImageClosesNonInjectiveProjection =
  assertRight "expected relation construction"
    (equivalenceFromPairs (IntSet.fromList [0, 1, 2, 3]) [(0 :: Int, 1), (2, 3)]) $ \sourceRelation ->
      assertRight "expected image relation"
        ( equivalenceImage
            (IntMap.fromList [(0, 0 :: Int), (1, 4), (2, 2), (3, 4)])
            (IntSet.fromList [0, 2, 4])
            sourceRelation
        ) $ \imageRelation -> do
          equivalenceRepresentativeAtKey imageRelation 0 @?= equivalenceRepresentativeAtKey imageRelation 2
          equivalenceRepresentativeAtKey imageRelation 2 @?= equivalenceRepresentativeAtKey imageRelation 4

testEndomapRejectsPartialMap :: Assertion
testEndomapRejectsPartialMap =
  mkEquivalenceEndomap (IntSet.fromList [0, 1]) (IntMap.singleton 0 (0 :: Int))
    @?= Left (EquivalenceEndomapMissingDomainKey 1)

testCheckedEndomapRejectsDomainMismatch :: Assertion
testCheckedEndomapRejectsDomainMismatch =
  case withEquivalenceDomain (IntSet.fromList [0, 1, 2]) checkMismatch of
    Left failureValue ->
      assertFailure ("expected domain construction, got " <> show failureValue)
    Right assertion ->
      assertion
  where
    checkMismatch ::
      EquivalenceDomain carrier Int ->
      Assertion
    checkMismatch domain =
      assertRight "expected relation construction"
        (equivalenceFromPairs (IntSet.fromList [0, 1, 2]) [(0 :: Int, 1)]) $ \sourceRelation ->
          assertRight "expected domain equivalence"
            (mkDomainEquivalence domain sourceRelation) $ \domainRelation ->
              assertRight "expected endomap construction"
                (mkEquivalenceEndomap (IntSet.fromList [0, 1]) (IntMap.fromList [(0, 0 :: Int), (1, 1)])) $ \endomap ->
                  fmap domainEquivalenceRaw (applyCheckedDomainEndomap endomap domainRelation)
                    @?= Left
                      ( EquivalenceEndomapDomainMismatch
                          (IntSet.fromList [0, 1])
                          (IntSet.fromList [0, 1, 2])
                      )

testDomainEquivalenceRejectsRelationDomainMismatch :: Assertion
testDomainEquivalenceRejectsRelationDomainMismatch =
  assertRight "expected relation construction"
    (equivalenceFromPairs (IntSet.fromList [0, 1, 2]) [(0 :: Int, 1)]) $ \relationValue ->
      case withEquivalenceDomain (IntSet.fromList [0, 1]) $ \domain ->
        fmap (const ()) (mkDomainEquivalence domain relationValue) of
        Left failureValue ->
          assertFailure ("expected domain construction, got " <> show failureValue)
        Right outcome ->
          outcome
            @?= Left
              ( EquivalenceDomainMismatch
                  (IntSet.fromList [0, 1])
                  (IntSet.fromList [0, 1, 2])
              )

testDomainEndomapRejectsInvalidMap :: Assertion
testDomainEndomapRejectsInvalidMap =
  case withEquivalenceDomain (IntSet.fromList [0, 1]) $ \domain ->
    fmap (const ()) (mkDomainEndomap domain (IntMap.singleton 0 (0 :: Int))) of
    Left failureValue ->
      assertFailure ("expected domain construction, got " <> show failureValue)
    Right outcome ->
      outcome @?= Left (EquivalenceEndomapMissingDomainKey 1)

testDomainEndomapProjectsBlocks :: Assertion
testDomainEndomapProjectsBlocks =
  case withEquivalenceDomain (IntSet.fromList [0, 1, 2, 3]) checkProjection of
    Left failureValue ->
      assertFailure ("expected domain construction, got " <> show failureValue)
    Right assertion ->
      assertion
  where
    checkProjection ::
      EquivalenceDomain carrier Int ->
      Assertion
    checkProjection domain =
      assertRight "expected domain image" (compileDomainImage domain) $ \imageRelation -> do
        equivalenceRepresentativeAtKey imageRelation 0 @?= equivalenceRepresentativeAtKey imageRelation 2
        equivalenceRepresentativeAtKey imageRelation 1 @?= Just 1
        equivalenceRepresentativeAtKey imageRelation 3 @?= Just 3
        validateEquivalenceRelation imageRelation @?= Right ()

    compileDomainImage ::
      EquivalenceDomain carrier Int ->
      Either EquivalenceRelationError (EquivalenceRelation Int)
    compileDomainImage domain = do
      relationValue <-
        equivalenceFromPairs
          (IntSet.fromList [0, 1, 2, 3])
          [(0 :: Int, 1), (2, 3)]
      domainEndomap <-
        mkDomainEndomap
          domain
          (IntMap.fromList [(0, 0 :: Int), (1, 2), (2, 2), (3, 2)])
      domainRelation <-
        mkDomainEquivalence domain relationValue
      pure (domainEquivalenceRaw (applyDomainEndomap domainEndomap domainRelation))

testEquivalenceRejectsOutOfDomainKeys :: Assertion
testEquivalenceRejectsOutOfDomainKeys =
  assertRight "expected discrete relation" (discreteEquivalence (IntSet.fromList [0, 1])) $ \relationValue -> do
    assertBool "two missing keys are not equivalent" (not (equivalenceEquivalent relationValue (2 :: Int) 3))
    assertBool "missing key is not equivalent to present key" (not (equivalenceEquivalent relationValue (2 :: Int) 0))
    assertBool "present key remains reflexive" (equivalenceEquivalent relationValue (0 :: Int) 0)

joinLawDomainKeys :: IntSet
joinLawDomainKeys =
  IntSet.fromList [0 .. 7]

genPartitionPairs :: Gen [(Int, Int)]
genPartitionPairs =
  listOf ((,) <$> chooseInt (0, 7) <*> chooseInt (0, 7))

compileLawWitness ::
  EquivalenceDomain carrier Int ->
  [(Int, Int)] ->
  Either EquivalenceRelationError (DomainEquivalence carrier Int)
compileLawWitness domain pairs =
  equivalenceFromPairs joinLawDomainKeys pairs >>= mkDomainEquivalence domain

collapseLawOutcome :: Either EquivalenceRelationError Property -> Property
collapseLawOutcome =
  either (\failureValue -> counterexample (show failureValue) (property False)) id

joinLawProperty ::
  ( forall carrier.
    EquivalenceDomain carrier Int ->
    DomainEquivalence carrier Int ->
    DomainEquivalence carrier Int ->
    DomainEquivalence carrier Int ->
    Property
  ) ->
  Property
joinLawProperty continue =
  forAll genPartitionPairs $ \leftPairs ->
    forAll genPartitionPairs $ \middlePairs ->
      forAll genPartitionPairs $ \rightPairs ->
        collapseLawOutcome $
          withEquivalenceDomain joinLawDomainKeys $ \domain ->
            collapseLawOutcome $ do
              leftWitness <- compileLawWitness domain leftPairs
              middleWitness <- compileLawWitness domain middlePairs
              rightWitness <- compileLawWitness domain rightPairs
              pure (continue domain leftWitness middleWitness rightWitness)

normalizedRaw :: DomainEquivalence carrier Int -> EquivalenceRelation Int
normalizedRaw =
  domainEquivalenceRaw

propJoinIdempotent :: Property
propJoinIdempotent =
  joinLawProperty $ \_ leftWitness _ _ ->
    normalizedRaw (join leftWitness leftWitness) === normalizedRaw leftWitness

propJoinCommutative :: Property
propJoinCommutative =
  joinLawProperty $ \_ leftWitness middleWitness _ ->
    normalizedRaw (join leftWitness middleWitness) === normalizedRaw (join middleWitness leftWitness)

propJoinAssociative :: Property
propJoinAssociative =
  joinLawProperty $ \_ leftWitness middleWitness rightWitness ->
    normalizedRaw (join (join leftWitness middleWitness) rightWitness)
      === normalizedRaw (join leftWitness (join middleWitness rightWitness))

propDiscreteIsJoinUnit :: Property
propDiscreteIsJoinUnit =
  joinLawProperty $ \domain leftWitness _ _ ->
    collapseLawOutcome $ do
      discreteRelation <- discreteEquivalence joinLawDomainKeys
      bottomWitness <- mkDomainEquivalence domain discreteRelation
      pure $
        (normalizedRaw (join bottomWitness leftWitness), normalizedRaw (join leftWitness bottomWitness))
          === (normalizedRaw leftWitness, normalizedRaw leftWitness)

propJoinUpperBound :: Property
propJoinUpperBound =
  forAll (chooseInt (0, 7)) $ \probeLeft ->
    forAll (chooseInt (0, 7)) $ \probeRight ->
      joinLawProperty $ \_ leftWitness middleWitness _ ->
        let joined = domainEquivalenceRaw (join leftWitness middleWitness)
            lower = domainEquivalenceRaw leftWitness
         in property $
              not (equivalenceEquivalent lower probeLeft probeRight)
                || equivalenceEquivalent joined probeLeft probeRight

assertRight :: Show left => String -> Either left right -> (right -> Assertion) -> Assertion
assertRight label outcome onRight =
  case outcome of
    Left failureValue -> assertFailure (label <> ": " <> show failureValue)
    Right successValue -> onRight successValue
