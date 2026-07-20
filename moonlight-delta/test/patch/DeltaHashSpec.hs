{-# LANGUAGE DerivingStrategies #-}

module DeltaHashSpec
  ( deltaHashTests,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( StableHashEncoding,
    stableHashEncodingWord64LE,
  )
import Moonlight.Delta.Patch
  ( ApplyError (..),
    DeltaHashApplyError (..),
    DeltaHashBuildError (..),
    DeltaHashDigest (..),
    Digest128,
    MerkleDeltaHash,
    MultisetDeltaHash,
  )
import Moonlight.Delta.Patch qualified as Patch
import Test.QuickCheck
  ( Gen,
    Property,
    chooseInt,
    counterexample,
    forAll,
    listOf,
    (===),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)

deltaHashTests :: TestTree
deltaHashTests =
  testGroup
    "delta hash"
    [ testProperty "Merkle patch agrees with rebuild" merklePatchAgreesWithRebuild,
      testProperty "Merkle edit history descends to one root" merkleHistoryDescendsToCanonicalRoot,
      testProperty "multiset patch agrees with rebuild" multisetPatchAgreesWithRebuild,
      testProperty "multiset disjoint union composes through the patch action" multisetDisjointUnionHomomorphism,
      testProperty "multiset insertion and deletion are additive inverses" multisetAdditiveInverse,
      testProperty "multiset disjoint edit order is history-independent" multisetOrderIndependence,
      testCase "Merkle crosses the adaptive boundary in both directions" adaptiveBoundaryCase,
      testCase "all four cell edits update both strategies" cellEditsCase,
      testCase "stale patch rejection is preserved on both strategies" stalePatchCase,
      testCase "equal key representatives rebuild on both strategies" representativeRewriteCase,
      testCase "equal value representatives rebuild on both strategies" valueRepresentativeRewriteCase,
      testCase "flat mode admits localized path collisions" flatModeCollisionCase,
      testCase "Merkle construction reports one localized path collision" constructionCollisionCase,
      testCase "Merkle insertion reports a localized path collision atomically" updateCollisionCase,
      testCase "128-bit digest construction matches protocol goldens" digestGoldenCase
    ]

merklePatchAgreesWithRebuild :: Property
merklePatchAgreesWithRebuild =
  forAll smallMapGen $ \beforeState ->
    forAll smallMapGen $ \afterState ->
      case (buildTestMerkleDeltaHash beforeState, buildTestMerkleDeltaHash afterState) of
        (Right beforeDeltaHash, Right rebuiltDeltaHash) ->
          fmap
            merkleObservation
            (Patch.applyMerkleDeltaHash (Patch.diff beforeState afterState) beforeDeltaHash)
            === Right (merkleObservation rebuiltDeltaHash)
        (Left obstruction, _) ->
          counterexample ("unexpected initial key collision: " <> show obstruction) False
        (_, Left obstruction) ->
          counterexample ("unexpected rebuilt key collision: " <> show obstruction) False

merkleHistoryDescendsToCanonicalRoot :: Property
merkleHistoryDescendsToCanonicalRoot =
  forAll smallMapGen $ \middleState ->
    forAll smallMapGen $ \finalState ->
      case (buildTestMerkleDeltaHash Map.empty, buildTestMerkleDeltaHash finalState) of
        (Right emptyDeltaHash, Right rebuiltDeltaHash) ->
          let descended = do
                middleDeltaHash <-
                  Patch.applyMerkleDeltaHash
                    (Patch.diff Map.empty middleState)
                    emptyDeltaHash
                Patch.applyMerkleDeltaHash
                  (Patch.diff middleState finalState)
                  middleDeltaHash
           in fmap merkleObservation descended
                === Right (merkleObservation rebuiltDeltaHash)
        (Left obstruction, _) ->
          counterexample ("unexpected empty Merkle failure: " <> show obstruction) False
        (_, Left obstruction) ->
          counterexample ("unexpected final key collision: " <> show obstruction) False

multisetPatchAgreesWithRebuild :: Property
multisetPatchAgreesWithRebuild =
  forAll smallMapGen $ \beforeState ->
    forAll smallMapGen $ \afterState ->
      let beforeDeltaHash = buildTestMultisetDeltaHash beforeState
          rebuiltDeltaHash = buildTestMultisetDeltaHash afterState
       in fmap
            multisetObservation
            (Patch.applyMultisetDeltaHash (Patch.diff beforeState afterState) beforeDeltaHash)
            === Right (multisetObservation rebuiltDeltaHash)

multisetDisjointUnionHomomorphism :: Property
multisetDisjointUnionHomomorphism =
  forAll smallMapGen $ \authoritativeState ->
    let (leftState, rightState) =
          Map.partitionWithKey (\key _value -> even key) authoritativeState
        combinedState = Map.union leftState rightState
        combinedByAction =
          Patch.applyMultisetDeltaHash
            (Patch.diff Map.empty rightState)
            (buildTestMultisetDeltaHash leftState)
     in fmap multisetObservation combinedByAction
          === Right (multisetObservation (buildTestMultisetDeltaHash combinedState))

multisetAdditiveInverse :: Property
multisetAdditiveInverse =
  forAll smallMapGen $ \authoritativeState ->
    let freshKey = 4096
        freshValue = 17
        initialDeltaHash = buildTestMultisetDeltaHash authoritativeState
        roundTrip = do
          insertedDeltaHash <-
            Patch.applyMultisetDeltaHash
              (Patch.singleton freshKey (Patch.insert freshValue))
              initialDeltaHash
          Patch.applyMultisetDeltaHash
            (Patch.singleton freshKey (Patch.delete freshValue))
            insertedDeltaHash
     in fmap multisetObservation roundTrip
          === Right (multisetObservation initialDeltaHash)

multisetOrderIndependence :: Property
multisetOrderIndependence =
  forAll smallMapGen $ \authoritativeState ->
    let (leftState, rightState) =
          Map.partitionWithKey (\key _value -> even key) authoritativeState
        leftPatch = Patch.diff Map.empty leftState
        rightPatch = Patch.diff Map.empty rightState
        emptyDeltaHash = buildTestMultisetDeltaHash Map.empty
        leftThenRight = do
          leftDeltaHash <- Patch.applyMultisetDeltaHash leftPatch emptyDeltaHash
          Patch.applyMultisetDeltaHash rightPatch leftDeltaHash
        rightThenLeft = do
          rightDeltaHash <- Patch.applyMultisetDeltaHash rightPatch emptyDeltaHash
          Patch.applyMultisetDeltaHash leftPatch rightDeltaHash
        expectedObservation =
          multisetObservation (buildTestMultisetDeltaHash authoritativeState)
     in (fmap multisetObservation leftThenRight, fmap multisetObservation rightThenLeft)
          === (Right expectedObservation, Right expectedObservation)

adaptiveBoundaryCase :: IO ()
adaptiveBoundaryCase = do
  let boundaryState = sequentialState deltaHashFlatBoundarySize
      insertedKey = deltaHashFlatBoundarySize
      expandedState = Map.insert insertedKey insertedKey boundaryState
  boundaryDeltaHash <- requireRight (buildTestMerkleDeltaHash boundaryState)
  expandedDeltaHash <-
    requireRight
      ( Patch.applyMerkleDeltaHash
          (Patch.singleton insertedKey (Patch.insert insertedKey))
          boundaryDeltaHash
      )
  rebuiltExpandedDeltaHash <- requireRight (buildTestMerkleDeltaHash expandedState)
  merkleObservation expandedDeltaHash
    @?= merkleObservation rebuiltExpandedDeltaHash
  contractedDeltaHash <-
    requireRight
      ( Patch.applyMerkleDeltaHash
          (Patch.singleton insertedKey (Patch.delete insertedKey))
          expandedDeltaHash
      )
  merkleObservation contractedDeltaHash
    @?= merkleObservation boundaryDeltaHash

cellEditsCase :: IO ()
cellEditsCase = do
  let initialState = Map.fromList [(1, 10), (2, 20)]
      expectedState = Map.fromList [(1, 11), (3, 30)]
      patchValue =
        Patch.fromList
          [ (1, Patch.replace 10 11),
            (2, Patch.delete 20),
            (3, Patch.insert 30),
            (4, Patch.assertAbsent)
          ]
  initialMerkleDeltaHash <- requireRight (buildTestMerkleDeltaHash initialState)
  updatedMerkleDeltaHash <-
    requireRight (Patch.applyMerkleDeltaHash patchValue initialMerkleDeltaHash)
  rebuiltMerkleDeltaHash <- requireRight (buildTestMerkleDeltaHash expectedState)
  merkleObservation updatedMerkleDeltaHash
    @?= merkleObservation rebuiltMerkleDeltaHash
  updatedMultisetDeltaHash <-
    requireRight
      ( Patch.applyMultisetDeltaHash
          patchValue
          (buildTestMultisetDeltaHash initialState)
      )
  multisetObservation updatedMultisetDeltaHash
    @?= multisetObservation (buildTestMultisetDeltaHash expectedState)

stalePatchCase :: IO ()
stalePatchCase = do
  let initialState = Map.singleton 1 10
      patchValue = Patch.singleton 1 (Patch.replace 9 11)
      expectedObstruction =
        DeltaHashPatchRejected
          ApplyBeforeMismatch
            { mismatchKey = 1,
              expectedBefore = Just 9,
              actualBefore = Just 10
            }
  initialMerkleDeltaHash <- requireRight (buildTestMerkleDeltaHash initialState)
  case Patch.applyMerkleDeltaHash patchValue initialMerkleDeltaHash of
    Left obstruction ->
      obstruction @?= expectedObstruction
    Right _deltaHash ->
      assertFailure "expected stale Merkle patch to be rejected"
  case Patch.applyMultisetDeltaHash patchValue (buildTestMultisetDeltaHash initialState) of
    Left obstruction ->
      obstruction @?= expectedObstruction
    Right _deltaHash ->
      assertFailure "expected stale multiset patch to be rejected"

representativeRewriteCase :: IO ()
representativeRewriteCase = do
  let stateEntry key =
        (RepresentativeKey key StateRepresentative, key * 10)
      expectedEntry key
        | key == 1 = (RepresentativeKey key PatchRepresentative, 11)
        | otherwise = stateEntry key
      keys = [1 .. deltaHashFlatBoundarySize + 1]
      initialState = Map.fromDistinctAscList (fmap stateEntry keys)
      expectedState = Map.fromDistinctAscList (fmap expectedEntry keys)
      patchKey = RepresentativeKey 1 PatchRepresentative
      patchValue = Patch.singleton patchKey (Patch.replace 10 11)
  initialMerkleDeltaHash <-
    requireRight
      (Patch.buildMerkleDeltaHash representativeKeyEncoding intEncoding initialState)
  updatedMerkleDeltaHash <-
    requireRight (Patch.applyMerkleDeltaHash patchValue initialMerkleDeltaHash)
  rebuiltMerkleDeltaHash <-
    requireRight
      (Patch.buildMerkleDeltaHash representativeKeyEncoding intEncoding expectedState)
  Patch.merkleDeltaHashDigest updatedMerkleDeltaHash
    @?= Patch.merkleDeltaHashDigest rebuiltMerkleDeltaHash
  fmap (representativeKind . fst) (Map.lookupGE patchKey (Patch.merkleDeltaHashState updatedMerkleDeltaHash))
    @?= Just PatchRepresentative
  let initialMultisetDeltaHash =
        Patch.buildMultisetDeltaHash representativeKeyEncoding intEncoding initialState
      rebuiltMultisetDeltaHash =
        Patch.buildMultisetDeltaHash representativeKeyEncoding intEncoding expectedState
  updatedMultisetDeltaHash <-
    requireRight (Patch.applyMultisetDeltaHash patchValue initialMultisetDeltaHash)
  Patch.multisetDeltaHashDigest updatedMultisetDeltaHash
    @?= Patch.multisetDeltaHashDigest rebuiltMultisetDeltaHash
  fmap (representativeKind . fst) (Map.lookupGE patchKey (Patch.multisetDeltaHashState updatedMultisetDeltaHash))
    @?= Just PatchRepresentative

valueRepresentativeRewriteCase :: IO ()
valueRepresentativeRewriteCase = do
  let stateValue = RepresentativeValue 10 StateRepresentative
      patchBefore = RepresentativeValue 10 PatchRepresentative
      patchAfter = RepresentativeValue 11 PatchRepresentative
      initialState = Map.singleton 1 stateValue
      expectedState = Map.singleton 1 patchAfter
      patchValue = Patch.singleton 1 (Patch.replace patchBefore patchAfter)
  initialMerkleDeltaHash <-
    requireRight
      (Patch.buildMerkleDeltaHash intEncoding representativeValueEncoding initialState)
  updatedMerkleDeltaHash <-
    requireRight (Patch.applyMerkleDeltaHash patchValue initialMerkleDeltaHash)
  rebuiltMerkleDeltaHash <-
    requireRight
      (Patch.buildMerkleDeltaHash intEncoding representativeValueEncoding expectedState)
  Patch.merkleDeltaHashDigest updatedMerkleDeltaHash
    @?= Patch.merkleDeltaHashDigest rebuiltMerkleDeltaHash
  let initialMultisetDeltaHash =
        Patch.buildMultisetDeltaHash intEncoding representativeValueEncoding initialState
      rebuiltMultisetDeltaHash =
        Patch.buildMultisetDeltaHash intEncoding representativeValueEncoding expectedState
  updatedMultisetDeltaHash <-
    requireRight (Patch.applyMultisetDeltaHash patchValue initialMultisetDeltaHash)
  Patch.multisetDeltaHashDigest updatedMultisetDeltaHash
    @?= Patch.multisetDeltaHashDigest rebuiltMultisetDeltaHash

flatModeCollisionCase :: IO ()
flatModeCollisionCase = do
  let authoritativeState = Map.fromList [(1, 10), (2, 20)]
  deltaHashValue <-
    requireRight
      (Patch.buildMerkleDeltaHash constantEncoding intEncoding authoritativeState)
  Patch.merkleDeltaHashState deltaHashValue @?= authoritativeState

constructionCollisionCase :: IO ()
constructionCollisionCase =
  case
      Patch.buildMerkleDeltaHash
        boundaryCollisionEncoding
        intEncoding
        (sequentialState (deltaHashFlatBoundarySize + 1))
    of
      Left DeltaHashKeyCollision {deltaHashExistingKey, deltaHashIncomingKey} ->
        (deltaHashExistingKey, deltaHashIncomingKey)
          @?= (0, deltaHashFlatBoundarySize)
      Right _deltaHash ->
        assertFailure "expected distinct keys with one digest path to be rejected"

updateCollisionCase :: IO ()
updateCollisionCase = do
  let authoritativeState = sequentialState deltaHashFlatBoundarySize
      incomingKey = deltaHashFlatBoundarySize
  initialDeltaHash <-
    requireRight
      (Patch.buildMerkleDeltaHash boundaryCollisionEncoding intEncoding authoritativeState)
  case
      Patch.applyMerkleDeltaHash
        (Patch.singleton incomingKey (Patch.insert incomingKey))
        initialDeltaHash
    of
      Left (DeltaHashUpdateRejected DeltaHashKeyCollision {}) ->
        Patch.merkleDeltaHashState initialDeltaHash @?= authoritativeState
      Left obstruction ->
        assertFailure ("expected collision obstruction, received: " <> show obstruction)
      Right _deltaHash ->
        assertFailure "expected insertion collision to be rejected"

digestGoldenCase :: IO ()
digestGoldenCase = do
  flatDeltaHash <-
    requireRight (buildTestMerkleDeltaHash (Map.fromList [(1, 10), (2, 20)]))
  Patch.merkleDeltaHashDigest flatDeltaHash
    @?= DeltaHashDigest 0x897d1ee3ac05ae2f 0x24244b4e097f0dd5
  merkleDeltaHash <-
    requireRight
      (buildTestMerkleDeltaHash (sequentialState (deltaHashFlatBoundarySize + 1)))
  Patch.merkleDeltaHashDigest merkleDeltaHash
    @?= DeltaHashDigest 0x34c7f2a72e9f90b2 0xd725d3dd1caef993
  Patch.multisetDeltaHashDigest
    (buildTestMultisetDeltaHash (Map.fromList [(1, 10), (2, 20)]))
    @?= DeltaHashDigest 0xa3b63bd3bb34ff9e 0x2e36babb08d75d1e

merkleObservation :: MerkleDeltaHash Int Int -> (Map Int Int, Digest128)
merkleObservation deltaHashValue =
  (Patch.merkleDeltaHashState deltaHashValue, Patch.merkleDeltaHashDigest deltaHashValue)

multisetObservation :: MultisetDeltaHash Int Int -> (Map Int Int, Digest128)
multisetObservation deltaHashValue =
  (Patch.multisetDeltaHashState deltaHashValue, Patch.multisetDeltaHashDigest deltaHashValue)

buildTestMerkleDeltaHash ::
  Map Int Int ->
  Either (DeltaHashBuildError Int) (MerkleDeltaHash Int Int)
buildTestMerkleDeltaHash =
  Patch.buildMerkleDeltaHash intEncoding intEncoding

buildTestMultisetDeltaHash :: Map Int Int -> MultisetDeltaHash Int Int
buildTestMultisetDeltaHash =
  Patch.buildMultisetDeltaHash intEncoding intEncoding

sequentialState :: Int -> Map Int Int
sequentialState stateSize =
  Map.fromDistinctAscList (fmap (\key -> (key, key)) [0 .. stateSize - 1])

intEncoding :: Int -> StableHashEncoding
intEncoding =
  stableHashEncodingWord64LE . fromIntegral

boundaryCollisionEncoding :: Int -> StableHashEncoding
boundaryCollisionEncoding key
  | key == deltaHashFlatBoundarySize = intEncoding 0
  | otherwise = intEncoding key

constantEncoding :: Int -> StableHashEncoding
constantEncoding _value =
  stableHashEncodingWord64LE 0

data RepresentativeKind
  = StateRepresentative
  | PatchRepresentative
  deriving stock (Eq, Ord, Show)

data RepresentativeKey = RepresentativeKey
  { representativeId :: !Int,
    representativeKind :: !RepresentativeKind
  }
  deriving stock (Show)

instance Eq RepresentativeKey where
  left == right =
    representativeId left == representativeId right

instance Ord RepresentativeKey where
  compare left right =
    compare (representativeId left) (representativeId right)

representativeKeyEncoding :: RepresentativeKey -> StableHashEncoding
representativeKeyEncoding representative =
  intEncoding (representativeId representative)
    <> representativeKindEncoding (representativeKind representative)

data RepresentativeValue = RepresentativeValue
  { representativeValueId :: !Int,
    representativeValueKind :: !RepresentativeKind
  }
  deriving stock (Show)

instance Eq RepresentativeValue where
  left == right =
    representativeValueId left == representativeValueId right

representativeValueEncoding :: RepresentativeValue -> StableHashEncoding
representativeValueEncoding representative =
  intEncoding (representativeValueId representative)
    <> representativeKindEncoding (representativeValueKind representative)

representativeKindEncoding :: RepresentativeKind -> StableHashEncoding
representativeKindEncoding representative =
  stableHashEncodingWord64LE
    ( case representative of
        StateRepresentative -> 0
        PatchRepresentative -> 1
    )

smallMapGen :: Gen (Map Int Int)
smallMapGen =
  Map.fromList <$> listOf ((,) <$> chooseInt (-32, 32) <*> chooseInt (-1000, 1000))

deltaHashFlatBoundarySize :: Int
deltaHashFlatBoundarySize =
  256

requireRight :: Show obstruction => Either obstruction value -> IO value
requireRight result =
  case result of
    Left obstruction ->
      assertFailure (show obstruction)
    Right value ->
      pure value
