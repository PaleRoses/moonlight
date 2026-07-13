{-# LANGUAGE RankNTypes #-}

module Moonlight.Sheaf.Core.Section.Stalk.CongruenceSpec
  ( tests,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import GHC.Stack (HasCallStack)
import Moonlight.Core (DenseKey (..))
import Moonlight.Sheaf.Section.Congruence.Equivalence.Relation
import Moonlight.Sheaf.Section.Certified
  ( SectionCertification (..),
    certifySectionCompatibility,
  )
import Moonlight.Sheaf.Section.Model
  ( sheafModelRestrictions,
  )
import Moonlight.Sheaf.Section.Morphism
  ( Restriction (..),
    RestrictionArrow (..),
    RestrictionKind (..),
  )
import Moonlight.Sheaf.Section.Restriction
  ( restrictionEntries,
  )
import Moonlight.Sheaf.Section.Restriction.Law
  ( RestrictionLawFailure (..),
    checkRestrictionCompositionLaw,
    checkRestrictionIdentityLaw,
  )
import Moonlight.Sheaf.Section.Stalk
  ( MergeObstruction (..),
    restrictStalk,
    stalkMismatches,
  )
import Moonlight.Sheaf.Section.Stalk.Congruence.Carrier
import Moonlight.Sheaf.Section.Stalk.Congruence.Mismatch
import Moonlight.Sheaf.Section.Stalk.Congruence.Model
import Moonlight.Sheaf.Section.Store.State
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "congruence-stalk"
    [ testCase "mkGlobalCarrier rejects duplicate atoms before dense indexing" testDuplicateCarrierAtoms,
      testCase "stalk construction rejects visible keys outside carrier" testVisibleOutsideCarrier,
      testCase "stalk construction delegates outside-domain pairs to equivalence kernel" testPairOutsideCarrier,
      testCase "restriction construction rejects partial carrier maps" testPartialRestrictionMap,
      testCase "restriction construction rejects map keys outside carrier" testRestrictionMapKeyOutsideCarrier,
      testCase "restriction construction rejects map images outside carrier" testRestrictionMapImageOutsideCarrier,
      testCase "restriction construction rejects source-visible images outside target-visible" testSourceVisibleImageOutsideTarget,
      testCase "restriction maps full relation image and replaces visible support" testRestrictionImage,
      testCase "restriction rejects carrier mismatch instead of identity" testRestrictionRejectsCarrierMismatch,
      testCase "mismatches report carrier, visible, then representatives" testMismatchOrdering,
      testCase "merge is idempotent after normalization" testMergeIdempotence,
      testCase "merge is commutative up to normalization" testMergeCommutativity,
      testCase "merge rejects different visible domains before relation union" testMergeRejectsVisibleMismatch,
      testCase "checked identity restriction has no mismatches" testCheckedIdentityRestriction,
      testCase "checked restriction composition agrees with composed endomap" testCheckedRestrictionComposition,
      testCase "merge closes the full-carrier relation union" testMergeClosesFullCarrierRelation,
      testCase "prepared compiler rejects invalid local data before total algebra exists" testPreparedCompilerRejectsInvalidLocalData,
      testCase "prepared restriction is total over compiled carrier domain" testPreparedRestrictionIsTotal,
      testCase "prepared congruence model composes with certification" testPreparedModelCertifiesCompatibleSection,
      testCase "prepared congruence restrictions compose through existing law checker" testPreparedRestrictionLaws
    ]

testDuplicateCarrierAtoms :: Assertion
testDuplicateCarrierAtoms =
  mkGlobalCarrier (CarrierId 0) ["a" :: String, "a"]
    @?= Left (CongruenceDuplicateCarrierAtoms ("a" :| []))

testVisibleOutsideCarrier :: Assertion
testVisibleOutsideCarrier = do
  carrier <- expectRight (mkGlobalCarrier (CarrierId 0) ["a" :: String, "b"])
  mkDiscreteCongruenceStalk carrier [key 0, key 2]
    @?= Left
      ( CongruenceVisibleKeyOutsideCarrier
          CongruenceStalkVisible
          2
      )

testPairOutsideCarrier :: Assertion
testPairOutsideCarrier = do
  carrier <- expectRight (mkGlobalCarrier (CarrierId 0) ["a" :: String, "b"])
  mkCongruenceStalkFromPairs carrier [] [(key 0, key 2)]
    @?= Left
      (CongruenceRelationFailure (EquivalencePairOutsideDomain 2))

testPartialRestrictionMap :: Assertion
testPartialRestrictionMap = do
  carrier <- expectRight (mkGlobalCarrier (CarrierId 0) ["a" :: String, "b"])
  mkCongruenceRestriction
    carrier
    [key 0]
    [key 1]
    (IntMap.fromList [(0, key 0)])
    @?= Left (CongruenceRestrictionMapMissingCarrierKey 1)

testRestrictionMapKeyOutsideCarrier :: Assertion
testRestrictionMapKeyOutsideCarrier = do
  carrier <- expectRight (mkGlobalCarrier (CarrierId 0) ["a" :: String, "b"])
  mkCongruenceRestriction
    carrier
    [key 0]
    [key 1]
    (IntMap.fromList [(0, key 0), (1, key 1), (2, key 1)])
    @?= Left (CongruenceRestrictionMapKeyOutsideCarrier 2)

testRestrictionMapImageOutsideCarrier :: Assertion
testRestrictionMapImageOutsideCarrier = do
  carrier <- expectRight (mkGlobalCarrier (CarrierId 0) ["a" :: String, "b"])
  mkCongruenceRestriction
    carrier
    [key 0]
    [key 1]
    (IntMap.fromList [(0, key 0), (1, key 2)])
    @?= Left (CongruenceRestrictionMapImageOutsideCarrier 1 2)

testSourceVisibleImageOutsideTarget :: Assertion
testSourceVisibleImageOutsideTarget = do
  carrier <- expectRight (mkGlobalCarrier (CarrierId 0) ["a" :: String, "b", "c"])
  mkCongruenceRestriction
    carrier
    [key 0]
    [key 1]
    (IntMap.fromList [(0, key 2), (1, key 1), (2, key 2)])
    @?= Left (CongruenceRestrictionImageOutsideTargetVisible 0 2)

testRestrictionImage :: Assertion
testRestrictionImage = do
  carrier <- expectRight (mkGlobalCarrier (CarrierId 0) ["a" :: String, "b", "c", "d", "e"])
  source <-
    expectRight $
      mkCongruenceStalkFromPairs
        carrier
        [key 1, key 3]
        [(key 0, key 1), (key 2, key 3)]
  restriction <-
    expectRight $
      mkCongruenceRestriction
        carrier
        [key 1, key 3]
        [key 4]
        ( IntMap.fromList
            [ (0, key 0),
              (1, key 4),
              (2, key 2),
              (3, key 4),
              (4, key 4)
            ]
        )

  restricted <- expectRight (restrictCongruenceStalk restriction source)
  let relationValue =
        congruenceStalkRelation restricted

  congruenceStalkVisible restricted @?= IntSet.singleton 4
  equivalenceRepresentativeAtKey relationValue 0 @?= equivalenceRepresentativeAtKey relationValue 2
  equivalenceRepresentativeAtKey relationValue 2 @?= equivalenceRepresentativeAtKey relationValue 4

testRestrictionRejectsCarrierMismatch :: Assertion
testRestrictionRejectsCarrierMismatch = do
  stalkCarrier <- expectRight (mkGlobalCarrier (CarrierId 0) ["a" :: String, "b"])
  restrictionCarrier <- expectRight (mkGlobalCarrier (CarrierId 1) ["x" :: String, "y"])
  source <- expectRight (mkDiscreteCongruenceStalk stalkCarrier [key 0])
  restriction <-
    expectRight $
      mkCongruenceRestriction
        restrictionCarrier
        [key 0]
        [key 1]
        (IntMap.fromList [(0, key 1), (1, key 1)])

  restrictCongruenceStalk restriction source
    @?= Left
      ( CongruenceRestrictionCarrierMismatch
          (CarrierId 1)
          (CarrierId 0)
          [(key 0, "x"), (key 1, "y")]
          [(key 0, "a"), (key 1, "b")]
      )

testMismatchOrdering :: Assertion
testMismatchOrdering = do
  carrier0 <- expectRight (mkGlobalCarrier (CarrierId 0) ["a" :: String, "b", "c"])
  carrier1 <- expectRight (mkGlobalCarrier (CarrierId 1) ["a" :: String, "b", "d"])

  leftStalk <-
    expectRight $
      mkCongruenceStalkFromPairs
        carrier0
        [key 0, key 1]
        [(key 0, key 1)]

  rightCarrierMismatch <-
    expectRight $
      mkDiscreteCongruenceStalk
        carrier1
        [key 0]

  case congruenceMismatches leftStalk rightCarrierMismatch of
    CongruenceCarrierMismatch _ _ _ _ : CongruenceVisibleMismatch _ _ : _ ->
      pure ()
    actual ->
      assertFailure ("unexpected carrier/visible mismatch order: " <> show actual)

  rightRepresentativeMismatch <-
    expectRight $
      mkDiscreteCongruenceStalk
        carrier0
        [key 0, key 1]

  congruenceMismatches leftStalk rightRepresentativeMismatch
    @?= [ CongruenceRepresentativeMismatch
            (key 1)
            (key 0)
            (key 1)
        ]

testMergeIdempotence :: Assertion
testMergeIdempotence = do
  carrier <- expectRight (mkGlobalCarrier (CarrierId 0) ["a" :: String, "b", "c"])
  stalkValue <-
    expectRight $
      mkCongruenceStalkFromPairs
        carrier
        [key 0, key 1]
        [(key 0, key 1)]

  mergeCongruenceStalks stalkValue stalkValue
    @?= Right (normalizeCongruenceStalk stalkValue)

testMergeCommutativity :: Assertion
testMergeCommutativity = do
  carrier <- expectRight (mkGlobalCarrier (CarrierId 0) ["a" :: String, "b", "c"])
  leftStalk <-
    expectRight $
      mkCongruenceStalkFromPairs
        carrier
        [key 0, key 1, key 2]
        [(key 0, key 1)]
  rightStalk <-
    expectRight $
      mkCongruenceStalkFromPairs
        carrier
        [key 0, key 1, key 2]
        [(key 1, key 2)]

  leftThenRight <- expectRight (mergeCongruenceStalks leftStalk rightStalk)
  rightThenLeft <- expectRight (mergeCongruenceStalks rightStalk leftStalk)

  normalizeCongruenceStalk leftThenRight
    @?= normalizeCongruenceStalk rightThenLeft

testMergeRejectsVisibleMismatch :: Assertion
testMergeRejectsVisibleMismatch = do
  carrier <- expectRight (mkGlobalCarrier (CarrierId 0) ["a" :: String, "b"])
  leftStalk <- expectRight (mkDiscreteCongruenceStalk carrier [key 0])
  rightStalk <- expectRight (mkDiscreteCongruenceStalk carrier [key 1])

  mergeCongruenceStalks leftStalk rightStalk
    @?= Left
      ( MergeMismatchObstruction
          (CongruenceVisibleMismatch (IntSet.singleton 0) (IntSet.singleton 1) :| [])
      )

testCheckedIdentityRestriction :: Assertion
testCheckedIdentityRestriction = do
  carrier <- expectRight (mkGlobalCarrier (CarrierId 0) ["a" :: String, "b", "c"])
  stalkValue <-
    expectRight $
      mkCongruenceStalkFromPairs
        carrier
        [key 0, key 1]
        [(key 0, key 1)]
  identityRestriction <-
    expectRight $
      mkCongruenceRestriction
        carrier
        [key 0, key 1]
        [key 0, key 1]
        ( IntMap.fromList
            [ (0, key 0),
              (1, key 1),
              (2, key 2)
            ]
        )

  restricted <- expectRight (restrictCongruenceStalk identityRestriction stalkValue)

  congruenceMismatches restricted stalkValue @?= []

testCheckedRestrictionComposition :: Assertion
testCheckedRestrictionComposition = do
  carrier <- expectRight (mkGlobalCarrier (CarrierId 0) ["a" :: String, "b", "c", "d"])
  stalkValue <-
    expectRight $
      mkCongruenceStalkFromPairs
        carrier
        [key 0, key 1]
        [(key 0, key 1)]
  firstRestriction <-
    expectRight $
      mkCongruenceRestriction
        carrier
        [key 0, key 1]
        [key 2]
        (IntMap.fromList [(0, key 2), (1, key 2), (2, key 2), (3, key 3)])
  secondRestriction <-
    expectRight $
      mkCongruenceRestriction
        carrier
        [key 2]
        [key 3]
        (IntMap.fromList [(0, key 0), (1, key 1), (2, key 3), (3, key 3)])
  composedRestriction <-
    expectRight $
      mkCongruenceRestriction
        carrier
        [key 0, key 1]
        [key 3]
        (IntMap.fromList [(0, key 3), (1, key 3), (2, key 3), (3, key 3)])

  firstRestricted <- expectRight (restrictCongruenceStalk firstRestriction stalkValue)
  sequential <- expectRight (restrictCongruenceStalk secondRestriction firstRestricted)
  direct <- expectRight (restrictCongruenceStalk composedRestriction stalkValue)

  congruenceMismatches sequential direct @?= []

testMergeClosesFullCarrierRelation :: Assertion
testMergeClosesFullCarrierRelation = do
  carrier <- expectRight (mkGlobalCarrier (CarrierId 0) ["a" :: String, "b", "c"])
  leftStalk <-
    expectRight $
      mkCongruenceStalkFromPairs
        carrier
        [key 0, key 1, key 2]
        [(key 0, key 1)]
  rightStalk <-
    expectRight $
      mkCongruenceStalkFromPairs
        carrier
        [key 0, key 1, key 2]
        [(key 1, key 2)]
  expected <-
    expectRight $
      mkCongruenceStalkFromPairs
        carrier
        [key 0, key 1, key 2]
        [(key 0, key 1), (key 1, key 2)]

  merged <- expectRight (mergeCongruenceStalks leftStalk rightStalk)

  normalizeCongruenceStalk merged
    @?= normalizeCongruenceStalk expected

testPreparedCompilerRejectsInvalidLocalData :: Assertion
testPreparedCompilerRejectsInvalidLocalData = do
  carrier <- sampleCarrier
  expectLeft
    (PreparedCongruenceVisibleSupportMissing "B")
    ( prepareCongruenceModelWith
        carrier
        ["A", "B"]
        (Map.singleton "A" [key 0, key 1])
        []
        (const ())
    )
  expectLeft
    ( PreparedCongruenceRestrictionInvalid
        "A"
        "B"
        (CongruenceRestrictionMapMissingCarrierKey 3)
    )
    ( prepareCongruenceModelWith
        carrier
        ["A", "B"]
        twoCellVisibleSupport
        [restrictionSpec "A" "B" (carrierMap [(0, 2), (1, 3), (2, 2)])]
        (const ())
    )
  expectLeft
    ( PreparedCongruenceRestrictionInvalid
        "A"
        "B"
        (CongruenceRestrictionImageOutsideTargetVisible 1 1)
    )
    ( prepareCongruenceModelWith
        carrier
        ["A", "B"]
        twoCellVisibleSupport
        [restrictionSpec "A" "B" (carrierMap [(0, 2), (1, 1), (2, 2), (3, 3)])]
        (const ())
    )

testPreparedRestrictionIsTotal :: Assertion
testPreparedRestrictionIsTotal =
  withPreparedModel $ \model -> do
    sourceStalk <-
      expectRight $
        mkPreparedStalkFromPairsAt
          model
          "A"
          [(key 0, key 1)]
    expectedTarget <-
      expectRight $
        mkPreparedStalkFromPairsAt
          model
          "B"
          [(key 2, key 3)]
    restriction <-
      expectRestriction "A" "B" model
    let restricted =
          restrictStalk
            preparedCongruenceStalkAlgebra
            (rWitness restriction)
            sourceStalk

    stalkMismatches preparedCongruenceStalkAlgebra restricted expectedTarget @?= []

testPreparedModelCertifiesCompatibleSection :: Assertion
testPreparedModelCertifiesCompatibleSection =
  withPreparedModel $ \model -> do
    sourceStalk <-
      expectRight $
        mkPreparedStalkFromPairsAt
          model
          "A"
          [(key 0, key 1)]
    middleStalk <-
      expectRight $
        mkPreparedStalkFromPairsAt
          model
          "B"
          [(key 2, key 3)]
    targetStalk <-
      expectRight $
        mkPreparedStalkFromPairsAt
          model
          "C"
          [(key 2, key 3)]
    sectionValue <-
      expectRight $
        mkTotalSectionStore
          (preparedCongruenceSheafModel model)
          (Map.fromList [("A", sourceStalk), ("B", middleStalk), ("C", targetStalk)])

    certifySectionCompatibility
      (preparedCongruenceSheafModel model)
      preparedCongruenceStalkAlgebra
      sectionValue
      @?= Right SectionCertified

testPreparedRestrictionLaws :: Assertion
testPreparedRestrictionLaws = do
  withPreparedModel $ \model -> do
    sourceStalk <-
      expectRight $
        mkPreparedStalkFromPairsAt
          model
          "A"
          [(key 0, key 1)]
    let restrictions =
          sheafModelRestrictions (preparedCongruenceSheafModel model)

    checkRestrictionIdentityLaw
      preparedCongruenceStalkAlgebra
      restrictions
      "A"
      sourceStalk
      @?= Nothing
    checkRestrictionCompositionLaw
      preparedCongruenceStalkAlgebra
      restrictions
      (RestrictionArrow "A" "B")
      (RestrictionArrow "B" "C")
      sourceStalk
      @?= Nothing

  withPreparedModelFrom sampleBadCompositeRestrictions $ \model -> do
    sourceStalk <-
      expectRight $
        mkPreparedStalkFromPairsAt
          model
          "A"
          [(key 0, key 1)]
    case
      checkRestrictionCompositionLaw
        preparedCongruenceStalkAlgebra
        (sheafModelRestrictions (preparedCongruenceSheafModel model))
        (RestrictionArrow "A" "B")
        (RestrictionArrow "B" "C")
        sourceStalk
      of
      Just (CompositionLawMismatch firstArrow secondArrow compositeArrow mismatches) -> do
        firstArrow @?= RestrictionArrow "A" "B"
        secondArrow @?= RestrictionArrow "B" "C"
        compositeArrow @?= RestrictionArrow "A" "C"
        mismatches @?= [CongruenceRepresentativeMismatch (key 3) (key 2) (key 3)]
      actual ->
        assertFailure ("expected typed composition mismatch, got " <> show actual)

sampleCarrier :: IO (GlobalCarrier (CarrierKey String) String)
sampleCarrier =
  expectRight (mkGlobalCarrier (CarrierId 0) ["a" :: String, "b", "c", "d"])

sampleCells :: [String]
sampleCells =
  ["A", "B", "C"]

twoCellVisibleSupport :: Map.Map String [CarrierKey String]
twoCellVisibleSupport =
  Map.fromList
    [ ("A", [key 0, key 1]),
      ("B", [key 2, key 3])
    ]

sampleVisibleSupport :: Map.Map String [CarrierKey String]
sampleVisibleSupport =
  Map.fromList
    [ ("A", [key 0, key 1]),
      ("B", [key 2, key 3]),
      ("C", [key 2, key 3])
    ]

sampleRestrictions :: [PreparedCongruenceRestrictionSpec String (CarrierKey String)]
sampleRestrictions =
  identityRestrictionSpecs
    <> [ restrictionSpec "A" "B" composedCarrierMap,
         restrictionSpec "B" "C" identityCarrierMap,
         restrictionSpec "A" "C" composedCarrierMap
       ]

sampleBadCompositeRestrictions :: [PreparedCongruenceRestrictionSpec String (CarrierKey String)]
sampleBadCompositeRestrictions =
  identityRestrictionSpecs
    <> [ restrictionSpec "A" "B" composedCarrierMap,
         restrictionSpec "B" "C" identityCarrierMap,
         restrictionSpec "A" "C" badCompositeCarrierMap
       ]

identityRestrictionSpecs :: [PreparedCongruenceRestrictionSpec String (CarrierKey String)]
identityRestrictionSpecs =
  fmap
    (\cell -> restrictionSpec cell cell identityCarrierMap)
    sampleCells

restrictionSpec ::
  String ->
  String ->
  IntMap.IntMap (CarrierKey String) ->
  PreparedCongruenceRestrictionSpec String (CarrierKey String)
restrictionSpec sourceCell targetCell mapValue =
  PreparedCongruenceRestrictionSpec
    { pcrsKind = PortalRestriction,
      pcrsSource = sourceCell,
      pcrsTarget = targetCell,
      pcrsCarrierMap = mapValue
    }

identityCarrierMap :: IntMap.IntMap (CarrierKey String)
identityCarrierMap =
  carrierMap [(0, 0), (1, 1), (2, 2), (3, 3)]

composedCarrierMap :: IntMap.IntMap (CarrierKey String)
composedCarrierMap =
  carrierMap [(0, 2), (1, 3), (2, 2), (3, 3)]

badCompositeCarrierMap :: IntMap.IntMap (CarrierKey String)
badCompositeCarrierMap =
  carrierMap [(0, 2), (1, 2), (2, 2), (3, 3)]

carrierMap :: [(Int, Int)] -> IntMap.IntMap (CarrierKey String)
carrierMap entries =
  IntMap.fromList [(sourceKey, key targetKey) | (sourceKey, targetKey) <- entries]

withPreparedModel ::
  ( forall carrier.
    PreparedCongruenceModel carrier String (CarrierKey String) String ->
    Assertion
  ) ->
  Assertion
withPreparedModel =
  withPreparedModelFrom sampleRestrictions

withPreparedModelFrom ::
  [PreparedCongruenceRestrictionSpec String (CarrierKey String)] ->
  ( forall carrier.
    PreparedCongruenceModel carrier String (CarrierKey String) String ->
    Assertion
  ) ->
  Assertion
withPreparedModelFrom restrictions continue = do
  carrier <- sampleCarrier
  case
    prepareCongruenceModelWith
      carrier
      sampleCells
      sampleVisibleSupport
      restrictions
      continue
    of
    Left failureValue ->
      assertFailure ("expected prepared congruence model, got " <> show failureValue)
    Right assertion ->
      assertion

expectRestriction ::
  Show rep =>
  String ->
  String ->
  PreparedCongruenceModel carrier String rep atom ->
  IO (Restriction String (PreparedCongruenceRestriction carrier rep atom))
expectRestriction sourceCell targetCell model =
  case matchingRestrictions of
    [restriction] ->
      pure restriction
    actual ->
      assertFailure ("expected one restriction, got " <> show actual)
  where
    matchingRestrictions =
      [ restriction
        | restriction <-
            restrictionEntries
              (sheafModelRestrictions (preparedCongruenceSheafModel model)),
          rSource restriction == sourceCell,
          rTarget restriction == targetCell
      ]

mkPreparedStalkFromPairsAt ::
  PreparedCongruenceModel carrier String (CarrierKey String) String ->
  String ->
  [(CarrierKey String, CarrierKey String)] ->
  Either
    (PreparedCongruenceBuildError String String)
    (PreparedCongruenceStalk carrier (CarrierKey String) String)
mkPreparedStalkFromPairsAt model cell pairs = do
  relationValue <-
    case equivalenceFromPairs sampleCarrierDomain pairs of
      Left failureValue ->
        Left (PreparedCongruenceStalkRelationInvalid cell failureValue)
      Right relationValue ->
        Right relationValue
  mkPreparedCongruenceStalkFromRelationAt model cell relationValue

sampleCarrierDomain :: IntSet.IntSet
sampleCarrierDomain =
  IntSet.fromList [0, 1, 2, 3]

key :: Int -> CarrierKey atom
key =
  decodeDenseKey

expectLeft :: (HasCallStack, Eq errorValue, Show errorValue) => errorValue -> Either errorValue value -> Assertion
expectLeft expected result =
  case result of
    Left actual ->
      actual @?= expected
    Right _ ->
      assertFailure "expected Left, got Right"

expectRight :: (HasCallStack, Show errorValue) => Either errorValue value -> IO value
expectRight result =
  case result of
    Right value ->
      pure value
    Left errorValue ->
      assertFailure ("expected Right, got Left " <> show errorValue)
