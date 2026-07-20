{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Presheaf.StalkColimitSpec
  ( tests,
  )
where

import Data.Map.Strict qualified as Map
import Moonlight.Sheaf.Presheaf.Finite
  ( FinitePresheaf (..),
    FinitePresheafFailure,
    mkFinitePresheaf,
  )
import Moonlight.Sheaf.Presheaf.Stalk.Colimit
  ( ColimitFactorFailure (..),
    ColimitStalkFailure (..),
    ColimitStalkRepresentative (..),
    FiniteColimitStalk,
    NeighborhoodFilter (..),
    NeighborhoodFilterFailure (..),
    colimitStalkEquivalent,
    colimitStalkRepresentatives,
    factorFiniteColimitStalk,
    finiteColimitStalkAt,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    PullbackSquare (..),
    Site (..),
  )
import Moonlight.Sheaf.TestFixture.Assertions (expectRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "finite presheaf stalk colimits"
    [ testCase "identity-only neighborhood has original fiber germs" testIdentityOnlyNeighborhood,
      testCase "restriction-generated equivalence collapses equal germs" testRestrictionGeneratedCollapse,
      testCase "unconnected representatives remain distinct germs" testDistinctGermsRemainDistinct,
      testCase "empty point neighborhood is a typed obstruction" testEmptyNeighborhood,
      testCase "nondirected point neighborhood is a typed obstruction" testNondirectedNeighborhood,
      testCase "restriction failure during quotient generation is typed" testRestrictionFailure,
      testCase "factorization accepts maps constant on germ classes" testFactorAcceptsCompatibleMap,
      testCase "factorization rejects maps that split a germ class" testFactorRejectsIncompatibleMap
    ]

testIdentityOnlyNeighborhood :: Assertion
testIdentityOnlyNeighborhood = do
  presheaf <- expectRight (mkChainPresheaf [RootSection 0, RootSection 1] [LeafSection 0, LeafSection 1])
  stalkValue <- expectRight (finiteColimitStalkAt rootOnlyFilter presheaf)
  fmap (colimitRepValue . snd) (colimitStalkRepresentatives stalkValue)
    @?= [RootSection 0, RootSection 1]
  assertBool
    "distinct root sections must remain distinct under identity-only neighborhoods"
    (not (colimitStalkEquivalent stalkValue (rootRepresentative 0) (rootRepresentative 1)))

testRestrictionGeneratedCollapse :: Assertion
testRestrictionGeneratedCollapse = do
  stalkValue <- sampleChainStalk
  assertBool
    "root sections with equal restriction to a smaller neighborhood must be the same germ"
    (colimitStalkEquivalent stalkValue (rootRepresentative 0) (rootRepresentative 2))

testDistinctGermsRemainDistinct :: Assertion
testDistinctGermsRemainDistinct = do
  stalkValue <- sampleChainStalk
  assertBool
    "root sections with different restriction shadows must remain distinct germs"
    (not (colimitStalkEquivalent stalkValue (rootRepresentative 0) (rootRepresentative 1)))

testEmptyNeighborhood :: Assertion
testEmptyNeighborhood = do
  presheaf <- expectRight (mkChainPresheaf [RootSection 0] [LeafSection 0])
  case finiteColimitStalkAt emptyFilter presheaf of
    Left (ColimitNeighborhoodInvalid NeighborhoodFilterEmpty) ->
      pure ()
    Left otherFailure ->
      assertFailure ("expected empty-neighborhood obstruction, received " <> show otherFailure)
    Right _ ->
      assertFailure "expected empty-neighborhood obstruction, received stalk"

testNondirectedNeighborhood :: Assertion
testNondirectedNeighborhood = do
  presheaf <- expectRight (mkForkPresheaf [RootSection 0] [LeftSection 0] [RightSection 0])
  case finiteColimitStalkAt forkAllFilter presheaf of
    Left (ColimitNeighborhoodInvalid (NeighborhoodFilterNotDirected LeftLeaf RightLeaf)) ->
      pure ()
    Left otherFailure ->
      assertFailure ("expected nondirected-neighborhood obstruction, received " <> show otherFailure)
    Right _ ->
      assertFailure "expected nondirected-neighborhood obstruction, received stalk"

testRestrictionFailure :: Assertion
testRestrictionFailure = do
  presheaf <- expectRight (mkChainPresheaf [RootSection 0] [LeafSection 0])
  let brokenPresheaf =
        presheaf
          { fpRestrict =
              \morphismValue sectionValue ->
                if morphismValue == leafToRoot
                  then Left (SampleRestrictionFailure LeafToRoot sectionValue)
                  else fpRestrict presheaf morphismValue sectionValue
          }
  case finiteColimitStalkAt chainAllFilter brokenPresheaf of
    Left (ColimitRestrictionFailed morphismValue (RootSection 0) (SampleRestrictionFailure LeafToRoot (RootSection 0))) ->
      morphismValue @?= leafToRoot
    Left otherFailure ->
      assertFailure ("expected restriction failure, received " <> show otherFailure)
    Right _ ->
      assertFailure "expected restriction failure, received stalk"

testFactorAcceptsCompatibleMap :: Assertion
testFactorAcceptsCompatibleMap = do
  stalkValue <- sampleChainStalk
  factorValue <- expectRight (factorFiniteColimitStalk stalkValue representativeParity)
  Map.elems factorValue @?= [0, 1]

testFactorRejectsIncompatibleMap :: Assertion
testFactorRejectsIncompatibleMap = do
  stalkValue <- sampleChainStalk
  case factorFiniteColimitStalk stalkValue representativeObjectCode of
    Left ColimitFactorIncompatible {} ->
      pure ()
    Left otherFailure ->
      assertFailure ("expected incompatible factorization, received " <> show otherFailure)
    Right factorValue ->
      assertFailure ("expected incompatible factorization, received " <> show factorValue)

sampleChainStalk :: AssertionResult
sampleChainStalk = do
  presheaf <- expectRight (mkChainPresheaf [RootSection 0, RootSection 1, RootSection 2] [LeafSection 0, LeafSection 1])
  expectRight (finiteColimitStalkAt chainAllFilter presheaf)

type AssertionResult = IO (FiniteColimitStalk () SampleObject SampleSection)

rootRepresentative :: Int -> ColimitStalkRepresentative SampleObject SampleSection
rootRepresentative value =
  ColimitStalkRepresentative
    { colimitRepObject = Root,
      colimitRepValue = RootSection value
    }

representativeParity :: ColimitStalkRepresentative SampleObject SampleSection -> Int
representativeParity representativeValue =
  case colimitRepValue representativeValue of
    LeafSection value -> value
    RootSection value -> value `mod` 2
    LeftSection value -> value
    RightSection value -> value

representativeObjectCode :: ColimitStalkRepresentative SampleObject SampleSection -> Int
representativeObjectCode representativeValue =
  case colimitRepObject representativeValue of
    Leaf -> 0
    Root -> 1
    LeftLeaf -> 0
    RightLeaf -> 0

rootOnlyFilter :: NeighborhoodFilter () SampleObject
rootOnlyFilter =
  NeighborhoodFilter
    { neighborhoodPoint = (),
      neighborhoodContains = \() objectValue -> objectValue == Root
    }

chainAllFilter :: NeighborhoodFilter () SampleObject
chainAllFilter =
  NeighborhoodFilter
    { neighborhoodPoint = (),
      neighborhoodContains = \() objectValue -> objectValue == Leaf || objectValue == Root
    }

emptyFilter :: NeighborhoodFilter () SampleObject
emptyFilter =
  NeighborhoodFilter
    { neighborhoodPoint = (),
      neighborhoodContains = \() _objectValue -> False
    }

forkAllFilter :: NeighborhoodFilter () SampleObject
forkAllFilter =
  NeighborhoodFilter
    { neighborhoodPoint = (),
      neighborhoodContains = \() objectValue -> objectValue == LeftLeaf || objectValue == RightLeaf || objectValue == Root
    }

data SampleSite
  = ChainSite
  | ForkSite
  deriving stock (Eq, Ord, Show)

data SampleObject
  = Leaf
  | LeftLeaf
  | RightLeaf
  | Root
  deriving stock (Eq, Ord, Show)

data SampleMorphism
  = SampleIdentity !SampleObject
  | LeafToRoot
  | LeftToRoot
  | RightToRoot
  deriving stock (Eq, Ord, Show)

data SampleSection
  = LeafSection !Int
  | LeftSection !Int
  | RightSection !Int
  | RootSection !Int
  deriving stock (Eq, Ord, Show)

data SampleMismatch = SampleMismatch !SampleObject !SampleSection !SampleSection
  deriving stock (Eq, Show)

data SampleRestrictionFailure = SampleRestrictionFailure !SampleMorphism !SampleSection
  deriving stock (Eq, Show)

instance Site SampleSite where
  type SiteObject SampleSite = SampleObject
  type SiteMorphism SampleSite = SampleMorphism

  siteObjects siteValue =
    case siteValue of
      ChainSite -> [Leaf, Root]
      ForkSite -> [LeftLeaf, RightLeaf, Root]

  siteMorphisms siteValue =
    fmap identityMorphism (siteObjects siteValue)
      <> case siteValue of
        ChainSite -> [leafToRoot]
        ForkSite -> [leftToRoot, rightToRoot]

  identityAt _siteValue =
    identityMorphism

  coversAt _siteValue _objectValue =
    []

  composeChecked _siteValue outerMorphism innerMorphism
    | cmSource outerMorphism /= cmTarget innerMorphism =
        Nothing
    | isIdentityMorphism outerMorphism =
        Just innerMorphism
    | isIdentityMorphism innerMorphism =
        Just outerMorphism
    | otherwise =
        Nothing

  pullbackPair siteValue leftMorphism rightMorphism
    | cmTarget leftMorphism /= cmTarget rightMorphism =
        Nothing
    | otherwise = do
        apex <- sampleMeet siteValue (cmSource leftMorphism) (cmSource rightMorphism)
        leftLeg <- sampleMorphism siteValue apex (cmSource leftMorphism)
        rightLeg <- sampleMorphism siteValue apex (cmSource rightMorphism)
        pure
          PullbackSquare
            { psLeftBase = leftMorphism,
              psRightBase = rightMorphism,
              psApex = apex,
              psToLeft = leftLeg,
              psToRight = rightLeg
            }

mkChainPresheaf ::
  [SampleSection] ->
  [SampleSection] ->
  Either
    (FinitePresheafFailure SampleObject SampleMorphism SampleSection SampleMismatch SampleRestrictionFailure)
    (FinitePresheaf SampleSite SampleSection SampleMismatch SampleRestrictionFailure)
mkChainPresheaf rootValues leafValues =
  mkFinitePresheaf
    ChainSite
    sampleRestrict
    sampleMismatches
    (\_objectValue sectionValue -> sectionValue)
    ( Map.fromList
        [ (Leaf, leafValues),
          (Root, rootValues)
        ]
    )

mkForkPresheaf ::
  [SampleSection] ->
  [SampleSection] ->
  [SampleSection] ->
  Either
    (FinitePresheafFailure SampleObject SampleMorphism SampleSection SampleMismatch SampleRestrictionFailure)
    (FinitePresheaf SampleSite SampleSection SampleMismatch SampleRestrictionFailure)
mkForkPresheaf rootValues leftValues rightValues =
  mkFinitePresheaf
    ForkSite
    sampleRestrict
    sampleMismatches
    (\_objectValue sectionValue -> sectionValue)
    ( Map.fromList
        [ (LeftLeaf, leftValues),
          (RightLeaf, rightValues),
          (Root, rootValues)
        ]
    )

sampleRestrict ::
  CheckedMorphism SampleObject SampleMorphism ->
  SampleSection ->
  Either SampleRestrictionFailure SampleSection
sampleRestrict morphismValue sectionValue =
  case (cmWitness morphismValue, sectionValue) of
    (SampleIdentity objectValue, _)
      | sectionObject sectionValue == objectValue -> Right sectionValue
    (LeafToRoot, RootSection value) ->
      Right (LeafSection (value `mod` 2))
    (LeftToRoot, RootSection value) ->
      Right (LeftSection value)
    (RightToRoot, RootSection value) ->
      Right (RightSection value)
    _ ->
      Left (SampleRestrictionFailure (cmWitness morphismValue) sectionValue)

sampleMismatches :: SampleObject -> SampleSection -> SampleSection -> [SampleMismatch]
sampleMismatches objectValue leftValue rightValue =
  [SampleMismatch objectValue leftValue rightValue | leftValue /= rightValue]

sectionObject :: SampleSection -> SampleObject
sectionObject sectionValue =
  case sectionValue of
    LeafSection _ -> Leaf
    LeftSection _ -> LeftLeaf
    RightSection _ -> RightLeaf
    RootSection _ -> Root

identityMorphism :: SampleObject -> CheckedMorphism SampleObject SampleMorphism
identityMorphism objectValue =
  CheckedMorphism
    { cmSource = objectValue,
      cmTarget = objectValue,
      cmWitness = SampleIdentity objectValue
    }

leafToRoot :: CheckedMorphism SampleObject SampleMorphism
leafToRoot =
  CheckedMorphism
    { cmSource = Leaf,
      cmTarget = Root,
      cmWitness = LeafToRoot
    }

leftToRoot :: CheckedMorphism SampleObject SampleMorphism
leftToRoot =
  CheckedMorphism
    { cmSource = LeftLeaf,
      cmTarget = Root,
      cmWitness = LeftToRoot
    }

rightToRoot :: CheckedMorphism SampleObject SampleMorphism
rightToRoot =
  CheckedMorphism
    { cmSource = RightLeaf,
      cmTarget = Root,
      cmWitness = RightToRoot
    }

isIdentityMorphism :: CheckedMorphism SampleObject SampleMorphism -> Bool
isIdentityMorphism morphismValue =
  case cmWitness morphismValue of
    SampleIdentity _ -> True
    LeafToRoot -> False
    LeftToRoot -> False
    RightToRoot -> False

sampleMorphism :: SampleSite -> SampleObject -> SampleObject -> Maybe (CheckedMorphism SampleObject SampleMorphism)
sampleMorphism siteValue sourceObject targetObject
  | sourceObject == targetObject =
      Just (identityMorphism sourceObject)
  | otherwise =
      case (siteValue, sourceObject, targetObject) of
        (ChainSite, Leaf, Root) -> Just leafToRoot
        (ForkSite, LeftLeaf, Root) -> Just leftToRoot
        (ForkSite, RightLeaf, Root) -> Just rightToRoot
        _ -> Nothing

sampleMeet :: SampleSite -> SampleObject -> SampleObject -> Maybe SampleObject
sampleMeet siteValue leftObject rightObject =
  case (siteValue, leftObject, rightObject) of
    (_, _, _) | leftObject == rightObject -> Just leftObject
    (ChainSite, Leaf, Root) -> Just Leaf
    (ChainSite, Root, Leaf) -> Just Leaf
    (ForkSite, LeftLeaf, Root) -> Just LeftLeaf
    (ForkSite, Root, LeftLeaf) -> Just LeftLeaf
    (ForkSite, RightLeaf, Root) -> Just RightLeaf
    (ForkSite, Root, RightLeaf) -> Just RightLeaf
    _ -> Nothing
