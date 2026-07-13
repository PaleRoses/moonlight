{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Presheaf.SiteMapSpec
  ( tests,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Moonlight.Sheaf.Image.Restrict
  ( pullbackFinitePresheaf,
  )
import Moonlight.Sheaf.Presheaf.Finite
  ( FinitePresheaf (..),
    FinitePresheafFailure,
    finiteFiberAt,
    finiteFiberValues,
    mkFinitePresheaf,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    CoverConstructionError,
    CoveringFamily,
    PullbackSquare (..),
    Site (..),
    coverTarget,
    mkCoveringFamily,
  )
import Moonlight.Sheaf.Site.CoverBasis.Finite
  ( mkFiniteCoverBasis,
  )
import Moonlight.Sheaf.Site.Plan
  ( effectiveCoverFamily,
  )
import Moonlight.Sheaf.Site.Construction.Map
  ( FiniteSiteMap,
    SiteMapFailure (..),
    coverImageAt,
    mkContinuousSiteMap,
    mkFiniteSiteMap,
    siteMapObjectImage,
  )
import Moonlight.Sheaf.TestFixture.Assertions (expectRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "finite site maps"
    [ testCase "validated finite site map preserves semantic images" testValidSiteMap,
      testCase "cover continuity maps source covers to canonical target plans" testCoverContinuity,
      testCase "site map rejects unknown target object images" testUnknownTargetObject,
      testCase "site map rejects unknown target morphism images" testUnknownTargetMorphism,
      testCase "site map rejects endpoint mismatches" testEndpointMismatch,
      testCase "site map rejects noncanonical identity images" testIdentityMismatch,
      testCase "site map rejects composition mismatches" testCompositionMismatch,
      testCase "finite presheaf pullback constructs an ordinary validated presheaf" testPullbackFinitePresheaf
    ]

testValidSiteMap :: Assertion
testValidSiteMap = do
  siteMapValue <- expectRight (identitySiteMap goodSite goodSite)
  siteMapObjectImage Leaf siteMapValue @?= Just Leaf
  siteMapObjectImage Root siteMapValue @?= Just Root

testCoverContinuity :: Assertion
testCoverContinuity = do
  siteMapValue <- expectRight (identitySiteMap goodSite goodSite)
  sourceBasis <- expectRight (mkFiniteCoverBasis goodSite)
  targetBasis <- expectRight (mkFiniteCoverBasis goodSite)
  continuous <- expectRight (mkContinuousSiteMap sourceBasis targetBasis siteMapValue)
  sourceCover <- expectRight leafCover
  case coverImageAt sourceCover continuous of
    Nothing ->
      assertFailure "expected source leaf cover to have a canonical target image"
    Just targetPlan ->
      coverTarget sourceCover @?= coverTarget (effectiveCoverFamily targetPlan)

testUnknownTargetObject :: Assertion
testUnknownTargetObject =
  case mkFiniteSiteMap goodSite goodSite objectImagesWithGhost identityMorphismImages of
    Left (SiteMapObjectImageUnknownTarget Leaf Ghost) ->
      pure ()
    other ->
      unexpectedSiteMapResult "expected unknown target object failure" other

testUnknownTargetMorphism :: Assertion
testUnknownTargetMorphism =
  case mkFiniteSiteMap goodSite goodSite identityObjectImages ghostMorphismImages of
    Left (SiteMapMorphismImageUnknownTarget sourceMorphism targetMorphism) -> do
      sourceMorphism @?= leafToRoot
      targetMorphism @?= ghostLeafToRoot
    other ->
      unexpectedSiteMapResult "expected unknown target morphism failure" other

testEndpointMismatch :: Assertion
testEndpointMismatch =
  case mkFiniteSiteMap goodSite goodSite identityObjectImages endpointMismatchImages of
    Left (SiteMapMorphismEndpointMismatch sourceMorphism targetMorphism Leaf Root) -> do
      sourceMorphism @?= leafToRoot
      targetMorphism @?= identityMorphism Root
    other ->
      unexpectedSiteMapResult "expected endpoint mismatch failure" other

testIdentityMismatch :: Assertion
testIdentityMismatch =
  case mkFiniteSiteMap goodSite identityEndomorphismSite identityObjectImages identityMismatchMorphismImages of
    Left (SiteMapIdentityMismatch Leaf expected actual) -> do
      expected @?= identityMorphism Leaf
      actual @?= leafEndomorphism
    other ->
      unexpectedSiteMapResult "expected identity mismatch failure" other

testCompositionMismatch :: Assertion
testCompositionMismatch =
  case mkFiniteSiteMap compositionSourceSite compositionTargetSite compositionObjectImages compositionMismatchMorphismImages of
    Left (SiteMapCompositionMismatch outerMorphism innerMorphism sourceComposite targetComposite imageComposite) -> do
      outerMorphism @?= middleToTarget
      innerMorphism @?= sourceToMiddle
      sourceComposite @?= firstSourceToTarget
      targetComposite @?= firstSourceToTarget
      imageComposite @?= secondSourceToTarget
    other ->
      unexpectedSiteMapResult "expected composition mismatch failure" other


unexpectedSiteMapResult ::
  Show failure =>
  String ->
  Either failure value ->
  Assertion
unexpectedSiteMapResult label result =
  case result of
    Left failure ->
      assertFailure (label <> ", received " <> show failure)
    Right _ ->
      assertFailure (label <> ", received successful site map")

testPullbackFinitePresheaf :: Assertion
testPullbackFinitePresheaf = do
  siteMapValue <- expectRight (identitySiteMap goodSite goodSite)
  presheaf <- expectRight (mkSimplePresheaf goodSite [RootSection 0] [LeafSection 0])
  pulled <- expectRight (pullbackFinitePresheaf siteMapValue presheaf)
  expectFiberValuesAt Leaf pulled >>= (@?= [LeafSection 0])
  expectFiberValuesAt Root pulled >>= (@?= [RootSection 0])
  restricted <- expectRight (fpRestrict pulled leafToRoot (RootSection 0))
  restricted @?= LeafSection 0

expectFiberValuesAt ::
  SimpleObject ->
  FinitePresheaf SimpleSite value mismatch restrictionFailure ->
  IO [value]
expectFiberValuesAt objectValue presheaf =
  maybe
    (assertFailure ("expected finite fiber at " <> show objectValue))
    (pure . finiteFiberValues)
    (finiteFiberAt objectValue presheaf)

identitySiteMap ::
  SimpleSite ->
  SimpleSite ->
  Either
    (SiteMapFailure SimpleObject SimpleMorphism SimpleObject SimpleMorphism)
    (FiniteSiteMap SimpleSite SimpleSite)
identitySiteMap sourceSite targetSite =
  mkFiniteSiteMap sourceSite targetSite identityObjectImages identityMorphismImages

identityObjectImages :: Map.Map SimpleObject SimpleObject
identityObjectImages =
  Map.fromList [(Leaf, Leaf), (Root, Root)]

objectImagesWithGhost :: Map.Map SimpleObject SimpleObject
objectImagesWithGhost =
  Map.insert Leaf Ghost identityObjectImages

identityMorphismImages ::
  Map.Map
    (CheckedMorphism SimpleObject SimpleMorphism)
    (CheckedMorphism SimpleObject SimpleMorphism)
identityMorphismImages =
  Map.fromList
    [ (identityMorphism Leaf, identityMorphism Leaf),
      (identityMorphism Root, identityMorphism Root),
      (leafToRoot, leafToRoot)
    ]

ghostMorphismImages ::
  Map.Map
    (CheckedMorphism SimpleObject SimpleMorphism)
    (CheckedMorphism SimpleObject SimpleMorphism)
ghostMorphismImages =
  Map.insert leafToRoot ghostLeafToRoot identityMorphismImages

endpointMismatchImages ::
  Map.Map
    (CheckedMorphism SimpleObject SimpleMorphism)
    (CheckedMorphism SimpleObject SimpleMorphism)
endpointMismatchImages =
  Map.insert leafToRoot (identityMorphism Root) identityMorphismImages

identityMismatchMorphismImages ::
  Map.Map
    (CheckedMorphism SimpleObject SimpleMorphism)
    (CheckedMorphism SimpleObject SimpleMorphism)
identityMismatchMorphismImages =
  Map.insert (identityMorphism Leaf) leafEndomorphism identityMorphismImages

compositionObjectImages :: Map.Map SimpleObject SimpleObject
compositionObjectImages =
  Map.fromList
    [ (CompositionSource, CompositionSource),
      (CompositionMiddle, CompositionMiddle),
      (CompositionTarget, CompositionTarget)
    ]

compositionMismatchMorphismImages ::
  Map.Map
    (CheckedMorphism SimpleObject SimpleMorphism)
    (CheckedMorphism SimpleObject SimpleMorphism)
compositionMismatchMorphismImages =
  Map.fromList
    [ (identityMorphism CompositionSource, identityMorphism CompositionSource),
      (identityMorphism CompositionMiddle, identityMorphism CompositionMiddle),
      (identityMorphism CompositionTarget, identityMorphism CompositionTarget),
      (sourceToMiddle, sourceToMiddle),
      (middleToTarget, middleToTarget),
      (firstSourceToTarget, secondSourceToTarget)
    ]

mkSimplePresheaf ::
  SimpleSite ->
  [SimpleSection] ->
  [SimpleSection] ->
  Either
    (FinitePresheafFailure SimpleObject SimpleMorphism SimpleSection SimpleMismatch SimpleRestrictionFailure)
    (FinitePresheaf SimpleSite SimpleSection SimpleMismatch SimpleRestrictionFailure)
mkSimplePresheaf siteValue rootValues leafValues =
  mkFinitePresheaf
    siteValue
    simpleRestrict
    simpleMismatches
    (\_objectValue sectionValue -> sectionValue)
    ( Map.fromList
        [ (Leaf, leafValues),
          (Root, rootValues)
        ]
    )

data SimpleSite
  = GoodSite
  | IdentityEndomorphismSite
  | CompositionSourceSite
  | CompositionTargetSite
  deriving stock (Eq, Ord, Show)

goodSite :: SimpleSite
goodSite =
  GoodSite

identityEndomorphismSite :: SimpleSite
identityEndomorphismSite =
  IdentityEndomorphismSite

compositionSourceSite :: SimpleSite
compositionSourceSite =
  CompositionSourceSite

compositionTargetSite :: SimpleSite
compositionTargetSite =
  CompositionTargetSite

data SimpleObject
  = Leaf
  | Root
  | CompositionSource
  | CompositionMiddle
  | CompositionTarget
  | Ghost
  deriving stock (Eq, Ord, Show)

data SimpleMorphism
  = SimpleIdentity SimpleObject
  | LeafEndomorphism
  | LeafToRoot
  | SourceToMiddle
  | MiddleToTarget
  | FirstSourceToTarget
  | SecondSourceToTarget
  | GhostLeafToRoot
  deriving stock (Eq, Ord, Show)

data SimpleSection
  = LeafSection Int
  | RootSection Int
  deriving stock (Eq, Ord, Show)

data SimpleMismatch = SimpleMismatch !SimpleObject !SimpleSection !SimpleSection
  deriving stock (Eq, Show)

data SimpleRestrictionFailure = SimpleRestrictionFailure !SimpleMorphism !SimpleSection
  deriving stock (Eq, Show)

instance Site SimpleSite where
  type SiteObject SimpleSite = SimpleObject
  type SiteMorphism SimpleSite = SimpleMorphism

  siteObjects siteValue =
    case siteValue of
      GoodSite ->
        [Leaf, Root]
      IdentityEndomorphismSite ->
        [Leaf, Root]
      CompositionSourceSite ->
        compositionObjects
      CompositionTargetSite ->
        compositionObjects

  siteMorphisms siteValue =
    case siteValue of
      GoodSite ->
        simpleMorphisms
      IdentityEndomorphismSite ->
        simpleMorphisms <> [leafEndomorphism]
      CompositionSourceSite ->
        compositionMorphisms
      CompositionTargetSite ->
        compositionMorphisms <> [secondSourceToTarget]

  identityAt _ =
    identityMorphism

  coversAt siteValue objectValue =
    [coverValue | siteValue == GoodSite, objectValue == Root, Right coverValue <- [leafCover]]

  composeChecked _site outerMorphism innerMorphism
    | cmSource outerMorphism /= cmTarget innerMorphism =
        Nothing
    | isCanonicalIdentity outerMorphism =
        Just innerMorphism
    | isCanonicalIdentity innerMorphism =
        Just outerMorphism
    | otherwise =
        composeNonIdentityMorphisms outerMorphism innerMorphism

  pullbackPair siteValue =
    simplePullbackPair siteValue

simpleRestrict ::
  CheckedMorphism SimpleObject SimpleMorphism ->
  SimpleSection ->
  Either SimpleRestrictionFailure SimpleSection
simpleRestrict morphismValue sectionValue =
  case (cmSource morphismValue, cmTarget morphismValue, sectionValue) of
    (Leaf, Leaf, LeafSection _) ->
      Right sectionValue
    (Root, Root, RootSection _) ->
      Right sectionValue
    (Leaf, Root, RootSection _) ->
      Right (LeafSection 0)
    _ ->
      Left (SimpleRestrictionFailure (cmWitness morphismValue) sectionValue)

simpleMismatches ::
  SimpleObject ->
  SimpleSection ->
  SimpleSection ->
  [SimpleMismatch]
simpleMismatches objectValue leftValue rightValue =
  [SimpleMismatch objectValue leftValue rightValue | leftValue /= rightValue]

identityMorphism :: SimpleObject -> CheckedMorphism SimpleObject SimpleMorphism
identityMorphism objectValue =
  CheckedMorphism
    { cmSource = objectValue,
      cmTarget = objectValue,
      cmWitness = SimpleIdentity objectValue
    }

leafEndomorphism :: CheckedMorphism SimpleObject SimpleMorphism
leafEndomorphism =
  CheckedMorphism
    { cmSource = Leaf,
      cmTarget = Leaf,
      cmWitness = LeafEndomorphism
    }

leafToRoot :: CheckedMorphism SimpleObject SimpleMorphism
leafToRoot =
  CheckedMorphism
    { cmSource = Leaf,
      cmTarget = Root,
      cmWitness = LeafToRoot
    }

ghostLeafToRoot :: CheckedMorphism SimpleObject SimpleMorphism
ghostLeafToRoot =
  CheckedMorphism
    { cmSource = Leaf,
      cmTarget = Root,
      cmWitness = GhostLeafToRoot
    }

compositionObjects :: [SimpleObject]
compositionObjects =
  [CompositionSource, CompositionMiddle, CompositionTarget]

simpleMorphisms :: [CheckedMorphism SimpleObject SimpleMorphism]
simpleMorphisms =
  [identityMorphism Leaf, identityMorphism Root, leafToRoot]

compositionMorphisms :: [CheckedMorphism SimpleObject SimpleMorphism]
compositionMorphisms =
  fmap identityMorphism compositionObjects
    <> [sourceToMiddle, middleToTarget, firstSourceToTarget]

sourceToMiddle :: CheckedMorphism SimpleObject SimpleMorphism
sourceToMiddle =
  CheckedMorphism CompositionSource CompositionMiddle SourceToMiddle

middleToTarget :: CheckedMorphism SimpleObject SimpleMorphism
middleToTarget =
  CheckedMorphism CompositionMiddle CompositionTarget MiddleToTarget

firstSourceToTarget :: CheckedMorphism SimpleObject SimpleMorphism
firstSourceToTarget =
  CheckedMorphism CompositionSource CompositionTarget FirstSourceToTarget

secondSourceToTarget :: CheckedMorphism SimpleObject SimpleMorphism
secondSourceToTarget =
  CheckedMorphism CompositionSource CompositionTarget SecondSourceToTarget

leafCover :: Either (CoverConstructionError SimpleObject) (CoveringFamily SimpleObject SimpleMorphism)
leafCover =
  mkCoveringFamily Root (leafToRoot :| [])

isCanonicalIdentity :: CheckedMorphism SimpleObject SimpleMorphism -> Bool
isCanonicalIdentity morphismValue =
  case cmWitness morphismValue of
    SimpleIdentity _ ->
      True
    LeafEndomorphism ->
      False
    LeafToRoot ->
      False
    SourceToMiddle ->
      False
    MiddleToTarget ->
      False
    FirstSourceToTarget ->
      False
    SecondSourceToTarget ->
      False
    GhostLeafToRoot ->
      False

composeNonIdentityMorphisms ::
  CheckedMorphism SimpleObject SimpleMorphism ->
  CheckedMorphism SimpleObject SimpleMorphism ->
  Maybe (CheckedMorphism SimpleObject SimpleMorphism)
composeNonIdentityMorphisms outerMorphism innerMorphism =
  case (cmWitness outerMorphism, cmWitness innerMorphism) of
    (LeafEndomorphism, LeafEndomorphism) ->
      Just leafEndomorphism
    (LeafToRoot, LeafEndomorphism) ->
      Just leafToRoot
    (MiddleToTarget, SourceToMiddle) ->
      Just firstSourceToTarget
    _ ->
      Nothing

simplePullbackPair ::
  SimpleSite ->
  CheckedMorphism SimpleObject SimpleMorphism ->
  CheckedMorphism SimpleObject SimpleMorphism ->
  Maybe (PullbackSquare SimpleObject SimpleMorphism)
simplePullbackPair siteValue leftMorphism rightMorphism
  | cmTarget leftMorphism /= cmTarget rightMorphism =
      Nothing
  | isCanonicalIdentity leftMorphism =
      pullbackSquare
        leftMorphism
        rightMorphism
        (cmSource rightMorphism)
        rightMorphism
        (identityMorphism (cmSource rightMorphism))
  | isCanonicalIdentity rightMorphism =
      pullbackSquare
        leftMorphism
        rightMorphism
        (cmSource leftMorphism)
        (identityMorphism (cmSource leftMorphism))
        leftMorphism
  | siteValue == GoodSite && leftMorphism == rightMorphism =
      pullbackSquare
        leftMorphism
        rightMorphism
        (cmSource leftMorphism)
        (identityMorphism (cmSource leftMorphism))
        (identityMorphism (cmSource rightMorphism))
  | otherwise =
      Nothing

pullbackSquare ::
  CheckedMorphism SimpleObject SimpleMorphism ->
  CheckedMorphism SimpleObject SimpleMorphism ->
  SimpleObject ->
  CheckedMorphism SimpleObject SimpleMorphism ->
  CheckedMorphism SimpleObject SimpleMorphism ->
  Maybe (PullbackSquare SimpleObject SimpleMorphism)
pullbackSquare leftMorphism rightMorphism apexObject toLeft toRight =
  Just
    PullbackSquare
      { psLeftBase = leftMorphism,
        psRightBase = rightMorphism,
        psApex = apexObject,
        psToLeft = toLeft,
        psToRight = toRight
      }
