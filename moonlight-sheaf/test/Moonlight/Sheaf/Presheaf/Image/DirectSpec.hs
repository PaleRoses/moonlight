{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Presheaf.Image.DirectSpec
  ( tests,
  )
where

import Data.Bifunctor (first)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Sheaf.Image.Direct
  ( DirectImageCone,
    DirectImageMismatch,
    DirectImageIndexObject (..),
    DirectImageRestrictionFailure,
    directImageConeAssignments,
    directImageConeTarget,
    directImageConeValueAt,
    pushforwardFinitePresheaf,
  )
import Moonlight.Sheaf.Presheaf.Enumeration
  ( FiniteEnumerationBudget (..),
  )
import Moonlight.Sheaf.Presheaf.Finite
  ( FinitePresheaf (..),
    finiteFiberAt,
    finiteFiberValues,
    mkFinitePresheaf,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    PullbackSquare (..),
    Site (..),
    siteMorphismUniverse,
  )
import Moonlight.Sheaf.Site.CoverBasis.Finite
  ( mkFiniteCoverBasis,
  )
import Moonlight.Sheaf.Site.Construction.FiniteMeet
  ( FiniteMeetMorphism,
    FiniteMeetSite,
    FiniteMeetSiteSpec (..),
    finiteMeetMorphism,
    mkFiniteMeetSite,
  )
import Moonlight.Sheaf.Site.Construction.Map
  ( ContinuousSiteMap,
    mkContinuousSiteMap,
    mkFiniteSiteMap,
  )
import Moonlight.Sheaf.TestFixture.Assertions (expectRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "finite-site direct image"
    [ testCase "identity chain direct image is the compatible comma-cone limit" testIdentityChainDirectImage,
      testCase "restriction projects direct-image cones along target arrows" testRestrictionProjection,
      testCase "empty comma fiber is the singleton terminal cone" testEmptyCommaFiber,
      testCase "diamond direct image accepts exactly globally compatible cones" testDiamondCompatibility,
      testCase "non-thin parallel arrows remain distinct comma coordinates" testNonThinParallelCoordinates
    ]

data ChainCell = ChainCoarse | ChainFine
  deriving stock (Eq, Ord, Show, Read)

data TargetCell = TargetBottom | TargetHit | TargetMiss
  deriving stock (Eq, Ord, Show, Read)

data SourceCell = SourceOnly
  deriving stock (Eq, Ord, Show, Read)

data DiamondCell = DiamondGlobal | DiamondLeft | DiamondRight | DiamondOverlap
  deriving stock (Eq, Ord, Show, Read)

data ParallelObject = ParallelRoot | ParallelLeaf
  deriving stock (Eq, Ord, Show)

data ParallelMorphism = ParallelIdentity !ParallelObject | ParallelFirst | ParallelSecond
  deriving stock (Eq, Ord, Show)

data ParallelSite = ParallelSite
  deriving stock (Eq, Ord, Show)

data VoidRestrictionFailure
  deriving stock (Eq, Show)

type ChainSite = FiniteMeetSite ChainCell

type TargetSite = FiniteMeetSite TargetCell

type SourceSite = FiniteMeetSite SourceCell

type DiamondSite = FiniteMeetSite DiamondCell

type IdentityDirectImage site =
  FinitePresheaf
    site
    (DirectImageCone (SiteObject site) (SiteObject site) (SiteMorphism site) Int)
    (DirectImageMismatch (SiteObject site) (SiteObject site) (SiteMorphism site) Int ())
    (DirectImageRestrictionFailure (SiteObject site) (SiteObject site) (SiteMorphism site))

unboundedBudget :: FiniteEnumerationBudget
unboundedBudget =
  FiniteEnumerationBudget Nothing

testIdentityChainDirectImage :: Assertion
testIdentityChainDirectImage = do
  withIdentityDirectImage chainSite chainFibers $ \site directImage -> do
    fineIdentityIndex <- finiteMeetIdentityIndex site ChainFine
    fineToCoarseIndex <- finiteMeetIndex site ChainFine ChainCoarse
    coarseIdentityIndex <- finiteMeetIdentityIndex site ChainCoarse
    assertCoordinateValues ChainFine fineIdentityIndex directImage [Just 10, Just 11]
    assertCoordinateValues ChainCoarse fineToCoarseIndex directImage [Just 10, Just 11]
    assertCoordinateValues ChainCoarse coarseIdentityIndex directImage [Just 10, Just 11]

testRestrictionProjection :: Assertion
testRestrictionProjection = do
  withIdentityDirectImage chainSite chainFibers $ \site directImage -> do
    fineToCoarse <- expectMorphism site ChainFine ChainCoarse
    coarseIdentityIndex <- finiteMeetIdentityIndex site ChainCoarse
    fineIdentityIndex <- finiteMeetIdentityIndex site ChainFine
    coarseCone <- expectConeWith coarseIdentityIndex 10 =<< fiberValuesAt ChainCoarse directImage
    restrictedCone <- expectRight (fpRestrict directImage fineToCoarse coarseCone)
    directImageConeTarget restrictedCone @?= ChainFine
    directImageConeValueAt fineIdentityIndex restrictedCone @?= Just 10

testEmptyCommaFiber :: Assertion
testEmptyCommaFiber = do
  source <- expectRight sourceSite
  target <- expectRight targetSite
  sourceIdentity <- expectMorphism source SourceOnly SourceOnly
  targetHitIdentity <- expectMorphism target TargetHit TargetHit
  sourceBasis <- expectRight (mkFiniteCoverBasis source)
  targetBasis <- expectRight (mkFiniteCoverBasis target)
  siteMapValue <-
    expectRight
      ( mkFiniteSiteMap
          source
          target
          (Map.singleton SourceOnly TargetHit)
          (Map.singleton sourceIdentity targetHitIdentity)
      )
  continuous <- expectRight (mkContinuousSiteMap sourceBasis targetBasis siteMapValue)
  sourcePresheaf <- expectRight (mkConstantIntPresheaf source (Map.singleton SourceOnly [1, 2]))
  directImage <- expectRight (pushforwardFinitePresheaf unboundedBudget continuous sourcePresheaf)
  missCones <- fiberValuesAt TargetMiss directImage
  fmap directImageConeTarget missCones @?= [TargetMiss]
  fmap directImageConeAssignments missCones @?= [Map.empty]

testDiamondCompatibility :: Assertion
testDiamondCompatibility =
  withIdentityDirectImage diamondSite diamondFibers $ \site directImage -> do
    globalIdentityIndex <- finiteMeetIdentityIndex site DiamondGlobal
    leftToGlobalIndex <- finiteMeetIndex site DiamondLeft DiamondGlobal
    rightToGlobalIndex <- finiteMeetIndex site DiamondRight DiamondGlobal
    overlapToGlobalIndex <- finiteMeetIndex site DiamondOverlap DiamondGlobal
    observed <- traverse (\indexObject -> coordinateValues DiamondGlobal indexObject directImage) [globalIdentityIndex, leftToGlobalIndex, rightToGlobalIndex, overlapToGlobalIndex]
    observed @?= replicate 4 [Just 0, Just 1]

testNonThinParallelCoordinates :: Assertion
testNonThinParallelCoordinates = do
  continuous <- expectRight (identityContinuousSiteMap ParallelSite)
  presheaf <- expectRight parallelPresheaf
  directImage <- expectRight (pushforwardFinitePresheaf unboundedBudget continuous presheaf)
  leafCones <- fiberValuesAt ParallelLeaf directImage
  cone <- expectConeWith parallelLeafIdentityIndex 1 leafCones
  directImageConeValueAt parallelLeafIdentityIndex cone @?= Just 1
  directImageConeValueAt parallelFirstIndex cone @?= Just 1
  directImageConeValueAt parallelSecondIndex cone @?= Just 101

chainSite :: Either String ChainSite
chainSite =
  finiteMeetSite (ChainCoarse :| [ChainFine]) (Set.singleton (ChainFine, ChainCoarse)) Map.empty

sourceSite :: Either String SourceSite
sourceSite =
  finiteMeetSite (SourceOnly :| []) Set.empty Map.empty

targetSite :: Either String TargetSite
targetSite =
  finiteMeetSite
    (TargetBottom :| [TargetHit, TargetMiss])
    (Set.fromList [(TargetBottom, TargetHit), (TargetBottom, TargetMiss)])
    Map.empty

diamondSite :: Either String DiamondSite
diamondSite =
  finiteMeetSite
    (DiamondGlobal :| [DiamondLeft, DiamondRight, DiamondOverlap])
    ( Set.fromList
        [ (DiamondLeft, DiamondGlobal),
          (DiamondRight, DiamondGlobal),
          (DiamondOverlap, DiamondLeft),
          (DiamondOverlap, DiamondRight)
        ]
    )
    (Map.singleton DiamondGlobal [DiamondLeft :| [DiamondRight]])

finiteMeetSite ::
  (Ord cell, Show cell) =>
  NonEmpty cell ->
  Set.Set (cell, cell) ->
  Map cell [NonEmpty cell] ->
  Either String (FiniteMeetSite cell)
finiteMeetSite cells refinements covers =
  first show $
    mkFiniteMeetSite
      FiniteMeetSiteSpec
        { fmssCells = cells,
          fmssRefinements = refinements,
          fmssCovers = covers
        }

chainFibers :: Map ChainCell [Int]
chainFibers =
  Map.fromList [(ChainCoarse, [10, 11]), (ChainFine, [10, 11])]

diamondFibers :: Map DiamondCell [Int]
diamondFibers =
  Map.fromList
    [ (DiamondGlobal, [0, 1]),
      (DiamondLeft, [0, 1]),
      (DiamondRight, [0, 1]),
      (DiamondOverlap, [0, 1])
    ]

withIdentityDirectImage ::
  ( Site site,
    Ord (SiteMorphism site),
    Show (SiteObject site),
    Show (SiteMorphism site)
  ) =>
  Either String site ->
  Map (SiteObject site) [Int] ->
  (site -> IdentityDirectImage site -> Assertion) ->
  Assertion
withIdentityDirectImage siteResult fibers continue = do
  site <- expectRight siteResult
  continuous <- expectRight (identityContinuousSiteMap site)
  presheaf <- expectRight (mkConstantIntPresheaf site fibers)
  directImage <- expectRight (pushforwardFinitePresheaf unboundedBudget continuous presheaf)
  continue site directImage

mkConstantIntPresheaf ::
  ( Site site,
    Show (SiteObject site),
    Show (SiteMorphism site)
  ) =>
  site ->
  Map (SiteObject site) [Int] ->
  Either String (FinitePresheaf site Int () VoidRestrictionFailure)
mkConstantIntPresheaf site fibers =
  first show $
    mkFinitePresheaf
      site
      (\_morphism value -> Right value)
      (\_object leftValue rightValue -> [() | leftValue /= rightValue])
      (\_object value -> value)
      fibers

parallelPresheaf :: Either String (FinitePresheaf ParallelSite Int () VoidRestrictionFailure)
parallelPresheaf =
  first show $
    mkFinitePresheaf
      ParallelSite
      parallelRestrict
      (\_object leftValue rightValue -> [() | leftValue /= rightValue])
      (\_object value -> value)
      ( Map.fromList
          [ (ParallelRoot, [1, 2, 101, 102]),
            (ParallelLeaf, [1, 2])
          ]
      )

parallelRestrict ::
  CheckedMorphism ParallelObject ParallelMorphism ->
  Int ->
  Either VoidRestrictionFailure Int
parallelRestrict morphismValue value =
  case cmWitness morphismValue of
    ParallelIdentity _object ->
      Right value
    ParallelFirst ->
      Right value
    ParallelSecond ->
      Right (value + 100)

identityContinuousSiteMap ::
  ( Site site,
    Ord (SiteMorphism site),
    Show (SiteObject site),
    Show (SiteMorphism site)
  ) =>
  site ->
  Either String (ContinuousSiteMap site site)
identityContinuousSiteMap site = do
  let objectImages =
        Map.fromList [(objectValue, objectValue) | objectValue <- siteObjects site]
      morphismImages =
        Map.fromList [(morphismValue, morphismValue) | morphismValue <- siteMorphismUniverse site]
  siteMapValue <- first show (mkFiniteSiteMap site site objectImages morphismImages)
  basis <- first show (mkFiniteCoverBasis site)
  first show (mkContinuousSiteMap basis basis siteMapValue)

finiteMeetIdentityIndex ::
  (Ord cell, Show cell) =>
  FiniteMeetSite cell ->
  cell ->
  AssertionWith (DirectImageIndexObject cell cell (FiniteMeetMorphism cell))
finiteMeetIdentityIndex site objectValue =
  finiteMeetIndex site objectValue objectValue

finiteMeetIndex ::
  (Ord cell, Show cell) =>
  FiniteMeetSite cell ->
  cell ->
  cell ->
  AssertionWith (DirectImageIndexObject cell cell (FiniteMeetMorphism cell))
finiteMeetIndex site source target = do
  morphismValue <- expectMorphism site source target
  pure
    DirectImageIndexObject
      { directImageIndexSourceObject = source,
        directImageIndexTargetMorphism = morphismValue
      }

parallelLeafIdentityIndex :: DirectImageIndexObject ParallelObject ParallelObject ParallelMorphism
parallelLeafIdentityIndex =
  DirectImageIndexObject ParallelLeaf (identityAt ParallelSite ParallelLeaf)

parallelFirstIndex :: DirectImageIndexObject ParallelObject ParallelObject ParallelMorphism
parallelFirstIndex =
  DirectImageIndexObject ParallelRoot (CheckedMorphism ParallelRoot ParallelLeaf ParallelFirst)

parallelSecondIndex :: DirectImageIndexObject ParallelObject ParallelObject ParallelMorphism
parallelSecondIndex =
  DirectImageIndexObject ParallelRoot (CheckedMorphism ParallelRoot ParallelLeaf ParallelSecond)

coordinateValues ::
  (Site site, Show (SiteObject site), Ord (SiteMorphism site)) =>
  SiteObject site ->
  DirectImageIndexObject (SiteObject site) (SiteObject site) (SiteMorphism site) ->
  IdentityDirectImage site ->
  AssertionWith [Maybe Int]
coordinateValues objectValue indexObject directImage =
  fmap (directImageConeValueAt indexObject) <$> fiberValuesAt objectValue directImage

assertCoordinateValues ::
  (Site site, Show (SiteObject site), Ord (SiteMorphism site)) =>
  SiteObject site ->
  DirectImageIndexObject (SiteObject site) (SiteObject site) (SiteMorphism site) ->
  IdentityDirectImage site ->
  [Maybe Int] ->
  Assertion
assertCoordinateValues objectValue indexObject directImage expectedValues =
  coordinateValues objectValue indexObject directImage >>= (@?= expectedValues)

fiberValuesAt ::
  ( Site site,
    Show (SiteObject site)
  ) =>
  SiteObject site ->
  FinitePresheaf site value mismatch restrictionFailure ->
  AssertionWith [value]
fiberValuesAt objectValue presheaf =
  maybe
    (assertFailure ("expected finite fiber at " <> show objectValue))
    (pure . finiteFiberValues)
    (finiteFiberAt objectValue presheaf)

expectConeWith ::
  (Ord sourceObj, Ord targetObj, Ord targetMor, Eq value, Show value) =>
  DirectImageIndexObject sourceObj targetObj targetMor ->
  value ->
  [DirectImageCone sourceObj targetObj targetMor value] ->
  AssertionWith (DirectImageCone sourceObj targetObj targetMor value)
expectConeWith indexObject expectedValue cones =
  maybe
    (assertFailure ("expected direct-image cone with coordinate " <> show expectedValue))
    pure
    (find ((== Just expectedValue) . directImageConeValueAt indexObject) cones)

expectMorphism ::
  (Ord cell, Show cell) =>
  FiniteMeetSite cell ->
  cell ->
  cell ->
  AssertionWith (CheckedMorphism cell (FiniteMeetMorphism cell))
expectMorphism site source target =
  maybe
    (assertFailure ("expected finite-meet morphism " <> show (source, target)))
    pure
    (finiteMeetMorphism site source target)

type AssertionWith value = IO value


instance Site ParallelSite where
  type SiteObject ParallelSite = ParallelObject
  type SiteMorphism ParallelSite = ParallelMorphism

  siteObjects _site =
    [ParallelRoot, ParallelLeaf]

  siteMorphisms _site =
    [ identityAt ParallelSite ParallelRoot,
      identityAt ParallelSite ParallelLeaf,
      CheckedMorphism ParallelRoot ParallelLeaf ParallelFirst,
      CheckedMorphism ParallelRoot ParallelLeaf ParallelSecond
    ]

  identityAt _site objectValue =
    CheckedMorphism objectValue objectValue (ParallelIdentity objectValue)

  coversAt _site _object =
    []

  composeChecked _site outerMorphism innerMorphism
    | cmTarget innerMorphism /= cmSource outerMorphism =
        Nothing
    | otherwise =
        composeParallel outerMorphism innerMorphism

  pullbackPair _site leftMorphism rightMorphism
    | cmTarget leftMorphism /= cmTarget rightMorphism =
        Nothing
    | otherwise =
        parallelPullbackSquare leftMorphism rightMorphism

composeParallel ::
  CheckedMorphism ParallelObject ParallelMorphism ->
  CheckedMorphism ParallelObject ParallelMorphism ->
  Maybe (CheckedMorphism ParallelObject ParallelMorphism)
composeParallel outerMorphism innerMorphism =
  case (cmWitness outerMorphism, cmWitness innerMorphism) of
    (ParallelIdentity _, _) ->
      Just innerMorphism
    (_, ParallelIdentity _) ->
      Just outerMorphism
    _ ->
      Nothing

parallelPullbackSquare ::
  CheckedMorphism ParallelObject ParallelMorphism ->
  CheckedMorphism ParallelObject ParallelMorphism ->
  Maybe (PullbackSquare ParallelObject ParallelMorphism)
parallelPullbackSquare leftMorphism rightMorphism =
  case (cmWitness leftMorphism, cmWitness rightMorphism) of
    (ParallelIdentity _, _) ->
      pullbackSquare leftMorphism rightMorphism (cmSource rightMorphism) rightMorphism (identityAt ParallelSite (cmSource rightMorphism))
    (_, ParallelIdentity _) ->
      pullbackSquare leftMorphism rightMorphism (cmSource leftMorphism) (identityAt ParallelSite (cmSource leftMorphism)) leftMorphism
    _
      | leftMorphism == rightMorphism ->
          pullbackSquare
            leftMorphism
            rightMorphism
            (cmSource leftMorphism)
            (identityAt ParallelSite (cmSource leftMorphism))
            (identityAt ParallelSite (cmSource rightMorphism))
      | otherwise ->
          Nothing

pullbackSquare ::
  CheckedMorphism ParallelObject ParallelMorphism ->
  CheckedMorphism ParallelObject ParallelMorphism ->
  ParallelObject ->
  CheckedMorphism ParallelObject ParallelMorphism ->
  CheckedMorphism ParallelObject ParallelMorphism ->
  Maybe (PullbackSquare ParallelObject ParallelMorphism)
pullbackSquare leftMorphism rightMorphism apex toLeft toRight =
  Just
    PullbackSquare
      { psLeftBase = leftMorphism,
        psRightBase = rightMorphism,
        psApex = apex,
        psToLeft = toLeft,
        psToRight = toRight
      }
