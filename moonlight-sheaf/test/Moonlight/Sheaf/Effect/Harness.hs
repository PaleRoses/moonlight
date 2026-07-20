{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

-- | Executable moonlight-sheaf law harness over existing checkers.
module Moonlight.Sheaf.Effect.Harness
  ( siteCompositionClosureLaw
  , siteLeftIdentityLaw
  , siteRightIdentityLaw
  , siteAssociativityLaw
  , sitePullbackCommutativityLaw
  , siteIdentityCoverLaw
  , sitePullbackStabilityLaw
  , siteCoverTransitivityLaw
  , siteMapFunctorialLaw
  , siteMapCoverContinuityLaw
  , restrictionIdentityLaw
  , restrictionCompositionLaw
  , presheafIdentityLaw
  , presheafCompositionLaw
  , finitePresheafLawsValidatedLaw
  , presheafMorphismNaturalityLaw
  , separatedLocalEqualityLaw
  , separatedPresheafConditionLaw
  , gluingPairwiseCompatibilityLaw
  , gluingAmalgamationLocalityLaw
  , gluingUniqueAmalgamationLaw
  , finiteSheafConditionLaw
  , imageAdjunctionTrianglesLaw
  , contextGaloisAdjunctionTrianglesLaw
  , cosheafCorestrictionIdentityLaw
  , cosheafCorestrictionCompositionLaw
  , deterministicFixtureLaw
  , discreteMergeLawsFixture
  , branchMergeLawsFixture
  ) where

import Data.Bifunctor (first)
import Data.Either (isRight)
import Data.IntMap.Strict qualified as IntMap
import Data.Vector qualified as Vector
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Set qualified as Set
import Data.Void (Void)
import Moonlight.Cosheaf.Core
  ( CosheafLawFailure (..)
  , checkCorestrictionCompositionLawWith
  , checkCorestrictionIdentityLawWith
  )
import Moonlight.FiniteLattice (ContextLattice, compileContextLattice, contextOrderDecl)
import Moonlight.Sheaf.Image.Adjunction
  ( FiniteImageAdjunction (..)
  , FiniteSiteMapLeftTriangleFailure (..)
  , FiniteSiteMapRightTriangleFailure (..)
  , finiteImageAdjunctionSatisfied
  , finiteSiteMapImageAdjunction
  , finiteSiteMapLeftTriangleFailures
  , finiteSiteMapRightTriangleFailures
  )
import Moonlight.Sheaf.Image.Direct
  ( DirectImageCone
  , directImageConeAssignments
  , directImageConeTarget
  , mkDirectImageCone
  , pushforwardFinitePresheaf
  )
import Moonlight.Sheaf.Image.ContextGalois
  ( ContextGaloisMapFailure (..)
  , checkContextImageAdjunction
  , mkContextGaloisMap
  )
import Moonlight.Sheaf.Presheaf.Core
  ( PresheafLawFailure (..)
  , checkCompositionLaw
  , checkCompositionLawWith
  , checkIdentityLaw
  , checkIdentityLawWith
  )
import Moonlight.Sheaf.Presheaf.Enumeration (FiniteEnumerationBudget (..))
import Moonlight.Sheaf.Presheaf.Finite
  ( FinitePresheaf (..)
  , FinitePresheafFailure (..)
  , mkFinitePresheaf
  , validateFinitePresheafLaws
  )
import Moonlight.Sheaf.Presheaf.Morphism
  ( FinitePresheafMorphismFailure (..)
  , mkFinitePresheafMorphism
  )
import Moonlight.Sheaf.Presheaf.Separation
  ( SeparationConditionFailure (..)
  , checkSeparated
  , locallyEqualOnCover
  , separateFinitePresheaf
  )
import Moonlight.Sheaf.Section.Morphism
  ( RestrictionArrow (..)
  , RestrictionKind (..)
  , RestrictionParts (..)
  )
import Moonlight.Sheaf.Section.ObjectIndex (mkObjectIndex)
import Moonlight.Sheaf.Section.Restriction
  ( RestrictionIndex
  , RestrictionIndexError
  , buildRestrictionIndex
  )
import Moonlight.Sheaf.Section.Restriction.Law
  ( checkRestrictionCompositionLaw
  , checkRestrictionIdentityLaw
  )
import Moonlight.Sheaf.Section.Stalk
  ( StalkAlgebra (..)
  , StalkRestrictionKernel (..)
  )
import Moonlight.Sheaf.Section.Stalk.Discrete
  ( DiscreteMismatch
  , DiscreteRepairObstruction
  , discreteStalkAlgebra
  )
import Moonlight.Sheaf.Sheaf.Gluing
  ( CompatibleMatchingFamily
  , CoverStalkUniverse (..)
  , MatchingFamily
  , MatchingFailure (..)
  , SeparatedUniquenessRefusal (..)
  , UniqueAmalgamation
  , certifyAmalgamation
  , certifyMatchingFamilyCompatibility
  , certifySeparatedCover
  , certifyUniqueAmalgamation
  , mkMatchingFamily
  )
import Moonlight.Sheaf.Sheafification.Finite
  ( checkFiniteSheafCondition
  , sheafConditionReportAccepted
  )
import Moonlight.Sheaf.Sheafification.Finite qualified as Sheafification
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..)
  , CoverConstructionError
  , CoveringFamily
  , PullbackSquare (..)
  , Site (..)
  , mkCoveringFamily
  , siteMorphismUniverse
  )
import Moonlight.Sheaf.Site.CoverBasis.Finite
  ( finiteCoverPlanForCover
  , mkFiniteCoverBasis
  )
import Moonlight.Sheaf.Site.Construction.FiniteMeet
  ( FiniteMeetSite
  , FiniteMeetSiteSpec (..)
  , mkFiniteMeetSite
  )
import Moonlight.Sheaf.Site.Construction.Map
  ( ContinuousSiteMap
  , CoverImageFailure (..)
  , FiniteSiteMap
  , SiteMapFailure (..)
  , mkContinuousSiteMap
  , mkFiniteSiteMap
  )
import Moonlight.Sheaf.Site.Class.Validation
  ( allAssociativityFailures
  , allCompositionClosureFailures
  , allIdentityCoverFailures
  , allLeftIdentityFailures
  , allPullbackSquareCommutativityFailures
  , allPullbackStabilityFailures
  , allRightIdentityFailures
  , allTransitivityFailures
  )
import Moonlight.Sheaf.TestFixture.Branch
  ( BranchContext (..)
  , BranchMismatch (..)
  , BranchStalk
  , branchCompatibleAmalgamatedStalk
  , branchLeftCompatibleStalk
  , branchRightCompatibleStalk
  , branchRightIncompatibleStalk
  , branchStalk
  , branchStalkAlgebra
  , branchStalkEntries
  )
import Moonlight.Sheaf.TestFixture.Branch.Presheaf (branchCompiledStalkAlgebra)
import Moonlight.Sheaf.TestFixture.Branch.Site
  ( BranchMorphism
  , BranchSite
  , branchRootCover
  , branchSite
  )
import Moonlight.Sheaf.TestFixture.SheafClassLaws
  ( StalkGluingSample (..)
  , StalkMergeLawsFixture (..)
  )
import Moonlight.Sheaf.TestFixture.Site (sampleSystem)
import Test.Tasty.QuickCheck qualified as QC

siteCompositionClosureLaw :: QC.Property
siteCompositionClosureLaw =
  QC.conjoin
    [ acceptOne "sample composition closure" allCompositionClosureFailures sampleSystem
    , acceptOne "branch composition closure" allCompositionClosureFailures branchSite
    ]

siteLeftIdentityLaw :: QC.Property
siteLeftIdentityLaw =
  QC.conjoin
    [ acceptOne "sample left identity" allLeftIdentityFailures sampleSystem
    , acceptOne "branch left identity" allLeftIdentityFailures branchSite
    ]

siteRightIdentityLaw :: QC.Property
siteRightIdentityLaw =
  QC.conjoin
    [ acceptOne "sample right identity" allRightIdentityFailures sampleSystem
    , acceptOne "branch right identity" allRightIdentityFailures branchSite
    ]

siteAssociativityLaw :: QC.Property
siteAssociativityLaw =
  QC.conjoin
    [ acceptOne "sample associativity" allAssociativityFailures sampleSystem
    , acceptOne "branch associativity" allAssociativityFailures branchSite
    ]

sitePullbackCommutativityLaw :: QC.Property
sitePullbackCommutativityLaw =
  QC.conjoin
    [ acceptOne "sample pullback commutativity" allPullbackSquareCommutativityFailures sampleSystem
    , acceptOne "branch pullback commutativity" allPullbackSquareCommutativityFailures branchSite
    ]

siteIdentityCoverLaw :: QC.Property
siteIdentityCoverLaw =
  QC.conjoin
    [ acceptOne "sample identity covers" allIdentityCoverFailures sampleSystem
    , acceptOne "branch identity covers" allIdentityCoverFailures branchSite
    ]

sitePullbackStabilityLaw :: QC.Property
sitePullbackStabilityLaw =
  QC.conjoin
    [ acceptOne "sample pullback stability" allPullbackStabilityFailures sampleSystem
    , acceptOne "branch pullback stability" allPullbackStabilityFailures branchSite
    ]

siteCoverTransitivityLaw :: QC.Property
siteCoverTransitivityLaw =
  QC.conjoin
    [ acceptOne "sample cover transitivity" allTransitivityFailures sampleSystem
    , acceptOne "branch cover transitivity" allTransitivityFailures branchSite
    ]

siteMapFunctorialLaw :: QC.Property
siteMapFunctorialLaw =
  QC.counterexample (show (() <$ lawful, () <$ identityBroken, () <$ compositionBroken)) $
    isRight lawful
      && case identityBroken of
        Left SiteMapIdentityMismatch {} -> True
        _ -> False
      && case compositionBroken of
        Left SiteMapCompositionMismatch {} -> True
        _ -> False
  where
    lawful = identityFiniteSiteMap goodSite goodSite
    identityBroken = mkFiniteSiteMap goodSite identityEndomorphismSite identityObjectImages identityMismatchMorphismImages
    compositionBroken = mkFiniteSiteMap compositionSourceSite compositionTargetSite compositionObjectImages compositionMismatchMorphismImages

siteMapCoverContinuityLaw :: QC.Property
siteMapCoverContinuityLaw =
  QC.counterexample (show (() <$ lawful, fmap (() <$) continuityBroken)) $
    isRight lawful
      && case continuityBroken of
        Right (Left CoverImageNotInTargetBasis {}) -> True
        _ -> False
  where
    lawful = do
      siteMapValue <- first show (identityFiniteSiteMap goodSite goodSite)
      sourceBasis <- first show (mkFiniteCoverBasis goodSite)
      targetBasis <- first show (mkFiniteCoverBasis goodSite)
      first show (mkContinuousSiteMap sourceBasis targetBasis siteMapValue)
    continuityBroken = do
      siteMapValue <- first show (identityFiniteSiteMap goodSite goodSite)
      sourceBasis <- first show (mkFiniteCoverBasis goodSite)
      targetBasis <- first show (mkFiniteCoverBasis (SimpleSite IdentityOnlyTopology))
      pure (mkContinuousSiteMap sourceBasis targetBasis siteMapValue)

restrictionIdentityLaw :: QC.Property
restrictionIdentityLaw =
  QC.withNumTests 100 $
    QC.forAll stepCellAndStalkGen $ \(cellValue, stalkValue) ->
      case stepRestrictionIndex of
        Left failure -> QC.counterexample (show failure) False
        Right restrictions ->
          let lawFailure = checkRestrictionIdentityLaw stepStalkAlgebra restrictions cellValue stalkValue
           in QC.counterexample (show lawFailure) (isNothing lawFailure)

restrictionCompositionLaw :: QC.Property
restrictionCompositionLaw =
  QC.withNumTests 100 $
    QC.forAll stepArrowPairAndStalkGen $ \((firstArrow, secondArrow), stalkValue) ->
      case stepRestrictionIndex of
        Left failure -> QC.counterexample (show failure) False
        Right restrictions ->
          let lawFailure = checkRestrictionCompositionLaw stepStalkAlgebra restrictions firstArrow secondArrow stalkValue
           in QC.counterexample (show lawFailure) (isNothing lawFailure)

presheafIdentityLaw :: QC.Property
presheafIdentityLaw =
  QC.withNumTests 100 $
    QC.forAll branchObjectAndStalkGen $ \(objectValue, stalkValue) ->
      let lawful = checkIdentityLaw branchCompiledStalkAlgebra branchSite objectValue stalkValue
          broken = checkIdentityLawWith branchCompiledStalkAlgebra branchSite bumpApexRestrictAction objectValue stalkValue
       in QC.counterexample (show (lawful, broken)) $
            isRight lawful
              && case broken of
                Left IdentityRestrictionMismatch {} -> True
                _ -> False

presheafCompositionLaw :: QC.Property
presheafCompositionLaw =
  QC.withNumTests 100 $
    QC.forAll branchComposableAndStalkGen $ \(outerMorphism, innerMorphism, stalkValue) ->
      let lawful = checkCompositionLaw branchCompiledStalkAlgebra branchSite outerMorphism innerMorphism stalkValue
          broken = checkCompositionLawWith branchCompiledStalkAlgebra branchSite bumpApexRestrictAction outerMorphism innerMorphism stalkValue
       in QC.counterexample (show (lawful, broken)) $
            isRight lawful
              && case broken of
                Left CompositionRestrictionMismatch {} -> True
                _ -> False

finitePresheafLawsValidatedLaw :: QC.Property
finitePresheafLawsValidatedLaw =
  case chainSiteValue of
    Left failure -> QC.counterexample failure False
    Right site ->
      let lawful = mkChainIntPresheaf site id
          identityBroken ::
            Either
              (FinitePresheafFailure (SiteObject ChainSite) (SiteMorphism ChainSite) Int () VoidRestrictionFailure)
              (FinitePresheaf ChainSite Int () VoidRestrictionFailure)
          identityBroken =
            mkFinitePresheaf
              site
              (\morphismValue value -> Right (if cmSource morphismValue == cmTarget morphismValue then 1 - value else value))
              chainMismatch
              (\_ value -> value)
              chainFibers
          normalizationBroken ::
            Either
              (FinitePresheafFailure (SiteObject ChainSite) (SiteMorphism ChainSite) Int () VoidRestrictionFailure)
              (FinitePresheaf ChainSite Int () VoidRestrictionFailure)
          normalizationBroken =
            mkFinitePresheaf site chainRestrict chainMismatch (\_ value -> if value == 1 then 0 else value) chainFibers
       in QC.counterexample (show (() <$ lawful, () <$ identityBroken, () <$ normalizationBroken)) $
            (lawful >>= validateFinitePresheafLaws) == Right ()
              && case identityBroken of
                Left FiniteIdentityRestrictionMismatch {} -> True
                _ -> False
              && case normalizationBroken of
                Left FiniteFiberValueNotNormalized {} -> True
                _ -> False

presheafMorphismNaturalityLaw :: QC.Property
presheafMorphismNaturalityLaw =
  case chainSiteValue of
    Left failure -> QC.counterexample failure False
    Right site ->
      case mkChainIntPresheaf site id of
        Left failure -> QC.counterexample (show failure) False
        Right presheaf ->
          let lawful = mkFinitePresheafMorphism presheaf presheaf identityChainComponent
              broken = mkFinitePresheafMorphism presheaf presheaf unnaturalChainComponent
           in QC.counterexample (show (() <$ lawful, () <$ broken)) $
                isRight lawful
                  && case broken of
                    Left FinitePresheafMorphismNaturalityMismatch {} -> True
                    _ -> False

separatedLocalEqualityLaw :: QC.Property
separatedLocalEqualityLaw =
  QC.counterexample (show (localEquality, separatedFailure)) $
    localEquality == Right True
      && case separatedFailure of
        Just LocallyEqualButGloballyDifferent {} -> True
        _ -> False
  where
    localEquality = do
      basis <- first show (mkFiniteCoverBasis coveredSimpleSite)
      coverValue <- first show leafCover
      planValue <- first show (finiteCoverPlanForCover basis coverValue)
      presheaf <- first show (mkSimplePresheaf coveredSimpleSite [RootSection 0, RootSection 1] [LeafSection 0])
      first show (locallyEqualOnCover presheaf planValue (RootSection 0) (RootSection 1))
    separatedFailure = do
      basis <- either (const Nothing) Just (mkFiniteCoverBasis coveredSimpleSite)
      presheaf <- either (const Nothing) Just (mkSimplePresheaf coveredSimpleSite [RootSection 0, RootSection 1] [LeafSection 0])
      separated <- either (const Nothing) Just (separateFinitePresheaf basis presheaf)
      case checkSeparated separated of
        failure : _ -> Just failure
        [] -> Nothing

separatedPresheafConditionLaw :: QC.Property
separatedPresheafConditionLaw =
  QC.counterexample (show (lawful, broken)) $
    either (const False) null lawful
      && case broken of
        Right (LocallyEqualButGloballyDifferent {} : _) -> True
        _ -> False
  where
    lawful = separatedFailuresFor [RootSection 0]
    broken = separatedFailuresFor [RootSection 0, RootSection 1]
    separatedFailuresFor rootValues = do
      basis <- first show (mkFiniteCoverBasis coveredSimpleSite)
      presheaf <- first show (mkSimplePresheaf coveredSimpleSite rootValues [LeafSection 0])
      separated <- first show (separateFinitePresheaf basis presheaf)
      pure (checkSeparated separated)

gluingPairwiseCompatibilityLaw :: QC.Property
gluingPairwiseCompatibilityLaw =
  QC.counterexample (show (() <$ lawful, fmap (() <$) broken)) $
    isRight lawful
      && case broken of
        Right (Left (PullbackDisagreement {} :| _)) -> True
        _ -> False
  where
    lawful = compatibleBranchFamily branchRightCompatibleStalk
    broken = do
      family <- matchingBranchFamily branchRightIncompatibleStalk
      pure (certifyMatchingFamilyCompatibility branchCompiledStalkAlgebra branchSite family)

gluingAmalgamationLocalityLaw :: QC.Property
gluingAmalgamationLocalityLaw =
  QC.counterexample (show (fmap (() <$) lawful, fmap (() <$) broken)) $
    either (const False) isRight lawful
      && case broken of
        Right (Left (_ :| _)) -> True
        _ -> False
  where
    lawful = branchAmalgamationFor branchCompatibleAmalgamatedStalk
    broken = branchAmalgamationFor branchRightIncompatibleStalk
    branchAmalgamationFor stalk = do
      family <- compatibleBranchFamily branchRightCompatibleStalk
      pure (certifyAmalgamation branchCompiledStalkAlgebra branchSite family stalk)

gluingUniqueAmalgamationLaw :: QC.Property
gluingUniqueAmalgamationLaw =
  QC.counterexample (show (fmap (() <$) lawful, fmap (() <$) ghost)) $
    either (const False) isRight lawful
      && case ghost of
        Right (Left UniquenessAmalgamatedStalkOutsideUniverse) -> True
        _ -> False
  where
    lawful = uniqueBranchAmalgamation branchCompatibleAmalgamatedStalk
    ghost = uniqueBranchAmalgamation (branchStalk [(BranchApex, 99)])

finiteSheafConditionLaw :: QC.Property
finiteSheafConditionLaw =
  QC.counterexample (show (accepted, rejected)) $
    either (const False) sheafConditionReportAccepted accepted
      && either (const False) (not . sheafConditionReportAccepted) rejected
  where
    accepted = sheafConditionReportFor [RootSection 0] [LeafSection 0]
    rejected = sheafConditionReportFor [RootSection 0, RootSection 1] [LeafSection 0]

imageAdjunctionTrianglesLaw :: QC.Property
imageAdjunctionTrianglesLaw =
  QC.conjoin
    [ case result of
        Left failure -> QC.counterexample failure False
        Right adjunction ->
          QC.counterexample
            (show (finiteImageAdjunctionLeftTriangleFailures adjunction, finiteImageAdjunctionRightTriangleFailures adjunction))
            ( finiteImageAdjunctionSatisfied adjunction
                && null (finiteImageAdjunctionLeftTriangleFailures adjunction)
                && null (finiteImageAdjunctionRightTriangleFailures adjunction)
            )
    , QC.counterexample (show leftBroken) $
        case leftBroken of
          Right failures -> not (null [failure | failure@FiniteSiteMapLeftTriangleMismatch {} <- failures])
          Left _ -> False
    , QC.counterexample (show rightBroken) $
        case rightBroken of
          Right failures -> not (null [failure | failure@FiniteSiteMapRightTriangleMismatch {} <- failures])
          Left _ -> False
    ]
  where
    result = do
      site <- chainSiteValue
      continuous <- identityContinuousSiteMap site
      presheaf <- first show (mkChainIntPresheaf site id)
      first show (finiteSiteMapImageAdjunction unboundedBudget continuous presheaf presheaf)
    leftBroken = do
      site <- chainSiteValue
      siteMapValue <- first show (selfIdentityFiniteSiteMap site)
      honest <- first show (mkChainIntPresheaf site id)
      let perturbed =
            honest {fpRestrict = \_ value -> Right (1 - value)} ::
              FinitePresheaf ChainSite Int () VoidRestrictionFailure
      pure (finiteSiteMapLeftTriangleFailures siteMapValue perturbed honest)
    rightBroken = do
      site <- chainSiteValue
      continuous <- identityContinuousSiteMap site
      siteMapValue <- first show (selfIdentityFiniteSiteMap site)
      sourcePresheaf <- first show (mkChainIntPresheaf site id)
      pushed <- first show (pushforwardFinitePresheaf unboundedBudget continuous sourcePresheaf)
      let flipCone ::
            DirectImageCone ChainCell ChainCell (SiteMorphism ChainSite) Int ->
            DirectImageCone ChainCell ChainCell (SiteMorphism ChainSite) Int
          flipCone coneValue =
            mkDirectImageCone (directImageConeTarget coneValue) (fmap (1 -) (directImageConeAssignments coneValue))
          perturbed =
            pushed {fpRestrict = \morphismValue coneValue -> fmap flipCone (fpRestrict pushed morphismValue coneValue)}
      pure (finiteSiteMapRightTriangleFailures siteMapValue perturbed)

contextGaloisAdjunctionTrianglesLaw :: QC.Property
contextGaloisAdjunctionTrianglesLaw =
  QC.counterexample (show (() <$ lawful, () <$ broken)) $
    either (const False) finiteImageAdjunctionSatisfied lawful
      && case broken of
        Left ContextGaloisAdjunctionFailed {} -> True
        _ -> False
  where
    lawful = do
      lattice <- tinyLattice
      site <- tinySiteValue
      galois <- first show (mkContextGaloisMap lattice lattice site site id id)
      presheaf <- tinyPresheaf site
      first show (checkContextImageAdjunction galois presheaf presheaf)
    broken =
      case (tinyLattice, tinySiteValue) of
        (Right lattice, Right site) -> mkContextGaloisMap lattice lattice site site (const TinyCoarse) id
        _ -> Left (ContextGaloisAdjunctionFailed TinyCoarse TinyFine TinyCoarse TinyFine False True)

cosheafCorestrictionIdentityLaw :: QC.Property
cosheafCorestrictionIdentityLaw =
  QC.counterexample (show (lawful, broken)) $
    lawful == Right ()
      && case broken of
        Left IdentityCorestrictionMismatch {} -> True
        _ -> False
  where
    lawful = checkCorestrictionIdentityLawWith chainCorestrict chainCostalkMismatch cosheafChainSite ChainA 0
    broken = checkCorestrictionIdentityLawWith perturbedIdentityCorestrict chainCostalkMismatch cosheafChainSite ChainA 0

cosheafCorestrictionCompositionLaw :: QC.Property
cosheafCorestrictionCompositionLaw =
  QC.counterexample (show (lawful, broken)) $
    lawful == Right ()
      && case broken of
        Left CompositionCorestrictionUndefined {} -> True
        _ -> False
  where
    lawful = checkCorestrictionCompositionLawWith chainCorestrict chainCostalkMismatch cosheafChainSite chainBC chainAB 0
    broken = checkCorestrictionCompositionLawWith chainCorestrict chainCostalkMismatch cosheafMissingCompositeSite chainBC chainAB 0

deterministicFixtureLaw :: Bool
deterministicFixtureLaw =
  branchCompatibleAmalgamatedStalk == branchCompatibleAmalgamatedStalk
    && fmap branchStalkEntries [branchLeftCompatibleStalk, branchRightCompatibleStalk]
      == fmap branchStalkEntries [branchLeftCompatibleStalk, branchRightCompatibleStalk]

acceptOne :: Show failure => String -> (site -> [failure]) -> site -> QC.Property
acceptOne label checker site =
  let failures = checker site
   in QC.counterexample (show (label, failures)) (null failures)

unboundedBudget :: FiniteEnumerationBudget
unboundedBudget =
  FiniteEnumerationBudget Nothing

data SimpleTopology
  = IdentityOnlyTopology
  | LeafCoverTopology
  | IdentityEndomorphismTopology
  | CompositionSourceTopology
  | CompositionTargetTopology
  deriving stock (Eq, Ord, Show)

newtype SimpleSite = SimpleSite SimpleTopology
  deriving stock (Eq, Ord, Show)

goodSite :: SimpleSite
goodSite =
  SimpleSite LeafCoverTopology

identityEndomorphismSite :: SimpleSite
identityEndomorphismSite =
  SimpleSite IdentityEndomorphismTopology

compositionSourceSite :: SimpleSite
compositionSourceSite =
  SimpleSite CompositionSourceTopology

compositionTargetSite :: SimpleSite
compositionTargetSite =
  SimpleSite CompositionTargetTopology

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
  siteObjects (SimpleSite topology) =
    case topology of
      IdentityOnlyTopology -> [Leaf, Root]
      LeafCoverTopology -> [Leaf, Root]
      IdentityEndomorphismTopology -> [Leaf, Root]
      CompositionSourceTopology -> compositionObjects
      CompositionTargetTopology -> compositionObjects
  siteMorphisms (SimpleSite topology) =
    case topology of
      IdentityOnlyTopology -> simpleMorphisms
      LeafCoverTopology -> simpleMorphisms
      IdentityEndomorphismTopology -> simpleMorphisms <> [leafEndomorphism]
      CompositionSourceTopology -> compositionMorphisms
      CompositionTargetTopology -> compositionMorphisms <> [secondSourceToTarget]
  identityAt _ = identityMorphism
  coversAt (SimpleSite topology) objectValue = [coverValue | topology == LeafCoverTopology, objectValue == Root, Right coverValue <- [leafCover]]
  composeChecked _ outerMorphism innerMorphism
    | cmSource outerMorphism /= cmTarget innerMorphism = Nothing
    | isCanonicalIdentity outerMorphism = Just innerMorphism
    | isCanonicalIdentity innerMorphism = Just outerMorphism
    | otherwise = composeNonIdentityMorphisms outerMorphism innerMorphism
  pullbackPair siteValue = simplePullbackPair siteValue

identityMorphism :: SimpleObject -> CheckedMorphism SimpleObject SimpleMorphism
identityMorphism objectValue =
  CheckedMorphism objectValue objectValue (SimpleIdentity objectValue)

leafEndomorphism :: CheckedMorphism SimpleObject SimpleMorphism
leafEndomorphism =
  CheckedMorphism Leaf Leaf LeafEndomorphism

leafToRoot :: CheckedMorphism SimpleObject SimpleMorphism
leafToRoot =
  CheckedMorphism Leaf Root LeafToRoot

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

simplePullbackPair :: SimpleSite -> CheckedMorphism SimpleObject SimpleMorphism -> CheckedMorphism SimpleObject SimpleMorphism -> Maybe (PullbackSquare SimpleObject SimpleMorphism)
simplePullbackPair siteValue leftMorphism rightMorphism
  | cmTarget leftMorphism /= cmTarget rightMorphism = Nothing
  | isCanonicalIdentity leftMorphism =
      Just
        ( PullbackSquare
            leftMorphism
            rightMorphism
            (cmSource rightMorphism)
            rightMorphism
            (identityMorphism (cmSource rightMorphism))
        )
  | isCanonicalIdentity rightMorphism =
      Just
        ( PullbackSquare
            leftMorphism
            rightMorphism
            (cmSource leftMorphism)
            (identityMorphism (cmSource leftMorphism))
            leftMorphism
        )
  | siteValue == SimpleSite LeafCoverTopology && leftMorphism == rightMorphism =
      Just
        ( PullbackSquare
            leftMorphism
            rightMorphism
            (cmSource leftMorphism)
            (identityMorphism (cmSource leftMorphism))
            (identityMorphism (cmSource rightMorphism))
        )
  | otherwise = Nothing

isCanonicalIdentity :: CheckedMorphism SimpleObject SimpleMorphism -> Bool
isCanonicalIdentity morphismValue =
  case cmWitness morphismValue of
    SimpleIdentity _ -> True
    LeafEndomorphism -> False
    LeafToRoot -> False
    SourceToMiddle -> False
    MiddleToTarget -> False
    FirstSourceToTarget -> False
    SecondSourceToTarget -> False
    GhostLeafToRoot -> False

composeNonIdentityMorphisms ::
  CheckedMorphism SimpleObject SimpleMorphism ->
  CheckedMorphism SimpleObject SimpleMorphism ->
  Maybe (CheckedMorphism SimpleObject SimpleMorphism)
composeNonIdentityMorphisms outerMorphism innerMorphism =
  case (cmWitness outerMorphism, cmWitness innerMorphism) of
    (LeafEndomorphism, LeafEndomorphism) -> Just leafEndomorphism
    (LeafToRoot, LeafEndomorphism) -> Just leafToRoot
    (MiddleToTarget, SourceToMiddle) -> Just firstSourceToTarget
    _ -> Nothing

identityObjectImages :: Map SimpleObject SimpleObject
identityObjectImages =
  Map.fromList [(Leaf, Leaf), (Root, Root)]

identityMorphismImages :: Map (CheckedMorphism SimpleObject SimpleMorphism) (CheckedMorphism SimpleObject SimpleMorphism)
identityMorphismImages =
  Map.fromList [(identityMorphism Leaf, identityMorphism Leaf), (identityMorphism Root, identityMorphism Root), (leafToRoot, leafToRoot)]

identityMismatchMorphismImages :: Map (CheckedMorphism SimpleObject SimpleMorphism) (CheckedMorphism SimpleObject SimpleMorphism)
identityMismatchMorphismImages =
  Map.insert (identityMorphism Leaf) leafEndomorphism identityMorphismImages

compositionObjectImages :: Map SimpleObject SimpleObject
compositionObjectImages =
  Map.fromList
    [ (CompositionSource, CompositionSource)
    , (CompositionMiddle, CompositionMiddle)
    , (CompositionTarget, CompositionTarget)
    ]

compositionMismatchMorphismImages :: Map (CheckedMorphism SimpleObject SimpleMorphism) (CheckedMorphism SimpleObject SimpleMorphism)
compositionMismatchMorphismImages =
  Map.fromList
    [ (identityMorphism CompositionSource, identityMorphism CompositionSource)
    , (identityMorphism CompositionMiddle, identityMorphism CompositionMiddle)
    , (identityMorphism CompositionTarget, identityMorphism CompositionTarget)
    , (sourceToMiddle, sourceToMiddle)
    , (middleToTarget, middleToTarget)
    , (firstSourceToTarget, secondSourceToTarget)
    ]

identityFiniteSiteMap ::
  SimpleSite ->
  SimpleSite ->
  Either
    (SiteMapFailure SimpleObject SimpleMorphism SimpleObject SimpleMorphism)
    (FiniteSiteMap SimpleSite SimpleSite)
identityFiniteSiteMap sourceSite targetSite =
  mkFiniteSiteMap sourceSite targetSite identityObjectImages identityMorphismImages

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
    (\_ sectionValue -> sectionValue)
    (Map.fromList [(Leaf, leafValues), (Root, rootValues)])

simpleRestrict ::
  CheckedMorphism SimpleObject SimpleMorphism ->
  SimpleSection ->
  Either SimpleRestrictionFailure SimpleSection
simpleRestrict morphismValue sectionValue =
  case (cmSource morphismValue, cmTarget morphismValue, sectionValue) of
    (Leaf, Leaf, LeafSection _) -> Right sectionValue
    (Root, Root, RootSection _) -> Right sectionValue
    (Leaf, Root, RootSection _) -> Right (LeafSection 0)
    _ -> Left (SimpleRestrictionFailure (cmWitness morphismValue) sectionValue)

simpleMismatches :: SimpleObject -> SimpleSection -> SimpleSection -> [SimpleMismatch]
simpleMismatches objectValue leftValue rightValue =
  [SimpleMismatch objectValue leftValue rightValue | leftValue /= rightValue]

data ChainCell = ChainCoarse | ChainFine
  deriving stock (Eq, Ord, Show, Read)

data VoidRestrictionFailure
  deriving stock (Eq, Show)

type ChainSite = FiniteMeetSite ChainCell

chainSiteValue :: Either String ChainSite
chainSiteValue =
  first show $
    mkFiniteMeetSite
      FiniteMeetSiteSpec
        { fmssCells = ChainCoarse :| [ChainFine]
        , fmssRefinements = Set.singleton (ChainFine, ChainCoarse)
        , fmssCovers = Map.empty
        }

chainFibers :: Map ChainCell [Int]
chainFibers =
  Map.fromList [(ChainCoarse, [0, 1]), (ChainFine, [0, 1])]

chainRestrict ::
  CheckedMorphism ChainCell (SiteMorphism ChainSite) ->
  Int ->
  Either VoidRestrictionFailure Int
chainRestrict _ value =
  Right value

chainMismatch :: ChainCell -> Int -> Int -> [()]
chainMismatch _ leftValue rightValue =
  [() | leftValue /= rightValue]

mkChainIntPresheaf ::
  ChainSite ->
  (Int -> Int) ->
  Either
    (FinitePresheafFailure (SiteObject ChainSite) (SiteMorphism ChainSite) Int () VoidRestrictionFailure)
    (FinitePresheaf ChainSite Int () VoidRestrictionFailure)
mkChainIntPresheaf site normalizeCoarse =
  mkFinitePresheaf
    site
    chainRestrict
    chainMismatch
    (\objectValue value -> if objectValue == ChainCoarse then normalizeCoarse value else value)
    chainFibers

identityChainComponent :: ChainCell -> Int -> Either Void Int
identityChainComponent _ value =
  Right value

unnaturalChainComponent :: ChainCell -> Int -> Either Void Int
unnaturalChainComponent objectValue value =
  case objectValue of
    ChainCoarse -> Right value
    ChainFine -> Right (if value == 0 then 1 else 0)

selfIdentityFiniteSiteMap ::
  ChainSite ->
  Either
    (SiteMapFailure ChainCell (SiteMorphism ChainSite) ChainCell (SiteMorphism ChainSite))
    (FiniteSiteMap ChainSite ChainSite)
selfIdentityFiniteSiteMap site =
  mkFiniteSiteMap
    site
    site
    (Map.fromList (fmap (\objectValue -> (objectValue, objectValue)) (siteObjects site)))
    (Map.fromList (fmap (\morphismValue -> (morphismValue, morphismValue)) (siteMorphismUniverse site)))

identityContinuousSiteMap ::
  ChainSite ->
  Either String (ContinuousSiteMap ChainSite ChainSite)
identityContinuousSiteMap site = do
  siteMapValue <- first show (selfIdentityFiniteSiteMap site)
  basis <- first show (mkFiniteCoverBasis site)
  first show (mkContinuousSiteMap basis basis siteMapValue)

sheafConditionReportFor ::
  [SimpleSection] ->
  [SimpleSection] ->
  Either String (Sheafification.SheafConditionReport SimpleObject SimpleSection SimpleMismatch)
sheafConditionReportFor rootValues leafValues = do
  basis <- first show (mkFiniteCoverBasis coveredSimpleSite)
  presheaf <- first show (mkSimplePresheaf coveredSimpleSite rootValues leafValues)
  first show (checkFiniteSheafCondition unboundedBudget basis presheaf)

coveredSimpleSite :: SimpleSite
coveredSimpleSite =
  SimpleSite LeafCoverTopology

bumpApexRestrictAction ::
  CheckedMorphism BranchContext BranchMorphism ->
  BranchStalk ->
  BranchStalk
bumpApexRestrictAction _ stalkValue =
  branchStalk (Map.toList (Map.adjust (+ 1) BranchApex (branchStalkEntries stalkValue)))

matchingBranchFamily :: BranchStalk -> Either String (MatchingFamily BranchSite BranchStalk)
matchingBranchFamily rightStalk = do
  let coverValue = branchRootCover
  basis <- first show (mkFiniteCoverBasis branchSite)
  planValue <- first show (finiteCoverPlanForCover basis coverValue)
  first show (mkMatchingFamily planValue (Vector.fromList [branchLeftCompatibleStalk, rightStalk]))

compatibleBranchFamily ::
  BranchStalk ->
  Either String (CompatibleMatchingFamily BranchSite BranchStalk)
compatibleBranchFamily rightStalk =
  matchingBranchFamily rightStalk
    >>= first show . certifyMatchingFamilyCompatibility branchCompiledStalkAlgebra branchSite

uniqueBranchAmalgamation ::
  BranchStalk ->
  Either
    String
    (Either (SeparatedUniquenessRefusal BranchMismatch) (UniqueAmalgamation BranchSite BranchStalk))
uniqueBranchAmalgamation stalk = do
  let coverValue = branchRootCover
  basis <- first show (mkFiniteCoverBasis branchSite)
  planValue <- first show (finiteCoverPlanForCover basis coverValue)
  family <- compatibleBranchFamily branchRightCompatibleStalk
  separated <-
    first show $
      certifySeparatedCover
        branchCompiledStalkAlgebra
        branchSite
        planValue
        CoverStalkUniverse
          { csuTargetStalks = [branchCompatibleAmalgamatedStalk]
          , csuSlotStalks = IntMap.fromList [(0, [branchLeftCompatibleStalk]), (1, [branchRightCompatibleStalk])]
          }
  pure (certifyUniqueAmalgamation branchCompiledStalkAlgebra branchSite separated family stalk)

branchObjectAndStalkGen :: QC.Gen (BranchContext, BranchStalk)
branchObjectAndStalkGen =
  QC.elements
    [ (BranchBase, branchCompatibleAmalgamatedStalk)
    , (BranchLeft, branchLeftCompatibleStalk)
    , (BranchRight, branchRightCompatibleStalk)
    , (BranchApex, branchStalk [(BranchApex, 7)])
    ]

branchComposableAndStalkGen ::
  QC.Gen
    ( CheckedMorphism BranchContext BranchMorphism
    , CheckedMorphism BranchContext BranchMorphism
    , BranchStalk
    )
branchComposableAndStalkGen =
  QC.elements
    [ (outerMorphism, innerMorphism, branchCompatibleAmalgamatedStalk)
    | outerMorphism <- siteMorphisms branchSite
    , innerMorphism <- siteMorphisms branchSite
    , cmSource outerMorphism == cmTarget innerMorphism
    ]

data StepCell = StepTop | StepMid | StepBottom
  deriving stock (Eq, Ord, Show)

stepCells :: [StepCell]
stepCells =
  [StepTop, StepMid, StepBottom]

stepDepth :: StepCell -> Int
stepDepth cellValue =
  case cellValue of
    StepTop -> 0
    StepMid -> 1
    StepBottom -> 2

stepStalkAlgebra :: StalkAlgebra (StepCell, StepCell) Int (Int, Int) ()
stepStalkAlgebra =
  StalkAlgebra
    { saRestrictionKernel = \(fromCell, toCell) -> StalkRestrictionMap (+ (stepDepth toCell - stepDepth fromCell))
    , saMismatches = \leftValue rightValue -> [(leftValue, rightValue) | leftValue /= rightValue]
    , saMerge = \leftValue _ -> Right leftValue
    , saRepair = const (Left ())
    , saNormalize = id
    }

stepRestrictionIndex :: Either (RestrictionIndexError StepCell) (RestrictionIndex StepCell (StepCell, StepCell))
stepRestrictionIndex =
  buildRestrictionIndex
    (mkObjectIndex stepCells)
    ( \(fromCell, toCell) ->
        RestrictionParts
          { partKind = PortalRestriction
          , partSource = fromCell
          , partTarget = toCell
          , partWitness = (fromCell, toCell)
          }
    )
    ([(cellValue, cellValue) | cellValue <- stepCells] <> [(StepTop, StepMid), (StepMid, StepBottom), (StepTop, StepBottom)])

stepCellAndStalkGen :: QC.Gen (StepCell, Int)
stepCellAndStalkGen =
  (,) <$> QC.elements stepCells <*> QC.arbitrary

stepArrowPairAndStalkGen :: QC.Gen ((RestrictionArrow StepCell, RestrictionArrow StepCell), Int)
stepArrowPairAndStalkGen =
  (,) (RestrictionArrow StepTop StepMid, RestrictionArrow StepMid StepBottom) <$> QC.arbitrary

data TinyContext = TinyCoarse | TinyFine
  deriving stock (Eq, Ord, Show, Read)

tinyLattice :: Either String (ContextLattice TinyContext)
tinyLattice =
  first show $
    compileContextLattice
      (Set.fromList [TinyCoarse, TinyFine])
      (contextOrderDecl TinyFine TinyCoarse [(TinyCoarse, TinyFine)])

tinySiteValue :: Either String (FiniteMeetSite TinyContext)
tinySiteValue =
  first show $
    mkFiniteMeetSite
      FiniteMeetSiteSpec
        { fmssCells = TinyCoarse :| [TinyFine]
        , fmssRefinements = Set.singleton (TinyFine, TinyCoarse)
        , fmssCovers = Map.empty
        }

tinyPresheaf ::
  FiniteMeetSite TinyContext ->
  Either String (FinitePresheaf (FiniteMeetSite TinyContext) Int () VoidRestrictionFailure)
tinyPresheaf site =
  first show $ mkFinitePresheaf site (\_ value -> Right value) (\_ leftValue rightValue -> [() | leftValue /= rightValue]) (\_ value -> value) (Map.fromList [(TinyCoarse, [0, 1]), (TinyFine, [0, 1])])

data ChainObject = ChainA | ChainB | ChainC
  deriving stock (Eq, Ord, Show)

data ChainMorphism = ChainIdentity ChainObject | ChainAB | ChainBC | ChainAC
  deriving stock (Eq, Ord, Show)

data CosheafChainSite = CosheafChainSite Bool
  deriving stock (Eq, Ord, Show)

cosheafChainSite :: CosheafChainSite
cosheafChainSite =
  CosheafChainSite True

cosheafMissingCompositeSite :: CosheafChainSite
cosheafMissingCompositeSite =
  CosheafChainSite False

instance Site CosheafChainSite where
  type SiteObject CosheafChainSite = ChainObject
  type SiteMorphism CosheafChainSite = ChainMorphism
  siteObjects _ = [ChainA, ChainB, ChainC]
  siteMorphisms _ = [identityAt cosheafChainSite ChainA, identityAt cosheafChainSite ChainB, identityAt cosheafChainSite ChainC, chainAB, chainBC, chainAC]
  identityAt _ objectValue = CheckedMorphism objectValue objectValue (ChainIdentity objectValue)
  coversAt _ _ = []
  composeChecked (CosheafChainSite hasComposite) outerMorphism innerMorphism
    | cmSource outerMorphism /= cmTarget innerMorphism = Nothing
    | isCosheafIdentity outerMorphism = Just innerMorphism
    | isCosheafIdentity innerMorphism = Just outerMorphism
    | hasComposite && outerMorphism == chainBC && innerMorphism == chainAB = Just chainAC
    | otherwise = Nothing
  pullbackPair _ _ _ = Nothing

chainAB :: CheckedMorphism ChainObject ChainMorphism
chainAB = CheckedMorphism ChainA ChainB ChainAB

chainBC :: CheckedMorphism ChainObject ChainMorphism
chainBC = CheckedMorphism ChainB ChainC ChainBC

chainAC :: CheckedMorphism ChainObject ChainMorphism
chainAC = CheckedMorphism ChainA ChainC ChainAC

isCosheafIdentity :: CheckedMorphism ChainObject ChainMorphism -> Bool
isCosheafIdentity morphismValue =
  case cmWitness morphismValue of
    ChainIdentity _ -> True
    _ -> False

chainCorestrict :: CheckedMorphism ChainObject ChainMorphism -> Int -> Either Void Int
chainCorestrict _ value =
  Right value

perturbedIdentityCorestrict :: CheckedMorphism ChainObject ChainMorphism -> Int -> Either Void Int
perturbedIdentityCorestrict morphismValue value =
  if isCosheafIdentity morphismValue then Right (value + 1) else Right value

chainCostalkMismatch :: ChainObject -> Int -> Int -> [()]
chainCostalkMismatch _ leftValue rightValue =
  [() | leftValue /= rightValue]

discreteMergeLawsFixture :: StalkMergeLawsFixture () Int (DiscreteMismatch Int) (DiscreteRepairObstruction Int)
discreteMergeLawsFixture =
  StalkMergeLawsFixture
    { smlfName = "discrete merge laws"
    , smlfStalkAlgebra = discreteStalkAlgebra
    , smlfGenStalk = QC.arbitrary
    , smlfGenCompatiblePair = (\value -> (value, value)) <$> QC.arbitrary
    , smlfGenGluingSample = (\value -> StalkGluingSample value value value value) <$> QC.arbitrary
    , smlfLeq = (==)
    }

branchMergeLawsFixture :: StalkMergeLawsFixture () BranchStalk BranchMismatch ()
branchMergeLawsFixture =
  StalkMergeLawsFixture
    { smlfName = "branch merge laws"
    , smlfStalkAlgebra = branchStalkAlgebra
    , smlfGenStalk = branchStalk <$> genBranchEntries
    , smlfGenCompatiblePair = genCompatibleBranchPair
    , smlfGenGluingSample = genBranchGluingSample
    , smlfLeq = \leftStalk rightStalk -> Map.isSubmapOf (branchStalkEntries leftStalk) (branchStalkEntries rightStalk)
    }

genBranchEntries :: QC.Gen [(BranchContext, Int)]
genBranchEntries =
  QC.listOf ((,) <$> QC.elements [minBound .. maxBound] <*> QC.arbitrary)

genGlobalSection :: QC.Gen (Map BranchContext Int)
genGlobalSection =
  Map.fromList <$> traverse (\contextValue -> (,) contextValue <$> QC.arbitrary) [minBound .. maxBound]

restrictedEntries ::
  Map BranchContext Int ->
  [BranchContext] ->
  [(BranchContext, Int)]
restrictedEntries globalSection domain =
  Map.toList (Map.restrictKeys globalSection (Set.fromList domain))

genCompatibleBranchPair :: QC.Gen (BranchStalk, BranchStalk)
genCompatibleBranchPair =
  compatibleBranchPairFromDomains <$> genGlobalSection <*> QC.sublistOf [minBound .. maxBound] <*> QC.sublistOf [minBound .. maxBound]

compatibleBranchPairFromDomains ::
  Map BranchContext Int ->
  [BranchContext] ->
  [BranchContext] ->
  (BranchStalk, BranchStalk)
compatibleBranchPairFromDomains globalSection leftDomain rightDomain =
  (branchStalk (restrictedEntries globalSection leftDomain), branchStalk (restrictedEntries globalSection rightDomain))

genBranchGluingSample :: QC.Gen (StalkGluingSample BranchStalk)
genBranchGluingSample =
  branchGluingSample <$> genGlobalSection <*> QC.sublistOf [minBound .. maxBound] <*> QC.sublistOf [minBound .. maxBound] <*> QC.sublistOf [minBound .. maxBound]

branchGluingSample ::
  Map BranchContext Int ->
  [BranchContext] ->
  [BranchContext] ->
  [BranchContext] ->
  StalkGluingSample BranchStalk
branchGluingSample globalSection firstDomain secondDomain thirdDomain =
  StalkGluingSample
    { sgsFirstStalk = branchStalk (restrictedEntries globalSection firstDomain)
    , sgsSecondStalk = branchStalk (restrictedEntries globalSection secondDomain)
    , sgsThirdStalk = branchStalk (restrictedEntries globalSection thirdDomain)
    , sgsExpectedGluedStalk = branchStalk (Map.toList (Map.restrictKeys globalSection (Set.fromList (firstDomain <> secondDomain <> thirdDomain))))
    }
