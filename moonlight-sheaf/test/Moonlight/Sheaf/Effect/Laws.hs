-- | moonlight-sheaf law bundle registry.
module Moonlight.Sheaf.Effect.Laws
  ( sheafLawBundles
  , tests
  ) where

import Moonlight.Sheaf.Effect.Harness qualified as Harness
import Moonlight.Sheaf.Effect.LawNames (LawName (..), lawName)
import Moonlight.Sheaf.Presheaf.MorphismCategorySpec qualified as MorphismCategorySpec
import Moonlight.Pale.Test.LawSuite
  ( LawBundle
  , lawBundleQuickCheck
  , lawGroup
  , lawSuiteGroup
  , quickCheckLawDefinition
  , renderedLawBundle
  , renderLawBundles
  , testTreeLaw
  )
import Moonlight.Sheaf.TestFixture.SheafClassLaws (stalkMergeLawTests)
import Test.Tasty (TestTree)

tests :: TestTree
tests =
  lawSuiteGroup "moonlight-sheaf laws" (renderLawBundles id sheafLawBundles)

sheafLawBundles :: [LawBundle String]
sheafLawBundles =
  [ lawBundleQuickCheck
      "site category"
      [ quickCheckLawDefinition SiteCompositionClosure Harness.siteCompositionClosureLaw
      , quickCheckLawDefinition SiteLeftIdentity Harness.siteLeftIdentityLaw
      , quickCheckLawDefinition SiteRightIdentity Harness.siteRightIdentityLaw
      , quickCheckLawDefinition SiteAssociativity Harness.siteAssociativityLaw
      , quickCheckLawDefinition SitePullbackCommutativity Harness.sitePullbackCommutativityLaw
      ]
  , lawBundleQuickCheck
      "site topology"
      [ quickCheckLawDefinition SiteIdentityCover Harness.siteIdentityCoverLaw
      , quickCheckLawDefinition SitePullbackStability Harness.sitePullbackStabilityLaw
      , quickCheckLawDefinition SiteCoverTransitivity Harness.siteCoverTransitivityLaw
      ]
  , lawBundleQuickCheck
      "site maps"
      [ quickCheckLawDefinition SiteMapFunctorial Harness.siteMapFunctorialLaw
      , quickCheckLawDefinition SiteMapCoverContinuity Harness.siteMapCoverContinuityLaw
      ]
  , lawBundleQuickCheck
      "restriction"
      [ quickCheckLawDefinition RestrictionIdentity Harness.restrictionIdentityLaw
      , quickCheckLawDefinition RestrictionComposition Harness.restrictionCompositionLaw
      ]
  , lawBundleQuickCheck
      "presheaf functoriality"
      [ quickCheckLawDefinition PresheafIdentity Harness.presheafIdentityLaw
      , quickCheckLawDefinition PresheafComposition Harness.presheafCompositionLaw
      , quickCheckLawDefinition FinitePresheafLawsValidated Harness.finitePresheafLawsValidatedLaw
      , quickCheckLawDefinition PresheafMorphismNaturality Harness.presheafMorphismNaturalityLaw
      ]
  , renderedLawBundle
      "finite presheaf morphism category"
      [ lawGroup
          (lawName FinitePresheafMorphismIdentity)
          [testTreeLaw MorphismCategorySpec.identityLawTests]
      , lawGroup
          (lawName FinitePresheafMorphismAssociativity)
          [testTreeLaw MorphismCategorySpec.associativityLawTests]
      ]
  , lawBundleQuickCheck
      "separation"
      [ quickCheckLawDefinition SeparatedLocalEquality Harness.separatedLocalEqualityLaw
      , quickCheckLawDefinition SeparatedPresheafCondition Harness.separatedPresheafConditionLaw
      ]
  , lawBundleQuickCheck
      "descent"
      [ quickCheckLawDefinition GluingPairwiseCompatibility Harness.gluingPairwiseCompatibilityLaw
      , quickCheckLawDefinition GluingAmalgamationLocality Harness.gluingAmalgamationLocalityLaw
      , quickCheckLawDefinition GluingUniqueAmalgamation Harness.gluingUniqueAmalgamationLaw
      , quickCheckLawDefinition FiniteSheafCondition Harness.finiteSheafConditionLaw
      ]
  , lawBundleQuickCheck
      "image adjunction"
      [ quickCheckLawDefinition ImageAdjunctionTriangles Harness.imageAdjunctionTrianglesLaw
      , quickCheckLawDefinition ContextGaloisAdjunctionTriangles Harness.contextGaloisAdjunctionTrianglesLaw
      ]
  , lawBundleQuickCheck
      "cosheaf duals"
      [ quickCheckLawDefinition CosheafCorestrictionIdentity Harness.cosheafCorestrictionIdentityLaw
      , quickCheckLawDefinition CosheafCorestrictionComposition Harness.cosheafCorestrictionCompositionLaw
      ]
  , renderedLawBundle
      "stalk merge"
      [ lawGroup
          (lawName StalkMergeLaws)
          [ testTreeLaw (stalkMergeLawTests Harness.discreteMergeLawsFixture)
          , testTreeLaw (stalkMergeLawTests Harness.branchMergeLawsFixture)
          ]
      ]
  , lawBundleQuickCheck
      "determinism"
      [ quickCheckLawDefinition SheafDeterministicFixture Harness.deterministicFixtureLaw
      ]
  ]
