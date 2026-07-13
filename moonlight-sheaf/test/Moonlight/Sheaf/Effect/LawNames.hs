{-# LANGUAGE DerivingStrategies #-}

-- | Closed law-name vocabulary for moonlight-sheaf law registries.
module Moonlight.Sheaf.Effect.LawNames
  ( LawName (..)
  , lawName
  ) where

import Data.Kind (Type)
import Moonlight.Core (IsLawName (..), constructorLawNameWithOverrides)

type LawName :: Type
data LawName
  = SiteCompositionClosure
  | SiteLeftIdentity
  | SiteRightIdentity
  | SiteAssociativity
  | SitePullbackCommutativity
  | SiteIdentityCover
  | SitePullbackStability
  | SiteCoverTransitivity
  | SiteMapFunctorial
  | SiteMapCoverContinuity
  | RestrictionIdentity
  | RestrictionComposition
  | PresheafIdentity
  | PresheafComposition
  | FinitePresheafLawsValidated
  | PresheafMorphismNaturality
  | FinitePresheafMorphismIdentity
  | FinitePresheafMorphismAssociativity
  | SeparatedLocalEquality
  | SeparatedPresheafCondition
  | GluingPairwiseCompatibility
  | GluingAmalgamationLocality
  | GluingUniqueAmalgamation
  | FiniteSheafCondition
  | ImageAdjunctionTriangles
  | ContextGaloisAdjunctionTriangles
  | CosheafCorestrictionIdentity
  | CosheafCorestrictionComposition
  | StalkMergeLaws
  | SheafDeterministicFixture
  deriving stock (Eq, Ord, Show)

instance IsLawName LawName where
  lawNameText = lawName

lawName :: LawName -> String
lawName =
  constructorLawNameWithOverrides
    [ ("SiteCompositionClosure", "sheaf_site_composition_closure")
    , ("SiteLeftIdentity", "sheaf_site_left_identity")
    , ("SiteRightIdentity", "sheaf_site_right_identity")
    , ("SiteAssociativity", "sheaf_site_associativity")
    , ("SitePullbackCommutativity", "sheaf_site_pullback_commutativity")
    , ("SiteIdentityCover", "sheaf_site_identity_cover")
    , ("SitePullbackStability", "sheaf_site_pullback_stability")
    , ("SiteCoverTransitivity", "sheaf_site_cover_transitivity")
    , ("SiteMapFunctorial", "sheaf_site_map_functorial")
    , ("SiteMapCoverContinuity", "sheaf_site_map_cover_continuity")
    , ("RestrictionIdentity", "sheaf_restriction_identity")
    , ("RestrictionComposition", "sheaf_restriction_composition")
    , ("PresheafIdentity", "sheaf_presheaf_identity")
    , ("PresheafComposition", "sheaf_presheaf_composition")
    , ("FinitePresheafLawsValidated", "sheaf_finite_presheaf_laws_validated")
    , ("PresheafMorphismNaturality", "sheaf_presheaf_morphism_naturality")
    , ("FinitePresheafMorphismIdentity", "sheaf_finite_presheaf_morphism_identity")
    , ("FinitePresheafMorphismAssociativity", "sheaf_finite_presheaf_morphism_associativity")
    , ("SeparatedLocalEquality", "sheaf_separated_local_equality")
    , ("SeparatedPresheafCondition", "sheaf_separated_presheaf_condition")
    , ("GluingPairwiseCompatibility", "sheaf_gluing_pairwise_compatibility")
    , ("GluingAmalgamationLocality", "sheaf_gluing_amalgamation_locality")
    , ("GluingUniqueAmalgamation", "sheaf_gluing_unique_amalgamation")
    , ("FiniteSheafCondition", "sheaf_finite_sheaf_condition")
    , ("ImageAdjunctionTriangles", "sheaf_image_adjunction_triangles")
    , ("ContextGaloisAdjunctionTriangles", "sheaf_context_galois_adjunction_triangles")
    , ("CosheafCorestrictionIdentity", "sheaf_cosheaf_corestriction_identity")
    , ("CosheafCorestrictionComposition", "sheaf_cosheaf_corestriction_composition")
    , ("StalkMergeLaws", "sheaf_stalk_merge_laws")
    , ("SheafDeterministicFixture", "sheaf_deterministic_fixture")
    ]
    . show
