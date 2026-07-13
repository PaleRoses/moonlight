module Moonlight.Sheaf.Proof
  ( ProofKind (..),
    ProofContextRestriction (..),
    ProofContextEvidence (..),
    SupportAwareProofEvidence (..),
    ProofAnnotationInput (..),
    ProofAnnotationBuilder (..),
    defaultProofAnnotationBuilder,
    contextualProofEvidenceFromPairs,
    supportAwareProofEvidenceFromPairs,
    mapProofContextEvidence,
    mapSupportAwareProofEvidence,
    ProofSection (..),
  )
where

import Data.Kind (Type)
import Data.Set (Set)

type ProofKind :: Type -> Type
data ProofKind ruleId
  = ProofRewrite ruleId
  | ProofCongruence
  | ProofAnalysis
  deriving stock (Eq, Ord, Show)

type ProofContextRestriction :: Type -> Type
data ProofContextRestriction ctx = ProofContextRestriction
  { pcrSourceContext :: ctx,
    pcrTargetContext :: ctx
  }
  deriving stock (Eq, Ord, Show, Read)

type ProofContextEvidence :: Type -> Type
data ProofContextEvidence ctx = ProofContextEvidence
  { pceActiveContext :: Maybe ctx,
    pceRestrictions :: [ProofContextRestriction ctx]
  }
  deriving stock (Eq, Ord, Show, Read)

type SupportAwareProofEvidence :: Type -> Type -> Type
data SupportAwareProofEvidence ctx support = SupportAwareProofEvidence
  { sapeSupport :: support,
    sapeRestrictions :: [ProofContextRestriction ctx]
  }
  deriving stock (Eq, Ord, Show, Read)

type ProofAnnotationInput ::
  Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data ProofAnnotationInput ctx ruleId node subst factDerivation guardEvidence guideEvidence support = ProofAnnotationInput
  { paiRewriteRuleId :: ruleId,
    paiLhsNode :: node,
    paiRhsNode :: node,
    paiSubstitution :: subst,
    paiGuardEvidence :: Maybe guardEvidence,
    paiGuideEvidence :: Maybe guideEvidence,
    paiFactDerivations :: Set factDerivation,
    paiContextEvidence :: Maybe (ProofContextEvidence ctx),
    paiSupportEvidence :: Maybe (SupportAwareProofEvidence ctx support)
  }
  deriving stock (Eq, Ord, Show)

type ProofAnnotationBuilder :: Type -> Type -> Type
newtype ProofAnnotationBuilder annotation p = ProofAnnotationBuilder
  { buildProofAnnotation :: annotation -> p
  }

instance Semigroup p => Semigroup (ProofAnnotationBuilder annotation p) where
  leftBuilder <> rightBuilder =
    ProofAnnotationBuilder
      (\annotation -> buildProofAnnotation leftBuilder annotation <> buildProofAnnotation rightBuilder annotation)

instance Monoid p => Monoid (ProofAnnotationBuilder annotation p) where
  mempty = defaultProofAnnotationBuilder

defaultProofAnnotationBuilder :: Monoid p => ProofAnnotationBuilder annotation p
defaultProofAnnotationBuilder =
  ProofAnnotationBuilder (const mempty)

contextualProofEvidenceFromPairs :: Maybe ctx -> [(ctx, ctx)] -> ProofContextEvidence ctx
contextualProofEvidenceFromPairs activeContext restrictionPairs =
  ProofContextEvidence
    { pceActiveContext = activeContext,
      pceRestrictions = fmap (uncurry ProofContextRestriction) restrictionPairs
    }

supportAwareProofEvidenceFromPairs :: support -> [(ctx, ctx)] -> SupportAwareProofEvidence ctx support
supportAwareProofEvidenceFromPairs supportValue restrictionPairs =
  SupportAwareProofEvidence
    { sapeSupport = supportValue,
      sapeRestrictions = fmap (uncurry ProofContextRestriction) restrictionPairs
    }

mapProofContextEvidence :: (left -> right) -> ProofContextEvidence left -> ProofContextEvidence right
mapProofContextEvidence mapContext evidenceValue =
  ProofContextEvidence
    { pceActiveContext = fmap mapContext (pceActiveContext evidenceValue),
      pceRestrictions =
        fmap
          (\restrictionValue -> ProofContextRestriction (mapContext (pcrSourceContext restrictionValue)) (mapContext (pcrTargetContext restrictionValue)))
          (pceRestrictions evidenceValue)
    }

mapSupportAwareProofEvidence ::
  (left -> right) ->
  SupportAwareProofEvidence left support ->
  SupportAwareProofEvidence right support
mapSupportAwareProofEvidence mapContext evidenceValue =
  SupportAwareProofEvidence
    { sapeSupport = sapeSupport evidenceValue,
      sapeRestrictions =
        fmap
          (\restrictionValue -> ProofContextRestriction (mapContext (pcrSourceContext restrictionValue)) (mapContext (pcrTargetContext restrictionValue)))
          (sapeRestrictions evidenceValue)
    }

type ProofSection :: Type -> Type -> Type
data ProofSection section proof = ProofSection
  { psSectionValue :: section,
    psSectionProof :: proof
  }
  deriving stock (Eq, Ord, Show, Read)
