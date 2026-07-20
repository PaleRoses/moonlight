module Moonlight.EGraph.Effect.LawNames
  ( EGraphLawName (..),
    eGraphLawName,
    module Moonlight.Core,
  )
where

import Data.Kind (Type)
import Moonlight.Core (CommonLawName (..), IsLawName (..), constructorLawNameWithOverrides)

type EGraphLawName :: Type
data EGraphLawName
  = CommonLaw CommonLawName
  | FindIdempotent
  | FindCanonical
  | MergeCommutative
  | MergeIdempotent
  | UnionFindPathCompression
  | CongruenceClosure
  | HashConsIdempotent
  | HashConsCanonicalChildren
  | RebuildRestoresCongruence
  | RebuildIdempotent
  | EMatchSoundness
  | EMatchCompleteness
  | SaturationBounded
  | SaturationDeterministic
  | SaturationMonotoneNodeCount
  | SupportUnionIdempotent
  | SupportUnionCommutative
  | SupportUnionAssociative
  | SupportMeetIntersection
  | SupportRestrictionDistributive
  | SupportSaturationOrderInvariant
  | ExtractInClass
  | ExtractOptimal
  | ExtractDeterministic
  | AnalysisJoinCommutative
  | AnalysisJoinAssociative
  | AnalysisMakeConsistent
  | ContextRestricts
  | ContextGlobalSection
  | ContextGlobalSectionInvariant
  | ContextRestrictionIdentity
  | ContextRestrictionComposition
  | ContextMorphismLeftIdentity
  | ContextMorphismRightIdentity
  | ContextMorphismAssociative
  | ContextRestrictionFunctorialAction
  | ContextMergeMonotone
  | ProofSoundness
  | ProofContextConsistency
  | AntiUnifyGeneralizes
  | AntiUnifyLeast
  | ObstructionComplete
  deriving stock (Eq, Ord, Show)

eGraphLawName :: EGraphLawName -> String
eGraphLawName lawNameValue =
  case lawNameValue of
    CommonLaw commonLawName -> lawNameText commonLawName
    specificLawName ->
      constructorLawNameWithOverrides
        [ ("EMatchSoundness", "ematch_soundness"),
          ("EMatchCompleteness", "ematch_completeness")
        ]
        (show specificLawName)

instance IsLawName EGraphLawName where
  lawNameText = eGraphLawName
