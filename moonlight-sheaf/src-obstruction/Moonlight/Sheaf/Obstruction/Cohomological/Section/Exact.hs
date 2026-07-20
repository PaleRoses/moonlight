module Moonlight.Sheaf.Obstruction.Cohomological.Section.Exact
  ( CohomologicalExactMatchEvidence (..),
    CohomologicalExactMatch (..),
    CohomologicalExactEvidenceCoverage,
    CohomologicalRootCoverage,
    CohomologicalExactCoverage (..),
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Moonlight.Sheaf.Obstruction.Cohomological.Section
  ( RelationEvidence,
    SectionCoverage,
  )

type CohomologicalExactMatchEvidence :: Type -> Type
data CohomologicalExactMatchEvidence coordinate = CohomologicalExactMatchEvidence
  { cemeFactRelations :: ![RelationEvidence coordinate],
    cemeProvenanceRelations :: ![RelationEvidence coordinate],
    cemeProofRelations :: ![RelationEvidence coordinate],
    cemeCapabilityRelations :: ![RelationEvidence coordinate]
  }
  deriving stock (Eq, Ord, Show, Read)

instance Semigroup (CohomologicalExactMatchEvidence coordinate) where
  left <> right =
    CohomologicalExactMatchEvidence
      { cemeFactRelations = cemeFactRelations left <> cemeFactRelations right,
        cemeProvenanceRelations = cemeProvenanceRelations left <> cemeProvenanceRelations right,
        cemeProofRelations = cemeProofRelations left <> cemeProofRelations right,
        cemeCapabilityRelations = cemeCapabilityRelations left <> cemeCapabilityRelations right
      }

instance Monoid (CohomologicalExactMatchEvidence coordinate) where
  mempty = CohomologicalExactMatchEvidence [] [] [] []

type CohomologicalExactMatch :: Type -> Type -> Type -> Type
data CohomologicalExactMatch root result coordinate = CohomologicalExactMatch
  { cemRootClass :: !root,
    cemSubstitution :: !result,
    cemEvidence :: !(CohomologicalExactMatchEvidence coordinate)
  }
  deriving stock (Eq, Ord, Show, Read)

type CohomologicalRootCoverage :: Type -> Type -> Type -> Type -> Type
type CohomologicalRootCoverage root result coordinate gap =
  SectionCoverage (CohomologicalExactMatch root result coordinate) gap

type CohomologicalExactEvidenceCoverage :: Type -> Type -> Type
type CohomologicalExactEvidenceCoverage coordinate gap =
  SectionCoverage (CohomologicalExactMatchEvidence coordinate) gap

type CohomologicalExactCoverage :: Type -> Type -> Type -> Type -> Type
data CohomologicalExactCoverage root result coordinate gap = CohomologicalExactCoverage
  { cecFeasibleRoots :: !(Set root),
    cecMatches :: ![CohomologicalExactMatch root result coordinate],
    cecLoweringGaps :: !(Map root [gap]),
    cecRootCoverage :: !(Map root (CohomologicalRootCoverage root result coordinate gap)),
    cecEvidenceCoverage :: !(Map root (CohomologicalExactEvidenceCoverage coordinate gap))
  }
  deriving stock (Eq, Show, Read)
