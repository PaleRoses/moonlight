module Moonlight.Sheaf.Obstruction.Cohomological.Evidence.Microsupport
  ( MicrosupportEnrichment (..),
    computeMicrosupportEnrichment,
    computeNerveMicrosupportEnrichment,
    microsupportNodeFilter,
    microsupportCandidateRegionFilter,
    microsupportCandidateRegionSeedFilter,
    nerveSiteToPoset,
  )
where

import Data.Kind (Type)
import Data.Bifunctor (first)
import Data.Set qualified as Set
import Moonlight.Core (MoonlightError (InvariantViolation), RegionNodeId)
import Moonlight.Derived.Failure (derivedFailureToMoonlightError)
import Moonlight.Derived.Morse
  ( MicrosupportResult (..),
    microsupportOfDifferential,
  )
import Moonlight.Derived.Matrix (entriesToBlockedMatGF2Checked)
import Moonlight.Derived.Site (Criticality (..))
import Moonlight.Derived.Site (FinObjectId (..), DerivedPoset, mkDerivedPosetFromOrderEdges)
import Moonlight.Sheaf.Operator.GradedComplex
import Moonlight.Homology
  ( HomologicalDegree (..),
  )
import Moonlight.Sheaf.Operator.LinearBasis (linearBasisCells)
import Moonlight.Sheaf.Obstruction.Cohomological.Types.Core
  ( CandidateRegion (crNodeId),
    CandidateRegionSeed (crsNodeId),
  )
import Moonlight.Sheaf.Site.Construction.Nerve
  ( CellKey (..),
    NerveCell,
    NerveSite,
    faceMorphismSource,
    faceMorphismTarget,
    nerveCellKey,
    nerveSiteCells,
    siteFaceMorphisms,
  )
import Moonlight.Homology
  ( boundaryCoefficient,
    boundaryEntries,
    sourceCardinality,
    sourceIndex,
    targetCardinality,
    targetIndex,
  )

type MicrosupportEnrichment :: Type -> Type
data MicrosupportEnrichment node = MicrosupportEnrichment
  { meResult :: MicrosupportResult,
    mePoset :: DerivedPoset,
    meCriticalNodes :: Set.Set node
  }
  deriving stock (Eq, Show)

microsupportNodeFilter ::
  Ord node =>
  MicrosupportEnrichment node ->
  node ->
  Bool
microsupportNodeFilter enrichment =
  (`Set.member` meCriticalNodes enrichment)

microsupportCandidateRegionFilter ::
  MicrosupportEnrichment RegionNodeId ->
  CandidateRegion root ->
  Bool
microsupportCandidateRegionFilter enrichment regionValue =
  maybe False (microsupportNodeFilter enrichment) (crNodeId regionValue)

microsupportCandidateRegionSeedFilter ::
  MicrosupportEnrichment RegionNodeId ->
  CandidateRegionSeed root ->
  Bool
microsupportCandidateRegionSeedFilter enrichment seedValue =
  microsupportNodeFilter enrichment (crsNodeId seedValue)

nerveSiteToPoset :: NerveSite tag -> Either MoonlightError DerivedPoset
nerveSiteToPoset site =
  either (Left . derivedFailureToMoonlightError) Right
    ( mkDerivedPosetFromOrderEdges
        (fmap (FinObjectId . ckOrdinal . nerveCellKey) (nerveSiteCells site))
        ( Set.toAscList $
            Set.fromList
              ( fmap
                  ( \faceMorphism ->
                      ( FinObjectId (ckOrdinal (nerveCellKey (faceMorphismSource faceMorphism))),
                        FinObjectId (ckOrdinal (nerveCellKey (faceMorphismTarget faceMorphism)))
                      )
                  )
                  (siteFaceMorphisms site)
              )
        )
    )

computeMicrosupportEnrichment ::
  Ord node =>
  (cell -> FinObjectId) ->
  DerivedPoset ->
  (FinObjectId -> Maybe node) ->
  GradedComplex cell Int ->
  Either MoonlightError (MicrosupportEnrichment node)
computeMicrosupportEnrichment cellNode poset nodeProjection complex = do
  differential0 <-
    case gradedOperatorAt (HomologicalDegree 0) complex of
      Left _ ->
        Left (InvariantViolation "missing degree-zero differential for microsupport enrichment")
      Right concreteDifferential ->
        Right concreteDifferential
  initialDifferential <-
    first derivedFailureToMoonlightError
      ( entriesToBlockedMatGF2Checked
          cellNode
          cellNode
          (linearBasisCells (gradedOperatorTargetBasis differential0))
          (linearBasisCells (gradedOperatorSourceBasis differential0))
          (targetCardinality (gradedOperatorIncidence differential0))
          (sourceCardinality (gradedOperatorIncidence differential0))
          (boundaryEntries (gradedOperatorIncidence differential0))
          (\entryValue -> (targetIndex entryValue, sourceIndex entryValue))
          (odd . boundaryCoefficient)
      )
  result <- microsupportOfDifferential poset initialDifferential
  let criticalNodes =
        foldr
          ( \(nodeValue, criticalityValue) ->
              case criticalityValue of
                Critical ->
                  maybe id Set.insert (nodeProjection nodeValue)
                NonCritical ->
                  id
          )
          Set.empty
          (mrCriticalFibers result)
  pure
    MicrosupportEnrichment
      { meResult = result,
        mePoset = poset,
        meCriticalNodes = criticalNodes
      }

computeNerveMicrosupportEnrichment ::
  Ord node =>
  (FinObjectId -> Maybe node) ->
  NerveSite tag ->
  GradedComplex (NerveCell tag) Int ->
  Either MoonlightError (MicrosupportEnrichment node)
computeNerveMicrosupportEnrichment nodeProjection site cache = do
  poset <- nerveSiteToPoset site
  computeMicrosupportEnrichment
    (FinObjectId . ckOrdinal . nerveCellKey)
    poset
    nodeProjection
    cache
