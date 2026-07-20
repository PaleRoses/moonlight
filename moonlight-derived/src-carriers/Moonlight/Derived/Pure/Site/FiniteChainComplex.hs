module Moonlight.Derived.Pure.Site.FiniteChainComplex
  ( derivedFromFiniteChainComplex,
    injectiveComplexFromFiniteChainComplex,
  )
where

import Data.Vector qualified as V
import Moonlight.Derived.Pure.Failure (DerivedFailure)
import Moonlight.Derived.Pure.Site.InjectiveComplex
  ( Derived,
    InjectiveComplex (..),
    mkNormalizedDerivedChecked,
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix
  ( BlockedMat,
    entriesToBlockedMatGF2,
  )
import Moonlight.Derived.Pure.Site.Poset
  ( FinObjectId (..),
    mkDerivedPosetFromOrderEdges,
  )
import Moonlight.Homology
  ( BasisCellRef (..),
    BoundaryEntry,
    BoundaryIncidence,
    FiniteChainComplex,
    HomologicalDegree (..),
    basisCellNodeId,
    boundaryCoefficient,
    boundaryEntries,
    degreeCardinality,
    emptyBoundaryIncidenceOf,
    finiteChainBasisRefsAtDegree,
    incidenceMatrixAt,
    incrementDegree,
    maxHomologicalDegree,
    sourceCardinality,
    sourceIndex,
    targetCardinality,
    targetIndex,
  )
import Moonlight.LinAlg (GF2)

derivedFromFiniteChainComplex :: Integral r => FiniteChainComplex r -> Either DerivedFailure (Derived GF2)
derivedFromFiniteChainComplex finite = do
  incidencePoset <-
    mkDerivedPosetFromOrderEdges
      (finiteChainObjectIds finite)
      (finiteChainIncidenceOrderEdges finite)
  mkNormalizedDerivedChecked incidencePoset (injectiveComplexFromFiniteChainComplex finite)

injectiveComplexFromFiniteChainComplex :: Integral r => FiniteChainComplex r -> InjectiveComplex GF2
injectiveComplexFromFiniteChainComplex finite =
  case maxHomologicalDegree finite of
    HomologicalDegree maxDegreeValue ->
      InjectiveComplex
          { icStart = 0,
            icDiffs =
              V.fromList
                ( fmap
                    (cochainDifferentialFromFiniteChainComplexGF2 finite . HomologicalDegree)
                    [0 .. maxDegreeValue]
                )
          }

finiteChainObjectIds :: FiniteChainComplex r -> [FinObjectId]
finiteChainObjectIds finite =
  fmap (FinObjectId . basisCellNodeId finite)
    ( concatMap
        (finiteChainBasisRefsAtDegree finite . HomologicalDegree)
        [0 .. maximumDegreeValue]
    )
  where
    HomologicalDegree maximumDegreeValue = maxHomologicalDegree finite

finiteChainIncidenceOrderEdges :: Integral r => FiniteChainComplex r -> [(FinObjectId, FinObjectId)]
finiteChainIncidenceOrderEdges finite =
  concatMap
    (incidenceOrderEdgesAtDegree finite . HomologicalDegree)
    [1 .. maximumDegreeValue]
  where
    HomologicalDegree maximumDegreeValue = maxHomologicalDegree finite

incidenceOrderEdgesAtDegree :: Integral r => FiniteChainComplex r -> HomologicalDegree -> [(FinObjectId, FinObjectId)]
incidenceOrderEdgesAtDegree finite sourceDegree =
  fmap (incidenceOrderEdge finite sourceDegree)
    ( filter
        boundaryCoefficientIsOdd
        (boundaryEntries (incidenceMatrixAt finite sourceDegree))
    )

incidenceOrderEdge :: FiniteChainComplex r -> HomologicalDegree -> BoundaryEntry r -> (FinObjectId, FinObjectId)
incidenceOrderEdge finite sourceDegree@(HomologicalDegree sourceDegreeValue) entryValue =
  ( FinObjectId
      ( basisCellNodeId
          finite
          BasisCellRef
            { cellDegree = sourceDegree,
              cellIndex = sourceIndex entryValue
            }
      ),
    FinObjectId
      ( basisCellNodeId
          finite
          BasisCellRef
            { cellDegree = HomologicalDegree (sourceDegreeValue - 1),
              cellIndex = targetIndex entryValue
            }
      )
  )

finiteCochainIncidenceGF2 :: FiniteChainComplex r -> HomologicalDegree -> BoundaryIncidence r
finiteCochainIncidenceGF2 finite degreeValue =
  if degreeValue == maxHomologicalDegree finite
    then
      emptyBoundaryIncidenceOf
        0
        (fromIntegral (degreeCardinality finite degreeValue))
    else incidenceMatrixAt finite (incrementDegree degreeValue)

cochainDifferentialFromFiniteChainComplexGF2 ::
  Integral r =>
  FiniteChainComplex r ->
  HomologicalDegree ->
  BlockedMat GF2
cochainDifferentialFromFiniteChainComplexGF2 finite degreeValue =
  entriesToBlockedMatGF2
    (FinObjectId . basisCellNodeId finite)
    (FinObjectId . basisCellNodeId finite)
    (finiteChainBasisRefsAtDegree finite (incrementDegree degreeValue))
    (finiteChainBasisRefsAtDegree finite degreeValue)
    (sourceCardinality cochainIncidence)
    (targetCardinality cochainIncidence)
    (boundaryEntries cochainIncidence)
    (\entryValue -> (sourceIndex entryValue, targetIndex entryValue))
    boundaryCoefficientIsOdd
  where
    cochainIncidence = finiteCochainIncidenceGF2 finite degreeValue

boundaryCoefficientIsOdd :: Integral r => BoundaryEntry r -> Bool
boundaryCoefficientIsOdd =
  odd . abs . boundaryCoefficient
