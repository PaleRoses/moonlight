{-# LANGUAGE DataKinds #-}

module Moonlight.Analysis.Cohomology
  ( CoboundaryNilpotenceEvidence (..),
    evidenceNilpotent,
    CoboundaryDimensionModel (..),
    CohomologyModel (..),
    mkCoboundarySpecAt,
    buildCoboundaryComplexWith,
    buildHodgeLaplacian0With,
    buildHodgeLaplacian1With,
    coboundaryNilpotenceEvidence,
    coboundaryNilpotenceEvidenceFromResult,
  )
where

import Data.Bifunctor (first)
import Moonlight.Homology
  ( BoundaryIncidence,
    HomologicalDegree,
    incrementDegree,
  )
import Moonlight.Sheaf.Cochain.Coboundary
  ( CoboundarySpec (..),
    buildCoboundaryComplex,
  )
import Moonlight.Sheaf.Cochain.Laplacian
  ( LaplacianKind (HodgeLaplacian),
    SheafLaplacian,
    buildHodgeLaplacian0,
    buildHodgeLaplacian1,
  )
import Moonlight.Sheaf.Operator.BuildError
  ( SheafOperatorBuildError,
  )
import Moonlight.Sheaf.Operator.GradedComplex (GradedComplex)
import Moonlight.Sheaf.Kernel.Basis (SheafBasis)
import Moonlight.Sheaf.Section.Restriction (RestrictionIndex)
import Moonlight.Pale.Diagnostic.Site.Cohomology
  ( CoboundaryConstructionError (CoboundaryOperatorBuildError),
    CoboundaryNilpotenceEvidence (..),
    evidenceNilpotent,
  )

data CoboundaryDimensionModel site cell = CoboundaryDimensionModel
  { cdmBasisAtDimension :: HomologicalDegree -> site -> SheafBasis cell
  }

data CohomologyModel cell stalk witness = CohomologyModel
  { cmStalkAtCell :: cell -> stalk,
    cmStalkDimension :: stalk -> Int,
    cmCoboundaryBlock :: stalk -> stalk -> BoundaryIncidence Int,
    cmRestrictions :: RestrictionIndex cell witness
  }

mkCoboundarySpecAt ::
  CoboundaryDimensionModel site cell ->
  HomologicalDegree ->
  site ->
  CoboundarySpec cell
mkCoboundarySpecAt dimensionModel sourceDegree siteValue =
  CoboundarySpec
    { csDimension = sourceDegree,
      csSourceBasis = cdmBasisAtDimension dimensionModel sourceDegree siteValue,
      csTargetBasis = cdmBasisAtDimension dimensionModel (incrementDegree sourceDegree) siteValue
    }

buildCoboundaryComplexWith ::
  (Ord cell, Show cell) =>
  CohomologyModel cell stalk witness ->
  CoboundarySpec cell ->
  CoboundarySpec cell ->
  Either CoboundaryConstructionError (GradedComplex cell Int)
buildCoboundaryComplexWith =
  buildCohomologyArtifactWith Right

buildHodgeLaplacian0With ::
  (Ord cell, Show cell) =>
  CohomologyModel cell stalk witness ->
  CoboundarySpec cell ->
  CoboundarySpec cell ->
  Either CoboundaryConstructionError (SheafLaplacian 'HodgeLaplacian cell)
buildHodgeLaplacian0With =
  buildCohomologyArtifactWith buildHodgeLaplacian0

buildHodgeLaplacian1With ::
  (Ord cell, Show cell) =>
  CohomologyModel cell stalk witness ->
  CoboundarySpec cell ->
  CoboundarySpec cell ->
  Either CoboundaryConstructionError (SheafLaplacian 'HodgeLaplacian cell)
buildHodgeLaplacian1With =
  buildCohomologyArtifactWith buildHodgeLaplacian1

buildCohomologyArtifactWith ::
  (Ord cell, Show cell) =>
  (GradedComplex cell Int -> Either (SheafOperatorBuildError cell) artifact) ->
  CohomologyModel cell stalk witness ->
  CoboundarySpec cell ->
  CoboundarySpec cell ->
  Either CoboundaryConstructionError artifact
buildCohomologyArtifactWith buildArtifact cohomologyModel sourceSpec targetSpec =
  first sheafOperatorToConstructionError
    ( buildCoboundaryComplex
        (cmStalkAtCell cohomologyModel)
        (cmStalkDimension cohomologyModel)
        (cmCoboundaryBlock cohomologyModel)
        sourceSpec
        targetSpec
        (cmRestrictions cohomologyModel)
        >>= buildArtifact
    )

coboundaryNilpotenceEvidence ::
  Int ->
  GradedComplex cell Int ->
  CoboundaryNilpotenceEvidence
coboundaryNilpotenceEvidence contextCount _ =
  if contextCount <= 1
    then SingleContextNilpotent
    else MultiContextNilpotent

coboundaryNilpotenceEvidenceFromResult ::
  Int ->
  Either CoboundaryConstructionError (GradedComplex cell Int) ->
  CoboundaryNilpotenceEvidence
coboundaryNilpotenceEvidenceFromResult contextCount =
  either
    CoboundaryConstructionFailed
    (coboundaryNilpotenceEvidence contextCount)

sheafOperatorToConstructionError ::
  Show cell =>
  SheafOperatorBuildError cell ->
  CoboundaryConstructionError
sheafOperatorToConstructionError =
  CoboundaryOperatorBuildError . show
