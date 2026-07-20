{-# LANGUAGE RankNTypes #-}

module Moonlight.Sheaf.Obstruction.Cohomological.Analysis.Lift
  ( buildCohomologicalLift,
    morphismCells,
    restrictionIndexFor,
    orientedRestrictionIndexFor,
    degreeToInt,
  )
where

import Data.Bifunctor (first)
import Data.Function ((&))
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.GADT.Compare (GCompare)
import Moonlight.Homology
  ( BoundaryIncidence,
    HomologicalDegree (..),
    emptyBoundaryIncidenceOf,
    mkFiniteChainComplexChecked,
  )
import Moonlight.Sheaf.Cochain.Coboundary
  ( CoboundarySpec (..),
    buildCoboundary,
    checkCoboundaryNilpotence,
    materializeCoboundaryDifferential,
  )
import Moonlight.Sheaf.Kernel.Basis
  ( SheafBasis,
    mkSheafBasis,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Algebra
  ( boundaryAt,
    buildCycleLayer,
    buildOneLayer,
    supportCellsFromBasis,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Analysis.Support
  ( buildCohomologicalRegionSupport,
    validateOccurrenceDomains,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Core.Environment
  ( ObstructionEnvironmentAlgebra (..),
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Evidence.Pruning
  ( RetainedCohomologicalRegion (rcrRegion),
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Substrate
  ( CohomologicalLift (..),
    CohomologicalRegionSupport (..),
    CohomologicalSubstrate (..),
    SubstrateWitness,
    modalityCoverageWitness,
    obstructionWitnessFor,
    substrateCanonicalRoot,
    substrateCollectGuards,
    substrateEnvironment,
    substrateOccurrenceId,
    substrateRequestQuery,
    substrateRootKey,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Types
  ( CandidateRegion (..),
    ExactLabelCode (..),
    ExpandedMorphism (..),
    ExpandedObstructionCell (..),
    ExpandedStalk (..),
    ObstructionLift (..),
    ObstructionReason (..),
    zeroCellForAnchor,
  )
import Moonlight.Sheaf.Operator.BuildError (SheafOperatorBuildError)
import Moonlight.Sheaf.Operator.GradedComplex
import Moonlight.Homology
  ( HomologicalDegree (..),
  )
import Moonlight.Sheaf.Section.Linearize (identityBoundaryIncidence)
import Moonlight.Sheaf.Kernel.Basis (basisCells)
import Moonlight.Sheaf.Section.ObjectIndex (mkObjectIndex)
import Moonlight.Sheaf.Section.Morphism
  ( RestrictionParts (..),
    RestrictionPresentation,
    mkIncidenceRestriction,
    unitIncidenceRestriction,
  )
import Moonlight.Sheaf.Section.Restriction
  ( RestrictionIndex,
    RestrictionIndexError (..),
    buildRestrictionIndex,
  )
import Moonlight.Pale.Diagnostic.Site.Cohomology
  ( CoboundaryConstructionError (CoboundaryOperatorBuildError),
  )

-- | Convert a HomologicalDegree to its Int index.
degreeToInt :: HomologicalDegree -> Int
degreeToInt (HomologicalDegree n) =
  n

-- | Extract source and target cells from a morphism.
morphismCells :: ExpandedMorphism -> [ExpandedObstructionCell]
morphismCells morphism =
  [emSource morphism, emTarget morphism]

-- | Build a restriction index for a flat (unoriented) set of morphisms.
restrictionIndexFor ::
  SheafBasis ExpandedObstructionCell ->
  [ExpandedMorphism] ->
  Either
    (RestrictionIndexError ExpandedObstructionCell)
    (RestrictionIndex ExpandedObstructionCell ExpandedMorphism)
restrictionIndexFor basis morphisms =
  buildRestrictionIndex
    (mkObjectIndex (basisCells basis))
    presentExpandedMorphism
    morphisms

presentExpandedMorphism :: RestrictionPresentation ExpandedMorphism ExpandedObstructionCell ExpandedMorphism
presentExpandedMorphism morphism =
  RestrictionParts
    { partKind = unitIncidenceRestriction,
      partSource = emSource morphism,
      partTarget = emTarget morphism,
      partWitness = morphism
    }

-- | Build a restriction index for an oriented set of morphisms.
orientedRestrictionIndexFor ::
  SheafBasis ExpandedObstructionCell ->
  Map (ExpandedObstructionCell, ExpandedObstructionCell) Int ->
  [ExpandedMorphism] ->
  Either
    (RestrictionIndexError ExpandedObstructionCell)
    (RestrictionIndex ExpandedObstructionCell ExpandedMorphism)
orientedRestrictionIndexFor basis orientation morphisms = do
  preparedMorphisms <- traverse prepareMorphism morphisms
  buildRestrictionIndex
    (mkObjectIndex (basisCells basis))
    ( \(restrictionKind, morphism) ->
        RestrictionParts
          { partKind = restrictionKind,
            partSource = emSource morphism,
            partTarget = emTarget morphism,
            partWitness = morphism
          }
    )
    preparedMorphisms
  where
    prepareMorphism morphism =
      case mkIncidenceRestriction (orientationFor morphism) of
        Just restrictionKind ->
          Right (restrictionKind, morphism)
        Nothing ->
          Left (RestrictionZeroIncidenceCoefficient (emSource morphism) (emTarget morphism))

    orientationFor morphism =
      Map.findWithDefault
        1
        (emSource morphism, emTarget morphism)
        orientation

-- | Build the full cohomological lift for a region.
buildCohomologicalLift ::
  ( CohomologicalSubstrate substrate,
    GCompare (SubstrateModalityKey substrate runtime)
  ) =>
  substrate ->
  SubstrateRequest substrate runtime ->
  RetainedCohomologicalRegion (SubstrateRoot substrate) ->
  Either (SubstrateWitness substrate) (CohomologicalLift substrate)
buildCohomologicalLift substrate request retainedRegion = do
  let region =
        rcrRegion retainedRegion

  let query =
        substrateRequestQuery substrate request

      envAlgebra =
        substrateEnvironment substrate

      occurrences =
        oeaCollectOccurrences envAlgebra query

      guards =
        substrateCollectGuards substrate query

  support <-
    first
      (modalityCoverageWitness substrate request region)
      (buildCohomologicalRegionSupport substrate request region occurrences guards)

  validateOccurrenceDomains
    substrate
    request
    region
    (\cells reason ->
       obstructionWitnessFor substrate request region (crRoot region) cells reason 0 0 0)
    support

  let root =
        substrateCanonicalRoot substrate request (crRoot region)

      rootKey =
        substrateRootKey substrate request root

      rootCode =
        ClassLabelCode rootKey

      zeroBasis =
        mkSheafBasis
          ( ExpandedRootCell rootCode :
            foldMap
              (\occurrence ->
                 Map.findWithDefault IntSet.empty (substrateOccurrenceId substrate occurrence) (crsOccurrenceDomains support)
                   & IntSet.toAscList
                   & fmap
                     ( ExpandedOccurrenceCell
                         (substrateOccurrenceId substrate occurrence)
                         . ClassLabelCode
                     )
              )
              (crsOccurrences support)
          )

      (oneCells, morphisms01, orientation01) =
        buildOneLayer
          (zeroCellForAnchor rootCode)
          (crsExactConstraints support)

      (cycleCells, morphisms12, orientation12) =
        buildCycleLayer morphisms01

      oneBasis =
        mkSheafBasis oneCells

      twoBasis =
        mkSheafBasis cycleCells

      supportCells =
        supportCellsFromBasis zeroBasis
          <> supportCellsFromBasis oneBasis
          <> supportCellsFromBasis twoBasis

      malformed constructionError =
        obstructionWitnessFor
          substrate
          request
          region
          root
          supportCells
          (MalformedCohomologyComplex constructionError)
          0
          0
          0

  oriented01 <-
    first
      (malformed . sheafOperatorToConstructionError)
      (orientedRestrictionIndexFor
        (mkSheafBasis (foldMap morphismCells morphisms01))
        orientation01
        morphisms01)

  oriented12 <-
    first
      (malformed . sheafOperatorToConstructionError)
      (orientedRestrictionIndexFor
        (mkSheafBasis (foldMap morphismCells morphisms12))
        orientation12
        morphisms12)

  restrictions <-
    first
      (malformed . sheafOperatorToConstructionError)
      (restrictionIndexFor
        (mkSheafBasis (foldMap morphismCells (morphisms01 <> morphisms12)))
        (morphisms01 <> morphisms12))

  cochainComplex <-
    first
      malformed
      (buildExpandedCochainComplex zeroBasis oneBasis twoBasis oriented01 oriented12)

  boundaryForDegree <-
    first
      (malformed . sheafOperatorToConstructionError)
      (boundaryForExpandedDegree zeroBasis cochainComplex)

  finiteComplex <-
    first (malformed . CoboundaryOperatorBuildError . show) $
      mkFiniteChainComplexChecked
        (HomologicalDegree 2)
        boundaryForDegree

  let exactEligible =
        null (crsExactLoweringGaps support)
          && checkCoboundaryNilpotence cochainComplex

      obstructionLift =
        ObstructionLift
          { olRegion = region,
            olExpandedComplex = finiteComplex,
            olRestrictions = restrictions,
            olCoboundaryCache = cochainComplex,
            olRoot = root,
            olExactH1Eligible = exactEligible
          }

  Right
    CohomologicalLift
      { clQuery = query,
        clOccurrences = crsOccurrences support,
        clOccurrenceDomains = crsOccurrenceDomains support,
        clGuards = crsGuards support,
        clExactConstraints = crsExactConstraints support,
        clExactLoweringGaps = crsExactLoweringGaps support,
        clSectionReification = crsSectionReification support,
        clSupportEvidence = crsEvidence support,
        clZeroBasis = zeroBasis,
        clOneBasis = oneBasis,
        clTwoBasis = twoBasis,
        clSupportCells = supportCells,
        clObstructionLift = obstructionLift
      }

-- Internal: build the cochain complex from three bases and two oriented indices.

boundaryForExpandedDegree ::
  SheafBasis ExpandedObstructionCell ->
  GradedComplex ExpandedObstructionCell Int ->
  Either
    (SheafOperatorBuildError ExpandedObstructionCell)
    (HomologicalDegree -> BoundaryIncidence Integer)
boundaryForExpandedDegree zeroBasis cochainComplex =
  boundaryByDegree
    <$> boundaryAt zeroBasis cochainComplex 0
    <*> boundaryAt zeroBasis cochainComplex 1
    <*> boundaryAt zeroBasis cochainComplex 2

boundaryByDegree ::
  BoundaryIncidence Integer ->
  BoundaryIncidence Integer ->
  BoundaryIncidence Integer ->
  HomologicalDegree ->
  BoundaryIncidence Integer
boundaryByDegree boundary0 boundary1 boundary2 degree =
  case degreeToInt degree of
    0 ->
      boundary0
    1 ->
      boundary1
    2 ->
      boundary2
    _ ->
      emptyBoundaryIncidenceOf 0 0

buildExpandedCochainComplex ::
  SheafBasis ExpandedObstructionCell ->
  SheafBasis ExpandedObstructionCell ->
  SheafBasis ExpandedObstructionCell ->
  RestrictionIndex ExpandedObstructionCell ExpandedMorphism ->
  RestrictionIndex ExpandedObstructionCell ExpandedMorphism ->
  Either CoboundaryConstructionError (GradedComplex ExpandedObstructionCell Int)
buildExpandedCochainComplex zeroBasis oneBasis twoBasis restrictionRegistry01 restrictionRegistry12 =
  first sheafOperatorToConstructionError $ do
    differential0 <-
      buildExpandedDifferential
        CoboundarySpec
          { csDimension = (HomologicalDegree 0),
            csSourceBasis = zeroBasis,
            csTargetBasis = oneBasis
          }
        restrictionRegistry01
    differential1 <-
      buildExpandedDifferential
        CoboundarySpec
          { csDimension = (HomologicalDegree 1),
            csSourceBasis = oneBasis,
            csTargetBasis = twoBasis
          }
        restrictionRegistry12
    mkGradedComplexFromList DegreeIncreasing [differential0, differential1]
  where
    buildExpandedDifferential ::
      CoboundarySpec ExpandedObstructionCell ->
      RestrictionIndex ExpandedObstructionCell ExpandedMorphism ->
      Either
        (SheafOperatorBuildError ExpandedObstructionCell)
        (GradedOperator ExpandedObstructionCell Int)
    buildExpandedDifferential spec registry = do
      coboundaryMatrix <- buildCoboundary spec registry
      materializeCoboundaryDifferential
        (const (ExpandedStalk ()))
        (const 1)
        (\_ _ -> identityBoundaryIncidence 1)
        coboundaryMatrix

sheafOperatorToConstructionError ::
  Show err =>
  err ->
  CoboundaryConstructionError
sheafOperatorToConstructionError =
  CoboundaryOperatorBuildError . show
