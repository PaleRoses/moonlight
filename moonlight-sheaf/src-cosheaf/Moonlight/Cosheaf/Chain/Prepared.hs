{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Cosheaf.Chain.Prepared
  ( CosheafCoordinate (..),
    BoundaryTerm (..),
    PreparedCosheafBoundary,
    pcbDegree,
    pcbSourceBasis,
    pcbTargetBasis,
    pcbIncidence,
    PreparedCosheafChain,
    pccSite,
    pccMaxDegree,
    pccBasisByDegree,
    pccBoundaryByDegree,
    pccChainComplex,
    PreparedCosheafChainFailure (..),
    buildPreparedCosheafBoundary,
    mkPreparedCosheafChain,
    preparedCosheafBoundaryAt,
    preparedCosheafBasisAt,
    preparedCosheafBoundaryIncidenceAt,
    preparedCosheafBoundaryEntryProvenance,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Algebra
  ( AdditiveMonoid (..), Semiring,
  )
import Moonlight.Cosheaf.Chain.Coefficient
  ( CoefficientOps (..),
  )
import Moonlight.Cosheaf.Chain.Provenance
  ( ProvenanceArena,
    ProvenanceId,
    appendProvenance,
    emptyProvenanceArena,
    lookupProvenance,
  )
import Moonlight.Homology
  ( BoundaryEntry,
    BoundaryIncidence,
    BoundaryIncidenceShapeError,
    FiniteChainComplex,
    HomologicalDegree (..),
    HomologyFailure,
    boundaryCoefficient,
    boundaryEntries,
    composeBoundaryIncidence,
    emptyBoundaryIncidenceOf,
    mkBoundaryEntry,
    mkBoundaryIncidenceFromOrderedEntries,
    mkFiniteChainComplexChecked,
    sourceCardinality,
    sourceIndex,
    targetCardinality,
    targetIndex,
  )
import Moonlight.Sheaf.Operator.LinearBasis
  ( LinearBasis,
    linearBasisCardinality,
  )

type CosheafCoordinate :: Type -> Type
data CosheafCoordinate cell = CosheafCoordinate
  { cosheafCoordinateDegree :: !HomologicalDegree,
    cosheafCoordinateCell :: !cell,
    cosheafCoordinateLocalIndex :: !Int
  }
  deriving stock (Eq, Ord, Show)

type BoundaryTerm :: Type -> Type -> Type
data BoundaryTerm coefficient provenance = BoundaryTerm
  { boundaryTermSourceIndex :: !Int,
    boundaryTermTargetIndex :: !Int,
    boundaryTermCoefficient :: !coefficient,
    boundaryTermProvenance :: !provenance
  }
  deriving stock (Eq, Show)

type PreparedCosheafBoundary :: Type -> Type -> Type -> Type
data PreparedCosheafBoundary cell coefficient provenance = PreparedCosheafBoundary
  { preparedCosheafBoundaryDegreeInternal :: !HomologicalDegree,
    preparedCosheafBoundarySourceBasisInternal :: !(LinearBasis cell),
    preparedCosheafBoundaryTargetBasisInternal :: !(LinearBasis cell),
    preparedCosheafBoundaryIncidenceInternal :: !(BoundaryIncidence coefficient),
    preparedCosheafBoundaryEntryProvenanceInternal :: !(Map (Int, Int) ProvenanceId),
    preparedCosheafBoundaryProvenanceArenaInternal :: !(ProvenanceArena (NonEmpty provenance))
  }
  deriving stock (Eq, Show)

type PreparedCosheafChain :: Type -> Type -> Type -> Type -> Type
data PreparedCosheafChain site cell coefficient provenance = PreparedCosheafChain
  { preparedCosheafChainSiteInternal :: !site,
    preparedCosheafChainMaxDegreeInternal :: !HomologicalDegree,
    preparedCosheafChainBasisByDegreeInternal :: !(Map HomologicalDegree (LinearBasis cell)),
    preparedCosheafChainBoundaryByDegreeInternal :: !(Map HomologicalDegree (PreparedCosheafBoundary cell coefficient provenance)),
    preparedCosheafChainComplexInternal :: !(FiniteChainComplex coefficient)
  }

pcbDegree :: PreparedCosheafBoundary cell coefficient provenance -> HomologicalDegree
pcbDegree =
  preparedCosheafBoundaryDegreeInternal

pcbSourceBasis :: PreparedCosheafBoundary cell coefficient provenance -> LinearBasis cell
pcbSourceBasis =
  preparedCosheafBoundarySourceBasisInternal

pcbTargetBasis :: PreparedCosheafBoundary cell coefficient provenance -> LinearBasis cell
pcbTargetBasis =
  preparedCosheafBoundaryTargetBasisInternal

pcbIncidence :: PreparedCosheafBoundary cell coefficient provenance -> BoundaryIncidence coefficient
pcbIncidence =
  preparedCosheafBoundaryIncidenceInternal

pccSite :: PreparedCosheafChain site cell coefficient provenance -> site
pccSite =
  preparedCosheafChainSiteInternal

pccMaxDegree :: PreparedCosheafChain site cell coefficient provenance -> HomologicalDegree
pccMaxDegree =
  preparedCosheafChainMaxDegreeInternal

pccBasisByDegree ::
  PreparedCosheafChain site cell coefficient provenance ->
  Map HomologicalDegree (LinearBasis cell)
pccBasisByDegree =
  preparedCosheafChainBasisByDegreeInternal

pccBoundaryByDegree ::
  PreparedCosheafChain site cell coefficient provenance ->
  Map HomologicalDegree (PreparedCosheafBoundary cell coefficient provenance)
pccBoundaryByDegree =
  preparedCosheafChainBoundaryByDegreeInternal

pccChainComplex :: PreparedCosheafChain site cell coefficient provenance -> FiniteChainComplex coefficient
pccChainComplex =
  preparedCosheafChainComplexInternal

type PreparedCosheafChainFailure :: Type -> Type -> Type
data PreparedCosheafChainFailure cell coefficient
  = PreparedCosheafChainBasisMissing !HomologicalDegree
  | PreparedCosheafChainBoundaryMissing !HomologicalDegree
  | PreparedCosheafChainBoundaryShapeFailed !BoundaryIncidenceShapeError
  | PreparedCosheafChainBoundaryShapeMismatch
      !HomologicalDegree
      !Int
      !Int
      !Int
      !Int
  | PreparedCosheafChainBoundaryIndexOutOfBounds
      !HomologicalDegree
      !Int
      !Int
      !Int
      !Int
  | PreparedCosheafChainBoundaryNilpotenceFailed
      !HomologicalDegree
      !HomologicalDegree
      !Int
      !Int
      !coefficient
  | PreparedCosheafChainBoundaryProvenanceMissing
      !HomologicalDegree
      !Int
      !Int
      !ProvenanceId
  | PreparedCosheafChainComplexFailed !HomologyFailure
  deriving stock (Eq, Show)

data CombinedBoundaryTerm coefficient provenance = CombinedBoundaryTerm
  { cbtSourceIndex :: !Int,
    cbtTargetIndex :: !Int,
    cbtCoefficient :: !coefficient,
    cbtProvenance :: !(NonEmpty provenance)
  }

buildPreparedCosheafBoundary ::
  (Eq coefficient, Semiring coefficient) =>
  CoefficientOps coefficient ->
  HomologicalDegree ->
  LinearBasis cell ->
  LinearBasis cell ->
  [BoundaryTerm coefficient provenance] ->
  Either
    (PreparedCosheafChainFailure cell coefficient)
    (PreparedCosheafBoundary cell coefficient provenance)
buildPreparedCosheafBoundary coefficientOps degreeValue sourceBasis targetBasis rawTerms = do
  traverse_ validateRawTerm rawTerms
  let combinedTerms =
        combineBoundaryTerms coefficientOps rawTerms
  incidence <-
    first PreparedCosheafChainBoundaryShapeFailed $
      mkBoundaryIncidenceFromOrderedEntries
        (fromIntegral sourceCount)
        (fromIntegral targetCount)
        (fmap combinedTermToBoundaryEntry combinedTerms)
  let (entryProvenanceValue, arenaValue) =
        provenanceMapAndArena combinedTerms
  pure
    PreparedCosheafBoundary
      { preparedCosheafBoundaryDegreeInternal = degreeValue,
        preparedCosheafBoundarySourceBasisInternal = sourceBasis,
        preparedCosheafBoundaryTargetBasisInternal = targetBasis,
        preparedCosheafBoundaryIncidenceInternal = incidence,
        preparedCosheafBoundaryEntryProvenanceInternal = entryProvenanceValue,
        preparedCosheafBoundaryProvenanceArenaInternal = arenaValue
      }
  where
    sourceCount =
      linearBasisCardinality sourceBasis

    targetCount =
      linearBasisCardinality targetBasis

    validateRawTerm term =
      let sourceIndexValue = boundaryTermSourceIndex term
          targetIndexValue = boundaryTermTargetIndex term
       in if sourceIndexValue < 0
            || sourceIndexValue >= sourceCount
            || targetIndexValue < 0
            || targetIndexValue >= targetCount
            then
              Left
                ( PreparedCosheafChainBoundaryIndexOutOfBounds
                    degreeValue
                    sourceIndexValue
                    targetIndexValue
                    sourceCount
                    targetCount
                )
            else Right ()

combinedTermToBoundaryEntry ::
  CombinedBoundaryTerm coefficient provenance ->
  BoundaryEntry coefficient
combinedTermToBoundaryEntry term =
  mkBoundaryEntry
    (fromIntegral (cbtSourceIndex term))
    (fromIntegral (cbtTargetIndex term))
    (cbtCoefficient term)

combineBoundaryTerms ::
  Semiring coefficient =>
  CoefficientOps coefficient ->
  [BoundaryTerm coefficient provenance] ->
  [CombinedBoundaryTerm coefficient provenance]
combineBoundaryTerms coefficientOps =
  filter (not . coIsZero coefficientOps . cbtCoefficient)
    . fmap (uncurry combinedBoundaryTermFromMapEntry)
    . Map.toAscList
    . Map.fromListWith mergeBoundaryTermPayload
    . fmap boundaryTermMapEntry

boundaryTermMapEntry ::
  BoundaryTerm coefficient provenance ->
  ((Int, Int), (coefficient, NonEmpty provenance))
boundaryTermMapEntry term =
  ( (boundaryTermSourceIndex term, boundaryTermTargetIndex term),
    (boundaryTermCoefficient term, boundaryTermProvenance term :| [])
  )

mergeBoundaryTermPayload ::
  Semiring coefficient =>
  (coefficient, NonEmpty provenance) ->
  (coefficient, NonEmpty provenance) ->
  (coefficient, NonEmpty provenance)
mergeBoundaryTermPayload (leftCoefficient, leftProvenance) (rightCoefficient, rightProvenance) =
  (add leftCoefficient rightCoefficient, leftProvenance <> rightProvenance)

combinedBoundaryTermFromMapEntry ::
  (Int, Int) ->
  (coefficient, NonEmpty provenance) ->
  CombinedBoundaryTerm coefficient provenance
combinedBoundaryTermFromMapEntry (sourceIndexValue, targetIndexValue) (coefficientValue, provenanceValue) =
  CombinedBoundaryTerm
    { cbtSourceIndex = sourceIndexValue,
      cbtTargetIndex = targetIndexValue,
      cbtCoefficient = coefficientValue,
      cbtProvenance = provenanceValue
    }

provenanceMapAndArena ::
  [CombinedBoundaryTerm coefficient provenance] ->
  (Map (Int, Int) ProvenanceId, ProvenanceArena (NonEmpty provenance))
provenanceMapAndArena =
  foldl' insertProvenanceTerm (Map.empty, emptyProvenanceArena)

insertProvenanceTerm ::
  (Map (Int, Int) ProvenanceId, ProvenanceArena (NonEmpty provenance)) ->
  CombinedBoundaryTerm coefficient provenance ->
  (Map (Int, Int) ProvenanceId, ProvenanceArena (NonEmpty provenance))
insertProvenanceTerm (entryMap, arena) term =
  let (provenanceId, arena') =
        appendProvenance (cbtProvenance term) arena
      entryKey =
        (cbtSourceIndex term, cbtTargetIndex term)
   in (Map.insert entryKey provenanceId entryMap, arena')

mkPreparedCosheafChain ::
  (Eq coefficient, Num coefficient, Semiring coefficient) =>
  CoefficientOps coefficient ->
  site ->
  HomologicalDegree ->
  Map HomologicalDegree (LinearBasis cell) ->
  Map HomologicalDegree (PreparedCosheafBoundary cell coefficient provenance) ->
  Either
    (PreparedCosheafChainFailure cell coefficient)
    (PreparedCosheafChain site cell coefficient provenance)
mkPreparedCosheafChain coefficientOps siteValue maxDegree basisByDegree boundaryByDegree = do
  traverse_ requireBasis degrees
  traverse_ validateBoundaryDegree positiveDegrees
  traverse_ validateNilpotence positiveDegrees
  chainComplex <-
    first PreparedCosheafChainComplexFailed $
      mkFiniteChainComplexChecked
        maxDegree
        boundaryIncidenceForComplex
  pure
    PreparedCosheafChain
      { preparedCosheafChainSiteInternal = siteValue,
        preparedCosheafChainMaxDegreeInternal = maxDegree,
        preparedCosheafChainBasisByDegreeInternal = basisByDegree,
        preparedCosheafChainBoundaryByDegreeInternal = boundaryByDegree,
        preparedCosheafChainComplexInternal = chainComplex
      }
  where
    HomologicalDegree maxDegreeInt =
      maxDegree

    degrees =
      fmap HomologicalDegree [0 .. maxDegreeInt]

    positiveDegrees =
      fmap HomologicalDegree [1 .. maxDegreeInt]

    requireBasis degreeValue =
      maybe
        (Left (PreparedCosheafChainBasisMissing degreeValue))
        (const (Right ()))
        (Map.lookup degreeValue basisByDegree)

    basisCardinalityAt degreeValue =
      maybe 0 linearBasisCardinality (Map.lookup degreeValue basisByDegree)

    boundaryIncidenceForComplex degreeValue@(HomologicalDegree degreeInt)
      | degreeInt <= 0 =
          emptyBoundaryIncidenceOf
            (fromIntegral (basisCardinalityAt (HomologicalDegree 0)))
            0
      | otherwise =
          maybe
            ( emptyBoundaryIncidenceOf
                (fromIntegral (basisCardinalityAt degreeValue))
                (fromIntegral (basisCardinalityAt (HomologicalDegree (degreeInt - 1))))
            )
            pcbIncidence
            (Map.lookup degreeValue boundaryByDegree)

    validateBoundaryDegree degreeValue@(HomologicalDegree degreeInt) = do
      boundaryValue <-
        maybe
          (Left (PreparedCosheafChainBoundaryMissing degreeValue))
          Right
          (Map.lookup degreeValue boundaryByDegree)
      sourceBasis <-
        maybe
          (Left (PreparedCosheafChainBasisMissing degreeValue))
          Right
          (Map.lookup degreeValue basisByDegree)
      targetBasis <-
        maybe
          (Left (PreparedCosheafChainBasisMissing (HomologicalDegree (degreeInt - 1))))
          Right
          (Map.lookup (HomologicalDegree (degreeInt - 1)) basisByDegree)
      let expectedSource = linearBasisCardinality sourceBasis
          expectedTarget = linearBasisCardinality targetBasis
          actualSource = sourceCardinality (pcbIncidence boundaryValue)
          actualTarget = targetCardinality (pcbIncidence boundaryValue)
      if expectedSource == actualSource && expectedTarget == actualTarget
        then Right ()
        else
          Left
            ( PreparedCosheafChainBoundaryShapeMismatch
                degreeValue
                expectedSource
                expectedTarget
                actualSource
                actualTarget
            )

    validateNilpotence rightDegree@(HomologicalDegree rightDegreeInt) = do
      let leftDegree =
            HomologicalDegree (rightDegreeInt - 1)
      composite <-
        first PreparedCosheafChainBoundaryShapeFailed $
          composeBoundaryIncidence
            (boundaryIncidenceForComplex leftDegree)
            (boundaryIncidenceForComplex rightDegree)
      case firstNonZeroBoundaryEntry coefficientOps composite of
        Nothing ->
          Right ()
        Just witness ->
          Left
            ( PreparedCosheafChainBoundaryNilpotenceFailed
                rightDegree
                leftDegree
                (sourceIndex witness)
                (targetIndex witness)
                (boundaryCoefficient witness)
            )

firstNonZeroBoundaryEntry ::
  CoefficientOps coefficient ->
  BoundaryIncidence coefficient ->
  Maybe (BoundaryEntry coefficient)
firstNonZeroBoundaryEntry coefficientOps =
  findFirst (not . coIsZero coefficientOps . boundaryCoefficient) . boundaryEntries

findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst predicate =
  foldr
    ( \value rest ->
        if predicate value
          then Just value
          else rest
    )
    Nothing

preparedCosheafBoundaryAt ::
  HomologicalDegree ->
  PreparedCosheafChain site cell coefficient provenance ->
  Maybe (PreparedCosheafBoundary cell coefficient provenance)
preparedCosheafBoundaryAt degreeValue =
  Map.lookup degreeValue . pccBoundaryByDegree

preparedCosheafBasisAt ::
  HomologicalDegree ->
  PreparedCosheafChain site cell coefficient provenance ->
  Maybe (LinearBasis cell)
preparedCosheafBasisAt degreeValue =
  Map.lookup degreeValue . pccBasisByDegree

preparedCosheafBoundaryIncidenceAt ::
  HomologicalDegree ->
  PreparedCosheafChain site cell coefficient provenance ->
  BoundaryIncidence coefficient
preparedCosheafBoundaryIncidenceAt degreeValue@(HomologicalDegree degreeInt) chain
  | degreeInt <= 0 =
      emptyBoundaryIncidenceOf
        (fromIntegral (maybe 0 linearBasisCardinality (preparedCosheafBasisAt (HomologicalDegree 0) chain)))
        0
  | otherwise =
      maybe
        ( emptyBoundaryIncidenceOf
            (fromIntegral (maybe 0 linearBasisCardinality (preparedCosheafBasisAt degreeValue chain)))
            (fromIntegral (maybe 0 linearBasisCardinality (preparedCosheafBasisAt (HomologicalDegree (degreeInt - 1)) chain)))
        )
        pcbIncidence
        (preparedCosheafBoundaryAt degreeValue chain)

preparedCosheafBoundaryEntryProvenance ::
  HomologicalDegree ->
  Int ->
  Int ->
  PreparedCosheafChain site cell coefficient provenance ->
  Either
    (PreparedCosheafChainFailure cell coefficient)
    (NonEmpty provenance)
preparedCosheafBoundaryEntryProvenance degreeValue sourceIndexValue targetIndexValue chain = do
  boundaryValue <-
    maybe
      (Left (PreparedCosheafChainBoundaryMissing degreeValue))
      Right
      (preparedCosheafBoundaryAt degreeValue chain)
  provenanceId <-
    maybe
      (Left (PreparedCosheafChainBoundaryMissing degreeValue))
      Right
      (Map.lookup (sourceIndexValue, targetIndexValue) (preparedCosheafBoundaryEntryProvenanceInternal boundaryValue))
  maybe
    (Left (PreparedCosheafChainBoundaryProvenanceMissing degreeValue sourceIndexValue targetIndexValue provenanceId))
    Right
    (lookupProvenance provenanceId (preparedCosheafBoundaryProvenanceArenaInternal boundaryValue))
