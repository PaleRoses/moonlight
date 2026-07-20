{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Cosheaf.Reduction.Morse
  ( CosheafMorsePolicy (..),
    defaultCosheafMorsePolicy,
    CosheafMorsePair (..),
    CosheafMorseMatching (..),
    CosheafMorseHomologyAgreement (..),
    MorseProvenance (..),
    CosheafMorseReduction (..),
    CosheafMorseFailure (..),
    morseReduceCosheafChain,
  )
where

import Data.Bifunctor (first)
import Data.IntMap.Strict (IntMap)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Algebra
  ( Semiring,
  )
import Moonlight.Cosheaf.Chain.Coefficient
  ( CoefficientOps (..),
    PivotOps (..),
  )
import Moonlight.Cosheaf.Chain.Prepared
  ( BoundaryTerm (..),
    CosheafCoordinate (..),
    PreparedCosheafBoundary,
    PreparedCosheafChain,
    PreparedCosheafChainFailure (..),
    buildPreparedCosheafBoundary,
    mkPreparedCosheafChain,
    pccBasisByDegree,
    pccChainComplex,
    pccSite,
  )
import Moonlight.Cosheaf.Homology.Linear
  ( LinearCosheafHomologyArtifact (..),
    LinearCosheafHomologyFailure,
    linearCosheafHomology,
  )
import Moonlight.Homology
  ( AlgebraicMorseComplex,
    AlgebraicMorseMatching,
    AlgebraicMorsePair,
    BasisCellRef (..),
    BoundaryIncidence,
    FiniteChainComplex,
    HomologyBackend,
    HomologyBackendTag,
    HomologicalDegree (..),
    HomologyFailure,
    HomologyGroup,
    MorsePivotOps (..),
    acyclicMatchingWith,
    boundaryCoefficient,
    boundaryEntries,
    finiteChainBasisRefsAtDegree,
    homologyBackendTag,
    incidenceMatrixAt,
    lapIncidenceCoefficient,
    lapLowerCell,
    lapUpperCell,
    lamCriticalCells,
    lamPairs,
    lmcCriticalBasis,
    lmcReducedComplex,
    maxHomologicalDegree,
    morseComplexWith,
    sourceIndex,
    targetIndex,
  )
import Moonlight.Sheaf.Kernel.Basis
  ( mkSheafBasis,
  )
import Moonlight.Sheaf.Operator.BuildError
  ( SheafOperatorBuildError,
  )
import Moonlight.Sheaf.Operator.LinearBasis
  ( LinearBasis,
    linearCoordinateCell,
    linearCoordinateLocalIndex,
    linearBasisIndexedCoordinates,
    mkLinearBasis,
  )

type CosheafMorsePolicy :: Type -> Type -> Type
data CosheafMorsePolicy cell coefficient = CosheafMorsePolicy
  { cmpPivotOps :: !(PivotOps coefficient),
    cmpCellScore :: CosheafCoordinate cell -> Double
  }

defaultCosheafMorsePolicy :: PivotOps coefficient -> CosheafMorsePolicy cell coefficient
defaultCosheafMorsePolicy pivotOps =
  CosheafMorsePolicy
    { cmpPivotOps = pivotOps,
      cmpCellScore =
        \coordinate ->
          case cosheafCoordinateDegree coordinate of
            HomologicalDegree degreeValue ->
              fromIntegral degreeValue
                + fromIntegral (cosheafCoordinateLocalIndex coordinate) / 1000000.0
    }

type CosheafMorsePair :: Type -> Type -> Type
data CosheafMorsePair cell coefficient = CosheafMorsePair
  { cmpLowerCoordinate :: !(CosheafCoordinate cell),
    cmpUpperCoordinate :: !(CosheafCoordinate cell),
    cmpIncidenceCoefficient :: !coefficient
  }
  deriving stock (Eq, Show)

type CosheafMorseMatching :: Type -> Type -> Type
data CosheafMorseMatching cell coefficient = CosheafMorseMatching
  { cmmPairs :: ![CosheafMorsePair cell coefficient],
    cmmCriticalCoordinates :: ![CosheafCoordinate cell]
  }
  deriving stock (Eq, Show)

type CosheafMorseHomologyAgreement :: Type -> Type
data CosheafMorseHomologyAgreement groupCoefficient = CosheafMorseHomologyAgreement
  { cmhaBackend :: !HomologyBackendTag,
    cmhaGroupsByDegree :: !(IntMap (HomologyGroup groupCoefficient))
  }
  deriving stock (Eq, Show)

type MorseProvenance :: Type
data MorseProvenance
  = MorseReducedBoundaryEntry !HomologicalDegree !Int !Int
  deriving stock (Eq, Show)

type CosheafMorseReduction :: Type -> Type -> Type -> Type -> Type -> Type
data CosheafMorseReduction site cell coefficient groupCoefficient provenance = CosheafMorseReduction
  { cmrOriginal :: !(PreparedCosheafChain site cell coefficient provenance),
    cmrMatching :: !(CosheafMorseMatching cell coefficient),
    cmrMorseComplex :: !(AlgebraicMorseComplex coefficient),
    cmrReducedChain :: !(PreparedCosheafChain site (CosheafCoordinate cell) coefficient MorseProvenance),
    cmrCriticalCoordinateByReducedBasis :: !(Map BasisCellRef (CosheafCoordinate cell)),
    cmrOriginalHomology :: !(LinearCosheafHomologyArtifact site cell coefficient groupCoefficient provenance),
    cmrReducedHomology :: !(LinearCosheafHomologyArtifact site (CosheafCoordinate cell) coefficient groupCoefficient MorseProvenance),
    cmrHomologyAgreement :: !(CosheafMorseHomologyAgreement groupCoefficient)
  }

type CosheafMorseFailure :: Type -> Type -> Type -> Type
data CosheafMorseFailure cell coefficient groupCoefficient
  = CosheafMorseHomologyFailed !HomologyFailure
  | CosheafMorseOriginalHomologyFailed !LinearCosheafHomologyFailure
  | CosheafMorseReducedHomologyFailed !LinearCosheafHomologyFailure
  | CosheafMorseHomologyMismatch
      !HomologyBackendTag
      !(IntMap (HomologyGroup groupCoefficient))
      !(IntMap (HomologyGroup groupCoefficient))
  | CosheafMorseOriginalCoordinateMissing !BasisCellRef
  | CosheafMorseReducedBasisFailed !(SheafOperatorBuildError (CosheafCoordinate cell))
  | CosheafMorseReducedBoundaryFailed !(PreparedCosheafChainFailure (CosheafCoordinate cell) coefficient)
  deriving stock (Eq, Show)

morseReduceCosheafChain ::
  (Ord cell, Eq coefficient, Eq groupCoefficient, Num coefficient, Semiring coefficient) =>
  HomologyBackend coefficient groupCoefficient ->
  CosheafMorsePolicy cell coefficient ->
  PreparedCosheafChain site cell coefficient provenance ->
  Either
    (CosheafMorseFailure cell coefficient groupCoefficient)
    (CosheafMorseReduction site cell coefficient groupCoefficient provenance)
morseReduceCosheafChain backend policy chain = do
  originalHomology <-
    first CosheafMorseOriginalHomologyFailed $
      linearCosheafHomology backend chain
  coordinateMap <-
    preparedCoordinateMap chain
  let score basisCellRef =
        maybe 0 (cmpCellScore policy) (Map.lookup basisCellRef coordinateMap)
      pivotOps = policyMorsePivotOps policy
      matching = acyclicMatchingWith pivotOps (pccChainComplex chain) score
  morseValue <-
    first CosheafMorseHomologyFailed $
      morseComplexWith pivotOps (pccChainComplex chain) matching
  matchingValue <-
    matchingCoordinates coordinateMap matching
  reducedChainValue <-
    reducedPreparedChain policy chain coordinateMap morseValue
  criticalCoordinatesByReducedBasis <-
    criticalCoordinateMap coordinateMap morseValue
  reducedHomology <-
    first CosheafMorseReducedHomologyFailed $
      linearCosheafHomology backend reducedChainValue
  agreementValue <-
    morseHomologyAgreement backend originalHomology reducedHomology
  pure
    CosheafMorseReduction
      { cmrOriginal = chain,
        cmrMatching = matchingValue,
        cmrMorseComplex = morseValue,
        cmrReducedChain = reducedChainValue,
        cmrCriticalCoordinateByReducedBasis = criticalCoordinatesByReducedBasis,
        cmrOriginalHomology = originalHomology,
        cmrReducedHomology = reducedHomology,
        cmrHomologyAgreement = agreementValue
      }

policyMorsePivotOps :: CosheafMorsePolicy cell coefficient -> MorsePivotOps coefficient
policyMorsePivotOps policy =
  MorsePivotOps
    { mpoUnitInverse = poUnitInverse (cmpPivotOps policy)
    }

morseHomologyAgreement ::
  Eq groupCoefficient =>
  HomologyBackend coefficient groupCoefficient ->
  LinearCosheafHomologyArtifact site cell coefficient groupCoefficient provenance ->
  LinearCosheafHomologyArtifact site reducedCell coefficient groupCoefficient reducedProvenance ->
  Either
    (CosheafMorseFailure cell coefficient groupCoefficient)
    (CosheafMorseHomologyAgreement groupCoefficient)
morseHomologyAgreement backend originalHomology reducedHomology =
  if originalGroups == reducedGroups
    then
      Right
        CosheafMorseHomologyAgreement
          { cmhaBackend = backendTag,
            cmhaGroupsByDegree = originalGroups
          }
    else
      Left
        ( CosheafMorseHomologyMismatch
            backendTag
            originalGroups
            reducedGroups
        )
  where
    backendTag =
      homologyBackendTag backend

    originalGroups =
      lchaGroupsByDegree originalHomology

    reducedGroups =
      lchaGroupsByDegree reducedHomology

preparedCoordinateMap ::
  PreparedCosheafChain site cell coefficient provenance ->
  Either (CosheafMorseFailure cell coefficient groupCoefficient) (Map BasisCellRef (CosheafCoordinate cell))
preparedCoordinateMap chain =
  coordinateMap <$ traverse (coordinateForBasisRef coordinateMap) (finiteBasisRefs (pccChainComplex chain))
  where
    coordinateMap =
      Map.fromList
        [ ( BasisCellRef degreeValue coordinateIndex,
            CosheafCoordinate
              { cosheafCoordinateDegree = degreeValue,
                cosheafCoordinateCell = linearCoordinateCell linearCoordinate,
                cosheafCoordinateLocalIndex = linearCoordinateLocalIndex linearCoordinate
              }
          )
        | (degreeValue, basisValue) <- Map.toAscList (pccBasisByDegree chain),
          (coordinateIndex, linearCoordinate) <- linearBasisIndexedCoordinates basisValue
        ]

finiteBasisRefs :: FiniteChainComplex coefficient -> [BasisCellRef]
finiteBasisRefs complex =
  let HomologicalDegree maxDegreeInt = maxHomologicalDegree complex
   in foldMap (finiteChainBasisRefsAtDegree complex . HomologicalDegree) [0 .. maxDegreeInt]

matchingCoordinates ::
  Map BasisCellRef (CosheafCoordinate cell) ->
  AlgebraicMorseMatching coefficient ->
  Either (CosheafMorseFailure cell coefficient groupCoefficient) (CosheafMorseMatching cell coefficient)
matchingCoordinates coordinateMap matching = do
  pairs <-
    traverse (matchingPairCoordinates coordinateMap) (lamPairs matching)
  criticalCoordinates <-
    traverse (coordinateForBasisRef coordinateMap) (lamCriticalCells matching)
  pure
    CosheafMorseMatching
      { cmmPairs = pairs,
        cmmCriticalCoordinates = criticalCoordinates
      }

matchingPairCoordinates ::
  Map BasisCellRef (CosheafCoordinate cell) ->
  AlgebraicMorsePair coefficient ->
  Either (CosheafMorseFailure cell coefficient groupCoefficient) (CosheafMorsePair cell coefficient)
matchingPairCoordinates coordinateMap pairValue = do
  lowerCoordinate <- coordinateForBasisRef coordinateMap (lapLowerCell pairValue)
  upperCoordinate <- coordinateForBasisRef coordinateMap (lapUpperCell pairValue)
  pure
    CosheafMorsePair
      { cmpLowerCoordinate = lowerCoordinate,
        cmpUpperCoordinate = upperCoordinate,
        cmpIncidenceCoefficient = lapIncidenceCoefficient pairValue
      }

coordinateForBasisRef ::
  Map BasisCellRef (CosheafCoordinate cell) ->
  BasisCellRef ->
  Either (CosheafMorseFailure cell coefficient groupCoefficient) (CosheafCoordinate cell)
coordinateForBasisRef coordinateMap basisCellRef =
  maybe
    (Left (CosheafMorseOriginalCoordinateMissing basisCellRef))
    Right
    (Map.lookup basisCellRef coordinateMap)

criticalCoordinateMap ::
  Map BasisCellRef (CosheafCoordinate cell) ->
  AlgebraicMorseComplex coefficient ->
  Either (CosheafMorseFailure cell coefficient groupCoefficient) (Map BasisCellRef (CosheafCoordinate cell))
criticalCoordinateMap coordinateMap morseValue =
  traverse (coordinateForBasisRef coordinateMap) (lmcCriticalBasis morseValue)

reducedPreparedChain ::
  (Ord cell, Eq coefficient, Num coefficient, Semiring coefficient) =>
  CosheafMorsePolicy cell coefficient ->
  PreparedCosheafChain site cell coefficient provenance ->
  Map BasisCellRef (CosheafCoordinate cell) ->
  AlgebraicMorseComplex coefficient ->
  Either
    (CosheafMorseFailure cell coefficient groupCoefficient)
    (PreparedCosheafChain site (CosheafCoordinate cell) coefficient MorseProvenance)
reducedPreparedChain policy originalChain coordinateMap morseValue = do
  let reducedComplex = lmcReducedComplex morseValue
      HomologicalDegree maxDegreeInt = maxHomologicalDegree reducedComplex
      degrees = fmap HomologicalDegree [0 .. maxDegreeInt]
      positiveDegrees = fmap HomologicalDegree [1 .. maxDegreeInt]
  basisByDegree <-
    Map.fromList <$> traverse (reducedBasisAtDegree coordinateMap morseValue reducedComplex) degrees
  boundaryByDegree <-
    Map.fromList
      <$> traverse (reducedBoundaryAtDegree (poCoefficientOps (cmpPivotOps policy)) reducedComplex basisByDegree) positiveDegrees
  first CosheafMorseReducedBoundaryFailed $
    mkPreparedCosheafChain
      (poCoefficientOps (cmpPivotOps policy))
      (pccSite originalChain)
      (maxHomologicalDegree reducedComplex)
      basisByDegree
      boundaryByDegree

reducedBasisAtDegree ::
  Ord cell =>
  Map BasisCellRef (CosheafCoordinate cell) ->
  AlgebraicMorseComplex coefficient ->
  FiniteChainComplex coefficient ->
  HomologicalDegree ->
  Either
    (CosheafMorseFailure cell coefficient groupCoefficient)
    (HomologicalDegree, LinearBasis (CosheafCoordinate cell))
reducedBasisAtDegree coordinateMap morseValue reducedComplex degreeValue = do
  coordinates <-
    traverse
      (reducedBasisCoordinate coordinateMap morseValue)
      (finiteChainBasisRefsAtDegree reducedComplex degreeValue)
  basisValue <-
    first CosheafMorseReducedBasisFailed $
      mkLinearBasis (const 1) (mkSheafBasis coordinates)
  pure (degreeValue, basisValue)

reducedBasisCoordinate ::
  Map BasisCellRef (CosheafCoordinate cell) ->
  AlgebraicMorseComplex coefficient ->
  BasisCellRef ->
  Either (CosheafMorseFailure cell coefficient groupCoefficient) (CosheafCoordinate cell)
reducedBasisCoordinate coordinateMap morseValue reducedBasisRef = do
  originalBasisRef <-
    maybe
      (Left (CosheafMorseOriginalCoordinateMissing reducedBasisRef))
      Right
      (Map.lookup reducedBasisRef (lmcCriticalBasis morseValue))
  coordinateForBasisRef coordinateMap originalBasisRef

reducedBoundaryAtDegree ::
  (Eq coefficient, Semiring coefficient) =>
  CoefficientOps coefficient ->
  FiniteChainComplex coefficient ->
  Map HomologicalDegree (LinearBasis (CosheafCoordinate cell)) ->
  HomologicalDegree ->
  Either
    (CosheafMorseFailure cell coefficient groupCoefficient)
    (HomologicalDegree, PreparedCosheafBoundary (CosheafCoordinate cell) coefficient MorseProvenance)
reducedBoundaryAtDegree coefficientOps reducedComplex basisByDegree degreeValue@(HomologicalDegree degreeInt) = do
  sourceBasis <- reducedBasisFromMap degreeValue basisByDegree
  targetBasis <- reducedBasisFromMap (HomologicalDegree (degreeInt - 1)) basisByDegree
  boundaryValue <-
    first CosheafMorseReducedBoundaryFailed $
      buildPreparedCosheafBoundary
        coefficientOps
        degreeValue
        sourceBasis
        targetBasis
        (reducedBoundaryTerms degreeValue (incidenceMatrixAt reducedComplex degreeValue))
  pure (degreeValue, boundaryValue)

reducedBasisFromMap ::
  HomologicalDegree ->
  Map HomologicalDegree (LinearBasis (CosheafCoordinate cell)) ->
  Either (CosheafMorseFailure cell coefficient groupCoefficient) (LinearBasis (CosheafCoordinate cell))
reducedBasisFromMap degreeValue =
  maybe
    (Left (CosheafMorseReducedBoundaryFailed (PreparedCosheafChainBasisMissing degreeValue)))
    Right
    . Map.lookup degreeValue

reducedBoundaryTerms ::
  HomologicalDegree ->
  BoundaryIncidence coefficient ->
  [BoundaryTerm coefficient MorseProvenance]
reducedBoundaryTerms degreeValue incidence =
  fmap
    ( \entryValue ->
        BoundaryTerm
          { boundaryTermSourceIndex = sourceIndex entryValue,
            boundaryTermTargetIndex = targetIndex entryValue,
            boundaryTermCoefficient = boundaryCoefficient entryValue,
            boundaryTermProvenance =
              MorseReducedBoundaryEntry
                degreeValue
                (sourceIndex entryValue)
                (targetIndex entryValue)
          }
    )
    (boundaryEntries incidence)
