{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Cosheaf.Reduction.BlockSchur
  ( CosheafBlockMorsePolicy (..),
    defaultWholeCostalkBlockSchurPolicy,
    CosheafBlockPivotPlan (..),
    CosheafResidualBlock (..),
    BlockSchurMorseProvenance (..),
    CosheafBlockMorseReduction (..),
    CosheafBlockMorseFailure (..),
    blockSchurReduceCosheafChain,
    blockSchurReduceCosheafChainWithPlan,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Moonlight.Algebra (Semiring)
import Moonlight.Cosheaf.Chain.Coefficient
  ( CoefficientOps,
  )
import Moonlight.Cosheaf.Chain.Prepared
  ( BoundaryTerm (..),
    PreparedCosheafBoundary,
    PreparedCosheafChain (..),
    PreparedCosheafChainFailure (..),
    buildPreparedCosheafBoundary,
    mkPreparedCosheafChain,
  )
import Moonlight.Homology
  ( BasisBlock (..),
    BlockPivotOps (..),
    BlockSchurFailure,
    BlockSchurPivot (..),
    BlockSchurReduction (..),
    BoundaryIncidence,
    FiniteChainComplex,
    HomologicalDegree (..),
    boundaryCoefficient,
    boundaryEntries,
    blockSchurReduceWith,
    incidenceMatrixAt,
    maxHomologicalDegree,
    sourceIndex,
    targetIndex,
  )
import Moonlight.Sheaf.Kernel.Basis (mkSheafBasis)
import Moonlight.Sheaf.Operator.BuildError (SheafOperatorBuildError)
import Moonlight.Sheaf.Operator.LinearBasis
  ( LinearBasis,
    linearBasisCellDimension,
    linearBasisCellSlot,
    linearBasisCells,
    mkLinearBasis,
  )

-- | Coefficient-sensitive policy for block-Schur cosheaf reduction.
type CosheafBlockMorsePolicy :: Type -> Type

data CosheafBlockMorsePolicy coefficient = CosheafBlockMorsePolicy
  { cbmpBlockPivotOps :: !(BlockPivotOps coefficient),
    cbmpCoefficientOps :: !(CoefficientOps coefficient)
  }

-- | Whole-costalk policy constructor. The default selection function groups
-- coordinates by original prepared-chain cell and picks the first invertible
-- degree-adjacent costalk block.
defaultWholeCostalkBlockSchurPolicy ::
  BlockPivotOps coefficient ->
  CoefficientOps coefficient ->
  CosheafBlockMorsePolicy coefficient
defaultWholeCostalkBlockSchurPolicy blockOps coefficientOps =
  CosheafBlockMorsePolicy
    { cbmpBlockPivotOps = blockOps,
      cbmpCoefficientOps = coefficientOps
    }

-- | Explicit semantic pivot: cancel the whole upper cell costalk against the
-- whole lower cell costalk in adjacent degrees.
type CosheafBlockPivotPlan :: Type -> Type

data CosheafBlockPivotPlan cell = CosheafBlockPivotPlan
  { cbppUpperDegree :: !HomologicalDegree,
    cbppUpperCell :: !cell,
    cbppLowerDegree :: !HomologicalDegree,
    cbppLowerCell :: !cell
  }
  deriving stock (Eq, Show)

-- | A residual semantic block in the reduced cosheaf chain. It is still one
-- block cell with its surviving local dimension, not loose scalar gravel.
type CosheafResidualBlock :: Type -> Type

data CosheafResidualBlock cell = CosheafResidualBlock
  { crbDegree :: !HomologicalDegree,
    crbCell :: !cell
  }
  deriving stock (Eq, Ord, Show)

type BlockSchurMorseProvenance :: Type

data BlockSchurMorseProvenance
  = BlockSchurReducedBoundaryEntry !HomologicalDegree !Int !Int
  deriving stock (Eq, Show)

type CosheafBlockMorseReduction :: Type -> Type -> Type -> Type -> Type

data CosheafBlockMorseReduction site cell coefficient provenance = CosheafBlockMorseReduction
  { cbmrOriginal :: !(PreparedCosheafChain site cell coefficient provenance),
    cbmrPivotPlan :: !(CosheafBlockPivotPlan cell),
    cbmrSchurReduction :: !(BlockSchurReduction coefficient),
    cbmrReducedChain :: !(PreparedCosheafChain site (CosheafResidualBlock cell) coefficient BlockSchurMorseProvenance)
  }

type CosheafBlockMorseFailure :: Type -> Type -> Type

data CosheafBlockMorseFailure cell coefficient
  = CosheafBlockMorseBasisMissing !HomologicalDegree
  | CosheafBlockMorseCellMissing !HomologicalDegree !cell
  | CosheafBlockMorseZeroDimensionalCell !HomologicalDegree !cell
  | CosheafBlockMorseCellDimensionMismatch !HomologicalDegree !cell !Int !HomologicalDegree !cell !Int
  | CosheafBlockMorseNoInvertibleWholeCostalkPivot
  | CosheafBlockMorseSchurFailed !(BlockSchurFailure coefficient)
  | CosheafBlockMorseReducedBasisFailed !(SheafOperatorBuildError (CosheafResidualBlock cell))
  | CosheafBlockMorseReducedBoundaryFailed !(PreparedCosheafChainFailure (CosheafResidualBlock cell) coefficient)
  deriving stock (Eq, Show)

type WholeCostalkPivotCandidate :: Type -> Type -> Type
data WholeCostalkPivotCandidate cell coefficient = WholeCostalkPivotCandidate
  { wcpcPlan :: !(CosheafBlockPivotPlan cell),
    wcpcMatrix :: ![[coefficient]]
  }

type BoundaryBlockKey :: Type -> Type
data BoundaryBlockKey cell = BoundaryBlockKey
  { bbkUpperCell :: !cell,
    bbkLowerCell :: !cell,
    bbkDimension :: !Int
  }
  deriving stock (Eq, Ord)

type BoundaryBlock :: Type -> Type -> Type
data BoundaryBlock cell coefficient = BoundaryBlock
  { bbKey :: !(BoundaryBlockKey cell),
    bbEntries :: ![(Int, Int, coefficient)]
  }

type BoundaryBlockCellCoordinate :: Type -> Type
data BoundaryBlockCellCoordinate cell = BoundaryBlockCellCoordinate
  { bbccCell :: !cell,
    bbccLocalIndex :: !Int,
    bbccDimension :: !Int
  }

blockSchurReduceCosheafChain ::
  (Ord cell, Eq coefficient, Num coefficient, Semiring coefficient) =>
  CosheafBlockMorsePolicy coefficient ->
  PreparedCosheafChain site cell coefficient provenance ->
  Either
    (CosheafBlockMorseFailure cell coefficient)
    (CosheafBlockMorseReduction site cell coefficient provenance)
blockSchurReduceCosheafChain policy chain = do
  plan <- selectDefaultWholeCostalkPivot policy chain
  blockSchurReduceCosheafChainWithPlan policy chain plan

blockSchurReduceCosheafChainWithPlan ::
  (Ord cell, Eq coefficient, Num coefficient, Semiring coefficient) =>
  CosheafBlockMorsePolicy coefficient ->
  PreparedCosheafChain site cell coefficient provenance ->
  CosheafBlockPivotPlan cell ->
  Either
    (CosheafBlockMorseFailure cell coefficient)
    (CosheafBlockMorseReduction site cell coefficient provenance)
blockSchurReduceCosheafChainWithPlan policy chain pivotPlan = do
  pivot <- pivotFromPlan chain pivotPlan
  schurReduction <-
    first CosheafBlockMorseSchurFailed $
      blockSchurReduceWith
        (cbmpBlockPivotOps policy)
        (pccChainComplex chain)
        pivot
  reducedChain <- rebuildReducedPreparedChain policy chain pivotPlan schurReduction
  pure
    CosheafBlockMorseReduction
      { cbmrOriginal = chain,
        cbmrPivotPlan = pivotPlan,
        cbmrSchurReduction = schurReduction,
        cbmrReducedChain = reducedChain
      }

selectDefaultWholeCostalkPivot ::
  Ord cell =>
  CosheafBlockMorsePolicy coefficient ->
  PreparedCosheafChain site cell coefficient provenance ->
  Either (CosheafBlockMorseFailure cell coefficient) (CosheafBlockPivotPlan cell)
selectDefaultWholeCostalkPivot policy chain =
  maybe
    (Left CosheafBlockMorseNoInvertibleWholeCostalkPivot)
    Right
    (firstInvertibleCandidate blockOps candidates)
  where
    HomologicalDegree maxDegreeInt = pccMaxDegree chain
    degrees = fmap HomologicalDegree [1 .. maxDegreeInt]
    candidates = foldMap candidatesAtDegree degrees
    blockOps = cbmpBlockPivotOps policy

    candidatesAtDegree upperDegree@(HomologicalDegree upperDegreeInt) =
      case (Map.lookup upperDegree (pccBasisByDegree chain), Map.lookup (HomologicalDegree (upperDegreeInt - 1)) (pccBasisByDegree chain)) of
        (Just upperBasis, Just lowerBasis) ->
          wholeCostalkPivotCandidatesAtDegree blockOps chain upperDegree upperBasis lowerBasis
        _ -> []

firstInvertibleCandidate :: BlockPivotOps coefficient -> [WholeCostalkPivotCandidate cell coefficient] -> Maybe (CosheafBlockPivotPlan cell)
firstInvertibleCandidate blockOps =
  fmap wcpcPlan . List.find invertibleCandidate
  where
    invertibleCandidate candidate =
      case bpoInvertBlock blockOps (wcpcMatrix candidate) of
        Right _ -> True
        Left _ -> False

wholeCostalkPivotCandidatesAtDegree ::
  Ord cell =>
  BlockPivotOps coefficient ->
  PreparedCosheafChain site cell coefficient provenance ->
  HomologicalDegree ->
  LinearBasis cell ->
  LinearBasis cell ->
  [WholeCostalkPivotCandidate cell coefficient]
wholeCostalkPivotCandidatesAtDegree blockOps chain upperDegree@(HomologicalDegree upperDegreeInt) upperBasis lowerBasis =
  fmap candidateFromBlock orderedBlocks
  where
    lowerDegree =
      HomologicalDegree (upperDegreeInt - 1)

    blockIndex =
      nonzeroBoundaryBlockIndex
        upperBasis
        lowerBasis
        (incidenceMatrixAt (pccChainComplex chain) upperDegree)

    orderedBlocks =
      List.sortOn
        (boundaryBlockOrder upperOrder lowerOrder)
        (Map.elems blockIndex)

    upperOrder =
      linearBasisCellOrder upperBasis

    lowerOrder =
      linearBasisCellOrder lowerBasis

    candidateFromBlock block =
      let key = bbKey block
       in WholeCostalkPivotCandidate
            { wcpcPlan =
                CosheafBlockPivotPlan
                  upperDegree
                  (bbkUpperCell key)
                  lowerDegree
                  (bbkLowerCell key),
              wcpcMatrix = boundaryBlockMatrix blockOps block
            }

nonzeroBoundaryBlockIndex ::
  Ord cell =>
  LinearBasis cell ->
  LinearBasis cell ->
  BoundaryIncidence coefficient ->
  Map (BoundaryBlockKey cell) (BoundaryBlock cell coefficient)
nonzeroBoundaryBlockIndex upperBasis lowerBasis incidence =
  Map.fromListWith mergeBoundaryBlock $
    mapMaybe boundaryBlockEntry (boundaryEntries incidence)
  where
    upperCoordinates =
      coordinateCellIndex upperBasis

    lowerCoordinates =
      coordinateCellIndex lowerBasis

    boundaryBlockEntry entry = do
      upperCoordinate <- Map.lookup (sourceIndex entry) upperCoordinates
      lowerCoordinate <- Map.lookup (targetIndex entry) lowerCoordinates
      let upperDimension = bbccDimension upperCoordinate
          lowerDimension = bbccDimension lowerCoordinate
      if upperDimension > 0 && upperDimension == lowerDimension
        then
          let key =
                BoundaryBlockKey
                  { bbkUpperCell = bbccCell upperCoordinate,
                    bbkLowerCell = bbccCell lowerCoordinate,
                    bbkDimension = upperDimension
                  }
           in Just
                ( key,
                  BoundaryBlock
                    { bbKey = key,
                      bbEntries =
                        [ ( bbccLocalIndex upperCoordinate,
                            bbccLocalIndex lowerCoordinate,
                            boundaryCoefficient entry
                          )
                        ]
                    }
                )
        else Nothing

mergeBoundaryBlock :: BoundaryBlock cell coefficient -> BoundaryBlock cell coefficient -> BoundaryBlock cell coefficient
mergeBoundaryBlock newBlock oldBlock =
  oldBlock
    { bbEntries = bbEntries oldBlock <> bbEntries newBlock
    }

coordinateCellIndex :: Ord cell => LinearBasis cell -> Map Int (BoundaryBlockCellCoordinate cell)
coordinateCellIndex basis =
  Map.fromList (linearBasisCells basis >>= cellCoordinates)
  where
    cellCoordinates cell =
      maybe
        []
        ( \(offset, dimensionValue) ->
            fmap
              ( \localIndexValue ->
                  ( offset + localIndexValue,
                    BoundaryBlockCellCoordinate
                      { bbccCell = cell,
                        bbccLocalIndex = localIndexValue,
                        bbccDimension = dimensionValue
                      }
                  )
              )
              [0 .. dimensionValue - 1]
        )
        (linearBasisCellSlot cell basis)

boundaryBlockOrder :: Ord cell => Map cell Int -> Map cell Int -> BoundaryBlock cell coefficient -> (Int, Int)
boundaryBlockOrder upperOrder lowerOrder block =
  ( cellOrder (bbkUpperCell key) upperOrder,
    cellOrder (bbkLowerCell key) lowerOrder
  )
  where
    key =
      bbKey block

    cellOrder cell =
      Map.findWithDefault maxBound cell

linearBasisCellOrder :: Ord cell => LinearBasis cell -> Map cell Int
linearBasisCellOrder basis =
  Map.fromList (zip (linearBasisCells basis) [0 :: Int ..])

boundaryBlockMatrix :: BlockPivotOps coefficient -> BoundaryBlock cell coefficient -> [[coefficient]]
boundaryBlockMatrix blockOps block =
  [ [ Map.findWithDefault (bpoZero blockOps) (sourceLocalIndex, targetLocalIndex) entryMap
      | sourceLocalIndex <- [0 .. dimensionValue - 1]
    ]
    | targetLocalIndex <- [0 .. dimensionValue - 1]
  ]
  where
    dimensionValue =
      bbkDimension (bbKey block)

    entryMap =
      Map.fromListWith
        const
        [ ((sourceLocalIndex, targetLocalIndex), coefficientValue)
          | (sourceLocalIndex, targetLocalIndex, coefficientValue) <- bbEntries block
        ]

pivotFromPlan ::
  Ord cell =>
  PreparedCosheafChain site cell coefficient provenance ->
  CosheafBlockPivotPlan cell ->
  Either (CosheafBlockMorseFailure cell coefficient) BlockSchurPivot
pivotFromPlan chain plan = do
  upperBasis <- basisAt (cbppUpperDegree plan) chain
  lowerBasis <- basisAt (cbppLowerDegree plan) chain
  upperIndices <- first (const (CosheafBlockMorseCellMissing (cbppUpperDegree plan) (cbppUpperCell plan))) (blockIndicesForCell (cbppUpperDegree plan) upperBasis (cbppUpperCell plan))
  lowerIndices <- first (const (CosheafBlockMorseCellMissing (cbppLowerDegree plan) (cbppLowerCell plan))) (blockIndicesForCell (cbppLowerDegree plan) lowerBasis (cbppLowerCell plan))
  validatePlanDimensions plan upperIndices lowerIndices
  pure
    BlockSchurPivot
      { bspUpperBlock = BasisBlock (cbppUpperDegree plan) upperIndices,
        bspLowerBlock = BasisBlock (cbppLowerDegree plan) lowerIndices
      }

basisAt ::
  HomologicalDegree ->
  PreparedCosheafChain site cell coefficient provenance ->
  Either (CosheafBlockMorseFailure cell coefficient) (LinearBasis cell)
basisAt degreeValue chain =
  maybe
    (Left (CosheafBlockMorseBasisMissing degreeValue))
    Right
    (Map.lookup degreeValue (pccBasisByDegree chain))

blockIndicesForCell ::
  Ord cell =>
  HomologicalDegree ->
  LinearBasis cell ->
  cell ->
  Either (HomologicalDegree, cell) [Int]
blockIndicesForCell degreeValue basis cell =
  case linearBasisCellSlot cell basis of
    Nothing -> Left (degreeValue, cell)
    Just (offset, dimension) -> Right [offset .. offset + dimension - 1]

validatePlanDimensions ::
  CosheafBlockPivotPlan cell ->
  [Int] ->
  [Int] ->
  Either (CosheafBlockMorseFailure cell coefficient) ()
validatePlanDimensions plan upperIndices lowerIndices
  | null upperIndices = Left (CosheafBlockMorseZeroDimensionalCell (cbppUpperDegree plan) (cbppUpperCell plan))
  | null lowerIndices = Left (CosheafBlockMorseZeroDimensionalCell (cbppLowerDegree plan) (cbppLowerCell plan))
  | length upperIndices == length lowerIndices = Right ()
  | otherwise =
      Left
        ( CosheafBlockMorseCellDimensionMismatch
            (cbppUpperDegree plan)
            (cbppUpperCell plan)
            (length upperIndices)
            (cbppLowerDegree plan)
            (cbppLowerCell plan)
            (length lowerIndices)
        )

rebuildReducedPreparedChain ::
  (Ord cell, Eq coefficient, Num coefficient, Semiring coefficient) =>
  CosheafBlockMorsePolicy coefficient ->
  PreparedCosheafChain site cell coefficient provenance ->
  CosheafBlockPivotPlan cell ->
  BlockSchurReduction coefficient ->
  Either
    (CosheafBlockMorseFailure cell coefficient)
    (PreparedCosheafChain site (CosheafResidualBlock cell) coefficient BlockSchurMorseProvenance)
rebuildReducedPreparedChain policy originalChain pivotPlan schurReduction = do
  basisByDegree <- reducedBasisByDegree originalChain pivotPlan
  boundaryByDegree <- reducedBoundaryByDegree policy basisByDegree (bsrReducedComplex schurReduction)
  first CosheafBlockMorseReducedBoundaryFailed $
    mkPreparedCosheafChain
      (cbmpCoefficientOps policy)
      (pccSite originalChain)
      (pccMaxDegree originalChain)
      basisByDegree
      boundaryByDegree

reducedBasisByDegree ::
  Ord cell =>
  PreparedCosheafChain site cell coefficient provenance ->
  CosheafBlockPivotPlan cell ->
  Either
    (CosheafBlockMorseFailure cell coefficient)
    (Map HomologicalDegree (LinearBasis (CosheafResidualBlock cell)))
reducedBasisByDegree originalChain pivotPlan =
  Map.fromList <$> traverse basisAtDegree degrees
  where
    HomologicalDegree maxDegreeInt = pccMaxDegree originalChain
    degrees = fmap HomologicalDegree [0 .. maxDegreeInt]

    basisAtDegree degreeValue = do
      originalBasis <- basisAt degreeValue originalChain
      let residualCells = fmap (CosheafResidualBlock degreeValue) (filter (cellSurvives degreeValue) (linearBasisCells originalBasis))
          dimensionOf residualBlock = maybe 0 id (linearBasisCellDimension (crbCell residualBlock) originalBasis)
      reducedBasis <-
        first CosheafBlockMorseReducedBasisFailed $
          mkLinearBasis dimensionOf (mkSheafBasis residualCells)
      pure (degreeValue, reducedBasis)

    cellSurvives degreeValue cell
      | degreeValue == cbppUpperDegree pivotPlan && cell == cbppUpperCell pivotPlan = False
      | degreeValue == cbppLowerDegree pivotPlan && cell == cbppLowerCell pivotPlan = False
      | otherwise = True

reducedBoundaryByDegree ::
  (Eq coefficient, Semiring coefficient) =>
  CosheafBlockMorsePolicy coefficient ->
  Map HomologicalDegree (LinearBasis (CosheafResidualBlock cell)) ->
  FiniteChainComplex coefficient ->
  Either
    (CosheafBlockMorseFailure cell coefficient)
    (Map HomologicalDegree (PreparedCosheafBoundary (CosheafResidualBlock cell) coefficient BlockSchurMorseProvenance))
reducedBoundaryByDegree policy basisByDegree reducedComplex =
  Map.fromList <$> traverse boundaryAtDegree positiveDegrees
  where
    HomologicalDegree maxDegreeInt = maxHomologicalDegree reducedComplex
    positiveDegrees = fmap HomologicalDegree [1 .. maxDegreeInt]

    boundaryAtDegree degreeValue@(HomologicalDegree degreeInt) = do
      sourceBasis <- reducedBasisAt degreeValue
      targetBasis <- reducedBasisAt (HomologicalDegree (degreeInt - 1))
      boundary <-
        first CosheafBlockMorseReducedBoundaryFailed $
          buildPreparedCosheafBoundary
            (cbmpCoefficientOps policy)
            degreeValue
            sourceBasis
            targetBasis
            (boundaryTerms degreeValue (incidenceMatrixAt reducedComplex degreeValue))
      pure (degreeValue, boundary)

    reducedBasisAt degreeValue =
      maybe
        (Left (CosheafBlockMorseReducedBoundaryFailed (PreparedCosheafChainBasisMissing degreeValue)))
        Right
        (Map.lookup degreeValue basisByDegree)

boundaryTerms :: HomologicalDegree -> BoundaryIncidence coefficient -> [BoundaryTerm coefficient BlockSchurMorseProvenance]
boundaryTerms degreeValue incidence =
  [ BoundaryTerm
      { boundaryTermSourceIndex = sourceIndex entry,
        boundaryTermTargetIndex = targetIndex entry,
        boundaryTermCoefficient = boundaryCoefficient entry,
        boundaryTermProvenance = BlockSchurReducedBoundaryEntry degreeValue (sourceIndex entry) (targetIndex entry)
      }
    | entry <- boundaryEntries incidence
  ]
