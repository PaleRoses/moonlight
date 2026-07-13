module Moonlight.Homology.Pure.Matrix.SparseLinAlg
  ( SparseRow,
    SparseMatrix (..),
    sparseBoundaryMatrixWith,
    sparseBoundaryMatrix,
    sparseTransposeMatrix,
    SparseRref (..),
    sparseRref,
    sparseKernelBasisFromRref,
    sparseKernelBasisOf,
    sparseKernelVector,
    sparseImageBasisFromRref,
    sparseImageBasisOf,
    SparseEchelonBasis (..),
    sparseEchelonBasis,
    sparseEchelonContains,
    sparseEchelonRank,
    sparseIndependentModulo,
    sparseIndependentModuloWithBasis,
    sparseSpanRank,
    SparseCoordinateBasis (..),
    sparseCoordinateBasis,
    sparseCoordinatesInBasis,
    compactSparseRow,
    scaleSparseRow,
    addScaledSparseRow,
    sparseMatrixFromRows,
    sparseRowFromDense,
    sparseRowToDense,
    sparseRowLookup,
  )
where

import Data.Function ((&))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Kind (Type)
import qualified Data.List as List
import Data.Ratio (denominator, numerator)
import Moonlight.Homology.Boundary.LinAlg
  ( BoundaryIncidence,
    boundaryCoefficient,
    boundaryEntries,
    sourceCardinality,
    sourceIndex,
    targetCardinality,
    targetIndex,
  )
import Moonlight.Homology.Pure.Filtration (enumerateFromZero)

type SparseRow :: Type
type SparseRow = IntMap Rational

type SparseMatrix :: Type
data SparseMatrix = SparseMatrix
  { smRows :: ![SparseRow],
    smColumnCount :: !Int
  }
  deriving stock (Eq, Show)

sparseBoundaryMatrixWith :: (r -> Rational) -> BoundaryIncidence r -> SparseMatrix
sparseBoundaryMatrixWith convert incidence =
  let rowCount = targetCardinality incidence
      columnCount = sourceCardinality incidence
      rowBuckets =
        boundaryEntries incidence
          & List.foldl'
            ( \accumulator entry ->
                let rowIndex = targetIndex entry
                    columnIndex = sourceIndex entry
                    coefficient = convert (boundaryCoefficient entry)
                 in IntMap.insertWith
                      (IntMap.unionWith (+))
                      rowIndex
                      (IntMap.singleton columnIndex coefficient)
                      accumulator
            )
            IntMap.empty
   in SparseMatrix
        { smRows =
            enumerateFromZero rowCount
              & fmap
                ( \rowIndex ->
                    IntMap.filter
                      (/= 0)
                      (IntMap.findWithDefault IntMap.empty rowIndex rowBuckets)
                ),
          smColumnCount = columnCount
        }

sparseBoundaryMatrix :: Integral r => BoundaryIncidence r -> SparseMatrix
sparseBoundaryMatrix =
  sparseBoundaryMatrixWith fromIntegral

sparseTransposeMatrix :: SparseMatrix -> SparseMatrix
sparseTransposeMatrix matrix =
  let transposedBuckets =
        smRows matrix
          & zip [0 :: Int ..]
          & List.foldl'
            ( \accumulator (rowIndex, rowValue) ->
                IntMap.foldlWithKey'
                  ( \innerAccumulator columnIndex coefficient ->
                      IntMap.insertWith
                        (IntMap.unionWith (+))
                        columnIndex
                        (IntMap.singleton rowIndex coefficient)
                        innerAccumulator
                  )
                  accumulator
                  rowValue
            )
            IntMap.empty
      newRowCount = smColumnCount matrix
      newColumnCount = length (smRows matrix)
   in SparseMatrix
        { smRows =
            enumerateFromZero newRowCount
              & fmap (\rowIndex -> IntMap.findWithDefault IntMap.empty rowIndex transposedBuckets),
          smColumnCount = newColumnCount
        }

type SparseRref :: Type
data SparseRref = SparseRref
  { srrefPivots :: ![(Int, SparseRow)],
    srrefColumnCount :: !Int
  }
  deriving stock (Eq, Show)

type SparseEchelonBasis :: Type
data SparseEchelonBasis = SparseEchelonBasis
  { sebColumnCount :: !Int,
    sebPivotRows :: !(IntMap SparseRow)
  }
  deriving stock (Eq, Show)

type SparseCoordinatePivot :: Type
data SparseCoordinatePivot = SparseCoordinatePivot
  { scpVector :: !SparseRow,
    scpCoordinates :: !SparseRow
  }
  deriving stock (Eq, Show)

type SparseCoordinateBasis :: Type
data SparseCoordinateBasis = SparseCoordinateBasis
  { scbAmbientDimension :: !Int,
    scbGeneratorCount :: !Int,
    scbPivotRows :: !(IntMap SparseCoordinatePivot)
  }
  deriving stock (Eq, Show)

type SparseCoordinateResidual :: Type
data SparseCoordinateResidual = SparseCoordinateResidual
  { scrVector :: !SparseRow,
    scrCoordinates :: !SparseRow
  }
  deriving stock (Eq, Show)

type SparseSupportIndex :: Type
data SparseSupportIndex = SparseSupportIndex
  { ssiColumnRows :: !(IntMap IntSet.IntSet),
    ssiSupportBuckets :: !(IntMap IntSet.IntSet),
    ssiRowBuckets :: !(IntMap IntSet.IntSet)
  }
  deriving stock (Eq, Show)

type SparseEliminationState :: Type
data SparseEliminationState = SparseEliminationState
  { sesActiveRows :: !(IntMap SparseRow),
    sesSupportIndex :: !SparseSupportIndex,
    sesSelectedPivots :: ![(Int, SparseRow)]
  }
  deriving stock (Eq, Show)

type PivotScore :: Type
data PivotScore = PivotScore
  { pivotMarkowitzFill :: !Int,
    pivotUnitPenalty :: !Int,
    pivotCoefficientHeight :: !Integer,
    pivotRowDegree :: !Int,
    pivotColumnScore :: !Int,
    pivotRowIdScore :: !Int
  }
  deriving stock (Eq, Ord, Show)

type PivotCandidate :: Type
data PivotCandidate = PivotCandidate
  { pcScore :: !PivotScore,
    pcRowId :: !Int,
    pcColumn :: !Int,
    pcRow :: !SparseRow
  }
  deriving stock (Eq, Show)

sparseRref :: SparseMatrix -> SparseRref
sparseRref matrix =
  let finalState = convergeSparseElimination (initialSparseEliminationState matrix)
      pivots = canonicalRrefPivots (reverse (sesSelectedPivots finalState))
   in SparseRref
        { srrefPivots = pivots,
          srrefColumnCount = smColumnCount matrix
        }

initialSparseEliminationState :: SparseMatrix -> SparseEliminationState
initialSparseEliminationState matrix =
  let activeRows =
        smRows matrix
          & zip [0 :: Int ..]
          & List.foldl'
            ( \accumulator (rowId, rowValue) ->
                let compactRow = compactSparseRow rowValue
                 in if IntMap.null compactRow
                      then accumulator
                      else IntMap.insert rowId compactRow accumulator
            )
            IntMap.empty
      supportIndex = buildSparseSupportIndex activeRows
   in SparseEliminationState
        { sesActiveRows = activeRows,
          sesSupportIndex = supportIndex,
          sesSelectedPivots = []
        }

convergeSparseElimination :: SparseEliminationState -> SparseEliminationState
convergeSparseElimination state =
  case sparseEliminationStep state of
    Nothing -> state
    Just nextState -> convergeSparseElimination nextState

sparseEliminationStep :: SparseEliminationState -> Maybe SparseEliminationState
sparseEliminationStep state =
  case choosePivot state of
    Nothing -> Nothing
    Just pivot ->
      let pivotColumn = pcColumn pivot
          pivotRowId = pcRowId pivot
          normalizedPivot = normalizeSparseRow pivotColumn (pcRow pivot)
          affectedRows =
            sesSupportIndex state
              & ssiColumnRows
              & IntMap.findWithDefault IntSet.empty pivotColumn
              & IntSet.delete pivotRowId
          withoutPivot = removeActiveRow pivotRowId state
          reducedState =
            IntSet.foldl'
              (eliminateTargetRow pivotColumn normalizedPivot)
              withoutPivot
              affectedRows
       in Just
            reducedState
              { sesSelectedPivots =
                  (pivotColumn, normalizedPivot) : sesSelectedPivots reducedState
              }

choosePivot :: SparseEliminationState -> Maybe PivotCandidate
choosePivot state =
  case chooseZeroFillPivot state of
    Just pivot -> Just pivot
    Nothing ->
      case firstSupportBucket (ssiRowBuckets (sesSupportIndex state)) of
        Nothing -> Nothing
        Just (minimumRowDegree, _) ->
          choosePivotFromSupportBuckets
            state
            minimumRowDegree
            (ssiSupportBuckets (sesSupportIndex state))
            Nothing

chooseZeroFillPivot :: SparseEliminationState -> Maybe PivotCandidate
chooseZeroFillPivot state =
  case IntMap.lookup 1 (ssiSupportBuckets (sesSupportIndex state)) of
    Nothing -> chooseSingletonRowPivot state
    Just singletonColumns ->
      choosePivotFromColumns
        state
        1
        (chooseSingletonRowPivot state)
        singletonColumns

firstSupportBucket :: IntMap IntSet.IntSet -> Maybe (Int, IntSet.IntSet)
firstSupportBucket buckets =
  case IntMap.lookupMin buckets of
    Nothing -> Nothing
    Just (supportCount, candidateColumns)
      | IntSet.null candidateColumns ->
          firstSupportBucket (IntMap.delete supportCount buckets)
      | otherwise ->
          Just (supportCount, candidateColumns)

choosePivotFromColumns ::
  SparseEliminationState ->
  Int ->
  Maybe PivotCandidate ->
  IntSet.IntSet ->
  Maybe PivotCandidate
choosePivotFromColumns state columnDegree bestCandidate =
  IntSet.foldl'
    ( \currentBestCandidate columnIndex ->
        choosePivotFromColumn state columnDegree columnIndex currentBestCandidate
    )
    bestCandidate

choosePivotFromSupportBuckets ::
  SparseEliminationState ->
  Int ->
  IntMap IntSet.IntSet ->
  Maybe PivotCandidate ->
  Maybe PivotCandidate
choosePivotFromSupportBuckets state minimumRowDegree supportBuckets bestCandidate =
  case firstSupportBucket supportBuckets of
    Nothing -> bestCandidate
    Just (columnDegree, candidateColumns) ->
      if pivotLowerBoundExceedsBest minimumRowDegree columnDegree bestCandidate
        then bestCandidate
        else
          choosePivotFromSupportBuckets
            state
            minimumRowDegree
            (IntMap.delete columnDegree supportBuckets)
            (choosePivotFromColumns state columnDegree bestCandidate candidateColumns)

pivotLowerBoundExceedsBest :: Int -> Int -> Maybe PivotCandidate -> Bool
pivotLowerBoundExceedsBest _ _ Nothing =
  False
pivotLowerBoundExceedsBest minimumRowDegree columnDegree (Just bestCandidate) =
  let lowerBound =
        max 0 (minimumRowDegree - 1) * max 0 (columnDegree - 1)
   in lowerBound > pivotMarkowitzFill (pcScore bestCandidate)

chooseSingletonRowPivot :: SparseEliminationState -> Maybe PivotCandidate
chooseSingletonRowPivot state =
  case IntMap.lookup 1 (ssiRowBuckets (sesSupportIndex state)) of
    Nothing -> Nothing
    Just singletonRows ->
      IntSet.foldl'
        ( \bestCandidate rowId ->
            case singletonRowPivotCandidate state rowId of
              Nothing -> bestCandidate
              Just candidate -> betterPivotCandidate bestCandidate candidate
        )
        Nothing
        singletonRows

singletonRowPivotCandidate :: SparseEliminationState -> Int -> Maybe PivotCandidate
singletonRowPivotCandidate state rowId =
  case IntMap.lookup rowId (sesActiveRows state) of
    Nothing -> Nothing
    Just rowValue ->
      case IntMap.lookupMin rowValue of
        Nothing -> Nothing
        Just (columnIndex, _) ->
          pivotCandidateAt
            state
            (columnSupportCount state columnIndex)
            columnIndex
            rowId

columnSupportCount :: SparseEliminationState -> Int -> Int
columnSupportCount state columnIndex =
  state
    & sesSupportIndex
    & ssiColumnRows
    & IntMap.findWithDefault IntSet.empty columnIndex
    & IntSet.size

choosePivotFromColumn ::
  SparseEliminationState ->
  Int ->
  Int ->
  Maybe PivotCandidate ->
  Maybe PivotCandidate
choosePivotFromColumn state columnDegree columnIndex bestCandidate =
  IntSet.foldl'
    ( \currentBestCandidate rowId ->
        case pivotCandidateAt state columnDegree columnIndex rowId of
          Nothing -> currentBestCandidate
          Just candidate -> betterPivotCandidate currentBestCandidate candidate
    )
    bestCandidate
    columnRows
  where
    columnRows =
      state
        & sesSupportIndex
        & ssiColumnRows
        & IntMap.findWithDefault IntSet.empty columnIndex

pivotCandidateAt ::
  SparseEliminationState ->
  Int ->
  Int ->
  Int ->
  Maybe PivotCandidate
pivotCandidateAt state columnDegree columnIndex rowId =
  case IntMap.lookup rowId (sesActiveRows state) of
    Nothing -> Nothing
    Just rowValue ->
      case IntMap.lookup columnIndex rowValue of
        Nothing -> Nothing
        Just coefficient
          | coefficient == 0 -> Nothing
          | otherwise ->
              Just
                PivotCandidate
                  { pcScore =
                      pivotScore
                        columnDegree
                        columnIndex
                        rowId
                        rowValue
                        coefficient,
                    pcRowId = rowId,
                    pcColumn = columnIndex,
                    pcRow = rowValue
                  }

pivotScore :: Int -> Int -> Int -> SparseRow -> Rational -> PivotScore
pivotScore columnDegree columnIndex rowId rowValue coefficient =
  let rowDegree = IntMap.size rowValue
   in PivotScore
        { pivotMarkowitzFill =
            max 0 (rowDegree - 1) * max 0 (columnDegree - 1),
          pivotUnitPenalty =
            if abs coefficient == 1
              then 0
              else 1,
          pivotCoefficientHeight =
            abs (numerator coefficient) + denominator coefficient,
          pivotRowDegree = rowDegree,
          pivotColumnScore = columnIndex,
          pivotRowIdScore = rowId
        }

betterPivotCandidate :: Maybe PivotCandidate -> PivotCandidate -> Maybe PivotCandidate
betterPivotCandidate Nothing candidate =
  Just candidate
betterPivotCandidate (Just incumbent) candidate =
  if pcScore candidate < pcScore incumbent
    then Just candidate
    else Just incumbent

buildSparseSupportIndex :: IntMap SparseRow -> SparseSupportIndex
buildSparseSupportIndex =
  IntMap.foldlWithKey'
    ( \supportIndex rowId rowValue ->
        addActiveRowSupport rowId rowValue supportIndex
    )
    emptySparseSupportIndex

emptySparseSupportIndex :: SparseSupportIndex
emptySparseSupportIndex =
  SparseSupportIndex
    { ssiColumnRows = IntMap.empty,
      ssiSupportBuckets = IntMap.empty,
      ssiRowBuckets = IntMap.empty
    }

removeActiveRow :: Int -> SparseEliminationState -> SparseEliminationState
removeActiveRow rowId state =
  case IntMap.lookup rowId (sesActiveRows state) of
    Nothing -> state
    Just rowValue ->
      state
        { sesActiveRows = IntMap.delete rowId (sesActiveRows state),
          sesSupportIndex =
            removeActiveRowSupport rowId rowValue (sesSupportIndex state)
        }

replaceActiveRow ::
  Int ->
  SparseRow ->
  SparseRow ->
  SparseEliminationState ->
  SparseEliminationState
replaceActiveRow rowId oldRow newRow state =
  let compactNewRow = compactSparseRow newRow
      withoutOldSupport = removeActiveRowSupport rowId oldRow (sesSupportIndex state)
   in if IntMap.null compactNewRow
        then
          state
            { sesActiveRows = IntMap.delete rowId (sesActiveRows state),
              sesSupportIndex = withoutOldSupport
            }
        else
          state
            { sesActiveRows = IntMap.insert rowId compactNewRow (sesActiveRows state),
              sesSupportIndex = addActiveRowSupport rowId compactNewRow withoutOldSupport
            }

eliminateTargetRow ::
  Int ->
  SparseRow ->
  SparseEliminationState ->
  Int ->
  SparseEliminationState
eliminateTargetRow pivotColumn pivotRow state rowId =
  case IntMap.lookup rowId (sesActiveRows state) of
    Nothing -> state
    Just targetRow ->
      replaceActiveRow
        rowId
        targetRow
        (eliminateColumnFromRow pivotColumn pivotRow targetRow)
        state

addRowSupport :: Int -> SparseRow -> SparseSupportIndex -> SparseSupportIndex
addRowSupport rowId rowValue supportIndex =
  IntMap.foldlWithKey'
    ( \currentSupportIndex columnIndex coefficient ->
        if coefficient == 0
          then currentSupportIndex
          else addRowToColumn rowId columnIndex currentSupportIndex
    )
    supportIndex
    rowValue

removeRowSupport :: Int -> SparseRow -> SparseSupportIndex -> SparseSupportIndex
removeRowSupport rowId rowValue supportIndex =
  IntMap.foldlWithKey'
    ( \currentSupportIndex columnIndex _ ->
        removeRowFromColumn rowId columnIndex currentSupportIndex
    )
    supportIndex
    rowValue

addActiveRowSupport :: Int -> SparseRow -> SparseSupportIndex -> SparseSupportIndex
addActiveRowSupport rowId rowValue =
  addRowDegreeSupport rowId rowValue . addRowSupport rowId rowValue

removeActiveRowSupport :: Int -> SparseRow -> SparseSupportIndex -> SparseSupportIndex
removeActiveRowSupport rowId rowValue =
  removeRowDegreeSupport rowId rowValue . removeRowSupport rowId rowValue

addRowDegreeSupport :: Int -> SparseRow -> SparseSupportIndex -> SparseSupportIndex
addRowDegreeSupport rowId rowValue supportIndex =
  let rowDegree = IntMap.size rowValue
   in if rowDegree <= 0
        then supportIndex
        else
          supportIndex
            { ssiRowBuckets =
                updateSupportBucket rowDegree (IntSet.insert rowId) (ssiRowBuckets supportIndex)
            }

removeRowDegreeSupport :: Int -> SparseRow -> SparseSupportIndex -> SparseSupportIndex
removeRowDegreeSupport rowId rowValue supportIndex =
  let rowDegree = IntMap.size rowValue
   in if rowDegree <= 0
        then supportIndex
        else
          supportIndex
            { ssiRowBuckets =
                updateSupportBucket rowDegree (IntSet.delete rowId) (ssiRowBuckets supportIndex)
            }

addRowToColumn :: Int -> Int -> SparseSupportIndex -> SparseSupportIndex
addRowToColumn rowId columnIndex supportIndex =
  let columnRows = ssiColumnRows supportIndex
      existingRows = IntMap.findWithDefault IntSet.empty columnIndex columnRows
      oldCount = IntSet.size existingRows
      newRows = IntSet.insert rowId existingRows
      newCount = IntSet.size newRows
   in if oldCount == newCount
        then supportIndex
        else
          supportIndex
            { ssiColumnRows = IntMap.insert columnIndex newRows columnRows,
              ssiSupportBuckets =
                moveColumnSupport columnIndex oldCount newCount (ssiSupportBuckets supportIndex)
            }

removeRowFromColumn :: Int -> Int -> SparseSupportIndex -> SparseSupportIndex
removeRowFromColumn rowId columnIndex supportIndex =
  case IntMap.lookup columnIndex (ssiColumnRows supportIndex) of
    Nothing -> supportIndex
    Just existingRows ->
      let oldCount = IntSet.size existingRows
          newRows = IntSet.delete rowId existingRows
          newCount = IntSet.size newRows
          columnRows =
            if IntSet.null newRows
              then IntMap.delete columnIndex (ssiColumnRows supportIndex)
              else IntMap.insert columnIndex newRows (ssiColumnRows supportIndex)
       in if oldCount == newCount
            then supportIndex
            else
              supportIndex
                { ssiColumnRows = columnRows,
                  ssiSupportBuckets =
                    moveColumnSupport columnIndex oldCount newCount (ssiSupportBuckets supportIndex)
                }

moveColumnSupport :: Int -> Int -> Int -> IntMap IntSet.IntSet -> IntMap IntSet.IntSet
moveColumnSupport columnIndex oldCount newCount buckets =
  if oldCount == newCount
    then buckets
    else
      let withoutOld =
            if oldCount <= 0
              then buckets
              else updateSupportBucket oldCount (IntSet.delete columnIndex) buckets
       in if newCount <= 0
            then withoutOld
            else updateSupportBucket newCount (IntSet.insert columnIndex) withoutOld

updateSupportBucket ::
  Int ->
  (IntSet.IntSet -> IntSet.IntSet) ->
  IntMap IntSet.IntSet ->
  IntMap IntSet.IntSet
updateSupportBucket count transform buckets =
  let updatedColumns = transform (IntMap.findWithDefault IntSet.empty count buckets)
   in if IntSet.null updatedColumns
        then IntMap.delete count buckets
        else IntMap.insert count updatedColumns buckets

canonicalRrefPivots :: [(Int, SparseRow)] -> [(Int, SparseRow)]
canonicalRrefPivots selectedPivots =
  selectedPivots
    & reverse
    & List.foldl'
      ( \laterPivots pivot ->
          reduceAgainstLaterPivot pivot laterPivots
      )
      IntMap.empty
    & IntMap.toAscList

reduceAgainstLaterPivot :: (Int, SparseRow) -> IntMap SparseRow -> IntMap SparseRow
reduceAgainstLaterPivot (pivotColumn, pivotRow) laterPivots =
  let reducedRow =
        IntMap.foldlWithKey'
          ( \rowValue laterPivotColumn laterPivotRow ->
              eliminateColumnFromRow laterPivotColumn laterPivotRow rowValue
          )
          pivotRow
          laterPivots
   in IntMap.insert pivotColumn reducedRow laterPivots

rowLeadingColumn :: SparseRow -> Maybe Int
rowLeadingColumn = fmap fst . IntMap.lookupMin

normalizeSparseRow :: Int -> SparseRow -> SparseRow
normalizeSparseRow pivotColumn rowValue =
  case IntMap.lookup pivotColumn rowValue of
    Nothing -> rowValue
    Just pivotCoefficient
      | pivotCoefficient == 0 -> rowValue
      | otherwise ->
          IntMap.mapMaybe
            ( \coefficient ->
                let normalizedCoefficient = coefficient / pivotCoefficient
                 in if normalizedCoefficient == 0
                      then Nothing
                      else Just normalizedCoefficient
            )
            rowValue

eliminateColumnFromRow :: Int -> SparseRow -> SparseRow -> SparseRow
eliminateColumnFromRow pivotColumn pivotRow rowValue =
  case IntMap.lookup pivotColumn rowValue of
    Nothing -> rowValue
    Just coefficient
      | coefficient == 0 -> rowValue
      | otherwise ->
          IntMap.mergeWithKey
            ( \_ leftValue rightValue ->
                nonZeroSparseCoefficient (leftValue - coefficient * rightValue)
            )
            (IntMap.filter (/= 0))
            (IntMap.mapMaybe (nonZeroSparseCoefficient . negate . (* coefficient)))
            rowValue
            pivotRow

compactSparseRow :: SparseRow -> SparseRow
compactSparseRow = IntMap.filter (/= 0)

nonZeroSparseCoefficient :: Rational -> Maybe Rational
nonZeroSparseCoefficient coefficient =
  if coefficient == 0
    then Nothing
    else Just coefficient

sparseRowLookup :: Int -> SparseRow -> Rational
sparseRowLookup columnIndex rowValue =
  IntMap.findWithDefault 0 columnIndex rowValue

sparseKernelBasisOf :: Int -> SparseMatrix -> [SparseRow]
sparseKernelBasisOf ambientDimension matrix =
  sparseKernelBasisFromRref ambientDimension (sparseRref matrix)

sparseKernelBasisFromRref :: Int -> SparseRref -> [SparseRow]
sparseKernelBasisFromRref ambientDimension reduced =
  if ambientDimension <= 0
    then []
    else
      let pivotColumnSet = IntMap.fromList (fmap (\(columnIndex, _) -> (columnIndex, ())) (srrefPivots reduced))
          freeColumns =
            enumerateFromZero ambientDimension
              & filter (\columnIndex -> not (IntMap.member columnIndex pivotColumnSet))
       in fmap (sparseKernelVector reduced) freeColumns

sparseKernelVector :: SparseRref -> Int -> SparseRow
sparseKernelVector reduced freeColumn =
  let pivotContributions =
        srrefPivots reduced
          & fmap
            ( \(pivotColumn, rowValue) ->
                (pivotColumn, negate (sparseRowLookup freeColumn rowValue))
            )
          & filter (\(_, coefficient) -> coefficient /= 0)
   in IntMap.insert freeColumn 1 (IntMap.fromList pivotContributions)

sparseImageBasisOf :: SparseMatrix -> [SparseRow]
sparseImageBasisOf matrix =
  sparseImageBasisFromRref matrix (sparseRref matrix)

sparseImageBasisFromRref :: SparseMatrix -> SparseRref -> [SparseRow]
sparseImageBasisFromRref matrix reduced =
  let columnCount = smColumnCount matrix
      validPivotColumns =
        srrefPivots reduced
          & fmap fst
          & filter (< columnCount)
      pivotColumnSet = IntSet.fromList validPivotColumns
      selectedColumns =
        smRows matrix
          & zip [0 :: Int ..]
          & List.foldl'
            ( \columnBuckets (rowIndex, rowValue) ->
                IntMap.foldlWithKey'
                  ( \innerBuckets columnIndex coefficient ->
                      if coefficient == 0 || not (IntSet.member columnIndex pivotColumnSet)
                        then innerBuckets
                        else
                          IntMap.insertWith
                            (IntMap.unionWith (+))
                            columnIndex
                            (IntMap.singleton rowIndex coefficient)
                            innerBuckets
                  )
                  columnBuckets
                  rowValue
            )
            IntMap.empty
   in fmap
        (\columnIndex -> IntMap.findWithDefault IntMap.empty columnIndex selectedColumns)
        validPivotColumns

sparseIndependentModulo :: Int -> [SparseRow] -> [SparseRow] -> [SparseRow]
sparseIndependentModulo ambientDimension imageBasis kernelBasis =
  sparseIndependentModuloWithBasis (sparseEchelonBasis ambientDimension imageBasis) kernelBasis

sparseIndependentModuloWithBasis :: SparseEchelonBasis -> [SparseRow] -> [SparseRow]
sparseIndependentModuloWithBasis spanBasis kernelBasis =
  let initialSelection =
        SparseModuloSelection
          { smsSpanBasis = spanBasis,
            smsSelectedRows = []
          }
   in kernelBasis
        & List.foldl' selectIndependentModulo initialSelection
        & reverse . smsSelectedRows

type SparseModuloSelection :: Type
data SparseModuloSelection = SparseModuloSelection
  { smsSpanBasis :: !SparseEchelonBasis,
    smsSelectedRows :: ![SparseRow]
  }
  deriving stock (Eq, Show)

selectIndependentModulo :: SparseModuloSelection -> SparseRow -> SparseModuloSelection
selectIndependentModulo selection candidateVector =
  case adjoinSparseEchelonRow (smsSpanBasis selection) candidateVector of
    (Nothing, unchangedBasis) ->
      selection {smsSpanBasis = unchangedBasis}
    (Just _residualVector, extendedBasis) ->
      SparseModuloSelection
        { smsSpanBasis = extendedBasis,
          smsSelectedRows = candidateVector : smsSelectedRows selection
        }

sparseEchelonBasis :: Int -> [SparseRow] -> SparseEchelonBasis
sparseEchelonBasis ambientDimension =
  List.foldl'
    ( \basis rowValue ->
        snd (adjoinSparseEchelonRow basis rowValue)
    )
    (emptySparseEchelonBasis ambientDimension)

emptySparseEchelonBasis :: Int -> SparseEchelonBasis
emptySparseEchelonBasis ambientDimension =
  SparseEchelonBasis
    { sebColumnCount = ambientDimension,
      sebPivotRows = IntMap.empty
    }

sparseEchelonContains :: SparseEchelonBasis -> SparseRow -> Bool
sparseEchelonContains basis =
  IntMap.null . reduceSparseEchelonRow basis

sparseEchelonRank :: SparseEchelonBasis -> Int
sparseEchelonRank =
  IntMap.size . sebPivotRows

adjoinSparseEchelonRow :: SparseEchelonBasis -> SparseRow -> (Maybe SparseRow, SparseEchelonBasis)
adjoinSparseEchelonRow basis rowValue =
  let residualRow =
        reduceSparseEchelonRow basis rowValue
   in case rowLeadingColumn residualRow of
        Nothing ->
          (Nothing, basis)
        Just pivotColumn ->
          let pivotRow =
                normalizeSparseRow pivotColumn residualRow
           in ( Just pivotRow,
                basis {sebPivotRows = IntMap.insert pivotColumn pivotRow (sebPivotRows basis)}
              )

reduceSparseEchelonRow :: SparseEchelonBasis -> SparseRow -> SparseRow
reduceSparseEchelonRow basis rowValue =
  IntMap.foldlWithKey'
    ( \residualRow pivotColumn pivotRow ->
        eliminateColumnFromRow pivotColumn pivotRow residualRow
    )
    rowValue
    (sebPivotRows basis)

sparseSpanRank :: Int -> [SparseRow] -> Int
sparseSpanRank ambientDimension vectorList =
  sparseEchelonRank (sparseEchelonBasis ambientDimension vectorList)

sparseCoordinateBasis :: Int -> [SparseRow] -> SparseCoordinateBasis
sparseCoordinateBasis ambientDimension generatorRows =
  generatorRows
    & zip [0 :: Int ..]
    & List.foldl'
      adjoinSparseCoordinateGenerator
      SparseCoordinateBasis
        { scbAmbientDimension = ambientDimension,
          scbGeneratorCount = length generatorRows,
          scbPivotRows = IntMap.empty
        }

adjoinSparseCoordinateGenerator :: SparseCoordinateBasis -> (Int, SparseRow) -> SparseCoordinateBasis
adjoinSparseCoordinateGenerator basis (generatorIndex, generatorRow) =
  let residual =
        reduceSparseCoordinateGenerator
          basis
          SparseCoordinateResidual
            { scrVector = compactSparseRow generatorRow,
              scrCoordinates = IntMap.singleton generatorIndex 1
            }
   in case rowLeadingColumn (scrVector residual) of
        Nothing -> basis
        Just pivotColumn ->
          let pivot =
                normalizeSparseCoordinatePivot
                  pivotColumn
                  residual
           in basis
                { scbPivotRows =
                    IntMap.insert pivotColumn pivot (scbPivotRows basis)
                }

reduceSparseCoordinateGenerator ::
  SparseCoordinateBasis ->
  SparseCoordinateResidual ->
  SparseCoordinateResidual
reduceSparseCoordinateGenerator basis residual =
  IntMap.foldlWithKey'
    eliminateCoordinateGeneratorPivot
    residual
    (scbPivotRows basis)

eliminateCoordinateGeneratorPivot ::
  SparseCoordinateResidual ->
  Int ->
  SparseCoordinatePivot ->
  SparseCoordinateResidual
eliminateCoordinateGeneratorPivot residual pivotColumn pivot =
  case IntMap.lookup pivotColumn (scrVector residual) of
    Nothing -> residual
    Just coefficient
      | coefficient == 0 -> residual
      | otherwise ->
          SparseCoordinateResidual
            { scrVector =
                eliminateColumnFromRow pivotColumn (scpVector pivot) (scrVector residual),
              scrCoordinates =
                addScaledSparseRow
                  (negate coefficient)
                  (scrCoordinates residual)
                  (scpCoordinates pivot)
            }

normalizeSparseCoordinatePivot ::
  Int ->
  SparseCoordinateResidual ->
  SparseCoordinatePivot
normalizeSparseCoordinatePivot pivotColumn residual =
  case IntMap.lookup pivotColumn (scrVector residual) of
    Nothing ->
      SparseCoordinatePivot
        { scpVector = scrVector residual,
          scpCoordinates = scrCoordinates residual
        }
    Just pivotCoefficient
      | pivotCoefficient == 0 ->
          SparseCoordinatePivot
            { scpVector = scrVector residual,
              scpCoordinates = scrCoordinates residual
            }
      | otherwise ->
          SparseCoordinatePivot
            { scpVector = scaleSparseRow (recip pivotCoefficient) (scrVector residual),
              scpCoordinates = scaleSparseRow (recip pivotCoefficient) (scrCoordinates residual)
            }

sparseCoordinatesInBasis :: SparseCoordinateBasis -> SparseRow -> Maybe SparseRow
sparseCoordinatesInBasis basis rowValue =
  let residual =
        IntMap.foldlWithKey'
          eliminateCoordinateCandidatePivot
          SparseCoordinateResidual
            { scrVector = compactSparseRow rowValue,
              scrCoordinates = IntMap.empty
            }
          (scbPivotRows basis)
   in if IntMap.null (scrVector residual)
        then Just (scrCoordinates residual)
        else Nothing

eliminateCoordinateCandidatePivot ::
  SparseCoordinateResidual ->
  Int ->
  SparseCoordinatePivot ->
  SparseCoordinateResidual
eliminateCoordinateCandidatePivot residual pivotColumn pivot =
  case IntMap.lookup pivotColumn (scrVector residual) of
    Nothing -> residual
    Just coefficient
      | coefficient == 0 -> residual
      | otherwise ->
          SparseCoordinateResidual
            { scrVector =
                eliminateColumnFromRow pivotColumn (scpVector pivot) (scrVector residual),
              scrCoordinates =
                addScaledSparseRow
                  coefficient
                  (scrCoordinates residual)
                  (scpCoordinates pivot)
            }

scaleSparseRow :: Rational -> SparseRow -> SparseRow
scaleSparseRow scalarValue =
  if scalarValue == 0
    then const IntMap.empty
    else
      IntMap.mapMaybe
        ( \coefficient ->
            nonZeroSparseCoefficient (scalarValue * coefficient)
        )

addScaledSparseRow :: Rational -> SparseRow -> SparseRow -> SparseRow
addScaledSparseRow scalarValue leftRow rightRow =
  IntMap.mergeWithKey
    ( \_ leftCoefficient rightCoefficient ->
        nonZeroSparseCoefficient (leftCoefficient + scalarValue * rightCoefficient)
    )
    (IntMap.filter (/= 0))
    (IntMap.mapMaybe (nonZeroSparseCoefficient . (scalarValue *)))
    leftRow
    rightRow

sparseMatrixFromRows :: Int -> [[Rational]] -> SparseMatrix
sparseMatrixFromRows columnCount rows =
  SparseMatrix
    { smRows = fmap sparseRowFromDense rows,
      smColumnCount = columnCount
    }

sparseRowFromDense :: [Rational] -> SparseRow
sparseRowFromDense values =
  values
    & zip [0 :: Int ..]
    & List.foldl'
      ( \rowValue (columnIndex, coefficient) ->
          if coefficient == 0
            then rowValue
            else IntMap.insert columnIndex coefficient rowValue
      )
      IntMap.empty

sparseRowToDense :: Int -> SparseRow -> [Rational]
sparseRowToDense ambientDimension rowValue =
  enumerateFromZero ambientDimension
    & fmap (\columnIndex -> sparseRowLookup columnIndex rowValue)
