{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Moonlight.Derived.Pure.Site.LabeledMatrix
  ( SparseMatrixEntry (..)
  , SparseMat (..)
  , sparseMatRows
  , sparseMatCols
  , sparseMatEntries
  , sparseMatToDense
  , denseToSparseMat
  , blockedToSparseMat
  , canonicalSparseMatEntries
  , sparseMatColumnEntries
  , DenseMat (..)
  , mkDenseMat
  , denseMatRows
  , denseMatCols
  , denseMatData
  , matShape
  , zeroMat
  , identMat
  , transposeMat
  , transposeMatChecked
  , matAdd
  , matAddChecked
  , matMul
  , matMulChecked
  , isZeroMat
  , matIndex
  , rowAt
  , setRow
  , submatrix
  , appendRowMat
  , swapRowsMat
  , scaleRowMat
  , addScaledRowMat
  , addScaledColMat
  , deleteRowsMat
  , deleteColsMat
  , hcat
  , hcatChecked
  , vcat
  , vcatChecked
  , blockCat
  , blockCatChecked
  , denseFromEntriesWith
  , denseFromEntriesWithChecked
  , entriesToBlockedMatWith
  , resolveEntriesToBlockedMatWith
  , denseFromEntriesGF2
  , entriesToBlockedMatGF2
  , entriesToBlockedMatGF2Checked
  , resolveEntriesToBlockedMatGF2
  , GroupedAxis (..)
  , groupedAxisOrder
  , groupedAxisMultiplicities
  , emptyAxis
  , fromLabels
  , axisLabelsExpanded
  , axisMultiplicity
  , axisSize
  , axisSlices
  , appendAxisLabel
  , restrictAxis
  , removeAxisIndices
  , relabelAxis
  , relabelOffsets
  , BlockedMat (..)
  , blockedMatRows
  , blockedMatCols
  , blockedMatBlocks
  , zeroBlocked
  , copyRowsInto
  , axisEmpty
  , vectorAtMaybe
  , transposeBlockedMat
  , storedBlockAt
  , blockAt
  , setBlock
  , setBlockChecked
  , modifyBlock
  , composeBlocked
  , composeBlockedIsZero
  , restrictBlocked
  , relabelBlocked
  , fromExpanded
  , fromExpandedChecked
  , expandBlocked
  , collapseBlockedDense
  , starView
  , appendRowOnLabel
  , appendRowsOnLabel
  , removeRowsOnLabel
  , removeColsOnLabel
  , leftMultiplyRowLabel
  , rightMultiplyColLabel
  , rowOp
  , colOp
  , splitByWidths
  ) where

import Control.Monad (foldM)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Moonlight.Core (MoonlightError (..))
import Moonlight.Core (dedupStableOn)
import Moonlight.Core (scanMap)
import Moonlight.Derived.Pure.Failure (DerivedFailure (..))
import Moonlight.LinAlg.Dense.GF2 (GF2, gf2FromBool)

import Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset
  , FinObjectId (..)
  , star
  )

type DenseMat :: Type -> Type
data DenseMat a = DenseMat
  { dmRows :: !Int
  , dmCols :: !Int
  , dmData :: !(Vector (Vector a))
  } deriving stock (Eq, Show)

type SparseMatrixEntry :: Type -> Type
data SparseMatrixEntry a = SparseMatrixEntry
  { smeRow :: !Int
  , smeColumn :: !Int
  , smeValue :: !a
  } deriving stock (Eq, Show)

type SparseMat :: Type -> Type
data SparseMat a = SparseMat
  { smRows :: !Int
  , smCols :: !Int
  , smEntries :: ![SparseMatrixEntry a]
  } deriving stock (Eq, Show)

sparseMatRows :: SparseMat a -> Int
sparseMatRows =
  smRows

sparseMatCols :: SparseMat a -> Int
sparseMatCols =
  smCols

sparseMatEntries :: SparseMat a -> [SparseMatrixEntry a]
sparseMatEntries =
  smEntries

sparseMatToDense :: Num a => SparseMat a -> DenseMat a
sparseMatToDense SparseMat {smRows, smCols, smEntries} =
  denseFromEntriesWith
    smRows
    smCols
    smEntries
    (\SparseMatrixEntry {smeRow, smeColumn} -> (smeRow, smeColumn))
    smeValue

denseToSparseMat :: (Eq a, Num a) => DenseMat a -> SparseMat a
denseToSparseMat denseMat@DenseMat {dmRows, dmCols} =
  SparseMat
    { smRows = dmRows
    , smCols = dmCols
    , smEntries = denseNonZeroEntries denseMat
    }

blockedToSparseMat :: forall a. (Eq a, Num a) => BlockedMat a -> SparseMat a
blockedToSparseMat blockedMat@BlockedMat {bmRows, bmCols} =
  SparseMat
    { smRows = axisSize bmRows
    , smCols = axisSize bmCols
    , smEntries =
        concatMap
          blockRowEntries
          (IM.toAscList (bmBlocks blockedMat))
    }
  where
    rowOffsets =
      axisSlices bmRows

    columnOffsets =
      axisSlices bmCols

    blockRowEntries (rowKey, rowMap) =
      case IM.lookup rowKey rowOffsets of
        Nothing ->
          []
        Just (rowOffset, _) ->
          concatMap
            (blockEntriesAt rowOffset)
            (IM.toAscList rowMap)

    blockEntriesAt rowOffset (columnKey, blockValue) =
      case IM.lookup columnKey columnOffsets of
        Nothing ->
          []
        Just (columnOffset, _) ->
          fmap
            (offsetSparseEntry rowOffset columnOffset)
            (denseNonZeroEntries blockValue)

    offsetSparseEntry :: Int -> Int -> SparseMatrixEntry a -> SparseMatrixEntry a
    offsetSparseEntry rowOffset columnOffset SparseMatrixEntry {smeRow, smeColumn, smeValue} =
      SparseMatrixEntry
        { smeRow = rowOffset + smeRow
        , smeColumn = columnOffset + smeColumn
        , smeValue
        }

canonicalSparseMatEntries :: forall a. (Eq a, Num a) => SparseMat a -> [SparseMatrixEntry a]
canonicalSparseMatEntries SparseMat {smEntries} =
  fmap renderEntry
    (Map.toAscList (Map.filter (/= 0) accumulatedEntries))
  where
    accumulatedEntries =
      foldl'
        ( \acc SparseMatrixEntry {smeRow, smeColumn, smeValue} ->
            Map.insertWith (+) (smeRow, smeColumn) smeValue
              acc
        )
        Map.empty
        smEntries

    renderEntry :: ((Int, Int), a) -> SparseMatrixEntry a
    renderEntry ((rowIndexValue, columnIndexValue), coefficientValue) =
      SparseMatrixEntry
        { smeRow = rowIndexValue
        , smeColumn = columnIndexValue
        , smeValue = coefficientValue
        }

sparseMatColumnEntries :: forall a. (Eq a, Num a) => SparseMat a -> Vector [(Int, a)]
sparseMatColumnEntries sparseMat@SparseMat {smCols} =
  V.generate
    smCols
    ( \columnIndexValue ->
        maybe
          []
          IM.toAscList
          (IM.lookup columnIndexValue entriesByColumn)
    )
  where
    entriesByColumn =
      foldl'
        insertEntry
        IM.empty
        (canonicalSparseMatEntries sparseMat)

    insertEntry :: IntMap (IntMap a) -> SparseMatrixEntry a -> IntMap (IntMap a)
    insertEntry entries SparseMatrixEntry {smeRow, smeColumn, smeValue} =
      IM.alter
        (Just . IM.insert smeRow smeValue . fromMaybe IM.empty)
        smeColumn
        entries

denseNonZeroEntries :: (Eq a, Num a) => DenseMat a -> [SparseMatrixEntry a]
denseNonZeroEntries DenseMat {dmData} =
  [ SparseMatrixEntry
      { smeRow = rowIndexValue
      , smeColumn = columnIndexValue
      , smeValue = coefficientValue
      }
  | (rowIndexValue, rowVector) <- zip [0 :: Int ..] (V.toList dmData)
  , (columnIndexValue, coefficientValue) <- zip [0 :: Int ..] (V.toList rowVector)
  , coefficientValue /= 0
  ]

mkDenseMat :: Int -> Int -> Vector (Vector a) -> Either DerivedFailure (DenseMat a)
mkDenseMat rowCount columnCount rowValues
  | rowCount < 0 || columnCount < 0 =
      Left (DerivedMatrixShapeMismatch "mkDenseMat" (max 0 rowCount, max 0 columnCount) (rowCount, columnCount))
  | payloadShape == metadataShape =
      Right (DenseMat rowCount columnCount rowValues)
  | otherwise =
      Left (DerivedMatrixMetadataMismatch "mkDenseMat" metadataShape payloadShape)
  where
    metadataShape = (rowCount, columnCount)
    payloadShape = densePayloadShape rowValues

matShape :: DenseMat a -> (Int, Int)
matShape denseMat =
  (dmRows denseMat, dmCols denseMat)

denseMatRows :: DenseMat a -> Int
denseMatRows =
  dmRows

denseMatCols :: DenseMat a -> Int
denseMatCols =
  dmCols

denseMatData :: DenseMat a -> Vector (Vector a)
denseMatData =
  dmData

zeroMat :: Num a => Int -> Int -> DenseMat a
zeroMat r c = DenseMat r c (V.replicate r (V.replicate c 0))

identMat :: Num a => Int -> DenseMat a
identMat n = DenseMat n n $ V.generate n $ \i ->
  V.generate n $ \j -> if i == j then 1 else 0

-- | Precondition: row and column indices lie within the stored shape; unchecked; use 'vectorAtMaybe' for total vector access.
matIndex :: DenseMat a -> Int -> Int -> a
matIndex DenseMat{dmData} i j = (dmData V.! i) V.! j

-- | Precondition: the row index lies within the stored shape; unchecked; use 'vectorAtMaybe' for total vector access.
rowAt :: DenseMat a -> Int -> Vector a
rowAt DenseMat{dmData} i = dmData V.! i

colAt :: DenseMat a -> Int -> Vector a
colAt m j = V.generate (dmRows m) (\i -> matIndex m i j)

-- | Precondition: the row index is in bounds and the replacement has exactly the recorded column count; unchecked.
setRow :: Int -> Vector a -> DenseMat a -> DenseMat a
setRow i row m = m { dmData = dmData m V.// [(i, row)] }

transposeMat :: DenseMat a -> DenseMat a
transposeMat m =
  DenseMat
    (dmCols m)
    (dmRows m)
    ( V.generate
        (dmCols m)
        (\columnIndexValue -> V.generate (dmRows m) (\rowIndexValue -> matIndex m rowIndexValue columnIndexValue))
    )

transposeMatChecked :: DenseMat a -> Either DerivedFailure (DenseMat a)
transposeMatChecked denseMat =
  validateDenseMat "transposeMat" denseMat *> Right (transposeMat denseMat)

isZeroMat :: (Eq a, Num a) => DenseMat a -> Bool
isZeroMat DenseMat{dmData} = V.all (V.all (== 0)) dmData

matAdd :: Num a => DenseMat a -> DenseMat a -> DenseMat a
matAdd a b =
  DenseMat
    (dmRows a)
    (dmCols a)
    ( V.generate
        (dmRows a)
        ( \rowIndexValue ->
            V.generate
              (dmCols a)
              ( \columnIndexValue ->
                  matIndex a rowIndexValue columnIndexValue
                    + matIndex b rowIndexValue columnIndexValue
              )
        )
    )

matAddChecked :: Num a => DenseMat a -> DenseMat a -> Either DerivedFailure (DenseMat a)
matAddChecked leftMat rightMat =
  validateDenseMat "matAdd left" leftMat
    *> validateDenseMat "matAdd right" rightMat
    *> if matShape leftMat == matShape rightMat
      then Right (matAdd leftMat rightMat)
      else Left (DerivedMatrixShapeMismatch "matAdd" (matShape leftMat) (matShape rightMat))

matMul :: Num a => DenseMat a -> DenseMat a -> DenseMat a
matMul a b =
  DenseMat
    (dmRows a)
    (dmCols b)
    ( V.generate
        (dmRows a)
        ( \rowIndexValue ->
            V.generate
              (dmCols b)
              ( \columnIndexValue ->
                  sum
                    [ matIndex a rowIndexValue innerIndexValue
                        * matIndex b innerIndexValue columnIndexValue
                    | innerIndexValue <- [0 .. dmCols a - 1]
                    ]
              )
        )
    )

matMulChecked :: Num a => DenseMat a -> DenseMat a -> Either DerivedFailure (DenseMat a)
matMulChecked leftMat rightMat =
  validateDenseMat "matMul left" leftMat
    *> validateDenseMat "matMul right" rightMat
    *> if dmCols leftMat == dmRows rightMat
      then Right (matMul leftMat rightMat)
      else Left (DerivedMatrixShapeMismatch "matMul" (matShape leftMat) (matShape rightMat))

-- | Precondition: every requested row and column index lies within the stored shape; unchecked.
submatrix :: [Int] -> [Int] -> DenseMat a -> DenseMat a
submatrix rs cs m = DenseMat (length rs) (length cs) $
  V.fromList [ V.fromList [ matIndex m i j | j <- cs ] | i <- rs ]

appendRowMat :: Vector a -> DenseMat a -> DenseMat a
appendRowMat row m = m { dmRows = dmRows m + 1, dmData = dmData m `V.snoc` row }

appendRowsMat :: Vector (Vector a) -> DenseMat a -> DenseMat a
appendRowsMat rowsValue m =
  m
    { dmRows = dmRows m + V.length rowsValue
    , dmData = dmData m <> rowsValue
    }

-- | Precondition: both row indices lie within the stored shape; unchecked.
swapRowsMat :: Int -> Int -> DenseMat a -> DenseMat a
swapRowsMat i j m
  | i == j = m
  | otherwise =
      let ri = rowAt m i; rj = rowAt m j
      in m { dmData = dmData m V.// [(i, rj), (j, ri)] }

-- | Precondition: the row index lies within the stored shape; unchecked.
scaleRowMat :: Num a => Int -> a -> DenseMat a -> DenseMat a
scaleRowMat i k m = setRow i (V.map (k *) (rowAt m i)) m

-- | Precondition: source and destination row indices lie within the stored shape; unchecked.
addScaledRowMat :: Num a => Int -> a -> Int -> DenseMat a -> DenseMat a
addScaledRowMat dst alpha src m =
  setRow dst (V.zipWith (+) (rowAt m dst) (V.map (alpha *) (rowAt m src))) m

-- | Precondition: source and destination column indices lie within the stored shape; unchecked.
addScaledColMat :: Num a => Int -> a -> Int -> DenseMat a -> DenseMat a
addScaledColMat dst alpha src m =
  m { dmData = V.map (\row -> row V.// [(dst, (row V.! dst) + alpha * (row V.! src))]) (dmData m) }

deleteRowsMat :: [Int] -> DenseMat a -> DenseMat a
deleteRowsMat del m =
  let delSet = IS.fromList del
      keep = [ i | i <- [0 .. dmRows m - 1], not (IS.member i delSet) ]
  in submatrix keep [0 .. dmCols m - 1] m

deleteColsMat :: [Int] -> DenseMat a -> DenseMat a
deleteColsMat del m =
  let delSet = IS.fromList del
      keep = [ j | j <- [0 .. dmCols m - 1], not (IS.member j delSet) ]
  in submatrix [0 .. dmRows m - 1] keep m

hcat :: Num a => [DenseMat a] -> DenseMat a
hcat [] = zeroMat 0 0
hcat matrices =
  DenseMat
    rowCount
    columnCount
    ( V.generate
        rowCount
        ( \rowIndexValue ->
            V.concat (fmap (\matrixValue -> rowAt matrixValue rowIndexValue) matrices)
        )
    )
  where
    rowCount =
      expectedSharedRows matrices
    columnCount =
      sum (fmap dmCols matrices)

hcatChecked :: Num a => [DenseMat a] -> Either DerivedFailure (DenseMat a)
hcatChecked matrices =
  traverse (validateDenseMat "hcat input") matrices
    *> if compatibleRowCounts matrices
      then Right (hcat matrices)
      else Left (DerivedMatrixShapeMismatch "hcat" (expectedSharedRows matrices, 0) (actualSharedRows matrices, 0))

vcat :: Num a => [DenseMat a] -> DenseMat a
vcat [] = zeroMat 0 0
vcat matrices =
  DenseMat
    rowCount
    columnCount
    (V.concat (fmap dmData matrices))
  where
    rowCount =
      sum (fmap dmRows matrices)
    columnCount =
      expectedSharedColumns matrices

vcatChecked :: Num a => [DenseMat a] -> Either DerivedFailure (DenseMat a)
vcatChecked matrices =
  traverse (validateDenseMat "vcat input") matrices
    *> if compatibleColumnCounts matrices
      then Right (vcat matrices)
      else Left (DerivedMatrixShapeMismatch "vcat" (0, expectedSharedColumns matrices) (0, actualSharedColumns matrices))

blockCat :: Num a => [[DenseMat a]] -> DenseMat a
blockCat [] = zeroMat 0 0
blockCat rows = vcat (map hcat rows)

blockCatChecked :: Num a => [[DenseMat a]] -> Either DerivedFailure (DenseMat a)
blockCatChecked rows =
  traverse hcatChecked rows >>= vcatChecked

denseFromEntriesWith ::
  Num a =>
  Int ->
  Int ->
  [entry] ->
  (entry -> (Int, Int)) ->
  (entry -> a) ->
  DenseMat a
denseFromEntriesWith rowCount colCount entries locateEntry entryCoefficient =
  DenseMat
    rowCount
    colCount
    ( V.generate
        rowCount
        ( \rowIndexValue ->
            V.generate
              colCount
              ( \columnIndexValue ->
                  Map.findWithDefault
                    0
                    (rowIndexValue, columnIndexValue)
                    entryMap
              )
        )
    )
  where
    entryMap =
      foldl'
        ( \acc entryValue ->
            Map.insertWith
              (+)
              (locateEntry entryValue)
              (entryCoefficient entryValue)
              acc
        )
        Map.empty
        entries

denseFromEntriesWithChecked ::
  Num a =>
  Int ->
  Int ->
  [entry] ->
  (entry -> (Int, Int)) ->
  (entry -> a) ->
  Either DerivedFailure (DenseMat a)
denseFromEntriesWithChecked rowCount columnCount entries locateEntry entryCoefficient =
  validateShapeNonnegative "denseFromEntriesWith" rowCount columnCount
    *> traverse_ validateEntry entries
    *> Right (denseFromEntriesWith rowCount columnCount entries locateEntry entryCoefficient)
  where
    validateEntry entryValue =
      let (rowIndexValue, columnIndexValue) =
            locateEntry entryValue
       in validateIndex "denseFromEntriesWith row" rowCount rowIndexValue
            *> validateIndex "denseFromEntriesWith column" columnCount columnIndexValue

entriesToBlockedMatWith ::
  (Eq a, Num a) =>
  (rowCell -> FinObjectId) ->
  (colCell -> FinObjectId) ->
  [rowCell] ->
  [colCell] ->
  Int ->
  Int ->
  [entry] ->
  (entry -> (Int, Int)) ->
  (entry -> a) ->
  BlockedMat a
entriesToBlockedMatWith resolveRow resolveCol rowCells colCells rowCount colCount entries locateEntry entryCoefficient =
  fromExpanded
    (V.fromList (fmap resolveRow rowCells))
    (V.fromList (fmap resolveCol colCells))
    (denseFromEntriesWith rowCount colCount entries locateEntry entryCoefficient)

resolveEntriesToBlockedMatWith ::
  (Eq a, Num a, Applicative f) =>
  (rowCell -> f FinObjectId) ->
  (colCell -> f FinObjectId) ->
  [rowCell] ->
  [colCell] ->
  Int ->
  Int ->
  [entry] ->
  (entry -> (Int, Int)) ->
  (entry -> a) ->
  f (BlockedMat a)
resolveEntriesToBlockedMatWith resolveRow resolveCol rowCells colCells rowCount colCount entries locateEntry entryCoefficient =
  fromExpanded
    <$> (V.fromList <$> traverse resolveRow rowCells)
    <*> (V.fromList <$> traverse resolveCol colCells)
    <*> pure (denseFromEntriesWith rowCount colCount entries locateEntry entryCoefficient)

denseFromEntriesGF2 ::
  Int ->
  Int ->
  [entry] ->
  (entry -> (Int, Int)) ->
  (entry -> Bool) ->
  DenseMat GF2
denseFromEntriesGF2 rowCount colCount entries locateEntry entryIsOdd =
  denseFromEntriesWith
    rowCount
    colCount
    entries
    locateEntry
    (gf2FromBool . entryIsOdd)

entriesToBlockedMatGF2 ::
  (rowCell -> FinObjectId) ->
  (colCell -> FinObjectId) ->
  [rowCell] ->
  [colCell] ->
  Int ->
  Int ->
  [entry] ->
  (entry -> (Int, Int)) ->
  (entry -> Bool) ->
  BlockedMat GF2
entriesToBlockedMatGF2 resolveRow resolveCol rowCells colCells rowCount colCount entries locateEntry entryIsOdd =
  entriesToBlockedMatWith
    resolveRow
    resolveCol
    rowCells
    colCells
    rowCount
    colCount
    entries
    locateEntry
    (gf2FromBool . entryIsOdd)

entriesToBlockedMatGF2Checked ::
  (rowCell -> FinObjectId) ->
  (colCell -> FinObjectId) ->
  [rowCell] ->
  [colCell] ->
  Int ->
  Int ->
  [entry] ->
  (entry -> (Int, Int)) ->
  (entry -> Bool) ->
  Either DerivedFailure (BlockedMat GF2)
entriesToBlockedMatGF2Checked resolveRow resolveCol rowCells colCells rowCount colCount entries locateEntry entryIsOdd =
  denseFromEntriesWithChecked rowCount colCount entries locateEntry (gf2FromBool . entryIsOdd)
    >>= fromExpandedChecked
      (V.fromList (fmap resolveRow rowCells))
      (V.fromList (fmap resolveCol colCells))

resolveEntriesToBlockedMatGF2 ::
  Applicative f =>
  (rowCell -> f FinObjectId) ->
  (colCell -> f FinObjectId) ->
  [rowCell] ->
  [colCell] ->
  Int ->
  Int ->
  [entry] ->
  (entry -> (Int, Int)) ->
  (entry -> Bool) ->
  f (BlockedMat GF2)
resolveEntriesToBlockedMatGF2 resolveRow resolveCol rowCells colCells rowCount colCount entries locateEntry entryIsOdd =
  resolveEntriesToBlockedMatWith
    resolveRow
    resolveCol
    rowCells
    colCells
    rowCount
    colCount
    entries
    locateEntry
    (gf2FromBool . entryIsOdd)

validateDenseMat :: String -> DenseMat a -> Either DerivedFailure ()
validateDenseMat context denseMat =
  validateShapeNonnegative context (dmRows denseMat) (dmCols denseMat)
    *> let metadataShape = matShape denseMat
           payloadShape = densePayloadShape (dmData denseMat)
           rowlessVacuous = dmRows denseMat == 0 && V.null (dmData denseMat)
        in if metadataShape == payloadShape || rowlessVacuous
             then Right ()
             else Left (DerivedMatrixMetadataMismatch context metadataShape payloadShape)

validateShapeNonnegative :: String -> Int -> Int -> Either DerivedFailure ()
validateShapeNonnegative context rowCount columnCount
  | rowCount < 0 || columnCount < 0 =
      Left (DerivedMatrixShapeMismatch context (max 0 rowCount, max 0 columnCount) (rowCount, columnCount))
  | otherwise =
      Right ()

validateIndex :: String -> Int -> Int -> Either DerivedFailure ()
validateIndex context axisCardinality axisIndex
  | axisIndex < 0 =
      Left (DerivedMatrixOutOfBounds context axisCardinality axisIndex)
  | axisIndex >= axisCardinality =
      Left (DerivedMatrixOutOfBounds context axisCardinality axisIndex)
  | otherwise =
      Right ()

densePayloadShape :: Vector (Vector a) -> (Int, Int)
densePayloadShape rowValues =
  case V.toList (V.map V.length rowValues) of
    [] -> (0, 0)
    firstWidth : widths
      | all (== firstWidth) widths -> (V.length rowValues, firstWidth)
      | otherwise -> (V.length rowValues, -1)

compatibleRowCounts :: [DenseMat a] -> Bool
compatibleRowCounts matrices =
  case fmap dmRows matrices of
    [] -> True
    firstRows : restRows -> all (== firstRows) restRows

compatibleColumnCounts :: [DenseMat a] -> Bool
compatibleColumnCounts matrices =
  case fmap dmCols matrices of
    [] -> True
    firstColumns : restColumns -> all (== firstColumns) restColumns

expectedSharedRows :: [DenseMat a] -> Int
expectedSharedRows matrices =
  case matrices of
    [] -> 0
    firstMatrix : _ -> dmRows firstMatrix

actualSharedRows :: [DenseMat a] -> Int
actualSharedRows =
  maybe 0 id . firstDifferent . fmap dmRows

expectedSharedColumns :: [DenseMat a] -> Int
expectedSharedColumns matrices =
  case matrices of
    [] -> 0
    firstMatrix : _ -> dmCols firstMatrix

actualSharedColumns :: [DenseMat a] -> Int
actualSharedColumns =
  maybe 0 id . firstDifferent . fmap dmCols

firstDifferent :: Eq value => [value] -> Maybe value
firstDifferent values =
  case values of
    [] -> Nothing
    firstValue : restValues ->
      safeHead (filter (/= firstValue) restValues)

safeHead :: [value] -> Maybe value
safeHead values =
  case values of
    [] -> Nothing
    firstValue : _ -> Just firstValue

type GroupedAxis :: Type
data GroupedAxis = GroupedAxis
  { gaOrder :: !(Vector FinObjectId)
  , gaMult  :: !(IntMap Int)
  } deriving stock (Eq, Show)

groupedAxisOrder :: GroupedAxis -> Vector FinObjectId
groupedAxisOrder =
  gaOrder

groupedAxisMultiplicities :: GroupedAxis -> IntMap Int
groupedAxisMultiplicities =
  gaMult

emptyAxis :: GroupedAxis
emptyAxis = GroupedAxis V.empty IM.empty

fromLabels :: Vector FinObjectId -> GroupedAxis
fromLabels labels = GroupedAxis order mult
  where
    order = V.fromList (dedupStableOn unFinObjectId (V.toList labels))
    mult  = V.foldl' (\acc (FinObjectId objectKey) -> IM.insertWith (+) objectKey 1 acc) IM.empty labels

axisLabelsExpanded :: GroupedAxis -> Vector FinObjectId
axisLabelsExpanded ga = V.fromList $ concatMap expandOne (V.toList (gaOrder ga))
  where expandOne x = replicate (axisMultiplicity ga x) x

axisMultiplicity :: GroupedAxis -> FinObjectId -> Int
axisMultiplicity GroupedAxis{gaMult} (FinObjectId objectKey) = IM.findWithDefault 0 objectKey gaMult

axisSize :: GroupedAxis -> Int
axisSize GroupedAxis{gaMult} = IM.foldl' (+) 0 gaMult

axisSlices :: GroupedAxis -> IntMap (Int, Int)
axisSlices GroupedAxis{gaOrder,gaMult} =
  IM.fromList (V.toList sliceEntries)
  where
    (_, sliceEntries) = scanMap step 0 gaOrder
    step off (FinObjectId objectKey) =
      let multiplicity = IM.findWithDefault 0 objectKey gaMult
       in (off + multiplicity, (objectKey, (off, multiplicity)))

appendAxisLabel :: FinObjectId -> Int -> GroupedAxis -> GroupedAxis
appendAxisLabel lab@(FinObjectId objectKey) k ga@GroupedAxis{gaOrder,gaMult}
  | k <= 0 = ga
  | axisMultiplicity ga lab == 0 = GroupedAxis (gaOrder `V.snoc` lab) (IM.insert objectKey k gaMult)
  | otherwise = GroupedAxis gaOrder (IM.adjust (+ k) objectKey gaMult)

restrictAxis :: IntSet -> GroupedAxis -> GroupedAxis
restrictAxis keep GroupedAxis{gaOrder,gaMult} =
  GroupedAxis
    (V.filter (\(FinObjectId objectKey) -> IS.member objectKey keep) gaOrder)
    (IM.filterWithKey (\k _ -> IS.member k keep) gaMult)

removeAxisIndices :: FinObjectId -> [Int] -> GroupedAxis -> GroupedAxis
removeAxisIndices lab@(FinObjectId objectKey) idxs ga@GroupedAxis{gaOrder,gaMult} =
  let k = axisMultiplicity ga lab
      validIndices = IS.fromList (filter (\idx -> idx >= 0 && idx < k) idxs)
      k' = k - IS.size validIndices
  in if k' == 0
       then GroupedAxis (V.filter (/= lab) gaOrder) (IM.delete objectKey gaMult)
       else GroupedAxis gaOrder (IM.insert objectKey k' gaMult)

relabelAxis :: (FinObjectId -> FinObjectId) -> GroupedAxis -> GroupedAxis
relabelAxis f ga@GroupedAxis{gaOrder} =
  let mapped = map f (V.toList gaOrder)
      order' = V.fromList (dedupStableOn unFinObjectId mapped)
      mult'  = foldl' (\acc old -> let FinObjectId objectKey = f old
                in IM.insertWith (+) objectKey (axisMultiplicity ga old) acc) IM.empty (V.toList gaOrder)
  in GroupedAxis order' mult'

relabelOffsets :: (FinObjectId -> FinObjectId) -> GroupedAxis -> (GroupedAxis, IntMap (FinObjectId, Int))
relabelOffsets f ga@GroupedAxis{gaOrder} =
  let ((axis', _), offsetEntries) = scanMap step (emptyAxis, IM.empty) gaOrder
   in (axis', IM.fromList (V.toList offsetEntries))
  where
    step (accAxis, localOff) old =
        let new@(FinObjectId objectKey) = f old
            multOld = axisMultiplicity ga old
            startOff = IM.findWithDefault 0 objectKey localOff
            accAxis' = appendAxisLabel new multOld accAxis
            localOff' = IM.insert objectKey (startOff + multOld) localOff
        in ((accAxis', localOff'), (unFinObjectId old, (new, startOff)))

type BlockedMat :: Type -> Type
data BlockedMat a = BlockedMat
  { bmRows   :: !GroupedAxis
  , bmCols   :: !GroupedAxis
  , bmBlocks :: !(IntMap (IntMap (DenseMat a)))
  } deriving stock (Eq, Show)

blockedMatRows :: BlockedMat a -> GroupedAxis
blockedMatRows =
  bmRows

blockedMatCols :: BlockedMat a -> GroupedAxis
blockedMatCols =
  bmCols

blockedMatBlocks :: BlockedMat a -> IntMap (IntMap (DenseMat a))
blockedMatBlocks =
  bmBlocks

zeroBlocked :: GroupedAxis -> GroupedAxis -> BlockedMat a
zeroBlocked rows cols = BlockedMat rows cols IM.empty

copyRowsInto :: (Eq a, Num a) => GroupedAxis -> Maybe (BlockedMat a) -> Either MoonlightError (BlockedMat a)
copyRowsInto cols =
  maybe
    (Right (zeroBlocked emptyAxis cols))
    (\blockedMat -> IM.foldlWithKey' placeRow (Right (zeroBlocked (bmRows blockedMat) cols)) (bmBlocks blockedMat))
  where
    placeRow accE rowKey rowMap =
      accE >>= \acc -> IM.foldlWithKey' (placeBlock (FinObjectId rowKey)) (Right acc) rowMap

    placeBlock rowLabel accE colKey blockValue =
      accE >>= \acc ->
        let colLabel = FinObjectId colKey
            targetRowCount = axisMultiplicity (bmRows acc) rowLabel
            targetColCount = axisMultiplicity cols colLabel
        in if dmRows blockValue > targetRowCount || dmCols blockValue > targetColCount
             then Left (InvariantViolation "copyRowsInto: target axis is smaller than the source block")
             else
               Right
                 ( setBlock
                     rowLabel
                     colLabel
                     (embedInto targetRowCount targetColCount blockValue 0 0)
                     acc
                 )

axisEmpty :: GroupedAxis -> Bool
axisEmpty = (== 0) . axisSize

vectorAtMaybe :: Int -> Vector a -> Maybe a
vectorAtMaybe i v
  | i < 0 || i >= V.length v = Nothing
  | otherwise = Just (v V.! i)

transposeBlockedMat :: BlockedMat a -> BlockedMat a
transposeBlockedMat blockedMat =
  BlockedMat
    { bmRows = bmCols blockedMat
    , bmCols = bmRows blockedMat
    , bmBlocks =
        IM.foldlWithKey'
          transposeRow
          IM.empty
          (bmBlocks blockedMat)
    }
  where
    transposeRow :: IntMap (IntMap (DenseMat a)) -> Int -> IntMap (DenseMat a) -> IntMap (IntMap (DenseMat a))
    transposeRow acc rowKey rowMap =
      IM.foldlWithKey'
        (transposeBlock rowKey)
        acc
        rowMap

    transposeBlock :: Int -> IntMap (IntMap (DenseMat a)) -> Int -> DenseMat a -> IntMap (IntMap (DenseMat a))
    transposeBlock rowKey acc colKey blockValue =
      IM.insertWith
        IM.union
        colKey
        (IM.singleton rowKey (transposeMat blockValue))
        acc

rowLabels :: GroupedAxis -> [FinObjectId]
rowLabels = V.toList . gaOrder

colLabels :: GroupedAxis -> [FinObjectId]
colLabels = V.toList . gaOrder

storedBlockAt :: FinObjectId -> FinObjectId -> BlockedMat a -> Maybe (DenseMat a)
storedBlockAt (FinObjectId rowKey) (FinObjectId columnKey) BlockedMat {bmBlocks} =
  IM.lookup rowKey bmBlocks >>= IM.lookup columnKey

blockAt :: Num a => FinObjectId -> FinObjectId -> BlockedMat a -> DenseMat a
blockAt r c blockedMat@BlockedMat{bmRows,bmCols} =
  fromMaybe
    (zeroMat (axisMultiplicity bmRows r) (axisMultiplicity bmCols c))
    (storedBlockAt r c blockedMat)

setBlock :: (Eq a, Num a) => FinObjectId -> FinObjectId -> DenseMat a -> BlockedMat a -> BlockedMat a
setBlock (FinObjectId rowKey) (FinObjectId columnKey) blk bm@BlockedMat{bmBlocks} =
  let updateRow Nothing
        | isZeroMat blk = Nothing
        | otherwise     = Just (IM.singleton columnKey blk)
      updateRow (Just rowMap)
        | isZeroMat blk =
            let rowMap' = IM.delete columnKey rowMap
            in if IM.null rowMap' then Nothing else Just rowMap'
        | otherwise = Just (IM.insert columnKey blk rowMap)
  in bm { bmBlocks = IM.alter updateRow rowKey bmBlocks }

setBlockChecked :: (Eq a, Num a) => FinObjectId -> FinObjectId -> DenseMat a -> BlockedMat a -> Either DerivedFailure (BlockedMat a)
setBlockChecked rowLabel columnLabel blockValue blockedMat =
  validateDenseMat "setBlock" blockValue
    *> if matShape blockValue == expectedShape
      then Right (setBlock rowLabel columnLabel blockValue blockedMat)
      else Left (DerivedMatrixShapeMismatch "setBlock" expectedShape (matShape blockValue))
  where
    expectedShape =
      ( axisMultiplicity (bmRows blockedMat) rowLabel
      , axisMultiplicity (bmCols blockedMat) columnLabel
      )

modifyBlock :: (Eq a, Num a) => FinObjectId -> FinObjectId -> (DenseMat a -> DenseMat a) -> BlockedMat a -> BlockedMat a
modifyBlock r c f bm = setBlock r c (f (blockAt r c bm)) bm

composeBlocked :: (Eq a, Num a) => BlockedMat a -> BlockedMat a -> BlockedMat a
composeBlocked g f =
  BlockedMat
    { bmRows = bmRows g
    , bmCols = bmCols f
    , bmBlocks =
        IM.mapMaybe
          nonEmptyRow
          (IM.map (composeBlockedRow (bmBlocks f)) (bmBlocks g))
    }

composeBlockedIsZero :: (Eq a, Num a) => BlockedMat a -> BlockedMat a -> Bool
composeBlockedIsZero g f =
  all
    rowCompositionIsZero
    (IM.elems (bmBlocks g))
  where
    fBlocks =
      bmBlocks f

    rowCompositionIsZero rowMapG =
      not (rowSupportsComposition fBlocks rowMapG)
        || IM.null (composeBlockedSparseRow fBlocks rowMapG)

composeBlockedRow ::
  (Eq a, Num a) =>
  IntMap (IntMap (DenseMat a)) ->
  IntMap (DenseMat a) ->
  IntMap (DenseMat a)
composeBlockedRow fBlocks rowMapG =
  IM.mapMaybe
    finishSparseProductBlock
    (composeBlockedSparseRow fBlocks rowMapG)

type SparseBlockEntries :: Type -> Type
type SparseBlockEntries a = IntMap (IntMap a)

type SparseProductBlock :: Type -> Type
data SparseProductBlock a = SparseProductBlock
  { spbRows :: !Int
  , spbCols :: !Int
  , spbEntries :: !(SparseBlockEntries a)
  }

composeBlockedSparseRow ::
  (Eq a, Num a) =>
  IntMap (IntMap (DenseMat a)) ->
  IntMap (DenseMat a) ->
  IntMap (SparseProductBlock a)
composeBlockedSparseRow fBlocks rowMapG =
  IM.foldlWithKey'
    addMiddleContribution
    IM.empty
    rowMapG
  where
    addMiddleContribution accumulatedRow midKey gBlock =
      case IM.lookup midKey fBlocks of
        Nothing ->
          accumulatedRow
        Just rowMapF ->
          IM.foldlWithKey'
            (addProductBlockEntries gBlock)
            accumulatedRow
            rowMapF

rowSupportsComposition ::
  IntMap (IntMap (DenseMat a)) ->
  IntMap (DenseMat a) ->
  Bool
rowSupportsComposition fBlocks rowMapG =
  any
    (`IM.member` fBlocks)
    (IM.keys rowMapG)

addProductBlockEntries ::
  (Eq a, Num a) =>
  DenseMat a ->
  IntMap (SparseProductBlock a) ->
  Int ->
  DenseMat a ->
  IntMap (SparseProductBlock a)
addProductBlockEntries gBlock accumulatedRow columnKey fBlock =
  let productEntries =
        multiplyDenseBlockEntries gBlock fBlock
   in if IM.null productEntries
        then accumulatedRow
        else
          IM.alter
            (insertSparseProductBlock (dmRows gBlock) (dmCols fBlock) productEntries)
            columnKey
            accumulatedRow

insertSparseProductBlock ::
  (Eq a, Num a) =>
  Int ->
  Int ->
  SparseBlockEntries a ->
  Maybe (SparseProductBlock a) ->
  Maybe (SparseProductBlock a)
insertSparseProductBlock rowCount columnCount newEntries Nothing =
  Just
    SparseProductBlock
      { spbRows = rowCount
      , spbCols = columnCount
      , spbEntries = newEntries
      }
insertSparseProductBlock rowCount columnCount newEntries (Just SparseProductBlock {spbEntries}) =
  nonEmptySparseProductBlock
    ( SparseProductBlock
        { spbRows = rowCount
        , spbCols = columnCount
        , spbEntries = addSparseBlockEntries spbEntries newEntries
        }
    )

finishSparseProductBlock :: Num a => SparseProductBlock a -> Maybe (DenseMat a)
finishSparseProductBlock SparseProductBlock {spbRows, spbCols, spbEntries}
  | IM.null spbEntries =
      Nothing
  | otherwise =
      Just
        DenseMat
          { dmRows = spbRows
          , dmCols = spbCols
          , dmData =
              V.generate
                spbRows
                ( \rowIndexValue ->
                    V.generate
                      spbCols
                      ( \columnIndexValue ->
                          IM.findWithDefault
                            0
                            columnIndexValue
                            (IM.findWithDefault IM.empty rowIndexValue spbEntries)
                      )
                )
          }

nonEmptySparseProductBlock :: SparseProductBlock a -> Maybe (SparseProductBlock a)
nonEmptySparseProductBlock productBlock
  | IM.null (spbEntries productBlock) =
      Nothing
  | otherwise =
      Just productBlock

multiplyDenseBlockEntries ::
  forall a.
  (Eq a, Num a) =>
  DenseMat a ->
  DenseMat a ->
  SparseBlockEntries a
multiplyDenseBlockEntries gBlock fBlock =
  IM.foldlWithKey'
    addLeftRow
    IM.empty
    (denseBlockEntries gBlock)
  where
    fRows =
      denseBlockEntries fBlock

    addLeftRow accumulatedEntries rowIndexValue leftRow =
      IM.foldlWithKey'
        (addMiddleEntry rowIndexValue)
        accumulatedEntries
        leftRow

    addMiddleEntry rowIndexValue accumulatedEntries middleIndexValue leftCoefficient =
      case IM.lookup middleIndexValue fRows of
        Nothing ->
          accumulatedEntries
        Just rightRow ->
          IM.foldlWithKey'
            (addRightEntry rowIndexValue leftCoefficient)
            accumulatedEntries
            rightRow

    addRightEntry :: Int -> a -> SparseBlockEntries a -> Int -> a -> SparseBlockEntries a
    addRightEntry rowIndexValue leftCoefficient accumulatedEntries columnIndexValue rightCoefficient =
      insertSparseBlockCoefficient
        rowIndexValue
        columnIndexValue
        (leftCoefficient * rightCoefficient)
        accumulatedEntries

denseBlockEntries :: forall a. (Eq a, Num a) => DenseMat a -> SparseBlockEntries a
denseBlockEntries denseMat =
  foldl'
    insertEntry
    IM.empty
    (smEntries (denseToSparseMat denseMat))
  where
    insertEntry :: SparseBlockEntries a -> SparseMatrixEntry a -> SparseBlockEntries a
    insertEntry entries SparseMatrixEntry {smeRow, smeColumn, smeValue} =
      insertSparseBlockCoefficient smeRow smeColumn smeValue entries

addSparseBlockEntries ::
  forall a.
  (Eq a, Num a) =>
  SparseBlockEntries a ->
  SparseBlockEntries a ->
  SparseBlockEntries a
addSparseBlockEntries =
  IM.foldlWithKey'
    addRow
  where
    addRow :: SparseBlockEntries a -> Int -> IntMap a -> SparseBlockEntries a
    addRow accumulatedEntries rowIndexValue rowEntries =
      IM.foldlWithKey'
        ( \nextEntries columnIndexValue coefficientValue ->
            insertSparseBlockCoefficient
              rowIndexValue
              columnIndexValue
              coefficientValue
              nextEntries
        )
        accumulatedEntries
        rowEntries

insertSparseBlockCoefficient ::
  (Eq a, Num a) =>
  Int ->
  Int ->
  a ->
  SparseBlockEntries a ->
  SparseBlockEntries a
insertSparseBlockCoefficient rowIndexValue columnIndexValue coefficientValue entries
  | coefficientValue == 0 =
      entries
  | otherwise =
      IM.alter updateRow rowIndexValue entries
  where
    updateRow Nothing =
      Just (IM.singleton columnIndexValue coefficientValue)
    updateRow (Just rowEntries) =
      nonEmptySparseRow (IM.alter updateColumn columnIndexValue rowEntries)

    updateColumn Nothing =
      Just coefficientValue
    updateColumn (Just oldValue) =
      let nextValue =
            oldValue + coefficientValue
       in if nextValue == 0
            then Nothing
            else Just nextValue

nonEmptySparseRow :: IntMap a -> Maybe (IntMap a)
nonEmptySparseRow rowMap
  | IM.null rowMap =
      Nothing
  | otherwise =
      Just rowMap

nonEmptyRow :: IntMap (DenseMat a) -> Maybe (IntMap (DenseMat a))
nonEmptyRow rowMap
  | IM.null rowMap =
      Nothing
  | otherwise =
      Just rowMap

restrictBlocked :: IntSet -> BlockedMat a -> BlockedMat a
restrictBlocked keep BlockedMat{bmRows,bmCols,bmBlocks} =
  BlockedMat (restrictAxis keep bmRows) (restrictAxis keep bmCols)
    (IM.mapMaybeWithKey (\rk rowMap ->
      if not (IS.member rk keep) then Nothing
      else let rowMap' = IM.filterWithKey (\ck _ -> IS.member ck keep) rowMap
           in if IM.null rowMap' then Nothing else Just rowMap') bmBlocks)

embedInto :: Num a => Int -> Int -> DenseMat a -> Int -> Int -> DenseMat a
embedInto totalRows totalCols small rowOff colOff =
  DenseMat totalRows totalCols $ V.generate totalRows $ \i ->
    V.generate totalCols $ \j ->
      let ii = i - rowOff; jj = j - colOff
      in if ii >= 0 && ii < dmRows small && jj >= 0 && jj < dmCols small
           then matIndex small ii jj else 0

relabelBlocked :: (Eq a, Num a) => (FinObjectId -> Either MoonlightError FinObjectId) -> BlockedMat a -> Either MoonlightError (BlockedMat a)
relabelBlocked f BlockedMat{bmRows,bmCols,bmBlocks} = do
  (rows', rowOffs) <- relabelOffsetsChecked f bmRows
  (cols', colOffs) <- relabelOffsetsChecked f bmCols
  let place accE oldR rowMap =
        accE >>= \acc ->
          case IM.lookup oldR rowOffs of
            Nothing -> Left (InvariantViolation "relabelBlocked: missing row offset")
            Just (newR, rowOff) -> IM.foldlWithKey' (placeBlock newR rowOff) (Right acc) rowMap
      placeBlock newR rowOff accE oldC blk =
        accE >>= \acc ->
          case IM.lookup oldC colOffs of
            Nothing -> Left (InvariantViolation "relabelBlocked: missing col offset")
            Just (newC, colOff) ->
              let big = embedInto (axisMultiplicity rows' newR) (axisMultiplicity cols' newC) blk rowOff colOff
                  rKey = unFinObjectId newR; cKey = unFinObjectId newC
                  existing = IM.findWithDefault IM.empty rKey acc
              in Right (IM.insert rKey (IM.insertWith matAdd cKey big existing) acc)
      cleaned :: (Eq a, Num a) => IntMap (IntMap (DenseMat a)) -> IntMap (IntMap (DenseMat a))
      cleaned blocks' = IM.mapMaybe (\rm -> let rm' = IM.filter (not . isZeroMat) rm
                in if IM.null rm' then Nothing else Just rm') blocks'
  fmap (\blocks' -> BlockedMat rows' cols' (cleaned blocks'))
    (IM.foldlWithKey' place (Right IM.empty) bmBlocks)

relabelOffsetsChecked :: (FinObjectId -> Either MoonlightError FinObjectId) -> GroupedAxis -> Either MoonlightError (GroupedAxis, IntMap (FinObjectId, Int))
relabelOffsetsChecked mapNode ga@GroupedAxis {gaOrder} = do
  (axisValue, _, offsets) <-
    foldM step (emptyAxis, IM.empty, IM.empty) (V.toList gaOrder)
  Right (axisValue, offsets)
  where
    step (axisValue, localOffsets, offsets) oldNode = do
      newNode@(FinObjectId newKey) <- mapNode oldNode
      let oldMultiplicity = axisMultiplicity ga oldNode
          startOffset = IM.findWithDefault 0 newKey localOffsets
      Right
        ( appendAxisLabel newNode oldMultiplicity axisValue
        , IM.insert newKey (startOffset + oldMultiplicity) localOffsets
        , IM.insert (unFinObjectId oldNode) (newNode, startOffset) offsets
        )

fromExpanded :: (Eq a, Num a) => Vector FinObjectId -> Vector FinObjectId -> DenseMat a -> BlockedMat a
fromExpanded rowLabelsVec colLabelsVec mat =
  let rows = fromLabels rowLabelsVec
      cols = fromLabels colLabelsVec
      rIndex = V.ifoldr (\i (FinObjectId objectKey) -> IM.insertWith (++) objectKey [i]) IM.empty rowLabelsVec
      cIndex = V.ifoldr (\j (FinObjectId objectKey) -> IM.insertWith (++) objectKey [j]) IM.empty colLabelsVec
      mkRow r@(FinObjectId rowKey) =
        let rowEntries = mapMaybe (mkBlock r) (V.toList (gaOrder cols))
        in if null rowEntries then Nothing else Just (rowKey, IM.fromList rowEntries)
      mkBlock r c =
        let rs = IM.findWithDefault [] (unFinObjectId r) rIndex
            cs = IM.findWithDefault [] (unFinObjectId c) cIndex
            blk = submatrix rs cs mat
        in if isZeroMat blk then Nothing else Just (unFinObjectId c, blk)
  in BlockedMat rows cols (IM.fromList (mapMaybe mkRow (V.toList (gaOrder rows))))

fromExpandedChecked :: (Eq a, Num a) => Vector FinObjectId -> Vector FinObjectId -> DenseMat a -> Either DerivedFailure (BlockedMat a)
fromExpandedChecked rowLabelsVec colLabelsVec denseMat =
  validateDenseMat "fromExpanded" denseMat
    *> if V.length rowLabelsVec /= dmRows denseMat
      then Left (DerivedMatrixShapeMismatch "fromExpanded rows" (V.length rowLabelsVec, dmCols denseMat) (matShape denseMat))
      else if V.length colLabelsVec /= dmCols denseMat
        then Left (DerivedMatrixShapeMismatch "fromExpanded cols" (dmRows denseMat, V.length colLabelsVec) (matShape denseMat))
        else Right (fromExpanded rowLabelsVec colLabelsVec denseMat)

expandBlocked :: Num a => BlockedMat a -> (Vector FinObjectId, Vector FinObjectId, DenseMat a)
expandBlocked bm@BlockedMat{bmRows,bmCols} =
  let rowsExp = axisLabelsExpanded bmRows; colsExp = axisLabelsExpanded bmCols
      rs = rowLabels bmRows; cs = colLabels bmCols
  in ( rowsExp, colsExp
     , if null rs || null cs then zeroMat (axisSize bmRows) (axisSize bmCols)
       else blockCat [ [ blockAt r c bm | c <- cs ] | r <- rs ] )

collapseBlockedDense :: Num a => BlockedMat a -> DenseMat a
collapseBlockedDense blockedMat =
  let (_, _, denseMat) = expandBlocked blockedMat
  in denseMat

starView :: Num a => DerivedPoset -> FinObjectId -> BlockedMat a -> DenseMat a
starView p x bm =
  let st = star p x
      rs = [ r | r <- rowLabels (bmRows bm), IS.member (unFinObjectId r) st ]
      cs = [ c | c <- colLabels (bmCols bm), IS.member (unFinObjectId c) st ]
  in if null rs || null cs
       then zeroMat (sum (map (axisMultiplicity (bmRows bm)) rs))
                    (sum (map (axisMultiplicity (bmCols bm)) cs))
       else blockCat [ [ blockAt r c bm | c <- cs ] | r <- rs ]

-- | Precondition: widths are nonnegative and their sum does not exceed the vector length; unchecked.
splitByWidths :: [Int] -> Vector a -> [Vector a]
splitByWidths ws vec = go 0 ws
  where
    go _ [] = []
    go off (w:rest) = V.slice off w vec : go (off + w) rest

appendRowOnLabel :: (Eq a, Num a) => FinObjectId -> [(FinObjectId, Vector a)] -> BlockedMat a -> BlockedMat a
appendRowOnLabel lab payload =
  appendRowsOnLabel lab [payload]

appendRowsOnLabel :: (Eq a, Num a) => FinObjectId -> [[(FinObjectId, Vector a)]] -> BlockedMat a -> BlockedMat a
appendRowsOnLabel _ [] bm =
  bm
appendRowsOnLabel lab payloadRows bm@BlockedMat{bmRows,bmCols} =
  foldl' updateOne bm0 (colLabels bmCols)
  where
    oldMult =
      axisMultiplicity bmRows lab

    rowCount =
      length payloadRows

    rows' =
      appendAxisLabel lab rowCount bmRows

    payloadMaps =
      fmap
        (IM.fromList . fmap (\(columnLabel, segmentValue) -> (unFinObjectId columnLabel, segmentValue)))
        payloadRows

    bm0 =
      bm { bmRows = rows' }

    updateOne acc columnLabel =
      let width =
            axisMultiplicity bmCols columnLabel
          segments =
            V.fromList
              ( fmap
                  ( IM.findWithDefault
                      (V.replicate width 0)
                      (unFinObjectId columnLabel)
                  )
                  payloadMaps
              )
       in if oldMult == 0
            then
              if V.all (V.all (== 0)) segments
                then acc
                else setBlock lab columnLabel (DenseMat rowCount width segments) acc
            else
              let blockValue =
                    blockAt lab columnLabel bm
               in setBlock lab columnLabel (appendRowsMat segments blockValue) acc

removeRowsOnLabel :: (Eq a, Num a) => FinObjectId -> [Int] -> BlockedMat a -> BlockedMat a
removeRowsOnLabel lab idxs bm@BlockedMat{bmRows,bmBlocks}
  | null idxs = bm
  | otherwise =
      let rows' = removeAxisIndices lab idxs bmRows
          rKey = unFinObjectId lab
          blocks' = case IM.lookup rKey bmBlocks of
            Nothing -> bmBlocks
            Just rowMap ->
              let rowMap' = IM.filter (not . isZeroMat) (IM.map (deleteRowsMat idxs) rowMap)
              in if IM.null rowMap' then IM.delete rKey bmBlocks
                 else IM.insert rKey rowMap' bmBlocks
      in bm { bmRows = rows', bmBlocks = blocks' }

removeColsOnLabel :: (Eq a, Num a) => FinObjectId -> [Int] -> BlockedMat a -> BlockedMat a
removeColsOnLabel lab idxs bm@BlockedMat{bmCols,bmBlocks}
  | null idxs = bm
  | otherwise =
      let cols' = removeAxisIndices lab idxs bmCols
          cKey = unFinObjectId lab
          tweak rowMap =
            let rowMap' = case IM.lookup cKey rowMap of
                  Nothing -> rowMap
                  Just blk -> IM.insert cKey (deleteColsMat idxs blk) rowMap
            in IM.filter (not . isZeroMat) rowMap'
          blocks' = IM.mapMaybe (\rm -> let rm' = tweak rm
                    in if IM.null rm' then Nothing else Just rm') bmBlocks
      in bm { bmCols = cols', bmBlocks = blocks' }

leftMultiplyRowLabel :: Num a => FinObjectId -> DenseMat a -> BlockedMat a -> BlockedMat a
leftMultiplyRowLabel (FinObjectId rowKey) u bm@BlockedMat{bmBlocks} =
  case IM.lookup rowKey bmBlocks of
    Nothing -> bm
    Just rowMap -> bm { bmBlocks = IM.insert rowKey (IM.map (matMul u) rowMap) bmBlocks }

rightMultiplyColLabel :: Num a => FinObjectId -> DenseMat a -> BlockedMat a -> BlockedMat a
rightMultiplyColLabel (FinObjectId columnKey) v bm@BlockedMat{bmBlocks} =
  bm { bmBlocks = IM.map (\rowMap -> case IM.lookup columnKey rowMap of
    Nothing -> rowMap
    Just blk -> IM.insert columnKey (matMul blk v) rowMap) bmBlocks }

rowOp :: (Eq a, Num a) => FinObjectId -> Int -> a -> FinObjectId -> Int -> BlockedMat a -> Either MoonlightError (BlockedMat a)
rowOp targetLab targetIx alpha sourceLab sourceIx bm =
  validateRowOpIndices targetLab targetIx sourceLab sourceIx bm
    *> Right (foldl' updateOne bm (dedupStableOn unFinObjectId (colLabels (bmCols bm))))
  where
    updateOne acc c =
      let tgtBlk = blockAt targetLab c acc
          srcBlk = blockAt sourceLab c acc
          srcRow = rowAt srcBlk sourceIx
          newRow = V.zipWith (+) (rowAt tgtBlk targetIx) (V.map (alpha *) srcRow)
      in setBlock targetLab c (setRow targetIx newRow tgtBlk) acc

colOp :: (Eq a, Num a) => FinObjectId -> Int -> a -> FinObjectId -> Int -> BlockedMat a -> Either MoonlightError (BlockedMat a)
colOp targetLab targetIx alpha sourceLab sourceIx bm =
  validateColOpIndices targetLab targetIx sourceLab sourceIx bm
    *> Right (foldl' updateOne bm (dedupStableOn unFinObjectId (rowLabels (bmRows bm))))
  where
    updateOne acc r =
      let tgtBlk = blockAt r targetLab acc
          srcBlk = blockAt r sourceLab acc
          srcCol = colAt srcBlk sourceIx
          tgtBlk' = tgtBlk { dmData = V.imap (\i row ->
            row V.// [(targetIx, (row V.! targetIx) + alpha * (srcCol V.! i))]) (dmData tgtBlk) }
      in setBlock r targetLab tgtBlk' acc

validateRowOpIndices :: FinObjectId -> Int -> FinObjectId -> Int -> BlockedMat a -> Either MoonlightError ()
validateRowOpIndices targetLab targetIx sourceLab sourceIx bm = do
  validateAxisIndex "rowOp target" (axisMultiplicity (bmRows bm) targetLab) targetIx
  validateAxisIndex "rowOp source" (axisMultiplicity (bmRows bm) sourceLab) sourceIx

validateColOpIndices :: FinObjectId -> Int -> FinObjectId -> Int -> BlockedMat a -> Either MoonlightError ()
validateColOpIndices targetLab targetIx sourceLab sourceIx bm = do
  validateAxisIndex "colOp target" (axisMultiplicity (bmCols bm) targetLab) targetIx
  validateAxisIndex "colOp source" (axisMultiplicity (bmCols bm) sourceLab) sourceIx

validateAxisIndex :: String -> Int -> Int -> Either MoonlightError ()
validateAxisIndex context axisCardinality axisIndex
  | axisIndex < 0 =
      Left (InvariantViolation (context <> ": negative index " <> show axisIndex))
  | axisIndex >= axisCardinality =
      Left
        ( InvariantViolation
            ( context
                <> ": index "
                <> show axisIndex
                <> " out of bounds for axis cardinality "
                <> show axisCardinality
            )
        )
  | otherwise = Right ()
