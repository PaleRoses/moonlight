{-# LANGUAGE PatternSynonyms #-}

module Moonlight.Derived.Pure.LinAlg.Rank
  ( RankBackend (RankBackend, computeDenseRank)
  , rankBackendWithSparse
  , DenseMatStableDigest (..)
  , SparseMatStableDigest (..)
  , denseMatStableDigest
  , sparseMatStableDigest
  , precomputeStableRankCache
  , precomputeStableSparseRankCache
  , stableDigestRankBackend
  , stableSparseDigestRankBackend
  , rankDenseWith
  , rankSparseWith
  , rankSparseDefault
  , rankSparseGF2Packed
  ) where

import Control.Monad (foldM)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import qualified Data.Vector as V
import Moonlight.Core
  ( Field (fieldValueValid)
  , MoonlightError (..)
  , StableHashDigest
  , requireInvertible
  , stableHashByteStrings
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix
  ( DenseMat (..)
  , SparseMat (..)
  , SparseMatrixEntry (..)
  , canonicalSparseMatEntries
  , sparseMatToDense
  )
import Moonlight.LinAlg.Dense.GF2
  ( GF2 (..)
  , GF2MatrixEntry (..)
  , mkGF2PackedMatrix
  , rankGF2PackedMatrix
  )

type RankBackend :: Type -> Type
data RankBackend a = RankBackendCore
  { computeDenseRank :: DenseMat a -> Either MoonlightError Int
  , computeSparseRank :: SparseMat a -> Either MoonlightError Int
  }

pattern RankBackend :: Num a => (DenseMat a -> Either MoonlightError Int) -> RankBackend a
pattern RankBackend denseRank <- RankBackendCore denseRank _
  where
    RankBackend denseRank =
      RankBackendCore denseRank (denseRank . sparseMatToDense)

{-# COMPLETE RankBackend #-}

rankBackendWithSparse ::
  (DenseMat a -> Either MoonlightError Int) ->
  (SparseMat a -> Either MoonlightError Int) ->
  RankBackend a
rankBackendWithSparse =
  RankBackendCore

type DenseMatStableDigest :: Type
newtype DenseMatStableDigest = DenseMatStableDigest
  { unDenseMatStableDigest :: StableHashDigest
  }
  deriving stock (Eq, Ord, Show)

type SparseMatStableDigest :: Type
newtype SparseMatStableDigest = SparseMatStableDigest
  { unSparseMatStableDigest :: StableHashDigest
  }
  deriving stock (Eq, Ord, Show)

denseMatStableDigest :: Show a => DenseMat a -> DenseMatStableDigest
denseMatStableDigest denseMat =
  DenseMatStableDigest
    ( stableHashByteStrings
        ( fmap
            ByteString.Char8.pack
            ( show (dmRows denseMat)
                : show (dmCols denseMat)
                : fmap show (denseToFlat denseMat)
            )
        )
    )

sparseMatStableDigest :: (Eq a, Num a) => SparseMat a -> SparseMatStableDigest
sparseMatStableDigest sparseMat =
  SparseMatStableDigest
    ( stableHashByteStrings
        ( fmap
            ByteString.Char8.pack
            ( show (smRows sparseMat)
                : show (smCols sparseMat)
                : fmap supportEntry (canonicalSparseMatEntries sparseMat)
            )
        )
    )
  where
    supportEntry SparseMatrixEntry {smeRow, smeColumn} =
      show (smeRow, smeColumn)

precomputeStableRankCache ::
  (Eq a, Show a) =>
  RankBackend a ->
  [DenseMat a] ->
  Either MoonlightError (Map DenseMatStableDigest [(DenseMat a, Int)])
precomputeStableRankCache rankBackend =
  foldM insertDenseRank Map.empty
  where
    insertDenseRank rankCache denseMat =
      let digestValue = denseMatStableDigest denseMat
          digestBucket = Map.findWithDefault [] digestValue rankCache
       in case find ((== denseMat) . fst) digestBucket of
            Just _ -> Right rankCache
            Nothing ->
              fmap
                (\rankValue -> Map.insert digestValue ((denseMat, rankValue) : digestBucket) rankCache)
                (rankDenseWith rankBackend denseMat)

precomputeStableSparseRankCache ::
  (Eq a, Num a) =>
  RankBackend a ->
  [SparseMat a] ->
  Either MoonlightError (Map SparseMatStableDigest [(SparseMat a, Int)])
precomputeStableSparseRankCache rankBackend =
  foldM insertSparseRank Map.empty
  where
    insertSparseRank rankCache sparseMat =
      let digestValue = sparseMatStableDigest sparseMat
          digestBucket = Map.findWithDefault [] digestValue rankCache
       in case find (sparseMatSemanticallyEqual sparseMat . fst) digestBucket of
            Just _ -> Right rankCache
            Nothing ->
              fmap
                (\rankValue -> Map.insert digestValue ((sparseMat, rankValue) : digestBucket) rankCache)
                (rankSparseWith rankBackend sparseMat)

stableDigestRankBackend ::
  (Eq a, Show a) =>
  Map DenseMatStableDigest [(DenseMat a, Int)] ->
  RankBackend a ->
  RankBackend a
stableDigestRankBackend rankCache fallbackBackend =
  RankBackendCore
    { computeDenseRank =
        \denseMat ->
          maybe
            (rankDenseWith fallbackBackend denseMat)
            Right
            (Map.lookup (denseMatStableDigest denseMat) rankCache >>= matchingRank denseMat)
    , computeSparseRank = computeSparseRank fallbackBackend
    }
  where
    matchingRank :: Eq a => DenseMat a -> [(DenseMat a, Int)] -> Maybe Int
    matchingRank denseMat =
      fmap snd . find (\(cachedMat, _) -> cachedMat == denseMat)

stableSparseDigestRankBackend ::
  (Eq a, Num a) =>
  Map SparseMatStableDigest [(SparseMat a, Int)] ->
  RankBackend a ->
  RankBackend a
stableSparseDigestRankBackend rankCache fallbackBackend =
  RankBackendCore
    { computeDenseRank = computeDenseRank fallbackBackend
    , computeSparseRank =
        \sparseMat ->
          maybe
            (rankSparseWith fallbackBackend sparseMat)
            Right
            (Map.lookup (sparseMatStableDigest sparseMat) rankCache >>= matchingSparseRank sparseMat)
    }
  where
    matchingSparseRank ::
      (Eq a, Num a) =>
      SparseMat a ->
      [(SparseMat a, Int)] ->
      Maybe Int
    matchingSparseRank sparseMat =
      fmap snd
        . find
          ( \(cachedMat, _) ->
              sparseMatSemanticallyEqual cachedMat sparseMat
          )

rankDenseWith :: RankBackend a -> DenseMat a -> Either MoonlightError Int
rankDenseWith rankBackend =
  computeDenseRank rankBackend

rankSparseWith :: RankBackend a -> SparseMat a -> Either MoonlightError Int
rankSparseWith rankBackend =
  computeSparseRank rankBackend

rankSparseDefault ::
  (Eq a, Field a, Num a) =>
  SparseMat a ->
  Either MoonlightError Int
rankSparseDefault sparseMat = do
  validateSparseMat "rankSparse" sparseMat
  finalSpan <-
    foldM
      (admitSparseRankRow "rankSparse")
      IntMap.empty
      (sparseRankRows sparseMat)
  Right (IntMap.size finalSpan)

rankSparseGF2Packed ::
  SparseMat GF2 ->
  Either MoonlightError Int
rankSparseGF2Packed sparseMat = do
  validateSparseShape "rankSparseGF2Packed" sparseMat
  packedMatrix <-
    either
      (Left . InvariantViolation . ("rankSparseGF2Packed: " <>) . show)
      Right
      ( mkGF2PackedMatrix
          (fromIntegral (smRows sparseMat))
          (fromIntegral (smCols sparseMat))
          (gf2SparseEntries sparseMat)
      )
  Right (rankGF2PackedMatrix packedMatrix)

denseToFlat :: DenseMat a -> [a]
denseToFlat denseMat =
  concatMap V.toList (V.toList (dmData denseMat))

type SparseRankRow :: Type -> Type
type SparseRankRow a = IntMap a

type SparseRankSpan :: Type -> Type
type SparseRankSpan a = IntMap (SparseRankRow a)

sparseMatSemanticallyEqual :: (Eq a, Num a) => SparseMat a -> SparseMat a -> Bool
sparseMatSemanticallyEqual leftMat rightMat =
  smRows leftMat == smRows rightMat
    && smCols leftMat == smCols rightMat
    && canonicalSparseMatEntries leftMat == canonicalSparseMatEntries rightMat

validateSparseMat ::
  Field a =>
  String ->
  SparseMat a ->
  Either MoonlightError ()
validateSparseMat context sparseMat =
  validateSparseShape context sparseMat
    *> traverse
      (validateSparseEntryValue context)
      (smEntries sparseMat)
    *> Right ()

validateSparseShape ::
  String ->
  SparseMat a ->
  Either MoonlightError ()
validateSparseShape context SparseMat {smRows, smCols, smEntries}
  | smRows < 0 || smCols < 0 =
      Left
        ( InvariantViolation
            ( context
                <> ": negative sparse shape "
                <> show (smRows, smCols)
            )
        )
  | otherwise =
      traverse
        (validateSparseEntryBounds context smRows smCols)
        smEntries
        *> Right ()

validateSparseEntryBounds ::
  String ->
  Int ->
  Int ->
  SparseMatrixEntry a ->
  Either MoonlightError ()
validateSparseEntryBounds context rowCount columnCount SparseMatrixEntry {smeRow, smeColumn}
  | smeRow < 0 =
      Left
        ( InvariantViolation
            ( context
                <> ": negative sparse row "
                <> show smeRow
            )
        )
  | smeRow >= rowCount =
      Left
        ( InvariantViolation
            ( context
                <> ": sparse row "
                <> show smeRow
                <> " is outside row count "
                <> show rowCount
            )
        )
  | smeColumn < 0 =
      Left
        ( InvariantViolation
            ( context
                <> ": negative sparse column "
                <> show smeColumn
            )
        )
  | smeColumn >= columnCount =
      Left
        ( InvariantViolation
            ( context
                <> ": sparse column "
                <> show smeColumn
                <> " is outside column count "
                <> show columnCount
            )
        )
  | otherwise =
      Right ()

validateSparseEntryValue ::
  Field a =>
  String ->
  SparseMatrixEntry a ->
  Either MoonlightError ()
validateSparseEntryValue context SparseMatrixEntry {smeRow, smeColumn, smeValue}
  | fieldValueValid smeValue =
      Right ()
  | otherwise =
      Left
        ( InvariantViolation
            ( context
                <> ": sparse matrix contains an invalid field value at coordinate "
                <> show (smeRow, smeColumn)
            )
        )

sparseRankRows :: (Eq a, Num a) => SparseMat a -> [SparseRankRow a]
sparseRankRows =
  fmap rowMap
    . IntMap.toAscList
    . foldr insertEntry IntMap.empty
    . canonicalSparseMatEntries
  where
    insertEntry SparseMatrixEntry {smeRow, smeColumn, smeValue} =
      IntMap.alter
        (Just . insertRankCoefficient smeColumn smeValue . maybe IntMap.empty id)
        smeRow

    rowMap =
      snd

admitSparseRankRow ::
  (Eq a, Field a, Num a) =>
  String ->
  SparseRankSpan a ->
  SparseRankRow a ->
  Either MoonlightError (SparseRankSpan a)
admitSparseRankRow context spanValue candidateRow =
  case IntMap.lookupMin reducedRow of
    Nothing ->
      Right spanValue
    Just (pivotIndex, pivotValue) -> do
      pivotInverse <-
        requireInvertible
          ( InvariantViolation
              ( context
                  <> ": Field.tryInv failed on a nonzero sparse pivot"
              )
          )
          pivotValue
      Right
        ( IntMap.insert
            pivotIndex
            (scaleSparseRankRow pivotInverse reducedRow)
            spanValue
        )
  where
    reducedRow =
      reduceSparseRankRow spanValue candidateRow

reduceSparseRankRow ::
  (Eq a, Num a) =>
  SparseRankSpan a ->
  SparseRankRow a ->
  SparseRankRow a
reduceSparseRankRow spanValue initialRow =
  foldl'
    ( \rowValue (pivotIndex, basisRow) ->
        case IntMap.lookup pivotIndex rowValue of
          Nothing ->
            rowValue
          Just coefficientValue ->
            addScaledSparseRankRow
              (negate coefficientValue)
              basisRow
              rowValue
    )
    initialRow
    (IntMap.toAscList spanValue)

scaleSparseRankRow ::
  (Eq a, Num a) =>
  a ->
  SparseRankRow a ->
  SparseRankRow a
scaleSparseRankRow coefficientValue
  | coefficientValue == 0 =
      const IntMap.empty
  | otherwise =
      IntMap.mapMaybe
        ( \entryValue ->
            let scaledValue =
                  coefficientValue * entryValue
             in if scaledValue == 0
                  then Nothing
                  else Just scaledValue
        )

addScaledSparseRankRow ::
  (Eq a, Num a) =>
  a ->
  SparseRankRow a ->
  SparseRankRow a ->
  SparseRankRow a
addScaledSparseRankRow coefficientValue sourceRow targetRow
  | coefficientValue == 0 =
      targetRow
  | otherwise =
      IntMap.foldlWithKey'
        ( \accumulatedRow columnIndex sourceValue ->
            insertRankCoefficient
              columnIndex
              (coefficientValue * sourceValue)
              accumulatedRow
        )
        targetRow
        sourceRow

insertRankCoefficient ::
  (Eq a, Num a) =>
  Int ->
  a ->
  SparseRankRow a ->
  SparseRankRow a
insertRankCoefficient columnIndex coefficientValue rowValue
  | coefficientValue == 0 =
      rowValue
  | otherwise =
      IntMap.alter updateEntry columnIndex rowValue
  where
    updateEntry Nothing =
      Just coefficientValue
    updateEntry (Just oldValue) =
      let nextValue =
            oldValue + coefficientValue
       in if nextValue == 0
            then Nothing
            else Just nextValue

gf2SparseEntries :: SparseMat GF2 -> [GF2MatrixEntry]
gf2SparseEntries =
  foldr collectEntry []
    . canonicalSparseMatEntries
  where
    collectEntry SparseMatrixEntry {smeRow, smeColumn, smeValue} entries =
      case smeValue of
        GF2Zero ->
          entries
        GF2One ->
          GF2MatrixEntry smeRow smeColumn : entries
