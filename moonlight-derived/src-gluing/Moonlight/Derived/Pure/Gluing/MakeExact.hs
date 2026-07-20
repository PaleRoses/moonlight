{-# LANGUAGE NamedFieldPuns #-}

module Moonlight.Derived.Pure.Gluing.MakeExact
  ( PreparedExactness
  , prepareExactness
  , makeExact
  , makeExactAtStar
  , makeExactPreparedAtStar
  , makeExactPreparedFreshAtStar
  , makeExactPrepared
  ) where

import Control.Monad (foldM)
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (sortOn)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Vector qualified as V
import Moonlight.Algebra (IntegralDomain)
import Moonlight.Core (Field, MoonlightError (..))
import Moonlight.Core (scanMap)
import Moonlight.Derived.Pure.LinAlg.SparseEchelon
  ( SparseRow
  , SparseSpan
  , TrackedLeftKernel
  , admitSparseRow
  , appendSparseRowEntries
  , emptySparseRow
  , emptySparseSpan
  , emptyTrackedLeftKernel
  , prependSparseSpanRows
  , prependTrackedLeftKernelRows
  , restrictSparseRow
  , sparseRowEntries
  , trackedLeftKernelRows
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix
  ( BlockedMat
  , DenseMat (..)
  , GroupedAxis
  , appendAxisLabel
  , appendRowsOnLabel
  , axisMultiplicity
  , axisSize
  , bmCols
  , bmRows
  , gaOrder
  , storedBlockAt
  )
import Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset
  , FinObjectId (..)
  , star
  )

data PreparedExactness a = PreparedExactness
  { pePreviousSource :: !(StarRowSource a)
  , peKernelState :: !(CachedKernelState a)
  , peCurrentSource :: !(StarRowSource a)
  , peSpanState :: !(CachedSpanState a)
  , peCurrentDifferential :: !(BlockedMat a)
  }

data StarRowSource a = StarRowSource
  { srsRowsAxis :: !GroupedAxis
  , srsColumnsAxis :: !GroupedAxis
  , srsBlockedMat :: !(BlockedMat a)
  , srsRowOffsets :: !(IntMap (Int, Int))
  , srsCurrentRowOffsets :: !(IntMap (Int, Int))
  , srsColumnOffsets :: !(IntMap (Int, Int))
  , srsOriginalRowsByLabel :: !(IntMap (V.Vector (SparseSourceRow a)))
  , srsAddedRowsByLabel :: !(IntMap (V.Vector (SparseSourceRow a)))
  , srsNextRowKey :: !Int
  }

data SparseSourceRow a = SparseSourceRow
  { ssrKey :: !Int
  , ssrRow :: !(SparseRow a)
  }

data CachedKernelState a = CachedKernelState
  { cksRows :: ![(Int, SparseRow a)]
  , cksKernel :: !(TrackedLeftKernel a)
  , cksKernelRows :: !(Seq (SparseRow a))
  }

data CachedSpanState a = CachedSpanState
  { cssRows :: ![(Int, SparseRow a)]
  , cssSpan :: !(SparseSpan a)
  }

data StarColumnProfile = StarColumnProfile
  { scpSegments :: ![StarColumnSegment]
  , scpAllowedColumns :: !IntSet
  , scpUsesSparseAxis :: !Bool
  }

data StarColumnSegment = StarColumnSegment
  { scsNode :: !FinObjectId
  , scsOffset :: !Int
  , scsWidth :: !Int
  }

data AcceptedRows a = AcceptedRows
  { arSpan :: !(SparseSpan a)
  , arRowsReversed :: ![SparseRow a]
  , arPiecesReversed :: ![[(FinObjectId, V.Vector a)]]
  }

makeExact ::
  (Field a, IntegralDomain a, Num a) =>
  DerivedPoset ->
  FinObjectId ->
  BlockedMat a ->
  BlockedMat a ->
  Either MoonlightError (BlockedMat a)
makeExact
  posetValue
  nodeValue
  previousDifferential
  currentDifferential =
    makeExactAtStar
      (star posetValue nodeValue)
      nodeValue
      previousDifferential
      currentDifferential

makeExactAtStar ::
  (Field a, IntegralDomain a, Num a) =>
  IntSet ->
  FinObjectId ->
  BlockedMat a ->
  BlockedMat a ->
  Either MoonlightError (BlockedMat a)
makeExactAtStar
  starSet
  nodeValue
  previousDifferential
  currentDifferential =
    fst
      <$> ( prepareExactness previousDifferential currentDifferential
              >>= makeExactPreparedFreshAtStar
                starSet
                nodeValue
          )

prepareExactness ::
  BlockedMat a ->
  BlockedMat a ->
  Either MoonlightError (PreparedExactness a)
prepareExactness previousDifferential currentDifferential = do
  previousSource <-
    starRowSourceFromBlocked
      "makeExact: previous differential"
      previousDifferential
  currentSource <-
    starRowSourceFromBlocked
      "makeExact: current differential"
      currentDifferential
  Right
    PreparedExactness
      { pePreviousSource = previousSource
      , peKernelState = emptyCachedKernelState
      , peCurrentSource = currentSource
      , peSpanState = emptyCachedSpanState
      , peCurrentDifferential = currentDifferential
      }

makeExactPrepared ::
  (Field a, IntegralDomain a, Num a) =>
  DerivedPoset ->
  FinObjectId ->
  PreparedExactness a ->
  Either MoonlightError (BlockedMat a, PreparedExactness a)
makeExactPrepared
  posetValue
  nodeValue
  preparedExactness =
    makeExactPreparedAtStar
      (star posetValue nodeValue)
      nodeValue
      preparedExactness

makeExactPreparedFreshAtStar ::
  (Field a, IntegralDomain a, Num a) =>
  IntSet ->
  FinObjectId ->
  PreparedExactness a ->
  Either MoonlightError (BlockedMat a, PreparedExactness a)
makeExactPreparedFreshAtStar
  starSet
  nodeValue
  preparedExactness =
    makeExactPreparedAtStar
      starSet
      nodeValue
      (resetPreparedExactnessCaches preparedExactness)

makeExactPreparedAtStar ::
  (Field a, IntegralDomain a, Num a) =>
  IntSet ->
  FinObjectId ->
  PreparedExactness a ->
  Either MoonlightError (BlockedMat a, PreparedExactness a)
makeExactPreparedAtStar
  starSet
  nodeValue
  preparedExactness@PreparedExactness
    { pePreviousSource
    , peKernelState
    , peCurrentSource
    , peSpanState
    , peCurrentDifferential
    } = do
    (previousRows, nextPreviousSource) <-
      sourceStarRows
        "makeExact: previous local differential"
        starSet
        pePreviousSource
    (kernelBasis, nextKernelState) <-
      cachedKernelRows
        "makeExact: previous local left kernel"
        previousRows
        peKernelState
    (currentRows, preparedCurrentSource) <-
      sourceStarRows
        "makeExact: current local differential"
        starSet
        peCurrentSource
    currentSpanState <-
      cachedSpan
        "makeExact: current local row span"
        currentRows
        peSpanState
    columnProfile <-
      starColumnProfileFromSource
        "makeExact: current local columns"
        starSet
        preparedCurrentSource
    (nextDifferential, nextSpanState, nextCurrentSource) <-
      appendIndependentRows
        nodeValue
        columnProfile
        peCurrentDifferential
        currentSpanState
        preparedCurrentSource
        kernelBasis
    Right
      ( nextDifferential
      , preparedExactness
          { pePreviousSource = nextPreviousSource
          , peKernelState = nextKernelState
          , peCurrentSource = nextCurrentSource
          , peSpanState = nextSpanState
          , peCurrentDifferential = nextDifferential
          }
      )

resetPreparedExactnessCaches :: PreparedExactness a -> PreparedExactness a
resetPreparedExactnessCaches preparedExactness =
  preparedExactness
    { peKernelState = emptyCachedKernelState
    , peSpanState = emptyCachedSpanState
    }

emptyCachedKernelState :: CachedKernelState a
emptyCachedKernelState =
  CachedKernelState
    { cksRows = []
    , cksKernel = emptyTrackedLeftKernel
    , cksKernelRows = Seq.empty
    }

emptyCachedSpanState :: CachedSpanState a
emptyCachedSpanState =
  CachedSpanState
    { cssRows = []
    , cssSpan = emptySparseSpan
    }

cachedKernelRows ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  [(Int, SparseRow a)] ->
  CachedKernelState a ->
  Either MoonlightError (Seq (SparseRow a), CachedKernelState a)
cachedKernelRows context rowsValue stateValue@CachedKernelState {cksRows, cksKernel, cksKernelRows} =
  case stripRowsPrefix cksRows rowsValue of
    Just suffixRows -> do
      (nextKernel, newKernelRows) <-
        trackedLeftKernelRows
          context
          cksKernel
          suffixRows
      let nextKernelRows =
            cksKernelRows Seq.>< Seq.fromList newKernelRows
      Right
        ( nextKernelRows
        , stateValue
            { cksRows = rowsValue
            , cksKernel = nextKernel
            , cksKernelRows = nextKernelRows
            }
        )
    Nothing ->
      case stripRowsSuffix cksRows rowsValue of
        Just prefixRows -> do
          prependedRows <-
            prependTrackedLeftKernelRows
              context
              prefixRows
              cksRows
              cksKernel
              (Foldable.toList cksKernelRows)
          acceptPrepended prependedRows
        Nothing ->
          rebuildKernelRows
  where
    rebuildKernelRows = do
      (nextKernel, kernelRows) <-
        trackedLeftKernelRows
          context
          emptyTrackedLeftKernel
          rowsValue
      let kernelRowSequence = Seq.fromList kernelRows
      Right
        ( kernelRowSequence
        , CachedKernelState
            { cksRows = rowsValue
            , cksKernel = nextKernel
            , cksKernelRows = kernelRowSequence
            }
        )

    acceptPrepended (nextKernel, kernelRows) =
      let kernelRowSequence = Seq.fromList kernelRows
       in Right
            ( kernelRowSequence
            , stateValue
                { cksRows = rowsValue
                , cksKernel = nextKernel
                , cksKernelRows = kernelRowSequence
                }
            )

cachedSpan ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  [(Int, SparseRow a)] ->
  CachedSpanState a ->
  Either MoonlightError (CachedSpanState a)
cachedSpan context rowsValue stateValue@CachedSpanState {cssRows, cssSpan} =
  case stripRowsPrefix cssRows rowsValue of
    Just suffixRows -> do
      nextSpan <-
        foldM
          admitRow
          cssSpan
          (fmap snd suffixRows)
      Right
        stateValue
          { cssRows = rowsValue
          , cssSpan = nextSpan
          }
    Nothing ->
      case stripRowsSuffix cssRows rowsValue of
        Just prefixRows -> do
          prependedSpan <-
            prependSparseSpanRows
              context
              (fmap snd prefixRows)
              (fmap snd cssRows)
              cssSpan
          acceptPrepended prependedSpan
        Nothing ->
          rebuildSpan
  where
    admitRow spanValue rowValue =
      snd
        <$> admitSparseRow
          context
          rowValue
          spanValue

    rebuildSpan = do
      nextSpan <-
        foldM
          admitRow
          emptySparseSpan
          (fmap snd rowsValue)
      Right
        CachedSpanState
          { cssRows = rowsValue
          , cssSpan = nextSpan
          }

    acceptPrepended nextSpan =
      Right
        stateValue
          { cssRows = rowsValue
          , cssSpan = nextSpan
          }

emptyAcceptedRows :: SparseSpan a -> AcceptedRows a
emptyAcceptedRows spanValue =
  AcceptedRows
    { arSpan = spanValue
    , arRowsReversed = []
    , arPiecesReversed = []
    }

appendIndependentRows ::
  (Field a, IntegralDomain a, Num a) =>
  FinObjectId ->
  StarColumnProfile ->
  BlockedMat a ->
  CachedSpanState a ->
  StarRowSource a ->
  Seq (SparseRow a) ->
  Either MoonlightError (BlockedMat a, CachedSpanState a, StarRowSource a)
appendIndependentRows
  nodeValue
  columnProfile
  currentDifferential
  currentSpanState@CachedSpanState {cssSpan}
  currentSource
  candidateRows = do
    acceptedRows <-
      foldM
        (admitCandidateRow columnProfile)
        (emptyAcceptedRows cssSpan)
        candidateRows
    let rowsToAppend =
          reverse (arRowsReversed acceptedRows)
        piecesToAppend =
          reverse (arPiecesReversed acceptedRows)
        nextDifferential =
          appendRowsOnLabel
            nodeValue
            piecesToAppend
            currentDifferential
        (keyedRows, nextSource) =
          appendSourceRows
            nodeValue
            rowsToAppend
            currentSource
    Right
      ( nextDifferential
      , currentSpanState
          { cssRows = cssRows currentSpanState <> keyedRows
          , cssSpan = arSpan acceptedRows
          }
      , nextSource
      )

admitCandidateRow ::
  (Field a, IntegralDomain a, Num a) =>
  StarColumnProfile ->
  AcceptedRows a ->
  SparseRow a ->
  Either MoonlightError (AcceptedRows a)
admitCandidateRow columnProfile acceptedRows candidateRow = do
  (maybeRemainder, nextSpan) <-
    admitSparseRow
      "makeExact: candidate independence"
      candidateRow
      (arSpan acceptedRows)
  case maybeRemainder of
    Nothing ->
      Right acceptedRows {arSpan = nextSpan}
    Just _ -> do
      rowPieces <-
        sparseRowPieces
          "makeExact: materialize accepted row"
          columnProfile
          candidateRow
      Right
        acceptedRows
          { arSpan = nextSpan
          , arRowsReversed = candidateRow : arRowsReversed acceptedRows
          , arPiecesReversed = rowPieces : arPiecesReversed acceptedRows
          }

stripRowsPrefix ::
  Eq a =>
  [(Int, SparseRow a)] ->
  [(Int, SparseRow a)] ->
  Maybe [(Int, SparseRow a)]
stripRowsPrefix [] remainingRows =
  Just remainingRows
stripRowsPrefix _ [] =
  Nothing
stripRowsPrefix
  ((oldKey, oldRow) : oldRows)
  ((newKey, newRow) : newRows)
    | oldKey == newKey && oldRow == newRow =
        stripRowsPrefix oldRows newRows
    | otherwise =
        Nothing

stripRowsSuffix ::
  Eq a =>
  [(Int, SparseRow a)] ->
  [(Int, SparseRow a)] ->
  Maybe [(Int, SparseRow a)]
stripRowsSuffix suffixRows rowsValue =
  reverse
    <$> stripRowsPrefix
      (reverse suffixRows)
      (reverse rowsValue)

starRowSourceFromBlocked ::
  String ->
  BlockedMat a ->
  Either MoonlightError (StarRowSource a)
starRowSourceFromBlocked _ blockedMat =
  Right
    StarRowSource
      { srsRowsAxis = bmRows blockedMat
      , srsColumnsAxis = bmCols blockedMat
      , srsBlockedMat = blockedMat
      , srsRowOffsets = rowOffsets
      , srsCurrentRowOffsets = rowOffsets
      , srsColumnOffsets = columnOffsets
      , srsOriginalRowsByLabel = IntMap.empty
      , srsAddedRowsByLabel = IntMap.empty
      , srsNextRowKey = axisSize (bmRows blockedMat)
      }
  where
    rowOffsets =
      axisOffsets (bmRows blockedMat)

    columnOffsets =
      axisOffsets (bmCols blockedMat)

sourceStarRows ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  IntSet ->
  StarRowSource a ->
  Either MoonlightError ([(Int, SparseRow a)], StarRowSource a)
sourceStarRows context starSet sourceValue@StarRowSource {srsRowsAxis, srsCurrentRowOffsets} = do
  columnProfile <-
    starColumnProfileFromSource
      (context <> ": columns")
      starSet
      sourceValue
  (nextSource, sourceRowGroupsReversed) <-
    foldM
      (collectLabelRows columnProfile)
      (sourceValue, [])
      (starAxisNodesFromSet srsCurrentRowOffsets starSet srsRowsAxis)
  let sourceRows =
        concat (reverse sourceRowGroupsReversed)
  Right
    ( [ (ssrKey rowValue, restrictSparseRow (scpAllowedColumns columnProfile) (ssrRow rowValue))
      | rowValue <- sourceRows
      ]
    , nextSource
    )
  where
    collectLabelRows columnProfile (accumulatedSource, sourceRowGroupsReversed) rowLabel = do
      (nextSource, sourceRowsForLabelValue) <-
        sourceRowsForLabel
          context
          columnProfile
          rowLabel
          accumulatedSource
      let addedRows =
            IntMap.findWithDefault
              V.empty
              (unFinObjectId rowLabel)
              (srsAddedRowsByLabel nextSource)
          labelRows =
            V.toList sourceRowsForLabelValue <> V.toList addedRows
      Right (nextSource, labelRows : sourceRowGroupsReversed)

sourceRowsForLabel ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  StarColumnProfile ->
  FinObjectId ->
  StarRowSource a ->
  Either MoonlightError (StarRowSource a, V.Vector (SparseSourceRow a))
sourceRowsForLabel context columnProfile rowLabel@(FinObjectId rowKey) sourceValue@StarRowSource {srsOriginalRowsByLabel}
  | scpUsesSparseAxis columnProfile
      && not (IntMap.member rowKey srsOriginalRowsByLabel) = do
      sourceRows <-
        localSourceRowsForLabel
          context
          columnProfile
          rowLabel
          sourceValue
      Right (sourceValue, sourceRows)
  | otherwise =
      materializedSourceRowsForLabel
        context
        rowLabel
        sourceValue

localSourceRowsForLabel ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  StarColumnProfile ->
  FinObjectId ->
  StarRowSource a ->
  Either MoonlightError (V.Vector (SparseSourceRow a))
localSourceRowsForLabel context StarColumnProfile {scpSegments} rowLabel sourceValue@StarRowSource {srsBlockedMat, srsRowOffsets}
  | axisMultiplicity (bmRows srsBlockedMat) rowLabel == 0 =
      Right V.empty
  | otherwise = do
      (rowStart, rowHeight) <-
        axisLabelExtent
          (context <> ": row offsets")
          (bmRows srsBlockedMat)
          srsRowOffsets
          rowLabel
      V.fromList
        <$> traverse
          (sourceRowAtSegments context sourceValue rowLabel rowHeight rowStart scpSegments)
          [0 .. rowHeight - 1]

materializedSourceRowsForLabel ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  FinObjectId ->
  StarRowSource a ->
  Either MoonlightError (StarRowSource a, V.Vector (SparseSourceRow a))
materializedSourceRowsForLabel context rowLabel@(FinObjectId rowKey) sourceValue@StarRowSource {srsOriginalRowsByLabel} =
  case IntMap.lookup rowKey srsOriginalRowsByLabel of
    Just sourceRows ->
      Right (sourceValue, sourceRows)
    Nothing -> do
      sourceRows <-
        originalSourceRowsForLabel
          context
          rowLabel
          sourceValue
      Right
        ( sourceValue
            { srsOriginalRowsByLabel =
                IntMap.insert
                  rowKey
                  sourceRows
                  srsOriginalRowsByLabel
            }
        , sourceRows
        )

originalSourceRowsForLabel ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  FinObjectId ->
  StarRowSource a ->
  Either MoonlightError (V.Vector (SparseSourceRow a))
originalSourceRowsForLabel context rowLabel sourceValue@StarRowSource {srsBlockedMat, srsRowOffsets}
  | axisMultiplicity (bmRows srsBlockedMat) rowLabel == 0 =
      Right V.empty
  | otherwise = do
      (rowStart, rowHeight) <-
        axisLabelExtent
          (context <> ": row offsets")
          (bmRows srsBlockedMat)
          srsRowOffsets
          rowLabel
      V.fromList
        <$> traverse
          (sourceRowAt context sourceValue rowLabel rowHeight rowStart)
          [0 .. rowHeight - 1]

sourceRowAt ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  StarRowSource a ->
  FinObjectId ->
  Int ->
  Int ->
  Int ->
  Either MoonlightError (SparseSourceRow a)
sourceRowAt context sourceValue rowLabel rowHeight rowStart localRowIndex = do
  rowValue <-
    foldM
      appendColumnEntries
      emptySparseRow
      (V.toList (gaOrder (srsColumnsAxis sourceValue)))
  Right
    SparseSourceRow
      { ssrKey = rowStart + localRowIndex
      , ssrRow = rowValue
      }
  where
    rowContext =
      context <> ": stored block: dense row " <> show localRowIndex

    appendColumnEntries accumulatedRow columnLabel = do
      entriesValue <-
        blockRowEntries
          context
          sourceValue
          rowLabel
          rowHeight
          localRowIndex
          columnLabel
      appendSparseRowEntries
        rowContext
        entriesValue
        accumulatedRow

sourceRowAtSegments ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  StarRowSource a ->
  FinObjectId ->
  Int ->
  Int ->
  [StarColumnSegment] ->
  Int ->
  Either MoonlightError (SparseSourceRow a)
sourceRowAtSegments context sourceValue rowLabel rowHeight rowStart columnSegments localRowIndex = do
  rowValue <-
    foldM
      appendSegmentEntries
      emptySparseRow
      columnSegments
  Right
    SparseSourceRow
      { ssrKey = rowStart + localRowIndex
      , ssrRow = rowValue
      }
  where
    rowContext =
      context <> ": stored block: dense row " <> show localRowIndex

    appendSegmentEntries accumulatedRow columnSegment = do
      entriesValue <-
        blockSegmentRowEntries
          context
          sourceValue
          rowLabel
          rowHeight
          localRowIndex
          columnSegment
      appendSparseRowEntries
        rowContext
        entriesValue
        accumulatedRow

blockRowEntries ::
  String ->
  StarRowSource a ->
  FinObjectId ->
  Int ->
  Int ->
  FinObjectId ->
  Either MoonlightError [(Int, a)]
blockRowEntries context StarRowSource {srsBlockedMat, srsColumnOffsets} rowLabel rowHeight localRowIndex columnLabel =
  case storedBlockAt rowLabel columnLabel srsBlockedMat of
    Nothing ->
      Right []
    Just denseBlock -> do
      (columnStart, columnWidth) <-
        axisLabelExtent
          (context <> ": column offsets")
          (bmCols srsBlockedMat)
          srsColumnOffsets
          columnLabel
      denseRowEntries
        (context <> ": stored block")
        rowHeight
        columnWidth
        localRowIndex
        columnStart
        denseBlock

blockSegmentRowEntries ::
  String ->
  StarRowSource a ->
  FinObjectId ->
  Int ->
  Int ->
  StarColumnSegment ->
  Either MoonlightError [(Int, a)]
blockSegmentRowEntries context StarRowSource {srsBlockedMat} rowLabel rowHeight localRowIndex StarColumnSegment {scsNode, scsOffset, scsWidth} =
  case storedBlockAt rowLabel scsNode srsBlockedMat of
    Nothing ->
      Right []
    Just denseBlock ->
      denseRowEntries
        (context <> ": stored block")
        rowHeight
        scsWidth
        localRowIndex
        scsOffset
        denseBlock

appendSourceRows ::
  FinObjectId ->
  [SparseRow a] ->
  StarRowSource a ->
  ([(Int, SparseRow a)], StarRowSource a)
appendSourceRows _ [] sourceValue =
  ([], sourceValue)
appendSourceRows nodeValue@(FinObjectId nodeKey) rowValues sourceValue@StarRowSource {srsRowsAxis, srsCurrentRowOffsets, srsAddedRowsByLabel, srsNextRowKey} =
  ( keyedRows
  , sourceValue
      { srsRowsAxis = nextRowsAxis
      , srsCurrentRowOffsets =
          appendCurrentRowOffsets
            nodeValue
            rowCount
            srsNextRowKey
            srsRowsAxis
            srsCurrentRowOffsets
      , srsAddedRowsByLabel =
          IntMap.alter
            appendRows
            nodeKey
            srsAddedRowsByLabel
      , srsNextRowKey = srsNextRowKey + rowCount
      }
  )
  where
    rowCount =
      length rowValues

    nextRowsAxis =
      appendAxisLabel nodeValue rowCount srsRowsAxis

    rowKeys =
      [srsNextRowKey .. srsNextRowKey + rowCount - 1]

    sourceRows =
      V.fromList
        ( zipWith
            (\rowKey rowValue -> SparseSourceRow {ssrKey = rowKey, ssrRow = rowValue})
            rowKeys
            rowValues
        )

    keyedRows =
      zip rowKeys rowValues

    appendRows Nothing =
      Just sourceRows
    appendRows (Just rowsValue) =
      Just (rowsValue <> sourceRows)

appendCurrentRowOffsets ::
  FinObjectId ->
  Int ->
  Int ->
  GroupedAxis ->
  IntMap (Int, Int) ->
  IntMap (Int, Int)
appendCurrentRowOffsets nodeValue@(FinObjectId nodeKey) rowCount totalRows axisValue offsets
  | rowCount <= 0 =
      offsets
  | axisMultiplicity axisValue nodeValue == 0 =
      IntMap.insert nodeKey (totalRows, rowCount) offsets
  | otherwise =
      case IntMap.lookup nodeKey offsets of
        Nothing ->
          IntMap.insert nodeKey (totalRows, rowCount) offsets
        Just (rowStart, rowHeight) ->
          IntMap.mapWithKey
            (shiftOffset rowStart rowHeight)
            offsets
  where
    shiftOffset rowStart rowHeight keyValue (offsetValue, widthValue)
      | keyValue == nodeKey =
          (offsetValue, rowHeight + rowCount)
      | offsetValue > rowStart =
          (offsetValue + rowCount, widthValue)
      | otherwise =
          (offsetValue, widthValue)

starColumnProfileFromSource ::
  String ->
  IntSet ->
  StarRowSource a ->
  Either MoonlightError StarColumnProfile
starColumnProfileFromSource context starSet StarRowSource {srsColumnsAxis, srsColumnOffsets} =
  starColumnProfileFromOffsets
    context
    starSet
    srsColumnsAxis
    srsColumnOffsets

starColumnProfileFromOffsets ::
  String ->
  IntSet ->
  GroupedAxis ->
  IntMap (Int, Int) ->
  Either MoonlightError StarColumnProfile
starColumnProfileFromOffsets context starSet axisValue offsets = do
  segments <-
    starColumnSegmentsFromOffsets
      context
      starSet
      axisValue
      offsets
  Right
    StarColumnProfile
      { scpSegments = segments
      , scpAllowedColumns =
          IntSet.unions
            (fmap segmentIndices segments)
      , scpUsesSparseAxis =
          usesSparseStarAxis starSet axisValue
      }

starColumnSegmentsFromOffsets ::
  String ->
  IntSet ->
  GroupedAxis ->
  IntMap (Int, Int) ->
  Either MoonlightError [StarColumnSegment]
starColumnSegmentsFromOffsets context starSet axisValue offsets =
  traverse
    segmentFor
    (starAxisNodesFromSet offsets starSet axisValue)
  where
    segmentFor columnNode = do
      (columnOffset, columnWidth) <-
        axisLabelExtent
          context
          axisValue
          offsets
          columnNode
      Right
        StarColumnSegment
          { scsNode = columnNode
          , scsOffset = columnOffset
          , scsWidth = columnWidth
          }

starAxisNodesFromSet :: IntMap (Int, Int) -> IntSet -> GroupedAxis -> [FinObjectId]
starAxisNodesFromSet offsets starSet axisValue
  | usesSparseStarAxis starSet axisValue =
      sparseStarAxisNodes offsets starSet
  | otherwise =
      [ axisNode
      | axisNode <- V.toList axisOrder
      , IntSet.member
          (unFinObjectId axisNode)
          starSet
      ]
  where
    axisOrder =
      gaOrder axisValue

usesSparseStarAxis :: IntSet -> GroupedAxis -> Bool
usesSparseStarAxis starSet axisValue =
  IntSet.size starSet * 4 < V.length (gaOrder axisValue)

sparseStarAxisNodes :: IntMap (Int, Int) -> IntSet -> [FinObjectId]
sparseStarAxisNodes offsets starSet =
  fmap snd
    (sortOn fst offsetOrderedNodes)
  where
    offsetOrderedNodes =
      IntSet.foldr
        insertAxisNode
        []
        starSet

    insertAxisNode nodeKey orderedNodes =
      case IntMap.lookup nodeKey offsets of
        Nothing ->
          orderedNodes
        Just (columnOffset, _) ->
          (columnOffset, FinObjectId nodeKey) : orderedNodes

sparseRowPieces ::
  Num a =>
  String ->
  StarColumnProfile ->
  SparseRow a ->
  Either MoonlightError [(FinObjectId, V.Vector a)]
sparseRowPieces context StarColumnProfile {scpSegments, scpAllowedColumns} rowValue =
  validateSparseRowSupport context scpAllowedColumns rowValue
    *> traverse
      segmentPiece
      scpSegments
  where
    rowEntries =
      sparseRowEntries rowValue

    segmentPiece StarColumnSegment {scsNode, scsOffset, scsWidth} =
      Right
        ( scsNode
        , V.generate
            scsWidth
            ( \localIndex ->
                IntMap.findWithDefault
                  0
                  (scsOffset + localIndex)
                  rowEntries
            )
        )

validateSparseRowSupport ::
  String ->
  IntSet ->
  SparseRow a ->
  Either MoonlightError ()
validateSparseRowSupport context allowedColumns rowValue =
  case IntMap.lookupMin outOfRangeEntries of
    Nothing ->
      Right ()
    Just (coordinateValue, _) ->
      Left
        ( InvariantViolation
            ( context
                <> ": sparse coordinate "
                <> show coordinateValue
                <> " is outside the local star columns"
            )
        )
  where
    outOfRangeEntries =
      IntMap.filterWithKey
        (\indexValue _ -> not (IntSet.member indexValue allowedColumns))
        (sparseRowEntries rowValue)

denseRowEntries ::
  String ->
  Int ->
  Int ->
  Int ->
  Int ->
  DenseMat a ->
  Either MoonlightError [(Int, a)]
denseRowEntries
  context
  expectedRows
  expectedColumns
  rowIndexValue
  columnStart
  DenseMat {dmRows, dmCols, dmData}
    | dmRows /= expectedRows || dmCols /= expectedColumns =
        Left
          ( InvariantViolation
              ( context
                  <> ": block shape "
                  <> show (dmRows, dmCols)
                  <> " does not match axis shape "
                  <> show (expectedRows, expectedColumns)
              )
          )
    | V.length dmData /= dmRows =
        Left
          ( InvariantViolation
              ( context
                  <> ": dense row metadata mismatch "
                  <> show (dmRows, V.length dmData)
              )
          )
    | otherwise =
        case dmData V.!? rowIndexValue of
          Nothing ->
            Left
              ( InvariantViolation
                  ( context
                      <> ": row index "
                      <> show rowIndexValue
                      <> " is outside block height "
                      <> show dmRows
                  )
              )
          Just rowVector
            | V.length rowVector /= dmCols ->
                Left
                  ( InvariantViolation
                      ( context
                          <> ": dense row "
                          <> show rowIndexValue
                          <> " has width "
                          <> show (V.length rowVector)
                          <> ", expected "
                          <> show dmCols
                      )
                  )
            | otherwise ->
                Right
                  ( V.ifoldr
                      ( \columnIndex coefficientValue entriesValue ->
                          (columnStart + columnIndex, coefficientValue) : entriesValue
                      )
                      []
                      rowVector
                  )

axisOffsets :: GroupedAxis -> IntMap (Int, Int)
axisOffsets axisValue =
  IntMap.fromList (V.toList offsetEntries)
  where
    (_, offsetEntries) =
      scanMap
        step
        0
        (gaOrder axisValue)

    step offsetValue nodeValue@(FinObjectId nodeKey) =
      let widthValue =
            axisMultiplicity axisValue nodeValue
       in ( offsetValue + widthValue
          , (nodeKey, (offsetValue, widthValue))
          )

axisLabelExtent ::
  String ->
  GroupedAxis ->
  IntMap (Int, Int) ->
  FinObjectId ->
  Either MoonlightError (Int, Int)
axisLabelExtent context axisValue offsets nodeValue@(FinObjectId nodeKey)
  | widthValue < 0 =
      Left
        ( InvariantViolation
            ( context
                <> ": negative axis multiplicity "
                <> show widthValue
                <> " for "
                <> show nodeValue
            )
        )
  | otherwise =
      maybe
        ( Left
            ( InvariantViolation
                ( context
                    <> ": missing axis offset for "
                    <> show nodeValue
                )
            )
        )
        Right
        (IntMap.lookup nodeKey offsets)
  where
    widthValue =
      axisMultiplicity axisValue nodeValue

segmentIndices :: StarColumnSegment -> IntSet
segmentIndices StarColumnSegment {scsOffset, scsWidth} =
  IntSet.fromDistinctAscList [scsOffset .. scsOffset + scsWidth - 1]
