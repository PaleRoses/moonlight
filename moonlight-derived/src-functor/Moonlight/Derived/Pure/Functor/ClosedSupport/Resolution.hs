{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Derived.Pure.Functor.ClosedSupport.Resolution
  ( ClosedSupportResolutionCounters (..)
  , ClosedSupportResolutionReport (..)
  , closedSupportResolution
  , closedSupportResolutionWithCounters
  ) where

import Control.Monad (foldM)
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Vector (Vector)
import Data.Vector qualified as V
import Moonlight.Algebra (IntegralDomain)
import Moonlight.Core
  ( Field
  , MoonlightError (..)
  )
import Moonlight.Core (unfoldM)
import Moonlight.Derived.Pure.Functor.ClosedSupport.Geometry
  ( ClosedSupport
  , closedSupportNodes
  , closedSupportPoset
  , maximalSupportNodes
  , supportNodesDescending
  )
import Moonlight.Derived.Pure.LinAlg.SparseEchelon
  ( SparseRow
  , admitSparseRow
  , restrictSparseRow
  , sparseRowEntries
  , sparseRowFromEntries
  , spanFromRows
  , trackedLeftKernel
  )
import Moonlight.Derived.Pure.Site.InjectiveComplex
  ( Derived
  , InjectiveComplex (..)
  , mkNormalizedDerivedTrusted
  , trustLawfulInjectiveComplex
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix
  ( BlockedMat
  , emptyAxis
  , fromLabels
  , zeroBlocked
  )
import Moonlight.Derived.Pure.LinAlg.LabeledMatrixSparse
  ( blockedFromLabeledSparseRows
  )
import Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset (..)
  , FinObjectId (..)
  , star
  )

type LabeledSparseRow :: Type -> Type
data LabeledSparseRow a =
  LabeledSparseRow !FinObjectId !(SparseRow a)

type ClosedSupportResolutionCounters :: Type
data ClosedSupportResolutionCounters = ClosedSupportResolutionCounters
  { csrcObjectGeneratorCounts :: !(Vector Int)
  , csrcTotalGenerators :: !Int
  , csrcStoredDifferentialNonZeros :: !Int
  , csrcAcceptedRows :: !Int
  , csrcLocalCandidateReductions :: !Int
  }
  deriving stock (Eq, Show)

type ClosedSupportResolutionReport :: Type -> Type
data ClosedSupportResolutionReport a = ClosedSupportResolutionReport
  { csrrDerived :: !(Derived a)
  , csrrCounters :: !ClosedSupportResolutionCounters
  }

type IndexedRowsByLabel :: Type -> Type
type IndexedRowsByLabel a =
  IntMap (Seq (Int, SparseRow a))

type SparseDifferential :: Type -> Type
data SparseDifferential a = SparseDifferential
  { sdColumnLabels :: !(Vector FinObjectId)
  , sdColumnIndicesByLabel :: !(IntMap IntSet)
  , sdRows :: !(Vector (LabeledSparseRow a))
  , sdRowsByLabel :: !(IndexedRowsByLabel a)
  , sdAcceptedRows :: !Int
  , sdLocalCandidateReductions :: !Int
  }

type RowBuilder :: Type -> Type
data RowBuilder a = RowBuilder
  { rbColumnLabels :: !(Vector FinObjectId)
  , rbRows :: !(Seq (LabeledSparseRow a))
  , rbRowsByLabel :: !(IndexedRowsByLabel a)
  , rbAcceptedRows :: !Int
  , rbLocalCandidateReductions :: !Int
  }

closedSupportResolution ::
  (Field a, IntegralDomain a, Num a) =>
  ClosedSupport ->
  Either MoonlightError (Derived a)
closedSupportResolution supportValue =
  csrrDerived
    <$> closedSupportResolutionWithCounters supportValue

closedSupportResolutionWithCounters ::
  (Field a, IntegralDomain a, Num a) =>
  ClosedSupport ->
  Either MoonlightError (ClosedSupportResolutionReport a)
closedSupportResolutionWithCounters supportValue = do
  let maximalNodes =
        maximalSupportNodes posetValue validatedSupport
      initialLabels =
        V.fromList maximalNodes
      posetValue = closedSupportPoset supportValue
      validatedSupport = closedSupportNodes supportValue

  if IntSet.null validatedSupport
    then
      mkConcentratedResolutionReport posetValue initialLabels
    else
      case maximalNodes of
        [] ->
          Left
            ( InvariantViolation
                "closedSupportResolution: nonempty closed support has no maximal node"
            )
        _ -> do
          firstDifferential <-
            buildFirstDifferential
              posetValue
              validatedSupport
              initialLabels

          if sparseDifferentialRowCount firstDifferential == 0
            then
              mkConcentratedResolutionReport
                posetValue
                initialLabels
            else do
              sparseDifferentials <-
                completeSparseResolution
                  posetValue
                  validatedSupport
                  (IntSet.size validatedSupport + 1)
                  firstDifferential

              blockedDifferentials <-
                traverse
                  materializeSparseDifferential
                  sparseDifferentials

              let derivedValue =
                    mkNormalizedDerivedTrusted
                      posetValue
                      ( trustLawfulInjectiveComplex
                          InjectiveComplex
                            { icStart = 0
                            , icDiffs =
                                V.fromList blockedDifferentials
                            }
                      )

              Right
                ClosedSupportResolutionReport
                  { csrrDerived = derivedValue
                  , csrrCounters =
                      sparseResolutionCounters
                        initialLabels
                        sparseDifferentials
                  }

mkConcentratedResolutionReport ::
  DerivedPoset ->
  Vector FinObjectId ->
  Either MoonlightError (ClosedSupportResolutionReport a)
mkConcentratedResolutionReport posetValue initialLabels =
  let derivedValue =
        mkNormalizedDerivedTrusted
          posetValue
          ( trustLawfulInjectiveComplex
              InjectiveComplex
                { icStart = 0
                , icDiffs =
                    V.singleton
                      (zeroBlocked emptyAxis (fromLabels initialLabels))
                }
          )
   in Right
        ClosedSupportResolutionReport
          { csrrDerived = derivedValue
          , csrrCounters =
              concentratedResolutionCounters initialLabels
          }

buildFirstDifferential ::
  (Field a, IntegralDomain a, Num a) =>
  DerivedPoset ->
  IntSet ->
  Vector FinObjectId ->
  Either MoonlightError (SparseDifferential a)
buildFirstDifferential posetValue supportNodeSet initialLabels = do
  completedBuilder <-
    foldM
      addRowsAtNode
      (emptyRowBuilder initialLabels)
      (supportNodesDescending posetValue supportNodeSet)
  Right (finishRowBuilder completedBuilder)
  where
    columnIndicesByLabel =
      indicesByLabel initialLabels

    addRowsAtNode builder nodeValue = do
      let visibleColumns =
            visibleIndicesAt
              posetValue
              nodeValue
              columnIndicesByLabel

      candidates <-
        sumZeroBasis
          "closedSupportResolution: first differential"
          visibleColumns

      case candidates of
        [] ->
          Right builder
        _ ->
          admitCandidatesAtNode
            "closedSupportResolution: first differential"
            posetValue
            nodeValue
            visibleColumns
            candidates
            builder

completeSparseResolution ::
  (Field a, IntegralDomain a, Num a) =>
  DerivedPoset ->
  IntSet ->
  Int ->
  SparseDifferential a ->
  Either MoonlightError [SparseDifferential a]
completeSparseResolution
  posetValue
  supportNodeSet
  initialFuel
  firstDifferential =
    fmap
      (firstDifferential :)
      (unfoldM step (initialFuel, firstDifferential))
  where
    step (remainingFuel, previousDifferential)
      | remainingFuel <= 0 =
          Left
            ( InvariantViolation
                "closedSupportResolution: sparse exact completion exceeded the finite-poset dimension bound"
            )
      | otherwise = do
          nextDifferential <-
            buildNextDifferential
              posetValue
              supportNodeSet
              previousDifferential

          if sparseDifferentialRowCount nextDifferential == 0
            then
              Right Nothing
            else
              Right
                ( Just
                    ( nextDifferential
                    , (remainingFuel - 1, nextDifferential)
                    )
                )

buildNextDifferential ::
  (Field a, IntegralDomain a, Num a) =>
  DerivedPoset ->
  IntSet ->
  SparseDifferential a ->
  Either MoonlightError (SparseDifferential a)
buildNextDifferential
  posetValue
  supportNodeSet
  previousDifferential = do
    completedBuilder <-
      foldM
        addRowsAtNode
        (emptyRowBuilder nextColumnLabels)
        (supportNodesDescending posetValue supportNodeSet)
    Right (finishRowBuilder completedBuilder)
  where
    nextColumnLabels =
      V.map
        labeledSparseRowNode
        (sdRows previousDifferential)

    addRowsAtNode builder nodeValue = do
      let visiblePreviousColumns =
            visibleIndicesAt
              posetValue
              nodeValue
              (sdColumnIndicesByLabel previousDifferential)

          visiblePreviousRows =
            visibleIndexedRowsAt
              posetValue
              nodeValue
              (sdRowsByLabel previousDifferential)

          restrictedPreviousRows =
            fmap
              ( \(rowIndexValue, rowValue) ->
                  ( rowIndexValue
                  , restrictSparseRow
                      visiblePreviousColumns
                      rowValue
                  )
              )
              visiblePreviousRows

          visibleCurrentColumns =
            IntSet.fromList
              (fmap fst visiblePreviousRows)

      kernelCandidates <-
        trackedLeftKernel
          ( "closedSupportResolution: exact completion at "
              <> show nodeValue
          )
          restrictedPreviousRows

      case kernelCandidates of
        [] ->
          Right builder
        _ ->
          admitCandidatesAtNode
            "closedSupportResolution: exact completion"
            posetValue
            nodeValue
            visibleCurrentColumns
            kernelCandidates
            builder

admitCandidatesAtNode ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  DerivedPoset ->
  FinObjectId ->
  IntSet ->
  [SparseRow a] ->
  RowBuilder a ->
  Either MoonlightError (RowBuilder a)
admitCandidatesAtNode
  context
  posetValue
  nodeValue
  visibleColumns
  candidates
  builder = do
    existingSpan <-
      spanFromRows
        (context <> ": existing local image at " <> show nodeValue)
        ( fmap
            (restrictSparseRow visibleColumns . snd)
            ( visibleIndexedRowsAt
                posetValue
                nodeValue
                (rbRowsByLabel builder)
            )
        )

    let builderWithReductionCount =
          recordCandidateReductions
            (length candidates)
            builder

    fst
      <$> foldM
        admitCandidate
        (builderWithReductionCount, existingSpan)
        candidates
  where
    admitCandidate
      (currentBuilder, currentSpan)
      candidateRow = do
        (maybeAdmittedRow, nextSpan) <-
          admitSparseRow
            (context <> ": candidate at " <> show nodeValue)
            candidateRow
            currentSpan

        case maybeAdmittedRow of
          Nothing ->
            Right (currentBuilder, nextSpan)
          Just admittedRow ->
            Right
              ( appendBuilderRow
                  nodeValue
                  admittedRow
                  currentBuilder
              , nextSpan
              )

sumZeroBasis ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  IntSet ->
  Either MoonlightError [SparseRow a]
sumZeroBasis context visibleColumns =
  case separateLast (IntSet.toAscList visibleColumns) of
    Nothing ->
      Left
        ( InvariantViolation
            ( context
                <> ": a support node is not below any maximal support node"
            )
        )
    Just ([], _) ->
      Right []
    Just (prefixIndices, lastIndex) ->
      traverse
        ( \prefixIndex ->
            sparseRowFromEntries
              context
              [ (prefixIndex, 1)
              , (lastIndex, negate 1)
              ]
        )
        prefixIndices

separateLast :: [value] -> Maybe ([value], value)
separateLast values =
  case reverse values of
    [] ->
      Nothing
    lastValue : reversedPrefix ->
      Just (reverse reversedPrefix, lastValue)

emptyRowBuilder :: Vector FinObjectId -> RowBuilder a
emptyRowBuilder columnLabels =
  RowBuilder
    { rbColumnLabels = columnLabels
    , rbRows = Seq.empty
    , rbRowsByLabel = IntMap.empty
    , rbAcceptedRows = 0
    , rbLocalCandidateReductions = 0
    }

appendBuilderRow ::
  FinObjectId ->
  SparseRow a ->
  RowBuilder a ->
  RowBuilder a
appendBuilderRow
  nodeValue@(FinObjectId nodeKey)
  rowValue
  builder@RowBuilder {rbRows, rbRowsByLabel, rbAcceptedRows} =
    builder
      { rbRows =
          rbRows |> LabeledSparseRow nodeValue rowValue
      , rbRowsByLabel =
          IntMap.insertWith
            appendNewRows
            nodeKey
            (Seq.singleton (Seq.length rbRows, rowValue))
            rbRowsByLabel
      , rbAcceptedRows =
          rbAcceptedRows + 1
      }

recordCandidateReductions ::
  Int ->
  RowBuilder a ->
  RowBuilder a
recordCandidateReductions candidateCount builder@RowBuilder {rbLocalCandidateReductions} =
  builder
    { rbLocalCandidateReductions =
        rbLocalCandidateReductions + candidateCount
    }

appendNewRows :: Seq row -> Seq row -> Seq row
appendNewRows newRows oldRows =
  oldRows <> newRows

finishRowBuilder :: RowBuilder a -> SparseDifferential a
finishRowBuilder
  RowBuilder
    { rbColumnLabels
    , rbRows
    , rbRowsByLabel
    , rbAcceptedRows
    , rbLocalCandidateReductions
    } =
      SparseDifferential
        { sdColumnLabels = rbColumnLabels
        , sdColumnIndicesByLabel =
            indicesByLabel rbColumnLabels
        , sdRows =
            V.fromList (toList rbRows)
        , sdRowsByLabel = rbRowsByLabel
        , sdAcceptedRows = rbAcceptedRows
        , sdLocalCandidateReductions = rbLocalCandidateReductions
        }

sparseDifferentialRowCount :: SparseDifferential a -> Int
sparseDifferentialRowCount =
  V.length . sdRows

labeledSparseRowNode :: LabeledSparseRow a -> FinObjectId
labeledSparseRowNode (LabeledSparseRow nodeValue _) =
  nodeValue

indicesByLabel :: Vector FinObjectId -> IntMap IntSet
indicesByLabel =
  V.ifoldl'
    ( \indicesMap indexValue (FinObjectId nodeKey) ->
        IntMap.insertWith
          IntSet.union
          nodeKey
          (IntSet.singleton indexValue)
          indicesMap
    )
    IntMap.empty

visibleIndicesAt ::
  DerivedPoset ->
  FinObjectId ->
  IntMap IntSet ->
  IntSet
visibleIndicesAt posetValue nodeValue indicesMap =
  IntSet.foldl'
    ( \visibleIndices labelKey ->
        IntSet.union
          visibleIndices
          ( IntMap.findWithDefault
              IntSet.empty
              labelKey
              indicesMap
          )
    )
    IntSet.empty
    (star posetValue nodeValue)

visibleIndexedRowsAt ::
  DerivedPoset ->
  FinObjectId ->
  IndexedRowsByLabel a ->
  [(Int, SparseRow a)]
visibleIndexedRowsAt posetValue nodeValue rowsByLabel =
  toList
    ( IntSet.foldl'
        ( \visibleRows labelKey ->
            visibleRows
              <> IntMap.findWithDefault
                Seq.empty
                labelKey
                rowsByLabel
        )
        Seq.empty
        (star posetValue nodeValue)
    )

materializeSparseDifferential ::
  (IntegralDomain a, Num a) =>
  SparseDifferential a ->
  Either MoonlightError (BlockedMat a)
materializeSparseDifferential
  SparseDifferential
    { sdColumnLabels
    , sdRows
    } =
      blockedFromLabeledSparseRows
        sdColumnLabels
        ( V.map
            ( \(LabeledSparseRow nodeValue rowValue) ->
                (nodeValue, rowValue)
            )
            sdRows
        )

concentratedResolutionCounters ::
  Vector FinObjectId ->
  ClosedSupportResolutionCounters
concentratedResolutionCounters initialLabels =
  ClosedSupportResolutionCounters
    { csrcObjectGeneratorCounts =
        V.singleton (V.length initialLabels)
    , csrcTotalGenerators =
        V.length initialLabels
    , csrcStoredDifferentialNonZeros = 0
    , csrcAcceptedRows = 0
    , csrcLocalCandidateReductions = 0
    }

sparseResolutionCounters ::
  Vector FinObjectId ->
  [SparseDifferential a] ->
  ClosedSupportResolutionCounters
sparseResolutionCounters initialLabels sparseDifferentials =
  ClosedSupportResolutionCounters
    { csrcObjectGeneratorCounts = objectGeneratorCounts
    , csrcTotalGenerators = V.sum objectGeneratorCounts
    , csrcStoredDifferentialNonZeros =
        sum (fmap sparseDifferentialNonZeroCount sparseDifferentials)
    , csrcAcceptedRows =
        sum (fmap sdAcceptedRows sparseDifferentials)
    , csrcLocalCandidateReductions =
        sum (fmap sdLocalCandidateReductions sparseDifferentials)
    }
  where
    objectGeneratorCounts =
      V.fromList
        ( V.length initialLabels
            : fmap sparseDifferentialRowCount sparseDifferentials
        )

sparseDifferentialNonZeroCount :: SparseDifferential a -> Int
sparseDifferentialNonZeroCount =
  V.sum
    . V.map labeledSparseRowNonZeroCount
    . sdRows

labeledSparseRowNonZeroCount :: LabeledSparseRow a -> Int
labeledSparseRowNonZeroCount (LabeledSparseRow _ rowValue) =
  sparseRowNonZeroCount rowValue

sparseRowNonZeroCount :: SparseRow a -> Int
sparseRowNonZeroCount =
  IntMap.size . sparseRowEntries
