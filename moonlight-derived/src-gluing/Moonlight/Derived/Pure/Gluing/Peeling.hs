{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Derived.Pure.Gluing.Peeling
  ( minimizeComplex
  , minimizeComposableComplex
  , minimizeComplexFromFrontier
  , minimizeComplexFromFrontierWithRank
  ) where

import Control.Monad (foldM)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List (foldl')
import Data.Sequence qualified as Seq
import Data.Vector qualified as V
import Moonlight.Algebra (IntegralDomain)
import Moonlight.Core (Field, MoonlightError)
import Moonlight.Derived.Pure.Gluing.Peeling.Dense
  ( denseIsZero
  , denseMul
  , denseSub
  , lookupVector
  , replaceVector
  , selectDenseSubmatrix
  , setBlockedBlock
  , survivingIndices
  )
import Moonlight.Derived.Pure.Gluing.Peeling.Pivot
  ( EliminationWitness (..)
  , PivotMinor (..)
  , pivotMinorFromWitness
  , solveLeftWithEliminationWitness
  , sortedPivotColumns
  , sortedPivotRows
  , trustedEliminationWitness
  )
import Moonlight.Derived.Pure.Site.InjectiveComplex
import Moonlight.Derived.Pure.Site.LabeledMatrix
import Moonlight.Derived.Pure.Site.Poset (FinObjectId (..))

type SchurLeftArm :: Type -> Type
data SchurLeftArm a = SchurLeftArm !FinObjectId !(DenseMat a)

type SchurRightArm :: Type -> Type
data SchurRightArm a = SchurRightArm !FinObjectId !(DenseMat a)

type SchurSolvedLeftArm :: Type -> Type
data SchurSolvedLeftArm a = SchurSolvedLeftArm !FinObjectId !(DenseMat a)

type SchurPeelResult :: Type -> Type
data SchurPeelResult a = SchurPeelResult
  { sprDifferential :: !(BlockedMat a)
  , sprTouchedDiagonals :: ![FinObjectId]
  }

type PeelingStepResult :: Type -> Type
data PeelingStepResult a = PeelingStepResult
  { psrComplex :: !(InjectiveComplex a)
  , psrAffected :: ![(Int, FinObjectId)]
  }

type MinimizationFrontier :: Type
data MinimizationFrontier = MinimizationFrontier
  { mfPending :: !(IntMap.IntMap IntSet.IntSet)
  , mfQueue :: !(Seq.Seq (Int, FinObjectId))
  }

minimizeComplex ::
  (Field a, Num a, IntegralDomain a) =>
  InjectiveComplex a ->
  Either MoonlightError (InjectiveComplex a)
minimizeComplex initialComplex =
  fmap
    (preserveDegreeWindow initialComplex)
    (drainMinimizationFrontier rankProfileDense initialComplex (initialNonMinimalFrontier initialComplex))

-- | Minimization transports the already established chain proof: Schur
-- cancellation preserves adjacent axes and @d^2 = 0@ by construction.
minimizeComposableComplex ::
  (Field a, Num a, IntegralDomain a) =>
  ComposableInjectiveComplex a ->
  Either MoonlightError (ComposableInjectiveComplex a)
minimizeComposableComplex composableValue =
  fmap
    trustComposableInjectiveComplex
    (minimizeComplex (composableInjectiveComplex composableValue))

minimizeComplexFromFrontier ::
  (Field a, Num a, IntegralDomain a) =>
  [(Int, FinObjectId)] ->
  InjectiveComplex a ->
  Either MoonlightError (InjectiveComplex a)
minimizeComplexFromFrontier initialFrontier initialComplex =
  fmap
    (preserveDegreeWindow initialComplex)
    (drainMinimizationFrontier rankProfileDense initialComplex initialFrontier)

minimizeComplexFromFrontierWithRank ::
  (Field a, Num a, IntegralDomain a) =>
  (DenseMat a -> Either MoonlightError Int) ->
  [(Int, FinObjectId)] ->
  InjectiveComplex a ->
  Either MoonlightError (InjectiveComplex a)
minimizeComplexFromFrontierWithRank rankDiagonal initialFrontier initialComplex =
  fmap
    (preserveDegreeWindow initialComplex)
    (drainMinimizationFrontier rankDiagonal initialComplex initialFrontier)

preserveDegreeWindow ::
  Num a =>
  InjectiveComplex a ->
  InjectiveComplex a ->
  InjectiveComplex a
preserveDegreeWindow originalComplex minimizedComplex
  | icStart originalComplex == icStart minimizedComplex
  , V.length (icDiffs originalComplex) == V.length (icDiffs minimizedComplex) =
      minimizedComplex
  | otherwise =
      InjectiveComplex
        { icStart = icStart originalComplex
        , icDiffs = V.fromList (fmap differentialAt originalDifferentialDegrees)
        }
  where
    originalDifferentialCount =
      V.length (icDiffs originalComplex)

    originalDifferentialDegrees =
      [icStart originalComplex .. icStart originalComplex + originalDifferentialCount - 1]

    minimizedAxes =
      complexObjectAxes minimizedComplex

    minimizedAxesByDegree =
      IntMap.fromList
        (zip [icStart minimizedComplex .. icStart minimizedComplex + length minimizedAxes - 1] minimizedAxes)

    minimizedDifferentialsByDegree =
      IntMap.fromList
        (zip [icStart minimizedComplex ..] (V.toList (icDiffs minimizedComplex)))

    objectAxisAt degreeValue =
      IntMap.findWithDefault emptyAxis degreeValue minimizedAxesByDegree

    differentialAt degreeValue =
      IntMap.findWithDefault
        (zeroBlocked (objectAxisAt (degreeValue + 1)) (objectAxisAt degreeValue))
        degreeValue
        minimizedDifferentialsByDegree

drainMinimizationFrontier ::
  (Field a, Num a, IntegralDomain a) =>
  (DenseMat a -> Either MoonlightError Int) ->
  InjectiveComplex a ->
  [(Int, FinObjectId)] ->
  Either MoonlightError (InjectiveComplex a)
drainMinimizationFrontier rankDiagonal initialComplex initialFrontier =
  drainFrontier initialComplex (frontierFromList initialFrontier)
  where
    drainFrontier injectiveComplex frontierValue =
      case dequeueFrontier frontierValue of
        Nothing ->
          Right injectiveComplex
        Just (nonMinimalLocation, remainingFrontier) ->
          if nonMinimalAt injectiveComplex nonMinimalLocation
            then do
              stepResult <- minimizeStep rankDiagonal injectiveComplex nonMinimalLocation
              drainFrontier
                (psrComplex stepResult)
                (enqueueFrontierLocations (psrAffected stepResult) remainingFrontier)
            else
              drainFrontier injectiveComplex remainingFrontier

frontierFromList ::
  [(Int, FinObjectId)] ->
  MinimizationFrontier
frontierFromList =
  foldl'
    (flip enqueueFrontierLocation)
    (MinimizationFrontier IntMap.empty Seq.empty)

enqueueFrontierLocations ::
  [(Int, FinObjectId)] ->
  MinimizationFrontier ->
  MinimizationFrontier
enqueueFrontierLocations locations frontierValue =
  foldl'
    (flip enqueueFrontierLocation)
    frontierValue
    locations

enqueueFrontierLocation ::
  (Int, FinObjectId) ->
  MinimizationFrontier ->
  MinimizationFrontier
enqueueFrontierLocation location@(differentialIndex, FinObjectId objectKey) frontierValue@MinimizationFrontier {mfPending, mfQueue} =
  if IntSet.member objectKey (IntMap.findWithDefault IntSet.empty differentialIndex mfPending)
    then frontierValue
    else
      MinimizationFrontier
        { mfPending =
            IntMap.insertWith
              IntSet.union
              differentialIndex
              (IntSet.singleton objectKey)
              mfPending
        , mfQueue = mfQueue Seq.|> location
        }

dequeueFrontier ::
  MinimizationFrontier ->
  Maybe ((Int, FinObjectId), MinimizationFrontier)
dequeueFrontier MinimizationFrontier {mfPending, mfQueue} =
  case Seq.viewl mfQueue of
    Seq.EmptyL ->
      Nothing
    location@(differentialIndex, FinObjectId objectKey) Seq.:< remainingQueue ->
      Just
        ( location
        , MinimizationFrontier
            { mfPending =
                IntMap.update
                  removeNode
                  differentialIndex
                  mfPending
            , mfQueue = remainingQueue
            }
        )
      where
        removeNode pendingNodes =
          let nextNodes =
                IntSet.delete objectKey pendingNodes
           in if IntSet.null nextNodes
                then Nothing
                else Just nextNodes

rankProfileDense ::
  (Field a, Num a, IntegralDomain a) =>
  DenseMat a ->
  Either MoonlightError Int
rankProfileDense =
  fmap ewRank . trustedEliminationWitness

initialNonMinimalFrontier ::
  IntegralDomain a =>
  InjectiveComplex a ->
  [(Int, FinObjectId)]
initialNonMinimalFrontier InjectiveComplex {icDiffs} =
  concat
    ( V.toList
        ( V.imap
            nonMinimalDifferentialLocations
            icDiffs
        )
    )

nonMinimalDifferentialLocations ::
  IntegralDomain a =>
  Int ->
  BlockedMat a ->
  [(Int, FinObjectId)]
nonMinimalDifferentialLocations differentialIndex blockedMat =
  fmap
    (differentialIndex,)
    (storedNonMinimalDiagonalLabels blockedMat)

storedNonMinimalDiagonalLabels ::
  IntegralDomain a =>
  BlockedMat a ->
  [FinObjectId]
storedNonMinimalDiagonalLabels BlockedMat {bmBlocks} =
  fmap
    FinObjectId
    ( IntMap.keys
        ( IntMap.filterWithKey
            hasStoredNonzeroDiagonal
            bmBlocks
        )
    )
  where
    hasStoredNonzeroDiagonal ::
      IntegralDomain a =>
      Int ->
      IntMap.IntMap (DenseMat a) ->
      Bool
    hasStoredNonzeroDiagonal rowKey rowMap =
      case IntMap.lookup rowKey rowMap of
        Nothing ->
          False
        Just diagonalBlock ->
          not (denseIsZero diagonalBlock)

nonMinimalAt ::
  IntegralDomain a =>
  InjectiveComplex a ->
  (Int, FinObjectId) ->
  Bool
nonMinimalAt InjectiveComplex {icDiffs} (differentialIndex, nodeValue) =
  case icDiffs V.!? differentialIndex of
    Nothing ->
      False
    Just blockedMat ->
      nonMinimalDiagonalAt nodeValue blockedMat

nonMinimalDiagonalAt ::
  IntegralDomain a =>
  FinObjectId ->
  BlockedMat a ->
  Bool
nonMinimalDiagonalAt nodeValue blockedMat =
  case storedBlockAt nodeValue nodeValue blockedMat of
    Nothing ->
      False
    Just diagonalBlock ->
      not (denseIsZero diagonalBlock)

minimizeStep ::
  (Field a, Num a, IntegralDomain a) =>
  (DenseMat a -> Either MoonlightError Int) ->
  InjectiveComplex a ->
  (Int, FinObjectId) ->
  Either MoonlightError (PeelingStepResult a)
minimizeStep _ injectiveComplex@InjectiveComplex {icDiffs} _
  | V.null icDiffs =
      Right
        PeelingStepResult
          { psrComplex = injectiveComplex
          , psrAffected = []
          }
minimizeStep rankDiagonal injectiveComplex@InjectiveComplex {icDiffs} (differentialIndex, nodeValue) = do
  currentDifferential <-
    lookupVector
      "minimizeComplex: current differential index is out of bounds"
      differentialIndex
      icDiffs
  case storedBlockAt nodeValue nodeValue currentDifferential of
    Nothing ->
      Right
        PeelingStepResult
          { psrComplex = injectiveComplex
          , psrAffected = []
          }
    Just diagonalBlock -> do
      if hasSchurArmAt nodeValue currentDifferential
        then minimizeStepWithSchur injectiveComplex differentialIndex nodeValue currentDifferential diagonalBlock
        else do
          let previousDifferential =
                if differentialIndex <= 0
                  then Nothing
                  else icDiffs V.!? (differentialIndex - 1)
              nextDifferential =
                icDiffs V.!? (differentialIndex + 1)
          rankPeeling <-
            rankOnlyPeeling rankDiagonal previousDifferential nextDifferential diagonalBlock
          case rankPeeling of
            NoPeeling ->
              Right
                PeelingStepResult
                  { psrComplex = injectiveComplex
                  , psrAffected = []
                  }
            IsolatedRankPeeling rankValue -> do
              let currentPeeled =
                    removeIsolatedDiagonalRank nodeValue rankValue currentDifferential

              peeledDiffs <-
                replacePeeledDifferentials
                  differentialIndex
                  icDiffs
                  currentPeeled
                  Nothing
                  Nothing

              pure
                PeelingStepResult
                  { psrComplex = injectiveComplex {icDiffs = peeledDiffs}
                  , psrAffected =
                      affectedLocations
                        differentialIndex
                        nodeValue
                        currentPeeled
                        Nothing
                        Nothing
                  }
            ProfileRankPeeling witness -> do
              let rowDeletions =
                    sortedIndices (ewRows witness)
                  columnDeletions =
                    sortedIndices (ewCols witness)
                  currentPeeled =
                    removeRowsOnLabel nodeValue rowDeletions
                      (removeColsOnLabel nodeValue columnDeletions currentDifferential)
                  previousPeeled =
                    fmap
                      (removeRowsOnLabel nodeValue columnDeletions)
                      previousDifferential
                  nextPeeled =
                    fmap
                      (removeColsOnLabel nodeValue rowDeletions)
                      nextDifferential

              peeledDiffs <-
                replacePeeledDifferentials
                  differentialIndex
                  icDiffs
                  currentPeeled
                  previousPeeled
                  nextPeeled

              pure
                PeelingStepResult
                  { psrComplex = injectiveComplex {icDiffs = peeledDiffs}
                  , psrAffected =
                      affectedLocations
                        differentialIndex
                        nodeValue
                        currentPeeled
                        previousPeeled
                        nextPeeled
                  }

type RankPeeling :: Type -> Type
data RankPeeling a
  = NoPeeling
  | IsolatedRankPeeling !Int
  | ProfileRankPeeling !(EliminationWitness a)

rankOnlyPeeling ::
  (Field a, Num a, IntegralDomain a) =>
  (DenseMat a -> Either MoonlightError Int) ->
  Maybe (BlockedMat a) ->
  Maybe (BlockedMat a) ->
  DenseMat a ->
  Either MoonlightError (RankPeeling a)
rankOnlyPeeling rankDiagonal previousDifferential nextDifferential diagonalBlock =
  case (previousDifferential, nextDifferential) of
    (Nothing, Nothing) -> do
      rankValue <- rankDiagonal diagonalBlock
      pure
        ( if rankValue <= 0
            then NoPeeling
            else IsolatedRankPeeling rankValue
        )
    _ -> do
      witness <- trustedEliminationWitness diagonalBlock
      pure
        ( if ewRank witness <= 0
            then NoPeeling
            else ProfileRankPeeling witness
        )

indexPrefix :: Int -> Int -> [Int]
indexPrefix rankValue boundValue =
  [0 .. min rankValue boundValue - 1]

removeIsolatedDiagonalRank ::
  FinObjectId ->
  Int ->
  BlockedMat a ->
  BlockedMat a
removeIsolatedDiagonalRank nodeValue@(FinObjectId objectKey) rankValue blockedMat@BlockedMat {bmRows, bmCols, bmBlocks} =
  blockedMat
    { bmRows = removeAxisIndices nodeValue (indexPrefix rankValue (axisMultiplicity bmRows nodeValue)) bmRows
    , bmCols = removeAxisIndices nodeValue (indexPrefix rankValue (axisMultiplicity bmCols nodeValue)) bmCols
    , bmBlocks = IntMap.alter removeDiagonalRow objectKey bmBlocks
    }
  where
    removeDiagonalRow Nothing =
      Nothing
    removeDiagonalRow (Just rowMap) =
      let nextRow =
            IntMap.delete objectKey rowMap
       in if IntMap.null nextRow
            then Nothing
            else Just nextRow

minimizeStepWithSchur ::
  (Field a, Num a, IntegralDomain a) =>
  InjectiveComplex a ->
  Int ->
  FinObjectId ->
  BlockedMat a ->
  DenseMat a ->
  Either MoonlightError (PeelingStepResult a)
minimizeStepWithSchur injectiveComplex@InjectiveComplex {icDiffs} differentialIndex nodeValue currentDifferential diagonalBlock = do
  witness <- trustedEliminationWitness diagonalBlock
  case pivotMinorFromWitness witness of
    Nothing ->
      Right
        PeelingStepResult
          { psrComplex = injectiveComplex
          , psrAffected = []
          }
    Just pivotMinor -> do
      let previousDifferential =
            if differentialIndex <= 0
              then Nothing
              else icDiffs V.!? (differentialIndex - 1)
          nextDifferential =
            icDiffs V.!? (differentialIndex + 1)

      schurResult <-
        schurPeelCurrentDifferential
          nodeValue
          witness
          pivotMinor
          currentDifferential
      let currentPeeled =
            sprDifferential schurResult

      let previousPeeled =
            fmap
              (removeRowsOnLabel nodeValue (sortedPivotColumns pivotMinor))
              previousDifferential
          nextPeeled =
            fmap
              (removeColsOnLabel nodeValue (sortedPivotRows pivotMinor))
              nextDifferential

      peeledDiffs <-
        replacePeeledDifferentials
          differentialIndex
          icDiffs
          currentPeeled
          previousPeeled
          nextPeeled

      pure
        PeelingStepResult
          { psrComplex = injectiveComplex {icDiffs = peeledDiffs}
          , psrAffected =
              schurAffectedLocations
                differentialIndex
                (sprTouchedDiagonals schurResult)
                <> affectedLocations
                differentialIndex
                nodeValue
                currentPeeled
                previousPeeled
                nextPeeled
          }

replacePeeledDifferentials ::
  Int ->
  V.Vector (BlockedMat a) ->
  BlockedMat a ->
  Maybe (BlockedMat a) ->
  Maybe (BlockedMat a) ->
  Either MoonlightError (V.Vector (BlockedMat a))
replacePeeledDifferentials differentialIndex originalDiffs currentPeeled previousPeeled nextPeeled = do
  diffsWithPrevious <-
    case previousPeeled of
      Nothing ->
        Right originalDiffs
      Just previousValue ->
        replaceVector
          "minimizeComplex: previous differential replacement index is out of bounds"
          (differentialIndex - 1)
          previousValue
          originalDiffs

  diffsWithCurrent <-
    replaceVector
      "minimizeComplex: current differential replacement index is out of bounds"
      differentialIndex
      currentPeeled
      diffsWithPrevious

  case nextPeeled of
    Nothing ->
      Right diffsWithCurrent
    Just nextValue ->
      replaceVector
        "minimizeComplex: next differential replacement index is out of bounds"
        (differentialIndex + 1)
        nextValue
        diffsWithCurrent

hasSchurArmAt ::
  FinObjectId ->
  BlockedMat a ->
  Bool
hasSchurArmAt (FinObjectId objectKey) BlockedMat {bmBlocks} =
  IntMap.foldlWithKey'
    hasArmRow
    False
    bmBlocks
  where
    hasArmRow already rowKey rowMap =
      already
        || (rowKey /= objectKey && IntMap.member objectKey rowMap)
        || (rowKey == objectKey && IntMap.foldlWithKey' (\acc columnKey _ -> acc || columnKey /= objectKey) False rowMap)

sortedIndices :: V.Vector Int -> [Int]
sortedIndices =
  IntSet.toAscList . IntSet.fromList . V.toList

affectedLocations ::
  IntegralDomain a =>
  Int ->
  FinObjectId ->
  BlockedMat a ->
  Maybe (BlockedMat a) ->
  Maybe (BlockedMat a) ->
  [(Int, FinObjectId)]
affectedLocations differentialIndex nodeValue currentPeeled previousPeeled nextPeeled =
  currentLocations
    <> previousLocations
    <> nextLocations
  where
    currentLocations =
      [ (differentialIndex, nodeValue)
      | nonMinimalDiagonalAt nodeValue currentPeeled
      ]
    previousLocations =
      maybe
        []
        ( \previousValue ->
            [ (differentialIndex - 1, nodeValue)
            | nonMinimalDiagonalAt nodeValue previousValue
            ]
        )
        previousPeeled
    nextLocations =
      maybe
        []
        ( \nextValue ->
            [ (differentialIndex + 1, nodeValue)
            | nonMinimalDiagonalAt nodeValue nextValue
            ]
        )
        nextPeeled

schurAffectedLocations ::
  Int ->
  [FinObjectId] ->
  [(Int, FinObjectId)]
schurAffectedLocations differentialIndex =
  fmap (differentialIndex,)

schurPeelCurrentDifferential ::
  (Num a, IntegralDomain a) =>
  FinObjectId ->
  EliminationWitness a ->
  PivotMinor a ->
  BlockedMat a ->
  Either MoonlightError (SchurPeelResult a)
schurPeelCurrentDifferential nodeValue witness pivotMinor currentDifferential = do
  let baseDifferential =
        removeRowsOnLabel nodeValue (sortedPivotRows pivotMinor)
          (removeColsOnLabel nodeValue (sortedPivotColumns pivotMinor) currentDifferential)

  leftArms <-
    buildSchurLeftArms
      nodeValue
      pivotMinor
      currentDifferential
      baseDifferential

  rightArms <-
    buildSchurRightArms
      nodeValue
      pivotMinor
      currentDifferential
      baseDifferential

  solvedLeftArms <-
    traverse
      (solveSchurLeftArm witness)
      leftArms

  foldM
    (applySolvedSchurLeftArm rightArms)
    ( SchurPeelResult
        { sprDifferential = baseDifferential
        , sprTouchedDiagonals = []
        }
    )
    solvedLeftArms

buildSchurLeftArms ::
  IntegralDomain a =>
  FinObjectId ->
  PivotMinor a ->
  BlockedMat a ->
  BlockedMat a ->
  Either MoonlightError [SchurLeftArm a]
buildSchurLeftArms nodeValue pivotMinor originalDifferential baseDifferential =
  fmap reverse
    ( foldM
        collectArm
        []
        (V.toList (gaOrder (bmRows baseDifferential)))
    )
  where
    collectArm accumulated rowLabel = do
      let survivingRows =
            survivingIndices
              (bmRows originalDifferential)
              nodeValue
              (pmRows pivotMinor)
              rowLabel
          pivotColumns =
            V.toList (pmCols pivotMinor)

      case storedBlockAt rowLabel nodeValue originalDifferential of
        Nothing ->
          Right accumulated
        Just storedBlock
          | null survivingRows || null pivotColumns ->
              Right accumulated
          | otherwise -> do
              armMatrix <-
                selectDenseSubmatrix
                  "minimizeComplex: Schur left arm"
                  storedBlock
                  survivingRows
                  pivotColumns

              Right
                ( if denseIsZero armMatrix
                    then accumulated
                    else SchurLeftArm rowLabel armMatrix : accumulated
                )

buildSchurRightArms ::
  IntegralDomain a =>
  FinObjectId ->
  PivotMinor a ->
  BlockedMat a ->
  BlockedMat a ->
  Either MoonlightError [SchurRightArm a]
buildSchurRightArms nodeValue pivotMinor originalDifferential baseDifferential =
  fmap reverse
    ( foldM
        collectArm
        []
        (V.toList (gaOrder (bmCols baseDifferential)))
    )
  where
    collectArm accumulated columnLabel = do
      let pivotRows =
            V.toList (pmRows pivotMinor)
          survivingColumns =
            survivingIndices
              (bmCols originalDifferential)
              nodeValue
              (pmCols pivotMinor)
              columnLabel

      case storedBlockAt nodeValue columnLabel originalDifferential of
        Nothing ->
          Right accumulated
        Just storedBlock
          | null pivotRows || null survivingColumns ->
              Right accumulated
          | otherwise -> do
              armMatrix <-
                selectDenseSubmatrix
                  "minimizeComplex: Schur right arm"
                  storedBlock
                  pivotRows
                  survivingColumns

              Right
                ( if denseIsZero armMatrix
                    then accumulated
                    else SchurRightArm columnLabel armMatrix : accumulated
                )

solveSchurLeftArm ::
  (Num a, IntegralDomain a) =>
  EliminationWitness a ->
  SchurLeftArm a ->
  Either MoonlightError (SchurSolvedLeftArm a)
solveSchurLeftArm witness (SchurLeftArm rowLabel leftArm) =
  fmap
    (SchurSolvedLeftArm rowLabel)
    ( solveLeftWithEliminationWitness
        "minimizeComplex: Schur left arm times pivot inverse"
        witness
        leftArm
    )

applySolvedSchurLeftArm ::
  (Num a, IntegralDomain a) =>
  [SchurRightArm a] ->
  SchurPeelResult a ->
  SchurSolvedLeftArm a ->
  Either MoonlightError (SchurPeelResult a)
applySolvedSchurLeftArm rightArms schurResult solvedLeftArm =
  foldM
    (applySchurCorrection solvedLeftArm)
    schurResult
    rightArms

applySchurCorrection ::
  (Num a, IntegralDomain a) =>
  SchurSolvedLeftArm a ->
  SchurPeelResult a ->
  SchurRightArm a ->
  Either MoonlightError (SchurPeelResult a)
applySchurCorrection (SchurSolvedLeftArm rowLabel leftTimesInverse) SchurPeelResult {sprDifferential, sprTouchedDiagonals} (SchurRightArm columnLabel rightArm) = do
  correction <-
    denseMul
      "minimizeComplex: Schur correction"
      leftTimesInverse
      rightArm

  if denseIsZero correction
    then
      Right
        SchurPeelResult
          { sprDifferential = sprDifferential
          , sprTouchedDiagonals = sprTouchedDiagonals
          }
    else do
      updatedBlock <-
        denseSub
          "minimizeComplex: Schur block subtraction"
          (blockAt rowLabel columnLabel sprDifferential)
          correction

      Right
        SchurPeelResult
          { sprDifferential =
              setBlockedBlock rowLabel columnLabel updatedBlock sprDifferential
          , sprTouchedDiagonals =
              if rowLabel == columnLabel && not (denseIsZero updatedBlock)
                then rowLabel : sprTouchedDiagonals
                else sprTouchedDiagonals
          }
