{-# LANGUAGE NamedFieldPuns #-}

module Moonlight.Derived.Pure.Gluing.Resolution
  ( completeDifferential
  , resolutionStep
  , resolveLoop
  ) where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Vector qualified as V
import Moonlight.Algebra (IntegralDomain)
import Moonlight.Core (Field, MoonlightError)
import Moonlight.Core (scanFoldM, unfoldM)
import Moonlight.Derived.Pure.Gluing.MakeExact
  ( PreparedExactness
  , makeExactPreparedAtStar
  , makeExactPreparedFreshAtStar
  , prepareExactness
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix
  ( BlockedMat
  , GroupedAxis
  , axisEmpty
  , bmRows
  , emptyAxis
  , zeroBlocked
  )
import Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset (..)
  , FinObjectId
  , star
  )

data ResolutionExactness a = ResolutionExactness
  { rePreparedExactness :: !(PreparedExactness a)
  , rePreviousStar :: !(Maybe IntSet)
  }

completeDifferential ::
  (Field a, IntegralDomain a, Num a) =>
  DerivedPoset ->
  [FinObjectId] ->
  BlockedMat a ->
  BlockedMat a ->
  Either MoonlightError (BlockedMat a)
completeDifferential
  posetValue
  nodeOrder
  previousDifferential
  initialDifferential = do
    initialExactness <-
      prepareExactness previousDifferential initialDifferential
    fst
      <$> scanFoldM
        completeNode
        (initialDifferential, emptyResolutionExactness initialExactness)
        nodeOrder
  where
    completeNode (_, exactnessState) nodeValue =
      let currentStar =
            star posetValue nodeValue
       in do
            (nextDifferential, nextExactnessState) <-
              completeNodeAtStar
                nodeValue
                currentStar
                exactnessState
            Right (nextDifferential, nextExactnessState)

emptyResolutionExactness :: PreparedExactness a -> ResolutionExactness a
emptyResolutionExactness preparedExactness =
  ResolutionExactness
    { rePreparedExactness = preparedExactness
    , rePreviousStar = Nothing
    }

completeNodeAtStar ::
  (Field a, IntegralDomain a, Num a) =>
  FinObjectId ->
  IntSet ->
  ResolutionExactness a ->
  Either MoonlightError (BlockedMat a, ResolutionExactness a)
completeNodeAtStar
  nodeValue
  currentStar
  ResolutionExactness {rePreparedExactness, rePreviousStar}
    | shouldThreadStar rePreviousStar currentStar = do
        (nextDifferential, nextPreparedExactness) <-
          makeExactPreparedAtStar
            currentStar
            nodeValue
            rePreparedExactness
        Right
          ( nextDifferential
          , ResolutionExactness
              { rePreparedExactness = nextPreparedExactness
              , rePreviousStar = Just currentStar
              }
          )
    | otherwise = do
        (nextDifferential, nextPreparedExactness) <-
          makeExactPreparedFreshAtStar
            currentStar
            nodeValue
            rePreparedExactness
        Right
          ( nextDifferential
          , ResolutionExactness
              { rePreparedExactness = nextPreparedExactness
              , rePreviousStar = Just currentStar
              }
          )

shouldThreadStar :: Maybe IntSet -> IntSet -> Bool
shouldThreadStar Nothing _ =
  False
shouldThreadStar (Just previousStar) currentStar =
  nestedStar previousStar currentStar
    || denseStarOverlap previousStar currentStar

nestedStar :: IntSet -> IntSet -> Bool
nestedStar previousStar currentStar =
  not (IntSet.null previousStar)
    && not (IntSet.null currentStar)
    && ( IntSet.isSubsetOf previousStar currentStar
          || IntSet.isSubsetOf currentStar previousStar
       )

denseStarOverlap :: IntSet -> IntSet -> Bool
denseStarOverlap previousStar currentStar =
  overlapSize > 0
    && overlapSize * 2 >= min (IntSet.size previousStar) (IntSet.size currentStar)
  where
    overlapSize =
      IntSet.size (IntSet.intersection previousStar currentStar)

resolutionStep ::
  (Field a, IntegralDomain a, Num a) =>
  DerivedPoset ->
  BlockedMat a ->
  Either MoonlightError (BlockedMat a)
resolutionStep posetValue previousDifferential =
  completeDifferential
    posetValue
    (V.toList (derivedPosetTopoDesc posetValue))
    previousDifferential
    (zeroBlocked emptyAxis (bmRows previousDifferential))

resolveLoop ::
  (Field a, IntegralDomain a, Num a) =>
  DerivedPoset ->
  [FinObjectId] ->
  (GroupedAxis -> Maybe (BlockedMat a) -> Either MoonlightError (BlockedMat a)) ->
  (BlockedMat a -> BlockedMat a) ->
  GroupedAxis ->
  V.Vector (BlockedMat a) ->
  Either MoonlightError (V.Vector (BlockedMat a))
resolveLoop
  posetValue
  nodeOrder
  seed
  postFilter
  initialAxis
  inputDifferentials =
    V.fromList
      <$> unfoldM
        step
        (0, zeroBlocked initialAxis emptyAxis)
  where
    step (indexValue, previousDifferential)
      | axisEmpty (bmRows previousDifferential)
          && indexValue >= V.length inputDifferentials =
          Right Nothing

      | otherwise = do
          seededDifferential <-
            seed
              (bmRows previousDifferential)
              (inputDifferentials V.!? indexValue)

          currentDifferential <-
            completeDifferential
              posetValue
              nodeOrder
              previousDifferential
              seededDifferential

          let filteredDifferential =
                postFilter currentDifferential

          Right
            ( Just
                ( filteredDifferential
                , (indexValue + 1, currentDifferential)
                )
            )
