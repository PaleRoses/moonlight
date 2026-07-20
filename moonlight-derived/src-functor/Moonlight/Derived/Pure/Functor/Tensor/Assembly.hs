{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Derived.Pure.Functor.Tensor.Assembly
  ( tensorBlockedDifferentials
  , tensorReducedBlockedDifferentials
  , tensorPresentationFromBlocks
  , blockedBlockCount
  , blockedBlockCellCount
  , blockedBlockNonZeroCount
  ) where

import Control.Monad (foldM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as V
import Moonlight.Core (Field, MoonlightError (..))
import Moonlight.Derived.Pure.Functor.Tensor.SupportPresentation
  ( TensorSupportPresentation (..)
  )
import Moonlight.Derived.Pure.Functor.Tensor.BlockAssembly
  ( BlockAssembly
  , addScaledSparseMatAt
  , blockAssemblyDiagonalNodes
  , blockedBlockCellCount
  , blockedBlockCount
  , blockedBlockNonZeroCount
  , contractIsolatedDiagonalAssembly
  , emptyBlockAssembly
  , finishBlockAssembly
  , layoutBlockIndex
  )
import Moonlight.Derived.Pure.Functor.Tensor.Layout
  ( DegreeLayout
  , ExpandedComplex
  , PairInstance (..)
  , PairKey
  , RestrictionCache
  , SummandInstance (..)
  , SummandKey (..)
  , TensorLayoutInput (..)
  , axisAtDegree
  , incrementSupportDegree
  , labelsAtDegree
  , lookupAxisNode
  , lookupRestrictionSparse
  , lookupSummandOffset
  , lookupSupportPresentation
  , lookupVector
  , nonZeroColumnEntriesAt
  , summandKey
  )
import Moonlight.Derived.Pure.Site.InjectiveComplex
  ( InjectiveComplex (..)
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix
  ( BlockedMat
  , fromLabels
  , zeroBlocked
  )
import Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset
  , FinObjectId
  , leq
  )

tensorBlockedDifferentials ::
  (Eq a, Num a) =>
  TensorLayoutInput a ->
  DerivedPoset ->
  InjectiveComplex a ->
  InjectiveComplex a ->
  Either MoonlightError (Vector (BlockedMat a), RestrictionCache a, [(Int, FinObjectId)])
tensorBlockedDifferentials layoutInput posetValue leftComplex rightComplex =
  case tensorEmptyDifferentials layoutInput of
    Just emptyDifferentials ->
      Right (emptyDifferentials, Map.empty, [])
    Nothing -> do
      let layouts = tliLayouts layoutInput
          differentialCount = V.length layouts - 1
      (blockedDiffsReversed, restrictionCache, minimizationFrontier) <-
        foldM
          ( \(reversedDiffs, restrictionCache, accumulatedFrontier) degreeValue -> do
              (blockedDiff, diagonalNodes, restrictionCache') <-
                tensorDifferentialAt
                  posetValue
                  (icStart leftComplex)
                  (icStart rightComplex)
                  (tliSupportCache layoutInput)
                  (tliLeftExpanded layoutInput)
                  (tliRightExpanded layoutInput)
                  layouts
                  (tliSummandsByDegree layoutInput)
                  restrictionCache
                  degreeValue
              pure
                ( blockedDiff : reversedDiffs
                , restrictionCache'
                , fmap (degreeValue,) diagonalNodes <> accumulatedFrontier
                )
          )
          ([], Map.empty, [])
          [0 .. differentialCount - 1]
      pure (V.fromList (reverse blockedDiffsReversed), restrictionCache, reverse minimizationFrontier)

tensorReducedBlockedDifferentials ::
  forall a.
  (Eq a, Field a, Num a) =>
  TensorLayoutInput a ->
  DerivedPoset ->
  InjectiveComplex a ->
  InjectiveComplex a ->
  Either MoonlightError (Vector (BlockedMat a), RestrictionCache a, [(Int, FinObjectId)])
tensorReducedBlockedDifferentials layoutInput posetValue leftComplex rightComplex =
  case tensorEmptyDifferentials layoutInput of
    Just emptyDifferentials ->
      Right (emptyDifferentials, Map.empty, [])
    Nothing ->
      case V.length (tliLayouts layoutInput) of
        2 ->
          tensorSingleReducedDifferential layoutInput posetValue leftComplex rightComplex
        _ ->
          tensorBlockedDifferentials layoutInput posetValue leftComplex rightComplex

tensorSingleReducedDifferential ::
  forall a.
  (Eq a, Field a, Num a) =>
  TensorLayoutInput a ->
  DerivedPoset ->
  InjectiveComplex a ->
  InjectiveComplex a ->
  Either MoonlightError (Vector (BlockedMat a), RestrictionCache a, [(Int, FinObjectId)])
tensorSingleReducedDifferential layoutInput posetValue leftComplex rightComplex = do
  (assembly, restrictionCache) <-
    tensorDifferentialAssemblyAt
      posetValue
      (icStart leftComplex)
      (icStart rightComplex)
      (tliSupportCache layoutInput)
      (tliLeftExpanded layoutInput)
      (tliRightExpanded layoutInput)
      (tliLayouts layoutInput)
      (tliSummandsByDegree layoutInput)
      Map.empty
      0
  maybeContracted <- contractIsolatedDiagonalAssembly assembly
  case maybeContracted of
    Just contractedDifferential ->
      Right (V.singleton contractedDifferential, restrictionCache, [])
    Nothing ->
      let blockedDiff =
            finishBlockAssembly assembly
       in Right
            ( V.singleton blockedDiff
            , restrictionCache
            , fmap (0,) (blockAssemblyDiagonalNodes assembly)
            )

tensorEmptyDifferentials ::
  TensorLayoutInput a ->
  Maybe (Vector (BlockedMat a))
tensorEmptyDifferentials layoutInput =
  let layouts = tliLayouts layoutInput
   in if V.length layouts <= 1
        then
          Just
            ( V.singleton
                ( zeroBlocked
                    (fromLabels V.empty)
                    (fromLabels (labelsAtDegree 0 layouts))
                )
            )
        else Nothing

tensorPresentationFromBlocks ::
  TensorLayoutInput a ->
  Vector (BlockedMat a) ->
  InjectiveComplex a
tensorPresentationFromBlocks layoutInput blockedDiffs =
  InjectiveComplex
    { icStart = tliStartDegree layoutInput
    , icDiffs = blockedDiffs
    }


tensorDifferentialAt ::
  (Eq a, Num a) =>
  DerivedPoset ->
  Int ->
  Int ->
  Map PairKey (TensorSupportPresentation a) ->
  ExpandedComplex a ->
  ExpandedComplex a ->
  Vector DegreeLayout ->
  Vector [SummandInstance a] ->
  RestrictionCache a ->
  Int ->
  Either MoonlightError (BlockedMat a, [FinObjectId], RestrictionCache a)
tensorDifferentialAt posetValue leftStart rightStart supportCache leftExpanded rightExpanded layouts summandsByDegree restrictionCache degreeValue = do
  (assembledBlocks, restrictionCache') <-
    tensorDifferentialAssemblyAt
      posetValue
      leftStart
      rightStart
      supportCache
      leftExpanded
      rightExpanded
      layouts
      summandsByDegree
      restrictionCache
      degreeValue
  pure
    ( finishBlockAssembly assembledBlocks
    , blockAssemblyDiagonalNodes assembledBlocks
    , restrictionCache'
    )

tensorDifferentialAssemblyAt ::
  (Eq a, Num a) =>
  DerivedPoset ->
  Int ->
  Int ->
  Map PairKey (TensorSupportPresentation a) ->
  ExpandedComplex a ->
  ExpandedComplex a ->
  Vector DegreeLayout ->
  Vector [SummandInstance a] ->
  RestrictionCache a ->
  Int ->
  Either MoonlightError (BlockAssembly a, RestrictionCache a)
tensorDifferentialAssemblyAt posetValue leftStart rightStart supportCache leftExpanded rightExpanded layouts summandsByDegree restrictionCache degreeValue = do
  sourceLayout <- lookupVector "tensorProduct: missing source layout" degreeValue layouts
  targetLayout <- lookupVector "tensorProduct: missing target layout" (degreeValue + 1) layouts
  sourceSummands <- lookupVector "tensorProduct: missing source summands" degreeValue summandsByDegree
  let initialAssembly =
        emptyBlockAssembly
          (layoutBlockIndex targetLayout)
          (layoutBlockIndex sourceLayout)
  (assembledBlocks, restrictionCache') <-
    foldM
      (addSummandDifferential posetValue leftStart rightStart supportCache leftExpanded rightExpanded sourceLayout targetLayout)
      (initialAssembly, restrictionCache)
      sourceSummands
  pure (assembledBlocks, restrictionCache')

addSummandDifferential ::
  (Eq a, Num a) =>
  DerivedPoset ->
  Int ->
  Int ->
  Map PairKey (TensorSupportPresentation a) ->
  ExpandedComplex a ->
  ExpandedComplex a ->
  DegreeLayout ->
  DegreeLayout ->
  (BlockAssembly a, RestrictionCache a) ->
  SummandInstance a ->
  Either MoonlightError (BlockAssembly a, RestrictionCache a)
addSummandDifferential posetValue leftStart rightStart supportCache leftExpanded rightExpanded sourceLayout targetLayout (accumulatedDense, restrictionCache) summandInstance@SummandInstance {siPair = pairInstance@PairInstance {piLeftDegree, piRightDegree, piSupport}, siSupportDegree} = do
  sourceOffset <- lookupSummandOffset "tensorProduct: missing source offset" sourceLayout (summandKey summandInstance)
  withInternal <-
    addInternalDifferential
      leftStart
      rightStart
      targetLayout
      sourceOffset
      piLeftDegree
      piRightDegree
      piSupport
      siSupportDegree
      accumulatedDense
      summandInstance
  (withLeft, restrictionCache') <-
    addLeftDifferentials
      posetValue
      supportCache
      leftExpanded
      targetLayout
      sourceOffset
      pairInstance
      siSupportDegree
      restrictionCache
      withInternal
  addRightDifferentials
    posetValue
    leftStart
    supportCache
    rightExpanded
    targetLayout
    sourceOffset
    pairInstance
    siSupportDegree
    restrictionCache'
    withLeft

addInternalDifferential ::
  (Eq a, Num a) =>
  Int ->
  Int ->
  DegreeLayout ->
  Int ->
  Int ->
  Int ->
  TensorSupportPresentation a ->
  Int ->
  BlockAssembly a ->
  SummandInstance a ->
  Either MoonlightError (BlockAssembly a)
addInternalDifferential leftStart rightStart targetLayout sourceOffset leftDegreeValue rightDegreeValue supportPresentation supportDegreeValue accumulatedDense summandInstance =
  case tspDiffs supportPresentation V.!? supportDegreeValue of
    Nothing -> Right accumulatedDense
    Just supportDifferential ->
      let targetAxis = axisAtDegree (supportDegreeValue + 1) supportPresentation
          signCoefficient =
            if odd (leftStart + leftDegreeValue + rightStart + rightDegreeValue)
              then negate 1
              else 1
       in if V.null targetAxis
            then Right accumulatedDense
            else do
              targetOffset <-
                lookupSummandOffset
                  "tensorProduct: missing internal target offset"
                  targetLayout
                  (incrementSupportDegree summandInstance)
              addScaledSparseMatAt
                "tensorProduct: internal support differential"
                targetOffset
                sourceOffset
                signCoefficient
                supportDifferential
                accumulatedDense

addLeftDifferentials ::
  (Eq a, Num a) =>
  DerivedPoset ->
  Map PairKey (TensorSupportPresentation a) ->
  ExpandedComplex a ->
  DegreeLayout ->
  Int ->
  PairInstance a ->
  Int ->
  RestrictionCache a ->
  BlockAssembly a ->
  Either MoonlightError (BlockAssembly a, RestrictionCache a)
addLeftDifferentials posetValue supportCache leftExpanded targetLayout sourceOffset PairInstance {piLeftDegree, piLeftBasis, piLeftNode, piRightDegree, piRightBasis, piRightNode, piSupportKey, piSupport} supportDegreeValue restrictionCache accumulatedDense =
  foldM
    ( \(nextDense, nextRestrictionCache) (targetLeftBasis, coefficientValue) -> do
        targetLeftNode <- lookupAxisNode leftExpanded (piLeftDegree + 1) targetLeftBasis
        validateTensorMorphism
          "tensorProduct: left differential"
          posetValue
          targetLeftNode
          piLeftNode
        let targetKey =
              SummandKey
                { skLeftDegree = piLeftDegree + 1
                , skLeftBasis = targetLeftBasis
                , skRightDegree = piRightDegree
                , skRightBasis = piRightBasis
                , skSupportDegree = supportDegreeValue
                }
            targetSupportKey = (targetLeftNode, piRightNode)
        targetSupport <- lookupSupportPresentation supportCache targetLeftNode piRightNode
        if V.null (axisAtDegree supportDegreeValue targetSupport)
          then Right (nextDense, nextRestrictionCache)
          else do
            targetOffset <-
              lookupSummandOffset
                "tensorProduct: missing left target offset"
                targetLayout
                targetKey
            (localSparse, nextRestrictionCache') <-
              lookupRestrictionSparse
                piSupportKey
                piSupport
                targetSupportKey
                targetSupport
                supportDegreeValue
                nextRestrictionCache
            updatedDense <-
              addScaledSparseMatAt
                "tensorProduct: left differential contribution"
                targetOffset
                sourceOffset
                coefficientValue
                localSparse
                nextDense
            pure (updatedDense, nextRestrictionCache')
    )
    (accumulatedDense, restrictionCache)
    (nonZeroColumnEntriesAt piLeftBasis leftExpanded piLeftDegree)

addRightDifferentials ::
  (Eq a, Num a) =>
  DerivedPoset ->
  Int ->
  Map PairKey (TensorSupportPresentation a) ->
  ExpandedComplex a ->
  DegreeLayout ->
  Int ->
  PairInstance a ->
  Int ->
  RestrictionCache a ->
  BlockAssembly a ->
  Either MoonlightError (BlockAssembly a, RestrictionCache a)
addRightDifferentials posetValue leftStart supportCache rightExpanded targetLayout sourceOffset PairInstance {piLeftDegree, piLeftBasis, piLeftNode, piRightDegree, piRightBasis, piRightNode, piSupportKey, piSupport} supportDegreeValue restrictionCache accumulatedDense =
  foldM
    ( \(nextDense, nextRestrictionCache) (targetRightBasis, coefficientValue0) -> do
        targetRightNode <- lookupAxisNode rightExpanded (piRightDegree + 1) targetRightBasis
        validateTensorMorphism
          "tensorProduct: right differential"
          posetValue
          targetRightNode
          piRightNode
        let signedCoefficient =
              if odd (leftStart + piLeftDegree)
                then negate coefficientValue0
                else coefficientValue0
            targetKey =
              SummandKey
                { skLeftDegree = piLeftDegree
                , skLeftBasis = piLeftBasis
                , skRightDegree = piRightDegree + 1
                , skRightBasis = targetRightBasis
                , skSupportDegree = supportDegreeValue
                }
            targetSupportKey = (piLeftNode, targetRightNode)
        targetSupport <- lookupSupportPresentation supportCache piLeftNode targetRightNode
        if V.null (axisAtDegree supportDegreeValue targetSupport)
          then Right (nextDense, nextRestrictionCache)
          else do
            targetOffset <-
              lookupSummandOffset
                "tensorProduct: missing right target offset"
                targetLayout
                targetKey
            (localSparse, nextRestrictionCache') <-
              lookupRestrictionSparse
                piSupportKey
                piSupport
                targetSupportKey
                targetSupport
                supportDegreeValue
                nextRestrictionCache
            updatedDense <-
              addScaledSparseMatAt
                "tensorProduct: right differential contribution"
                targetOffset
                sourceOffset
                signedCoefficient
                localSparse
                nextDense
            pure (updatedDense, nextRestrictionCache')
    )
    (accumulatedDense, restrictionCache)
    (nonZeroColumnEntriesAt piRightBasis rightExpanded piRightDegree)

validateTensorMorphism ::
  String ->
  DerivedPoset ->
  FinObjectId ->
  FinObjectId ->
  Either MoonlightError ()
validateTensorMorphism context posetValue targetNode sourceNode =
  if leq posetValue targetNode sourceNode
    then Right ()
    else
      Left
        ( InvariantViolation
            ( context
                <> ": encountered a non-sheaf morphism from "
                <> show sourceNode
                <> " to "
                <> show targetNode
            )
        )
