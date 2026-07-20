{-# LANGUAGE DerivingStrategies #-}

-- |
-- The degree-one calculus of the triangulated layer: chain maps between
-- sealed complexes, the translation functor, and the brutal truncations.
--
-- Carrier contract: a value of type 'DerivedMap' witnesses that every stored
-- component's axes coincide with the source and target object axes at its
-- degree, and that every square commutes — @d_B ∘ f_n = f_{n+1} ∘ d_A@ —
-- where components and differentials outside their windows are the zero
-- maps. Source and target need not share a degree window. The constructor
-- is unexported; 'mkDerivedMapChecked' is the sole gate.
--
-- Soundness of the square check: the axis check runs first, so both
-- composites at every degree are formed against identical 'GroupedAxis'
-- values on all boundaries; dense expansion follows the axis order, hence
-- equality of the expanded composites is exactly equality of the blocked
-- composites.
--
-- 'shift' is total on the trusted path: negation fixes zero, so the stored
-- sparse block structure, minimality of diagonal blocks, axis compatibility,
-- and @(−d)∘(−d) = d∘d = 0@ are all preserved; reindexing @icStart@ touches
-- no matrix. The brutal truncations are total on the trusted path for the
-- same reason: a contiguous subvector of a minimal complex's differentials
-- is minimal, axis-compatible, and still composes to zero.
module Moonlight.Derived.Pure.Site.DerivedMap
  ( DerivedMap
  , mkDerivedMap
  , mkDerivedMapChecked
  , derivedMapSource
  , derivedMapTarget
  , derivedMapComponents
  , derivedMapComponentAt
  , derivedObjectAxes
  , derivedObjectWindow
  , axisAtDegree
  , diffDenseAtDegree
  , identityMap
  , zeroMap
  , zeroDerived
  , shift
  , stupidTruncateBelow
  , stupidTruncateAbove
  , negateBlocked
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Kind (Type)
import qualified Data.Vector as V
import Moonlight.Core (MoonlightError (..))
import Moonlight.Derived.Pure.Failure
  ( DerivedFailure (..)
  , derivedFailureToMoonlightError
  )
import Moonlight.Derived.Pure.Site.InjectiveComplex
import Moonlight.Derived.Pure.Site.LabeledMatrix
import Moonlight.Derived.Pure.Site.Poset (DerivedPoset)

type DerivedMap :: Type -> Type
data DerivedMap a = DerivedMap
  { dmapSource :: !(Derived a)
  , dmapTarget :: !(Derived a)
  , dmapComponents :: !(IntMap (BlockedMat a))
  } deriving stock (Eq, Show)

derivedMapSource :: DerivedMap a -> Derived a
derivedMapSource =
  dmapSource

derivedMapTarget :: DerivedMap a -> Derived a
derivedMapTarget =
  dmapTarget

derivedMapComponents :: DerivedMap a -> IntMap (BlockedMat a)
derivedMapComponents =
  dmapComponents

derivedMapComponentAt :: DerivedMap a -> Int -> BlockedMat a
derivedMapComponentAt DerivedMap {dmapSource, dmapTarget, dmapComponents} degreeValue =
  IM.findWithDefault
    (zeroBlocked (axisAtDegree dmapTarget degreeValue) (axisAtDegree dmapSource degreeValue))
    degreeValue
    dmapComponents

derivedObjectAxes :: Derived a -> [GroupedAxis]
derivedObjectAxes =
  complexObjectAxes . derivedInjectiveComplex

derivedObjectWindow :: Derived a -> (Int, Int)
derivedObjectWindow derivedValue =
  ( injectiveComplexStart complexValue
  , injectiveComplexStart complexValue + V.length (injectiveComplexDiffs complexValue)
  )
  where
    complexValue = derivedInjectiveComplex derivedValue

axisAtDegree :: Derived a -> Int -> GroupedAxis
axisAtDegree derivedValue degreeValue =
  case drop (degreeValue - fst (derivedObjectWindow derivedValue)) (derivedObjectAxes derivedValue) of
    axisValue : _ | degreeValue >= fst (derivedObjectWindow derivedValue) -> axisValue
    _ -> emptyAxis

diffDenseAtDegree :: Num a => Derived a -> Int -> DenseMat a
diffDenseAtDegree derivedValue degreeValue
  | degreeValue >= lowDegree && degreeValue < highDegree =
      collapseBlockedDense (injectiveComplexDiffs complexValue V.! (degreeValue - lowDegree))
  | otherwise =
      zeroMat
        (axisSize (axisAtDegree derivedValue (degreeValue + 1)))
        (axisSize (axisAtDegree derivedValue degreeValue))
  where
    complexValue = derivedInjectiveComplex derivedValue
    (lowDegree, highDegree) = derivedObjectWindow derivedValue

mkDerivedMap :: (Eq a, Num a) => Derived a -> Derived a -> IntMap (BlockedMat a) -> Either MoonlightError (DerivedMap a)
mkDerivedMap sourceValue targetValue componentValues =
  either (Left . derivedFailureToMoonlightError) Right (mkDerivedMapChecked sourceValue targetValue componentValues)

mkDerivedMapChecked :: (Eq a, Num a) => Derived a -> Derived a -> IntMap (BlockedMat a) -> Either DerivedFailure (DerivedMap a)
mkDerivedMapChecked sourceValue targetValue componentValues =
  if derivedPoset sourceValue /= derivedPoset targetValue
    then Left DerivedFunctorSiteMismatch
    else
      case IM.foldrWithKey checkComponent Nothing componentValues of
        Just failureValue -> Left failureValue
        Nothing ->
          case firstNonCommutingSquare of
            Just badDegree -> Left (DerivedMapSquareNotCommuting badDegree)
            Nothing -> Right (DerivedMap sourceValue targetValue componentValues)
  where
    (sourceLow, sourceHigh) = derivedObjectWindow sourceValue
    (targetLow, targetHigh) = derivedObjectWindow targetValue

    checkComponent degreeValue componentValue acc =
      case acc of
        Just _ -> acc
        Nothing
          | degreeValue < max sourceLow targetLow || degreeValue > min sourceHigh targetHigh ->
              Just (DerivedMapDegreeMismatch ("component at degree " <> show degreeValue <> " lies outside the shared object window"))
          | bmCols componentValue /= axisAtDegree sourceValue degreeValue
              || bmRows componentValue /= axisAtDegree targetValue degreeValue ->
              Just (DerivedMapAxisMismatch degreeValue)
          | otherwise ->
              Nothing

    componentDense degreeValue =
      case IM.lookup degreeValue componentValues of
        Just componentValue -> collapseBlockedDense componentValue
        Nothing ->
          zeroMat
            (axisSize (axisAtDegree targetValue degreeValue))
            (axisSize (axisAtDegree sourceValue degreeValue))

    firstNonCommutingSquare =
      lookup
        False
        [ ( matMul (diffDenseAtDegree targetValue degreeValue) (componentDense degreeValue)
              == matMul (componentDense (degreeValue + 1)) (diffDenseAtDegree sourceValue degreeValue)
          , degreeValue
          )
        | degreeValue <- [min sourceLow targetLow - 1 .. max sourceHigh targetHigh]
        ]

identityMap :: Num a => Derived a -> DerivedMap a
identityMap complexValue =
  DerivedMap complexValue complexValue identityComponents
  where
    identityComponents =
      IM.fromList
        (zip [fst (derivedObjectWindow complexValue) ..] (fmap identityBlocked (derivedObjectAxes complexValue)))

identityBlocked :: Num a => GroupedAxis -> BlockedMat a
identityBlocked axisValue =
  BlockedMat axisValue axisValue identityBlocks
  where
    identityBlocks =
      IM.foldrWithKey
        ( \nodeKey multiplicityValue acc ->
            if multiplicityValue > 0
              then IM.insert nodeKey (IM.singleton nodeKey (identMat multiplicityValue)) acc
              else acc
        )
        IM.empty
        (groupedAxisMultiplicities axisValue)

zeroMap :: Derived a -> Derived a -> DerivedMap a
zeroMap sourceValue targetValue =
  DerivedMap sourceValue targetValue IM.empty

zeroDerived :: DerivedPoset -> Derived a
zeroDerived posetValue =
  mkDerivedTrusted posetValue
    (trustLawfulInjectiveComplex (InjectiveComplex 0 (V.singleton (zeroBlocked emptyAxis emptyAxis))))

shift :: Num a => Int -> Derived a -> Derived a
shift shiftAmount derivedValue =
  mkDerivedTrusted (derivedPoset derivedValue) (trustLawfulInjectiveComplex shiftedComplex)
  where
    complexValue = derivedInjectiveComplex derivedValue
    shiftedComplex =
      InjectiveComplex
        { icStart = injectiveComplexStart complexValue - shiftAmount
        , icDiffs = shiftedDiffs
        }
    shiftedDiffs
      | even shiftAmount = injectiveComplexDiffs complexValue
      | otherwise = fmap negateBlocked (injectiveComplexDiffs complexValue)

stupidTruncateBelow :: Int -> Derived a -> Derived a
stupidTruncateBelow cutoffDegree derivedValue
  | cutoffDegree >= highDegree = derivedValue
  | cutoffDegree < lowDegree = zeroDerived siteValue
  | cutoffDegree == lowDegree =
      maybe
        (zeroDerived siteValue)
        (\(firstDifferential, _) -> trusted siteValue lowDegree (V.singleton (zeroBlocked emptyAxis (bmCols firstDifferential))))
        (V.uncons diffs)
  | otherwise =
      trusted siteValue lowDegree (V.slice 0 (cutoffDegree - lowDegree) diffs)
  where
    siteValue = derivedPoset derivedValue
    (lowDegree, highDegree) = derivedObjectWindow derivedValue
    diffs = injectiveComplexDiffs (derivedInjectiveComplex derivedValue)

stupidTruncateAbove :: Int -> Derived a -> Derived a
stupidTruncateAbove cutoffDegree derivedValue
  | cutoffDegree <= lowDegree = derivedValue
  | cutoffDegree > highDegree = zeroDerived siteValue
  | cutoffDegree == highDegree =
      maybe
        (zeroDerived siteValue)
        (\(_, lastDifferential) -> trusted siteValue highDegree (V.singleton (zeroBlocked emptyAxis (bmRows lastDifferential))))
        (V.unsnoc diffs)
  | otherwise =
      trusted siteValue cutoffDegree (V.slice (cutoffDegree - lowDegree) (highDegree - cutoffDegree) diffs)
  where
    siteValue = derivedPoset derivedValue
    (lowDegree, highDegree) = derivedObjectWindow derivedValue
    diffs = injectiveComplexDiffs (derivedInjectiveComplex derivedValue)

trusted :: DerivedPoset -> Int -> V.Vector (BlockedMat a) -> Derived a
trusted posetValue startDegree diffs =
  mkDerivedTrusted posetValue (trustLawfulInjectiveComplex (InjectiveComplex startDegree diffs))

negateBlocked :: Num a => BlockedMat a -> BlockedMat a
negateBlocked blockedValue =
  blockedValue { bmBlocks = fmap (fmap negateDense) (bmBlocks blockedValue) }

negateDense :: Num a => DenseMat a -> DenseMat a
negateDense denseValue =
  denseValue { dmData = fmap (fmap negate) (dmData denseValue) }
