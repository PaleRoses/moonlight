-- |
-- Canonical truncation, constructed entirely from production kernel pieces.
--
-- @τ≤n A@ keeps the differentials of @A@ through degree @n@ — so the final
-- kept map still lands in the injective @A^{n+1}@ — and then kills all
-- higher cohomology with an exact injective tail built by iterating
-- 'resolutionStep': the first tail map @e@ satisfies @ker e = im d^n@, which
-- zeroes @H^{n+1}@, and every later joint is exact by the same construction.
-- Cohomology at and below @n@ is untouched because no object or map at
-- those degrees changes. The shared skeleton crosses the composability gate
-- once; minimization transports that proof, and the final seal checks only
-- site membership, order, and minimality.
--
-- @τ≥n A@ is the cone of the canonical map @ι : τ≤n−1 A → A@: identity
-- components on the shared objects through degree @n@, and on the tail the
-- extensions @φ_{j+1} e_{j+1} = d_A φ_j@ guaranteed by injectivity of the
-- @A@ objects and computed by the Smith-witnessed left-solve of the
-- hardened linear-algebra grade. The long exact
-- sequence of the triangle then gives @H^i(cone ι) = H^i(A)@ for @i ≥ n@
-- and @0@ below. A failed solve is mathematically impossible; it is
-- surfaced loudly rather than repaired.
--
-- Both truncations grow from the same skeleton: the kept differentials
-- through the cutoff and the exact tail completed from the last kept map.
-- 'canonicalTruncationPair' computes that skeleton once and returns the
-- canonical decomposition @(τ≤n A, τ≥n+1 A)@ — the two halves of the
-- truncation triangle @τ≤n A → A → τ≥n+1 A → τ≤n A[1]@ — so consumers of
-- the decomposition never pay for the tail twice.
--
-- == Precondition: sheaf-lawfulness
--
-- The exactness completion works star-locally over the site. The completed
-- skeleton is therefore sealed as one composable complex before either
-- truncation consumes it. This is the single critical structural validation;
-- neither minimization nor the two result assemblies repeat it.
module Moonlight.Derived.Pure.Gluing.Truncation
  ( canonicalTruncateAtMost
  , canonicalTruncateAtLeast
  , canonicalTruncationPair
  ) where

import qualified Data.IntMap.Strict as IM
import qualified Data.Vector as V
import Moonlight.Algebra (EuclideanDomain, IntegralDomain)
import Moonlight.Core (Field, MoonlightError (..))
import Moonlight.Derived.Pure.Failure (derivedFailureToMoonlightError)
import Moonlight.Derived.Pure.LinAlg.Interpreter (leftSolveDense)
import Moonlight.Derived.Pure.Gluing.Cone
  ( RawComplex (..)
  , coneOfRaw
  , rawDiffAt
  , rawFromDerived
  , rawLabelsAt
  )
import Moonlight.Derived.Pure.Gluing.Peeling (minimizeComposableComplex)
import Moonlight.Derived.Pure.Gluing.Resolution (resolutionStep)
import Moonlight.Derived.Pure.Site.DerivedMap
  ( derivedObjectWindow
  , zeroDerived
  )
import Moonlight.Derived.Pure.Site.InjectiveComplex
import Moonlight.Derived.Pure.Site.LabeledMatrix
import Moonlight.Derived.Pure.Site.Poset (DerivedPoset)

-- | The canonical truncation @τ≤n@: cohomology agrees with the input at and
-- below @n@ and vanishes above.
canonicalTruncateAtMost ::
  (Eq a, Field a, IntegralDomain a, Num a) =>
  Int -> Derived a -> Either MoonlightError (Derived a)
canonicalTruncateAtMost cutoffDegree derivedValue
  | cutoffDegree >= highDegree = Right derivedValue
  | cutoffDegree < lowDegree = Right (zeroDerived posetValue)
  | otherwise = do
      skeletonValue <- truncationSkeleton posetValue cutoffDegree derivedValue
      lowerTruncationFrom posetValue skeletonValue
  where
    posetValue = derivedPoset derivedValue
    (lowDegree, highDegree) = derivedObjectWindow derivedValue

-- | The canonical truncation @τ≥n@: cohomology agrees with the input at and
-- above @n@ and vanishes below.
canonicalTruncateAtLeast ::
  (Eq a, Field a, EuclideanDomain a, IntegralDomain a, Num a) =>
  Int -> Derived a -> Either MoonlightError (Derived a)
canonicalTruncateAtLeast cutoffDegree derivedValue
  | cutoffDegree <= lowDegree = Right derivedValue
  | cutoffDegree > highDegree = Right (zeroDerived posetValue)
  | otherwise = do
      skeletonValue <- truncationSkeleton posetValue (cutoffDegree - 1) derivedValue
      upperTruncationFrom posetValue (cutoffDegree - 1) derivedValue skeletonValue
  where
    posetValue = derivedPoset derivedValue
    (lowDegree, highDegree) = derivedObjectWindow derivedValue

-- | The canonical decomposition @(τ≤n A, τ≥n+1 A)@ — both halves of the
-- truncation triangle @τ≤n A → A → τ≥n+1 A → τ≤n A[1]@ from one skeleton:
-- the kept differentials and the exact tail are computed once and feed both
-- assemblies. Agrees with the two individual truncations exactly.
canonicalTruncationPair ::
  (Eq a, Field a, EuclideanDomain a, IntegralDomain a, Num a) =>
  Int -> Derived a -> Either MoonlightError (Derived a, Derived a)
canonicalTruncationPair cutoffDegree derivedValue
  | cutoffDegree >= highDegree = Right (derivedValue, zeroDerived posetValue)
  | cutoffDegree < lowDegree = Right (zeroDerived posetValue, derivedValue)
  | otherwise = do
      skeletonValue <- truncationSkeleton posetValue cutoffDegree derivedValue
      lowerValue <- lowerTruncationFrom posetValue skeletonValue
      upperValue <- upperTruncationFrom posetValue cutoffDegree derivedValue skeletonValue
      pure (lowerValue, upperValue)
  where
    posetValue = derivedPoset derivedValue
    (lowDegree, highDegree) = derivedObjectWindow derivedValue

data TruncationSkeleton a = TruncationSkeleton
  { truncationKeptDifferentials :: !(V.Vector (BlockedMat a))
  , truncationTailDifferentials :: ![BlockedMat a]
  , truncationComposableComplex :: !(ComposableInjectiveComplex a)
  }

truncationSkeleton ::
  (Eq a, Field a, IntegralDomain a, Num a) =>
  DerivedPoset -> Int -> Derived a -> Either MoonlightError (TruncationSkeleton a)
truncationSkeleton posetValue cutoffDegree derivedValue = do
  case V.unsnoc keptDiffs of
    Nothing -> Left (InvariantViolation "canonical truncation: empty retained differential window")
    Just (_, finalKeptDiff) -> do
      tailDiffs <- exactTail posetValue finalKeptDiff
      composableComplex <-
        either (Left . derivedFailureToMoonlightError) Right $
          mkComposableInjectiveComplex lowDegree (keptDiffs <> V.fromList tailDiffs)
      pure
        TruncationSkeleton
          { truncationKeptDifferentials = keptDiffs
          , truncationTailDifferentials = tailDiffs
          , truncationComposableComplex = composableComplex
          }
  where
    (lowDegree, _) = derivedObjectWindow derivedValue
    keptDiffs =
      V.slice
        0
        (cutoffDegree - lowDegree + 1)
        (injectiveComplexDiffs (derivedInjectiveComplex derivedValue))

lowerTruncationFrom ::
  (Eq a, Field a, IntegralDomain a, Num a) =>
  DerivedPoset -> TruncationSkeleton a -> Either MoonlightError (Derived a)
lowerTruncationFrom posetValue skeletonValue = do
  minimizedValue <- minimizeComposableComplex (truncationComposableComplex skeletonValue)
  either (Left . derivedFailureToMoonlightError) Right $
    mkNormalizedDerivedFromComposableChecked posetValue minimizedValue

upperTruncationFrom ::
  (Eq a, Field a, EuclideanDomain a, IntegralDomain a, Num a) =>
  DerivedPoset -> Int -> Derived a -> TruncationSkeleton a -> Either MoonlightError (Derived a)
upperTruncationFrom posetValue pairCutoff derivedValue skeletonValue = do
  extensionMaps <- solveExtensions
  let truncatedRaw =
        RawComplex
          { rcStart = lowDegree
          , rcLabels =
              V.fromList
                ( [ rawLabelsAt fullRaw degreeValue | degreeValue <- [lowDegree .. pairCutoff + 1] ]
                    <> fmap (axisLabelsExpanded . blockedMatRows) tailDiffs
                )
          , rcDiffs = fmap collapseBlockedDense (keptDiffs <> V.fromList tailDiffs)
          }
      componentMap =
        IM.fromList
          ( [ (degreeValue, identMat (V.length (rawLabelsAt fullRaw degreeValue)))
            | degreeValue <- [lowDegree .. pairCutoff + 1]
            ]
              <> zip [pairCutoff + 2 ..] extensionMaps
          )
      componentAt degreeValue =
        IM.findWithDefault
          (zeroMat
            (V.length (rawLabelsAt fullRaw degreeValue))
            (V.length (rawLabelsAt truncatedRaw degreeValue)))
          degreeValue
          componentMap
  coneOfRaw posetValue truncatedRaw fullRaw componentAt
  where
    keptDiffs = truncationKeptDifferentials skeletonValue
    tailDiffs = truncationTailDifferentials skeletonValue
    (lowDegree, _) = derivedObjectWindow derivedValue
    fullRaw = rawFromDerived derivedValue

    solveExtensions =
      go (pairCutoff + 1) (identMat (V.length (rawLabelsAt fullRaw (pairCutoff + 1)))) (fmap collapseBlockedDense tailDiffs)
      where
        go _ _ [] = Right []
        go degreeValue priorExtension (tailDense : laterTails) = do
          solvedValue <- leftSolveDense tailDense (matMul (rawDiffAt fullRaw degreeValue) priorExtension)
          case solvedValue of
            Nothing ->
              Left (InvariantViolation ("canonical truncation: extension solve failed at degree " <> show (degreeValue + 1)))
            Just extensionValue ->
              fmap (extensionValue :) (go (degreeValue + 1) extensionValue laterTails)

exactTail ::
  (Field a, IntegralDomain a, Num a) =>
  DerivedPoset -> BlockedMat a -> Either MoonlightError [BlockedMat a]
exactTail posetValue seedDiff = do
  nextDiff <- resolutionStep posetValue seedDiff
  if axisEmpty (blockedMatRows nextDiff)
    then Right [nextDiff]
    else fmap (nextDiff :) (exactTail posetValue nextDiff)
