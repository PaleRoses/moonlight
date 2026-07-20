{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Derived.Pure.Site.InjectiveComplex
  ( Degree
  , InjectiveComplex (..)
  , Derived (..)
  , validateDerivedSiteMembership
  , LawfulInjectiveComplex
  , lawfulInjectiveComplex
  , trustLawfulInjectiveComplex
  , ComposableInjectiveComplex
  , composableInjectiveComplex
  , mkComposableInjectiveComplex
  , trustComposableInjectiveComplex
  , injectiveComplexStart
  , injectiveComplexDiffs
  , derivedInjectiveComplex
  , mkDerivedTrusted
  , mkDerivedChecked
  , mkDerived
  , isMinimal
  , firstNonMinimal
  , allDiagLabels
  , initialObjectAxis
  , complexObjectAxes
  , mkNormalizedDerived
  , mkNormalizedDerivedTrusted
  , mkNormalizedDerivedChecked
  , mkNormalizedDerivedFromComposableChecked
  , composesToZero
  , hasCompatibleObjectAxes
  , adjacentDifferentials
  , normalizeBoundaryPresentation
  , canonicalizeComplexAxes
  , normalizeComplexPresentation
  ) where

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Moonlight.Core (MoonlightError (..))
import Moonlight.Derived.Pure.Failure
  ( DerivedFailure (..)
  , derivedFailureToMoonlightError
  )
import Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset (..)
  , FinObjectId (..)
  , leq
  , memberOfDerivedPoset
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix

type Degree :: Type
type Degree = Int

type InjectiveComplex :: Type -> Type
data InjectiveComplex a = InjectiveComplex
  { icStart :: !Degree
  , icDiffs :: !(V.Vector (BlockedMat a))
  } deriving stock (Eq, Show)

type Derived :: Type -> Type
data Derived a = Derived
  { derivedPoset :: !DerivedPoset
  , getDerived :: !(InjectiveComplex a)
  }
  deriving stock (Eq, Show)

-- | Establish that every object axis of a formal derived complex is labelled by
-- the supplied site.  This is weaker than local restrictability: no order law is
-- claimed for its differential blocks.
validateDerivedSiteMembership :: DerivedPoset -> Derived a -> Either DerivedFailure ()
validateDerivedSiteMembership posetValue Derived {getDerived = complexValue} =
  traverse_ validateAxis (complexObjectAxes complexValue)
  where
    validateAxis GroupedAxis {gaOrder} =
      case V.find (not . memberOfDerivedPoset posetValue) gaOrder of
        Just (FinObjectId objectKey) -> Left (DerivedPosetUnknownNode objectKey)
        Nothing -> Right ()

type LawfulInjectiveComplex :: Type -> Type
newtype LawfulInjectiveComplex a = LawfulInjectiveComplex
  { lawfulInjectiveComplex :: InjectiveComplex a
  } deriving stock (Eq, Show)

type ComposableInjectiveComplex :: Type -> Type
-- | A nonempty injective-complex presentation whose adjacent axes agree and
-- whose consecutive differentials compose to zero.  Site membership, order
-- variance, and minimality are deliberately established only by the final
-- derived-object seal.
newtype ComposableInjectiveComplex a = ComposableInjectiveComplex
  { composableInjectiveComplex :: InjectiveComplex a
  }
  deriving stock (Eq, Show)

mkComposableInjectiveComplex :: (Eq a, Num a) => Degree -> Vector (BlockedMat a) -> Either DerivedFailure (ComposableInjectiveComplex a)
mkComposableInjectiveComplex startValue differentialValues
  | V.null differentialValues = Left DerivedComplexEmpty
  | not (hasCompatibleObjectAxes complexValue) = Left DerivedComplexIncompatibleAxes
  | not (composesToZero complexValue) = Left DerivedComplexNonzeroAdjacentComposition
  | otherwise = Right (ComposableInjectiveComplex complexValue)
  where
    complexValue =
      InjectiveComplex startValue differentialValues

-- | Internal proof transport for transformations, such as Schur
-- minimization, whose algebra preserves compatible axes and @d^2 = 0@.
-- Raw construction must use 'mkComposableInjectiveComplex' instead.
trustComposableInjectiveComplex :: InjectiveComplex a -> ComposableInjectiveComplex a
trustComposableInjectiveComplex =
  ComposableInjectiveComplex

injectiveComplexStart :: InjectiveComplex a -> Degree
injectiveComplexStart =
  icStart

injectiveComplexDiffs :: InjectiveComplex a -> Vector (BlockedMat a)
injectiveComplexDiffs =
  icDiffs

derivedInjectiveComplex :: Derived a -> InjectiveComplex a
derivedInjectiveComplex =
  getDerived

trustLawfulInjectiveComplex :: InjectiveComplex a -> LawfulInjectiveComplex a
trustLawfulInjectiveComplex =
  LawfulInjectiveComplex

mkDerivedTrusted :: DerivedPoset -> LawfulInjectiveComplex a -> Derived a
mkDerivedTrusted posetValue =
  Derived posetValue . lawfulInjectiveComplex

mkDerived :: (Eq a, Num a) => DerivedPoset -> InjectiveComplex a -> Either MoonlightError (Derived a)
mkDerived posetValue =
  either (Left . derivedFailureToMoonlightError) Right . mkDerivedChecked posetValue

mkDerivedChecked :: (Eq a, Num a) => DerivedPoset -> InjectiveComplex a -> Either DerivedFailure (Derived a)
mkDerivedChecked posetValue rawComplex = do
  composableComplex <-
    mkComposableInjectiveComplex
      (icStart normalizedComplex)
      (icDiffs normalizedComplex)
  sealNormalizedComposable posetValue composableComplex
  where
    normalizedComplex =
      normalizeComplexPresentation (derivedPosetTopoAsc posetValue) rawComplex

sealNormalizedComposable :: (Eq a, Num a) => DerivedPoset -> ComposableInjectiveComplex a -> Either DerivedFailure (Derived a)
sealNormalizedComposable posetValue composableValue
  | not (isMinimal complexValue) = Left DerivedComplexNonminimal
  | otherwise = do
      let derivedValue = Derived posetValue complexValue
      validateDerivedSiteMembership posetValue derivedValue
      validateDerivedOrder posetValue complexValue
      Right derivedValue
  where
    complexValue = composableInjectiveComplex composableValue

validateDerivedOrder :: DerivedPoset -> InjectiveComplex a -> Either DerivedFailure ()
validateDerivedOrder posetValue complexValue =
  if all differentialRespectsOrder (V.toList (icDiffs complexValue))
    then Right ()
    else Left DerivedComplexRestrictionUnstable
  where
    differentialRespectsOrder BlockedMat {bmBlocks} =
      all
        ( \(rowKey, rowBlocks) ->
            all (leq posetValue (FinObjectId rowKey) . FinObjectId) (IM.keys rowBlocks)
        )
        (IM.toAscList bmBlocks)

allDiagLabels :: BlockedMat a -> [FinObjectId]
allDiagLabels bm =
  [ r | r <- V.toList (gaOrder (bmRows bm)), axisMultiplicity (bmCols bm) r > 0 ]

isMinimal :: (Eq a, Num a) => InjectiveComplex a -> Bool
isMinimal InjectiveComplex {icDiffs} =
  V.all differentialIsMinimal icDiffs

differentialIsMinimal :: (Eq a, Num a) => BlockedMat a -> Bool
differentialIsMinimal blockedMat =
  case firstNonMinimalDiagonalBlock blockedMat of
    Nothing ->
      True
    Just _ ->
      False

firstNonMinimal :: (Eq a, Num a) => InjectiveComplex a -> Maybe (Int, FinObjectId)
firstNonMinimal InjectiveComplex {icDiffs} =
  go 0
  where
    go differentialIndex
      | differentialIndex >= V.length icDiffs =
          Nothing
      | otherwise =
          case firstNonMinimalDiagonalBlock (icDiffs V.! differentialIndex) of
            Nothing ->
              go (differentialIndex + 1)
            Just nodeValue ->
              Just (differentialIndex, nodeValue)

firstNonMinimalDiagonalBlock :: (Eq a, Num a) => BlockedMat a -> Maybe FinObjectId
firstNonMinimalDiagonalBlock blockedMat@BlockedMat {bmRows, bmCols} =
  firstStoredNonzeroDiagonal (V.toList (gaOrder bmRows))
  where
    firstStoredNonzeroDiagonal [] =
      Nothing
    firstStoredNonzeroDiagonal (nodeValue : remainingNodes)
      | axisMultiplicity bmCols nodeValue <= 0 =
          firstStoredNonzeroDiagonal remainingNodes
      | otherwise =
          case lookupStoredBlock nodeValue nodeValue blockedMat of
            Just diagonalBlock
              | not (isZeroMat diagonalBlock) ->
                  Just nodeValue
            _ ->
              firstStoredNonzeroDiagonal remainingNodes

lookupStoredBlock :: FinObjectId -> FinObjectId -> BlockedMat a -> Maybe (DenseMat a)
lookupStoredBlock (FinObjectId rowKey) (FinObjectId columnKey) BlockedMat {bmBlocks} =
  IM.lookup rowKey bmBlocks >>= IM.lookup columnKey

adjacentDifferentials :: InjectiveComplex a -> [(BlockedMat a, BlockedMat a)]
adjacentDifferentials InjectiveComplex{icDiffs} =
  zip (V.toList icDiffs) (drop 1 (V.toList icDiffs))

hasCompatibleObjectAxes :: InjectiveComplex a -> Bool
hasCompatibleObjectAxes =
  all (\(currentDiff, nextDiff) -> bmRows currentDiff == bmCols nextDiff)
    . adjacentDifferentials

composesToZero :: (Eq a, Num a) => InjectiveComplex a -> Bool
composesToZero =
  all
    (\(currentDiff, nextDiff) -> composeBlockedIsZero nextDiff currentDiff)
    . adjacentDifferentials

initialObjectAxis :: InjectiveComplex a -> Maybe GroupedAxis
initialObjectAxis InjectiveComplex{icDiffs}
  | Just (firstDifferential, _) <- V.uncons icDiffs =
      Just (bmCols firstDifferential)
  | otherwise =
      Nothing

complexObjectAxes :: InjectiveComplex a -> [GroupedAxis]
complexObjectAxes InjectiveComplex{icDiffs}
  | Just (firstDifferential, _) <- V.uncons icDiffs =
      bmCols firstDifferential : fmap bmRows (V.toList icDiffs)
  | otherwise =
      []

mkNormalizedDerived :: (Eq a, Num a) => DerivedPoset -> InjectiveComplex a -> Either MoonlightError (Derived a)
mkNormalizedDerived =
  mkDerived

mkNormalizedDerivedTrusted :: DerivedPoset -> LawfulInjectiveComplex a -> Derived a
mkNormalizedDerivedTrusted posetValue =
  mkDerivedTrusted posetValue
    . trustLawfulInjectiveComplex
    . normalizeComplexPresentation (derivedPosetTopoAsc posetValue)
    . lawfulInjectiveComplex

mkNormalizedDerivedChecked :: (Eq a, Num a) => DerivedPoset -> InjectiveComplex a -> Either DerivedFailure (Derived a)
mkNormalizedDerivedChecked =
  mkDerivedChecked

mkNormalizedDerivedFromComposableChecked :: (Eq a, Num a) => DerivedPoset -> ComposableInjectiveComplex a -> Either DerivedFailure (Derived a)
mkNormalizedDerivedFromComposableChecked posetValue =
  sealNormalizedComposable posetValue
    . ComposableInjectiveComplex
    . normalizeComplexPresentation (derivedPosetTopoAsc posetValue)
    . composableInjectiveComplex

canonicalizeAxisOrder :: Vector FinObjectId -> GroupedAxis -> GroupedAxis
canonicalizeAxisOrder canonical ga@GroupedAxis{gaOrder} =
  let axisKeys =
        IS.fromList (fmap unFinObjectId (V.toList gaOrder))
      ordered =
        [ objectValue
        | objectValue@(FinObjectId key) <- V.toList canonical
        , IS.member key axisKeys
        ]
      orderedKeys =
        IS.fromList (fmap unFinObjectId ordered)
      leftovers =
        [ objectValue
        | objectValue@(FinObjectId key) <- V.toList gaOrder
        , IS.member key axisKeys
        , not (IS.member key orderedKeys)
        ]
   in ga { gaOrder = V.fromList (ordered <> leftovers) }

canonicalizeBlockedAxes :: Vector FinObjectId -> BlockedMat a -> BlockedMat a
canonicalizeBlockedAxes canonical blockedMat =
  blockedMat
    { bmRows = canonicalizeAxisOrder canonical (bmRows blockedMat)
    , bmCols = canonicalizeAxisOrder canonical (bmCols blockedMat)
    }

canonicalizeComplexAxes :: Vector FinObjectId -> InjectiveComplex a -> InjectiveComplex a
canonicalizeComplexAxes canonical complex =
  complex { icDiffs = V.map (canonicalizeBlockedAxes canonical) (icDiffs complex) }

normalizeBoundaryPresentation :: InjectiveComplex a -> InjectiveComplex a
normalizeBoundaryPresentation complex@InjectiveComplex{icStart, icDiffs} =
  complex
    { icStart = normalizedStart
    , icDiffs = normalizedDiffs
    }
  where
    (!leadingStart, !leadingDiffs) =
      stripLeadingEmptyBoundarySources icStart (stripTrailingEmptyBoundaryTargets icDiffs)

    (!normalizedStart, !normalizedDiffs) =
      normalizeSingletonBoundaryTarget leadingStart leadingDiffs

stripTrailingEmptyBoundaryTargets :: V.Vector (BlockedMat a) -> V.Vector (BlockedMat a)
stripTrailingEmptyBoundaryTargets !diffs =
  case V.unsnoc diffs of
    Just (prefixDiffs, lastDifferential)
      | not (V.null prefixDiffs)
      , axisEmpty (bmRows lastDifferential) ->
          stripTrailingEmptyBoundaryTargets prefixDiffs
    _ ->
      diffs

stripLeadingEmptyBoundarySources :: Degree -> V.Vector (BlockedMat a) -> (Degree, V.Vector (BlockedMat a))
stripLeadingEmptyBoundarySources !startValue !diffs =
  case V.uncons diffs of
    Just (firstDifferential, suffixDiffs)
      | not (V.null suffixDiffs)
      , axisEmpty (bmCols firstDifferential) ->
          stripLeadingEmptyBoundarySources (startValue + 1) suffixDiffs
    _ ->
      (startValue, diffs)

normalizeSingletonBoundaryTarget :: Degree -> V.Vector (BlockedMat a) -> (Degree, V.Vector (BlockedMat a))
normalizeSingletonBoundaryTarget !startValue !diffs =
  case V.toList diffs of
    [onlyDifferential]
      | axisEmpty (bmCols onlyDifferential)
      , not (axisEmpty (bmRows onlyDifferential)) ->
          ( startValue + 1
          , V.singleton (zeroBlocked emptyAxis (bmRows onlyDifferential))
          )
    _ ->
      (startValue, diffs)

normalizeComplexPresentation :: Vector FinObjectId -> InjectiveComplex a -> InjectiveComplex a
normalizeComplexPresentation canonical =
  canonicalizeComplexAxes canonical . normalizeBoundaryPresentation
