module Test.Moonlight.Flow.Property.Carrier.RestrictionChain
  ( tests,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( mkSlotId,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
    mkRuntimeBoundary,
    runtimeBoundaryKeys,
  )
import Moonlight.Flow.Carrier.Boundary.Restrict
  ( BoundaryRestrictionError,
    restrictRuntimeBoundary,
  )
import Moonlight.Differential.Row.Patch
  ( mapPlainRowPatchRows,
    plainRowPatchChangeMap,
    plainRowPatchFromList,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Delta.Signed
  ( MultiplicityChange (..)
  )
import Moonlight.Differential.Row.Tuple
import Test.QuickCheck
  ( Gen,
    Property,
    chooseInt,
    elements,
    forAll,
    listOf,
    vectorOf,
    (===),
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.QuickCheck
  ( testProperty,
  )

tests :: TestTree
tests =
  testGroup
    "restriction-chain-functoriality"
    [ testProperty
        "restrictTupleKeyPatch over a composite class map agrees with stepwise restriction"
        rowDeltaChainFunctoriality,
      testProperty
        "restrictRuntimeBoundary over a composite class map agrees with stepwise restriction"
        boundaryChainFunctoriality
    ]

rowDeltaChainFunctoriality :: Property
rowDeltaChainFunctoriality =
  forAll rowDeltaPatchGen $ \delta ->
    forAll restrictionMapChainGen $ \maps ->
      let composite =
            composeMapsOver (rowDeltaPatchSourceKeys delta) maps
          direct =
            restrictRows composite delta
          stepwise =
            List.foldl'
              (\acc classMap -> restrictRows classMap acc)
              delta
              maps
       in direct === stepwise

boundaryChainFunctoriality :: Property
boundaryChainFunctoriality =
  forAll boundaryGen $ \boundary ->
    forAll restrictionMapChainGen $ \maps ->
      let composite =
            composeMapsOver (runtimeBoundaryKeys boundary) maps
          direct =
            restrictRuntimeBoundary composite boundary
          stepwise =
            stepwiseBoundaryRestriction maps boundary
       in direct === stepwise

stepwiseBoundaryRestriction ::
  [IntMap RepKey] ->
  RuntimeBoundary ->
  Either BoundaryRestrictionError RuntimeBoundary
stepwiseBoundaryRestriction maps boundary =
  List.foldl'
    ( \accumulated classMap ->
        case accumulated of
          Left err ->
            Left err
          Right currentBoundary ->
            restrictRuntimeBoundary classMap currentBoundary
    )
    (Right boundary)
    maps

restrictionMapChainGen :: Gen [IntMap RepKey]
restrictionMapChainGen = do
  chainLength <- elements [10, 100, 1000]
  vectorOf chainLength restrictionMapGen

restrictionMapGen :: Gen (IntMap RepKey)
restrictionMapGen =
  IntMap.fromList <$> listOf entry
  where
    entry = do
      source <- chooseInt (0, 16)
      target <- chooseInt (0, 8)
      pure (source, RepKey target)

rowDeltaPatchGen :: Gen RowDelta
rowDeltaPatchGen =
  rowDeltaGen

rowDeltaGen :: Gen RowDelta
rowDeltaGen =
  plainRowPatchFromList
    <$> listOf
      ((,) <$> generatedAtomRow <*> generatedMultiplicity)
  where
    generatedMultiplicity =
      MultiplicityChange . fromIntegral <$> chooseInt (-4, 4)
    generatedAtomRow :: Gen RowTupleKey
    generatedAtomRow = do
      width <- chooseInt (0, 6)
      tupleKeyFromRepKeys . fmap RepKey
        <$> vectorOf width (chooseInt (0, 16))

boundaryGen :: Gen RuntimeBoundary
boundaryGen = do
  width <- chooseInt (0, 8)
  let schema =
        fmap mkSlotId [0 .. width - 1]
  sensitives <- subsetOfRange width
  slotKeys <- slotKeysGen width
  case mkRuntimeBoundary schema sensitives slotKeys of
    Right boundary ->
      pure boundary
    Left err ->
      error
        ( "boundaryGen produced an invalid boundary: "
            <> show err
        )

subsetOfRange :: Int -> Gen IntSet
subsetOfRange width
  | width <= 0 = pure IntSet.empty
  | otherwise =
      IntSet.fromList
        <$> listOf (chooseInt (0, width - 1))

slotKeysGen :: Int -> Gen (IntMap IntSet)
slotKeysGen width
  | width <= 0 = pure IntMap.empty
  | otherwise = do
      keysBySlot <-
        traverse
          ( \slot -> do
              representatives <- listOf (chooseInt (0, 16))
              pure (slot, IntSet.fromList representatives)
          )
          [0 .. width - 1]
      pure (IntMap.fromList keysBySlot)

rowDeltaPatchSourceKeys :: RowDelta -> IntSet
rowDeltaPatchSourceKeys =
  rowDeltaSourceKeys

rowDeltaSourceKeys :: RowDelta -> IntSet
rowDeltaSourceKeys =
  Map.foldlWithKey'
    (\acc row _multiplicity -> IntSet.union acc (tupleKeyClassKeys row))
    IntSet.empty
    . plainRowPatchChangeMap

composeMapsOver :: IntSet -> [IntMap RepKey] -> IntMap RepKey
composeMapsOver sourceDomain maps =
  IntSet.foldl'
    ( \composite sourceKey ->
        let targetKey =
              applyMaps maps sourceKey
         in if targetKey == sourceKey
              then composite
              else IntMap.insert sourceKey (RepKey targetKey) composite
    )
    IntMap.empty
    sourceDomain

restrictRows ::
  IntMap RepKey ->
  RowDelta ->
  RowDelta
restrictRows targetClasses =
  mapPlainRowPatchRows (restrictTupleKey targetClasses)
{-# INLINE restrictRows #-}

applyMaps :: [IntMap RepKey] -> Int -> Int
applyMaps maps sourceKey =
  List.foldl'
    ( \current classMap ->
        case IntMap.lookup current classMap of
          Nothing -> current
          Just (RepKey value) -> value
    )
    sourceKey
    maps
