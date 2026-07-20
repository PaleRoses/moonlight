{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Carrier.Morphism.SubsumptionProjectionFixtures
  ( InjectiveProjectionCase (..),
    CollisionProjectionCase (..),
    ImpossibleBoundaryProjectionCase (..),
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
    mkRuntimeBoundary,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Delta.Signed
  ( MultiplicityChange (..)
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchFromList,
  )

import Moonlight.Flow.Model.Schema.Morphism
  ( BoundaryProjection (..),
    SchemaProjection,
    compileSchemaProjection,
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Flow.Plan.Shape.Term
  ( CanonSlot (..),
  )
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    suchThatMap,
  )

data InjectiveProjectionCase = InjectiveProjectionCase
  { ipcProjection :: !(SchemaProjection SlotId CanonSlot),
    ipcRows :: !RowDelta,
    ipcExpectedRows :: !RowDelta
  }
  deriving stock (Show)

data CollisionProjectionCase = CollisionProjectionCase
  { cpcProjection :: !(SchemaProjection SlotId CanonSlot),
    cpcRows :: !RowDelta,
    cpcMergedRows :: !RowDelta
  }
  deriving stock (Show)

data ImpossibleBoundaryProjectionCase = ImpossibleBoundaryProjectionCase
  { ibpcBoundaryProjection :: !(BoundaryProjection CanonSlot),
    ibpcBoundary :: !RuntimeBoundary
  }
  deriving stock (Show)

instance Arbitrary InjectiveProjectionCase where
  arbitrary =
    genInjectiveProjectionCase

instance Arbitrary CollisionProjectionCase where
  arbitrary =
    genCollisionProjectionCase

instance Arbitrary ImpossibleBoundaryProjectionCase where
  arbitrary =
    genImpossibleBoundaryProjectionCase

genInjectiveProjectionCase :: Gen InjectiveProjectionCase
genInjectiveProjectionCase =
  genMaybeCase $ do
    projection <-
      rightToMaybe $
        compileSchemaProjection
          slotAsCanon
          [slot 0, slot 1]
          [slot 0, slot 1]
    let rows =
          coveredRows
            [ ([1, 2], 1),
              ([3, 4], 2)
            ]
        expected =
          rows
    pure
      InjectiveProjectionCase
        { ipcProjection = projection,
          ipcRows = rows,
          ipcExpectedRows = expected
        }

genCollisionProjectionCase :: Gen CollisionProjectionCase
genCollisionProjectionCase =
  genMaybeCase $ do
    projection <-
      rightToMaybe $
        compileSchemaProjection
          slotAsCanon
          [slot 0, slot 1]
          [slot 0]
    let rows =
          coveredRows
            [ ([7, 10], 1),
              ([7, 20], 2)
            ]
        expected =
          coveredRows
            [ ([7], 3)
            ]
    pure
      CollisionProjectionCase
        { cpcProjection = projection,
          cpcRows = rows,
          cpcMergedRows = expected
        }

genImpossibleBoundaryProjectionCase :: Gen ImpossibleBoundaryProjectionCase
genImpossibleBoundaryProjectionCase =
  genMaybeCase $ do
    projection <-
      rightToMaybe $
        compileSchemaProjection
          slotAsCanon
          [slot 0, slot 1]
          [slot 0]
    boundary <-
      rightToMaybe $
        mkRuntimeBoundary
          [slot 0, slot 1]
          (IntSet.singleton (slotIdKey (slot 1)))
          IntMap.empty
    pure
      ImpossibleBoundaryProjectionCase
        { ibpcBoundaryProjection = BoundaryProjection projection,
          ibpcBoundary = boundary
        }

coveredRows ::
  [([Int], Int)] ->
  RowDelta
coveredRows entries =
  plainRowPatchFromList
    [ (tupleKeyFromRepKeys (fmap RepKey values), MultiplicityChange (fromIntegral multiplicity))
    | (values, multiplicity) <- entries
    ]
{-# INLINE coveredRows #-}

slot :: Int -> SlotId
slot =
  mkSlotId
{-# INLINE slot #-}

rightToMaybe ::
  Either left right ->
  Maybe right
rightToMaybe value =
  case value of
    Left _ ->
      Nothing
    Right right ->
      Just right
{-# INLINE rightToMaybe #-}

genMaybeCase ::
  Maybe value ->
  Gen value
genMaybeCase =
  suchThatMap (pure ()) . const
{-# INLINE genMaybeCase #-}

slotAsCanon :: SlotId -> CanonSlot
slotAsCanon =
  CanonSlot . slotIdKey
{-# INLINE slotAsCanon #-}
