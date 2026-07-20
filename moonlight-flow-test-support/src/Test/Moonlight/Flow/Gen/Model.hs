{-# LANGUAGE DerivingStrategies #-}

module Test.Moonlight.Flow.Gen.Model
  ( GeneratedRepKey (..),
    GeneratedAtomRow (..),
    GeneratedMultiplicity (..),
    genRepKey,
    genAtomRow,
    genMultiplicity,
    genRowPatch,
    genRestrictionMap,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Delta.Signed
  ( MultiplicityChange (..)
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchFromList
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    RepKey (..),
    tupleKeyFromRepKeys,
  )
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    chooseInt,
    listOf,
    vectorOf,
  )

newtype GeneratedRepKey = GeneratedRepKey
  { unGeneratedRepKey :: RepKey
  }
  deriving stock (Eq, Show)

newtype GeneratedAtomRow = GeneratedAtomRow
  { unGeneratedAtomRow :: RowTupleKey
  }
  deriving stock (Eq, Show)

newtype GeneratedMultiplicity = GeneratedMultiplicity
  { unGeneratedMultiplicity :: MultiplicityChange
  }
  deriving stock (Eq, Show)

instance Arbitrary GeneratedRepKey where
  arbitrary = GeneratedRepKey <$> genRepKey

instance Arbitrary GeneratedAtomRow where
  arbitrary = GeneratedAtomRow <$> genAtomRow

instance Arbitrary GeneratedMultiplicity where
  arbitrary = GeneratedMultiplicity <$> genMultiplicity

genRepKey :: Gen RepKey
genRepKey = RepKey <$> chooseInt (0, 16)

genAtomRow :: Gen RowTupleKey
genAtomRow = do
  width <- chooseInt (0, 6)
  tupleKeyFromRepKeys <$> vectorOf width genRepKey

genMultiplicity :: Gen MultiplicityChange
genMultiplicity = MultiplicityChange . fromIntegral <$> chooseInt (-4, 4)

genRowPatch :: Gen RowDelta
genRowPatch =
  plainRowPatchFromList
    <$> listOf ((,) <$> genAtomRow <*> genMultiplicity)

genRestrictionMap :: Gen (IntMap RepKey)
genRestrictionMap =
  IntMap.fromList <$> listOf entry
  where
    entry = do
      source <- chooseInt (0, 16)
      target <- chooseInt (0, 8)
      pure (source, RepKey target)
