{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Moonlight.Sheaf.Section.Store.Internal
  ( SectionEpoch (..),
    DenseSection (..),
    SparseSection (..),
    TotalSectionStore (..),
    PartialSectionStore (..),
    advanceTotalSectionStore,
  )
where

import Data.IntSet (IntSet)
import Data.Map.Strict (Map)
import Data.Vector (Vector)
import Moonlight.Delta.Scope (Scope)
import Moonlight.Sheaf.Section.Model (ModelFingerprint)
import Moonlight.Sheaf.Section.ObjectIndex (SheafModelVersion)

newtype SectionEpoch = SectionEpoch
  { unSectionEpoch :: Int
  }
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum, Num)

newtype DenseSection stalk = DenseSection
  { unDenseSection :: Vector stalk
  }
  deriving stock (Eq, Show)

newtype SparseSection cell stalk = SparseSection
  { unSparseSection :: Map cell stalk
  }
  deriving stock (Eq, Show)

data TotalSectionStore cell stalk = TotalSectionStore
  { tssModelFingerprint :: !ModelFingerprint,
    tssModelVersion :: !SheafModelVersion,
    tssValues :: !(DenseSection stalk),
    tssExtent :: !(Scope IntSet),
    tssEpoch :: !SectionEpoch
  }
  deriving stock (Eq, Show)

data PartialSectionStore cell stalk = PartialSectionStore
  { pssModelFingerprint :: !ModelFingerprint,
    pssModelVersion :: !SheafModelVersion,
    pssValues :: !(SparseSection cell stalk),
    pssExtent :: !(Scope IntSet),
    pssEpoch :: !SectionEpoch
  }
  deriving stock (Eq, Show)

advanceTotalSectionStore ::
  DenseSection stalk ->
  Scope IntSet ->
  TotalSectionStore cell stalk ->
  TotalSectionStore cell stalk
advanceTotalSectionStore values extent store =
  store
    { tssValues = values,
      tssExtent = extent,
      tssEpoch = nextSectionEpoch (tssEpoch store)
    }
{-# INLINE advanceTotalSectionStore #-}

nextSectionEpoch :: SectionEpoch -> SectionEpoch
nextSectionEpoch (SectionEpoch epoch) =
  SectionEpoch (epoch + 1)
