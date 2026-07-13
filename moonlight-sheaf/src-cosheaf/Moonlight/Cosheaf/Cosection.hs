{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Cosheaf.Cosection
  ( CosectionRepKey (..),
    CosectionClassKey (..),
    CosectionRepresentative (..),
    GlobalCosection (..),
    cosectionRepKeyInt,
    cosectionClassKeyInt,
    cosectionClassOfRepresentativeKey,
  )
where

import Data.Kind (Type)
import Moonlight.Core (DenseKey (..))

type CosectionRepKey :: Type
newtype CosectionRepKey = CosectionRepKey
  { unCosectionRepKey :: Int
  }
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum)

instance DenseKey CosectionRepKey where
  encodeDenseKey =
    unCosectionRepKey
  {-# INLINE encodeDenseKey #-}

  decodeDenseKey =
    CosectionRepKey
  {-# INLINE decodeDenseKey #-}

type CosectionClassKey :: Type
newtype CosectionClassKey = CosectionClassKey
  { unCosectionClassKey :: Int
  }
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum)

type CosectionRepresentative :: Type -> Type -> Type
data CosectionRepresentative obj value = CosectionRepresentative
  { cosectionRepObject :: !obj,
    cosectionRepValue :: !value
  }
  deriving stock (Eq, Ord, Show)

type GlobalCosection :: Type -> Type -> Type
data GlobalCosection obj value = GlobalCosection
  { globalCosectionClass :: !CosectionClassKey,
    globalCosectionRepresentative :: !(CosectionRepresentative obj value)
  }
  deriving stock (Eq, Ord, Show)

cosectionRepKeyInt :: CosectionRepKey -> Int
cosectionRepKeyInt =
  unCosectionRepKey
{-# INLINE cosectionRepKeyInt #-}

cosectionClassKeyInt :: CosectionClassKey -> Int
cosectionClassKeyInt =
  unCosectionClassKey
{-# INLINE cosectionClassKeyInt #-}

cosectionClassOfRepresentativeKey :: CosectionRepKey -> CosectionClassKey
cosectionClassOfRepresentativeKey =
  CosectionClassKey . unCosectionRepKey
{-# INLINE cosectionClassOfRepresentativeKey #-}
