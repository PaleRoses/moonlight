{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectKey (..),
    SheafModelVersion (..),
    initialSheafModelVersion,
    ObjectIndex,
    mkObjectIndex,
  )
where

import Data.Kind (Type)
import Moonlight.Core (DenseKey (..))
import Moonlight.Sheaf.Index.Dense
  ( DenseIndex,
    mkDenseIndex,
  )

type ObjectKey :: Type
newtype ObjectKey = ObjectKey
  { unObjectKey :: Int
  }
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum)

type SheafModelVersion :: Type
newtype SheafModelVersion = SheafModelVersion
  { unSheafModelVersion :: Int
  }
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum, Num)

initialSheafModelVersion :: SheafModelVersion
initialSheafModelVersion =
  SheafModelVersion 0
{-# INLINE initialSheafModelVersion #-}

instance DenseKey ObjectKey where
  encodeDenseKey = unObjectKey
  decodeDenseKey = ObjectKey

type ObjectIndex :: Type -> Type
type ObjectIndex cell = DenseIndex ObjectKey cell

mkObjectIndex :: Ord cell => [cell] -> ObjectIndex cell
mkObjectIndex =
  mkDenseIndex
