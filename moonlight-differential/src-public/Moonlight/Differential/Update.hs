{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Update
  ( Update (..),
  )
where

import Data.Kind
  ( Type,
  )

type Update :: Type -> Type -> Type -> Type -> Type
data Update time key val weight = Update
  { updateTime :: !time,
    updateKey :: !key,
    updateVal :: !val,
    updateWeight :: !weight
  }
  deriving stock (Eq, Ord, Show, Read)
