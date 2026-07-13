{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Model.Id
  ( BagId (..),
  )
where

import Data.Kind (Type)

type BagId :: Type
newtype BagId = BagId {unBagId :: Int}
  deriving stock (Eq, Ord, Show, Read)
