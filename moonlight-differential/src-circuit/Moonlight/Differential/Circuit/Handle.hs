{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Internal handle representations; the public surface re-exports these
-- abstract so minting stays a builder monopoly.
module Moonlight.Differential.Circuit.Handle
  ( Node (..),
    IndexedNode (..),
    InputPort (..),
  )
where

import Data.Kind
  ( Type,
  )

type Node :: Type -> Type -> Type
newtype Node s value = Node Int
  deriving stock (Eq, Ord, Show)

type role Node nominal nominal

type IndexedNode :: Type -> Type -> Type -> Type
newtype IndexedNode s key value = IndexedNode Int
  deriving stock (Eq, Ord, Show)

type role IndexedNode nominal nominal nominal

type InputPort :: Type -> Type -> Type
newtype InputPort s value = InputPort Int
  deriving stock (Eq, Ord, Show)

type role InputPort nominal nominal
