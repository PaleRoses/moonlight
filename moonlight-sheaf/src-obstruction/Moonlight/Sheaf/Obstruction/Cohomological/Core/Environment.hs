{-# LANGUAGE RankNTypes #-}

module Moonlight.Sheaf.Obstruction.Cohomological.Core.Environment
  ( IndexedEnvironment,
    emptyIndexedEnvironment,
    indexedEnvironmentFromList,
    insertEnvironmentBinding,
    lookupEnvironmentBinding,
    environmentBindingKeys,
    IndexedEnvironmentBuilder (..),
    IndexedEnvironmentAlgebra,
    emptyIndexedEnvironmentAlgebra,
    registerEnvironmentBuilder,
    indexedEnvironmentAlgebraFromList,
    environmentBuilderKeys,
    buildIndexedEnvironment,
    ObstructionEnvironmentAlgebra (..),
  )
where

import Data.Kind (Type)
import Data.Dependent.Map (DMap)
import Data.Dependent.Map qualified as DMap
import Data.Dependent.Sum (DSum ((:=>)))
import Data.Functor.Identity (Identity (..))
import Data.GADT.Compare (GCompare)
import Data.List qualified as List
import Data.Proxy (Proxy (..))

type IndexedEnvironment :: (Type -> Type) -> Type
newtype IndexedEnvironment key = IndexedEnvironment
  { unIndexedEnvironment :: DMap key Identity
  }

emptyIndexedEnvironment :: IndexedEnvironment key
emptyIndexedEnvironment =
  IndexedEnvironment DMap.empty

indexedEnvironmentFromList ::
  GCompare key =>
  [DSum key Identity] ->
  IndexedEnvironment key
indexedEnvironmentFromList =
  IndexedEnvironment . DMap.fromList

insertEnvironmentBinding ::
  GCompare key =>
  key value ->
  value ->
  IndexedEnvironment key ->
  IndexedEnvironment key
insertEnvironmentBinding key value =
  IndexedEnvironment
    . DMap.insert key (Identity value)
    . unIndexedEnvironment

lookupEnvironmentBinding ::
  GCompare key =>
  key value ->
  IndexedEnvironment key ->
  Maybe value
lookupEnvironmentBinding key =
  fmap runIdentity
    . DMap.lookup key
    . unIndexedEnvironment

environmentBindingKeys ::
  IndexedEnvironment key ->
  DMap key Proxy
environmentBindingKeys =
  DMap.map (const Proxy)
    . unIndexedEnvironment

type IndexedEnvironmentBuilder :: Type -> Type -> Type -> Type -> Type -> Type
newtype IndexedEnvironmentBuilder request region occurrence guard value = IndexedEnvironmentBuilder
  { runIndexedEnvironmentBuilder ::
      request ->
      region ->
      [occurrence] ->
      [guard] ->
      value
  }

type IndexedEnvironmentAlgebra :: Type -> Type -> Type -> Type -> (Type -> Type) -> Type
newtype IndexedEnvironmentAlgebra request region occurrence guard key = IndexedEnvironmentAlgebra
  { unIndexedEnvironmentAlgebra ::
      DMap key (IndexedEnvironmentBuilder request region occurrence guard)
  }

emptyIndexedEnvironmentAlgebra :: IndexedEnvironmentAlgebra request region occurrence guard key
emptyIndexedEnvironmentAlgebra =
  IndexedEnvironmentAlgebra DMap.empty

registerEnvironmentBuilder ::
  GCompare key =>
  key value ->
  IndexedEnvironmentBuilder request region occurrence guard value ->
  IndexedEnvironmentAlgebra request region occurrence guard key ->
  IndexedEnvironmentAlgebra request region occurrence guard key
registerEnvironmentBuilder key builder =
  IndexedEnvironmentAlgebra
    . DMap.insert key builder
    . unIndexedEnvironmentAlgebra

indexedEnvironmentAlgebraFromList ::
  GCompare key =>
  [DSum key (IndexedEnvironmentBuilder request region occurrence guard)] ->
  IndexedEnvironmentAlgebra request region occurrence guard key
indexedEnvironmentAlgebraFromList =
  List.foldl'
    (\registry (key :=> builder) -> registerEnvironmentBuilder key builder registry)
    emptyIndexedEnvironmentAlgebra

environmentBuilderKeys ::
  IndexedEnvironmentAlgebra request region occurrence guard key ->
  DMap key Proxy
environmentBuilderKeys =
  DMap.map (const Proxy)
    . unIndexedEnvironmentAlgebra

buildIndexedEnvironment ::
  GCompare key =>
  request ->
  region ->
  [occurrence] ->
  [guard] ->
  IndexedEnvironmentAlgebra request region occurrence guard key ->
  IndexedEnvironment key
buildIndexedEnvironment request region occurrences guards =
  indexedEnvironmentFromList
    . fmap
      (\(key :=> builder) ->
         key :=> Identity (runIndexedEnvironmentBuilder builder request region occurrences guards)
      )
    . DMap.toAscList
    . unIndexedEnvironmentAlgebra

type ObstructionEnvironmentAlgebra :: (Type -> Type) -> (Type -> Type -> Type) -> Type -> Type -> Type -> Type -> Type
data ObstructionEnvironmentAlgebra request key query occurrence guard region = ObstructionEnvironmentAlgebra
  { oeaCollectOccurrences :: query -> [occurrence],
    oeaEnumerateRegions ::
      forall runtime.
      request runtime ->
      query ->
      [region],
    oeaRefineRegion ::
      forall runtime.
      request runtime ->
      query ->
      region ->
      [region],
    oeaIndexedEnvironmentAlgebra ::
      forall runtime.
      IndexedEnvironmentAlgebra (request runtime) region occurrence guard (key runtime),
    oeaQueryFingerprint :: query -> Int,
    oeaEnvironmentFingerprint ::
      forall runtime.
      request runtime ->
      Maybe Int
  }
