{-# LANGUAGE BangPatterns #-}

module Moonlight.Flow.Model.Schema.Digest.Words
  ( digestListWords,
    digestSetWords,
    digestMapSetWords,
    digestMaybeWords,
    digestIntSetWords,
    digestIntMapIntSetWords,
    digestIntMapWords,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Data.Word
  ( Word64,
  )
import Moonlight.Flow.Internal.Digest
  ( wordOfInt,
  )

digestListWords ::
  Word64 ->
  (value -> [Word64]) ->
  [value] ->
  [Word64]
digestListWords tag valueWords values =
  tag : wordOfInt (length values) : foldMap valueWords values
{-# INLINE digestListWords #-}

digestSetWords ::
  Word64 ->
  (value -> [Word64]) ->
  Set value ->
  [Word64]
digestSetWords tag valueWords values =
  tag : wordOfInt (Set.size values) : foldMap valueWords (Set.toAscList values)
{-# INLINE digestSetWords #-}

digestMapSetWords ::
  Word64 ->
  Word64 ->
  (key -> [Word64]) ->
  (value -> [Word64]) ->
  Map key (Set value) ->
  [Word64]
digestMapSetWords mapTag setTag keyWords valueWords values =
  mapTag : wordOfInt (Map.size values) : Map.foldrWithKey consEntry [] values
  where
    consEntry key valueSet acc =
      keyWords key <> digestSetWords setTag valueWords valueSet <> acc
{-# INLINE digestMapSetWords #-}

digestMaybeWords ::
  Word64 ->
  Word64 ->
  (value -> [Word64]) ->
  Maybe value ->
  [Word64]
digestMaybeWords nothingTag _justTag _valueWords Nothing =
  [nothingTag]
digestMaybeWords _nothingTag justTag valueWords (Just value) =
  justTag : valueWords value
{-# INLINE digestMaybeWords #-}

digestIntSetWords ::
  Word64 ->
  IntSet ->
  [Word64]
digestIntSetWords tag values =
  tag : wordOfInt (IntSet.size values) : fmap wordOfInt (IntSet.toAscList values)
{-# INLINE digestIntSetWords #-}

digestIntMapIntSetWords ::
  Word64 ->
  Word64 ->
  IntMap IntSet ->
  [Word64]
digestIntMapIntSetWords mapTag setTag values =
  mapTag : wordOfInt (IntMap.size values) : IntMap.foldrWithKey consEntry [] values
  where
    consEntry key valueSet acc =
      wordOfInt key : digestIntSetWords setTag valueSet <> acc
{-# INLINE digestIntMapIntSetWords #-}

digestIntMapWords ::
  Word64 ->
  (value -> [Word64]) ->
  IntMap value ->
  [Word64]
digestIntMapWords tag valueWords values =
  tag : wordOfInt (IntMap.size values) : IntMap.foldrWithKey consEntry [] values
  where
    consEntry key value acc =
      wordOfInt key : valueWords value <> acc
{-# INLINE digestIntMapWords #-}
