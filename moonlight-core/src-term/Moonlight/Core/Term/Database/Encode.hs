{-# LANGUAGE QuantifiedConstraints #-}

module Moonlight.Core.Term.Database.Encode where

import Moonlight.Core.DenseKey (DenseKey (..))
import Moonlight.Core.Term.Database.Types
import Prelude

encodedRow ::
  (DenseKey key, Foldable f) =>
  key ->
  f key ->
  DatabaseRow
encodedRow resultValue tupleValue =
  DatabaseRow
    { rowResult = encodeDenseKey resultValue,
      rowChildren = encodedChildren tupleValue
    }

encodedChildren ::
  (DenseKey key, Foldable f) =>
  f key ->
  [Int]
encodedChildren =
  foldr ((:) . encodeDenseKey) []
{-# INLINE encodedChildren #-}
