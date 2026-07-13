{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Sheaf.Twist.Extraction
  ( ContextualExtractionPartition (..),
    ExtractionGate (..),
    unrestrictedExtractionGate,
    contextualExtractionPartitionsWith,
    contextualExtractionPartitionsWithGate,
    accumulateContextExtraction,
    accumulateContextExtractionWithGate,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

type ContextualExtractionPartition :: Type -> Type -> Type
data ContextualExtractionPartition result ctx = ContextualExtractionPartition
  { cepContexts :: !(Set ctx),
    cepResult :: !result
  }
  deriving stock (Eq, Ord, Show)

type ExtractionGate :: Type -> Type -> Type
newtype ExtractionGate ctx result = ExtractionGate
  { runExtractionGate :: ctx -> result -> Bool
  }

unrestrictedExtractionGate :: ExtractionGate ctx result
unrestrictedExtractionGate =
  ExtractionGate (\_contextValue _resultValue -> True)

contextualExtractionPartitionsWith ::
  (Ord ctx, Ord result) =>
  [ctx] ->
  (ctx -> Maybe result) ->
  [ContextualExtractionPartition result ctx]
contextualExtractionPartitionsWith =
  contextualExtractionPartitionsWithGate unrestrictedExtractionGate

contextualExtractionPartitionsWithGate ::
  (Ord ctx, Ord result) =>
  ExtractionGate ctx result ->
  [ctx] ->
  (ctx -> Maybe result) ->
  [ContextualExtractionPartition result ctx]
contextualExtractionPartitionsWithGate gate contexts extractAt =
  fmap
    (\(resultValue, contextSet) -> ContextualExtractionPartition contextSet resultValue)
    (Map.toAscList (foldr (accumulateContextExtractionWithGate gate extractAt) Map.empty contexts))

accumulateContextExtraction ::
  (Ord ctx, Ord result) =>
  (ctx -> Maybe result) ->
  ctx ->
  Map result (Set ctx) ->
  Map result (Set ctx)
accumulateContextExtraction =
  accumulateContextExtractionWithGate unrestrictedExtractionGate

accumulateContextExtractionWithGate ::
  (Ord ctx, Ord result) =>
  ExtractionGate ctx result ->
  (ctx -> Maybe result) ->
  ctx ->
  Map result (Set ctx) ->
  Map result (Set ctx)
accumulateContextExtractionWithGate (ExtractionGate admissible) extractAt contextValue grouped =
  case extractAt contextValue of
    Nothing ->
      grouped
    Just resultValue ->
      if admissible contextValue resultValue
        then Map.insertWith Set.union resultValue (Set.singleton contextValue) grouped
        else grouped
