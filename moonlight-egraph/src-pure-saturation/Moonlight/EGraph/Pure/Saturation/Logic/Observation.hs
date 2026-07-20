{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Pure.Saturation.Logic.Observation
  ( StableObservation (..),
    SomeStableObservation (..),
    SomeStableObservationResult (..),
    StableObservationError (..),
    runStableObservation,
    runSomeStableObservation,
    runSomeStableObservations,
  )
where

import Data.Kind (Type)
import Data.Bifunctor (first)
import Moonlight.Core (ClassId, Language)
import Moonlight.EGraph.Pure.Change (GraphPhase (..), eGraphPhase)
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    contextRepresentativeAt,
  )
import Moonlight.EGraph.Pure.Context (cegBase)
import Moonlight.EGraph.Pure.Extraction
  ( AnalysisCostAlgebra,
    ExtractionResult,
    extractFromTable,
    extractWithAnalysis,
    stableExtractionSnapshotFromEGraph,
  )
import Moonlight.EGraph.Pure.Saturation.Extraction
  ( ContextualExtractionObstruction,
    contextualExtractionTable,
  )
import Moonlight.EGraph.Pure.Types (EGraph, canonicalizeClassId)
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingContextEGraph,
    sceContextGraph,
  )
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSupportError,
  )

type StableObservation :: (Type -> Type) -> Type -> Type -> Type -> Type
data StableObservation f a c result where
  CheckEquivalentBase ::
    ClassId ->
    ClassId ->
    StableObservation f a c Bool
  CheckEquivalentAt ::
    c ->
    ClassId ->
    ClassId ->
    StableObservation f a c Bool
  ExtractBase ::
    Ord cost =>
    AnalysisCostAlgebra f a cost ->
    ClassId ->
    StableObservation f a c (Maybe (ExtractionResult f cost))
  ExtractAt ::
    Ord cost =>
    c ->
    AnalysisCostAlgebra f a cost ->
    ClassId ->
    StableObservation f a c (Maybe (ExtractionResult f cost))

type SomeStableObservation :: (Type -> Type) -> Type -> Type -> Type
data SomeStableObservation f a c where
  SomeStableObservation :: StableObservation f a c result -> SomeStableObservation f a c

type SomeStableObservationResult :: (Type -> Type) -> Type
data SomeStableObservationResult f where
  SomeCheckResult :: !Bool -> SomeStableObservationResult f
  SomeExtractionResult :: Maybe (ExtractionResult f cost) -> SomeStableObservationResult f

data StableObservationError c
  = StableObservationDirtyBaseGraph
  | StableObservationDirtyContextGraph !c
  | StableObservationContextLookupFailed !(PreparedContextSupportError c)
  | StableObservationContextualExtractionFailed !(ContextualExtractionObstruction c)
  deriving stock (Eq, Ord, Show)

runSomeStableObservations ::
  (Language f, Ord c) =>
  SaturatingContextEGraph owner capability f a c ->
  [SomeStableObservation f a c] ->
  Either (StableObservationError c) [SomeStableObservationResult f]
runSomeStableObservations graph =
  traverse (`runSomeStableObservation` graph)
{-# INLINE runSomeStableObservations #-}

runSomeStableObservation ::
  (Language f, Ord c) =>
  SomeStableObservation f a c ->
  SaturatingContextEGraph owner capability f a c ->
  Either (StableObservationError c) (SomeStableObservationResult f)
runSomeStableObservation (SomeStableObservation observation) graph =
  case observation of
    CheckEquivalentBase {} ->
      SomeCheckResult <$> runStableObservation observation graph
    CheckEquivalentAt {} ->
      SomeCheckResult <$> runStableObservation observation graph
    ExtractBase {} ->
      SomeExtractionResult <$> runStableObservation observation graph
    ExtractAt {} ->
      SomeExtractionResult <$> runStableObservation observation graph
{-# INLINE runSomeStableObservation #-}

runStableObservation ::
  (Language f, Ord c) =>
  StableObservation f a c result ->
  SaturatingContextEGraph owner capability f a c ->
  Either (StableObservationError c) result
runStableObservation observation graph =
  case observation of
    CheckEquivalentBase leftClass rightClass ->
      stableEquivalentBase leftClass rightClass (baseGraphOf graph)
    CheckEquivalentAt contextValue leftClass rightClass ->
      stableEquivalentAt contextValue leftClass rightClass (sceContextGraph graph)
    ExtractBase costAlgebra classId ->
      stableExtractBase costAlgebra classId (baseGraphOf graph)
    ExtractAt contextValue costAlgebra classId ->
      stableExtractAt contextValue costAlgebra classId (sceContextGraph graph)
{-# INLINE runStableObservation #-}

baseGraphOf :: SaturatingContextEGraph owner capability f a c -> EGraph f a
baseGraphOf =
  cegBase . sceContextGraph
{-# INLINE baseGraphOf #-}

stableEquivalentBase ::
  ClassId ->
  ClassId ->
  EGraph f a ->
  Either (StableObservationError c) Bool
stableEquivalentBase leftClass rightClass graph =
  if eGraphPhase graph == Stable
    then Right (canonicalizeClassId graph leftClass == canonicalizeClassId graph rightClass)
    else Left StableObservationDirtyBaseGraph
{-# INLINE stableEquivalentBase #-}

stableEquivalentAt ::
  Ord c =>
  c ->
  ClassId ->
  ClassId ->
  ContextEGraph owner f a c ->
  Either (StableObservationError c) Bool
stableEquivalentAt contextValue leftClass rightClass contextGraph =
  if eGraphPhase (cegBase contextGraph) == Stable
    then do
      leftRepresentative <-
        first StableObservationContextLookupFailed
          (contextRepresentativeAt contextValue leftClass contextGraph)
      rightRepresentative <-
        first StableObservationContextLookupFailed
          (contextRepresentativeAt contextValue rightClass contextGraph)
      pure (leftRepresentative == rightRepresentative)
    else Left (StableObservationDirtyContextGraph contextValue)
{-# INLINE stableEquivalentAt #-}

stableExtractBase ::
  (Language f, Ord cost) =>
  AnalysisCostAlgebra f a cost ->
  ClassId ->
  EGraph f a ->
  Either (StableObservationError c) (Maybe (ExtractionResult f cost))
stableExtractBase costAlgebra classId graph =
  case stableExtractionSnapshotFromEGraph graph of
    Nothing ->
      Left StableObservationDirtyBaseGraph
    Just snapshot ->
      Right (extractWithAnalysis costAlgebra classId snapshot)
{-# INLINE stableExtractBase #-}

stableExtractAt ::
  (Language f, Ord cost, Ord c) =>
  c ->
  AnalysisCostAlgebra f a cost ->
  ClassId ->
  ContextEGraph owner f a c ->
  Either (StableObservationError c) (Maybe (ExtractionResult f cost))
stableExtractAt contextValue costAlgebra classId contextGraph = do
  table <-
    first StableObservationContextualExtractionFailed
      (contextualExtractionTable contextValue contextGraph)
  pure (extractFromTable costAlgebra classId table)
{-# INLINE stableExtractAt #-}
