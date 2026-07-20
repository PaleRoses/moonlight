{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Carrier.Morphism.Internal.RestrictionGraph
  ( CarrierRestrictionGraph (..),
    emptyCarrierRestrictionGraph,
    insertCompiledCarrierRestriction,
    installCarrierRestrictionPrograms,
    carrierRestrictionProgramCount,
    restrictionProgramsFrom,
    restrictionProgramsTo,
    lookupCompiledCarrierRestriction,
    hasCompiledCarrierRestriction,
    restrictionProgramsBetweenFrom,
    restrictionProgramsBetweenTo,
    restrictionProgramBetween,
    restrictCarrierDeltaBranches,
    compileCarrierRestrictionGraph,
    validateRestrictionAcyclic,
  )
where

import Algebra.Graph.AdjacencyMap qualified as AdjacencyMap
import Algebra.Graph.AdjacencyMap.Algorithm qualified as AdjacencyMapAlgorithm
import Data.Bifunctor
  ( first,
  )
import Data.Kind
  ( Type,
  )
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Maybe
  ( isJust,
  )
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Core
  ( DenseKey,
  )
import Moonlight.Differential.Context.Restriction
  ( ContextRestrictionEdge (..),
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
    RestrictKey,
    rkSource,
    rkTarget,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Morphism.Restriction
  ( CarrierRestrictionDiagnostic (..),
    CarrierRestrictionEdgeSpec (..),
    CarrierRestrictionInstallError (..),
    CompiledCarrierRestriction (..),
    ContextRank,
    RestrictionDeltaError,
    compileCarrierRestrictionsForEdge,
    restrictCarrierDelta,
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchNull,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.FiniteLattice
  ( ContextLattice
  )

type CarrierRestrictionGraph :: Type -> Type -> Type -> Type -> Type
data CarrierRestrictionGraph ctx carrier prop boundary = CarrierRestrictionGraph
  { crgProgramsBySource :: !(Map (CarrierAddr ctx carrier prop) [CompiledCarrierRestriction ctx carrier prop boundary]),
    crgProgramsByTarget :: !(Map (CarrierAddr ctx carrier prop) [CompiledCarrierRestriction ctx carrier prop boundary])
  }
  deriving stock (Eq, Show)

emptyCarrierRestrictionGraph :: CarrierRestrictionGraph ctx carrier prop boundary
emptyCarrierRestrictionGraph =
  CarrierRestrictionGraph
    { crgProgramsBySource = Map.empty,
      crgProgramsByTarget = Map.empty
    }
{-# INLINE emptyCarrierRestrictionGraph #-}

insertCompiledCarrierRestriction ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CompiledCarrierRestriction ctx carrier prop boundary ->
  CarrierRestrictionGraph ctx carrier prop boundary ->
  CarrierRestrictionGraph ctx carrier prop boundary
insertCompiledCarrierRestriction program graph =
  let key =
        ccrKey program
      sourceAddress =
        rkSource key
      targetAddress =
        rkTarget key
   in graph
        { crgProgramsBySource =
            Map.insertWith
              (<>)
              sourceAddress
              [program]
              (crgProgramsBySource graph),
          crgProgramsByTarget =
            Map.insertWith
              (<>)
              targetAddress
              [program]
              (crgProgramsByTarget graph)
        }
{-# INLINE insertCompiledCarrierRestriction #-}

installCarrierRestrictionPrograms ::
  (Ord ctx, Ord carrier, Ord prop) =>
  [CompiledCarrierRestriction ctx carrier prop boundary] ->
  CarrierRestrictionGraph ctx carrier prop boundary
installCarrierRestrictionPrograms =
  List.foldl'
    (flip insertCompiledCarrierRestriction)
    emptyCarrierRestrictionGraph
{-# INLINE installCarrierRestrictionPrograms #-}

carrierRestrictionProgramCount ::
  CarrierRestrictionGraph ctx carrier prop boundary ->
  Int
carrierRestrictionProgramCount =
  sum . fmap length . Map.elems . crgProgramsBySource
{-# INLINE carrierRestrictionProgramCount #-}

restrictionProgramsFrom ::
  Ord (CarrierAddr ctx carrier prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierRestrictionGraph ctx carrier prop boundary ->
  [CompiledCarrierRestriction ctx carrier prop boundary]
restrictionProgramsFrom sourceAddress graph =
  Map.findWithDefault [] sourceAddress (crgProgramsBySource graph)
{-# INLINE restrictionProgramsFrom #-}

restrictionProgramsTo ::
  Ord (CarrierAddr ctx carrier prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierRestrictionGraph ctx carrier prop boundary ->
  [CompiledCarrierRestriction ctx carrier prop boundary]
restrictionProgramsTo targetAddress graph =
  Map.findWithDefault [] targetAddress (crgProgramsByTarget graph)
{-# INLINE restrictionProgramsTo #-}

lookupCompiledCarrierRestriction ::
  (Eq ctx, Eq carrier, Eq prop, Ord (CarrierAddr ctx carrier prop)) =>
  RestrictKey ctx carrier prop ->
  CarrierRestrictionGraph ctx carrier prop boundary ->
  Maybe (CompiledCarrierRestriction ctx carrier prop boundary)
lookupCompiledCarrierRestriction restrictKey graph =
  List.find
    ((== restrictKey) . ccrKey)
    (restrictionProgramsFrom (rkSource restrictKey) graph)
{-# INLINE lookupCompiledCarrierRestriction #-}

hasCompiledCarrierRestriction ::
  (Eq ctx, Eq carrier, Eq prop, Ord (CarrierAddr ctx carrier prop)) =>
  RestrictKey ctx carrier prop ->
  CarrierRestrictionGraph ctx carrier prop boundary ->
  Bool
hasCompiledCarrierRestriction restrictKey =
  isJust
    . lookupCompiledCarrierRestriction restrictKey
{-# INLINE hasCompiledCarrierRestriction #-}

restrictionProgramsBetweenFrom ::
  (Eq ctx, Ord (CarrierAddr ctx carrier prop)) =>
  ctx ->
  ctx ->
  CarrierAddr ctx carrier prop ->
  CarrierRestrictionGraph ctx carrier prop boundary ->
  [CompiledCarrierRestriction ctx carrier prop boundary]
restrictionProgramsBetweenFrom sourceContext targetContext sourceAddress graph =
  filter
    (restrictionProgramBetween sourceContext targetContext)
    (restrictionProgramsFrom sourceAddress graph)
{-# INLINE restrictionProgramsBetweenFrom #-}

restrictionProgramsBetweenTo ::
  (Eq ctx, Ord (CarrierAddr ctx carrier prop)) =>
  ctx ->
  ctx ->
  CarrierAddr ctx carrier prop ->
  CarrierRestrictionGraph ctx carrier prop boundary ->
  [CompiledCarrierRestriction ctx carrier prop boundary]
restrictionProgramsBetweenTo sourceContext targetContext targetAddress graph =
  filter
    (restrictionProgramBetween sourceContext targetContext)
    (restrictionProgramsTo targetAddress graph)
{-# INLINE restrictionProgramsBetweenTo #-}

restrictionProgramBetween ::
  Eq ctx =>
  ctx ->
  ctx ->
  CompiledCarrierRestriction ctx carrier prop boundary ->
  Bool
restrictionProgramBetween sourceContext targetContext program =
  let key =
        ccrKey program
   in caContext (rkSource key) == sourceContext
        && caContext (rkTarget key) == targetContext
{-# INLINE restrictionProgramBetween #-}

restrictCarrierDeltaBranches ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierRestrictionGraph ctx carrier prop boundary ->
  RelationalCarrierDelta ctx carrier prop boundary evidence ->
  ([CarrierRestrictionDiagnostic ctx carrier prop], [RelationalCarrierDelta ctx carrier prop boundary evidence])
restrictCarrierDeltaBranches graph sourceDelta =
  foldr
    collectBranch
    ([], [])
    (restrictionProgramsFrom (deAddr sourceDelta) graph)
  where
    collectBranch program (diagnostics, restrictedDeltas) =
      case restrictCarrierDelta program sourceDelta of
        Left restrictionError ->
          (restrictionDiagnostic program restrictionError : diagnostics, restrictedDeltas)
        Right restrictedDelta
          | plainRowPatchNull (deRows restrictedDelta) ->
              (diagnostics, restrictedDeltas)
          | otherwise ->
              (diagnostics, restrictedDelta : restrictedDeltas)
{-# INLINE restrictCarrierDeltaBranches #-}

compileCarrierRestrictionGraph ::
  (Ord ctx, Ord carrier, Ord prop, DenseKey classId) =>
  ContextLattice ctx ->
  ContextRank ctx ->
  [CarrierRestrictionEdgeSpec ctx carrier prop classId] ->
  Either
    (CarrierRestrictionInstallError ctx carrier prop classId)
    (CarrierRestrictionGraph ctx carrier prop RuntimeBoundary)
compileCarrierRestrictionGraph latticeValue rankOf specs = do
  validateRestrictionAcyclic (fmap cresEdge specs)
  compiled <-
    traverse
      (first CarrierRestrictionCompileFailed . compileCarrierRestrictionsForEdge latticeValue rankOf)
      specs
  pure (installCarrierRestrictionPrograms (concat compiled))
{-# INLINE compileCarrierRestrictionGraph #-}

validateRestrictionAcyclic ::
  Ord ctx =>
  [ContextRestrictionEdge ctx] ->
  Either (CarrierRestrictionInstallError ctx carrier prop classId) ()
validateRestrictionAcyclic edges =
  case findDirectedCycle adjacency of
    Nothing ->
      Right ()
    Just cycleEdges ->
      Left (CarrierRestrictionCycleDetected cycleEdges)
  where
    adjacency =
      List.foldl'
        ( \acc edge ->
            Map.insertWith
              Set.union
              (creSourceContext edge)
              (Set.singleton (creTargetContext edge))
              acc
        )
        Map.empty
        edges

findDirectedCycle ::
  Ord vertex =>
  Map vertex (Set vertex) ->
  Maybe [(vertex, vertex)]
findDirectedCycle adjacency =
  case AdjacencyMapAlgorithm.topSort (AdjacencyMap.fromAdjacencySets (Map.toAscList adjacency)) of
    Right _ ->
      Nothing
    Left cycleVertices ->
      Just (cycleEdges cycleVertices)

cycleEdges :: NonEmpty.NonEmpty vertex -> [(vertex, vertex)]
cycleEdges vertices =
  zip orderedVertices (drop 1 orderedVertices <> [NonEmpty.head vertices])
  where
    orderedVertices =
      NonEmpty.toList vertices

restrictionDiagnostic ::
  CompiledCarrierRestriction ctx carrier prop boundary ->
  RestrictionDeltaError ->
  CarrierRestrictionDiagnostic ctx carrier prop
restrictionDiagnostic program restrictionError =
  CarrierRestrictionDiagnostic
    { crdSource = rkSource (ccrKey program),
      crdTarget = rkTarget (ccrKey program),
      crdError = restrictionError
    }
{-# INLINE restrictionDiagnostic #-}
