{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Runtime.Backend
  ( RuntimeBackend (..),
    RuntimeBackendError (..),
    defaultBackend,
    sheafBackend,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.Kind
  ( Type,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty
  ( NonEmpty (..),
  )
import Data.Map.Strict qualified as Map
import Data.Ord
  ( comparing,
  )
import Data.Void
  ( Void,
  )
import Moonlight.FiniteLattice qualified as FiniteLattice
import Moonlight.Core
  ( AtomId,
    QueryId,
    atomIdKey,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
  )
import Moonlight.Flow.Carrier.Core.Summary
  ( CarrierBatchSummaryOps (..),
    CarrierStoreSummaryEntry (..),
  )
import Moonlight.Flow.Carrier.Core.Origin
  ( OriginEvent (OriginCompacted),
    originAddParent,
    originMerge,
  )
import Moonlight.Flow.Carrier.Engine.Project
  ( carrierProjectOp,
  )
import Moonlight.Flow.Carrier.Morphism.Engine
  ( carrierMorphismOp,
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
    RuntimeBoundaryError,
    mkRuntimeBoundary,
  )
import Moonlight.Flow.Plan.Query.Core
  ( FactorNode,
    SlotId,
  )
import Moonlight.Flow.Plan.Residual
  ( ResidualTheoryRegistry,
    emptyResidualTheoryRegistry,
  )
import Moonlight.Flow.Runtime.Kernel.Operators
  ( RuntimeCarrierOperators (..),
  )
import Moonlight.Flow.Runtime.Core.Patch.Validation
  ( CanonicalityOracle (..),
  )
import Moonlight.Flow.Carrier.Reuse.Config
  ( ReuseMode (ExactOnly),
  )
import Moonlight.Flow.Runtime.Spec.Schema
  ( RuntimeAtomSchema (..),
    RuntimeContextSchema (..),
    RuntimeSchema (..),
  )
import Moonlight.Flow.Runtime.Carrier.Emit
  ( AtomCarrierPayload,
  )
import Moonlight.Flow.Runtime.Carrier.Emit
  ( FactorCarrierPayload,
  )
import Moonlight.FiniteLattice
  ( ContextLattice,
    ContextLatticeCompileError,
    compileContextLattice
  )

data RuntimeBackendError ctx prop
  = RuntimeBackendNoContexts
  | RuntimeBackendContextOrderRequired !Int
  | RuntimeBackendContextLatticeInvalid !(ContextLatticeCompileError ctx)
  | RuntimeBackendAtomBoundaryInvalid !AtomId !RuntimeBoundaryError
  | RuntimeBackendFactorBoundaryInvalid !QueryId !FactorNode !RuntimeBoundaryError
  deriving stock (Eq, Ord, Show, Read)

type RuntimeBackend :: Type -> Type -> Type -> Type -> Type -> Type
data RuntimeBackend ctx prop evidence joinState joinErr = RuntimeBackend
  { rbContextLattice ::
      RuntimeSchema ctx prop ->
      Either (RuntimeBackendError ctx prop) (ContextLattice ctx),
    rbCanonicalityOracle ::
      RuntimeSchema ctx prop ->
      CanonicalityOracle RowTupleKey,
    rbAtomBoundary ::
      RuntimeAtomSchema ->
      Either RuntimeBoundaryError RuntimeBoundary,
    rbFactorBoundary ::
      QueryId ->
      FactorNode ->
      [SlotId] ->
      Either RuntimeBoundaryError RuntimeBoundary,
    rbDefaultEvidence :: !evidence,
    rbAtomEvidence ::
      AtomCarrierPayload ->
      evidence,
    rbFactorEvidence ::
      QueryId ->
      FactorCarrierPayload ->
      evidence,
    rbCarrierOperators ::
      !(RuntimeCarrierOperators ctx prop RuntimeBoundary evidence),
    rbCarrierSummaryOps ::
      !( CarrierBatchSummaryOps
           ctx
           Carrier
           prop
           RuntimeBoundary
           evidence
           (CarrierStoreSummaryEntry ctx Carrier prop RuntimeBoundary evidence)
       ),
    rbReuseMode :: !ReuseMode,
    rbResidualTheoryRegistry :: !ResidualTheoryRegistry
  }

defaultBackend ::
  (Ord ctx, Ord prop) =>
  RuntimeBackend ctx prop () () Void
defaultBackend =
  defaultBackendBase
    { rbContextLattice =
        defaultContextLattice
    }
{-# INLINE defaultBackend #-}

defaultContextLattice ::
  Ord ctx =>
  RuntimeSchema ctx prop ->
  Either (RuntimeBackendError ctx prop) (ContextLattice ctx)
defaultContextLattice schemaValue =
  case rscContextOrder schemaValue of
    Just decl ->
      first RuntimeBackendContextLatticeInvalid $
        compileContextLattice
          (Map.keysSet (rscContexts schemaValue))
          decl
    Nothing ->
      singletonContextLattice (rscContexts schemaValue)
{-# INLINE defaultContextLattice #-}

sheafBackend ::
  (Ord ctx, Ord prop) =>
  ContextLattice ctx ->
  RuntimeBackend ctx prop () () Void
sheafBackend lattice =
  defaultBackendBase
    { rbContextLattice =
        \_schema -> Right lattice
    }
{-# INLINE sheafBackend #-}

defaultBackendBase ::
  (Ord ctx, Ord prop) =>
  RuntimeBackend ctx prop () () Void
defaultBackendBase =
  RuntimeBackend
    { rbContextLattice =
        \_schema -> Left RuntimeBackendNoContexts,
      rbCanonicalityOracle =
        defaultCanonicalityOracle,
      rbAtomBoundary =
        defaultAtomBoundary,
      rbFactorBoundary =
        defaultFactorBoundary,
      rbDefaultEvidence =
        (),
      rbAtomEvidence =
        const (),
      rbFactorEvidence =
        \_queryId _payload -> (),
      rbCarrierOperators =
        RuntimeCarrierOperators
          { rcoProjectOperator =
              carrierProjectOp,
            rcoRestrictOperator =
              carrierMorphismOp
          },
      rbCarrierSummaryOps =
        unitCarrierSummaryOps,
      rbReuseMode =
        ExactOnly,
      rbResidualTheoryRegistry =
        emptyResidualTheoryRegistry
    }
{-# INLINE defaultBackendBase #-}

singletonContextLattice ::
  Map.Map ctx a ->
  Either (RuntimeBackendError ctx prop) (ContextLattice ctx)
singletonContextLattice contexts =
  case Map.keys contexts of
    [] ->
      Left RuntimeBackendNoContexts
    [contextValue] ->
      Right (FiniteLattice.singletonContextLattice contextValue)
    _ ->
      Left
        ( RuntimeBackendContextOrderRequired
            (Map.size contexts)
        )
{-# INLINE singletonContextLattice #-}

defaultCanonicalityOracle ::
  RuntimeSchema ctx prop ->
  CanonicalityOracle RowTupleKey
defaultCanonicalityOracle schema =
  CanonicalityOracle
    { isCanonicalRowAt = \_epoch _row -> True,
      canonicalizeRowAt = \_epoch row -> row,
      expectedRowWidthAt =
        \_epoch atomId ->
          IntMap.lookup (atomIdKey atomId) atomWidths,
      dirtyKeysOfRowAt = \_epoch _row -> IntSet.empty,
      dirtyTopoForDirtyKey =
        IntSet.singleton,
      dirtyTopoForAtom =
        IntSet.singleton . atomIdKey
    }
  where
    atomWidths =
      schemaAtomWidths schema
{-# INLINE defaultCanonicalityOracle #-}

schemaAtomWidths :: RuntimeSchema ctx prop -> IntMap Int
schemaAtomWidths schema =
  IntMap.fromListWith
    max
    [ (atomIdKey atomId, length (rasColumns atomSchema))
    | contextSchema <- Map.elems (rscContexts schema),
      (atomId, atomSchema) <- Map.toAscList (rcsAtoms contextSchema)
    ]
{-# INLINE schemaAtomWidths #-}

defaultAtomBoundary :: RuntimeAtomSchema -> Either RuntimeBoundaryError RuntimeBoundary
defaultAtomBoundary atomSchema =
  mkRuntimeBoundary
    (rasColumns atomSchema)
    (rasBoundarySensitiveSlots atomSchema)
    (rasBoundarySlotKeys atomSchema)
{-# INLINE defaultAtomBoundary #-}

defaultFactorBoundary :: QueryId -> FactorNode -> [SlotId] -> Either RuntimeBoundaryError RuntimeBoundary
defaultFactorBoundary _queryId _node schema =
  mkRuntimeBoundary schema IntSet.empty IntMap.empty
{-# INLINE defaultFactorBoundary #-}

unitCarrierSummaryOps ::
  (Ord ctx, Ord prop) =>
  CarrierBatchSummaryOps
    ctx
    Carrier
    prop
    RuntimeBoundary
    ()
    (CarrierStoreSummaryEntry ctx Carrier prop RuntimeBoundary ())
unitCarrierSummaryOps =
  CarrierBatchSummaryOps
    { cbsoSummaryBoundary =
        \_addr entries ->
          csseBoundary (latestSummaryEntry entries),
      cbsoSummaryEvidence =
        \_addr _entries -> (),
      cbsoSummaryOrigin =
        \addr entries ->
          originAddParent
            addr
            (originMerge OriginCompacted (fmap csseOrigin entries))
    }
{-# INLINE unitCarrierSummaryOps #-}

latestSummaryEntry ::
  Ord ctx =>
  NonEmpty (CarrierStoreSummaryEntry ctx carrier prop boundary evidence) ->
  CarrierStoreSummaryEntry ctx carrier prop boundary evidence
latestSummaryEntry =
  Foldable.maximumBy (comparing csseTime)
{-# INLINE latestSummaryEntry #-}
