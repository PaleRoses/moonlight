{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Runtime.Carrier.Emit.FactorPayload
  ( FactorCarrierPayload (..),
    factorPayloadRelationalScope,
    factorPayloadNode,
    factorPayloadCarrier,
    factorPayloadSchema,
    factorPayloadRows,
    factorDeltaMembershipRows,
    factorSnapshotMembershipRows,
  )
where

import Data.Map.Strict qualified as Map
import Moonlight.Delta.Patch qualified as CorePatch
import Moonlight.Core
  ( QueryId,
    SlotId,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
    queryFactorCarrier,
  )
import Moonlight.Flow.Execution.Factor.Core
  ( Factor,
  )
import Moonlight.Flow.Execution.Factor.Delta
  ( FactorDelta,
  )
import Moonlight.Flow.Execution.Observe.Provenance.Types
  ( ProvVal (..),
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Delta.Signed
  ( MultiplicityChange (..)
  )
import Moonlight.Differential.Row.Patch
  ( ShapedPatch (..),
    plainRowPatchFromList
  )
import Moonlight.Flow.Model.Scope
  ( RelationalScope,
  )
import Moonlight.Differential.Row.Tuple
  ( AssignmentTupleKey,
    RowTupleKey,
    coerceTupleKey,
  )
import Moonlight.Flow.Plan.Query.Core
  ( FactorNode,
  )
import Moonlight.Differential.Index.IndexedRows
  ( indexedRowsPayloadMap,
  )

data FactorCarrierPayload = FactorCarrierPayload
  { fcpRelationalScope :: !RelationalScope,
    fcpNode :: !FactorNode,
    fcpSchema :: ![SlotId],
    fcpRows :: !RowDelta
  }
  deriving stock (Eq, Show)

factorPayloadRelationalScope :: FactorCarrierPayload -> RelationalScope
factorPayloadRelationalScope =
  fcpRelationalScope
{-# INLINE factorPayloadRelationalScope #-}

factorPayloadNode :: FactorCarrierPayload -> FactorNode
factorPayloadNode =
  fcpNode
{-# INLINE factorPayloadNode #-}

factorPayloadCarrier :: QueryId -> FactorCarrierPayload -> Carrier
factorPayloadCarrier queryId payload =
  queryFactorCarrier queryId (fcpNode payload)
{-# INLINE factorPayloadCarrier #-}

factorPayloadSchema :: FactorCarrierPayload -> [SlotId]
factorPayloadSchema =
  fcpSchema
{-# INLINE factorPayloadSchema #-}

factorPayloadRows :: FactorCarrierPayload -> RowDelta
factorPayloadRows =
  fcpRows
{-# INLINE factorPayloadRows #-}

factorDeltaMembershipRows :: FactorDelta -> RowDelta
factorDeltaMembershipRows deltaValue =
  plainRowPatchFromList
    ( CorePatch.foldWithKey
        (\_assignmentKey rows -> rows)
        (\assignmentKey _newValue rows ->
           (assignmentKeyRow assignmentKey, MultiplicityChange 1) : rows)
        (\assignmentKey _oldValue rows ->
           (assignmentKeyRow assignmentKey, MultiplicityChange (-1)) : rows)
        (\_assignmentKey _oldValue _newValue rows -> rows)
        []
        (spdDelta deltaValue)
    )
{-# INLINE factorDeltaMembershipRows #-}

factorSnapshotMembershipRows :: Factor -> RowDelta
factorSnapshotMembershipRows factorValue =
  plainRowPatchFromList
    ( Map.foldrWithKey
        snapshotMembershipStep
        []
        (indexedRowsPayloadMap factorValue)
    )
{-# INLINE factorSnapshotMembershipRows #-}

snapshotMembershipStep ::
  AssignmentTupleKey ->
  ProvVal ->
  [(RowTupleKey, MultiplicityChange)] ->
  [(RowTupleKey, MultiplicityChange)]
snapshotMembershipStep assignmentKey _provenance rows =
  (assignmentKeyRow assignmentKey, MultiplicityChange 1) : rows
{-# INLINE snapshotMembershipStep #-}

assignmentKeyRow :: AssignmentTupleKey -> RowTupleKey
assignmentKeyRow =
  coerceTupleKey
{-# INLINE assignmentKeyRow #-}
