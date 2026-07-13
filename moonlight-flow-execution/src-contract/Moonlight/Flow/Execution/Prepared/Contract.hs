{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Execution.Prepared.Contract
  ( SupportView (..),
    supportIds,
    supportExists,
    mkSupportView,
    PreparedProvenanceError (..),
    PreparedProvenanceRow (..),
    PreparedProvenanceRows (..),
    PreparedOp (..),
  )
where

import Data.Kind (Type)
import Moonlight.Core
  ( AtomId,
  )
import Moonlight.Differential.Row.Tuple
  ( AssignmentTupleKey,
    RowTupleKey,
  )
import Moonlight.Flow.Execution.Observe.Provenance.Types
  ( ProvArena,
    ProvVal,
  )
import Moonlight.Flow.Plan.Query.Core
  ( BagId,
  )
import Moonlight.Flow.Storage.View
  ( SupportIds,
    normalizeSupportIds,
    supportAllRelationsFeasible,
  )

type SupportView :: Type
newtype SupportView = SupportView
  { svRawIds :: SupportIds
  }

supportIds :: SupportView -> SupportIds
supportIds =
  normalizeSupportIds . svRawIds
{-# INLINE supportIds #-}

supportExists :: SupportView -> Bool
supportExists =
  supportAllRelationsFeasible . svRawIds
{-# INLINE supportExists #-}

mkSupportView :: SupportIds -> SupportView
mkSupportView rawIds =
  SupportView
    { svRawIds = rawIds
    }
{-# INLINE mkSupportView #-}

-- | A violated invariant while attaching the prepared factor cache's
-- provenance to an emitted assignment.  These cases are represented rather
-- than silently dropping an otherwise valid query result.
type PreparedProvenanceError :: Type
data PreparedProvenanceError
  = PreparedProvenanceRequiresFactorCache
  | PreparedProvenanceRowArityMismatch !Int !Int
  | PreparedProvenanceFactorMissing !BagId
  | PreparedProvenanceFactorCellMissing !BagId !AssignmentTupleKey
  deriving stock (Eq, Show)

type PreparedProvenanceRow :: Type
data PreparedProvenanceRow = PreparedProvenanceRow
  { pprTuple :: !RowTupleKey,
    -- | One value per decomposition bag. Their product is the provenance of
    -- the complete assignment; retaining the factors avoids manufacturing
    -- short-lived product nodes solely for observation.
    pprFactors :: ![ProvVal]
  }
  deriving stock (Eq, Show)

-- | Output assignments paired with values interpreted in the retained,
-- post-collection factor-cache arena.
type PreparedProvenanceRows :: Type
data PreparedProvenanceRows = PreparedProvenanceRows
  { pprsArena :: !ProvArena,
    pprsRows :: ![PreparedProvenanceRow]
  }
  deriving stock (Eq, Show)

type PreparedOp :: Type -> Type
data PreparedOp a where
  PreparedRows :: !(Maybe Int) -> PreparedOp [RowTupleKey]
  PreparedDeltaRows :: !(Maybe Int) -> PreparedOp [RowTupleKey]
  PreparedRowsWithProvenance ::
    !(Maybe Int) ->
    PreparedOp (Either PreparedProvenanceError PreparedProvenanceRows)
  PreparedSupport :: PreparedOp SupportView
  PreparedExists :: PreparedOp Bool
  PreparedExistsPinned :: !AtomId -> !RowTupleKey -> PreparedOp Bool
