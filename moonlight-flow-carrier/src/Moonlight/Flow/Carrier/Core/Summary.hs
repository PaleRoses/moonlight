{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Carrier.Core.Summary
  ( CarrierBatchSummaryOps (..),
    CarrierBatchSummary (..),
    CarrierStoreSummaryEntry (..),
    carrierBatchSummary,
  )
where

import Data.Kind
  ( Type,
  )
import Data.List.NonEmpty
  ( NonEmpty,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Origin
  ( RelationalOrigin,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
  )

type CarrierBatchSummaryOps :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data CarrierBatchSummaryOps ctx carrier prop boundary evidence batch = CarrierBatchSummaryOps
  { cbsoSummaryBoundary ::
      CarrierAddr ctx carrier prop ->
      NonEmpty batch ->
      boundary,
    cbsoSummaryEvidence ::
      CarrierAddr ctx carrier prop ->
      NonEmpty batch ->
      evidence,
    cbsoSummaryOrigin ::
      CarrierAddr ctx carrier prop ->
      NonEmpty batch ->
      RelationalOrigin ctx carrier prop
  }

type CarrierBatchSummary :: Type -> Type -> Type -> Type -> Type -> Type
data CarrierBatchSummary ctx carrier prop boundary evidence = CarrierBatchSummary
  { cbsBoundary :: !boundary,
    cbsEvidence :: !evidence,
    cbsOrigin :: !(RelationalOrigin ctx carrier prop)
  }
  deriving stock (Eq, Show)

type CarrierStoreSummaryEntry :: Type -> Type -> Type -> Type -> Type -> Type
data CarrierStoreSummaryEntry ctx carrier prop boundary evidence = CarrierStoreSummaryEntry
  { csseAddr :: !(CarrierAddr ctx carrier prop),
    csseTime :: !(RelationalCarrierTime ctx),
    csseBoundary :: !boundary,
    csseEvidence :: !evidence,
    csseOrigin :: !(RelationalOrigin ctx carrier prop)
  }
  deriving stock (Eq, Show)

carrierBatchSummary ::
  CarrierBatchSummaryOps ctx carrier prop boundary evidence batch ->
  CarrierAddr ctx carrier prop ->
  NonEmpty batch ->
  CarrierBatchSummary ctx carrier prop boundary evidence
carrierBatchSummary ops addr entries =
  CarrierBatchSummary
    { cbsBoundary = cbsoSummaryBoundary ops addr entries,
      cbsEvidence = cbsoSummaryEvidence ops addr entries,
      cbsOrigin = cbsoSummaryOrigin ops addr entries
    }
{-# INLINE carrierBatchSummary #-}
