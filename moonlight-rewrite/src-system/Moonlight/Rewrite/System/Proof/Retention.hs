{-# LANGUAGE DerivingStrategies #-}

-- | Proof-retention policy vocabulary for system and proof surfaces.
-- Owns full, summary, recent, and absent retention modes, query
-- unavailability reasons, and the standalone default.
-- Contract: standalone proof recording defaults to full retention; relational
-- saturation chooses its throughput-oriented default separately.
module Moonlight.Rewrite.System.Proof.Retention
  ( ProofRetention (..),
    defaultProofRetention,
    ProofQueryError (..),
    proofRetentionStoresFullLog,
    proofRetentionStoresAnyLog,
  )
where

import Data.Kind (Type)
import Numeric.Natural
  ( Natural,
  )

type ProofRetention :: Type
data ProofRetention
  = KeepNoProof
  | KeepProofSummary
  | KeepRecentProofSteps !Natural
  | KeepFullProof
  deriving stock (Eq, Ord, Show, Read)

-- | Standalone proof recording keeps everything by default; callers opting
-- into proofs usually want the full derivation. This deliberately diverges
-- from @defaultSaturationConfig@ in the relational front, whose
-- @scProofRetention@ defaults to 'KeepNoProof' because saturation runs are
-- throughput-bound and proofs there are opt-in.
defaultProofRetention :: ProofRetention
defaultProofRetention =
  KeepFullProof

type ProofQueryError :: Type
data ProofQueryError
  = ProofNotRecorded
  | ProofPruned
  | ProofUnavailableForRetention !ProofRetention
  deriving stock (Eq, Ord, Show, Read)

proofRetentionStoresFullLog :: ProofRetention -> Bool
proofRetentionStoresFullLog retention =
  case retention of
    KeepFullProof ->
      True
    KeepNoProof ->
      False
    KeepProofSummary ->
      False
    KeepRecentProofSteps _ ->
      False

proofRetentionStoresAnyLog :: ProofRetention -> Bool
proofRetentionStoresAnyLog retention =
  case retention of
    KeepFullProof ->
      True
    KeepRecentProofSteps retained ->
      retained > 0
    KeepNoProof ->
      False
    KeepProofSummary ->
      False
