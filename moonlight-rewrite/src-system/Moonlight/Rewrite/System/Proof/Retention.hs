{-# LANGUAGE DerivingStrategies #-}

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

-- | Standalone registries retain full proofs.
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
