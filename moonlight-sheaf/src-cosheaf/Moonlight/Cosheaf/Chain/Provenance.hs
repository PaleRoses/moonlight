{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Cosheaf.Chain.Provenance
  ( ProvenanceId (..),
    ProvenanceArena (..),
    emptyProvenanceArena,
    appendProvenance,
    lookupProvenance,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)

type ProvenanceId :: Type
newtype ProvenanceId = ProvenanceId
  { unProvenanceId :: Int
  }
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum)

-- | Compact, append-only provenance store. Boundaries keep ids; payloads live
-- here, so later Morse paths can form DAG provenance without smearing nested
-- lists through every sparse entry.
type ProvenanceArena :: Type -> Type
data ProvenanceArena provenance = ProvenanceArena
  { provenanceArenaNextKey :: !Int,
    provenanceArenaEntries :: !(IntMap provenance)
  }
  deriving stock (Eq, Show)

emptyProvenanceArena :: ProvenanceArena provenance
emptyProvenanceArena =
  ProvenanceArena
    { provenanceArenaNextKey = 0,
      provenanceArenaEntries = IntMap.empty
    }

appendProvenance ::
  provenance ->
  ProvenanceArena provenance ->
  (ProvenanceId, ProvenanceArena provenance)
appendProvenance provenanceValue arena =
  let keyValue = provenanceArenaNextKey arena
   in ( ProvenanceId keyValue,
        ProvenanceArena
          { provenanceArenaNextKey = keyValue + 1,
            provenanceArenaEntries =
              IntMap.insert keyValue provenanceValue (provenanceArenaEntries arena)
          }
      )

lookupProvenance ::
  ProvenanceId ->
  ProvenanceArena provenance ->
  Maybe provenance
lookupProvenance (ProvenanceId keyValue) =
  IntMap.lookup keyValue . provenanceArenaEntries
