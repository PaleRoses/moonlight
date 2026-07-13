{-# LANGUAGE DataKinds #-}

module Moonlight.Flow.Execution.Subsumption.Proof
  ( AtomEmbedding (..),
    AtomEmbeddingError (..),
    CQContainmentWitness (..),
    ContainmentAtomWitness (..),
    AtomEmbeddingProof,
    BoundaryProjectionProof (..),
    ResidualImplicationProof (..),
    ContainmentProof (..),
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Moonlight.Core
  ( SlotId,
  )
import Moonlight.Flow.Model.Schema.Morphism
  ( SchemaProjection,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )
import Moonlight.Flow.Plan.Shape
  ( CanonAtom,
    CanonAtomMultiset,
  )
import Moonlight.Flow.Plan.Shape.Term
  ( CanonSlot,
    PlanShape,
    PlanStage (..),
  )
import Moonlight.Flow.Plan.Residual
  ( ResidualImplicationProof (..),
  )

data AtomEmbedding = AtomEmbedding
  { aeRequiredAtoms :: !CanonAtomMultiset,
    aeSourceRemainder :: !CanonAtomMultiset,
    aeDigest :: !StableDigest128
  }
  deriving stock (Eq, Ord, Show, Read)

data AtomEmbeddingError
  = AtomEmbeddingNonPositiveTargetMultiplicity !CanonAtom !Int
  | AtomEmbeddingNegativeSourceMultiplicity !CanonAtom !Int
  | AtomEmbeddingMissingSourceMultiplicity !CanonAtom !Int !Int
  deriving stock (Eq, Ord, Show, Read)

data CQContainmentWitness = CQContainmentWitness
  { cqwSlotMap :: !(IntMap CanonSlot),
    cqwAtomImages :: ![(CanonAtom, CanonAtom)],
    cqwDigest :: !StableDigest128
  }
  deriving stock (Eq, Ord, Show, Read)

data ContainmentAtomWitness
  = StructuralAtomEmbedding !AtomEmbedding
  | CQHomomorphism !CQContainmentWitness
  deriving stock (Eq, Ord, Show, Read)

type AtomEmbeddingProof =
  ContainmentAtomWitness

data BoundaryProjectionProof = BoundaryProjectionProof
  { bppSourceBoundaryDigest :: !StableDigest128,
    bppRequestedBoundaryDigest :: !StableDigest128,
    bppProjectionDigest :: !StableDigest128,
    bppExact :: !Bool,
    bppDigest :: !StableDigest128
  }
  deriving stock (Eq, Ord, Show, Read)

data ContainmentProof = ContainmentProof
  { cpSourceShape :: !(PlanShape 'FactorShape),
    cpRequestedShape :: !(PlanShape 'FactorShape),
    cpSlotProjection :: !(SchemaProjection SlotId CanonSlot),
    cpAtomEmbedding :: !AtomEmbeddingProof,
    cpResidualProof :: !ResidualImplicationProof,
    cpBoundaryProof :: !BoundaryProjectionProof,
    cpProjectionDigest :: !StableDigest128
  }
  deriving stock (Eq, Ord, Show, Read)
