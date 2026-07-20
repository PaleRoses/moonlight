{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Plan.Rewrite.Internal.Types
  ( PlanAnalysis (..),
    PlanReuseShapeKey (..),
    planReuseShapeKeyWords,
    FactorShapeNormalizationProof (..),
    fsnpRepresentativeDigest,
    fsnpRewriteSystemDigest,
    FactorShapeNormalization (..),
    fsnInputDigest,
    fsnClassId,
    fsnKey,
    fsnRepresentativeDigest,
    PlanSaturationState (..),
    PlanSaturationError (..),
    PlanEGraphResult (..),
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.Kind (Type)
import Data.List qualified as List
import Data.Set (Set)
import Data.Word (Word64)
import Moonlight.Core (UnionFindAllocationError, classIdKey)
import Moonlight.EGraph.Pure.Types
  ( EGraph,
    ENode (..),
    canonicalizeClassId,
    eGraphAnalysis,
    eGraphClassCount,
    eGraphNodeCount,
    eGraphStore,
  )
import Moonlight.EGraph.Pure.Structural.Store
  ( structuralEntries,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128 (..),
    stableDigestWords,
  )
import Moonlight.Flow.Plan.Reuse.Factor.Signature
  ( PlanFactorRootSignature,
  )
import Moonlight.Flow.Plan.Rewrite.Node
  ( PlanClassId (..),
    PlanNode,
  )
import Moonlight.Flow.Plan.Rewrite.Proof
  ( PlanEquivalenceProof,
    PlanRewriteSystem,
  )
import Moonlight.Flow.Plan.Shape.Build qualified as ShapeBuild
import Moonlight.Flow.Plan.Shape.CanonicalKey qualified as Canonical
import Moonlight.Flow.Plan.Shape.Term
  ( PlanShape (..),
    PlanStage (..),
  )

type PlanAnalysis :: Type
data PlanAnalysis = PlanAnalysis
  { paCanonicalCandidate :: !(Maybe (PlanShape 'Canonical)),
    paKnownShapeDigests :: !(Set StableDigest128),
    paRootShapeDigests :: !(Set StableDigest128),
    paFactorRootSignatures :: !(Set PlanFactorRootSignature)
  }
  deriving stock (Eq, Ord, Show, Read)

type PlanReuseShapeKey :: Type
data PlanReuseShapeKey = PlanReuseShapeKey
  { prskRewriteSystemDigest :: !StableDigest128,
    prskRepresentativeDigest :: !StableDigest128
  }
  deriving stock (Eq, Ord, Show, Read)

planReuseShapeKeyWords :: PlanReuseShapeKey -> [Word64]
planReuseShapeKeyWords key =
  [0x706c616e52657573, 0x654b6579]
    <> stableDigestWords (prskRewriteSystemDigest key)
    <> stableDigestWords (prskRepresentativeDigest key)
{-# INLINE planReuseShapeKeyWords #-}

type FactorShapeNormalizationProof :: Type
data FactorShapeNormalizationProof = FactorShapeNormalizationProof
  { fsnpSourceDigest :: !StableDigest128,
    fsnpKey :: !PlanReuseShapeKey,
    fsnpClassId :: !(PlanClassId (PlanShape 'FactorShape)),
    fsnpStepDigest :: !StableDigest128,
    fsnpDigest :: !StableDigest128
  }
  deriving stock (Eq, Ord, Show, Read)

fsnpRepresentativeDigest :: FactorShapeNormalizationProof -> StableDigest128
fsnpRepresentativeDigest =
  prskRepresentativeDigest . fsnpKey
{-# INLINE fsnpRepresentativeDigest #-}

fsnpRewriteSystemDigest :: FactorShapeNormalizationProof -> StableDigest128
fsnpRewriteSystemDigest =
  prskRewriteSystemDigest . fsnpKey
{-# INLINE fsnpRewriteSystemDigest #-}

type FactorShapeNormalization :: Type
data FactorShapeNormalization = FactorShapeNormalization
  { fsnProof :: !FactorShapeNormalizationProof
  }
  deriving stock (Eq, Ord, Show, Read)

fsnInputDigest :: FactorShapeNormalization -> StableDigest128
fsnInputDigest =
  fsnpSourceDigest . fsnProof
{-# INLINE fsnInputDigest #-}

fsnClassId :: FactorShapeNormalization -> PlanClassId (PlanShape 'FactorShape)
fsnClassId =
  fsnpClassId . fsnProof
{-# INLINE fsnClassId #-}

fsnKey :: FactorShapeNormalization -> PlanReuseShapeKey
fsnKey =
  fsnpKey . fsnProof
{-# INLINE fsnKey #-}

fsnRepresentativeDigest :: FactorShapeNormalization -> StableDigest128
fsnRepresentativeDigest =
  fsnpRepresentativeDigest . fsnProof
{-# INLINE fsnRepresentativeDigest #-}

type PlanSaturationState :: Type
data PlanSaturationState = PlanSaturationState
  { pssGraph :: !(EGraph PlanNode PlanAnalysis),
    pssDirtyClassKeys :: !IntSet,
    pssCanonicalizationMemo :: !Canonical.PlanCanonicalizationMemo
  }

instance Eq PlanSaturationState where
  left == right =
    planSaturationStateKey left == planSaturationStateKey right

instance Show PlanSaturationState where
  showsPrec precedence state =
    showParen (precedence > 10) $
      showString "PlanSaturationState "
        . showsPrec 11 (planSaturationStateKey state)

type PlanSaturationStateKey :: Type
data PlanSaturationStateKey = PlanSaturationStateKey
  { psskNodeCount :: !Int,
    psskClassCount :: !Int,
    psskDirtyClassKeys :: !IntSet,
    psskCanonicalEntries :: ![(Int, PlanNode Int)],
    psskCanonicalAnalysis :: ![(Int, PlanAnalysis)],
    psskCanonicalizationMemo :: !Canonical.PlanCanonicalizationMemo
  }
  deriving stock (Eq, Show)

planSaturationStateKey :: PlanSaturationState -> PlanSaturationStateKey
planSaturationStateKey state =
  PlanSaturationStateKey
    { psskNodeCount = eGraphNodeCount graph,
      psskClassCount = eGraphClassCount graph,
      psskDirtyClassKeys = pssDirtyClassKeys state,
      psskCanonicalEntries = canonicalDatabaseEntries graph,
      psskCanonicalAnalysis = IntMap.toAscList (eGraphAnalysis graph),
      psskCanonicalizationMemo = pssCanonicalizationMemo state
    }
  where
    graph = pssGraph state
{-# INLINE planSaturationStateKey #-}

canonicalDatabaseEntries ::
  EGraph PlanNode PlanAnalysis ->
  [(Int, PlanNode Int)]
canonicalDatabaseEntries graph =
  List.sort
    [ ( classIdKey (canonicalizeClassId graph classId),
        fmap (classIdKey . canonicalizeClassId graph) node
      )
    | (classId, ENode node) <- structuralEntries (eGraphStore graph)
  ]
{-# INLINE canonicalDatabaseEntries #-}

data PlanSaturationError
  = PlanSaturationClassIdAllocationFailed !UnionFindAllocationError
  | PlanSaturationCanonicalizationError !Canonical.PlanCanonicalizationError
  | PlanSaturationNoCanonicalCandidate !(PlanClassId (PlanShape 'RawLogical))
  | PlanSaturationNoFactorRepresentative !(PlanClassId (PlanShape 'FactorShape))
  | PlanSaturationProjectionShapeError !ShapeBuild.ProjectionShapeError
  | PlanSaturationIterationLimit !Int
  | PlanSaturationNodeLimit !Int
  deriving stock (Eq, Ord, Show)


data PlanEGraphResult = PlanEGraphResult
  { pegrRewriteSystem :: !PlanRewriteSystem,
    pegrLogicalClass :: !(PlanClassId (PlanShape 'RawLogical)),
    pegrCanonicalClass :: !(PlanClassId (PlanShape 'Canonical)),
    pegrCanonicalPlanShape :: !(PlanShape 'Canonical),
    pegrCanonicalProof :: !PlanEquivalenceProof,
    pegrState :: !PlanSaturationState
  }

instance Eq PlanEGraphResult where
  left == right =
    pegrRewriteSystem left == pegrRewriteSystem right
      && pegrLogicalClass left == pegrLogicalClass right
      && pegrCanonicalClass left == pegrCanonicalClass right
      && pegrCanonicalPlanShape left == pegrCanonicalPlanShape right
      && pegrCanonicalProof left == pegrCanonicalProof right
      && pegrState left == pegrState right

instance Show PlanEGraphResult where
  showsPrec precedence result =
    showParen (precedence > 10) $
      showString "PlanEGraphResult "
        . showsPrec 11 (pegrRewriteSystem result)
        . showChar ' '
        . showsPrec 11 (pegrLogicalClass result)
        . showChar ' '
        . showsPrec 11 (pegrCanonicalClass result)
        . showChar ' '
        . showsPrec 11 (pegrCanonicalPlanShape result)
        . showChar ' '
        . showsPrec 11 (pegrCanonicalProof result)
        . showChar ' '
        . showsPrec 11 (pegrState result)
