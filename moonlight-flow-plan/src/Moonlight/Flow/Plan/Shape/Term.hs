{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Plan.Shape.Term
  ( RawSlot (..),
    rawSlotKey,

    CanonSlot (..),
    canonSlotKey,

    CanonSlotSource (..),
    CanonStalkRecipe (..),

    ResidualShape (..),

    RawAtomTerm (..),
    LogicalPlanTerm (..),

    PlanStage (..),
    PlanShape (..),
    PlanShapePayload,
    FragmentPayload (..),
    ProjectionPayload (..),
    RestrictionPayload (..),
    CoverPayload (..),
    CoverageTransformPayload (..),

    queryPlanToLogicalPlanTerm,
    queryPlanToLogicalPlanTermWithOutputs,
    queryPlanToOutputErasedLogicalPlanTerm,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntSet
  ( IntSet,
  )
import Data.Kind
  ( Type,
  )
import Data.Set
  ( Set,
  )
import Data.Vector qualified as Vector
import Data.Word
  ( Word64,
  )
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )
import Moonlight.Flow.Plan.Residual
  ( ResidualShape (..),
    queryPlanResidualShape,
  )

type RawSlot :: Type
newtype RawSlot = RawSlot
  { unRawSlot :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

rawSlotKey :: RawSlot -> Int
rawSlotKey =
  unRawSlot
{-# INLINE rawSlotKey #-}

type CanonSlot :: Type
newtype CanonSlot = CanonSlot
  { unCanonSlot :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

canonSlotKey :: CanonSlot -> Int
canonSlotKey =
  unCanonSlot
{-# INLINE canonSlotKey #-}

type CanonSlotSource :: Type
data CanonSlotSource
  = CanonSourceResult
  | CanonSourceChild {-# UNPACK #-} !Int
  deriving stock (Eq, Ord, Show, Read)

type CanonStalkRecipe :: Type
newtype CanonStalkRecipe = CanonStalkRecipe
  { csrColumns :: [[CanonSlotSource]]
  }
  deriving stock (Eq, Ord, Show, Read)

type RawAtomTerm :: Type
data RawAtomTerm = RawAtomTerm
  { ratRawAtomKey :: {-# UNPACK #-} !Int,
    ratTagDigest :: {-# UNPACK #-} !Word64,
    ratColumns :: ![RawSlot],
    ratRecipe :: !CanonStalkRecipe
  }
  deriving stock (Eq, Ord, Show, Read)

type LogicalPlanTerm :: Type
data LogicalPlanTerm = LogicalPlanTerm
  { lptDomain :: !QueryPlanDomain,
    lptAtoms :: ![RawAtomTerm],
    lptRoot :: !RawSlot,
    lptOutputs :: ![RawSlot],
    lptResidual :: !ResidualShape
  }
  deriving stock (Eq, Ord, Show, Read)

type PlanStage :: Type
data PlanStage
  = RawLogical
  | Canonical
  | FactorShape
  | Fragment
  | Projection
  | Restriction
  | Cover
  | CoverageTransform

type PlanShapePayload :: PlanStage -> Type
type family PlanShapePayload stage

type PlanShape :: PlanStage -> Type
data PlanShape stage = PlanShape
  { psDigest :: !StableDigest128,
    psPayload :: !(PlanShapePayload stage)
  }

deriving stock instance Eq (PlanShapePayload stage) => Eq (PlanShape stage)
deriving stock instance Ord (PlanShapePayload stage) => Ord (PlanShape stage)
deriving stock instance Show (PlanShapePayload stage) => Show (PlanShape stage)
deriving stock instance Read (PlanShapePayload stage) => Read (PlanShape stage)

type FragmentPayload :: Type
data FragmentPayload
  = RootFragmentPayload !StableDigest128
  | BagFragmentPayload !StableDigest128
  | SeparatorFragmentPayload
      !StableDigest128
      !StableDigest128
      !StableDigest128
  deriving stock (Eq, Ord, Show, Read)

type ProjectionPayload :: Type
data ProjectionPayload = ProjectionPayload
  { ppSourceShape :: !StableDigest128,
    ppTargetShape :: !StableDigest128,
    ppSourceSchema :: ![CanonSlot],
    ppTargetSchema :: ![CanonSlot],
    ppSlotMap :: !(IntMap CanonSlot)
  }
  deriving stock (Eq, Ord, Show, Read)

type RestrictionPayload :: Type
data RestrictionPayload = RestrictionPayload
  { rpSourceShape :: !StableDigest128,
    rpTargetShape :: !StableDigest128,
    rpPinnedSlots :: !(IntMap IntSet)
  }
  deriving stock (Eq, Ord, Show, Read)

type CoverPayload :: Type
data CoverPayload = CoverPayload
  { cpFamilyDigest :: !StableDigest128,
    cpTargetShape :: !StableDigest128,
    cpMembers :: !(Set StableDigest128)
  }
  deriving stock (Eq, Ord, Show, Read)

type CoverageTransformPayload :: Type
data CoverageTransformPayload
  = CoveragePreserveExact
  | CoverageDowngradeLowerBound
  | CoverageExactByCover !StableDigest128
  | CoverageObstructedBy !StableDigest128
  deriving stock (Eq, Ord, Show, Read)

type instance PlanShapePayload 'RawLogical = LogicalPlanTerm
type instance PlanShapePayload 'Fragment = FragmentPayload
type instance PlanShapePayload 'Projection = ProjectionPayload
type instance PlanShapePayload 'Restriction = RestrictionPayload
type instance PlanShapePayload 'Cover = CoverPayload
type instance PlanShapePayload 'CoverageTransform = CoverageTransformPayload

queryPlanToLogicalPlanTerm ::
  QueryPlan compiled output guard tag tuple key ->
  LogicalPlanTerm
queryPlanToLogicalPlanTerm plan =
  queryPlanToLogicalPlanTermWithOutputs
    (Vector.toList (qpOutputSlots plan))
    plan
{-# INLINE queryPlanToLogicalPlanTerm #-}

queryPlanToOutputErasedLogicalPlanTerm ::
  QueryPlan compiled output guard tag tuple key ->
  LogicalPlanTerm
queryPlanToOutputErasedLogicalPlanTerm =
  queryPlanToLogicalPlanTermWithOutputs []
{-# INLINE queryPlanToOutputErasedLogicalPlanTerm #-}

queryPlanToLogicalPlanTermWithOutputs ::
  [SlotId] ->
  QueryPlan compiled output guard tag tuple key ->
  LogicalPlanTerm
queryPlanToLogicalPlanTermWithOutputs outputSlots plan =
  LogicalPlanTerm
    { lptDomain = qpDomain plan,
      lptAtoms = fmap atomSpecToRawTerm (Vector.toList (qpAtoms plan)),
      lptRoot = rawSlotFromSlotId (qpRootSlot plan),
      lptOutputs = fmap rawSlotFromSlotId outputSlots,
      lptResidual = queryPlanResidualShape (qpResidual plan)
    }
{-# INLINE queryPlanToLogicalPlanTermWithOutputs #-}

atomSpecToRawTerm ::
  AtomSpec tag tuple key ->
  RawAtomTerm
atomSpecToRawTerm atomSpec =
  RawAtomTerm
    { ratRawAtomKey = queryAtomKey (asQueryAtomId atomSpec),
      ratTagDigest = asTagDigest atomSpec,
      ratColumns = fmap rawSlotFromSlotId (Vector.toList (asColumns atomSpec)),
      ratRecipe = canonicalizeStalkRecipe (asStalkRecipe atomSpec)
    }
{-# INLINE atomSpecToRawTerm #-}

rawSlotFromSlotId :: SlotId -> RawSlot
rawSlotFromSlotId =
  RawSlot . slotIdKey
{-# INLINE rawSlotFromSlotId #-}

canonicalizeStalkRecipe :: StalkRecipe -> CanonStalkRecipe
canonicalizeStalkRecipe recipe =
  CanonStalkRecipe
    { csrColumns =
        fmap
          (fmap canonicalizeSlotSource)
          (Vector.toList (stalkRecipeColumns recipe))
    }
{-# INLINE canonicalizeStalkRecipe #-}

canonicalizeSlotSource :: SlotSource -> CanonSlotSource
canonicalizeSlotSource source =
  case source of
    SourceResult ->
      CanonSourceResult
    SourceChild childIndex ->
      CanonSourceChild childIndex
{-# INLINE canonicalizeSlotSource #-}
