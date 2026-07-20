{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Moonlight.Flow.Plan.Shape
  ( CanonAtom (..),
    CanonAtomMultiset,
    emptyCanonAtomMultiset,
    insertCanonAtom,

    LogicalQueryShape (..),
    CanonicalizationResult (..),
    FactorShapePayload (..),
    factorShapePlan,
    factorShapePlanDigest,
    factorShapeLogical,
    factorShapeFragmentPayload,
    factorShapeAtoms,
    factorShapeSourceSchema,
    factorShapeOutputSchema,
    factorShapeSeparator,
    factorShapeBoundary,
    factorShapeResidual,

    CanonBagShape (..),
    CanonSeparator (..),
    CanonicalFragment (..),

    CanonicalBoundaryShape,
    cbsShape,
    cbsSchema,
    cbsSensitiveSlots,
    cbsSlotKeys,
    cbsKeys,
    cbsDigest,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Word
  ( Word64,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( Boundary,
    BoundaryShape,
    boundaryDigest,
    boundaryKeys,
    boundaryShape,
    bsSchema,
    bsSensitive,
    bsSlotKeys,
  )
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128 (..),
  )
import Moonlight.Flow.Plan.Shape.Term
  ( CanonSlot,
    CanonStalkRecipe,
    FragmentPayload,
    PlanShape (..),
    PlanShapePayload,
    PlanStage (..),
    ResidualShape,
  )

type CanonicalBoundaryShape :: Type
type CanonicalBoundaryShape =
  Boundary CanonSlot Int

cbsShape :: CanonicalBoundaryShape -> BoundaryShape CanonSlot Int
cbsShape =
  boundaryShape
{-# INLINE cbsShape #-}

cbsSchema :: CanonicalBoundaryShape -> [CanonSlot]
cbsSchema =
  bsSchema . boundaryShape
{-# INLINE cbsSchema #-}

cbsSensitiveSlots :: CanonicalBoundaryShape -> Set CanonSlot
cbsSensitiveSlots =
  bsSensitive . boundaryShape
{-# INLINE cbsSensitiveSlots #-}

cbsSlotKeys :: CanonicalBoundaryShape -> Map CanonSlot (Set Int)
cbsSlotKeys =
  bsSlotKeys . boundaryShape
{-# INLINE cbsSlotKeys #-}

cbsKeys :: CanonicalBoundaryShape -> Set Int
cbsKeys =
  boundaryKeys
{-# INLINE cbsKeys #-}

cbsDigest :: CanonicalBoundaryShape -> StableDigest128
cbsDigest =
  boundaryDigest
{-# INLINE cbsDigest #-}

type CanonAtom :: Type
data CanonAtom = CanonAtom
  { caTagDigest :: {-# UNPACK #-} !Word64,
    caColumns :: ![CanonSlot],
    caRecipe :: !CanonStalkRecipe
  }
  deriving stock (Eq, Ord, Show, Read)

type CanonAtomMultiset :: Type
type CanonAtomMultiset =
  Map CanonAtom Int

emptyCanonAtomMultiset :: CanonAtomMultiset
emptyCanonAtomMultiset =
  Map.empty
{-# INLINE emptyCanonAtomMultiset #-}

insertCanonAtom :: CanonAtom -> CanonAtomMultiset -> CanonAtomMultiset
insertCanonAtom atomValue =
  Map.insertWith (+) atomValue 1
{-# INLINE insertCanonAtom #-}

type LogicalQueryShape :: Type
data LogicalQueryShape = LogicalQueryShape
  { lqsDomain :: !QueryPlanDomain,
    lqsAtoms :: !CanonAtomMultiset,
    lqsRoot :: !CanonSlot,
    lqsOutputs :: ![CanonSlot],
    lqsResidual :: !ResidualShape
  }
  deriving stock (Eq, Ord, Show, Read)

type instance PlanShapePayload 'Canonical = LogicalQueryShape

type CanonicalizationResult :: Type
data CanonicalizationResult = CanonicalizationResult
  { crPlan :: !(PlanShape 'Canonical),
    crSlotMap :: !(IntMap CanonSlot),
    crAtomShapes :: !(IntMap CanonAtom),
    crResidual :: !ResidualShape
  }
  deriving stock (Eq, Ord, Show, Read)

type CanonBagShape :: Type
data CanonBagShape = CanonBagShape
  { cbgSlots :: ![CanonSlot],
    cbgAtoms :: !CanonAtomMultiset,
    cbgDigest :: !StableDigest128
  }
  deriving stock (Eq, Ord, Show, Read)

type CanonSeparator :: Type
data CanonSeparator = CanonSeparator
  { csepChild :: !CanonBagShape,
    csepParent :: !CanonBagShape,
    csepSlots :: ![CanonSlot],
    csepDigest :: !StableDigest128
  }
  deriving stock (Eq, Ord, Show, Read)

type CanonicalFragment :: Type
data CanonicalFragment
  = CanonRootFragment
  | CanonBagFragment !CanonBagShape
  | CanonSeparatorFragment !CanonSeparator
  deriving stock (Eq, Ord, Show, Read)

type instance PlanShapePayload 'FactorShape = FactorShapePayload

type FactorShapePayload :: Type
data FactorShapePayload = FactorShapePayload
  { fspPlan :: !(PlanShape 'Canonical),
    fspFragment :: !(PlanShape 'Fragment),
    fspAtoms :: !CanonAtomMultiset,
    fspSourceSchema :: ![CanonSlot],
    fspOutputSchema :: ![CanonSlot],
    fspSeparator :: !(Maybe CanonSeparator),
    fspBoundary :: !CanonicalBoundaryShape,
    fspResidual :: !ResidualShape
  }
  deriving stock (Eq, Ord, Show, Read)

factorShapePlan :: PlanShape 'FactorShape -> PlanShape 'Canonical
factorShapePlan =
  fspPlan . psPayload
{-# INLINE factorShapePlan #-}

factorShapePlanDigest :: PlanShape 'FactorShape -> StableDigest128
factorShapePlanDigest =
  psDigest . factorShapePlan
{-# INLINE factorShapePlanDigest #-}

factorShapeLogical :: PlanShape 'FactorShape -> LogicalQueryShape
factorShapeLogical =
  psPayload . factorShapePlan
{-# INLINE factorShapeLogical #-}

factorShapeFragmentPayload :: PlanShape 'FactorShape -> FragmentPayload
factorShapeFragmentPayload =
  psPayload . fspFragment . psPayload
{-# INLINE factorShapeFragmentPayload #-}

factorShapeAtoms :: PlanShape 'FactorShape -> CanonAtomMultiset
factorShapeAtoms =
  fspAtoms . psPayload
{-# INLINE factorShapeAtoms #-}

factorShapeSourceSchema :: PlanShape 'FactorShape -> [CanonSlot]
factorShapeSourceSchema =
  fspSourceSchema . psPayload
{-# INLINE factorShapeSourceSchema #-}

factorShapeOutputSchema :: PlanShape 'FactorShape -> [CanonSlot]
factorShapeOutputSchema =
  fspOutputSchema . psPayload
{-# INLINE factorShapeOutputSchema #-}

factorShapeSeparator :: PlanShape 'FactorShape -> Maybe CanonSeparator
factorShapeSeparator =
  fspSeparator . psPayload
{-# INLINE factorShapeSeparator #-}

factorShapeBoundary :: PlanShape 'FactorShape -> CanonicalBoundaryShape
factorShapeBoundary =
  fspBoundary . psPayload
{-# INLINE factorShapeBoundary #-}

factorShapeResidual :: PlanShape 'FactorShape -> ResidualShape
factorShapeResidual =
  fspResidual . psPayload
{-# INLINE factorShapeResidual #-}
