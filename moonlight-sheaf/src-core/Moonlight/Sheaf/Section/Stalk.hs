{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Stalk algebras: merge, mismatch, bounds, and repair kernels for stalk
-- values.
module Moonlight.Sheaf.Section.Stalk
  ( StalkAlgebra (..),
    StalkRestrictionKernel (..),
    StalkBounds (..),
    MergeObstruction (..),
    RepairInput (..),
    applyStalkRestrictionKernel,
    restrictStalk,
    stalkApproxEq,
    stalkMismatches,
    mergeStalks,
    normalizeStalk,
    mismatchObstruction,
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty

type MergeObstruction :: Type -> Type
data MergeObstruction mismatch
  = MergeMismatchObstruction !(NonEmpty mismatch)
  deriving stock (Eq, Show, Functor)

type RepairInput :: Type -> Type -> Type -> Type
data RepairInput witness stalk mismatch
  = RepairMergeInput !(NonEmpty stalk) !(NonEmpty mismatch)
  | RepairRestrictionInput !witness !stalk !stalk !(NonEmpty mismatch)
  deriving stock (Eq, Show)

type StalkBounds :: Type -> Type -> Type
data StalkBounds cell stalk = StalkBounds
  { stalkTopAt :: cell -> stalk,
    stalkBottomAt :: cell -> stalk,
    widenStalkAt :: cell -> stalk -> stalk -> stalk
  }

type StalkAlgebra :: Type -> Type -> Type -> Type -> Type
data StalkAlgebra witness stalk mismatch repairObstruction = StalkAlgebra
  { saRestrictionKernel :: witness -> StalkRestrictionKernel stalk,
    saMismatches :: stalk -> stalk -> [mismatch],
    saMerge :: stalk -> stalk -> Either (MergeObstruction mismatch) stalk,
    saRepair :: RepairInput witness stalk mismatch -> Either repairObstruction stalk,
    saNormalize :: stalk -> stalk
  }

type StalkRestrictionKernel :: Type -> Type
data StalkRestrictionKernel stalk
  = StalkRestrictionIdentity
  | StalkRestrictionMap !(stalk -> stalk)

applyStalkRestrictionKernel :: StalkRestrictionKernel stalk -> stalk -> stalk
applyStalkRestrictionKernel restrictionKernel =
  case restrictionKernel of
    StalkRestrictionIdentity ->
      id
    StalkRestrictionMap restrictValue ->
      restrictValue
{-# INLINE applyStalkRestrictionKernel #-}

restrictStalk :: StalkAlgebra witness stalk mismatch repairObstruction -> witness -> stalk -> stalk
restrictStalk algebra witness =
  applyStalkRestrictionKernel (saRestrictionKernel algebra witness)
{-# INLINE restrictStalk #-}

stalkApproxEq :: StalkAlgebra witness stalk mismatch repairObstruction -> stalk -> stalk -> Bool
stalkApproxEq algebra left right =
  null (saMismatches algebra left right)

stalkMismatches :: StalkAlgebra witness stalk mismatch repairObstruction -> stalk -> stalk -> [mismatch]
stalkMismatches =
  saMismatches

mergeStalks :: StalkAlgebra witness stalk mismatch repairObstruction -> stalk -> stalk -> Either (MergeObstruction mismatch) stalk
mergeStalks =
  saMerge

normalizeStalk :: StalkAlgebra witness stalk mismatch repairObstruction -> stalk -> stalk
normalizeStalk =
  saNormalize

mismatchObstruction :: [mismatch] -> Maybe (MergeObstruction mismatch)
mismatchObstruction mismatches =
  fmap MergeMismatchObstruction (NonEmpty.nonEmpty mismatches)
