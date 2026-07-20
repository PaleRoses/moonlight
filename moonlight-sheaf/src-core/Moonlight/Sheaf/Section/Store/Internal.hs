{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}

module Moonlight.Sheaf.Section.Store.Internal
  ( SectionEpoch (..),
    DenseSection (..),
    SparseSection (..),
    TotalSectionStore (..),
    PartialSectionStore (..),
    SectionDelta (..),
    KeyedSectionDelta (..),
    KeyedSectionEdit (..),
    SectionConstructionError (..),
    SectionLookupError (..),
    SectionUpdateError (..),
    SectionStoreError (..),
    SectionRestrictionResult (..),
    SectionDescentObservation (..),
    SectionDescentPreparationError (..),
    SectionDescentError (..),
    SectionDescentResult (..),
    SectionDescentRestrictionRow (..),
    PreparedSectionDescent (..),
    PreparedSectionDescentViews (..),
    AlgebraPreparedSectionDescent (..),
    SectionDescentRowMode (..),
    PinnedDescentTarget (..),
    FrontierClosureBudget (..),
    DescentDirtyCoverage (..),
    SectionDescentAccumulator (..),
    PreparedSectionProgram (..),
    PreparedSectionInstruction (..),
    SectionFastEditProgram (..),
    SectionFastEditKernel (..),
    SectionFastEditKernelStep (..),
    advanceTotalSectionStore,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Vector (Vector)
import Data.Vector.Unboxed qualified as UVector
import Moonlight.Delta.Scope (Scope)
import Moonlight.Sheaf.Section.Morphism
  ( Restriction,
    RestrictionCheck,
    RestrictionId,
  )
import Moonlight.Sheaf.Section.ObjectIndex (ObjectKey)
import Moonlight.Sheaf.Section.Stalk (StalkAlgebra)

newtype SectionEpoch = SectionEpoch
  { unSectionEpoch :: Int
  }
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum, Num)

newtype DenseSection stalk = DenseSection
  { unDenseSection :: Vector stalk
  }
  deriving stock (Eq, Show)

newtype SparseSection cell stalk = SparseSection
  { unSparseSection :: Map cell stalk
  }
  deriving stock (Eq, Show)

data TotalSectionStore owner cell stalk = TotalSectionStore
  { tssValues :: !(DenseSection stalk),
    tssExtent :: !(Scope IntSet),
    tssEpoch :: !SectionEpoch
  }
  deriving stock (Eq, Show)

type role TotalSectionStore nominal nominal representational

data PartialSectionStore owner cell stalk = PartialSectionStore
  { pssValues :: !(SparseSection cell stalk),
    pssExtent :: !(Scope IntSet),
    pssEpoch :: !SectionEpoch
  }
  deriving stock (Eq, Show)

type role PartialSectionStore nominal nominal representational

data SectionDelta owner cell stalk = SectionDelta
  { sdAssignments :: !(Map cell stalk)
  }
  deriving stock (Eq, Show)

type role SectionDelta nominal nominal representational

data KeyedSectionDelta owner stalk = KeyedSectionDelta
  { ksdExtent :: !(Scope IntSet),
    ksdAssignments :: !(IntMap stalk)
  }
  deriving stock (Eq, Show)

type role KeyedSectionDelta nominal representational

data KeyedSectionEdit owner stalk = KeyedSectionEdit
  { kseObjectKey :: !ObjectKey,
    kseValue :: !stalk
  }
  deriving stock (Eq, Show)

type role KeyedSectionEdit nominal representational

data SectionConstructionError cell = SectionConstructionError
  { sceMissingCells :: !(Set cell),
    sceExtraCells :: !(Set cell)
  }
  deriving stock (Eq, Show)

data SectionLookupError cell
  = SectionLookupOutOfBasis !cell
  | SectionLookupInvariantMissing !cell
  deriving stock (Eq, Show)

data SectionUpdateError cell
  = SectionUpdateOutOfBasis !cell
  | SectionUpdateInvariantMissing !cell
  deriving stock (Eq, Show)

data SectionStoreError cell
  = SectionStoreConstructionFailed !(SectionConstructionError cell)
  | SectionStoreUnknownCell !cell
  | SectionStoreUnknownObjectKey !ObjectKey
  | SectionStoreObjectCountMismatch !Int !Int
  | SectionStoreInvariantMissing !cell
  deriving stock (Eq, Show)

data SectionRestrictionResult cell stalk mismatch
  = SectionRestrictionSatisfied
  | SectionRestrictionMismatch !(RestrictionCheck stalk mismatch)
  deriving stock (Eq, Show)

data SectionDescentObservation
  = ObserveFinalSection
  | ObserveEachStep
  deriving stock (Eq, Ord, Show, Read)

data SectionDescentPreparationError
  = SectionDescentPreparationRestrictionMissing !RestrictionId
  | SectionDescentPreparationBudgetOverflow !Integer
  deriving stock (Eq, Show)

data SectionDescentError cell stalk mismatch
  = SectionDescentStoreFailed !(SectionStoreError cell)
  | SectionDescentRestrictionMissing !RestrictionId
  | SectionDescentPinnedConflict !cell !stalk !stalk ![mismatch]
  | SectionDescentRejected !(Map cell [mismatch])
  | SectionDescentFrontierDidNotConverge !(Scope IntSet)
  deriving stock (Eq, Show)

data SectionDescentResult owner cell stalk = SectionDescentResult
  { sdrSection :: !(TotalSectionStore owner cell stalk),
    sdrObservedSteps :: !Int
  }
  deriving stock (Eq, Show)

data SectionDescentRestrictionRow cell witness = SectionDescentRestrictionRowInternal
  { sectionDescentRowRestrictionKeyInternal :: !Int,
    sectionDescentRowRestrictionInternal :: !(Restriction cell witness),
    sectionDescentRowSourceKeyInternal :: !ObjectKey,
    sectionDescentRowTargetKeyInternal :: !ObjectKey,
    sectionDescentRowSourceOrdinalInternal :: !Int,
    sectionDescentRowTargetOrdinalInternal :: !Int
  }

data PreparedSectionDescent owner cell witness = PreparedSectionDescentInternal
  { preparedSectionDescentObjectCountInternal :: !Int,
    preparedSectionDescentFrontierClosureBudgetInternal :: !FrontierClosureBudget,
    preparedSectionDescentRowsByRestrictionIdInternal :: !(Vector (SectionDescentRestrictionRow cell witness)),
    preparedSectionDescentViewsInternal :: !(PreparedSectionDescentViews owner cell witness)
  }

type role PreparedSectionDescent nominal nominal representational

data PreparedSectionDescentViews owner cell witness = PreparedSectionDescentViewsInternal
  { preparedSectionDescentIncidentRestrictionIdsInternal :: !(Vector (UVector.Vector Int)),
    preparedSectionDescentIncomingRestrictionIdsInternal :: !(Vector (UVector.Vector Int)),
    preparedSectionDescentOutgoingRestrictionIdsInternal :: !(Vector (UVector.Vector Int)),
    preparedSectionDescentFastEditProgramsInternal :: !(Vector (Maybe (SectionFastEditProgram owner cell witness))),
    preparedSectionDescentAllRestrictionIdsInternal :: !(UVector.Vector Int)
  }

type role PreparedSectionDescentViews nominal nominal representational

data AlgebraPreparedSectionDescent owner cell witness stalk mismatch repairObstruction = AlgebraPreparedSectionDescentInternal
  { algebraPreparedSectionDescentInternal :: !(PreparedSectionDescent owner cell witness),
    algebraPreparedStalkAlgebraInternal :: !(StalkAlgebra witness stalk mismatch repairObstruction),
    algebraPreparedFastEditKernelsInternal :: !(Vector (Maybe (Either (SectionDescentError cell stalk mismatch) (SectionFastEditKernel owner stalk))))
  }

data SectionDescentRowMode
  = DescentIncidentRows
  | DescentOutgoingRows

data PinnedDescentTarget stalk
  = PinnedDescentAssignments !(IntMap stalk)
  | PinnedDescentSingleton !Int !stalk

newtype FrontierClosureBudget = FrontierClosureBudget
  { unFrontierClosureBudget :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

data DescentDirtyCoverage
  = DescentDirtyObjects
  | DescentDirtyFull
  deriving stock (Eq, Ord, Show, Read)

data SectionDescentAccumulator = SectionDescentAccumulator
  { sdaDirtyCoverage :: !DescentDirtyCoverage,
    sdaObservedSteps :: !Int
  }

data PreparedSectionProgram owner stalk = PreparedSectionProgram
  { pspObjectCount :: !Int,
    pspInstructions :: !(Vector (PreparedSectionInstruction stalk))
  }
  deriving stock (Eq, Show)

type role PreparedSectionProgram nominal representational

data PreparedSectionInstruction stalk
  = PreparedSectionAssign !Int !stalk
  | PreparedSectionValueStream !Int !(Vector stalk)
  | PreparedSectionDelta !(Scope IntSet) !(IntMap stalk)
  deriving stock (Eq, Show)

data SectionFastEditProgram owner cell witness = SectionFastEditProgramInternal
  { sfepRestrictionIds :: !(UVector.Vector Int),
    sfepDirtyKeys :: !(UVector.Vector Int)
  }

type role SectionFastEditProgram nominal nominal representational

newtype SectionFastEditKernel owner stalk = SectionFastEditKernelInternal
  { sectionFastEditKernelStepsInternal :: Vector (SectionFastEditKernelStep stalk)
  }

type role SectionFastEditKernel nominal representational

data SectionFastEditKernelStep stalk
  = SectionFastEditCopyStepInternal !Int !Int
  | SectionFastEditMapStepInternal !(stalk -> stalk) !Int !Int

advanceTotalSectionStore ::
  DenseSection stalk ->
  Scope IntSet ->
  TotalSectionStore owner cell stalk ->
  TotalSectionStore owner cell stalk
advanceTotalSectionStore values extent store =
  store
    { tssValues = values,
      tssExtent = extent,
      tssEpoch = nextSectionEpoch (tssEpoch store)
    }
{-# INLINE advanceTotalSectionStore #-}

nextSectionEpoch :: SectionEpoch -> SectionEpoch
nextSectionEpoch (SectionEpoch epoch) =
  SectionEpoch (epoch + 1)
