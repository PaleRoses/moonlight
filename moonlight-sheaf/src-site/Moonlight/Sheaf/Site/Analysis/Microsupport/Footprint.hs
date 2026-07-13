{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Site.Analysis.Microsupport.Footprint
  ( MicrosupportFootprint (..),
    MicrosupportFootprintMeasure (..),
    MicrosupportFootprintReduction (..),
    MicrosupportMaterializationPlan,
    microsupportFootprintReduction,
    microsupportUnitFootprintReduction,
    microsupportStrictlyReducesFootprint,
    microsupportMaterializationPlan,
    microsupportPlanFootprintReduction,
    microsupportPlanRetainedFibers,
    microsupportPlanPrunedFibers,
    microsupportPlanRetainedNodes,
    microsupportPlanPrunedNodes,
    materializeMicrosupportPlan,
  )
where

import Data.Kind (Type)
import Moonlight.Derived.Morse (MicrosupportResult (..))
import Moonlight.Derived.Site (Criticality (..))
import Moonlight.Derived.Site (FinObjectId)
import Numeric.Natural (Natural)

type MicrosupportFootprint :: Type
newtype MicrosupportFootprint = MicrosupportFootprint
  { unMicrosupportFootprint :: Natural
  }
  deriving stock (Eq, Ord, Show)

instance Semigroup MicrosupportFootprint where
  MicrosupportFootprint left <> MicrosupportFootprint right =
    MicrosupportFootprint (left + right)

instance Monoid MicrosupportFootprint where
  mempty =
    MicrosupportFootprint 0

-- | Cheap metadata for the stalk payload over a node. This is deliberately
-- separate from payload construction: microlocal support is a restriction of
-- where sections can matter, not a license to build every stalk and sweep the
-- noncritical ashes afterward. See Kashiwara--Schapira's microsupport/singular
-- support program: support controls the local sheaf data that must be seen.
type MicrosupportFootprintMeasure :: Type
newtype MicrosupportFootprintMeasure = MicrosupportFootprintMeasure
  { runMicrosupportFootprintMeasure :: FinObjectId -> MicrosupportFootprint
  }

type MicrosupportFootprintReduction :: Type
data MicrosupportFootprintReduction = MicrosupportFootprintReduction
  { mfrTotalFootprint :: !MicrosupportFootprint,
    mfrRetainedFootprint :: !MicrosupportFootprint,
    mfrPrunedFootprint :: !MicrosupportFootprint,
    mfrRetainedFibers :: ![(FinObjectId, MicrosupportFootprint)],
    mfrPrunedFibers :: ![(FinObjectId, MicrosupportFootprint)]
  }
  deriving stock (Eq, Show)

type MicrosupportMaterializationPlan :: Type
newtype MicrosupportMaterializationPlan = MicrosupportMaterializationPlan
  { planFootprintReduction :: MicrosupportFootprintReduction
  }
  deriving stock (Eq, Show)

microsupportFootprintReduction ::
  (FinObjectId -> MicrosupportFootprint) ->
  MicrosupportResult ->
  MicrosupportFootprintReduction
microsupportFootprintReduction footprintOf microsupportResult =
  let classifiedFibers =
        fmap
          ( \(nodeValue, criticalityValue) ->
              (nodeValue, footprintOf nodeValue, criticalityValue)
          )
          (mrCriticalFibers microsupportResult)
      retainedFibers =
        foldMap retainedFiber classifiedFibers
      prunedFibers =
        foldMap prunedFiber classifiedFibers
   in MicrosupportFootprintReduction
        { mfrTotalFootprint = foldMap fiberFootprint classifiedFibers,
          mfrRetainedFootprint = foldMap snd retainedFibers,
          mfrPrunedFootprint = foldMap snd prunedFibers,
          mfrRetainedFibers = retainedFibers,
          mfrPrunedFibers = prunedFibers
        }

microsupportMaterializationPlan ::
  MicrosupportFootprintMeasure ->
  MicrosupportResult ->
  MicrosupportMaterializationPlan
microsupportMaterializationPlan footprintMeasure microsupportResult =
  MicrosupportMaterializationPlan
    (microsupportFootprintReduction (runMicrosupportFootprintMeasure footprintMeasure) microsupportResult)

microsupportPlanFootprintReduction ::
  MicrosupportMaterializationPlan ->
  MicrosupportFootprintReduction
microsupportPlanFootprintReduction =
  planFootprintReduction

microsupportPlanRetainedFibers ::
  MicrosupportMaterializationPlan ->
  [(FinObjectId, MicrosupportFootprint)]
microsupportPlanRetainedFibers =
  mfrRetainedFibers . planFootprintReduction

microsupportPlanPrunedFibers ::
  MicrosupportMaterializationPlan ->
  [(FinObjectId, MicrosupportFootprint)]
microsupportPlanPrunedFibers =
  mfrPrunedFibers . planFootprintReduction

microsupportPlanRetainedNodes ::
  MicrosupportMaterializationPlan ->
  [FinObjectId]
microsupportPlanRetainedNodes =
  fmap fst . microsupportPlanRetainedFibers

microsupportPlanPrunedNodes ::
  MicrosupportMaterializationPlan ->
  [FinObjectId]
microsupportPlanPrunedNodes =
  fmap fst . microsupportPlanPrunedFibers

materializeMicrosupportPlan ::
  (FinObjectId -> payload) ->
  MicrosupportMaterializationPlan ->
  [(FinObjectId, payload)]
materializeMicrosupportPlan materializePayload =
  fmap
    (\nodeValue -> (nodeValue, materializePayload nodeValue))
    . microsupportPlanRetainedNodes

microsupportUnitFootprintReduction ::
  MicrosupportResult ->
  MicrosupportFootprintReduction
microsupportUnitFootprintReduction =
  microsupportFootprintReduction (const (MicrosupportFootprint 1))

microsupportStrictlyReducesFootprint ::
  MicrosupportFootprintReduction ->
  Bool
microsupportStrictlyReducesFootprint =
  (> mempty) . mfrPrunedFootprint

type ClassifiedFiber :: Type
type ClassifiedFiber = (FinObjectId, MicrosupportFootprint, Criticality)

fiberFootprint :: ClassifiedFiber -> MicrosupportFootprint
fiberFootprint (_, footprintValue, _) =
  footprintValue

retainedFiber :: ClassifiedFiber -> [(FinObjectId, MicrosupportFootprint)]
retainedFiber (nodeValue, footprintValue, criticalityValue) =
  case criticalityValue of
    Critical ->
      [(nodeValue, footprintValue)]
    NonCritical ->
      []

prunedFiber :: ClassifiedFiber -> [(FinObjectId, MicrosupportFootprint)]
prunedFiber (nodeValue, footprintValue, criticalityValue) =
  case criticalityValue of
    Critical ->
      []
    NonCritical ->
      [(nodeValue, footprintValue)]
