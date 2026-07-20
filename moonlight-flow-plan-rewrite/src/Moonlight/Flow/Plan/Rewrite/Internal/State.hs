{-# LANGUAGE DataKinds #-}

module Moonlight.Flow.Plan.Rewrite.Internal.State
  ( emptyPlanSaturationState,
    insertPlanTerm,
    canonicalizePlanClass,
    planClassCanonicalShape,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Moonlight.Core
  ( classIdKey,
  )
import Moonlight.EGraph.Pure.Change
  ( EGraphMutationResult (..),
    emtTouchedClassKeys,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons
  ( insertTermTracked,
  )
import Moonlight.EGraph.Pure.Types
  ( canonicalizeClassId,
    eGraphAnalysis,
    emptyEGraph,
  )
import Data.Fix
  ( Fix,
  )
import Moonlight.Flow.Plan.Rewrite.Internal.Analysis
  ( planAnalysisSpec,
  )
import Moonlight.Flow.Plan.Rewrite.Internal.Types
  ( PlanAnalysis (..),
    PlanSaturationError (..),
    PlanSaturationState (..),
  )
import Moonlight.Flow.Plan.Rewrite.Node
  ( PlanClassId (..),
    PlanNode,
  )
import Moonlight.Flow.Plan.Shape.CanonicalKey qualified as Canonical
import Moonlight.Flow.Plan.Shape.Term
  ( PlanShape,
    PlanStage (..),
  )

emptyPlanSaturationState :: PlanSaturationState
emptyPlanSaturationState =
  PlanSaturationState
    { pssGraph = emptyEGraph planAnalysisSpec,
      pssDirtyClassKeys = IntSet.empty,
      pssCanonicalizationMemo = Canonical.emptyPlanCanonicalizationMemo
    }
{-# INLINE emptyPlanSaturationState #-}


insertPlanTerm :: Fix PlanNode -> PlanSaturationState -> Either PlanSaturationError (PlanClassId stage, PlanSaturationState)
insertPlanTerm term state =
  case insertTermTracked term (pssGraph state) of
    Left allocationError ->
      Left (PlanSaturationClassIdAllocationFailed allocationError)
    Right EGraphMutationResult
        { emrResult = classId,
          emrTrace = traceValue,
          emrGraph = graph
        } ->
      Right
        ( PlanClassId classId,
          state
            { pssGraph = graph,
              pssDirtyClassKeys =
                IntSet.union (emtTouchedClassKeys traceValue) (pssDirtyClassKeys state)
            }
        )
{-# INLINE insertPlanTerm #-}


canonicalizePlanClass :: PlanSaturationState -> PlanClassId stage -> PlanClassId stage
canonicalizePlanClass state (PlanClassId classId) =
  PlanClassId (canonicalizeClassId (pssGraph state) classId)
{-# INLINE canonicalizePlanClass #-}

planClassCanonicalShape :: PlanSaturationState -> PlanClassId stage -> Maybe (PlanShape 'Canonical)
planClassCanonicalShape state (PlanClassId classId) =
  paCanonicalCandidate =<< IntMap.lookup canonicalKey (eGraphAnalysis (pssGraph state))
  where
    canonicalKey = classIdKey (canonicalizeClassId (pssGraph state) classId)
{-# INLINE planClassCanonicalShape #-}
