{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Plan.Rewrite.Internal.RewriteState
  ( PlanRewriteState (..),
    addPlanENodeToState,
    mergeClassesForSimpleLaw,
    lawEnabled,
  )
where

import Data.Kind
  ( Type,
  )
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.Set qualified as Set
import Moonlight.Core
  ( ClassId,
    classIdKey,
  )
import Moonlight.EGraph.Pure.Rebuild
  ( merge,
  )
import Moonlight.EGraph.Pure.Change
  ( EGraphMutationResult (..),
    emtTouchedClassKeys,
  )
import Moonlight.EGraph.Pure.Types
  ( EGraph,
    canonicalizeClassId,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )
import Moonlight.Flow.Plan.Rewrite.Internal.Analysis
  ( addPlanENodeTracked,
  )
import Moonlight.Flow.Plan.Rewrite.Internal.Types
  ( PlanAnalysis,
    PlanSaturationError (..),
  )
import Moonlight.Flow.Plan.Rewrite.Node
  ( PlanClassId (..),
    PlanNode,
  )
import Moonlight.Flow.Plan.Rewrite.Proof
  ( PlanEqualityLaw,
    PlanEquivalenceStep (..),
    PlanRewriteSystem (..),
    mkPlanLawProof,
  )

type PlanRewriteState :: Type
data PlanRewriteState = PlanRewriteState
  { pwsGraph :: !(EGraph PlanNode PlanAnalysis),
    pwsDirtyClassKeys :: !IntSet,
    pwsStepsRev :: ![PlanEquivalenceStep]
  }

addPlanENodeToState :: PlanNode ClassId -> PlanRewriteState -> Either PlanSaturationError (ClassId, PlanRewriteState)
addPlanENodeToState node state =
  case addPlanENodeTracked node (pwsGraph state) of
    Left allocationError ->
      Left (PlanSaturationClassIdAllocationFailed allocationError)
    Right EGraphMutationResult
        { emrResult = classId,
          emrTrace = traceValue,
          emrGraph = graph
        } ->
      Right
        ( classId,
          state
            { pwsGraph = graph,
              pwsDirtyClassKeys =
                IntSet.union (emtTouchedClassKeys traceValue) (pwsDirtyClassKeys state)
            }
        )
{-# INLINE addPlanENodeToState #-}

mergeClassesForSimpleLaw ::
  PlanEqualityLaw ->
  StableDigest128 ->
  ClassId ->
  ClassId ->
  PlanRewriteState ->
  PlanRewriteState
mergeClassesForSimpleLaw law =
  mergeClassesForLaw law Nothing Nothing Nothing Nothing Nothing
{-# INLINE mergeClassesForSimpleLaw #-}

mergeClassesForLaw ::
  PlanEqualityLaw ->
  Maybe StableDigest128 ->
  Maybe StableDigest128 ->
  Maybe StableDigest128 ->
  Maybe StableDigest128 ->
  Maybe StableDigest128 ->
  StableDigest128 ->
  ClassId ->
  ClassId ->
  PlanRewriteState ->
  PlanRewriteState
mergeClassesForLaw law slotMapDigest boundaryBeforeDigest boundaryAfterDigest residualProofDigest coverProofDigest sideConditionDigest sourceClass targetClass state
  | sourceRoot == targetRoot =
      state
  | otherwise =
      state
        { pwsGraph = merge sourceRoot targetRoot graph,
          pwsDirtyClassKeys =
            IntSet.insert
              (classIdKey sourceRoot)
              (IntSet.insert (classIdKey targetRoot) (pwsDirtyClassKeys state)),
          pwsStepsRev =
            EqStepAppliedLaw
              ( mkPlanLawProof
                  law
                  (PlanClassId sourceRoot)
                  (PlanClassId targetRoot)
                  slotMapDigest
                  boundaryBeforeDigest
                  boundaryAfterDigest
                  residualProofDigest
                  coverProofDigest
                  sideConditionDigest
              )
              : pwsStepsRev state
        }
  where
    graph = pwsGraph state
    sourceRoot = canonicalizeClassId graph sourceClass
    targetRoot = canonicalizeClassId graph targetClass
{-# INLINE mergeClassesForLaw #-}

lawEnabled :: PlanEqualityLaw -> PlanRewriteSystem -> Bool
lawEnabled law =
  Set.member law . prsEnabledLaws
{-# INLINE lawEnabled #-}
