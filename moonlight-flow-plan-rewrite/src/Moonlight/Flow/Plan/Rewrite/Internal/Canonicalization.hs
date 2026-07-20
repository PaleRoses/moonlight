{-# LANGUAGE DataKinds #-}

module Moonlight.Flow.Plan.Rewrite.Internal.Canonicalization
  ( applyCanonicalizationMerge,
    canonicalizeDirtyClassKeys,
  )
where

import Moonlight.EGraph.Pure.Rebuild
  ( rebuild,
  )
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Moonlight.Core
  ( ClassId (..),
    classIdKey,
  )
import Moonlight.EGraph.Pure.Types
  ( EGraph,
    canonicalizeClassId,
  )
import Moonlight.Flow.Plan.Rewrite.Internal.RewriteState
  ( PlanRewriteState (..),
    lawEnabled,
    mergeClassesForSimpleLaw,
  )
import Moonlight.Flow.Plan.Rewrite.Internal.Types
  ( PlanSaturationState (..),
  )
import Moonlight.Flow.Plan.Rewrite.Node
  ( PlanClassId (..),
  )
import Moonlight.Flow.Plan.Rewrite.Proof
  ( PlanEqualityLaw (..),
    PlanEquivalenceStep,
    PlanRewriteSystem,
  )
import Moonlight.Flow.Plan.Shape.Term
  ( PlanShape (..),
    PlanStage (..),
  )

applyCanonicalizationMerge ::
  PlanRewriteSystem ->
  PlanShape 'Canonical ->
  PlanClassId (PlanShape 'RawLogical) ->
  PlanClassId (PlanShape 'Canonical) ->
  PlanSaturationState ->
  (PlanSaturationState, [PlanEquivalenceStep])
applyCanonicalizationMerge rewriteSystem canonicalShape (PlanClassId rawClass) (PlanClassId canonicalClass) state =
  case canonicalizationEqualityLaw rewriteSystem of
    Nothing -> (state, [])
    Just law ->
      let rewriteState =
            mergeClassesForSimpleLaw
              law
              (psDigest canonicalShape)
              rawClass
              canonicalClass
              ( PlanRewriteState
                  { pwsGraph = pssGraph state,
                    pwsDirtyClassKeys = pssDirtyClassKeys state,
                    pwsStepsRev = []
                  }
              )
          graph1 =
            rebuild (pwsGraph rewriteState)
       in ( state
              { pssGraph = graph1,
                pssDirtyClassKeys =
                  canonicalizeDirtyClassKeys graph1 (pwsDirtyClassKeys rewriteState)
              },
            reverse (pwsStepsRev rewriteState)
          )

canonicalizeDirtyClassKeys ::
  EGraph f a ->
  IntSet ->
  IntSet
canonicalizeDirtyClassKeys graph =
  IntSet.map (classIdKey . canonicalizeClassId graph . ClassId)

canonicalizationEqualityLaw :: PlanRewriteSystem -> Maybe PlanEqualityLaw
canonicalizationEqualityLaw rewriteSystem
  | lawEnabled LawAlphaCanonical rewriteSystem = Just LawAlphaCanonical
  | lawEnabled LawAtomOrder rewriteSystem = Just LawAtomOrder
  | otherwise = Nothing
