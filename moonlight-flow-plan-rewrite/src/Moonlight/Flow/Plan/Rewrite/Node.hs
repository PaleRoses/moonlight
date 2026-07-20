{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Plan.Rewrite.Node
  ( PlanClassId (..),
    PlanNode (..),
    rawLogicalPlanTerm,
    canonicalPlanTerm,
    factorPlanTerm,
    fragmentPlanTerm,
    projectionPlanTerm,
    restrictionPlanTerm,
    amalgamationPlanTerm,
    coverageTransformPlanTerm,
  )
where

import Data.Kind
  ( Type,
  )
import Moonlight.Core
  ( ClassId,
  )
import Data.Fix
  ( Fix (..),
  )
import Moonlight.Flow.Plan.Shape.Build qualified as ShapeBuild
import Moonlight.Flow.Plan.Shape
  ( CanonBagShape (..),
    CanonSeparator (..),
    FactorShapePayload (..),
  )
import Moonlight.Flow.Plan.Shape.Term
  ( PlanShape (..),
    PlanStage (..),
  )

type PlanClassId :: Type -> Type
newtype PlanClassId stage = PlanClassId
  { unPlanClassId :: ClassId
  }
  deriving stock (Eq, Ord, Show, Read)

type PlanNode :: Type -> Type
data PlanNode child
  = PlanRawLogicalNode !(PlanShape 'RawLogical)
  | PlanCanonicalNode !(PlanShape 'Canonical)
  | PlanBagNode !CanonBagShape
  | PlanSeparatorNode !CanonSeparator
  | PlanFactorNode !(PlanShape 'FactorShape) !child !child !child
  | PlanFragmentNode !(PlanShape 'Fragment)
  | PlanProjectionNode !(PlanShape 'Projection) !child
  | PlanRestrictionNode !(PlanShape 'Restriction) !child
  | PlanAmalgamationNode !(PlanShape 'Cover) ![child]
  | PlanCoverageTransformNode !(PlanShape 'CoverageTransform) !child
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Functor,
      Foldable,
      Traversable
    )

rawLogicalPlanTerm ::
  PlanShape 'RawLogical ->
  Fix PlanNode
rawLogicalPlanTerm =
  Fix . PlanRawLogicalNode
{-# INLINE rawLogicalPlanTerm #-}

canonicalPlanTerm ::
  PlanShape 'Canonical ->
  Fix PlanNode
canonicalPlanTerm =
  Fix . PlanCanonicalNode
{-# INLINE canonicalPlanTerm #-}

factorPlanTerm ::
  PlanShape 'FactorShape ->
  Fix PlanNode
factorPlanTerm planShape =
  let payload =
        psPayload planShape
   in Fix
        ( PlanFactorNode
            planShape
            (canonicalPlanTerm (fspPlan payload))
            (fragmentPlanTerm (fspFragment payload))
            (factorStructurePlanTerm payload)
        )
{-# INLINE factorPlanTerm #-}

factorStructurePlanTerm ::
  FactorShapePayload ->
  Fix PlanNode
factorStructurePlanTerm payload =
  case fspSeparator payload of
    Just separator ->
      Fix (PlanSeparatorNode separator)
    Nothing ->
      Fix
        ( PlanBagNode
            ( ShapeBuild.mkCanonBagShape
                (fspOutputSchema payload)
                (fspAtoms payload)
            )
        )
{-# INLINE factorStructurePlanTerm #-}

fragmentPlanTerm ::
  PlanShape 'Fragment ->
  Fix PlanNode
fragmentPlanTerm =
  Fix . PlanFragmentNode
{-# INLINE fragmentPlanTerm #-}

projectionPlanTerm ::
  PlanShape 'Projection ->
  Fix PlanNode ->
  Fix PlanNode
projectionPlanTerm planShape source =
  Fix (PlanProjectionNode planShape source)
{-# INLINE projectionPlanTerm #-}

restrictionPlanTerm ::
  PlanShape 'Restriction ->
  Fix PlanNode ->
  Fix PlanNode
restrictionPlanTerm planShape source =
  Fix (PlanRestrictionNode planShape source)
{-# INLINE restrictionPlanTerm #-}

amalgamationPlanTerm ::
  PlanShape 'Cover ->
  [Fix PlanNode] ->
  Fix PlanNode
amalgamationPlanTerm planShape members =
  Fix (PlanAmalgamationNode planShape members)
{-# INLINE amalgamationPlanTerm #-}

coverageTransformPlanTerm ::
  PlanShape 'CoverageTransform ->
  Fix PlanNode ->
  Fix PlanNode
coverageTransformPlanTerm planShape source =
  Fix (PlanCoverageTransformNode planShape source)
{-# INLINE coverageTransformPlanTerm #-}
