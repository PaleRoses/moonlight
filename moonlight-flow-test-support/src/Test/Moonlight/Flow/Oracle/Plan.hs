module Test.Moonlight.Flow.Oracle.Plan
  ( canonicalDigestViaSaturation,
  )
where

import Moonlight.Flow.Model.Schema.Digest (StableDigest128)
import Moonlight.Flow.Plan.Rewrite
  ( PlanSaturationError,
    SaturationBudget (..),
    extractCanonicalPlanKey,
    saturatePlanShape,
    semanticNormalizationPlanRewriteSystem,
  )
import Moonlight.Flow.Plan.Shape.Term
  ( PlanShape,
    PlanStage (RawLogical),
  )

canonicalDigestViaSaturation :: PlanShape 'RawLogical -> Either PlanSaturationError StableDigest128
canonicalDigestViaSaturation =
  fmap extractCanonicalPlanKey . saturatePlanShape oraclePlanSaturationBudget semanticNormalizationPlanRewriteSystem

oraclePlanSaturationBudget :: SaturationBudget
oraclePlanSaturationBudget =
  SaturationBudget
    { sbMaxIterations = 8,
      sbMaxNodes = maxBound
    }
{-# INLINE oraclePlanSaturationBudget #-}
