module Moonlight.Flow.Carrier.Morphism.Internal.Apply
  ( applyCarrierMorphism,
  )
where

import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDeltaP (..),
    RelationalCarrierDelta,
  )
import Moonlight.Flow.Carrier.Morphism.Core.Types
  ( CarrierMorphism (..),
    CarrierMorphismPlan (..),
  )

applyCarrierMorphism ::
  CarrierMorphism profile ctx carrier prop boundary evidence err ->
  RelationalCarrierDelta ctx carrier prop boundary evidence ->
  Either err (RelationalCarrierDelta ctx carrier prop boundary evidence)
applyCarrierMorphism morph sourceDelta = do
  plan <-
    cmPrepare morph sourceDelta
  targetRows <-
    cmRows morph
      (cmpProfile plan)
      (deRows sourceDelta)
  targetSupport <-
    cmSupport morph
      (cmpProfile plan)
      (deSupport sourceDelta)
  targetEvidence <-
    cmEvidence morph
      (cmpProfile plan)
      (deEvidence sourceDelta)
  pure
    sourceDelta
      { deAddr = cmpTarget plan,
        deTime = cmTime morph (deTime sourceDelta),
        deSupport = targetSupport,
        deBoundary = cmpBoundary plan,
        deEvidence = targetEvidence,
        deRows = targetRows,
        deOrigin = cmOrigin morph (deOrigin sourceDelta),
        deScope = cmScope morph (deScope sourceDelta)
      }
{-# INLINE applyCarrierMorphism #-}
