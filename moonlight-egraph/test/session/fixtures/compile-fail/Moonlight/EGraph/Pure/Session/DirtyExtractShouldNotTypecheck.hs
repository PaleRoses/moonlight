{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Pure.Session.DirtyExtractShouldNotTypecheck where

import Data.Kind (Type)
import Moonlight.EGraph.Pure.Extraction
  ( AnalysisCostAlgebra,
    ExtractionResult,
  )
import Moonlight.EGraph.Pure.Session

type PhaseFailNode :: Type -> Type
data PhaseFailNode child

dirtyExtractShouldNotTypecheck ::
  Ord cost =>
  ClassRef ->
  ClassRef ->
  AnalysisCostAlgebra PhaseFailNode analysis cost ->
  EGraphScript PhaseFailNode analysis 'Stable 'Stable (Maybe (ExtractionResult PhaseFailNode cost))
dirtyExtractShouldNotTypecheck leftRef rightRef costAlgebra =
  mergeClassRefs leftRef rightRef >>>= \mergedRef ->
    extractClass costAlgebra mergedRef
