{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Pure.Session.PhaseCompositionExample
  ( goodScriptRequiresRebuildBeforeExtraction,
  )
where

import Data.Kind (Type)
import Moonlight.EGraph.Pure.Extraction
  ( AnalysisCostAlgebra,
    ExtractionResult,
  )
import Moonlight.EGraph.Pure.Session

type PhaseExampleNode :: Type -> Type
data PhaseExampleNode child

goodScriptRequiresRebuildBeforeExtraction ::
  Ord cost =>
  ClassRef ->
  ClassRef ->
  AnalysisCostAlgebra PhaseExampleNode analysis cost ->
  EGraphScript PhaseExampleNode analysis 'Stable 'Stable (Maybe (ExtractionResult PhaseExampleNode cost))
goodScriptRequiresRebuildBeforeExtraction leftRef rightRef costAlgebra =
  mergeClassRefs leftRef rightRef >>>= \mergedRef ->
    rebuildGraph >>>= \_report ->
      canonicalClass mergedRef >>>= \representativeRef ->
        extractClass costAlgebra representativeRef
