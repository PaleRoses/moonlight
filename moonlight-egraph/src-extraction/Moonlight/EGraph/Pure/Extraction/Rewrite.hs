module Moonlight.EGraph.Pure.Extraction.Rewrite
  ( extractGuided,
    extractAllGuided,
    extractWithProof,
    extractGuidedWithProof,
    extractGuidedWithAnalysis,
    extractAllGuidedWithAnalysis,
    extractionResultForClass,
    proofWitnessResult,
    extractionOrderingKey,
  )
where

import Data.Fix (Fix)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (mapMaybe, maybeToList)
import Moonlight.Control.Gate (GuidanceConfig)
import Moonlight.Core
  ( Language,
    OrderedFix (..),
    Pattern,
    ZipMatch,
  )
import Moonlight.EGraph.Pure.Context.Proof
  ( ProofGraph (pgGraph),
    proofClassWitnesses,
  )
import Moonlight.EGraph.Pure.Extraction
  ( AnalysisCostAlgebra,
    CostAlgebra,
    ExtractionChoiceSection,
    ExtractionResult (..),
    StableExtractionSnapshot,
    extract,
    extractChoiceSection,
    extractChoiceSectionForClass,
    extractFromChoiceSection,
    extractionClasses,
    liftCostAlgebra,
    minimumMaybe,
    stableExtractionSnapshotFromEGraph,
    stableExtractionSnapshotTable,
    termCost,
    termSize,
  )
import Moonlight.EGraph.Pure.Extraction.Guide
  ( GuidedOrderingKey,
    PlainOrderingKey,
    candidateExtractionsWithPrepared,
    guidedCandidateWithPrepared,
    guidedExtractionResult,
    guidedExtractionResultWithPrepared,
    guidedOrderingKey,
    prepareGuidance,
  )
import Moonlight.Rewrite.ProofContext (ProofQueryError)
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    EGraph,
    canonicalizeClassId,
  )

extractGuided :: (Language f, ZipMatch f, Ord cost) => GuidanceConfig (Pattern f) -> CostAlgebra f cost -> ClassId -> StableExtractionSnapshot f a -> Maybe (ExtractionResult f cost)
extractGuided guidanceConfig costAlgebraValue classId =
  extractGuidedWithAnalysis guidanceConfig (liftCostAlgebra costAlgebraValue) classId

extractAllGuided :: (Language f, ZipMatch f, Ord cost) => GuidanceConfig (Pattern f) -> CostAlgebra f cost -> StableExtractionSnapshot f a -> IntMap (ExtractionResult f cost)
extractAllGuided guidanceConfig costAlgebraValue =
  extractAllGuidedWithAnalysis guidanceConfig (liftCostAlgebra costAlgebraValue)

extractWithProof :: (Language f, Ord cost) => (graph -> EGraph f a) -> CostAlgebra f cost -> ClassId -> ProofGraph graph f c p -> Either ProofQueryError (Maybe (ExtractionResult f cost))
extractWithProof projectBaseGraph costAlgebraValue classId proofEGraph =
  let baseGraph = projectBaseGraph (pgGraph proofEGraph)
      canonicalClassId = canonicalizeClassId baseGraph classId
      graphCandidates =
        maybeToList (stableExtractionSnapshotFromEGraph baseGraph >>= extract costAlgebraValue canonicalClassId)
   in do
        proofTerms <- proofClassWitnesses projectBaseGraph canonicalClassId proofEGraph
        let proofCandidates =
              fmap
                (proofWitnessResult costAlgebraValue canonicalClassId)
                proofTerms
        pure
          ( minimumMaybe
              ( fmap
                  (\extractionResult -> (plainExtractionOrderingKey extractionResult, extractionResult))
                  (graphCandidates <> proofCandidates)
              )
          )

extractGuidedWithProof :: (Language f, ZipMatch f, Ord cost) => (graph -> EGraph f a) -> GuidanceConfig (Pattern f) -> CostAlgebra f cost -> ClassId -> ProofGraph graph f c p -> Either ProofQueryError (Maybe (ExtractionResult f cost))
extractGuidedWithProof projectBaseGraph guidanceConfig costAlgebraValue classId proofEGraph =
  let baseGraph = projectBaseGraph (pgGraph proofEGraph)
      canonicalClassId = canonicalizeClassId baseGraph classId
      preparedGuidance = prepareGuidance guidanceConfig
      graphCandidates =
        maybe
          []
          ( \section ->
              candidateExtractionsWithPrepared
                preparedGuidance
                section
                canonicalClassId
          )
          ( stableExtractionSnapshotFromEGraph baseGraph
              >>= extractChoiceSectionForClass
                (liftCostAlgebra costAlgebraValue)
                canonicalClassId
                . stableExtractionSnapshotTable
          )
   in do
        proofTerms <- proofClassWitnesses projectBaseGraph canonicalClassId proofEGraph
        let proofCandidates =
              mapMaybe
                ( guidedCandidateWithPrepared preparedGuidance
                    . proofWitnessResult costAlgebraValue canonicalClassId
                )
                proofTerms
        pure
          (minimumMaybe (graphCandidates <> proofCandidates))

extractGuidedWithAnalysis :: (Language f, ZipMatch f, Ord cost) => GuidanceConfig (Pattern f) -> AnalysisCostAlgebra f a cost -> ClassId -> StableExtractionSnapshot f a -> Maybe (ExtractionResult f cost)
extractGuidedWithAnalysis guidanceConfig costAlgebraValue classId snapshot =
  let table = stableExtractionSnapshotTable snapshot
      preparedGuidance = prepareGuidance guidanceConfig
   in extractChoiceSectionForClass costAlgebraValue classId table
        >>= \section ->
          guidedExtractionResultWithPrepared preparedGuidance section classId

extractAllGuidedWithAnalysis :: (Language f, ZipMatch f, Ord cost) => GuidanceConfig (Pattern f) -> AnalysisCostAlgebra f a cost -> StableExtractionSnapshot f a -> IntMap (ExtractionResult f cost)
extractAllGuidedWithAnalysis guidanceConfig costAlgebraValue snapshot =
  let table = stableExtractionSnapshotTable snapshot
      section = extractChoiceSection costAlgebraValue table
      preparedGuidance = prepareGuidance guidanceConfig
   in IntMap.mapMaybeWithKey
        (\classKey _extractionClass -> guidedExtractionResultWithPrepared preparedGuidance section (ClassId classKey))
        (extractionClasses table)

extractionResultForClass :: (Language f, ZipMatch f, Ord cost) => Maybe (GuidanceConfig (Pattern f)) -> ExtractionChoiceSection f a cost -> ClassId -> Maybe (ExtractionResult f cost)
extractionResultForClass maybeGuidance section classId =
  case maybeGuidance of
    Nothing ->
      extractFromChoiceSection classId section
    Just guidanceConfig ->
      guidedExtractionResult guidanceConfig section classId

proofWitnessResult :: Language f => CostAlgebra f cost -> ClassId -> Fix f -> ExtractionResult f cost
proofWitnessResult costAlgebraValue classId witnessTerm =
  ExtractionResult
    { erTerm = witnessTerm,
      erCost = termCost costAlgebraValue witnessTerm,
      erClass = classId
    }

extractionOrderingKey :: (Language f, ZipMatch f) => Maybe (GuidanceConfig (Pattern f)) -> ExtractionResult f cost -> Either (PlainOrderingKey f cost) (GuidedOrderingKey f cost)
extractionOrderingKey maybeGuidance extractionResult =
  maybe
    (Left (plainExtractionOrderingKey extractionResult))
    (\guidanceConfig -> Right (guidedOrderingKey guidanceConfig extractionResult))
    maybeGuidance

plainExtractionOrderingKey :: Language f => ExtractionResult f cost -> PlainOrderingKey f cost
plainExtractionOrderingKey extractionResult =
  (erCost extractionResult, termSize (erTerm extractionResult), OrderedFix (erTerm extractionResult))
