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
import Data.Maybe (maybeToList)
import Data.Set qualified as Set
import Moonlight.Control.Gate
  ( GuidanceConfig,
  )
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
import Moonlight.EGraph.Pure.Extraction.Algebra
  ( buildExtractionResult,
    extractChoices,
  )
import Moonlight.EGraph.Pure.Extraction.Compile
  ( extract,
  )
import Moonlight.EGraph.Pure.Extraction.Core
  ( AnalysisCostAlgebra,
    BestChoice,
    CostAlgebra,
    ExtractionResult (..),
    ExtractionTable,
    StableExtractionSnapshot,
    extractionCanonicalClass,
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
    guidedExtractionResult,
    guidedOrderingKey,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    EGraph,
    canonicalizeClassId,
    classIdKey,
  )

extractGuided :: (Language f, ZipMatch f, Ord cost) => GuidanceConfig (Pattern f) -> CostAlgebra f cost -> ClassId -> StableExtractionSnapshot f a -> Maybe (ExtractionResult f cost)
extractGuided guidanceConfig costAlgebraValue classId =
  extractGuidedWithAnalysis guidanceConfig (liftCostAlgebra costAlgebraValue) classId

extractAllGuided :: (Language f, ZipMatch f, Ord cost) => GuidanceConfig (Pattern f) -> CostAlgebra f cost -> StableExtractionSnapshot f a -> IntMap (ExtractionResult f cost)
extractAllGuided guidanceConfig costAlgebraValue =
  extractAllGuidedWithAnalysis guidanceConfig (liftCostAlgebra costAlgebraValue)

extractWithProof :: (Language f, Ord cost) => (graph -> EGraph f a) -> CostAlgebra f cost -> ClassId -> ProofGraph graph f c p -> Maybe (ExtractionResult f cost)
extractWithProof projectBaseGraph costAlgebraValue classId proofEGraph =
  let baseGraph = projectBaseGraph (pgGraph proofEGraph)
      canonicalClassId = canonicalizeClassId baseGraph classId
      graphCandidates =
        maybeToList (stableExtractionSnapshotFromEGraph baseGraph >>= extract costAlgebraValue canonicalClassId)
      proofCandidates =
        fmap
          (proofWitnessResult Nothing costAlgebraValue canonicalClassId)
          (either (const []) id (proofClassWitnesses projectBaseGraph canonicalClassId proofEGraph))
   in minimumMaybe
        ( fmap
            (\extractionResult -> (plainExtractionOrderingKey extractionResult, extractionResult))
            (graphCandidates <> proofCandidates)
        )

extractGuidedWithProof :: (Language f, ZipMatch f, Ord cost) => (graph -> EGraph f a) -> GuidanceConfig (Pattern f) -> CostAlgebra f cost -> ClassId -> ProofGraph graph f c p -> Maybe (ExtractionResult f cost)
extractGuidedWithProof projectBaseGraph guidanceConfig costAlgebraValue classId proofEGraph =
  let baseGraph = projectBaseGraph (pgGraph proofEGraph)
      canonicalClassId = canonicalizeClassId baseGraph classId
      graphCandidates =
        maybeToList
          (stableExtractionSnapshotFromEGraph baseGraph >>= extractGuided guidanceConfig costAlgebraValue canonicalClassId)
      proofCandidates =
        fmap
          (proofWitnessResult (Just guidanceConfig) costAlgebraValue canonicalClassId)
          (either (const []) id (proofClassWitnesses projectBaseGraph canonicalClassId proofEGraph))
   in minimumMaybe
        ( fmap
            (\extractionResult -> (extractionOrderingKey (Just guidanceConfig) extractionResult, extractionResult))
            (graphCandidates <> proofCandidates)
        )

extractGuidedWithAnalysis :: (Language f, ZipMatch f, Ord cost) => GuidanceConfig (Pattern f) -> AnalysisCostAlgebra f a cost -> ClassId -> StableExtractionSnapshot f a -> Maybe (ExtractionResult f cost)
extractGuidedWithAnalysis guidanceConfig costAlgebraValue classId snapshot =
  let table = stableExtractionSnapshotTable snapshot
   in extractionCanonicalClass table classId
        >>= \canonicalClassId ->
          IntMap.lookup (classIdKey canonicalClassId) (extractAllGuidedWithAnalysis guidanceConfig costAlgebraValue snapshot)

extractAllGuidedWithAnalysis :: (Language f, ZipMatch f, Ord cost) => GuidanceConfig (Pattern f) -> AnalysisCostAlgebra f a cost -> StableExtractionSnapshot f a -> IntMap (ExtractionResult f cost)
extractAllGuidedWithAnalysis guidanceConfig costAlgebraValue snapshot =
  let table = stableExtractionSnapshotTable snapshot
      stableChoices = extractChoices costAlgebraValue table
   in IntMap.mapMaybeWithKey
        (\classKey bestChoice -> extractionResultForClass (Just guidanceConfig) costAlgebraValue stableChoices table (ClassId classKey) bestChoice)
        stableChoices

extractionResultForClass :: (Language f, ZipMatch f, Ord cost) => Maybe (GuidanceConfig (Pattern f)) -> AnalysisCostAlgebra f a cost -> IntMap (Maybe (BestChoice f cost)) -> ExtractionTable f a -> ClassId -> Maybe (BestChoice f cost) -> Maybe (ExtractionResult f cost)
extractionResultForClass maybeGuidance costAlgebraValue stableChoices table classId maybeBestChoice =
  case maybeGuidance of
    Nothing ->
      maybeBestChoice >>= buildExtractionResult stableChoices table classId Set.empty
    Just guidanceConfig ->
      guidedExtractionResult guidanceConfig costAlgebraValue stableChoices table classId

proofWitnessResult :: Language f => Maybe (GuidanceConfig (Pattern f)) -> CostAlgebra f cost -> ClassId -> Fix f -> ExtractionResult f cost
proofWitnessResult _maybeGuidance costAlgebraValue classId witnessTerm =
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
