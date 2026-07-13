module Moonlight.EGraph.Pure.Extraction.Compile
  ( extract,
    extractBounded,
    extractAll,
    extractAllBounded,
    extractWithAnalysis,
    extractWithAnalysisBounded,
    extractAllWithAnalysis,
    extractAllWithAnalysisBounded,
  )
where

import Data.IntMap.Strict (IntMap)
import Moonlight.Core (Language)
import Moonlight.EGraph.Pure.Extraction.Algebra
  ( extractAllFromTable,
    extractAllFromTableBounded,
    extractFromTable,
    extractFromTableBounded,
  )
import Moonlight.EGraph.Pure.Extraction.Core
  ( AnalysisCostAlgebra,
    CostAlgebra,
    ExtractionConvergenceReport,
    ExtractionFixpointBudget,
    ExtractionResult,
    StableExtractionSnapshot,
    liftCostAlgebra,
    stableExtractionSnapshotTable,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId,
  )

extract :: (Language f, Ord cost) => CostAlgebra f cost -> ClassId -> StableExtractionSnapshot f a -> Maybe (ExtractionResult f cost)
extract costAlgebraValue classId =
  extractWithAnalysis (liftCostAlgebra costAlgebraValue) classId

extractBounded :: (Language f, Ord cost) => ExtractionFixpointBudget -> CostAlgebra f cost -> ClassId -> StableExtractionSnapshot f a -> Either ExtractionConvergenceReport (Maybe (ExtractionResult f cost))
extractBounded budget costAlgebraValue classId =
  extractWithAnalysisBounded budget (liftCostAlgebra costAlgebraValue) classId

extractAll :: (Language f, Ord cost) => CostAlgebra f cost -> StableExtractionSnapshot f a -> IntMap (ExtractionResult f cost)
extractAll costAlgebraValue =
  extractAllWithAnalysis (liftCostAlgebra costAlgebraValue)

extractAllBounded :: (Language f, Ord cost) => ExtractionFixpointBudget -> CostAlgebra f cost -> StableExtractionSnapshot f a -> Either ExtractionConvergenceReport (IntMap (ExtractionResult f cost))
extractAllBounded budget costAlgebraValue =
  extractAllWithAnalysisBounded budget (liftCostAlgebra costAlgebraValue)

extractWithAnalysis :: (Language f, Ord cost) => AnalysisCostAlgebra f a cost -> ClassId -> StableExtractionSnapshot f a -> Maybe (ExtractionResult f cost)
extractWithAnalysis costAlgebraValue classId =
  extractFromTable costAlgebraValue classId . stableExtractionSnapshotTable

extractWithAnalysisBounded :: (Language f, Ord cost) => ExtractionFixpointBudget -> AnalysisCostAlgebra f a cost -> ClassId -> StableExtractionSnapshot f a -> Either ExtractionConvergenceReport (Maybe (ExtractionResult f cost))
extractWithAnalysisBounded budget costAlgebraValue classId =
  extractFromTableBounded budget costAlgebraValue classId . stableExtractionSnapshotTable

extractAllWithAnalysis :: (Language f, Ord cost) => AnalysisCostAlgebra f a cost -> StableExtractionSnapshot f a -> IntMap (ExtractionResult f cost)
extractAllWithAnalysis costAlgebraValue =
  extractAllFromTable costAlgebraValue . stableExtractionSnapshotTable

extractAllWithAnalysisBounded :: (Language f, Ord cost) => ExtractionFixpointBudget -> AnalysisCostAlgebra f a cost -> StableExtractionSnapshot f a -> Either ExtractionConvergenceReport (IntMap (ExtractionResult f cost))
extractAllWithAnalysisBounded budget costAlgebraValue =
  extractAllFromTableBounded budget costAlgebraValue . stableExtractionSnapshotTable
