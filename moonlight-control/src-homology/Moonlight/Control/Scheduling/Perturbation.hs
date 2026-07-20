{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Control.Scheduling.Perturbation
  ( PerturbationSample (..),
    MicrolocalInvalidationFailure (..),
    MicrolocalMerge,
    MicrolocalSpectralInvalidation,
    mkMicrolocalMerge,
    microlocalInvalidationNeighborhood,
    microlocalSpectralRefreshRequired,
    microlocalSpectralInvalidation,
    influenceEdgeSupports,
    perturbationSample,
    perturbationSampleWithMicrolocalGate,
    sampleObservedEdgeCoverage,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Moonlight.Homology
  ( GraphSpectralMode,
    HomologyFailure,
  )
import Moonlight.Graph.Pure.LocalTopology
  ( LocalAdj,
    LocalTopologyError,
    buildLocalAdjFromChildren,
    mergeCreatesNewCycle,
    mergeTopologyFromAdj,
    mergeTopologyNeighborhood,
  )
import Moonlight.Homology.Sequence
  ( defaultSparseSpectralConfig,
    gapFromModes,
    weightedGraphSparseSpectralModes,
  )
import Moonlight.Control.Schedule
  ( SchedulerConfig,
  )
import Moonlight.Control.Scheduling.Successor
  ( InfluenceComplex,
    SuccessorEdge,
    SuccessorNode (snContext, snRule),
    ricEdgeInfluences,
    ricSuccessorComplex,
    rscNodeOrdinals,
    schedulerInfluenceWeightRatio,
    seSource,
    seTarget,
  )

data PerturbationSample scope key = PerturbationSample
  { psScope :: !scope,
    psPolicyConfig :: !(SchedulerConfig key),
    psStructuralEdgeCount :: !Int,
    psEffectiveEdgeCount :: !Int,
    psObservedEdgeCount :: !(Maybe Int),
    psSpectralGap :: !(Maybe Double),
    psLeadingModes :: ![GraphSpectralMode]
  }

deriving stock instance (Eq scope, Eq key) => Eq (PerturbationSample scope key)

deriving stock instance (Show scope, Show key) => Show (PerturbationSample scope key)

type MicrolocalMerge :: Type
data MicrolocalMerge = MicrolocalMerge
  { mmLeftCell :: !Int,
    mmRightCell :: !Int
  }
  deriving stock (Eq, Ord, Show, Read)

data MicrolocalInvalidationFailure
  = NegativeMicrolocalNodeCount !Int
  | MicrolocalSupportEndpointOutOfBounds !Int !Int !Int
  | MicrolocalMergeEndpointOutOfBounds !Int !(Int, Int)
  | MicrolocalTopologyFailure !LocalTopologyError
  deriving stock (Eq, Ord, Show, Read)

type MicrolocalSpectralInvalidation :: Type
data MicrolocalSpectralInvalidation = MicrolocalSpectralInvalidation
  { msiDirtyNeighborhood :: !IntSet,
    msiRefreshRequired :: !Bool
  }
  deriving stock (Eq, Show)

mkMicrolocalMerge :: Int -> Int -> Maybe MicrolocalMerge
mkMicrolocalMerge leftCell rightCell
  | leftCell < 0 || rightCell < 0 = Nothing
  | otherwise =
      Just
        MicrolocalMerge
          { mmLeftCell = leftCell,
            mmRightCell = rightCell
          }

microlocalMergeEndpoints :: MicrolocalMerge -> (Int, Int)
microlocalMergeEndpoints mergeValue =
  (mmLeftCell mergeValue, mmRightCell mergeValue)

microlocalInvalidationNeighborhood :: MicrolocalSpectralInvalidation -> IntSet
microlocalInvalidationNeighborhood =
  msiDirtyNeighborhood

microlocalSpectralRefreshRequired :: MicrolocalSpectralInvalidation -> Bool
microlocalSpectralRefreshRequired =
  msiRefreshRequired

influenceEdgeSupports ::
  (Ord context, Ord rule) =>
  InfluenceComplex key context rule runtimeRule composite compositionObstruction ->
  (SuccessorEdge context rule runtimeRule composite -> Double) ->
  [(Int, Int, Double)]
influenceEdgeSupports influenceComplex runtimeWeight =
  ricEdgeInfluences influenceComplex
    & mapMaybe
      ( \(edgeValue, influenceValue) ->
          let effectiveWeight = fromRational (schedulerInfluenceWeightRatio influenceValue) * runtimeWeight edgeValue
           in if effectiveWeight <= 0.0
                then Nothing
                else
                  (\sourceIndex targetIndex -> (sourceIndex, targetIndex, effectiveWeight))
                    <$> edgeEndpointIndex (seSource edgeValue)
                    <*> edgeEndpointIndex (seTarget edgeValue)
    )
  where
    nodeIndex =
      rscNodeOrdinals (ricSuccessorComplex influenceComplex)

    edgeEndpointIndex targetNode =
      Map.lookup (snContext targetNode, snRule targetNode) nodeIndex

microlocalSpectralInvalidation ::
  Int ->
  [(Int, Int, Double)] ->
  [MicrolocalMerge] ->
  Either MicrolocalInvalidationFailure MicrolocalSpectralInvalidation
microlocalSpectralInvalidation nodeCount supports dirtyMerges
  | nodeCount < 0 = Left (NegativeMicrolocalNodeCount nodeCount)
  | otherwise = do
      traverse_ (validateSupportEndpoint nodeCount) supports
      traverse_ (validateMergeEndpoint nodeCount) dirtyMerges
      adjacency <- first MicrolocalTopologyFailure (supportAdjacency nodeCount supports)
      let topologies =
            fmap
              (\mergeValue ->
                 let (leftCell, rightCell) = microlocalMergeEndpoints mergeValue
                  in mergeTopologyFromAdj leftCell rightCell adjacency
              )
              dirtyMerges
      pure
        MicrolocalSpectralInvalidation
          { msiDirtyNeighborhood = foldMap mergeTopologyNeighborhood topologies,
            msiRefreshRequired = any mergeCreatesNewCycle topologies
          }

validateSupportEndpoint :: Int -> (Int, Int, Double) -> Either MicrolocalInvalidationFailure ()
validateSupportEndpoint nodeCount (sourceCell, targetCell, _weightValue) =
  if cellInBounds nodeCount sourceCell && cellInBounds nodeCount targetCell
    then Right ()
    else Left (MicrolocalSupportEndpointOutOfBounds nodeCount sourceCell targetCell)

validateMergeEndpoint :: Int -> MicrolocalMerge -> Either MicrolocalInvalidationFailure ()
validateMergeEndpoint nodeCount mergeValue =
  let endpoints@(leftCell, rightCell) = microlocalMergeEndpoints mergeValue
   in if cellInBounds nodeCount leftCell && cellInBounds nodeCount rightCell
        then Right ()
        else Left (MicrolocalMergeEndpointOutOfBounds nodeCount endpoints)

cellInBounds :: Int -> Int -> Bool
cellInBounds nodeCount cellValue =
  cellValue >= 0 && cellValue < nodeCount

supportAdjacency ::
  Int ->
  [(Int, Int, Double)] ->
  Either LocalTopologyError (IntMap LocalAdj)
supportAdjacency nodeCount supports =
  buildLocalAdjFromChildren childrenByParent
  where
    activeSupports =
      filter positiveNonLoopSupport supports

    childrenByParent =
      IntMap.unionWith
        (IntMap.unionWith (+))
        (IntMap.fromList [(cellValue, IntMap.empty) | cellValue <- [0 .. nodeCount - 1]])
        ( IntMap.fromListWith
            (IntMap.unionWith (+))
            [ (sourceCell, IntMap.singleton targetCell (1 :: Int))
            | (sourceCell, targetCell, _weightValue) <- activeSupports
            ]
        )

positiveNonLoopSupport :: (Int, Int, Double) -> Bool
positiveNonLoopSupport (sourceCell, targetCell, weightValue) =
  weightValue > 0.0 && sourceCell /= targetCell

perturbationSample ::
  scope ->
  SchedulerConfig key ->
  Int ->
  Int ->
  Maybe Int ->
  [(Int, Int, Double)] ->
  Either HomologyFailure (PerturbationSample scope key)
perturbationSample scopeValue schedulerConfig nodeCount structuralEdgeCount observedEdgeCount supports =
  fmap
    ( perturbationSampleFromModes
        scopeValue
        schedulerConfig
        structuralEdgeCount
        observedEdgeCount
        supports
    )
    (weightedGraphSparseSpectralModes defaultSparseSpectralConfig 2 nodeCount supports)

perturbationSampleWithMicrolocalGate ::
  MicrolocalSpectralInvalidation ->
  scope ->
  SchedulerConfig key ->
  Int ->
  Int ->
  Maybe Int ->
  [(Int, Int, Double)] ->
  Either HomologyFailure (PerturbationSample scope key)
perturbationSampleWithMicrolocalGate invalidationValue scopeValue schedulerConfig nodeCount structuralEdgeCount observedEdgeCount supports
  | microlocalSpectralRefreshRequired invalidationValue =
      perturbationSample
        scopeValue
        schedulerConfig
        nodeCount
        structuralEdgeCount
        observedEdgeCount
        supports
  | otherwise =
      Right
        ( perturbationSampleFromModes
            scopeValue
            schedulerConfig
            structuralEdgeCount
            observedEdgeCount
            supports
            []
        )

perturbationSampleFromModes ::
  scope ->
  SchedulerConfig key ->
  Int ->
  Maybe Int ->
  [(Int, Int, Double)] ->
  [GraphSpectralMode] ->
  PerturbationSample scope key
perturbationSampleFromModes scopeValue schedulerConfig structuralEdgeCount observedEdgeCount supports modeValues =
  PerturbationSample
    { psScope = scopeValue,
      psPolicyConfig = schedulerConfig,
      psStructuralEdgeCount = structuralEdgeCount,
      psEffectiveEdgeCount = length supports,
      psObservedEdgeCount = observedEdgeCount,
      psSpectralGap = gapFromModes modeValues,
      psLeadingModes = modeValues
    }

sampleObservedEdgeCoverage :: PerturbationSample scope key -> Maybe Double
sampleObservedEdgeCoverage sample =
  if psStructuralEdgeCount sample <= 0
    then Nothing
    else
      fmap
        (\observedEdgeCount -> fromIntegral observedEdgeCount / fromIntegral (psStructuralEdgeCount sample))
        (psObservedEdgeCount sample)
