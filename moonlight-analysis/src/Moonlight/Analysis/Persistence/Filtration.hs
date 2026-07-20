module Moonlight.Analysis.Persistence.Filtration
  ( ObstructionBarcode (..),
    SpectralPersistencePoint (..),
    buildFiltration,
    persistentObstructions,
    obstructionBarcodes,
    spectralPersistence,
    spectralModePersistence,
    filtrationThresholds,
    subcomplexAtThreshold,
    spectralGapAtThreshold,
    spectralModesAtThreshold,
  )
where

import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Analysis.Spectral (gapFromModes)
import Moonlight.Homology
  ( BasisCellRef (..),
    FilteredFiniteChainComplex (..),
    FiltrationValue (..),
    FiniteChainComplex,
    GraphSpectralMode,
    HomologicalDegree (..),
    HomologyFailure (InvalidTopologyInput),
    PersistencePair (..),
    graph1SkeletonFromComplex,
    graphSpectralModes,
    mkFilteredFiniteChainComplex,
    mod2PersistentPairs,
    restrictComplex,
  )

type ObstructionBarcode :: Type
data ObstructionBarcode = ObstructionBarcode
  { obDegree :: HomologicalDegree,
    obBirth :: FiltrationValue,
    obDeath :: Maybe FiltrationValue,
    obLifetime :: Maybe Double
  }
  deriving stock (Eq, Show)

type SpectralPersistencePoint :: Type
data SpectralPersistencePoint = SpectralPersistencePoint
  { sppThreshold :: FiltrationValue,
    sppGapValue :: Maybe Double
  }
  deriving stock (Eq, Show)

buildFiltration ::
  FiniteChainComplex Int ->
  [(BasisCellRef, FiltrationValue)] ->
  Either HomologyFailure (FilteredFiniteChainComplex Int)
buildFiltration = mkFilteredFiniteChainComplex

persistentObstructions ::
  FiniteChainComplex Int ->
  [(BasisCellRef, FiltrationValue)] ->
  Either HomologyFailure [PersistencePair FiltrationValue]
persistentObstructions finiteComplex birthAssignments =
  mod2PersistentPairs =<< buildFiltration finiteComplex birthAssignments

obstructionBarcodes :: [PersistencePair FiltrationValue] -> [ObstructionBarcode]
obstructionBarcodes = fmap toBarcode

spectralPersistence ::
  FiniteChainComplex Int ->
  [(BasisCellRef, FiltrationValue)] ->
  Either HomologyFailure [SpectralPersistencePoint]
spectralPersistence finiteComplex birthAssignments = do
  filteredComplex <- buildFiltration finiteComplex birthAssignments
  let thresholds = filtrationThresholds filteredComplex
  traverse
    (\thresholdValue -> SpectralPersistencePoint thresholdValue <$> spectralGapAtThreshold filteredComplex thresholdValue)
    thresholds

spectralModePersistence ::
  FiniteChainComplex Int ->
  [(BasisCellRef, FiltrationValue)] ->
  Either HomologyFailure [(FiltrationValue, [GraphSpectralMode])]
spectralModePersistence finiteComplex birthAssignments = do
  filteredComplex <- buildFiltration finiteComplex birthAssignments
  traverse
    (\thresholdValue -> fmap ((,) thresholdValue) (spectralModesAtThreshold filteredComplex thresholdValue))
    (filtrationThresholds filteredComplex)

toBarcode :: PersistencePair FiltrationValue -> ObstructionBarcode
toBarcode pairValue =
  ObstructionBarcode
    { obDegree = persistenceDegree pairValue,
      obBirth = persistenceBirth pairValue,
      obDeath = persistenceDeath pairValue,
      obLifetime = lifetimeOf pairValue
    }

lifetimeOf :: PersistencePair FiltrationValue -> Maybe Double
lifetimeOf pairValue =
  fmap
    (\deathValue -> unFiltrationValue deathValue - unFiltrationValue (persistenceBirth pairValue))
    (persistenceDeath pairValue)

filtrationThresholds :: FilteredFiniteChainComplex Int -> [FiltrationValue]
filtrationThresholds filteredComplex =
  filteredCellBirths filteredComplex
    & Map.elems
    & Set.fromList
    & Set.toAscList

spectralGapAtThreshold :: FilteredFiniteChainComplex Int -> FiltrationValue -> Either HomologyFailure (Maybe Double)
spectralGapAtThreshold filteredComplex thresholdValue =
  gapFromModes <$> spectralModesAtThreshold filteredComplex thresholdValue

spectralModesAtThreshold :: FilteredFiniteChainComplex Int -> FiltrationValue -> Either HomologyFailure [GraphSpectralMode]
spectralModesAtThreshold filteredComplex thresholdValue =
  do
    thresholdComplex <- subcomplexAtThreshold filteredComplex thresholdValue
    skeletonValue <-
      first
        (InvalidTopologyInput . ("threshold subcomplex is not a graph 1-skeleton: " <>) . show)
        (graph1SkeletonFromComplex thresholdComplex)
    graphSpectralModes 2 skeletonValue

subcomplexAtThreshold :: FilteredFiniteChainComplex Int -> FiltrationValue -> Either HomologyFailure (FiniteChainComplex Int)
subcomplexAtThreshold filteredComplex thresholdValue =
  restrictComplex (retainedCells filteredComplex thresholdValue) (filteredBaseComplex filteredComplex)

retainedCells :: FilteredFiniteChainComplex Int -> FiltrationValue -> Set.Set BasisCellRef
retainedCells filteredComplex thresholdValue =
  filteredCellBirths filteredComplex
    & Map.toAscList
    & filter ((<= thresholdValue) . snd)
    & fmap fst
    & Set.fromList
