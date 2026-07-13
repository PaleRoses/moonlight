module Moonlight.Homology.Pure.Topology.Persistence
  ( mkFilteredFiniteChainComplex,
    mod2PersistentPairs,
    mod2PersistenceTopologyWitness,
    mod2PersistentBoundaryColumn,
    persistentPairs,
    persistenceTopologyWitness,
    persistentBoundaryColumn,
    persistenceEssentialBirths,
    reducePersistentColumn,
    reduceBoundaryColumn,
    materializeFinitePersistencePair,
    materializeEssentialPersistencePair,
    orderedFilteredCells,
    validateBirthCoverage,
    validateBirthExactness,
    validateFiltrationMonotonicity,
  )
where

import Data.Function ((&))
import Data.Kind (Type)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Moonlight.Homology.Boundary.Finite
  ( FiniteChainComplex,
    incidenceMatrixAt,
  )
import Moonlight.Homology.Boundary.LinAlg (boundaryCoefficient, boundaryEntries, sourceIndex, targetIndex)
import Moonlight.Homology.Pure.Chain
  ( HomologicalDegree (..),
    PersistencePair (..),
    TopologyWitness (..),
    decrementDegree,
    emptyTopologyWitness,
  )
import Moonlight.Homology.Pure.Failure (HomologyFailure (..))
import Moonlight.Homology.Pure.Topology.Core

type OrderedFilteredCell :: Type
data OrderedFilteredCell = OrderedFilteredCell
  { orderedCellIdentity :: BasisCellRef,
    orderedCellBirth :: FiltrationValue
  }
  deriving stock (Eq, Show)

type PersistenceState :: Type
data PersistenceState = PersistenceState
  { persistenceLowColumns :: Map.Map Int (Set.Set Int),
    persistencePairsByIndex :: [(Int, Int)],
    persistenceCreators :: Set.Set Int
  }

emptyPersistenceState :: PersistenceState
emptyPersistenceState =
  PersistenceState
    { persistenceLowColumns = Map.empty,
      persistencePairsByIndex = [],
      persistenceCreators = Set.empty
    }

mkFilteredFiniteChainComplex ::
  Integral r =>
  FiniteChainComplex r ->
  [(BasisCellRef, FiltrationValue)] ->
  Either HomologyFailure (FilteredFiniteChainComplex r)
mkFilteredFiniteChainComplex finite births = do
  let birthMap = Map.fromList births
  validateBirthUniqueness births birthMap
  validateBirthCoverage finite birthMap
  validateBirthExactness finite birthMap
  validateFiltrationMonotonicity finite birthMap
  pure
    FilteredFiniteChainComplex
      { filteredBaseComplex = finite,
        filteredCellBirths = birthMap
      }

mod2PersistentPairs ::
  Integral r =>
  FilteredFiniteChainComplex r ->
  Either HomologyFailure [PersistencePair FiltrationValue]
mod2PersistentPairs filtered = do
  let orderedCells = orderedFilteredCells filtered
      orderedCellByIndex =
        orderedCells
          & zip [0 :: Int ..]
          & Map.fromList
      globalIndexByCell =
        orderedCells
          & zip [0 :: Int ..]
          & fmap (\(globalIndexValue, orderedCell) -> (orderedCellIdentity orderedCell, globalIndexValue))
          & Map.fromList
      boundaryColumns = fmap (mod2PersistentBoundaryColumn filtered globalIndexByCell) orderedCells
      stateAfterReduction = foldl' reducePersistentColumn emptyPersistenceState (zip [0 :: Int ..] boundaryColumns)
      finitePairs =
        persistencePairsByIndex stateAfterReduction
          & reverse
          & mapMaybe (uncurry (materializeFinitePersistencePair orderedCellByIndex))
      essentialPairs =
        persistenceEssentialBirths stateAfterReduction
          & Set.toAscList
          & mapMaybe (materializeEssentialPersistencePair orderedCellByIndex)
  pure (finitePairs <> essentialPairs)

persistentPairs ::
  Integral r =>
  FilteredFiniteChainComplex r ->
  Either HomologyFailure [PersistencePair FiltrationValue]
{-# DEPRECATED persistentPairs "Use mod2PersistentPairs — this computes mod-2 persistence despite its Integral constraint" #-}
persistentPairs = mod2PersistentPairs

mod2PersistenceTopologyWitness ::
  Integral r =>
  FilteredFiniteChainComplex r ->
  Either HomologyFailure (TopologyWitness scaffold spectral FiltrationValue coefficient basis)
mod2PersistenceTopologyWitness filtered = do
  pairs <- mod2PersistentPairs filtered
  pure
    emptyTopologyWitness
      { topologyPersistencePairs = pairs
      }

persistenceTopologyWitness ::
  Integral r =>
  FilteredFiniteChainComplex r ->
  Either HomologyFailure (TopologyWitness scaffold spectral FiltrationValue coefficient basis)
{-# DEPRECATED persistenceTopologyWitness "Use mod2PersistenceTopologyWitness — this computes mod-2 persistence despite its Integral constraint" #-}
persistenceTopologyWitness = mod2PersistenceTopologyWitness

persistenceEssentialBirths :: PersistenceState -> Set.Set Int
persistenceEssentialBirths stateValue =
  let pairedBirths = persistencePairsByIndex stateValue & fmap fst & Set.fromList
   in persistenceCreators stateValue `Set.difference` pairedBirths

reducePersistentColumn :: PersistenceState -> (Int, Set.Set Int) -> PersistenceState
reducePersistentColumn stateValue (columnIndexValue, initialColumn) =
  let reducedColumn = reduceBoundaryColumn (persistenceLowColumns stateValue) initialColumn
   in case lowIndex reducedColumn of
        Nothing ->
          stateValue
            { persistenceCreators = Set.insert columnIndexValue (persistenceCreators stateValue)
            }
        Just lowValue ->
          stateValue
            { persistenceLowColumns = Map.insert lowValue reducedColumn (persistenceLowColumns stateValue),
              persistencePairsByIndex = (lowValue, columnIndexValue) : persistencePairsByIndex stateValue
            }

reduceBoundaryColumn :: Map.Map Int (Set.Set Int) -> Set.Set Int -> Set.Set Int
reduceBoundaryColumn lowColumns columnValue =
  case lowIndex columnValue >>= (`Map.lookup` lowColumns) of
    Nothing -> columnValue
    Just pivotColumn -> reduceBoundaryColumn lowColumns (symmetricDifference columnValue pivotColumn)

materializeFinitePersistencePair ::
  Map.Map Int OrderedFilteredCell ->
  Int ->
  Int ->
  Maybe (PersistencePair FiltrationValue)
materializeFinitePersistencePair orderedCellByIndex birthIndexValue deathIndexValue =
  case (Map.lookup birthIndexValue orderedCellByIndex, Map.lookup deathIndexValue orderedCellByIndex) of
    (Just birthCell, Just deathCell) ->
      Just
        PersistencePair
          { persistenceDegree = cellDegree (orderedCellIdentity birthCell),
            persistenceBirth = orderedCellBirth birthCell,
            persistenceDeath = Just (orderedCellBirth deathCell)
          }
    _ -> Nothing

materializeEssentialPersistencePair ::
  Map.Map Int OrderedFilteredCell ->
  Int ->
  Maybe (PersistencePair FiltrationValue)
materializeEssentialPersistencePair orderedCellByIndex birthIndexValue =
  Map.lookup birthIndexValue orderedCellByIndex
    & fmap
      ( \birthCell ->
          PersistencePair
            { persistenceDegree = cellDegree (orderedCellIdentity birthCell),
              persistenceBirth = orderedCellBirth birthCell,
              persistenceDeath = Nothing
            }
      )

orderedFilteredCells :: FilteredFiniteChainComplex r -> [OrderedFilteredCell]
orderedFilteredCells filtered =
  allBasisCellRefs (filteredBaseComplex filtered)
    & mapMaybe
      ( \cellRefValue ->
          fmap (OrderedFilteredCell cellRefValue)
            (Map.lookup cellRefValue (filteredCellBirths filtered))
      )
    & List.sortOn
      ( \orderedCell ->
          let degreeValue = cellDegree (orderedCellIdentity orderedCell)
           in ( orderedCellBirth orderedCell,
                unHomologicalDegree degreeValue,
                cellIndex (orderedCellIdentity orderedCell)
              )
      )

mod2PersistentBoundaryColumn ::
  Integral r =>
  FilteredFiniteChainComplex r ->
  Map.Map BasisCellRef Int ->
  OrderedFilteredCell ->
  Set.Set Int
mod2PersistentBoundaryColumn filtered globalIndexByCell orderedCell =
  let cellRefValue = orderedCellIdentity orderedCell
      degreeValue = cellDegree cellRefValue
      incidence = incidenceMatrixAt (filteredBaseComplex filtered) degreeValue
   in boundaryEntries incidence
        & filter (\entry -> sourceIndex entry == cellIndex cellRefValue)
        & filter (\entry -> odd (abs (boundaryCoefficient entry)))
        & fmap
          ( \entry ->
              BasisCellRef
                { cellDegree = decrementDegree degreeValue,
                  cellIndex = targetIndex entry
                }
          )
        & mapMaybeWithLookup globalIndexByCell
        & Set.fromList

persistentBoundaryColumn ::
  Integral r =>
  FilteredFiniteChainComplex r ->
  Map.Map BasisCellRef Int ->
  OrderedFilteredCell ->
  Set.Set Int
{-# DEPRECATED persistentBoundaryColumn "Use mod2PersistentBoundaryColumn — this computes mod-2 boundary despite its Integral constraint" #-}
persistentBoundaryColumn = mod2PersistentBoundaryColumn

validateBirthUniqueness ::
  [(BasisCellRef, FiltrationValue)] ->
  Map.Map BasisCellRef FiltrationValue ->
  Either HomologyFailure ()
validateBirthUniqueness births birthMap =
  if length births == Map.size birthMap
    then Right ()
    else Left (InvalidTopologyInput "duplicate birth assignments for the same cell")

validateBirthCoverage ::
  FiniteChainComplex r ->
  Map.Map BasisCellRef FiltrationValue ->
  Either HomologyFailure ()
validateBirthCoverage finite birthMap =
  allBasisCellRefs finite
    & List.find (\cellRefValue -> Map.notMember cellRefValue birthMap)
    & maybe (Right ()) missingCellFailure
  where
    missingCellFailure :: BasisCellRef -> Either HomologyFailure ()
    missingCellFailure cellRefValue =
      Left
        ( InvalidTopologyInput
            ( "missing filtration value for cell "
                <> show cellRefValue
            )
        )

validateBirthExactness ::
  FiniteChainComplex r ->
  Map.Map BasisCellRef FiltrationValue ->
  Either HomologyFailure ()
validateBirthExactness finite birthMap =
  let basisSet = Set.fromList (allBasisCellRefs finite)
      extraKeys = Map.keysSet birthMap `Set.difference` basisSet
   in if Set.null extraKeys
        then Right ()
        else
          Left
            ( InvalidTopologyInput
                ( "birth map contains cells absent from the chain complex: "
                    <> show (Set.toList extraKeys)
                )
            )

validateFiltrationMonotonicity ::
  Integral r =>
  FiniteChainComplex r ->
  Map.Map BasisCellRef FiltrationValue ->
  Either HomologyFailure ()
validateFiltrationMonotonicity finite birthMap =
  filtrationViolations
    & List.find (const True)
    & maybe (Right ()) (Left . InvalidTopologyInput)
  where
    filtrationViolations =
      dimensionsOf finite
        >>= ( \degreeValue@(HomologicalDegree degreeIndex) ->
                if degreeIndex <= 0
                  then []
                  else
                    let incidence = incidenceMatrixAt finite degreeValue
                     in boundaryEntries incidence
                          & filter (\entry -> boundaryCoefficient entry /= 0)
                          & mapMaybe
                            ( \entry ->
                                let sourceCell =
                                      BasisCellRef
                                        { cellDegree = degreeValue,
                                          cellIndex = sourceIndex entry
                                        }
                                    targetCell =
                                      BasisCellRef
                                        { cellDegree = decrementDegree degreeValue,
                                          cellIndex = targetIndex entry
                                        }
                                 in case (Map.lookup sourceCell birthMap, Map.lookup targetCell birthMap) of
                                      (Just sourceBirth, Just targetBirth) ->
                                        if targetBirth <= sourceBirth
                                          then Nothing
                                          else
                                            Just
                                              ( "filtration violates face monotonicity for "
                                                  <> show sourceCell
                                                  <> " -> "
                                                  <> show targetCell
                                              )
                                      _ -> Nothing
                            )
           )
