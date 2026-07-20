{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Graph.Pure.CycleClosure
  ( CycleClosureEdge (..),
    CycleClosure (..),
    CycleClosureState,
    emptyCycleClosureState,
    insertCycleClosureEdge,
    insertCycleClosureEdges,
    cycleClosureStateClosures,
    cycleClosureStateRecentClosures,
  )
where

import Data.Foldable qualified as Foldable
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set

-- | A typed edge insertion in a dynamic undirected forest, with domain payload
-- retained for the closure witness emitted when the insertion closes a cycle.
type CycleClosureEdge :: Type -> Type -> Type
data CycleClosureEdge vertex payload = CycleClosureEdge
  { cceSource :: !vertex,
    cceTarget :: !vertex,
    ccePayload :: !payload
  }
  deriving stock (Eq, Ord, Show)

-- | The local obstruction created by an edge whose endpoints were already
-- connected by the maintained forest.
type CycleClosure :: Type -> Type -> Type
data CycleClosure vertex payload = CycleClosure
  { ccInsertedEdge :: !(CycleClosureEdge vertex payload),
    ccMemberVertices :: !(Set vertex)
  }
  deriving stock (Eq, Ord, Show)

-- | Incremental cycle-closure state. This is not a link-cut tree; it is a
-- component-indexed forest with explicit closure witnesses.
type CycleClosureState :: Type -> Type -> Type
data CycleClosureState vertex payload = CycleClosureState
  { ccsVertexComponent :: !(Map vertex Int),
    ccsComponentMembers :: !(IntMap (Set vertex)),
    ccsForestAdjacency :: !(Map vertex (Set vertex)),
    ccsCycleClosures :: ![CycleClosure vertex payload],
    ccsNextComponent :: !Int
  }
  deriving stock (Eq, Show)

emptyCycleClosureState :: CycleClosureState vertex payload
emptyCycleClosureState =
  CycleClosureState
    { ccsVertexComponent = Map.empty,
      ccsComponentMembers = IntMap.empty,
      ccsForestAdjacency = Map.empty,
      ccsCycleClosures = [],
      ccsNextComponent = 0
    }

insertCycleClosureEdge ::
  Ord vertex =>
  CycleClosureEdge vertex payload ->
  CycleClosureState vertex payload ->
  (CycleClosureState vertex payload, Maybe (CycleClosure vertex payload))
insertCycleClosureEdge edge state =
  let stateWithVertices = ensureVertex (cceTarget edge) (ensureVertex (cceSource edge) state)
   in if sameComponent (cceSource edge) (cceTarget edge) stateWithVertices
        then
          let closure = cycleClosureFor edge stateWithVertices
           in (stateWithVertices {ccsCycleClosures = closure : ccsCycleClosures stateWithVertices}, Just closure)
        else
          (linkComponents (cceSource edge) (cceTarget edge) stateWithVertices, Nothing)

insertCycleClosureEdges ::
  (Foldable edges, Ord vertex) =>
  edges (CycleClosureEdge vertex payload) ->
  CycleClosureState vertex payload ->
  (CycleClosureState vertex payload, [CycleClosure vertex payload])
insertCycleClosureEdges edges state =
  let (nextState, reverseClosures) =
        Foldable.foldl'
          ( \(stateAcc, closureAcc) edge ->
              case insertCycleClosureEdge edge stateAcc of
                (edgeState, Just closure) -> (edgeState, closure : closureAcc)
                (edgeState, Nothing) -> (edgeState, closureAcc)
          )
          (state, [])
          edges
   in (nextState, reverse reverseClosures)

cycleClosureStateClosures :: CycleClosureState vertex payload -> [CycleClosure vertex payload]
cycleClosureStateClosures =
  reverse . ccsCycleClosures

cycleClosureStateRecentClosures :: CycleClosureState vertex payload -> [CycleClosure vertex payload]
cycleClosureStateRecentClosures =
  ccsCycleClosures

ensureVertex :: Ord vertex => vertex -> CycleClosureState vertex payload -> CycleClosureState vertex payload
ensureVertex vertex state =
  case Map.lookup vertex (ccsVertexComponent state) of
    Just _componentKey -> state
    Nothing ->
      let componentKey = ccsNextComponent state
       in state
            { ccsVertexComponent = Map.insert vertex componentKey (ccsVertexComponent state),
              ccsComponentMembers = IntMap.insert componentKey (Set.singleton vertex) (ccsComponentMembers state),
              ccsForestAdjacency = Map.insertWith Set.union vertex Set.empty (ccsForestAdjacency state),
              ccsNextComponent = componentKey + 1
            }

sameComponent :: Ord vertex => vertex -> vertex -> CycleClosureState vertex payload -> Bool
sameComponent source target state =
  case (Map.lookup source (ccsVertexComponent state), Map.lookup target (ccsVertexComponent state)) of
    (Just sourceComponent, Just targetComponent) -> sourceComponent == targetComponent
    _ -> False

linkComponents :: Ord vertex => vertex -> vertex -> CycleClosureState vertex payload -> CycleClosureState vertex payload
linkComponents source target state =
  let stateWithVertices = ensureVertex target (ensureVertex source state)
   in case (Map.lookup source (ccsVertexComponent stateWithVertices), Map.lookup target (ccsVertexComponent stateWithVertices)) of
        (Just sourceComponent, Just targetComponent) ->
          let sourceMembers = IntMap.findWithDefault (Set.singleton source) sourceComponent (ccsComponentMembers stateWithVertices)
              targetMembers = IntMap.findWithDefault (Set.singleton target) targetComponent (ccsComponentMembers stateWithVertices)
              (survivorComponent, survivorMembers, retiredComponent, retiredMembers) =
                if Set.size sourceMembers >= Set.size targetMembers
                  then (sourceComponent, sourceMembers, targetComponent, targetMembers)
                  else (targetComponent, targetMembers, sourceComponent, sourceMembers)
              mergedMembers = Set.union survivorMembers retiredMembers
              vertexComponents =
                Set.foldr
                  (`Map.insert` survivorComponent)
                  (ccsVertexComponent stateWithVertices)
                  retiredMembers
           in stateWithVertices
                { ccsVertexComponent = vertexComponents,
                  ccsComponentMembers =
                    IntMap.insert
                      survivorComponent
                      mergedMembers
                      (IntMap.delete retiredComponent (ccsComponentMembers stateWithVertices)),
                  ccsForestAdjacency = insertForestEdge source target (ccsForestAdjacency stateWithVertices)
                }
        _ -> stateWithVertices

insertForestEdge :: Ord vertex => vertex -> vertex -> Map vertex (Set vertex) -> Map vertex (Set vertex)
insertForestEdge source target =
  Map.insertWith Set.union source (Set.singleton target)
    . Map.insertWith Set.union target (Set.singleton source)

cycleClosureFor :: Ord vertex => CycleClosureEdge vertex payload -> CycleClosureState vertex payload -> CycleClosure vertex payload
cycleClosureFor edge state =
  let memberVertices = shortestForestPathMembers (cceSource edge) (cceTarget edge) (ccsForestAdjacency state)
      closureMembers =
        if Set.null memberVertices
          then Set.fromList [cceSource edge, cceTarget edge]
          else memberVertices
   in CycleClosure
        { ccInsertedEdge = edge,
          ccMemberVertices = closureMembers
        }

shortestForestPathMembers :: Ord vertex => vertex -> vertex -> Map vertex (Set vertex) -> Set vertex
shortestForestPathMembers source target adjacency
  | source == target = Set.singleton source
  | otherwise =
      findPath
        (Seq.singleton source)
        (Set.singleton source)
        Map.empty
  where
    findPath queue visited parents =
      case Seq.viewl queue of
        Seq.EmptyL -> Set.empty
        current Seq.:< rest
          | current == target -> reconstructPath source target parents
          | otherwise ->
              let neighbours = Set.difference (Map.findWithDefault Set.empty current adjacency) visited
                  (nextQueue, nextVisited, nextParents) =
                    Set.foldr
                      ( \neighbour (queueAcc, visitedAcc, parentsAcc) ->
                          ( queueAcc Seq.|> neighbour,
                            Set.insert neighbour visitedAcc,
                            Map.insert neighbour current parentsAcc
                          )
                      )
                      (rest, visited, parents)
                      neighbours
               in findPath nextQueue nextVisited nextParents

reconstructPath :: Ord vertex => vertex -> vertex -> Map vertex vertex -> Set vertex
reconstructPath source target parents =
  walk target Set.empty
  where
    walk current acc
      | current == source = Set.insert source acc
      | otherwise =
          case Map.lookup current parents of
            Just parent -> walk parent (Set.insert current acc)
            Nothing -> Set.insert source (Set.insert current acc)
