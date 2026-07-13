{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Differential.Effect.Harness.WCOJ
  ( TestGenericWCOJProblem (..),
    genericWCOJSlots,
    genericWCOJEnvSamples,
    genericWCOJAlgebra,
    indexedGenericWCOJAlgebra,
    genericWCOJDenotation,
    adaptiveWCOJDenotation,
    indexedAdaptiveWCOJDenotation,
    fusedIndexedWCOJDenotation,
    foldAdaptiveWCOJDenotation,
    bruteForceWCOJDenotation,
    bruteForceTriangleCount,
    normalizedEdgeSet,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List
  ( tails,
  )
import Data.Maybe
  ( mapMaybe,
  )
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set

import Moonlight.Differential.Join.WCOJ
  ( Env,
    IntIndexedJoinProblem,
    JoinAlgebra (..),
    Slot,
    adaptiveJoin,
    domainFromListPreservingOrder,
    domainSize,
    foldAdaptiveJoin,
    foldGenericJoin,
    foldIntIndexedAdaptiveJoin,
    intBinaryConstraintIndex,
    intBinaryRelationIndexFromList,
    intIndexedJoinAlgebra,
    intIndexedJoinProblem,
  )
import Moonlight.Differential.Join.WCOJ.Dense.Triangle
  ( normalizeUndirectedEdge,
  )

data TestGenericWCOJProblem = TestGenericWCOJProblem
  { tgwUniverse :: !(Set Int),
    tgwRel01 :: !(Set (Int, Int)),
    tgwRel02 :: !(Set (Int, Int)),
    tgwRel12 :: !(Set (Int, Int))
  }
  deriving stock (Eq, Show)

genericWCOJSlots :: [Slot]
genericWCOJSlots =
  [0, 1, 2]

genericWCOJEnvSamples :: TestGenericWCOJProblem -> [Env Int]
genericWCOJEnvSamples problem =
  IntMap.empty
    : [ IntMap.singleton slot value
      | slot <- genericWCOJSlots,
        value <- Set.toAscList (tgwUniverse problem)
      ]

genericWCOJAlgebra :: JoinAlgebra TestGenericWCOJProblem Int
genericWCOJAlgebra =
  JoinAlgebra
    { joinCount = \problem env slot -> domainSize (joinPropose genericWCOJAlgebra problem env slot),
      joinPropose =
        \problem env slot ->
          domainFromListPreservingOrder
            ( Set.toAscList
                (Set.filter (candidateAllowed problem env slot) (tgwUniverse problem))
            ),
      joinValidate = problemWitness
    }

indexedGenericWCOJAlgebra :: JoinAlgebra TestGenericWCOJProblem Int
indexedGenericWCOJAlgebra =
  JoinAlgebra
    { joinCount =
        \problem ->
          joinCount intIndexedJoinAlgebra (indexedWCOJProblem problem),
      joinPropose =
        \problem ->
          joinPropose intIndexedJoinAlgebra (indexedWCOJProblem problem),
      joinValidate =
        \problem ->
          joinValidate intIndexedJoinAlgebra (indexedWCOJProblem problem)
    }

genericWCOJDenotation :: TestGenericWCOJProblem -> Set (Int, Int, Int)
genericWCOJDenotation problem =
  Set.fromList
    ( mapMaybe
        envAssignment
        ( foldGenericJoin
            genericWCOJAlgebra
            problem
            genericWCOJSlots
            IntMap.empty
            (\envs env -> env : envs)
            []
        )
    )

adaptiveWCOJDenotation :: TestGenericWCOJProblem -> Set (Int, Int, Int)
adaptiveWCOJDenotation problem =
  Set.fromList
    (mapMaybe envAssignment (adaptiveJoin genericWCOJAlgebra problem genericWCOJSlots IntMap.empty))

indexedAdaptiveWCOJDenotation :: TestGenericWCOJProblem -> Set (Int, Int, Int)
indexedAdaptiveWCOJDenotation problem =
  Set.fromList
    (mapMaybe envAssignment (adaptiveJoin indexedGenericWCOJAlgebra problem genericWCOJSlots IntMap.empty))

fusedIndexedWCOJDenotation :: TestGenericWCOJProblem -> Set (Int, Int, Int)
fusedIndexedWCOJDenotation problem =
  Set.fromList
    ( mapMaybe
        envAssignment
        ( foldIntIndexedAdaptiveJoin
            (indexedWCOJProblem problem)
            genericWCOJSlots
            IntMap.empty
            (\envs env -> env : envs)
            []
        )
    )

foldAdaptiveWCOJDenotation :: TestGenericWCOJProblem -> Set (Int, Int, Int)
foldAdaptiveWCOJDenotation problem =
  Set.fromList
    ( mapMaybe
        envAssignment
        ( foldAdaptiveJoin
            genericWCOJAlgebra
            problem
            genericWCOJSlots
            IntMap.empty
            (\envs env -> env : envs)
            []
        )
    )

bruteForceWCOJDenotation :: TestGenericWCOJProblem -> Set (Int, Int, Int)
bruteForceWCOJDenotation problem =
  Set.fromList
    (filter (assignmentWitness problem) (candidateAssignments (Set.toAscList (tgwUniverse problem))))

candidateAssignments :: [Int] -> [(Int, Int, Int)]
candidateAssignments values =
  (,,) <$> values <*> values <*> values

envAssignment :: Env Int -> Maybe (Int, Int, Int)
envAssignment env =
  (,,)
    <$> IntMap.lookup 0 env
    <*> IntMap.lookup 1 env
    <*> IntMap.lookup 2 env

problemWitness :: TestGenericWCOJProblem -> Env Int -> Bool
problemWitness problem env =
  maybe False (assignmentWitness problem) (envAssignment env)

assignmentWitness :: TestGenericWCOJProblem -> (Int, Int, Int) -> Bool
assignmentWitness problem (value0, value1, value2) =
  Set.member (value0, value1) (tgwRel01 problem)
    && Set.member (value0, value2) (tgwRel02 problem)
    && Set.member (value1, value2) (tgwRel12 problem)

candidateAllowed :: TestGenericWCOJProblem -> Env Int -> Slot -> Int -> Bool
candidateAllowed problem env slot candidate =
  all (constraintAllows env slot candidate) (problemConstraints problem)

problemConstraints :: TestGenericWCOJProblem -> [(Slot, Slot, Set (Int, Int))]
problemConstraints problem =
  [ (0, 1, tgwRel01 problem),
    (0, 2, tgwRel02 problem),
    (1, 2, tgwRel12 problem)
  ]

indexedWCOJProblem :: TestGenericWCOJProblem -> IntIndexedJoinProblem
indexedWCOJProblem problem =
  intIndexedJoinProblem
    (IntSet.fromAscList (Set.toAscList (tgwUniverse problem)))
    [ intBinaryConstraintIndex 0 1 (intBinaryRelationIndexFromList (Set.toAscList (tgwRel01 problem))),
      intBinaryConstraintIndex 0 2 (intBinaryRelationIndexFromList (Set.toAscList (tgwRel02 problem))),
      intBinaryConstraintIndex 1 2 (intBinaryRelationIndexFromList (Set.toAscList (tgwRel12 problem)))
    ]

constraintAllows :: Env Int -> Slot -> Int -> (Slot, Slot, Set (Int, Int)) -> Bool
constraintAllows env slot candidate (leftSlot, rightSlot, relation)
  | slot == leftSlot =
      maybe True (\rightValue -> Set.member (candidate, rightValue) relation) (IntMap.lookup rightSlot env)
  | slot == rightSlot =
      maybe True (\leftValue -> Set.member (leftValue, candidate) relation) (IntMap.lookup leftSlot env)
  | otherwise =
      True

bruteForceTriangleCount :: [(Int, Int)] -> Int
bruteForceTriangleCount rawEdges =
  length
    ( filter
        (trianglePresent edges)
        (vertexTriples (Set.toAscList vertices))
    )
  where
    edges =
      normalizedEdgeSet rawEdges

    vertices =
      foldMap (\(leftVertex, rightVertex) -> Set.fromList [leftVertex, rightVertex]) edges

trianglePresent :: Set (Int, Int) -> (Int, Int, Int) -> Bool
trianglePresent edges (leftVertex, middleVertex, rightVertex) =
  Set.member (leftVertex, middleVertex) edges
    && Set.member (leftVertex, rightVertex) edges
    && Set.member (middleVertex, rightVertex) edges

vertexTriples :: [Int] -> [(Int, Int, Int)]
vertexTriples vertices =
  [ (leftVertex, middleVertex, rightVertex)
  | leftVertex : leftRest <- tails vertices,
    middleVertex : middleRest <- tails leftRest,
    rightVertex <- middleRest
  ]

normalizedEdgeSet :: [(Int, Int)] -> Set (Int, Int)
normalizedEdgeSet =
  Set.fromList . mapMaybe normalizeUndirectedEdge
