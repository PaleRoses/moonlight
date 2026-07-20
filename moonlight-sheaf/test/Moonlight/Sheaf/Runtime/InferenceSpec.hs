module Moonlight.Sheaf.Runtime.InferenceSpec
  ( tests,
  )
where

import Data.Map.Strict qualified as Map
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Foldable (traverse_)
import Moonlight.Sheaf.Inference
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

newtype Pid = Pid Int
  deriving stock (Eq, Ord, Show)

newtype Obj = Obj Int
  deriving stock (Eq, Ord, Show)

tests :: TestTree
tests =
  testGroup
    "inference"
    [ testCase "blueprint compilation rejects unknown factor variables" testRejectsUnknownFactorVariables,
      testCase "blueprint compilation rejects tuple missing scoped variable" testRejectsMissingScopedVariable,
      testCase "blueprint compilation rejects out-of-domain tuple object" testRejectsOutOfDomainObject,
      testCase "buildWeightedBlueprint rejects empty domains" testRejectsEmptyDomains,
      testCase "inferPosteriorExact returns Right for a valid blueprint" testValidPosteriorIsTotal,
      testCase "free variables contribute multiplicity, MAP values, and uniform marginals" testFreeVariableContract,
      testCase "impossible factors return typed posterior and MAP obstructions" testImpossibleModelIsTyped,
      testCase "NaN weights are rejected at compilation" testNaNWeightRejected,
      testCase "positive-infinite alternatives normalize without NaN" testPositiveInfinityContract,
      testCase "global mixed-radix overflow is rejected before key encoding" testAssignmentSpaceOverflow,
      testCase "zero top-k is empty and negative top-k is rejected" testTopKContract,
      testCase "elimination heuristics exercise distinct lawful orders" testHeuristicsChooseDistinctOrders,
      testCase "exact posterior agrees with a brute-force finite oracle" testPosteriorMatchesBruteForce
    ]

testRejectsUnknownFactorVariables :: Assertion
testRejectsUnknownFactorVariables = do
  let domains = Map.fromList [(Pid 0, Set.fromList [Obj 0])]
      factorSpec =
        FactorSpec
          { fsScope = Set.fromList [Pid 1],
            fsTuples = [] :: [(Map.Map Pid Obj, Double)]
          }
  assertBlueprintError
    (BlueprintFactorCompileError (FactorScopeUnknownVariable (Pid 1)))
    (buildWeightedBlueprint domains (\_ _ -> 0.0) [factorSpec])

testRejectsMissingScopedVariable :: Assertion
testRejectsMissingScopedVariable = do
  let domains = Map.fromList [(Pid 0, Set.fromList [Obj 0])]
      factorSpec =
        FactorSpec
          { fsScope = Set.fromList [Pid 0],
            fsTuples = [(Map.empty :: Map.Map Pid Obj, 0.0)]
          }
  assertBlueprintError
    (BlueprintFactorCompileError (FactorTupleMissingVariable (Pid 0)))
    (buildWeightedBlueprint domains (\_ _ -> 0.0) [factorSpec])

testRejectsOutOfDomainObject :: Assertion
testRejectsOutOfDomainObject = do
  let domains = Map.fromList [(Pid 0, Set.fromList [Obj 0])]
      factorSpec =
        FactorSpec
          { fsScope = Set.fromList [Pid 0],
            fsTuples = [(Map.singleton (Pid 0) (Obj 1), 0.0)]
          }
  assertBlueprintError
    (BlueprintFactorCompileError (FactorTupleObjectOutOfDomain (Pid 0) (Obj 1)))
    (buildWeightedBlueprint domains (\_ _ -> 0.0) [factorSpec])

testRejectsEmptyDomains :: Assertion
testRejectsEmptyDomains =
  assertBlueprintError
    (BlueprintEmptyDomain (Pid 0))
    ( buildWeightedBlueprint
        (Map.fromList [(Pid 0, Set.empty :: Set.Set Obj)])
        (\_ _ -> 0.0)
        []
    )

testValidPosteriorIsTotal :: Assertion
testValidPosteriorIsTotal = do
  let domains =
        Map.fromList
          [ (Pid 0, Set.fromList [Obj 0, Obj 1]),
            (Pid 1, Set.fromList [Obj 0, Obj 1])
          ]
      factorSpec =
        FactorSpec
          { fsScope = Set.fromList [Pid 0, Pid 1],
            fsTuples =
              [ (Map.fromList [(Pid 0, Obj 0), (Pid 1, Obj 0)], 0.0),
                (Map.fromList [(Pid 0, Obj 1), (Pid 1, Obj 1)], 0.0)
              ]
          }
  case buildWeightedBlueprint domains (\_ _ -> 0.0) [factorSpec] of
    Left blueprintError ->
      assertFailure ("expected blueprint construction to succeed: " <> show blueprintError)
    Right blueprint ->
      case inferPosteriorExact defaultInferenceConfig blueprint of
        Left executionError ->
          assertFailure ("expected posterior inference to succeed: " <> show executionError)
        Right _ ->
          assertBool "valid posterior inference succeeded" True

testFreeVariableContract :: Assertion
testFreeVariableContract = do
  let domains = Map.singleton (Pid 0) (Set.fromList [Obj 0, Obj 1])
  case buildWeightedBlueprint domains (\_ _ -> 0.0) [] of
    Left blueprintError ->
      assertFailure ("free-variable blueprint refused: " <> show blueprintError)
    Right blueprint -> do
      assertApprox "free log partition" (log 2.0) =<< requireRight (inferLogZExact defaultInferenceConfig blueprint)
      mapSolution <- requireRight (inferMapExact defaultInferenceConfig blueprint)
      msAssignment mapSolution @?= Map.singleton (Pid 0) (Obj 0)
      marginals <- requireRight (inferMarginalsExact defaultInferenceConfig blueprint)
      Map.lookup (Pid 0) marginals
        @?= Just (Map.fromList [(Obj 0, 0.5), (Obj 1, 0.5)])

testImpossibleModelIsTyped :: Assertion
testImpossibleModelIsTyped = do
  let domains = Map.singleton (Pid 0) (Set.singleton (Obj 0))
      impossibleFactor :: FactorSpec Pid Obj
      impossibleFactor =
        FactorSpec
          { fsScope = Set.singleton (Pid 0),
            fsTuples = []
          }
  blueprint <-
    requireRight
      (buildWeightedBlueprint domains (\_ _ -> 0.0) [impossibleFactor])
  assertInferenceError
    InferenceImpossiblePosterior
    (inferPosteriorExact defaultInferenceConfig blueprint)
  assertInferenceError
    InferenceNoMapAssignment
    (inferMapExact defaultInferenceConfig blueprint)

testNaNWeightRejected :: Assertion
testNaNWeightRejected = do
  let domains = Map.singleton (Pid 0) (Set.singleton (Obj 0))
  case buildWeightedBlueprint domains (\_ _ -> 0.0 / 0.0) [] of
    Left (BlueprintFactorCompileError (FactorLocalInvalidLogWeight (Pid 0) (Obj 0) LogWeightNaN)) ->
      pure ()
    other ->
      assertFailure ("expected typed NaN refusal, received " <> showBlueprintResult other)

testPositiveInfinityContract :: Assertion
testPositiveInfinityContract = do
  let domains = Map.singleton (Pid 0) (Set.fromList [Obj 0, Obj 1])
  blueprint <-
    requireRight
      (buildWeightedBlueprint domains (\_ _ -> 1.0 / 0.0) [])
  posterior <- requireRight (inferPosteriorExact defaultInferenceConfig blueprint)
  assertBool "positive-infinite partition is retained" (isInfinite (spLogPartition posterior) && spLogPartition posterior > 0.0)
  Map.lookup (Pid 0) (spMarginals posterior)
    @?= Just (Map.fromList [(Obj 0, 0.5), (Obj 1, 0.5)])
  msAssignment (spMap posterior)
    @?= Map.singleton (Pid 0) (Obj 0)

testAssignmentSpaceOverflow :: Assertion
testAssignmentSpaceOverflow = do
  let domains =
        Map.fromList
          [ (Pid variable, Set.fromList [Obj 0, Obj 1])
          | variable <- [0 .. 64]
          ]
      expectedCardinality = 2 ^ (65 :: Int)
  assertBlueprintError
    (BlueprintAssignmentSpaceOverflow expectedCardinality)
    (buildWeightedBlueprint domains (\_ _ -> 0.0) [])

testTopKContract :: Assertion
testTopKContract = do
  zeroCount <- requireRight (mkTopKCount 0)
  topKDomains zeroCount (Map.singleton (Pid 0) (Map.fromList [(Obj 0, 0.75), (Obj 1, 0.25)]))
    @?= Map.singleton (Pid 0) Set.empty
  mkTopKCount (-1) @?= Left (TopKCountNegative (-1))

testHeuristicsChooseDistinctOrders :: Assertion
testHeuristicsChooseDistinctOrders = do
  let domains =
        Map.fromList
          [ (Pid variable, Set.singleton (Obj 0))
          | variable <- [0 .. 7]
          ]
      factors :: [FactorSpec Pid Obj]
      factors =
        fmap
          ( \(leftVariable, rightVariable) ->
              FactorSpec
                { fsScope = Set.fromList [Pid leftVariable, Pid rightVariable],
                  fsTuples = []
                }
          )
          [ (0, 1),
            (1, 2),
            (2, 3),
            (3, 0),
            (4, 5),
            (4, 6),
            (4, 7),
            (5, 6),
            (5, 7),
            (6, 7)
          ]
  blueprint <- requireRight (buildWeightedBlueprint domains (\_ _ -> 0.0) factors)
  let minFillOrder = selectEliminationOrder (InferenceConfig MinFill) blueprint
      minDegreeOrder = selectEliminationOrder (InferenceConfig MinDegree) blueprint
  assertBool
    ("expected distinct orders, received " <> show minFillOrder)
    (minFillOrder /= minDegreeOrder)

testPosteriorMatchesBruteForce :: Assertion
testPosteriorMatchesBruteForce = do
  let domains =
        Map.fromList
          [ (Pid 0, Set.fromList [Obj 0, Obj 1]),
            (Pid 1, Set.fromList [Obj 0, Obj 1]),
            (Pid 2, Set.fromList [Obj 0, Obj 1]),
            (Pid 3, Set.fromList [Obj 0, Obj 1])
          ]
      localWeight :: Pid -> Obj -> Double
      localWeight (Pid variable) (Obj value) =
        fromIntegral (variable - value) / 7.0
      factors :: [FactorSpec Pid Obj]
      factors =
        [ FactorSpec
            { fsScope = Set.fromList [Pid 0, Pid 1],
              fsTuples =
                [ (Map.fromList [(Pid 0, Obj 0), (Pid 1, Obj 0)], 0.2),
                  (Map.fromList [(Pid 0, Obj 0), (Pid 1, Obj 0)], -0.3),
                  (Map.fromList [(Pid 0, Obj 1), (Pid 1, Obj 1)], 0.1)
                ]
            },
          FactorSpec
            { fsScope = Set.singleton (Pid 2),
              fsTuples =
                [ (Map.singleton (Pid 2) (Obj 0), -0.4),
                  (Map.singleton (Pid 2) (Obj 1), 0.35)
                ]
            },
          FactorSpec
            { fsScope = Set.empty,
              fsTuples = [(Map.empty, 0.15)]
            }
        ]
  blueprint <- requireRight (buildWeightedBlueprint domains localWeight factors)
  let oracle = bruteForcePosterior domains localWeight factors
  traverse_
    (assertPosteriorMatchesOracle blueprint oracle)
    [InferenceConfig MinFill, InferenceConfig MinDegree]

assertPosteriorMatchesOracle ::
  WeightedBlueprint Pid Obj ->
  OraclePosterior Pid Obj ->
  InferenceConfig ->
  Assertion
assertPosteriorMatchesOracle blueprint oracle config = do
  posterior <- requireRight (inferPosteriorExact config blueprint)
  let label = show (icEliminationHeuristic config)
  assertApprox (label <> " oracle log partition") (oracleLogPartition oracle) (spLogPartition posterior)
  msAssignment (spMap posterior) @?= oracleMapAssignment oracle
  assertApprox (label <> " oracle MAP score") (oracleMapScore oracle) (msLogScore (spMap posterior))
  assertMarginalsApprox (oracleMarginals oracle) (spMarginals posterior)

data OraclePosterior pid obj = OraclePosterior
  { oracleLogPartition :: !Double,
    oracleMarginals :: !(Map.Map pid (Map.Map obj Double)),
    oracleMapAssignment :: !(Map.Map pid obj),
    oracleMapScore :: !Double
  }

bruteForcePosterior ::
  (Ord pid, Ord obj) =>
  Map.Map pid (Set.Set obj) ->
  (pid -> obj -> Double) ->
  [FactorSpec pid obj] ->
  OraclePosterior pid obj
bruteForcePosterior domains localWeight factors =
  OraclePosterior
    { oracleLogPartition = log totalMass + maximumScore,
      oracleMarginals =
        Map.mapWithKey
          (\variable values ->
            Map.fromSet
              (\value -> marginalMass variable value / totalMass)
              values
          )
          domains,
      oracleMapAssignment = fst bestAssignment,
      oracleMapScore = snd bestAssignment
    }
  where
    assignments =
      fmap Map.fromList
        (traverse (\(variable, values) -> fmap (variable,) (Set.toAscList values)) (Map.toAscList domains))
    scoredAssignments = fmap (\assignment -> (assignment, assignmentScore assignment)) assignments
    maximumScore = maximum (fmap snd scoredAssignments)
    weightedAssignments =
      fmap
        (\(assignment, score) -> (assignment, exp (score - maximumScore)))
        scoredAssignments
    totalMass = sum (fmap snd weightedAssignments)
    bestAssignment =
      maximumBy
        (comparing snd <> flip (comparing fst))
        scoredAssignments
    marginalMass variable value =
      sum
        [ mass
        | (assignment, mass) <- weightedAssignments,
          Map.lookup variable assignment == Just value
        ]
    assignmentScore assignment =
      sum
        [ localWeight variable value
        | (variable, value) <- Map.toAscList assignment
        ]
        + sum (fmap (factorScore assignment) factors)

factorScore :: (Ord pid, Ord obj) => Map.Map pid obj -> FactorSpec pid obj -> Double
factorScore assignment factor =
  logSumExp
    [ tupleWeight
    | (tuple, tupleWeight) <- fsTuples factor,
      all (\variable -> Map.lookup variable tuple == Map.lookup variable assignment) (Set.toAscList (fsScope factor))
    ]

logSumExp :: [Double] -> Double
logSumExp [] = -(1.0 / 0.0)
logSumExp weights =
  maximumWeight + log (sum (fmap (exp . subtract maximumWeight) weights))
  where
    maximumWeight = maximum weights

assertMarginalsApprox ::
  (Ord pid, Ord obj, Show pid, Show obj) =>
  Map.Map pid (Map.Map obj Double) ->
  Map.Map pid (Map.Map obj Double) ->
  Assertion
assertMarginalsApprox expected actual =
  traverse_
    (\(variable, expectedValues) ->
      case Map.lookup variable actual of
        Nothing -> assertFailure ("missing marginal for " <> show variable)
        Just actualValues ->
          traverse_
            (\(value, expectedProbability) ->
              case Map.lookup value actualValues of
                Nothing -> assertFailure ("missing marginal value " <> show (variable, value))
                Just actualProbability ->
                  assertApprox ("marginal " <> show (variable, value)) expectedProbability actualProbability
            )
            (Map.toAscList expectedValues)
    )
    (Map.toAscList expected)

assertApprox :: String -> Double -> Double -> Assertion
assertApprox label expected actual =
  assertBool
    (label <> ": expected " <> show expected <> ", received " <> show actual)
    (abs (expected - actual) <= 1.0e-10)

requireRight :: Show error => Either error value -> IO value
requireRight result =
  case result of
    Left failure -> assertFailure ("expected Right, received " <> show failure) >> fail "unreachable"
    Right value -> pure value

showBlueprintResult :: (Show pid, Show obj) => Either (BlueprintError pid obj) (WeightedBlueprint pid obj) -> String
showBlueprintResult result =
  case result of
    Left blueprintError -> show blueprintError
    Right _ -> "Right <weighted blueprint>"

assertInferenceError ::
  InferenceExecutionError ->
  Either InferenceExecutionError value ->
  Assertion
assertInferenceError expected result =
  case result of
    Left actual -> actual @?= expected
    Right _ -> assertFailure ("expected inference error " <> show expected)

assertBlueprintError ::
  (Eq pid, Eq obj, Show pid, Show obj) =>
  BlueprintError pid obj ->
  Either (BlueprintError pid obj) (WeightedBlueprint pid obj) ->
  Assertion
assertBlueprintError expected result =
  case result of
    Left actual ->
      actual @?= expected
    Right _ ->
      assertFailure "expected blueprint construction to fail"
