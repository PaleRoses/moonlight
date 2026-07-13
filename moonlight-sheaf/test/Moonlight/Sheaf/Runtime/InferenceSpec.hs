module Moonlight.Sheaf.Runtime.InferenceSpec
  ( tests,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
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
    [ testCase "compileFactorSpec rejects unknown factor variables" testRejectsUnknownFactorVariables,
      testCase "compileFactorSpec rejects tuple missing scoped variable" testRejectsMissingScopedVariable,
      testCase "compileFactorSpec rejects out-of-domain tuple object" testRejectsOutOfDomainObject,
      testCase "buildWeightedBlueprint rejects empty domains" testRejectsEmptyDomains,
      testCase "inferPosteriorExact returns Right for a valid blueprint" testValidPosteriorIsTotal
    ]

testRejectsUnknownFactorVariables :: Assertion
testRejectsUnknownFactorVariables = do
  let index =
        buildDomainIndex
          (Map.fromList [(Pid 0, Set.fromList [Obj 0])])
      factorSpec =
        FactorSpec
          { fsScope = Set.fromList [Pid 1],
            fsTuples = [] :: [(Map.Map Pid Obj, Double)]
          }
  assertFactorCompileError
    (FactorScopeUnknownVariable (Pid 1))
    (compileFactorSpec index factorSpec)

testRejectsMissingScopedVariable :: Assertion
testRejectsMissingScopedVariable = do
  let index =
        buildDomainIndex
          (Map.fromList [(Pid 0, Set.fromList [Obj 0])])
      factorSpec =
        FactorSpec
          { fsScope = Set.fromList [Pid 0],
            fsTuples = [(Map.empty :: Map.Map Pid Obj, 0.0)]
          }
  assertFactorCompileError
    (FactorTupleMissingVariable (Pid 0))
    (compileFactorSpec index factorSpec)

testRejectsOutOfDomainObject :: Assertion
testRejectsOutOfDomainObject = do
  let index =
        buildDomainIndex
          (Map.fromList [(Pid 0, Set.fromList [Obj 0])])
      factorSpec =
        FactorSpec
          { fsScope = Set.fromList [Pid 0],
            fsTuples = [(Map.singleton (Pid 0) (Obj 1), 0.0)]
          }
  assertFactorCompileError
    (FactorTupleObjectOutOfDomain (Pid 0) (Obj 1))
    (compileFactorSpec index factorSpec)

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

assertFactorCompileError ::
  (Eq pid, Eq obj, Show pid, Show obj) =>
  FactorCompileError pid obj ->
  Either (FactorCompileError pid obj) WeightedFactor ->
  Assertion
assertFactorCompileError expected result =
  case result of
    Left actual ->
      actual @?= expected
    Right _ ->
      assertFailure "expected factor compilation to fail"

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
