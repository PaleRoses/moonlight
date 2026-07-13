module Moonlight.Analysis.SolverInfraSpec
  ( tests,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Moonlight.Analysis
  ( defaultMonotoneSolverConfig,
    defaultSemiringSolverConfig,
    monotoneSolve,
    semiringSolveExact,
    solverResultChanged,
    solverResultState,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, (@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "solver_infra"
    [ testCase "semiringSolveExact reaches a propagation fixpoint" $
        let propagate :: Map.Map Int Integer -> Map.Map Int Integer
            propagate active = Map.map (\value -> if value == (0 :: Integer) then 1 else 0) active
            seed = Map.singleton 0 (0 :: Integer)
            result = semiringSolveExact defaultSemiringSolverConfig propagate seed
         in case result of
              Left err -> assertBool (show err) False
              Right solved -> solverResultState solved @?= Map.singleton 0 (1 :: Integer),
      testCase "monotoneSolve consumes a frontier until empty" $
        let step :: Set.Set Int -> Int -> (Int, Set.Set Int)
            step frontier current =
              let nextFrontier = Set.filter (> (0 :: Int)) (Set.map (subtract 1) frontier)
               in (current + Set.foldr (+) 0 frontier, nextFrontier)
            result = monotoneSolve defaultMonotoneSolverConfig step (Set.fromList [2 :: Int]) (0 :: Int)
         in case result of
              Left err -> assertBool (show err) False
              Right solved -> do
                solverResultState solved @?= 3
                solverResultChanged solved @?= Set.fromList [1 :: Int]
    ]
