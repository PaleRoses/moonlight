module DPLLSpec
  ( tests,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Moonlight.Constraint
  ( dpll,
    literalPolarity,
    literalVariable,
    Literal (..),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, (@?=), testCase)

satisfiesCNF :: Ord a => Map.Map a Bool -> [Set.Set (Literal a)] -> Bool
satisfiesCNF assignment clauses =
  let evalLiteral literal =
        Map.findWithDefault False (literalVariable literal) assignment
          == literalPolarity literal
      evalClause clause =
        any evalLiteral (Set.toList clause)
   in all evalClause clauses

returnsSatisfyingAssignmentClauses :: [Set.Set (Literal Int)]
returnsSatisfyingAssignmentClauses =
  [ Set.fromList [Pos 1, Pos 2],
    Set.fromList [Neg 1, Pos 2]
  ]

tests :: TestTree
tests =
  testGroup
    "dpll"
    [ testCase "empty_clause_list_is_satisfiable" $ do
        dpll @Int [] @?= Just Map.empty,
      testCase "single_empty_clause_is_unsatisfiable" $ do
        dpll @Int [Set.empty] @?= Nothing,
      testCase "contradictory_units_unsat" $ do
        dpll @Int [Set.singleton (Pos 1), Set.singleton (Neg 1)] @?= Nothing,
      testCase "returns_satisfying_assignment" $ do
        case dpll @Int returnsSatisfyingAssignmentClauses of
          Nothing -> assertBool "expected satisfiable CNF" False
          Just assignment ->
            assertBool
              "assignment should satisfy CNF"
              (satisfiesCNF assignment returnsSatisfyingAssignmentClauses)
    ]
