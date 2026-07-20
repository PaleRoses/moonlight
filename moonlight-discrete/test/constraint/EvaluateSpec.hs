module EvaluateSpec
  ( tests,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Moonlight.Constraint
  ( ConstraintExpr (..),
    atoms,
    equivalent,
    evaluate,
    implies,
    satisfiable,
    tautology,
    unsatisfiable,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

resolverFromMap :: Ord a => Map.Map a Bool -> a -> Bool
resolverFromMap assignments atom =
  Map.findWithDefault False atom assignments

evaluateAndOrNotResolver :: Int -> Bool
evaluateAndOrNotResolver = resolverFromMap (Map.fromList [(1, True), (2, False)])

evaluateAndOrNotExpr :: ConstraintExpr Int
evaluateAndOrNotExpr = And [Atom 1, Not (Atom 2)]

satisfiableExpr :: ConstraintExpr Int
satisfiableExpr = Or [Atom 1, Not (Atom 1)]

unsatisfiableExpr :: ConstraintExpr Int
unsatisfiableExpr = And [Atom 1, Not (Atom 1)]

implicationLeftExpr :: ConstraintExpr Int
implicationLeftExpr = And [Atom 1, Atom 2]

implicationRightExpr :: ConstraintExpr Int
implicationRightExpr = Atom 1

equivalentLeftExpr :: ConstraintExpr Int
equivalentLeftExpr = Atom 1

equivalentRightExpr :: ConstraintExpr Int
equivalentRightExpr = Not (Not (Atom 1))

atomsCollectionExpr :: ConstraintExpr Int
atomsCollectionExpr = And [Atom 1, Not (Or [Atom 2, Atom 1])]

atomsCollectionExpected :: Set.Set Int
atomsCollectionExpected = Set.fromList [1, 2]

tests :: TestTree
tests =
  testGroup
    "evaluate"
    [ testCase "evaluate_and_or_not" $ do
        evaluate evaluateAndOrNotResolver evaluateAndOrNotExpr @?= True,
      testCase "satisfiable_and_unsatisfiable" $ do
        satisfiable satisfiableExpr @?= True
        unsatisfiable unsatisfiableExpr @?= True,
      testCase "tautology_detection" $ do
        tautology satisfiableExpr @?= True,
      testCase "implication_via_unsat" $ do
        implies implicationLeftExpr implicationRightExpr @?= True,
      testCase "equivalence_is_mutual_implication" $ do
        equivalent equivalentLeftExpr equivalentRightExpr @?= True,
      testCase "atoms_collection" $ do
        atoms atomsCollectionExpr @?= atomsCollectionExpected
    ]
