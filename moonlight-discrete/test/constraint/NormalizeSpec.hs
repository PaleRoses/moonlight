module NormalizeSpec
  ( tests,
  )
where

import Moonlight.Constraint
  ( ConstraintExpr (..),
    equivalent,
    normalize,
  )
import ConstraintArbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import qualified Test.Tasty.QuickCheck as QC

propNormalizeIdempotent :: ConstraintExpr Int -> Bool
propNormalizeIdempotent expression =
  normalize (normalize expression) == normalize expression

propNormalizeSemanticPreservation :: ConstraintExpr Int -> Bool
propNormalizeSemanticPreservation expression =
  equivalent (normalize expression) expression

tests :: TestTree
tests =
  testGroup
    "normalize"
    [ QC.testProperty "normalize_idempotent" propNormalizeIdempotent,
      QC.testProperty "normalize_semantic_preservation" propNormalizeSemanticPreservation,
      testCase "eliminates_double_negation" $ do
        normalize (Not (Not (Atom (1 :: Int)))) @?= Atom 1,
      testCase "absorbs_join_meet" $ do
        normalize (And [Atom (1 :: Int), Or [Atom 1, Atom 2]]) @?= Atom 1,
      testCase "absorbs_meet_join" $ do
        normalize (Or [Atom (1 :: Int), And [Atom 1, Atom 2]]) @?= Atom 1
    ]
