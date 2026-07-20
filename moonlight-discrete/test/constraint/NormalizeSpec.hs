module NormalizeSpec
  ( tests,
  )
where

import Moonlight.Constraint
  ( ConstraintExpr (..),
    isLocallyIrreducible,
    normalize,
  )
import ConstraintArbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import qualified Test.Tasty.QuickCheck as QC

propNormalizeLocallyIrreducible :: ConstraintExpr Int -> Bool
propNormalizeLocallyIrreducible =
  isLocallyIrreducible . normalize

tests :: TestTree
tests =
  testGroup
    "normalize"
    [ QC.testProperty "normalizes every depth to a locally irreducible form" propNormalizeLocallyIrreducible,
      testCase "eliminates_double_negation" $ do
        normalize (Not (Not (Atom (1 :: Int)))) @?= Atom 1,
      testCase "absorbs_join_meet" $ do
        normalize (And [Atom (1 :: Int), Or [Atom 1, Atom 2]]) @?= Atom 1,
      testCase "absorbs_meet_join" $ do
        normalize (Or [Atom (1 :: Int), And [Atom 1, Atom 2]]) @?= Atom 1,
      testCase "normalizes nested absorption bottom-up" $ do
        normalize
          ( And
              [ Atom (0 :: Int),
                Or [Atom 1, And [Atom 1, Atom 2]]
              ]
          )
          @?= And [Atom 0, Atom 1],
      testCase "propagates a nested complement through its parent" $ do
        normalize
          ( And
              [ Atom (0 :: Int),
                Or [Atom 1, Not (Not (Not (Atom 1)))]
              ]
          )
          @?= Atom 0,
      testCase "recognizes normalized compound complements" $ do
        normalize
          ( Or
              [ And [Atom (1 :: Int), Atom 2],
                Not (And [Atom 1, Atom 2])
              ]
          )
          @?= And []
    ]
