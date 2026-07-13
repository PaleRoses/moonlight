module CNFSpec
  ( tests,
  )
where

import qualified Data.Set as Set
import Moonlight.Constraint
  ( ConstraintExpr (..),
    Literal (..),
    NNFExpr (..),
    toCNF,
    toNNF,
  )
import ConstraintArbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import qualified Test.Tasty.QuickCheck as QC

nnfShape :: NNFExpr a -> Bool
nnfShape expression =
  case expression of
    NAtom _ -> True
    NNegAtom _ -> True
    NAnd children -> all nnfShape children
    NOr children -> all nnfShape children

propNnfShape :: ConstraintExpr Int -> Bool
propNnfShape expression =
  nnfShape (toNNF expression)

distributesOrOverAndInput :: ConstraintExpr Int
distributesOrOverAndInput = Or [And [Atom 1, Atom 2], Atom 3]

distributesOrOverAndExpected :: [Set.Set (Literal Int)]
distributesOrOverAndExpected =
  [ Set.fromList [Pos 1, Pos 3],
    Set.fromList [Pos 2, Pos 3]
  ]

manualSpotCheckInput :: ConstraintExpr Int
manualSpotCheckInput = And [Or [Atom 1, Atom 2], Or [Atom 3, Atom 4]]

manualSpotCheckExpected :: [Set.Set (Literal Int)]
manualSpotCheckExpected =
  [ Set.fromList [Pos 1, Pos 2],
    Set.fromList [Pos 3, Pos 4]
  ]

tests :: TestTree
tests =
  testGroup
    "cnf"
    [ QC.testProperty "nnf_shape" propNnfShape,
      testCase "distributes_or_over_and" $ do
        toCNF distributesOrOverAndInput @?= distributesOrOverAndExpected,
      testCase "manual_spot_check" $ do
        toCNF manualSpotCheckInput @?= manualSpotCheckExpected
    ]
