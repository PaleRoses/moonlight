{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module PolynomialFunctorWitnessSpec
  ( tests,
  )
where

import Data.Kind (Type)
import Moonlight.Category
  ( Direction,
    Exists (..),
    ParameterizedDirection,
    ParameterizedPolynomialFunctor (..),
    PolynomialFunctor (..),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

type DemoPolynomial :: Type
data DemoPolynomial

type RootPosition :: Type
data RootPosition

type BranchPosition :: Type
data BranchPosition

type DemoParameterizedPolynomial :: Type
data DemoParameterizedPolynomial

type FullSliceRootPosition :: Type
data FullSliceRootPosition

type FullSliceBranchPosition :: Type
data FullSliceBranchPosition

type TrimmedSliceRootPosition :: Type
data TrimmedSliceRootPosition

instance PolynomialFunctor DemoPolynomial where
  data Position DemoPolynomial position where
    RootWitness :: Position DemoPolynomial RootPosition
    BranchWitness :: Position DemoPolynomial BranchPosition
  type Direction DemoPolynomial RootPosition = Bool
  type Direction DemoPolynomial BranchPosition = Maybe Bool
  allPositions = [Exists RootWitness, Exists BranchWitness]

rootDirectionWitness :: Direction DemoPolynomial RootPosition
rootDirectionWitness = True

branchDirectionWitness :: Direction DemoPolynomial BranchPosition
branchDirectionWitness = Just False

instance ParameterizedPolynomialFunctor DemoParameterizedPolynomial where
  type PolynomialParameter DemoParameterizedPolynomial = Bool

  data ParameterizedPosition DemoParameterizedPolynomial position where
    FullSliceRootWitness :: ParameterizedPosition DemoParameterizedPolynomial FullSliceRootPosition
    FullSliceBranchWitness :: ParameterizedPosition DemoParameterizedPolynomial FullSliceBranchPosition
    TrimmedSliceRootWitness :: ParameterizedPosition DemoParameterizedPolynomial TrimmedSliceRootPosition

  type ParameterizedDirection DemoParameterizedPolynomial FullSliceRootPosition = Bool
  type ParameterizedDirection DemoParameterizedPolynomial FullSliceBranchPosition = Maybe Bool
  type ParameterizedDirection DemoParameterizedPolynomial TrimmedSliceRootPosition = ()

  positionsAt includeBranch =
    if includeBranch
      then [Exists FullSliceRootWitness, Exists FullSliceBranchWitness]
      else [Exists TrimmedSliceRootWitness]

fullSliceRootDirectionWitness :: ParameterizedDirection DemoParameterizedPolynomial FullSliceRootPosition
fullSliceRootDirectionWitness = True

fullSliceBranchDirectionWitness :: ParameterizedDirection DemoParameterizedPolynomial FullSliceBranchPosition
fullSliceBranchDirectionWitness = Just True

trimmedSliceRootDirectionWitness :: ParameterizedDirection DemoParameterizedPolynomial TrimmedSliceRootPosition
trimmedSliceRootDirectionWitness = ()

tests :: TestTree
tests =
  testGroup
    "PolynomialFunctor"
    [ testGroup
        "closed witness families"
        [ testCase "position witnesses enumerate the polynomial support" $
            length demoPositions @?= 2,
          testCase "root positions admit the indexed direction carrier" $
            rootDirectionWitness @?= True,
          testCase "branch positions admit a distinct indexed direction carrier" $
            branchDirectionWitness @?= Just False
        ],
      testGroup
        "parameterized witness families"
        [ testCase "positionsAt enumerates the requested slice" $
            ( length (demoParameterizedPositions True),
              length (demoParameterizedPositions False)
            )
              @?= (2, 1),
          testCase "parameterized positions admit slice-specific directions" $
            ( fullSliceRootDirectionWitness,
              fullSliceBranchDirectionWitness,
              trimmedSliceRootDirectionWitness
            )
              @?= (True, Just True, ())
        ]
    ]

demoPositions :: [Exists (Position DemoPolynomial)]
demoPositions = allPositions

demoParameterizedPositions :: Bool -> [Exists (ParameterizedPosition DemoParameterizedPolynomial)]
demoParameterizedPositions = positionsAt
