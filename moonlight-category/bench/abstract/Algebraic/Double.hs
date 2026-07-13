{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Algebraic.Double
  ( doubleCategoryBenchmarks,
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Moonlight.Category.Pure.DoubleCategory (DoubleCategory (..), interchangeLaw)
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

doubleCategoryBenchmarks :: Benchmark
doubleCategoryBenchmarks =
  bgroup
    "DoubleCategory"
    [ bench "interchangeLaw symbolic 2x2 grid batch x512" (nf (batchWeight doubleInterchangeWeight) sampleBatch),
      bench "typed horizontal identity compose batch x512" (nf (batchWeight doubleHorizontalIdentityWeight) sampleBatch),
      bench "typed vertical identity compose batch x512" (nf (batchWeight doubleVerticalIdentityWeight) sampleBatch)
    ]
type SymbolicObject :: Type
data SymbolicObject
  = SObjectA
  | SObjectB
  | SObjectC
  | SObjectD
  | SObjectE
  | SObjectF
  | SObjectG
  | SObjectH
  | SObjectI

type SymbolicHorizontal :: SymbolicObject -> SymbolicObject -> Type
newtype SymbolicHorizontal (source :: SymbolicObject) (target :: SymbolicObject) = SymbolicHorizontal
  { symbolicHorizontalTrace :: [Int]
  }
  deriving stock (Eq, Show)

type SymbolicVertical :: SymbolicObject -> SymbolicObject -> Type
newtype SymbolicVertical (source :: SymbolicObject) (target :: SymbolicObject) = SymbolicVertical
  { symbolicVerticalTrace :: [Int]
  }
  deriving stock (Eq, Show)

type SymbolicSquare ::
  SymbolicObject ->
  SymbolicObject ->
  SymbolicObject ->
  SymbolicObject ->
  Type
data SymbolicSquare northWest northEast southWest southEast = SymbolicSquare
  { symbolicSquareTop :: SymbolicHorizontal northWest northEast,
    symbolicSquareBottom :: SymbolicHorizontal southWest southEast,
    symbolicSquareLeft :: SymbolicVertical northWest southWest,
    symbolicSquareRight :: SymbolicVertical northEast southEast
  }
  deriving stock (Eq, Show)

type SymbolicDouble :: Type
data SymbolicDouble

instance DoubleCategory SymbolicObject SymbolicDouble where
  type ObjectWitness SymbolicObject SymbolicDouble = Proxy
  type HorizontalMor SymbolicObject SymbolicDouble = SymbolicHorizontal
  type VerticalMor SymbolicObject SymbolicDouble = SymbolicVertical
  type Square SymbolicObject SymbolicDouble = SymbolicSquare

  horizontalIdentity _ = SymbolicHorizontal []
  verticalIdentity _ = SymbolicVertical []
  composeHorizontal leftHorizontal rightHorizontal =
    Just
      SymbolicHorizontal
        { symbolicHorizontalTrace = symbolicHorizontalTrace rightHorizontal <> symbolicHorizontalTrace leftHorizontal
        }
  composeVertical lowerVertical upperVertical =
    Just
      SymbolicVertical
        { symbolicVerticalTrace = symbolicVerticalTrace upperVertical <> symbolicVerticalTrace lowerVertical
        }
  squareTop = symbolicSquareTop
  squareBottom = symbolicSquareBottom
  squareLeft = symbolicSquareLeft
  squareRight = symbolicSquareRight
  composeSquaresHorizontal eastSquare westSquare =
    Just
      SymbolicSquare
        { symbolicSquareTop =
            SymbolicHorizontal
              { symbolicHorizontalTrace =
                  symbolicHorizontalTrace (symbolicSquareTop westSquare)
                    <> symbolicHorizontalTrace (symbolicSquareTop eastSquare)
              },
          symbolicSquareBottom =
            SymbolicHorizontal
              { symbolicHorizontalTrace =
                  symbolicHorizontalTrace (symbolicSquareBottom westSquare)
                    <> symbolicHorizontalTrace (symbolicSquareBottom eastSquare)
              },
          symbolicSquareLeft = symbolicSquareLeft westSquare,
          symbolicSquareRight = symbolicSquareRight eastSquare
        }
  composeSquaresVertical southSquare northSquare =
    Just
      SymbolicSquare
        { symbolicSquareTop = symbolicSquareTop northSquare,
          symbolicSquareBottom = symbolicSquareBottom southSquare,
          symbolicSquareLeft =
            SymbolicVertical
              { symbolicVerticalTrace =
                  symbolicVerticalTrace (symbolicSquareLeft northSquare)
                    <> symbolicVerticalTrace (symbolicSquareLeft southSquare)
              },
          symbolicSquareRight =
            SymbolicVertical
              { symbolicVerticalTrace =
                  symbolicVerticalTrace (symbolicSquareRight northSquare)
                    <> symbolicVerticalTrace (symbolicSquareRight southSquare)
              }
        }

doubleInterchangeWeight :: Int -> Int
doubleInterchangeWeight seed =
  seed + maybe 0 boolWeight (interchangeLaw @SymbolicObject @SymbolicDouble (northWestSquare seed) (northEastSquare seed) (southWestSquare seed) (southEastSquare seed))

doubleHorizontalIdentityWeight :: Int -> Int
doubleHorizontalIdentityWeight seed =
  seed + maybe 0 horizontalWeight (composeHorizontal @SymbolicObject @SymbolicDouble (horizontalIdentity @SymbolicObject @SymbolicDouble (Proxy @'SObjectB)) (horizontalArrow seed :: SymbolicHorizontal 'SObjectA 'SObjectB))

doubleVerticalIdentityWeight :: Int -> Int
doubleVerticalIdentityWeight seed =
  seed + maybe 0 verticalWeight (composeVertical @SymbolicObject @SymbolicDouble (verticalIdentity @SymbolicObject @SymbolicDouble (Proxy @'SObjectB)) (verticalArrow seed :: SymbolicVertical 'SObjectA 'SObjectB))

horizontalArrow :: Int -> SymbolicHorizontal source target
horizontalArrow labelValue = SymbolicHorizontal [labelValue]

verticalArrow :: Int -> SymbolicVertical source target
verticalArrow labelValue = SymbolicVertical [labelValue]

northWestSquare :: Int -> SymbolicSquare 'SObjectA 'SObjectB 'SObjectD 'SObjectE
northWestSquare seed =
  SymbolicSquare
    { symbolicSquareTop = horizontalArrow (seed + 1),
      symbolicSquareBottom = horizontalArrow (seed + 4),
      symbolicSquareLeft = verticalArrow (seed + 7),
      symbolicSquareRight = verticalArrow (seed + 8)
    }

northEastSquare :: Int -> SymbolicSquare 'SObjectB 'SObjectC 'SObjectE 'SObjectF
northEastSquare seed =
  SymbolicSquare
    { symbolicSquareTop = horizontalArrow (seed + 2),
      symbolicSquareBottom = horizontalArrow (seed + 5),
      symbolicSquareLeft = verticalArrow (seed + 8),
      symbolicSquareRight = verticalArrow (seed + 9)
    }

southWestSquare :: Int -> SymbolicSquare 'SObjectD 'SObjectE 'SObjectG 'SObjectH
southWestSquare seed =
  SymbolicSquare
    { symbolicSquareTop = horizontalArrow (seed + 4),
      symbolicSquareBottom = horizontalArrow (seed + 6),
      symbolicSquareLeft = verticalArrow (seed + 10),
      symbolicSquareRight = verticalArrow (seed + 11)
    }

southEastSquare :: Int -> SymbolicSquare 'SObjectE 'SObjectF 'SObjectH 'SObjectI
southEastSquare seed =
  SymbolicSquare
    { symbolicSquareTop = horizontalArrow (seed + 5),
      symbolicSquareBottom = horizontalArrow (seed + 7),
      symbolicSquareLeft = verticalArrow (seed + 11),
      symbolicSquareRight = verticalArrow (seed + 12)
    }

horizontalWeight :: SymbolicHorizontal source target -> Int
horizontalWeight (SymbolicHorizontal traceValue) = sum traceValue

verticalWeight :: SymbolicVertical source target -> Int
verticalWeight (SymbolicVertical traceValue) = sum traceValue

boolWeight :: Bool -> Int
boolWeight value =
  if value then 1 else 0

sampleBatch :: [Int]
sampleBatch = [0 .. 511]

batchWeight :: (Int -> Int) -> [Int] -> Int
batchWeight weight =
  sum . fmap weight
