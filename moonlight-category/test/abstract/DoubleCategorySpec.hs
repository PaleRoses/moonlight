{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module DoubleCategorySpec
  ( tests,
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Moonlight.Category (DoubleCategory (..), interchangeLaw)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

type SymbolicObject :: Type
data SymbolicObject
  = ObjectA
  | ObjectB
  | ObjectC
  | ObjectD
  | ObjectE
  | ObjectF
  | ObjectG
  | ObjectH
  | ObjectI

type SymbolicHorizontal :: SymbolicObject -> SymbolicObject -> Type
newtype SymbolicHorizontal (source :: SymbolicObject) (target :: SymbolicObject) = SymbolicHorizontal
  { symbolicHorizontalTrace :: [String]
  }
  deriving stock (Eq, Show)

type SymbolicVertical :: SymbolicObject -> SymbolicObject -> Type
newtype SymbolicVertical (source :: SymbolicObject) (target :: SymbolicObject) = SymbolicVertical
  { symbolicVerticalTrace :: [String]
  }
  deriving stock (Eq, Show)

type SymbolicSquare ::
  SymbolicObject -> SymbolicObject -> SymbolicObject -> SymbolicObject -> Type
data SymbolicSquare (northWest :: SymbolicObject) (northEast :: SymbolicObject) (southWest :: SymbolicObject) (southEast :: SymbolicObject) = SymbolicSquare
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

tests :: TestTree
tests =
  testGroup
    "DoubleCategory"
    [ testCase "interchange law holds for a symbolic 2x2 grid" testInterchangeLaw,
      testCase "typed identities are neutral on symbolic morphisms" testIdentity
    ]

horizontalArrow :: String -> SymbolicHorizontal source target
horizontalArrow labelValue = SymbolicHorizontal [labelValue]

verticalArrow :: String -> SymbolicVertical source target
verticalArrow labelValue = SymbolicVertical [labelValue]

northWestSquare :: SymbolicSquare 'ObjectA 'ObjectB 'ObjectD 'ObjectE
northWestSquare =
  SymbolicSquare
    { symbolicSquareTop = horizontalArrow "top-west",
      symbolicSquareBottom = horizontalArrow "middle-west",
      symbolicSquareLeft = verticalArrow "left-north",
      symbolicSquareRight = verticalArrow "middle-north"
    }

northEastSquare :: SymbolicSquare 'ObjectB 'ObjectC 'ObjectE 'ObjectF
northEastSquare =
  SymbolicSquare
    { symbolicSquareTop = horizontalArrow "top-east",
      symbolicSquareBottom = horizontalArrow "middle-east",
      symbolicSquareLeft = verticalArrow "middle-north",
      symbolicSquareRight = verticalArrow "right-north"
    }

southWestSquare :: SymbolicSquare 'ObjectD 'ObjectE 'ObjectG 'ObjectH
southWestSquare =
  SymbolicSquare
    { symbolicSquareTop = horizontalArrow "middle-west",
      symbolicSquareBottom = horizontalArrow "bottom-west",
      symbolicSquareLeft = verticalArrow "left-south",
      symbolicSquareRight = verticalArrow "middle-south"
    }

southEastSquare :: SymbolicSquare 'ObjectE 'ObjectF 'ObjectH 'ObjectI
southEastSquare =
  SymbolicSquare
    { symbolicSquareTop = horizontalArrow "middle-east",
      symbolicSquareBottom = horizontalArrow "bottom-east",
      symbolicSquareLeft = verticalArrow "middle-south",
      symbolicSquareRight = verticalArrow "right-south"
    }

testInterchangeLaw :: IO ()
testInterchangeLaw =
  interchangeLaw @SymbolicObject @SymbolicDouble northWestSquare northEastSquare southWestSquare southEastSquare
    @?= Just True

testIdentity :: IO ()
testIdentity = do
  composeHorizontal @SymbolicObject @SymbolicDouble (horizontalIdentity @SymbolicObject @SymbolicDouble (Proxy @'ObjectB)) (horizontalArrow "edge" :: SymbolicHorizontal 'ObjectA 'ObjectB)
    @?= Just (horizontalArrow "edge")
  composeVertical @SymbolicObject @SymbolicDouble (verticalIdentity @SymbolicObject @SymbolicDouble (Proxy @'ObjectB)) (verticalArrow "edge" :: SymbolicVertical 'ObjectA 'ObjectB)
    @?= Just (verticalArrow "edge")
