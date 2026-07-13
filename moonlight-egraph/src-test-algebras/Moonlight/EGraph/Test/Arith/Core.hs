{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Test.Arith.Core
  ( ArithF (..),
    ArithTag (..),
    ArithView (..),
    NodeCount (..),
    analysisSpec,
    arithTheorySpec,
    arithNodeCount,
    numTerm,
    varTerm,
    addTermNode,
    mulTermNode,
    negTermNode,
    viewArithTerm,
    fromArithView,
  )
where

import Moonlight.Core (ZipMatch (..))
import Data.Functor.Classes (Eq1 (..), Show1 (..), showsBinaryWith, showsUnaryWith)
import Data.Kind (Type)
import Moonlight.Algebra (JoinSemilattice (join))
import Moonlight.Core (ConstructorTag, HasConstructorTag (..), zipSameNodeShape)
import Moonlight.Core (StructuralLaw (..), TheorySpec (..), commutativeBinary)
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec, semilatticeAnalysis)
import Data.Fix (Fix (..))
import Test.Tasty.QuickCheck qualified as QC

type ArithF :: Type -> Type
data ArithF a
  = Num Int
  | Var Int
  | Add a a
  | Mul a a
  | Neg a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Eq1 ArithF where
  liftEq eqChild leftNode rightNode =
    case (leftNode, rightNode) of
      (Num leftNumber, Num rightNumber) ->
        leftNumber == rightNumber
      (Var leftIndex, Var rightIndex) ->
        leftIndex == rightIndex
      (Add leftA leftB, Add rightA rightB) ->
        eqChild leftA rightA && eqChild leftB rightB
      (Mul leftA leftB, Mul rightA rightB) ->
        eqChild leftA rightA && eqChild leftB rightB
      (Neg leftChild, Neg rightChild) ->
        eqChild leftChild rightChild
      _ ->
        False

instance Show1 ArithF where
  liftShowsPrec showChild _ depth arithNode =
    case arithNode of
      Num number ->
        showsUnaryWith showsPrec "Num" depth number
      Var index ->
        showsUnaryWith showsPrec "Var" depth index
      Add leftChild rightChild ->
        showsBinaryWith showChild showChild "Add" depth leftChild rightChild
      Mul leftChild rightChild ->
        showsBinaryWith showChild showChild "Mul" depth leftChild rightChild
      Neg child ->
        showsUnaryWith showChild "Neg" depth child

type ArithTag :: Type
data ArithTag
  = NumTag Int
  | VarTag Int
  | AddTag
  | MulTag
  | NegTag
  deriving stock (Eq, Ord, Show)

instance HasConstructorTag ArithF where
  type ConstructorTag ArithF = ArithTag

  constructorTag arithNode =
    case arithNode of
      Num number -> NumTag number
      Var index -> VarTag index
      Add {} -> AddTag
      Mul {} -> MulTag
      Neg {} -> NegTag

instance ZipMatch ArithF where
  zipMatch = zipSameNodeShape

type ArithView :: Type
data ArithView
  = NumView Int
  | VarView Int
  | AddView ArithView ArithView
  | MulView ArithView ArithView
  | NegView ArithView
  deriving stock (Eq, Ord, Show)

type NodeCount :: Type
newtype NodeCount = NodeCount Int
  deriving stock (Eq, Ord, Show)

instance JoinSemilattice NodeCount where
  join (NodeCount leftCount) (NodeCount rightCount) =
    NodeCount (max leftCount rightCount)

instance QC.Arbitrary NodeCount where
  arbitrary = NodeCount <$> QC.arbitrary

analysisSpec :: AnalysisSpec ArithF NodeCount
analysisSpec =
  semilatticeAnalysis arithNodeCount

arithTheorySpec :: TheorySpec ArithF
arithTheorySpec =
  TheorySpec
    { tsClassify = \case
        Add _ _ -> commutativeBinary Add
        Mul _ _ -> commutativeBinary Mul
        _ -> Ordinary
    }

arithNodeCount :: ArithF NodeCount -> NodeCount
arithNodeCount arithNode =
  case arithNode of
    Num _ -> NodeCount 1
    Var _ -> NodeCount 1
    Add (NodeCount leftCount) (NodeCount rightCount) ->
      NodeCount (leftCount + rightCount + 1)
    Mul (NodeCount leftCount) (NodeCount rightCount) ->
      NodeCount (leftCount + rightCount + 1)
    Neg (NodeCount childCount) ->
      NodeCount (childCount + 1)

numTerm :: Int -> Fix ArithF
numTerm value =
  Fix (Num value)

varTerm :: Int -> Fix ArithF
varTerm index =
  Fix (Var index)

addTermNode :: Fix ArithF -> Fix ArithF -> Fix ArithF
addTermNode leftTerm rightTerm =
  Fix (Add leftTerm rightTerm)

mulTermNode :: Fix ArithF -> Fix ArithF -> Fix ArithF
mulTermNode leftTerm rightTerm =
  Fix (Mul leftTerm rightTerm)

negTermNode :: Fix ArithF -> Fix ArithF
negTermNode childTerm =
  Fix (Neg childTerm)

viewArithTerm :: Fix ArithF -> ArithView
viewArithTerm termValue =
  case termValue of
    Fix arithNode ->
      case arithNode of
        Num value -> NumView value
        Var index -> VarView index
        Add leftTerm rightTerm -> AddView (viewArithTerm leftTerm) (viewArithTerm rightTerm)
        Mul leftTerm rightTerm -> MulView (viewArithTerm leftTerm) (viewArithTerm rightTerm)
        Neg childTerm -> NegView (viewArithTerm childTerm)

fromArithView :: ArithView -> Fix ArithF
fromArithView arithView =
  case arithView of
    NumView value -> numTerm value
    VarView index -> varTerm index
    AddView leftView rightView -> addTermNode (fromArithView leftView) (fromArithView rightView)
    MulView leftView rightView -> mulTermNode (fromArithView leftView) (fromArithView rightView)
    NegView childView -> negTermNode (fromArithView childView)
