{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Test.Ring.Core
  ( RingF (..),
    RingTag (..),
    NodeCount (..),
    RingTermView (..),
    ringAnalysis,
    ringNodeCount,
    ringCost,
    ringVar,
    ringAdd,
    ringMul,
    ringNeg,
    ringZero,
    ringOne,
    viewRingTerm,
  )
where

import Moonlight.Core (ZipMatch (..))
import Moonlight.Algebra (JoinSemilattice (join))
import Data.Kind (Type)
import Moonlight.Core (ConstructorTag, HasConstructorTag (..), zipSameNodeShape)
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec, semilatticeAnalysis)
import Moonlight.EGraph.Pure.Extraction (CostAlgebra (..))
import Data.Fix (Fix (..))

type RingF :: Type -> Type
data RingF a
  = Var String
  | Num Int
  | Add a a
  | Mul a a
  | Neg a
  | RZero
  | ROne
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type RingTag :: Type
data RingTag
  = VarTag String
  | NumTag Int
  | AddTag
  | MulTag
  | NegTag
  | RZeroTag
  | ROneTag
  deriving stock (Eq, Ord, Show)

instance HasConstructorTag RingF where
  type ConstructorTag RingF = RingTag
  constructorTag = \case
    Var name -> VarTag name
    Num value -> NumTag value
    Add {} -> AddTag
    Mul {} -> MulTag
    Neg {} -> NegTag
    RZero -> RZeroTag
    ROne -> ROneTag

instance ZipMatch RingF where
  zipMatch = zipSameNodeShape

type NodeCount :: Type
newtype NodeCount = NodeCount Int
  deriving stock (Eq, Ord, Show)

instance JoinSemilattice NodeCount where
  join (NodeCount leftCount) (NodeCount rightCount) =
    NodeCount (max leftCount rightCount)

ringAnalysis :: AnalysisSpec RingF NodeCount
ringAnalysis =
  semilatticeAnalysis ringNodeCount

ringNodeCount :: RingF NodeCount -> NodeCount
ringNodeCount = \case
  Var _ -> NodeCount 1
  Num _ -> NodeCount 1
  Add (NodeCount leftCount) (NodeCount rightCount) -> NodeCount (leftCount + rightCount + 1)
  Mul (NodeCount leftCount) (NodeCount rightCount) -> NodeCount (leftCount + rightCount + 1)
  Neg (NodeCount childCount) -> NodeCount (childCount + 1)
  RZero -> NodeCount 1
  ROne -> NodeCount 1

ringCost :: CostAlgebra RingF Int
ringCost =
  CostAlgebra $ \case
    Var _ -> 1
    Num _ -> 1
    Add leftCost rightCost -> leftCost + rightCost + 1
    Mul leftCost rightCost -> leftCost + rightCost + 1
    Neg childCost -> childCost + 1
    RZero -> 1
    ROne -> 1

ringVar :: String -> Fix RingF
ringVar name =
  Fix (Var name)

ringAdd :: Fix RingF -> Fix RingF -> Fix RingF
ringAdd leftTerm rightTerm =
  Fix (Add leftTerm rightTerm)

ringMul :: Fix RingF -> Fix RingF -> Fix RingF
ringMul leftTerm rightTerm =
  Fix (Mul leftTerm rightTerm)

ringNeg :: Fix RingF -> Fix RingF
ringNeg childTerm =
  Fix (Neg childTerm)

ringZero :: Fix RingF
ringZero =
  Fix RZero

ringOne :: Fix RingF
ringOne =
  Fix ROne

type RingTermView :: Type
data RingTermView
  = VarView String
  | NumView Int
  | AddView RingTermView RingTermView
  | MulView RingTermView RingTermView
  | NegView RingTermView
  | RZeroView
  | ROneView
  deriving stock (Eq, Show)

viewRingTerm :: Fix RingF -> RingTermView
viewRingTerm (Fix ringNode) =
  case ringNode of
    Var name -> VarView name
    Num value -> NumView value
    Add leftTerm rightTerm -> AddView (viewRingTerm leftTerm) (viewRingTerm rightTerm)
    Mul leftTerm rightTerm -> MulView (viewRingTerm leftTerm) (viewRingTerm rightTerm)
    Neg childTerm -> NegView (viewRingTerm childTerm)
    RZero -> RZeroView
    ROne -> ROneView
