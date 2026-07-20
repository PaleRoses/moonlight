{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Moonlight.EGraph.Test.Arith.Matcher
  ( buildGraph,
    bindingsView,
    directMatchPattern,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.Kind (Type)
import Control.Monad (foldM)
import Moonlight.Core (UnionFindAllocationError)
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.Core
  ( Pattern (..)
  )
import Moonlight.Core qualified as EGraph
import Moonlight.EGraph.Pure.Types (EGraph, emptyEGraph)
import Moonlight.EGraph.Test.Arith.Core
  ( ArithF (..),
    ArithView (..),
    NodeCount,
    analysisSpec,
    fromArithView,
    numTerm,
    varTerm,
    addTermNode,
    mulTermNode,
    negTermNode,
    viewArithTerm,
  )
import Data.Fix (Fix (..))
import Moonlight.Core.Pattern.Automata
  ( compilePatternAutomaton,
    matchPatternAutomaton,
  )
import qualified Test.Tasty.QuickCheck as QC
instance QC.Arbitrary (Fix ArithF) where
  arbitrary = QC.sized arbitraryArithTerm
  shrink = fmap fromArithView . shrinkArithView . viewArithTerm

instance QC.Arbitrary (Pattern ArithF) where
  arbitrary = QC.sized arbitraryPattern
  shrink = fmap fromPatternView . shrinkPatternView . patternView

instance QC.Arbitrary ArithView where
  arbitrary = viewArithTerm <$> (QC.arbitrary :: QC.Gen (Fix ArithF))
  shrink = shrinkArithView

buildGraph :: [Fix ArithF] -> Either UnionFindAllocationError (EGraph ArithF NodeCount)
buildGraph =
  foldM
    (\graph termValue -> snd <$> addTerm termValue graph)
    (emptyEGraph analysisSpec)

bindingsView :: IntMap (Fix ArithF) -> IntMap ArithView
bindingsView =
  fmap viewArithTerm

directMatchPattern :: Pattern ArithF -> Fix ArithF -> IntMap (Fix ArithF) -> Maybe (IntMap (Fix ArithF))
directMatchPattern patternValue termValue bindings =
  matchPatternAutomaton (compilePatternAutomaton patternValue) termValue bindings

arbitraryArithTerm :: Int -> QC.Gen (Fix ArithF)
arbitraryArithTerm size =
  if size <= 0
    then QC.oneof [numTerm <$> smallInt, varTerm <$> smallInt]
    else
      QC.oneof
        [ numTerm <$> smallInt,
          varTerm <$> smallInt,
          addTermNode
            <$> QC.resize (size `div` 2) (arbitraryArithTerm (size `div` 2))
            <*> QC.resize (size `div` 2) (arbitraryArithTerm (size `div` 2)),
          mulTermNode
            <$> QC.resize (size `div` 2) (arbitraryArithTerm (size `div` 2))
            <*> QC.resize (size `div` 2) (arbitraryArithTerm (size `div` 2)),
          negTermNode
            <$> QC.resize (size `div` 2) (arbitraryArithTerm (size `div` 2))
        ]

smallInt :: QC.Gen Int
smallInt =
  QC.chooseInt (0, 3)

shrinkArithView :: ArithView -> [ArithView]
shrinkArithView arithView =
  case arithView of
    NumView value -> fmap NumView (QC.shrink value)
    VarView index -> fmap VarView (QC.shrink index)
    AddView leftView rightView ->
      [leftView, rightView]
        <> fmap (`AddView` rightView) (shrinkArithView leftView)
        <> fmap (AddView leftView) (shrinkArithView rightView)
    MulView leftView rightView ->
      [leftView, rightView]
        <> fmap (`MulView` rightView) (shrinkArithView leftView)
        <> fmap (MulView leftView) (shrinkArithView rightView)
    NegView childView ->
      [childView]
        <> fmap NegView (shrinkArithView childView)

type PatternView :: Type
data PatternView
  = PatternVarView Int
  | PatternNumView Int
  | VarPatternView Int
  | PatternAddView PatternView PatternView
  | MulPatternView PatternView PatternView
  | NegPatternView PatternView
  deriving stock (Eq, Show)

patternView :: Pattern ArithF -> PatternView
patternView patternValue =
  case patternValue of
    PatternVar patternVar -> PatternVarView (EGraph.patternVarKey patternVar)
    PatternNode arithNode ->
      case arithNode of
        Num value -> PatternNumView value
        Var index -> VarPatternView index
        Add leftPattern rightPattern -> PatternAddView (patternView leftPattern) (patternView rightPattern)
        Mul leftPattern rightPattern -> MulPatternView (patternView leftPattern) (patternView rightPattern)
        Neg childPattern -> NegPatternView (patternView childPattern)

fromPatternView :: PatternView -> Pattern ArithF
fromPatternView patternValue =
  case patternValue of
    PatternVarView patternKey -> PatternVar (EGraph.mkPatternVar patternKey)
    PatternNumView value -> PatternNode (Num value)
    VarPatternView index -> PatternNode (Var index)
    PatternAddView leftPattern rightPattern ->
      PatternNode (Add (fromPatternView leftPattern) (fromPatternView rightPattern))
    MulPatternView leftPattern rightPattern ->
      PatternNode (Mul (fromPatternView leftPattern) (fromPatternView rightPattern))
    NegPatternView childPattern ->
      PatternNode (Neg (fromPatternView childPattern))

arbitraryPattern :: Int -> QC.Gen (Pattern ArithF)
arbitraryPattern size =
  if size <= 0
    then QC.oneof [PatternVar <$> arbitraryPatternVar, PatternNode . Num <$> smallInt]
    else
      QC.frequency
        [ (1, PatternVar <$> arbitraryPatternVar),
          (2, PatternNode . Num <$> smallInt),
          (2, PatternNode . Var <$> smallInt),
          (3, PatternNode <$> (Add <$> nextPattern <*> nextPattern)),
          (3, PatternNode <$> (Mul <$> nextPattern <*> nextPattern)),
          (2, PatternNode <$> (Neg <$> nextPattern))
        ]
  where
    nextPattern = QC.resize (size `div` 2) (arbitraryPattern (size `div` 2))

arbitraryPatternVar :: QC.Gen EGraph.PatternVar
arbitraryPatternVar =
  EGraph.mkPatternVar <$> QC.chooseInt (0, 2)

shrinkPatternView :: PatternView -> [PatternView]
shrinkPatternView patternValue =
  case patternValue of
    PatternVarView patternKey -> fmap PatternVarView (QC.shrink patternKey)
    PatternNumView value -> fmap PatternNumView (QC.shrink value)
    VarPatternView index -> fmap VarPatternView (QC.shrink index)
    PatternAddView leftPattern rightPattern ->
      [leftPattern, rightPattern]
        <> fmap (`PatternAddView` rightPattern) (shrinkPatternView leftPattern)
        <> fmap (PatternAddView leftPattern) (shrinkPatternView rightPattern)
    MulPatternView leftPattern rightPattern ->
      [leftPattern, rightPattern]
        <> fmap (`MulPatternView` rightPattern) (shrinkPatternView leftPattern)
        <> fmap (MulPatternView leftPattern) (shrinkPatternView rightPattern)
    NegPatternView childPattern ->
      [childPattern]
        <> fmap NegPatternView (shrinkPatternView childPattern)
