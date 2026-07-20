module Moonlight.EGraph.Core.AnalysisSpec
  ( tests,
  )
where

import Data.Kind ( Type )
import Moonlight.Algebra ( JoinSemilattice(join) )
import Moonlight.EGraph.Pure.Analysis
    ( asJoin,
      asMake,
      semilatticeAnalysis,
      AnalysisSpec )
import Moonlight.EGraph.Test.Case (HUnitCase (..), hunitCases)
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( (@?=) )

type PairF :: Type -> Type
data PairF a = PairF a a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type MaxInt :: Type
newtype MaxInt = MaxInt Int
  deriving stock (Eq, Show)

instance JoinSemilattice MaxInt where
  join (MaxInt left) (MaxInt right) =
    MaxInt (max left right)

analysisSpec :: AnalysisSpec PairF MaxInt
analysisSpec =
  semilatticeAnalysis
    (\(PairF (MaxInt left) (MaxInt right)) -> MaxInt (left + right))

tests :: TestTree
tests =
  testGroup "analysis" . hunitCases $
    [ HUnitCase "asMake delegates to asMake" $
        asMake analysisSpec (PairF (MaxInt 2) (MaxInt 5)) @?= MaxInt 7,
      HUnitCase "asJoin delegates to semilattice join" $
        asJoin analysisSpec (MaxInt 2) (MaxInt 5) @?= MaxInt 5
    ]
