module Moonlight.EGraph.Test.Arith.Cost
  ( analysisAwareCost,
    arithCost,
  )
where

import Moonlight.EGraph.Test.Arith.Core
  ( ArithF (..),
    NodeCount (..),
  )

import Moonlight.EGraph.Pure.Extraction (AnalysisCostAlgebra (..), CostAlgebra (..))
arithCost :: CostAlgebra ArithF Int
arithCost =
  CostAlgebra
    ( \arithNode ->
        case arithNode of
          Num _ -> 1
          Var _ -> 1
          Add leftCost rightCost -> leftCost + rightCost + 1
          Mul leftCost rightCost -> leftCost + rightCost + 1
          Neg childCost -> childCost + 1
    )

analysisAwareCost :: AnalysisCostAlgebra ArithF NodeCount Int
analysisAwareCost =
  AnalysisCostAlgebra
    ( \_ arithNode ->
        case arithNode of
          Num _ -> 1
          Var _ -> 1
          Add (NodeCount leftNodes, _) (NodeCount rightNodes, _) -> leftNodes + rightNodes + 1
          Mul (NodeCount leftNodes, _) (NodeCount rightNodes, _) -> leftNodes + rightNodes + 1
          Neg (NodeCount childNodes, _) -> childNodes + 1
    )
