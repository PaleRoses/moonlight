module ConstraintProbabilityBench
  ( constraintProbabilityBenchmarks,
  )
where

import Control.DeepSeq (NFData (rnf))
import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Moonlight.Constraint
  ( SlotId (..),
    WFCError,
    WFCSearchResult (..),
  )
import Moonlight.Constraint.Pure.WFC.Probability
  ( ProbabilisticWFCProblem (..),
    defaultWFCProbabilityOptions,
    solveProbabilisticWFCWith,
  )
import BenchSupport (caseLabel, keys, mediumSizes)
import Moonlight.Probability (Categorical, mkCategorical)
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)
import Test.Tasty.HUnit (assertFailure, testCase)

type Tile :: Type
data Tile = LowTile | HighTile
  deriving stock (Eq, Ord)

data ProbabilityBenchmarkFailure
  = ProbabilityDistributionFailure !String
  | ProbabilitySearchFailure !String
  | ProbabilityUnexpectedUnsatisfiable
  | ProbabilityUnexpectedBacktrackLimit
  deriving stock (Show)

instance NFData ProbabilityBenchmarkFailure where
  rnf failure =
    case failure of
      ProbabilityDistributionFailure detail -> rnf detail
      ProbabilitySearchFailure detail -> rnf detail
      ProbabilityUnexpectedUnsatisfiable -> ()
      ProbabilityUnexpectedBacktrackLimit -> ()

constraintProbabilityBenchmarks :: Benchmark
constraintProbabilityBenchmarks =
  bgroup
    "constraint-probability"
    [ bgroup "solve-weighted-independent" (fmap solveBenchmark mediumSizes)
    ]

solveBenchmark :: Int -> Benchmark
solveBenchmark size =
  let label = caseLabel "slots" size
   in case probabilityProblem size of
        Left obstruction ->
          testCase label (assertFailure (show obstruction))
        Right fixture ->
          case probabilitySolvedSize fixture of
            Left obstruction ->
              testCase label (assertFailure (show obstruction))
            Right _ ->
              bench label (nf probabilitySolvedSize fixture)

probabilitySolvedSize :: ProbabilisticWFCProblem Int Tile -> Either ProbabilityBenchmarkFailure Int
probabilitySolvedSize problem = do
  result <-
    first probabilitySearchFailure
      (solveProbabilisticWFCWith defaultWFCProbabilityOptions problem)
  searchResultSize result

probabilitySearchFailure :: WFCError Int -> ProbabilityBenchmarkFailure
probabilitySearchFailure =
  ProbabilitySearchFailure . show

searchResultSize :: WFCSearchResult Int Tile -> Either ProbabilityBenchmarkFailure Int
searchResultSize result =
  case result of
    WFCSolved assignments -> Right (Map.size assignments)
    WFCUnsatisfiable -> Left ProbabilityUnexpectedUnsatisfiable
    WFCBacktrackLimitReached -> Left ProbabilityUnexpectedBacktrackLimit

probabilityProblem :: Int -> Either ProbabilityBenchmarkFailure (ProbabilisticWFCProblem Int Tile)
probabilityProblem size = do
  distribution <- tileDistribution
  pure
    ProbabilisticWFCProblem
      { pwfcProblemDomains = Map.fromAscList (fmap (slotDistribution distribution) (keys size)),
        pwfcProblemAdjacencyRules = []
      }

slotDistribution :: Categorical Tile -> Int -> (SlotId Int, Categorical Tile)
slotDistribution distribution key =
  (SlotId key, distribution)

tileDistribution :: Either ProbabilityBenchmarkFailure (Categorical Tile)
tileDistribution =
  first
    (ProbabilityDistributionFailure . show)
    (mkCategorical (Map.fromList [(LowTile, 1.0), (HighTile, 3.0)]))
