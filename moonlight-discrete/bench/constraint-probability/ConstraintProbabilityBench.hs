module ConstraintProbabilityBench
  ( constraintProbabilityBenchmarks,
  )
where

import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Moonlight.Constraint
  ( SlotId (..),
    WFCSearchResult (..),
  )
import Moonlight.Constraint.Pure.WFC.Probability
  ( ProbabilisticWFCProblem (..),
    defaultWFCProbabilityOptions,
    solveProbabilisticWFCWith,
  )
import BenchSupport (benchFailure, caseLabel, keys, mediumSizes)
import Moonlight.Probability (Categorical, mkCategorical)
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

type Tile :: Type
data Tile = LowTile | HighTile
  deriving stock (Eq, Ord, Show)

constraintProbabilityBenchmarks :: Benchmark
constraintProbabilityBenchmarks =
  bgroup
    "constraint-probability"
    [ bgroup "solve-weighted-independent" (fmap solveBenchmark mediumSizes)
    ]

solveBenchmark :: Int -> Benchmark
solveBenchmark size =
  bench (caseLabel "slots" size) (nf probabilitySolvedSize size)

probabilitySolvedSize :: Int -> Int
probabilitySolvedSize size =
  case solveProbabilisticWFCWith defaultWFCProbabilityOptions (probabilityProblem size) of
    Left err -> benchFailure "probabilistic WFC" err
    Right result -> searchResultSize result

searchResultSize :: WFCSearchResult Int Tile -> Int
searchResultSize result =
  case result of
    WFCSolved assignments -> Map.size assignments
    WFCUnsatisfiable -> 0
    WFCBacktrackLimitReached -> 0

probabilityProblem :: Int -> ProbabilisticWFCProblem Int Tile
probabilityProblem size =
  ProbabilisticWFCProblem
    { pwfcProblemDomains = Map.fromAscList (fmap slotDistribution (keys size)),
      pwfcProblemAdjacencyRules = []
    }

slotDistribution :: Int -> (SlotId Int, Categorical Tile)
slotDistribution key =
  (SlotId key, tileDistribution)

tileDistribution :: Categorical Tile
tileDistribution =
  case mkCategorical (Map.fromList [(LowTile, 1.0), (HighTile, 3.0)]) of
    Left err -> benchFailure "tile categorical" err
    Right distribution -> distribution
