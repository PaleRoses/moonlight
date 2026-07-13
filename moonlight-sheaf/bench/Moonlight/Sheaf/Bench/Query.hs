{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Sheaf.Bench.Query
  ( queryBenchmarks,
  )
where

import Control.DeepSeq (NFData (..))
import Control.Exception (evaluate)
import Data.Vector qualified as Vector
import Data.Vector.Unboxed qualified as Unboxed
import Data.Word (Word64)
import Moonlight.Core (mkSlotId)
import Moonlight.Differential.Row.Block
  ( RowBlock,
    RowBuildError,
    RowBlockIdentity (..),
    RowDesc,
    RowState (Canonical),
    foldRowBlock,
    fromSlotRows,
    rowSlots,
  )
import Moonlight.Sheaf.Query.Restriction
  ( ObstructionVerdict,
    PruningReport (..),
    RowPruningObstruction (..),
    RowPruningResult (..),
    pruneRowsWithVerdict,
    rowPruningVerdict,
  )
import Moonlight.Sheaf.Bench.QueryTrianglePowerLaw
  ( trianglePowerLawBenchmarks,
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    env,
    nf,
  )

queryBenchmarks :: IO Benchmark
queryBenchmarks = do
  triangleBenchmarks <- trianglePowerLawBenchmarks
  pure
    ( bgroup
        "query"
        [ rowRestrictionBenchmarks,
          triangleBenchmarks
        ]
    )

rowRestrictionBenchmarks :: Benchmark
rowRestrictionBenchmarks =
  env setupQueryBenchEnv $ \benchEnv ->
    bgroup
      "row-restriction-pruning"
      [ bench "prune/keep-half-1k" (nf (pruneRowsMeasure keepOddRows) (qbeRows1k benchEnv)),
        bench "prune/keep-half-8k" (nf (pruneRowsMeasure keepOddRows) (qbeRows8k benchEnv)),
        bench "prune/all-accepted-8k" (nf (pruneRowsMeasure keepAllRows) (qbeRows8k benchEnv))
      ]

data QueryBenchEnv = QueryBenchEnv
  { qbeRows1k :: !(RowBlock 'Canonical),
    qbeRows8k :: !(RowBlock 'Canonical)
  }

instance NFData QueryBenchEnv where
  rnf benchEnv =
    rowBlockWeight (qbeRows1k benchEnv)
      `seq` rowBlockWeight (qbeRows8k benchEnv)
      `seq` ()

setupQueryBenchEnv :: IO QueryBenchEnv
setupQueryBenchEnv =
  either (ioError . userError . show) evaluate queryBenchEnv

queryBenchEnv :: Either RowBuildError QueryBenchEnv
queryBenchEnv =
  QueryBenchEnv
    <$> rowsFromWords (singleColumnRows 1024)
    <*> rowsFromWords (singleColumnRows 8192)

singleColumnRows :: Int -> [[Word64]]
singleColumnRows rowCount =
  fmap (\rowValue -> [fromIntegral rowValue]) [1 .. rowCount]

rowsFromWords :: [[Word64]] -> Either RowBuildError (RowBlock 'Canonical)
rowsFromWords =
  fromSlotRows neutralIdentity (Vector.singleton (mkSlotId 0))
    . fmap Unboxed.fromList

neutralIdentity :: RowBlockIdentity
neutralIdentity =
  RowBlockIdentity
    { rowBlockBaseRevision = 0,
      rowBlockOverlayEpoch = 0,
      rowBlockPlanFingerprint = 0,
      rowBlockEntityKey = 0,
      rowBlockGeneration = 0
    }

data QueryBenchMeasure
  = QueryBenchMeasured !Int
  | QueryBenchObstructed !RowBuildError
  deriving stock (Eq, Show)

instance NFData QueryBenchMeasure where
  rnf measure =
    case measure of
      QueryBenchMeasured weightValue -> rnf weightValue
      QueryBenchObstructed failure -> failure `seq` ()

type RowKeepPredicate =
  RowBlock 'Canonical ->
  RowDesc ->
  Either RowBuildError (ObstructionVerdict RowPruningObstruction)

pruneRowsMeasure :: RowKeepPredicate -> RowBlock 'Canonical -> QueryBenchMeasure
pruneRowsMeasure keep rows =
  either
    QueryBenchObstructed
    (QueryBenchMeasured . rowPruningResultWeight)
    (pruneRowsWithVerdict id neutralIdentity keep rows)

rowPruningResultWeight :: RowPruningResult RowPruningObstruction -> Int
rowPruningResultWeight result =
  rowBlockWeight (rprRows result)
    + length (prLive report)
    + length (prPruned report)
  where
    report =
      rprReport result

keepOddRows :: RowKeepPredicate
keepOddRows rows desc =
  Right (rowPruningVerdict LocalRowAbsent (odd (rowSlotChecksum rows desc)))

keepAllRows :: RowKeepPredicate
keepAllRows rows desc =
  Right (rowPruningVerdict LocalRowAbsent (rowSlotChecksum rows desc /= maxBound))

rowBlockWeight :: RowBlock 'Canonical -> Int
rowBlockWeight rows =
  foldRowBlock (\weight desc -> weight + rowSlotChecksum rows desc) 0 rows

rowSlotChecksum :: RowBlock 'Canonical -> RowDesc -> Int
rowSlotChecksum rows =
  fromIntegral . Unboxed.sum . rowSlots rows
