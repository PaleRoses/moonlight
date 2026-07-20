-- | Allocation arbiter for the circuit's recursive fixpoint maintenance:
-- incremental Delete–Rederive advance (what 'advanceCircuit' ships) versus
-- re-saturating the whole fixpoint from scratch on every batch (the naive
-- baseline DRed replaces). Both lanes drive the SAME transitive-closure circuit
-- through the public 'advanceCircuit', so the allocation delta between them is
-- attributable to the maintenance strategy alone, not to any harness asymmetry.
-- The @result=@ figures differ by construction (the incremental lane emits only
-- per-batch reachability deltas; the re-saturate lane re-emits the full closure
-- of each prefix) and exist only to force the work; correctness is the law
-- fence's job (@propCircuitFixpoint@, the diamond fixture), not the bench's.
module CircuitFixpoint where

import Control.DeepSeq (NFData (..))
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Void (Void)
import Moonlight.Differential.Algebra.ZSet qualified as ZSet
import Common (eitherShow)
import Moonlight.Differential.Circuit
  ( Circuit,
    CircuitBuilder,
    InputPort,
    Node,
    SealedCircuit,
    advanceCircuit,
    buildCircuit,
    emptyCircuitBatch,
    feedInput,
    fixpointNode,
    indexByNode,
    inputNode,
    joinNodes,
    mapNode,
    outputDelta,
    withSealedCircuit,
  )
import Moonlight.Differential.Operator.Fixpoint (SemiNaiveBudget (..))

type Edge = (Int, Int)

data ClosurePorts s = ClosurePorts
  { closureEdgesPort :: !(InputPort s Edge),
    closureNode :: !(Node s Edge)
  }

closureBuilder :: SemiNaiveBudget -> CircuitBuilder s Void Int (ClosurePorts s)
closureBuilder budget = do
  (edgesPort, edges) <- inputNode
  closure <-
    fixpointNode budget edges $ \frontier -> do
      byTarget <- indexByNode snd frontier
      bySource <- indexByNode fst edges
      hops <- joinNodes byTarget bySource
      mapNode (\(_, (source, _), (_, target)) -> (source, target)) hops
  pure (ClosurePorts edgesPort closure)

-- | Batch @k@ inserts the path edge @(k, k+1)@; the closure grows by @k+1@
-- reachability pairs each batch, so incremental maintenance touches @O(k)@ while
-- a from-scratch recompute touches the whole @O(k^2)@ closure.
pathGrowthBatches :: Int -> [[(Edge, Int)]]
pathGrowthBatches n =
  [[((k, k + 1), 1)] | k <- [0 .. n - 1]]

-- | Path growth with retraction churn: every fourth batch also retracts an
-- earlier edge, splitting the path and cascading an over-delete of every pair
-- that crossed the removed edge — the DRed over-delete path the pure-insert
-- lane never exercises.
churnBatches :: Int -> [[(Edge, Int)]]
churnBatches n =
  [batchAt k | k <- [0 .. n - 1]]
  where
    batchAt :: Int -> [(Edge, Int)]
    batchAt k =
      ((k, k + 1), 1) : retractAt k
    retractAt :: Int -> [(Edge, Int)]
    retractAt k
      | k >= 4 && k `mod` 4 == 0 =
          [((k - 4, k - 3), -1)]
      | otherwise =
          []

data FixpointCase = FixpointCase
  { fixpointCaseBatches :: ![[(Edge, Int)]],
    fixpointCaseSealed :: !(SealedCircuit Void Int ClosurePorts)
  }

instance NFData FixpointCase where
  rnf (FixpointCase batches sealed) =
    rnf batches `seq` sealed `seq` ()

closureBudget :: Int -> SemiNaiveBudget
closureBudget n =
  SemiNaiveBudget (fromIntegral n + 8)

pathGrowthCase :: Int -> Either String FixpointCase
pathGrowthCase n =
  FixpointCase (pathGrowthBatches n)
    <$> eitherShow (buildCircuit (closureBuilder (closureBudget n)))

churnCase :: Int -> Either String FixpointCase
churnCase n =
  FixpointCase (churnBatches n)
    <$> eitherShow (buildCircuit (closureBuilder (closureBudget n)))

-- | The shipped strategy: one persistent circuit, per-batch edge deltas,
-- maintained incrementally by DRed.
incrementalWeight :: FixpointCase -> Either String Int
incrementalWeight (FixpointCase batches sealed) =
  withSealedCircuit sealed $ \circuit ports ->
    snd
      <$> Foldable.foldl'
        (advanceIncrementalBatch ports)
        (Right (circuit, 0))
        batches

advanceIncrementalBatch ::
  ClosurePorts s ->
  Either String (Circuit s Void Int, Int) ->
  [(Edge, Int)] ->
  Either String (Circuit s Void Int, Int)
advanceIncrementalBatch ports state delta =
  state >>= \(circuit, total) ->
    let batch =
          feedInput (closureEdgesPort ports) (ZSet.zsetFromList delta) emptyCircuitBatch
     in case advanceCircuit batch circuit of
          Left obstruction ->
            Left (show obstruction)
          Right (outputs, nextCircuit) -> do
            batchWeight <-
              eitherShow
                (fmap ZSet.zsetSize (outputDelta (closureNode ports) outputs))
            let nextTotal = total + batchWeight
            nextTotal `seq` Right (nextCircuit, nextTotal)

-- | The naive baseline: recompute the whole fixpoint from an empty circuit on
-- every prefix (a cold DRed advance from @P = ∅@ degenerates to a full eager
-- saturation of the cumulative edge set).
resaturateWeight :: FixpointCase -> Either String Int
resaturateWeight (FixpointCase batches sealed) =
  fmap sum (traverse (resaturatePrefix sealed) (nonEmptyPrefixes batches))

nonEmptyPrefixes :: [[(Edge, Int)]] -> [[(Edge, Int)]]
nonEmptyPrefixes =
  fmap concat . drop 1 . List.inits

resaturatePrefix ::
  SealedCircuit Void Int ClosurePorts ->
  [(Edge, Int)] ->
  Either String Int
resaturatePrefix sealed cumulativeDelta =
  withSealedCircuit sealed $ \circuit ports ->
    let batch =
          feedInput
            (closureEdgesPort ports)
            (ZSet.zsetFromList cumulativeDelta)
            emptyCircuitBatch
     in case advanceCircuit batch circuit of
          Left obstruction ->
            Left (show obstruction)
          Right (outputs, _) ->
            eitherShow
              (fmap ZSet.zsetSize (outputDelta (closureNode ports) outputs))
