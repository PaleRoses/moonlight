{-# LANGUAGE RankNTypes #-}

module Moonlight.Saturation.Test.ProtocolFixture
  ( ProbeRequest (..),
    ProbeAdvance (..),
    ProbeObstruction (..),
    probeMatchingAlgebra,
    probeUnitMatchingAlgebra,
    emptyPreparationAlgebra,
    probeRequestBatch,
    probeScopedDelta,
    probeScopeWeight,
  )
where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (find)
import Moonlight.Delta.Scope
  ( Scope,
    Scoped,
    dirtyScope,
    foldScope,
    scopedDelta,
    scopedDeltaSupport,
  )
import Moonlight.Saturation.Matching
  ( MatchingAlgebra (..),
    MatchingQuery (..),
  )

newtype ProbeRequest host = ProbeRequest
  { probeRequestValue :: Int
  }
  deriving stock (Eq, Ord, Show)

newtype ProbeAdvance host = ProbeAdvance
  { probeAdvanceAmount :: Int
  }
  deriving stock (Eq, Ord, Show)

data ProbeObstruction
  = NegativeProbeRequest !Int
  deriving stock (Eq, Ord, Show)

probeMatchingAlgebra ::
  MatchingAlgebra
    ()
    Int
    IntSet
    IntSet
    Int
    ProbeRequest
    ProbeAdvance
    ProbeObstruction
    Int
probeMatchingAlgebra =
  MatchingAlgebra
    { maInitialState = 0,
      maEnvironment = (),
      maPrepareQueries = prepareProbeQueries,
      maRunQueries = runProbeQueries,
      maPreviewQuery = \_state _world _query -> Nothing,
      maAdvanceState = \_delta advanceValue state ->
        state + probeAdvanceAmount advanceValue,
      maReplayDiagnostics = const Nothing
    }

probeUnitMatchingAlgebra ::
  MatchingAlgebra
    ()
    Int
    IntSet
    IntSet
    ()
    ProbeRequest
    ProbeAdvance
    ProbeObstruction
    Int
probeUnitMatchingAlgebra =
  MatchingAlgebra
    { maInitialState = 0,
      maEnvironment = (),
      maPrepareQueries = prepareProbeQueries,
      maRunQueries = \state () -> runProbeQueries state 0,
      maPreviewQuery = \_state () _query -> Nothing,
      maAdvanceState = \_delta advanceValue state ->
        state + probeAdvanceAmount advanceValue,
      maReplayDiagnostics = const Nothing
    }

emptyPreparationAlgebra ::
  MatchingAlgebra
    ()
    Int
    IntSet
    IntSet
    Int
    ProbeRequest
    ProbeAdvance
    ProbeObstruction
    Int
emptyPreparationAlgebra =
  probeMatchingAlgebra
    { maPrepareQueries = \state _delta _world _requests -> (state + 1, [])
    }

prepareProbeQueries ::
  forall host world.
  Int ->
  Scoped IntSet IntSet ->
  world ->
  [ProbeRequest host] ->
  (Int, [MatchingQuery IntSet ProbeRequest host])
prepareProbeQueries state matchingDelta _world requests =
  ( state + length requests,
    fmap (MatchingQuery (scopedDeltaSupport matchingDelta)) requests
  )

runProbeQueries ::
  forall host.
  Int ->
  Int ->
  [MatchingQuery IntSet ProbeRequest host] ->
  (Int, Either ProbeObstruction [[Int]])
runProbeQueries state world queries =
  ( state + length queries,
    case find ((< 0) . probeRequestValue . mqRequest) queries of
      Just rejectedQuery ->
        Left (NegativeProbeRequest (probeRequestValue (mqRequest rejectedQuery)))
      Nothing ->
        Right (fmap (probeMatches world) queries)
  )

probeMatches :: Int -> MatchingQuery IntSet ProbeRequest host -> [Int]
probeMatches world matchingQuery =
  [ world
      + probeRequestValue (mqRequest matchingQuery)
      + probeScopeWeight (mqScope matchingQuery)
  ]

probeRequestBatch :: Int -> [ProbeRequest host]
probeRequestBatch size =
  fmap ProbeRequest [0 .. max 0 size - 1]

probeScopedDelta :: Int -> Scoped IntSet IntSet
probeScopedDelta size =
  let keys = IntSet.fromDistinctAscList [0 .. max 0 size - 1]
   in scopedDelta (dirtyScope keys) (Just keys)

probeScopeWeight :: Scope IntSet -> Int
probeScopeWeight =
  foldScope 0 IntSet.size 1
