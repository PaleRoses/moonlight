module Moonlight.Saturation.MatchingSpec
  ( matchingTests,
  )
where

import Data.IntSet qualified as IntSet
import Moonlight.Delta.Scope
  ( cleanScope,
    dirtyScope,
    fullScope,
  )
import Moonlight.Saturation.Matching
  ( MatchingQuery (..),
    Scope,
    mapMatchingQueryScope,
    prepareSingleQuery,
    prepareUnitSingleQuery,
    runPreparedQueries,
    runSingleQuery,
    runUnitPreparedQueries,
    runUnitSingleQuery,
  )
import Moonlight.Saturation.Test.ProtocolFixture
  ( ProbeObstruction (..),
    ProbeRequest (..),
    emptyPreparationAlgebra,
    probeMatchingAlgebra,
    probeScopedDelta,
    probeUnitMatchingAlgebra,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

matchingTests :: TestTree
matchingTests =
  testGroup
    "matching protocol"
    [ testCase "single-query preparation preserves the matching frontier" $ do
        let matchingDelta = probeScopedDelta 3
            request = ProbeRequest 7 :: ProbeRequest ()
        prepareSingleQuery probeMatchingAlgebra 10 matchingDelta 0 request
          @?= (11, dirtyScope (IntSet.fromList [0, 1, 2])),
      testCase "empty preparation descends to a clean scope" $ do
        let request = ProbeRequest 7 :: ProbeRequest ()
        prepareSingleQuery emptyPreparationAlgebra 10 (probeScopedDelta 3) 0 request
          @?= (11, cleanScope),
      testCase "single-query execution returns its typed result" $ do
        let request = ProbeRequest 7 :: ProbeRequest ()
            matchingScope = dirtyScope (IntSet.fromList [1, 2, 3])
        runSingleQuery probeMatchingAlgebra 10 5 matchingScope request
          @?= (11, Right [15]),
      testCase "prepared batches preserve request order and local scope" $ do
        let requests =
              [ (cleanScope, ProbeRequest 1),
                (dirtyScope (IntSet.fromList [2, 3]), ProbeRequest 2)
              ] :: [(Scope IntSet.IntSet, ProbeRequest ())]
        runPreparedQueries probeMatchingAlgebra 0 5 requests
          @?= (2, Right [[6], [9]]),
      testCase "matching obstruction remains a typed failure" $ do
        let request = ProbeRequest (-1) :: ProbeRequest ()
        runSingleQuery probeMatchingAlgebra 0 0 cleanScope request
          @?= (1, Left (NegativeProbeRequest (-1))),
      testCase "scope transport changes only the query scope" $ do
        let request = ProbeRequest 4 :: ProbeRequest ()
            transported :: MatchingQuery IntSet.IntSet ProbeRequest ()
            transported =
              mapMatchingQueryScope
                (const fullScope)
                (MatchingQuery cleanScope request)
        mqScope transported @?= fullScope
        mqRequest transported @?= request,
      testCase "unit wrappers agree with their explicit-world forms" $ do
        let request = ProbeRequest 3 :: ProbeRequest ()
            matchingDelta = probeScopedDelta 2
            matchingScope = dirtyScope (IntSet.fromList [0, 1])
            batch = [(matchingScope, request)]
        prepareUnitSingleQuery probeUnitMatchingAlgebra 0 matchingDelta request
          @?= prepareSingleQuery probeUnitMatchingAlgebra 0 matchingDelta () request
        runUnitSingleQuery probeUnitMatchingAlgebra 0 matchingScope request
          @?= runSingleQuery probeUnitMatchingAlgebra 0 () matchingScope request
        runUnitPreparedQueries probeUnitMatchingAlgebra 0 batch
          @?= runPreparedQueries probeUnitMatchingAlgebra 0 () batch,
      testCase "empty prepared batches preserve state" $ do
        let noRequests = [] :: [(Scope IntSet.IntSet, ProbeRequest ())]
        runPreparedQueries probeMatchingAlgebra 13 5 noRequests
          @?= (13, Right [])
    ]
