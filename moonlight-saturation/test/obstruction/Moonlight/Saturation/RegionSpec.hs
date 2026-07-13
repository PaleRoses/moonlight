module Moonlight.Saturation.RegionSpec
  ( regionTests,
  )
where

import Control.Foldl qualified as Foldl
import Moonlight.Saturation.Obstruction.Cohomological.Region
  ( RegionFoldAlgebra (..),
    regionFoldForRequest,
    regionFoldWith,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( (@?=),
    testCase,
  )

data ProbeRequest = ProbeRequest
  { prUpperBound :: !Int
  }
  deriving stock (Eq, Show)

type ProbeCache = [Int]

type ProbeRegion = Int

type ProbeSummary = Int

type ProbeAggregate = [Int]

probeAlgebra ::
  RegionFoldAlgebra
    ProbeCache
    ProbeRequest
    ProbeRegion
    ProbeSummary
    ProbeAggregate
probeAlgebra =
  RegionFoldAlgebra
    { rfaAcceptRegion =
        \request regionValue ->
          regionValue <= prUpperBound request,
      rfaAnalyzeRegion =
        \cacheValue _request regionValue ->
          (cacheValue <> [regionValue], regionValue * 10),
      rfaInsertSummary =
        \_request summaryValue aggregateValue ->
          aggregateValue <> [summaryValue],
      rfaInitialAggregate =
        const []
    }

regionTests :: TestTree
regionTests =
  testGroup
    "context-region"
    [ testCase "regionFoldForRequest preserves accepted-region order" $
        Foldl.fold
          ( regionFoldForRequest
              probeAlgebra
              []
              ProbeRequest {prUpperBound = 3}
          )
          [1, 4, 2, 5, 3]
          @?= ([1, 2, 3], [10, 20, 30]),
      testCase "regionFoldForRequest returns the untouched cache and initial aggregate on an empty region list" $
        let algebra =
              probeAlgebra
                { rfaInitialAggregate =
                    \request -> [prUpperBound request]
                }
         in Foldl.fold
              ( regionFoldForRequest
                  algebra
                  [99]
                  ProbeRequest {prUpperBound = 7}
              )
              []
              @?= ([99], [7]),
      testCase "regionFoldWith is extensionally equal to the record driver" $
        let request =
              ProbeRequest {prUpperBound = 10}
            regions =
              [3, 1, 4, 1, 5]
            directResult =
              Foldl.fold
                ( regionFoldWith
                    (rfaAcceptRegion probeAlgebra)
                    (rfaAnalyzeRegion probeAlgebra)
                    (rfaInsertSummary probeAlgebra)
                    (rfaInitialAggregate probeAlgebra)
                    []
                    request
                )
                regions
            recordResult =
              Foldl.fold
                (regionFoldForRequest probeAlgebra [] request)
                regions
         in directResult @?= recordResult
    ]
