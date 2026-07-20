module Moonlight.Analysis.Mesh.GraphSpec
  ( tests
  ) where

import Data.Vector.Unboxed qualified as VU
import Moonlight.Analysis.Mesh.Graph
  ( DirectedPairOrientation(..)
  , FaceDirectedPairIncidence
  , FacePairIncidenceObstruction(..)
  , FacePairVectorComponent(..)
  , Graph(..)
  , buildFaceDirectedPairIncidence
  , edgeRange
  , faceDirectedPairFaceCount
  , faceDirectedPairIdAt
  , faceDirectedPairOrientationAt
  , faceDirectedPairRange
  , pairMetricNormalFactor
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Prelude


tests :: TestTree
tests =
  testGroup
    "mesh-graph"
    [ testCase "edge ranges retain canonical CSR order" edgeRangeOrderSpec
    , testCase "pair metric contracts the stored normal in field order" pairMetricNormalFactorSpec
    , testCase "canonical CSR incidence retains graph order exactly" canonicalFacePairIncidenceSpec
    , testCase "face incidence glues pair order across asymmetric duplicate CSR" facePairIncidenceOrderSpec
    , testCase "face incidence treats every nonzero direction flag as present" facePairIncidenceNoncanonicalFlagSpec
    , testCase "face incidence rejects malformed CSR and pair lengths" facePairIncidenceObstructionSpec
    ]


edgeRangeOrderSpec :: IO ()
edgeRangeOrderSpec =
  fmap (edgeRange twoFaceGraph) [0, 1] @?= [(0, 1), (1, 2)]


pairMetricNormalFactorSpec :: IO ()
pairMetricNormalFactorSpec =
  pairMetricNormalFactor twoFaceGraph 0 @?= 3.0


canonicalFacePairIncidenceSpec :: IO ()
canonicalFacePairIncidenceSpec =
  fmap logicalIncidence (buildFaceDirectedPairIncidence twoFaceGraph)
    @?= Right
      [ [(0, PairFromA)]
      , [(0, PairFromB)]
      ]


facePairIncidenceOrderSpec :: IO ()
facePairIncidenceOrderSpec =
  case buildFaceDirectedPairIncidence mixedDirectionGraph of
    Left obstruction ->
      assertFailure ("unexpected incidence obstruction: " <> show obstruction)
    Right incidence -> do
      faceDirectedPairFaceCount incidence @?= 3
      logicalIncidence incidence
        @?=
          [ [ (1, PairFromA)
            , (2, PairFromA)
            , (2, PairFromB)
            ]
          , [ (0, PairFromA)
            , (3, PairFromB)
            ]
          , [(0, PairFromB)]
          ]


facePairIncidenceNoncanonicalFlagSpec :: IO ()
facePairIncidenceNoncanonicalFlagSpec =
  fmap logicalIncidence
    (buildFaceDirectedPairIncidence mixedDirectionGraph)
    @?= fmap logicalIncidence
      ( buildFaceDirectedPairIncidence
          mixedDirectionGraph
            { grPairHasAB = VU.fromList [1, 2, 3, 0]
            , grPairHasBA = VU.fromList [1, 0, 4, 5]
            }
      )


facePairIncidenceObstructionSpec :: IO ()
facePairIncidenceObstructionSpec = do
  assertIncidenceObstruction
    (FaceOffsetLengthMismatch 4 3)
    mixedDirectionGraph {grOffsets = VU.fromList [0, 3, 5]}
  assertIncidenceObstruction
    (FacePairVectorLengthMismatch PairFaceB 4 3)
    mixedDirectionGraph {grPairB = VU.fromList [2, 2, 0]}
  assertIncidenceObstruction
    (FacePairEndpointOutOfRange PairFromA 0 3 3)
    mixedDirectionGraph {grPairA = VU.fromList [3, 0, 0, 0]}
  assertIncidenceObstruction
    (DirectedEdgeEndpointMismatch 0 0 1 2 0 0)
    mixedDirectionGraph
      { grNbrs = VU.fromList [1, 2, 0, 0, 2, 1]
      }
  assertIncidenceObstruction
    (DirectedEdgeEndpointMismatch 0 0 0 0 0 1)
    twoFaceGraph
      { grNbrs = VU.fromList [0, 1]
      }


assertIncidenceObstruction
  :: FacePairIncidenceObstruction
  -> Graph
  -> IO ()
assertIncidenceObstruction !expected !graphValue =
  case buildFaceDirectedPairIncidence graphValue of
    Left actual -> actual @?= expected
    Right _ ->
      assertFailure
        ("expected incidence obstruction: " <> show expected)


logicalIncidence
  :: FaceDirectedPairIncidence
  -> [[(Int, DirectedPairOrientation)]]
logicalIncidence !incidence =
  fmap logicalFace [0 .. faceDirectedPairFaceCount incidence - 1]
  where
    logicalFace !faceIndex =
      let (!lowerEntry, !upperEntry) =
            faceDirectedPairRange incidence faceIndex
      in fmap
           (\entryIndex ->
             ( faceDirectedPairIdAt incidence entryIndex
             , faceDirectedPairOrientationAt incidence entryIndex
             )
           )
           [lowerEntry .. upperEntry - 1]


mixedDirectionGraph :: Graph
mixedDirectionGraph =
  Graph
    { grFaces = 3
    , grOffsets = VU.fromList [0, 3, 5, 6]
    , grNbrs = VU.fromList [0, 2, 0, 0, 2, 1]
    , grEdgePair = VU.fromList [2, 1, 2, 3, 0, 0]
    , grPairA = VU.fromList [1, 0, 0, 0]
    , grPairB = VU.fromList [2, 2, 0, 1]
    , grPairHasAB = VU.fromList [1, 1, 1, 0]
    , grPairHasBA = VU.fromList [1, 0, 1, 1]
    , grPairBaseW = VU.replicate 4 0.5
    , grFaceArea = VU.replicate 3 1.0
    , grPairEdgeLen = VU.replicate 4 1.0
    , grPairCenterDist = VU.replicate 4 1.0
    , grPairNx = VU.replicate 4 1.0
    , grPairNy = VU.replicate 4 0.0
    , grPairMetric11 = VU.replicate 4 1.0
    , grPairMetric12 = VU.replicate 4 0.0
    , grPairMetric22 = VU.replicate 4 1.0
    , grFaceOutDeg = VU.fromList [3, 2, 1]
    , grNewToOld = VU.fromList [0, 1, 2]
    , grOldToNew = VU.fromList [0, 1, 2]
    }


twoFaceGraph :: Graph
twoFaceGraph =
  Graph
    { grFaces = 2
    , grOffsets = VU.fromList [0, 1, 2]
    , grNbrs = VU.fromList [1, 0]
    , grEdgePair = VU.fromList [0, 0]
    , grPairA = VU.singleton 0
    , grPairB = VU.singleton 1
    , grPairHasAB = VU.singleton 1
    , grPairHasBA = VU.singleton 1
    , grPairBaseW = VU.singleton 0.5
    , grFaceArea = VU.fromList [1.0, 2.0]
    , grPairEdgeLen = VU.singleton 2.0
    , grPairCenterDist = VU.singleton 4.0
    , grPairNx = VU.singleton 1.0
    , grPairNy = VU.singleton 0.0
    , grPairMetric11 = VU.singleton 3.0
    , grPairMetric12 = VU.singleton 0.0
    , grPairMetric22 = VU.singleton 1.0
    , grFaceOutDeg = VU.fromList [1, 1]
    , grNewToOld = VU.fromList [0, 1]
    , grOldToNew = VU.fromList [0, 1]
    }
