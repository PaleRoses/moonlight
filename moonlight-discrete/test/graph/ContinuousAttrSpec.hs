module ContinuousAttrSpec
  ( tests,
  )
where

import Data.Text (pack)
import Moonlight.Algebra (Action (act))
import Moonlight.Graph
  ( AttrKey (..),
    ContinuousAttr (..),
    ContinuousDelta (MkContinuousDelta),
    applyContinuousDelta,
    foldDeltasByKey,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "ContinuousAttr"
    [ testCase "monoid identity leaves attributes unchanged" $ do
        assertEqual
          "identity delta"
          sampleAttr
          (applyContinuousDelta mempty sampleAttr),
      testCase "action is compatible with delta composition" $ do
        let composed = (<>) deltaAlpha deltaBeta
            appliedSeparately = act deltaAlpha (act deltaBeta sampleAttr)
        assertEqual "composed action" appliedSeparately (act composed sampleAttr),
      testCase "continuous delta composition is commutative" $ do
        assertEqual
          "commutative delta"
          ((<>) deltaAlpha deltaBeta)
          ((<>) deltaBeta deltaAlpha),
      testCase "foldDeltasByKey is deterministic across input order" $ do
        let leftFolded = foldDeltasByKey deltaEntriesForward
            rightFolded = foldDeltasByKey deltaEntriesReversed
        assertEqual "fold order should not affect per-key aggregation" leftFolded rightFolded
    ]

sampleAttr :: ContinuousAttr
sampleAttr =
  ContinuousAttr
    { continuousBase = 10.0,
      continuousPendingAdd = 1.5,
      continuousPendingMul = 2.0
    }

deltaAlpha :: ContinuousDelta
deltaAlpha = MkContinuousDelta 3.0 0.5

deltaBeta :: ContinuousDelta
deltaBeta = MkContinuousDelta (-1.0) 4.0

massKey :: AttrKey
massKey = AttrKey (pack "mass")

heatKey :: AttrKey
heatKey = AttrKey (pack "heat")

deltaEntriesForward :: [(AttrKey, ContinuousDelta)]
deltaEntriesForward =
  [ (massKey, deltaAlpha),
    (heatKey, deltaBeta),
    (massKey, deltaBeta)
  ]

deltaEntriesReversed :: [(AttrKey, ContinuousDelta)]
deltaEntriesReversed = reverse deltaEntriesForward
