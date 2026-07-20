module Moonlight.EGraph.Introspection.PruningSpec.Gluing
  ( tests,
  )
where

import Moonlight.Derived.Matrix (transposeMatChecked)
import Moonlight.Derived.Site (FinObjectId (..))
import Moonlight.Derived.Pruning
  ( laplacianGate,
    localSheafLaplacian,
    pruningGapAt
  )
import Moonlight.EGraph.Introspection.PruningSpec.CommonPrelude
import Moonlight.EGraph.Introspection.PruningSpec.Fixture
import Moonlight.Homology (HomologicalDegree (..))
import Moonlight.Sheaf.Obstruction (mkCandidateRegionSeed)
import Moonlight.Core (RegionNodeId (..))

tests :: TestTree
tests =
  testGroup
    "gluing"
    [ testCase "localSheafLaplacian is symmetric on a one-step incoming differential" testLocalSheafLaplacianSymmetry,
      testCase "pruningGapAt is zero on a local harmonic kernel" testPruningGapKernel,
      testCase "pruningGapAt is positive away from the local harmonic kernel" testPruningGapPositive,
      testCase "laplacianGate with threshold zero keeps every seed" testLaplacianGateThresholdZero
    ]

testLocalSheafLaplacianSymmetry :: Assertion
testLocalSheafLaplacianSymmetry =
  let laplacianValue =
        localSheafLaplacian
          chainPoset
          (HomologicalDegree 1)
          (FinObjectId 0)
          incomingDerived
   in transposeMatChecked laplacianValue @?= Right laplacianValue

testPruningGapPositive :: Assertion
testPruningGapPositive =
  fmap (> 0.0) (pruningGapAt chainPoset (HomologicalDegree 1) (FinObjectId 0) incomingDerived)
    @?= Right True

testPruningGapKernel :: Assertion
testPruningGapKernel =
  pruningGapAt sphereLikePoset (HomologicalDegree 0) (FinObjectId 0) zeroDerived @?= Right 0.0

testLaplacianGateThresholdZero :: Assertion
testLaplacianGateThresholdZero =
  let seedValue = mkCandidateRegionSeed () (RegionNodeId 0) 41
   in laplacianGate
        0.0
        (const (FinObjectId 0))
        (HomologicalDegree 1)
        chainPoset
        incomingDerived
        seedValue
        @?= Right True
