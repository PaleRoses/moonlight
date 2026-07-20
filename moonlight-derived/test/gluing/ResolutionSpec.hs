module ResolutionSpec (tests) where

import Data.IntMap.Strict qualified as IntMap
import qualified Data.IntSet as IS
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import qualified Data.Vector as V
import Moonlight.Core (MoonlightError (..))
import Moonlight.Derived.Pure.LinAlg.Interpreter
  ( fieldRankBackend
  , inverseDense
  , kernelDense
  , leftKernelDense
  , rankDense
  )
import Moonlight.Derived.Pure.LinAlg.Rank
  ( DenseMatStableDigest
  , RankBackend (..)
  , denseMatStableDigest
  , precomputeStableRankCache
  , rankDenseWith
  , stableDigestRankBackend
  )
import Moonlight.Derived.Pure.Dimension (gradedKernelImageDims)
import Moonlight.Derived.Pure.Failure (DerivedFailure (..))
import Moonlight.Derived.Pure.Functor.Pullback (pullback)
import Moonlight.Derived.Pure.Functor.QuillenA
  ( QuillenACertificate (..)
  , quillenAMaximumCertificate
  )
import Moonlight.Derived.Pure.Functor.VerdierDual (verdierDualComplex)
import Moonlight.Derived.Pure.Morse.Hypercohomology
  ( hypercohomologyDimsWith
  , hypercohomologyReducedVanishes
  , hypercohomologyVanishes
  )
import Moonlight.Derived.Pure.Morse.Support (fiberSubsets)
import Moonlight.Derived.Pure.Pruning.LaplacianGate (pruningGapOfSymmetricDenseMat)
import Moonlight.Derived.Pure.Site.Gorenstein (isGorensteinStar)
import Moonlight.Derived.Pure.Site.InjectiveComplex
import Moonlight.Derived.Pure.Site.LabeledMatrix
import Moonlight.Derived.Pure.Site.Microsupport
  ( Criticality (..)
  , localClosedNodes
  , mkLocalClosed
  )
import Moonlight.Derived.Pure.Site.Poset (FinObjectId (..), DerivedPoset (..), mkDerivedPosetFromCovers)
import Moonlight.Derived.Pure.Gluing.MakeExact (makeExact)
import Moonlight.Derived.Pure.Gluing.Resolution (resolutionStep)
import Moonlight.Derived.Pure.Functor.ProperPullback
  ( prepareProperPullback
  , properPullback
  )
import Moonlight.Derived.Pure.Pipeline
  ( MicrosupportResult (..)
  , computeMicrosupport
  , prepareMicrosupport
  )
import Moonlight.LinAlg (GF2)
import Moonlight.Derived.Test.Fixture (mkTestFunctor)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

expectPoset :: [FinObjectId] -> [(FinObjectId, FinObjectId)] -> DerivedPoset
expectPoset ns cs = either (error . show) id (mkDerivedPosetFromCovers ns cs)

tests :: TestTree
tests = testGroup "Resolution"
  [ linAlgBridgeTests
  , dimensionTests
  , pruningTests
  , makeExactTests
  , resolutionTests
  , microsupportTests
  , quillenTests
  , verdierTests
  ]

expectRight :: Show err => Either err a -> IO a
expectRight =
  either (assertFailure . show) pure

assertLeftSatisfies :: (Show err, Show value) => (err -> Bool) -> Either err value -> Assertion
assertLeftSatisfies predicate eitherValue =
  case eitherValue of
    Left err -> assertBool ("error did not satisfy predicate: " <> show err) (predicate err)
    Right value -> assertFailure ("expected Left, received Right: " <> show value)

stressMatrix :: Int -> Int -> Int -> DenseMat Double
stressMatrix seed rowCount columnCount =
  DenseMat rowCount columnCount
    ( V.generate rowCount
        ( \rowIndex ->
            V.generate columnCount
              ( \columnIndex ->
                  fromIntegral
                    ( ((seed + 3) * (rowIndex + 1) + (seed * 5 + 7) * (columnIndex + 2) + if rowIndex == columnIndex then seed + 11 else 0) `mod` 19
                        - 9
                    )
              )
        )
    )

stressMatrices :: [DenseMat Double]
stressMatrices =
  fmap
    (\seed -> stressMatrix seed (8 + seed `mod` 5) (8 + (seed * 3) `mod` 5))
    [0 .. 47]

stressResolutionPoset :: DerivedPoset
stressResolutionPoset =
  expectPoset
    [FinObjectId 0, FinObjectId 1, FinObjectId 2, FinObjectId 3, FinObjectId 4]
    [(FinObjectId 0, FinObjectId 1), (FinObjectId 1, FinObjectId 2), (FinObjectId 2, FinObjectId 3), (FinObjectId 3, FinObjectId 4)]

stressDifferential :: BlockedMat GF2
stressDifferential =
  let axis = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1, FinObjectId 2, FinObjectId 3, FinObjectId 4])
      block = DenseMat 1 1 (V.fromList [V.fromList [1]])
   in foldr
        (\(rowNode, columnNode) accumulated -> setBlock rowNode columnNode block accumulated)
        (zeroBlocked axis axis)
        [(FinObjectId 0, FinObjectId 1), (FinObjectId 1, FinObjectId 2), (FinObjectId 2, FinObjectId 3), (FinObjectId 3, FinObjectId 4)]

invalidDenseMat :: DenseMat Double
invalidDenseMat =
  DenseMat 2 2
    ( V.fromList
        [ V.fromList [0 / 0, 1]
        , V.fromList [0, 1]
        ]
    )

invalidBlockMat :: DenseMat Double
invalidBlockMat =
  DenseMat 1 1 (V.fromList [V.fromList [0 / 0]])

linAlgBridgeTests :: TestTree
linAlgBridgeTests = testGroup "LinAlgBridge"
  [ testCase "direct rank backend agrees with stable-digest cache across a deterministic matrix family" $ do
      cache <- expectRight (precomputeStableRankCache fieldRankBackend stressMatrices)
      let cachedBackend = stableDigestRankBackend cache fieldRankBackend
      directRanks <- expectRight (traverse rankDense stressMatrices)
      cachedRanks <- expectRight (traverse (rankDenseWith cachedBackend) stressMatrices)
      cachedRanks @?= directRanks

  , testCase "stable-digest cache refuses polluted buckets and falls back to the backend" $ do
      let queryMatrix = stressMatrix 91 12 12
          decoyMatrices = filter (/= queryMatrix) (fmap (\seed -> stressMatrix seed 12 12) [0 .. 31])
          bogusRank = 777
          pollutedCache :: Map.Map DenseMatStableDigest [(DenseMat Double, Int)]
          pollutedCache =
            Map.singleton
              (denseMatStableDigest queryMatrix)
              (fmap (\denseMat -> (denseMat, bogusRank)) decoyMatrices)
          cachedBackend = stableDigestRankBackend pollutedCache fieldRankBackend
      expectedRank <- expectRight (rankDense queryMatrix)
      actualRank <- expectRight (rankDenseWith cachedBackend queryMatrix)
      actualRank @?= expectedRank

  , testCase "precomputed cache survives adversarial bucket pollution when the matching matrix is buried last" $ do
      let queryMatrix = stressMatrix 137 10 10
      expectedRank <- expectRight (rankDense queryMatrix)
      let decoyMatrices = filter (/= queryMatrix) (fmap (\seed -> stressMatrix seed 10 10) [0 .. 24])
          pollutedCache :: Map.Map DenseMatStableDigest [(DenseMat Double, Int)]
          pollutedCache =
            Map.singleton
              (denseMatStableDigest queryMatrix)
              ( fmap (\denseMat -> (denseMat, 999)) decoyMatrices
                  <> [(queryMatrix, expectedRank)]
              )
          cachedBackend = stableDigestRankBackend pollutedCache fieldRankBackend
      actualRank <- expectRight (rankDenseWith cachedBackend queryMatrix)
      actualRank @?= expectedRank

  , testCase "invalid floating matrices fail through rank, kernel, and cache precomputation instead of crashing" $ do
      assertLeftSatisfies (\err -> case err of InvariantViolation msg -> "invalid field values" `isInfixOf` msg; _ -> False) (rankDense invalidDenseMat)
      assertLeftSatisfies (\err -> case err of InvariantViolation msg -> "invalid field values" `isInfixOf` msg; _ -> False) (kernelDense invalidDenseMat)
      assertLeftSatisfies (\err -> case err of InvariantViolation msg -> "invalid field values" `isInfixOf` msg; _ -> False) (precomputeStableRankCache fieldRankBackend [invalidDenseMat])

  , testCase "inverseDense inverts a full-rank GF2 matrix without fallback semantics" $ do
      let identityDense =
            DenseMat
              2
              2
              ( V.fromList
                  [ V.fromList [1, 0]
                  , V.fromList [0, 1]
                  ]
              ) :: DenseMat GF2
      inverseDense identityDense @?= Right (Just identityDense)

  , testCase "left and right kernels on a structured matrix agree on nullity" $ do
      let structuredMatrix = stressMatrix 51 9 11
      rightKernel <- expectRight (kernelDense structuredMatrix)
      leftKernel <- expectRight (leftKernelDense structuredMatrix)
      rightRank <- expectRight (rankDense structuredMatrix)
      leftRank <- expectRight (rankDense (DenseMat (dmCols structuredMatrix) (dmRows structuredMatrix) (V.generate (dmCols structuredMatrix) (\j -> V.generate (dmRows structuredMatrix) (\i -> (dmData structuredMatrix V.! i) V.! j)))))
      length rightKernel @?= dmCols structuredMatrix - rightRank
      length leftKernel @?= dmRows structuredMatrix - leftRank
  ]

dimensionTests :: TestTree
dimensionTests = testGroup "Dimension"
  [ testCase "gradedKernelImageDims computes cochain dimensions from outgoing and incoming ranks" $
      gradedKernelImageDims 0 [3, 4, 2] [2, 1]
        @?= IntMap.fromList [(0, 1), (1, 1), (2, 1)]
  , testCase "gradedKernelImageDims supports chain-style cohomology dimensions with ascending degrees" $
      gradedKernelImageDims 0 [3, 3, 1] [2, 1]
        @?= IntMap.fromList [(0, 1), (1, 0), (2, 0)]
  , testCase "hypercohomologyDimsWith delegates rank computation through the injected backend" $ do
      let axis = fromLabels (V.fromList [FinObjectId 0])
          zeroDifferential = zeroBlocked axis axis :: BlockedMat Double
          derivedValue = Derived (expectPoset [FinObjectId 0] []) (InjectiveComplex 0 (V.singleton zeroDifferential))
          backend :: RankBackend Double
          backend = RankBackend (const (Right 0))
      dims <- expectRight (hypercohomologyDimsWith backend derivedValue)
      dims @?= IntMap.fromList [(0, 1), (1, 1)]
  , testCase "hypercohomology fails loudly on invalid differentials" $ do
      let axis = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1])
          invalidDifferential = setBlock (FinObjectId 0) (FinObjectId 1) invalidBlockMat (zeroBlocked axis axis)
          derivedValue = Derived (expectPoset [FinObjectId 0, FinObjectId 1] []) (InjectiveComplex 0 (V.singleton invalidDifferential))
      assertLeftSatisfies (\err -> case err of InvariantViolation msg -> "invalid field value" `isInfixOf` msg; _ -> False) (hypercohomologyDimsWith fieldRankBackend derivedValue)

  , testCase "reduced hypercohomology does not ignore negative degrees" $ do
      let axis = fromLabels (V.singleton (FinObjectId 0))
          derivedValue =
            Derived
              (expectPoset [FinObjectId 0] [])
              (InjectiveComplex (-1) (V.singleton (zeroBlocked emptyAxis axis :: BlockedMat GF2)))
      reducedVanishes <- expectRight (hypercohomologyReducedVanishes derivedValue)
      reducedVanishes @?= False
  ]

pruningTests :: TestTree
pruningTests = testGroup "Pruning"
  [ testCase "pruning gap verdict is stable under uniform symmetric-matrix rescaling" $ do
      let positiveGapMatrix =
            DenseMat
              2
              2
              ( V.fromList
                  [ V.fromList [2.0, -1.0]
                  , V.fromList [-1.0, 2.0]
                  ]
              )
          kernelMatrix =
            DenseMat
              2
              2
              ( V.fromList
                  [ V.fromList [1.0, -1.0]
                  , V.fromList [-1.0, 1.0]
                  ]
              )
          scales = [1.0e-8, 1.0, 1.0e8]
          scaleDenseMat :: Double -> DenseMat Double -> DenseMat Double
          scaleDenseMat scaleValue DenseMat{dmRows, dmCols, dmData} =
            DenseMat dmRows dmCols (V.map (V.map (scaleValue *)) dmData)
      positiveGaps <-
        expectRight
          (traverse (\scaleValue -> pruningGapOfSymmetricDenseMat (scaleDenseMat scaleValue positiveGapMatrix)) scales)
      kernelGaps <-
        expectRight
          (traverse (\scaleValue -> pruningGapOfSymmetricDenseMat (scaleDenseMat scaleValue kernelMatrix)) scales)
      let positiveGapVerdicts = fmap (> 0.0) positiveGaps
          kernelVerdicts = fmap (== 0.0) kernelGaps
      positiveGapVerdicts @?= [True, True, True]
      kernelVerdicts @?= [True, True, True]

  , testCase "pruning gap rejects rectangular and nonsymmetric matrices" $ do
      let rectangularMatrix =
            DenseMat 2 1 (V.fromList [V.singleton 1.0, V.singleton 0.0])
          nonsymmetricMatrix =
            DenseMat 2 2 (V.fromList [V.fromList [1.0, 1.0], V.fromList [0.0, 1.0]])
      assertLeftSatisfies (const True) (pruningGapOfSymmetricDenseMat rectangularMatrix)
      assertLeftSatisfies (const True) (pruningGapOfSymmetricDenseMat nonsymmetricMatrix)
  ]

makeExactTests :: TestTree
makeExactTests = testGroup "MakeExact"
  [ testCase "makeExact on zero previous differential adds kernel rows on a nontrivial star" $ do
      let posetValue = expectPoset [FinObjectId 0, FinObjectId 1, FinObjectId 2] [(FinObjectId 0, FinObjectId 1), (FinObjectId 1, FinObjectId 2)]
          axis = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1, FinObjectId 2])
          previousDifferential = zeroBlocked axis axis :: BlockedMat GF2
          currentDifferential :: BlockedMat GF2
          currentDifferential = zeroBlocked emptyAxis axis
      result <- expectRight (makeExact posetValue (FinObjectId 1) previousDifferential currentDifferential)
      assertBool "makeExact should add rows on the middle node" (axisSize (bmRows result) > 0)

  , testCase "makeExact preserves existing content while appending only independent rows" $ do
      let posetValue = expectPoset [FinObjectId 0, FinObjectId 1] [(FinObjectId 0, FinObjectId 1)]
          axis = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1])
          block = DenseMat 1 1 (V.fromList [V.fromList [1]]) :: DenseMat GF2
          previousDifferential = setBlock (FinObjectId 0) (FinObjectId 1) block (zeroBlocked axis axis)
          currentDifferential :: BlockedMat GF2
          currentDifferential = zeroBlocked emptyAxis axis
      result <- expectRight (makeExact posetValue (FinObjectId 0) previousDifferential currentDifferential)
      assertBool "makeExact should preserve a compatible codomain" (axisSize (bmCols result) == axisSize axis)
  ]

resolutionTests :: TestTree
resolutionTests = testGroup "ResolutionStep"
  [ testCase "resolution step on a structured chain differential still satisfies d^2 = 0" $ do
      nextDifferential <- expectRight (resolutionStep stressResolutionPoset stressDifferential)
      let composed = composeBlocked nextDifferential stressDifferential
          (_, _, denseComposed) = expandBlocked composed
      assertBool "resolution must preserve the chain condition" (isZeroMat denseComposed)
  ]

microsupportTests :: TestTree
microsupportTests = testGroup "Microsupport"
  [ testCase "fiberSubsets of identity are singletons" $ do
      let posetValue = expectPoset [FinObjectId 0, FinObjectId 1, FinObjectId 2] [(FinObjectId 0, FinObjectId 1), (FinObjectId 1, FinObjectId 2)]
      identityFunctor <- expectRight (mkTestFunctor posetValue posetValue id)
      fibers <- expectRight (fiberSubsets identityFunctor)
      length fibers @?= 3

  , testCase "fiberSubsets of a constant map collapse to one fiber" $ do
      let src = expectPoset [FinObjectId 0, FinObjectId 1] [(FinObjectId 0, FinObjectId 1)]
          tgt = expectPoset [FinObjectId 0] []
      functorValue <- expectRight (mkTestFunctor src tgt (const (FinObjectId 0)))
      fibers <- expectRight (fiberSubsets functorValue)
      length fibers @?= 1
      case fibers of
        [fiberValue] -> localClosedNodes fiberValue @?= IS.fromList [0, 1]
        _ -> assertFailure "expected exactly one fiber"

  , testCase "computeMicrosupport classifies its prepared fibers without reconstructing them" $ do
      let posetValue = expectPoset [FinObjectId 0, FinObjectId 1] []
          axis = fromLabels (V.singleton (FinObjectId 0))
      derivedValue <- expectRight (mkDerivedChecked posetValue (InjectiveComplex 0 (V.singleton (zeroBlocked axis axis :: BlockedMat GF2))))
      identityFunctor <- expectRight (mkTestFunctor posetValue posetValue id)
      preparedValue <- expectRight (prepareMicrosupport identityFunctor derivedValue)
      resultValue <- expectRight (computeMicrosupport preparedValue)
      mrCriticalFibers resultValue @?= [(FinObjectId 0, Critical), (FinObjectId 1, NonCritical)]
      (mrCriticalCount resultValue, mrNoncriticalCount resultValue) @?= (1, 1)

  , testCase "properPullback restricts differentials to the selected support" $ do
      let posetValue = expectPoset [FinObjectId 0, FinObjectId 1, FinObjectId 2] [(FinObjectId 0, FinObjectId 1), (FinObjectId 1, FinObjectId 2)]
          axis = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1, FinObjectId 2])
          block = DenseMat 1 1 (V.fromList [V.fromList [1]]) :: DenseMat Int
          differential = setBlock (FinObjectId 0) (FinObjectId 1) block (setBlock (FinObjectId 1) (FinObjectId 2) block (zeroBlocked axis axis))
          injectiveComplex = InjectiveComplex 0 (V.fromList [differential])
          derivedValue = Derived posetValue injectiveComplex
      supportValue <- expectRight (mkLocalClosed posetValue (IS.fromList [0, 1]))
      preparedPullback <- expectRight (prepareProperPullback supportValue derivedValue)
      let restricted = properPullback preparedPullback
      let restrictedDifferential = V.head (icDiffs (getDerived restricted))
      axisMultiplicity (bmRows restrictedDifferential) (FinObjectId 2) @?= 0
      axisMultiplicity (bmCols restrictedDifferential) (FinObjectId 2) @?= 0
      blockAt (FinObjectId 0) (FinObjectId 1) restrictedDifferential @?= block

  , testCase "locally closed support refuses foreign nodes" $ do
      let posetValue = expectPoset [FinObjectId 0] []
      assertLeftSatisfies
        (\failureValue -> case failureValue of DerivedFunctorInvalidSupport _ -> True; _ -> False)
        (mkLocalClosed posetValue (IS.singleton 404))

  , testCase "pullback preserves the first differential on identity" $ do
      let posetValue = expectPoset [FinObjectId 0, FinObjectId 1] [(FinObjectId 0, FinObjectId 1)]
          axis = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1])
          block = DenseMat 1 1 (V.fromList [V.fromList [1]]) :: DenseMat GF2
          differential = setBlock (FinObjectId 0) (FinObjectId 1) block (zeroBlocked axis axis)
          expected = InjectiveComplex 0 (V.singleton differential)
      identityFunctor <- expectRight (mkTestFunctor posetValue posetValue id)
      pulled <- expectRight (pullback identityFunctor (Derived posetValue expected))
      getDerived pulled @?= expected

  , testCase "hypercohomology of the zero complex does not vanish" $ do
      let axis0 = fromLabels (V.fromList [FinObjectId 0])
          axis1 = fromLabels (V.fromList [FinObjectId 1])
          differential = zeroBlocked axis1 axis0 :: BlockedMat Double
          derivedValue = Derived (expectPoset [FinObjectId 0, FinObjectId 1] []) (InjectiveComplex 0 (V.fromList [differential]))
      vanishes <- expectRight (hypercohomologyVanishes derivedValue)
      assertBool "the zero complex has nontrivial H^0" (not vanishes)
  ]

quillenTests :: TestTree
quillenTests = testGroup "QuillenA"
  [ testCase "an empty lower fiber refutes the maximum certificate" $ do
      let sourcePoset = expectPoset [FinObjectId 0] []
          targetPoset = expectPoset [FinObjectId 0, FinObjectId 1] []
      functorValue <- expectRight (mkTestFunctor sourcePoset targetPoset (const (FinObjectId 0)))
      certificate <- expectRight (quillenAMaximumCertificate functorValue)
      certificate @?= QuillenARefutedByEmptyFiber (FinObjectId 1)
  , testCase "a nonempty V-shaped fiber without a maximum is inconclusive" $ do
      let sourcePoset =
            expectPoset
              [FinObjectId 0, FinObjectId 1, FinObjectId 2]
              [(FinObjectId 0, FinObjectId 1), (FinObjectId 0, FinObjectId 2)]
          targetPoset = expectPoset [FinObjectId 0] []
      functorValue <- expectRight (mkTestFunctor sourcePoset targetPoset (const (FinObjectId 0)))
      certificate <- expectRight (quillenAMaximumCertificate functorValue)
      certificate @?= QuillenAInconclusive (FinObjectId 0)
  ]

verdierTests :: TestTree
verdierTests = testGroup "Verdier"
  [ testCase "discrete two-point boundary is Gorenstein*" $ do
      let posetValue = expectPoset [FinObjectId 0, FinObjectId 1] []
      gorensteinValue <- expectRight (isGorensteinStar posetValue)
      assertBool "expected S^0 face poset to be Gorenstein*" gorensteinValue

  , testCase "singleton poset is not Gorenstein*" $ do
      let posetValue = expectPoset [FinObjectId 0] []
      gorensteinValue <- expectRight (isGorensteinStar posetValue)
      assertBool "expected singleton poset to fail the Gorenstein* test" (not gorensteinValue)

  , testCase "verdierDualComplex preserves total hypercohomology rank on a zero complex" $ do
      let posetValue = expectPoset [FinObjectId 0, FinObjectId 1] []
          axis = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1])
          zeroDifferential = zeroBlocked axis axis :: BlockedMat GF2
          derivedValue = Derived posetValue (InjectiveComplex 0 (V.singleton zeroDifferential))
          totalRank = sum . IntMap.elems
      dualValue <- expectRight (verdierDualComplex derivedValue)
      dualRank <- expectRight (hypercohomologyDimsWith (RankBackend (const (Right 0))) dualValue)
      primalRank <- expectRight (hypercohomologyDimsWith (RankBackend (const (Right 0))) derivedValue)
      totalRank dualRank @?= totalRank primalRank
  ]
