module ClosedSupportSpec
  ( tests
  ) where

import Data.Foldable (traverse_)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List (isInfixOf, subsequences)
import Data.Set qualified as Set
import Data.Vector qualified as V
import Moonlight.Core (MoonlightError (..))
import Moonlight.Derived.Pure.Dimension
  ( gradedKernelImageDims
  )
import Moonlight.Derived.Pure.Functor.ClosedSupport (closedSupportResolution, mkClosedSupport)
import Moonlight.Derived.Pure.Functor.ClosedSupport.Resolution
  ( ClosedSupportResolutionCounters (..)
  , ClosedSupportResolutionReport (..)
  , closedSupportResolutionWithCounters
  )
import Moonlight.Derived.Pure.Functor.ProperPullback
  ( prepareProperPullback
  , properPullback
  )
import Moonlight.Derived.Pure.Morse.Hypercohomology
  ( hypercohomologyDims
  , hypercohomologyVanishes
  )
import Moonlight.Derived.Pure.LinAlg.Interpreter
  ( rankDense
  )
import Moonlight.Derived.Pure.Site.InjectiveComplex
  ( Derived
  , InjectiveComplex (..)
  , complexObjectAxes
  , composesToZero
  , derivedInjectiveComplex
  , isMinimal
  , mkNormalizedDerived
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix
  ( BlockedMat (..)
  , DenseMat (..)
  , axisSize
  , fromLabels
  , restrictAxis
  , starView
  , zeroBlocked
  )
import Moonlight.Derived.Pure.Site.Microsupport (mkLocalClosed)
import Moonlight.Derived.Pure.Site.Poset
  ( FinObjectId (..)
  , DerivedPoset (..)
  , mkDerivedPosetFromCovers
  , star
  )
import Moonlight.LinAlg (GF2)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool
  , assertFailure
  , testCase
  , (@?=)
  )

tests :: TestTree
tests =
  testGroup
    "ClosedSupport"
    [ testCase "principal closed support normalizes to the corresponding injective" $ do
        posetValue <- expectRight (mkDerivedPosetFromCovers [FinObjectId 0, FinObjectId 1] [(FinObjectId 0, FinObjectId 1)])
        expectedValue <- injectiveConcentratedAt posetValue (FinObjectId 1) 1
        support <- expectRight (mkClosedSupport posetValue (IS.fromList [0, 1]))
        supportValue <-
          expectRight
            (closedSupportResolution support :: Either MoonlightError (Derived GF2))
        supportValue @?= expectedValue

    , testCase "closedSupportResolution rejects supports that are not closed under specialization" $ do
        posetValue <- expectRight (mkDerivedPosetFromCovers [FinObjectId 0, FinObjectId 1] [(FinObjectId 0, FinObjectId 1)])
        case mkClosedSupport posetValue (IS.singleton 1) >>= closedSupportResolution :: Either MoonlightError (Derived GF2) of
          Left (InvariantViolation messageValue) ->
            assertBool
              "error should identify closure failure"
              ("not closed under specialization" `isInfixOf` messageValue)
          Left otherError ->
            assertFailure ("expected InvariantViolation, got: " <> show otherError)
          Right _ ->
            assertFailure "expected closedSupportResolution to reject a non-closed support"

    , testCase "non-principal closed supports are resolved canonically and vanish off support" $ do
        posetValue <- nonPrincipalSupportPoset
        let supportNodeSet = IS.fromList [0, 1, 2]
        support <- expectRight (mkClosedSupport posetValue supportNodeSet)
        supportValue <-
          expectRight
            (closedSupportResolution support :: Either MoonlightError (Derived GF2))
        dims <- expectRight (hypercohomologyDims supportValue)
        dims @?= IM.fromList [(0, 1), (1, 0)]
        traverse_
          (assertSupportDetected posetValue supportValue)
          [0, 1, 2]
        traverse_
          (assertSupportAbsent posetValue supportValue)
          [3, 4]
        assertClosedConstantStalks posetValue supportNodeSet supportValue

    , testCase "path-triangle support is emitted in its 14-to-13 minimal form" $ do
        posetValue <-
          expectRight (pathTrianglePoset 16)

        let fullSupport =
              IS.fromList
                [ unFinObjectId nodeValue
                | nodeValue <- V.toList (derivedPosetNodes posetValue)
                ]
        checkedFullSupport <- expectRight (mkClosedSupport posetValue fullSupport)

        supportReport <-
          expectRight
            ( closedSupportResolutionWithCounters
                checkedFullSupport
                :: Either MoonlightError (ClosedSupportResolutionReport GF2)
            )

        let supportValue =
              csrrDerived supportReport

            supportCounters =
              csrrCounters supportReport

        let injectiveComplex =
              derivedInjectiveComplex supportValue

            objectDimensions =
              fmap
                axisSize
                (complexObjectAxes injectiveComplex)

            differentialNonZeros =
              sum
                ( fmap
                    blockedNonZeroCount
                    (V.toList (icDiffs injectiveComplex))
                )

        objectDimensions @?= [14, 13]
        V.length (icDiffs injectiveComplex) @?= 1
        differentialNonZeros @?= 26
        csrcObjectGeneratorCounts supportCounters @?= V.fromList [14, 13]
        csrcTotalGenerators supportCounters @?= 27
        csrcStoredDifferentialNonZeros supportCounters @?= 26
        csrcAcceptedRows supportCounters @?= 13

        assertBool
          "direct closed-support resolution must compose"
          (composesToZero injectiveComplex)

        assertBool
          "direct closed-support resolution must be minimal"
          (isMinimal injectiveComplex)

        assertClosedConstantStalks posetValue fullSupport supportValue
    ]

nonPrincipalSupportPoset :: IO DerivedPoset
nonPrincipalSupportPoset =
  expectRight
    ( mkDerivedPosetFromCovers
        [FinObjectId 0, FinObjectId 1, FinObjectId 2, FinObjectId 3, FinObjectId 4]
        [ (FinObjectId 0, FinObjectId 1)
        , (FinObjectId 0, FinObjectId 2)
        , (FinObjectId 1, FinObjectId 3)
        , (FinObjectId 2, FinObjectId 3)
        , (FinObjectId 1, FinObjectId 4)
        , (FinObjectId 2, FinObjectId 4)
        ]
    )

assertSupportDetected :: DerivedPoset -> Derived GF2 -> Int -> IO ()
assertSupportDetected posetValue derivedValue nodeKey = do
  supportValue <- expectRight (mkLocalClosed posetValue (IS.singleton nodeKey))
  preparedPullback <- expectRight (prepareProperPullback supportValue derivedValue)
  let restrictedValue = properPullback preparedPullback
  vanishes <- expectRight (hypercohomologyVanishes restrictedValue)
  assertBool ("support should remain detectable at node " <> show nodeKey) (not vanishes)

assertSupportAbsent :: DerivedPoset -> Derived GF2 -> Int -> IO ()
assertSupportAbsent posetValue derivedValue nodeKey = do
  supportValue <- expectRight (mkLocalClosed posetValue (IS.singleton nodeKey))
  preparedPullback <- expectRight (prepareProperPullback supportValue derivedValue)
  let restrictedValue = properPullback preparedPullback
  vanishes <- expectRight (hypercohomologyVanishes restrictedValue)
  assertBool ("support should vanish outside the closed support at node " <> show nodeKey) vanishes

assertClosedConstantStalks ::
  DerivedPoset ->
  IS.IntSet ->
  Derived GF2 ->
  IO ()
assertClosedConstantStalks posetValue supportNodeSet derivedValue =
  traverse_
    assertAtNode
    (V.toList (derivedPosetNodes posetValue))
  where
    injectiveComplex =
      derivedInjectiveComplex derivedValue

    objectAxes =
      complexObjectAxes injectiveComplex

    differentialValues =
      V.toList (icDiffs injectiveComplex)

    assertAtNode nodeValue@(FinObjectId nodeKey) = do
      localRanks <-
        expectRight
          ( traverse
              (rankDense . starView posetValue nodeValue)
              differentialValues
          )

      let localObjectDimensions =
            fmap
              ( axisSize
                  . restrictAxis (star posetValue nodeValue)
              )
              objectAxes

          localCohomology =
            gradedKernelImageDims
              (icStart injectiveComplex)
              localObjectDimensions
              localRanks

          expectedDegreeZero =
            if IS.member nodeKey supportNodeSet
              then 1
              else 0

      IM.findWithDefault 0 0 localCohomology
        @?= expectedDegreeZero

      assertBool
        ("positive-degree stalk cohomology must vanish at " <> show nodeValue)
        ( all
            ( \(degreeValue, dimensionValue) ->
                degreeValue == 0 || dimensionValue == 0
            )
            (IM.toList localCohomology)
        )

pathTrianglePoset :: Int -> Either MoonlightError DerivedPoset
pathTrianglePoset vertexCount =
  mkDerivedPosetFromCovers
    (fmap simplexNode facesValue)
    (simplicialCovers facesValue)
  where
    facesValue =
      facesFromFacets
        [ [vertexKey, vertexKey + 1, vertexKey + 2]
        | vertexKey <- triangleStarts
        ]

    triangleStarts
      | vertexCount < 3 =
          []
      | otherwise =
          [0 .. vertexCount - 3]

facesFromFacets :: [[Int]] -> [[Int]]
facesFromFacets =
  Set.toAscList
    . Set.fromList
    . concatMap
      (filter (not . null) . subsequences)

simplicialCovers :: [[Int]] -> [(FinObjectId, FinObjectId)]
simplicialCovers facesValue =
  [ (simplexNode faceValue, simplexNode cofaceValue)
  | faceValue <- facesValue
  , cofaceValue <- facesValue
  , length cofaceValue == length faceValue + 1
  , all (`elem` cofaceValue) faceValue
  ]

simplexNode :: [Int] -> FinObjectId
simplexNode =
  FinObjectId . sum . fmap (2 ^)

blockedNonZeroCount :: BlockedMat GF2 -> Int
blockedNonZeroCount BlockedMat {bmBlocks} =
  IM.foldl'
    ( \rowTotal rowBlocks ->
        rowTotal
          + IM.foldl'
            ( \blockTotal denseValue ->
                blockTotal + denseNonZeroCount denseValue
            )
            0
            rowBlocks
    )
    0
    bmBlocks

denseNonZeroCount :: DenseMat GF2 -> Int
denseNonZeroCount DenseMat {dmData} =
  V.sum
    (V.map (V.length . V.filter (/= 0)) dmData)

injectiveConcentratedAt ::
  DerivedPoset ->
  FinObjectId ->
  Int ->
  IO (Derived GF2)
injectiveConcentratedAt posetValue nodeValue multiplicityValue =
  expectRight
    ( mkNormalizedDerived
        posetValue
        InjectiveComplex
          { icStart = 0
          , icDiffs =
              V.singleton
                ( zeroBlocked
                    (fromLabels V.empty)
                    (fromLabels (V.replicate multiplicityValue nodeValue))
                    :: BlockedMat GF2
                )
          }
    )

expectRight :: Show err => Either err a -> IO a
expectRight =
  either (assertFailure . show) pure
