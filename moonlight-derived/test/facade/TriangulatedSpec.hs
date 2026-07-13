module TriangulatedSpec
  ( -- | The semantic law tests for the triangulated derived-category surface.
    tests
  ) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Vector qualified as V
import Moonlight.Derived.Complex
  ( Derived
  , complexObjectAxes
  , derivedInjectiveComplex
  )
import Moonlight.Derived.Failure (DerivedFailure (DerivedMapSquareNotCommuting))
import Moonlight.Derived.Matrix
  ( axisLabelsExpanded
  , fromExpandedChecked
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix
  ( BlockedMat
  , DenseMat (..)
  )
import Moonlight.Derived.Morse (hypercohomologyDims, hypercohomologyVanishes)
import Moonlight.Derived.Triangulated
  ( DerivedMap
  , canonicalTruncateAtLeast
  , canonicalTruncateAtMost
  , cone
  , derivedMapComponents
  , derivedMapSource
  , derivedMapTarget
  , identityMap
  , mkDerivedMapChecked
  , mkTriangleOf
  , quasiIsoTo
  , rotateTriangle
  , shift
  , stupidTruncateAbove
  , stupidTruncateBelow
  , triG
  , triH
  , zeroMap
  , derivedObjectWindow
  )
import Moonlight.Derived.Presentation.Builder
  ( component
  , derivedObject
  , differential
  , object
  , objectsFrom
  )
import Moonlight.Derived.Site
  ( DerivedPoset
  , FinObjectId (..)
  , mkDerivedPosetFromOrderEdges
  )
import Data.Foldable (traverse_)
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
    "Triangulated"
    [ testCase "cone of identity is acyclic and detects a quasi-isomorphism" $ do
        posetValue <- singletonPoset
        objectValue <- concentratedAt posetValue 0 (FinObjectId 0) 1
        coneValue <- expectRight (cone (identityMap objectValue))
        hypercohomologyVanishes coneValue @?= Right True
        quasiIsoTo (identityMap objectValue) @?= Right True

    , testCase "cone of zero map has degreewise shifted-source plus target dimensions" $ do
        posetValue <- singletonPoset
        sourceValue <- concentratedAt posetValue 0 (FinObjectId 0) 2
        targetValue <- concentratedAt posetValue 1 (FinObjectId 0) 3
        coneValue <- expectRight (cone (zeroMap sourceValue targetValue))
        coneDimensions <- expectRight (hypercohomologyDims coneValue)
        shiftedDimensions <- expectRight (hypercohomologyDims (shift 1 sourceValue))
        targetDimensions <- expectRight (hypercohomologyDims targetValue)
        nonzeroDimensions coneDimensions @?= nonzeroDimensions (IM.unionWith (+) shiftedDimensions targetDimensions)

    , testCase "Euler characteristic is additive across a nonzero non-isomorphism cone" $ do
        posetValue <- chainPoset
        sourceValue <- concentratedAt posetValue 0 (FinObjectId 1) 1
        targetValue <- zeroExtensionOnChain posetValue
        mapValue <- identityAtDegree sourceValue targetValue 0
        coneValue <- expectRight (cone mapValue)
        sourceDimensions <- expectRight (hypercohomologyDims sourceValue)
        targetDimensions <- expectRight (hypercohomologyDims targetValue)
        coneDimensions <- expectRight (hypercohomologyDims coneValue)
        assertBool "fixture map is nonzero" (not (IM.null (derivedMapComponents mapValue)))
        eulerCharacteristic coneDimensions @?= eulerCharacteristic targetDimensions - eulerCharacteristic sourceDimensions

    , testCase "mkDerivedMapChecked rejects a non-commuting square" $ do
        posetValue <- chainPoset
        sourceValue <- simpleChainComplex posetValue
        targetValue <- simpleChainComplex posetValue
        badComponent <- componentAt sourceValue targetValue 0 [[1]]
        mkDerivedMapChecked sourceValue targetValue (IM.singleton 0 badComponent)
          @?= Left (DerivedMapSquareNotCommuting 0)

    , testCase "mkTriangleOf revalidates connecting maps and rotation succeeds" $ do
        posetValue <- singletonPoset
        sourceValue <- concentratedAt posetValue 0 (FinObjectId 0) 1
        targetValue <- concentratedAt posetValue 0 (FinObjectId 0) 1
        let mapValue = identityMap sourceValue
        triangleValue <- expectRight (mkTriangleOf mapValue)
        mkDerivedMapChecked
          (derivedMapSource (triG triangleValue))
          (derivedMapTarget (triG triangleValue))
          (derivedMapComponents (triG triangleValue))
          @?= Right (triG triangleValue)
        mkDerivedMapChecked
          (derivedMapSource (triH triangleValue))
          (derivedMapTarget (triH triangleValue))
          (derivedMapComponents (triH triangleValue))
          @?= Right (triH triangleValue)
        _ <- expectRight (rotateTriangle triangleValue)
        pure ()

    , testCase "shift reindexes hypercohomology dimensions" $ do
        posetValue <- singletonPoset
        objectValue <- threeDegreeSingleton posetValue
        sourceDimensions <- expectRight (hypercohomologyDims objectValue)
        shiftedDimensions <- traverse (\n -> fmap ((,) n) (expectRight (hypercohomologyDims (shift n objectValue)))) [-2 .. 2]
        traverse_
          (\(n, dimensionsValue) ->
            traverse_
              (\k -> dimensionAt k dimensionsValue @?= dimensionAt (k + n) sourceDimensions)
              [-4 .. 4]
          )
          shiftedDimensions

    , testCase "stupid truncations cut the expected cohomological windows" $ do
        posetValue <- singletonPoset
        objectValue <- threeDegreeSingleton posetValue
        sourceDimensions <- expectRight (hypercohomologyDims objectValue)
        belowDimensions <- expectRight (hypercohomologyDims (stupidTruncateBelow 0 objectValue))
        aboveDimensions <- expectRight (hypercohomologyDims (stupidTruncateAbove 1 objectValue))
        traverse_ (\k -> dimensionAt k belowDimensions @?= dimensionAt k sourceDimensions) [-1, 0]
        traverse_ (\k -> dimensionAt k belowDimensions @?= 0) [1, 2]
        traverse_ (\k -> dimensionAt k aboveDimensions @?= 0) [-2, -1, 0]
        traverse_ (\k -> dimensionAt k aboveDimensions @?= dimensionAt k sourceDimensions) [1]

    , testCase "canonical truncations preserve and vanish on the singleton half-windows" $ do
        posetValue <- singletonPoset
        objectValue <- threeDegreeSingleton posetValue
        sourceDimensions <- expectRight (hypercohomologyDims objectValue)
        atMostValue <- expectRight (canonicalTruncateAtMost 0 objectValue)
        atLeastValue <- expectRight (canonicalTruncateAtLeast 1 objectValue)
        atMostDimensions <- expectRight (hypercohomologyDims atMostValue)
        atLeastDimensions <- expectRight (hypercohomologyDims atLeastValue)
        traverse_ (\k -> dimensionAt k atMostDimensions @?= dimensionAt k sourceDimensions) [-1, 0]
        traverse_ (\k -> dimensionAt k atMostDimensions @?= 0) [1, 2]
        traverse_ (\k -> dimensionAt k atLeastDimensions @?= 0) [-2, -1, 0]
        traverse_ (\k -> dimensionAt k atLeastDimensions @?= dimensionAt k sourceDimensions) [1]
    ]

singletonPoset :: IO DerivedPoset
singletonPoset =
  expectRight (mkDerivedPosetFromOrderEdges [FinObjectId 0] [])

chainPoset :: IO DerivedPoset
chainPoset =
  expectRight (mkDerivedPosetFromOrderEdges [FinObjectId 0, FinObjectId 1] [(FinObjectId 0, FinObjectId 1)])

concentratedAt :: DerivedPoset -> Int -> FinObjectId -> Int -> IO (Derived GF2)
concentratedAt posetValue startDegree nodeValue multiplicityValue =
  expectRight
    ( derivedObject posetValue $ do
        (topObject, _) <- object startDegree (replicate multiplicityValue nodeValue)
        (endObject, _) <- object (startDegree + 1) []
        differential topObject endObject []
    )

simpleChainComplex :: DerivedPoset -> IO (Derived GF2)
simpleChainComplex posetValue =
  expectRight
    ( derivedObject posetValue $ do
        (x0, [sourceSummand]) <- object 0 [FinObjectId 1]
        (x1, [targetSummand]) <- object 1 [FinObjectId 0]
        differential x0 x1 [component sourceSummand targetSummand 1]
    )

zeroExtensionOnChain :: DerivedPoset -> IO (Derived GF2)
zeroExtensionOnChain posetValue =
  expectRight
    ( derivedObject posetValue $ do
        (x0, _) <- object 0 [FinObjectId 1]
        (x1, _) <- object 1 [FinObjectId 0]
        differential x0 x1 []
    )

threeDegreeSingleton :: DerivedPoset -> IO (Derived GF2)
threeDegreeSingleton posetValue =
  expectRight
    ( derivedObject posetValue $ do
        [(bottomObject, _), (middleObject, _), (topObject, _)] <-
          objectsFrom (-1) [[FinObjectId 0], [FinObjectId 0], [FinObjectId 0]]
        differential bottomObject middleObject []
        differential middleObject topObject []
    )

identityAtDegree :: Derived GF2 -> Derived GF2 -> Int -> IO (DerivedMap GF2)
identityAtDegree sourceValue targetValue degreeValue = do
  componentValue <- componentAt sourceValue targetValue degreeValue [[1]]
  expectRight (mkDerivedMapChecked sourceValue targetValue (IM.singleton degreeValue componentValue))

componentAt :: Derived GF2 -> Derived GF2 -> Int -> [[GF2]] -> IO (BlockedMat GF2)
componentAt sourceValue targetValue degreeValue rowsValue =
  expectRight
    ( fromExpandedChecked
        (axisLabelsAtDegree targetValue degreeValue)
        (axisLabelsAtDegree sourceValue degreeValue)
        (DenseMat (length rowsValue) (rowWidth rowsValue) (V.fromList (fmap V.fromList rowsValue)))
    )

axisLabelsAtDegree :: Derived GF2 -> Int -> V.Vector FinObjectId
axisLabelsAtDegree derivedValue degreeValue =
  case drop (degreeValue - startDegree) (complexObjectAxes (derivedInjectiveComplex derivedValue)) of
    axisValue : _ | degreeValue >= startDegree -> axisLabelsExpanded axisValue
    _ -> V.empty
  where
    (startDegree, _) = derivedObjectWindow derivedValue

rowWidth :: [[a]] -> Int
rowWidth rowsValue =
  case rowsValue of
    firstRow : _ -> length firstRow
    [] -> 0

nonzeroDimensions :: IntMap Int -> IntMap Int
nonzeroDimensions =
  IM.filter (/= 0)

dimensionAt :: Int -> IntMap Int -> Int
dimensionAt degreeValue =
  IM.findWithDefault 0 degreeValue

eulerCharacteristic :: IntMap Int -> Int
eulerCharacteristic =
  IM.foldrWithKey (\degreeValue dimensionValue acc -> signed degreeValue dimensionValue + acc) 0
  where
    signed degreeValue dimensionValue =
      if even degreeValue
        then dimensionValue
        else negate dimensionValue

expectRight :: Show err => Either err a -> IO a
expectRight =
  either (assertFailure . show) pure
