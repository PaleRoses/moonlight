module TensorSpec
  ( tests
  ) where

import Data.Foldable (traverse_)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Vector qualified as V
import Moonlight.Core (MoonlightError)
import Moonlight.Derived.Pure.Functor.ClosedSupport (closedSupportResolution, mkClosedSupport)
import Moonlight.Derived.Pure.Functor.ProperPullback
  ( prepareProperPullback
  , properPullback
  )
import Moonlight.Derived.Pure.Functor.Tensor
  ( internalHom
  , tensorProduct
  , tensorProductPresentation
  )
import Moonlight.Derived.Pure.Morse.Hypercohomology
  ( hypercohomologyDims
  )
import Moonlight.Derived.Pure.Site.InjectiveComplex
  ( Derived (..)
  , InjectiveComplex (..)
  , complexObjectAxes
  , composesToZero
  , getDerived
  , isMinimal
  , mkNormalizedDerived
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix
  ( BlockedMat
  , DenseMat (..)
  , GroupedAxis
  , axisMultiplicity
  , fromLabels
  , gaOrder
  , setBlock
  , zeroBlocked
  )
import Moonlight.Derived.Pure.Site.Microsupport (mkLocalClosed)
import Moonlight.Derived.Pure.Site.Poset
  ( FinObjectId (..)
  , DerivedPoset (..)
  , closureOfValidated
  , mkDerivedPosetFromCovers
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
    "Tensor"
    [ testCase "principal injectives tensor by principal support intersection" $ do
        posetValue <- expectRight (mkDerivedPosetFromCovers [FinObjectId 0, FinObjectId 1] [(FinObjectId 0, FinObjectId 1)])
        leftValue <- injectiveConcentratedAt posetValue (FinObjectId 0) 1
        rightValue <- injectiveConcentratedAt posetValue (FinObjectId 1) 1
        tensorValue <- expectRight (tensorProduct leftValue rightValue)
        firstAxisLabels tensorValue @?= [FinObjectId 0]
        firstAxisMultiplicity (FinObjectId 0) tensorValue @?= 1

    , testCase "tensor multiplicities multiply on a principal support" $ do
        posetValue <- expectRight (mkDerivedPosetFromCovers [FinObjectId 0] [])
        leftValue <- injectiveConcentratedAt posetValue (FinObjectId 0) 2
        rightValue <- injectiveConcentratedAt posetValue (FinObjectId 0) 3
        tensorValue <- expectRight (tensorProduct leftValue rightValue)
        firstAxisLabels tensorValue @?= [FinObjectId 0]
        firstAxisMultiplicity (FinObjectId 0) tensorValue @?= 6

    , testCase "tensor presentation preserves repeated-label multiplicity directly" $ do
        posetValue <- expectRight (mkDerivedPosetFromCovers [FinObjectId 0] [])
        leftValue <- injectiveConcentratedAt posetValue (FinObjectId 0) 2
        rightValue <- injectiveConcentratedAt posetValue (FinObjectId 0) 3
        tensorPresentation <- expectRight (tensorProductPresentation leftValue rightValue)
        case complexObjectAxes tensorPresentation of
          firstAxis : _ ->
            axisMultiplicity firstAxis (FinObjectId 0) @?= 6
          axesValue ->
            assertFailure ("expected a tensor presentation object axis, got " <> show (length axesValue))

    , testCase "tensor output degree is the sum of the input start degrees" $ do
        posetValue <- expectRight (mkDerivedPosetFromCovers [FinObjectId 0] [])
        leftValue <- injectiveConcentratedAtWithStart posetValue 2 (FinObjectId 0) 1
        rightValue <- injectiveConcentratedAtWithStart posetValue 3 (FinObjectId 0) 1
        tensorValue <- expectRight (tensorProduct leftValue rightValue)
        icStart (getDerived tensorValue) @?= 5

    , testCase "tensoring one-step complexes preserves total-complex composition" $ do
        posetValue <- expectRight (mkDerivedPosetFromCovers [FinObjectId 0, FinObjectId 1] [(FinObjectId 0, FinObjectId 1)])
        leftValue <- simpleChainComplex posetValue
        rightValue <- simpleChainComplex posetValue
        tensorValue <- expectRight (tensorProduct leftValue rightValue)
        let axes = complexObjectAxes (getDerived tensorValue)
        case axes of
          [firstAxis, lastAxis] -> do
            gaOrder firstAxis @?= V.fromList [FinObjectId 1]
            gaOrder lastAxis @?= V.fromList [FinObjectId 0]
          _ ->
            assertFailure ("expected minimization to leave two object axes, got " <> show (length axes))
        assertBool "tensor total complex should still satisfy d^2 = 0" (composesToZero (getDerived tensorValue))

    , testCase "principal chain tensor presentation is already minimal and composable" $ do
        posetValue <- expectRight (mkDerivedPosetFromCovers [FinObjectId 0, FinObjectId 1] [(FinObjectId 0, FinObjectId 1)])
        leftValue <- simpleChainComplex posetValue
        rightValue <- injectiveConcentratedAt posetValue (FinObjectId 1) 2
        tensorPresentation <- expectRight (tensorProductPresentation leftValue rightValue)
        assertBool "presentation differentials compose" (composesToZero tensorPresentation)
        assertBool "principal supports should not require generic peeling" (isMinimal tensorPresentation)

    , testCase "non-principal support intersections agree with the canonical closed-support object" $ do
        posetValue <- nonPrincipalTensorPoset
        leftValue <- injectiveConcentratedAt posetValue (FinObjectId 3) 1
        rightValue <- injectiveConcentratedAt posetValue (FinObjectId 4) 1
        leftSupport <- expectRight (closureOfValidated posetValue (IS.singleton 3))
        rightSupport <- expectRight (closureOfValidated posetValue (IS.singleton 4))
        let expectedSupport = IS.intersection leftSupport rightSupport
        tensorValue <- expectRight (tensorProduct leftValue rightValue)
        checkedSupport <- expectRight (mkClosedSupport posetValue expectedSupport)
        expectedValue <-
          expectRight
            (closedSupportResolution checkedSupport :: Either MoonlightError (Derived GF2))
        assertBool
          "tensor result must compose"
          (composesToZero (getDerived tensorValue))

        assertBool
          "tensor result must be minimal"
          (isMinimal (getDerived tensorValue))

        assertSameStalkCohomology
          posetValue
          expectedValue
          tensorValue

    , testCase "internalHom is surfaced through Verdier duality over GF2" $ do
        posetValue <- expectRight (mkDerivedPosetFromCovers [FinObjectId 0] [])
        sourceValue <- injectiveConcentratedAt posetValue (FinObjectId 0) 1
        targetValue <- injectiveConcentratedAt posetValue (FinObjectId 0) 1
        homValue <- expectRight (internalHom sourceValue targetValue)
        firstAxisLabels homValue @?= [FinObjectId 0]
        firstAxisMultiplicity (FinObjectId 0) homValue @?= 1
    ]

assertSameStalkCohomology ::
  DerivedPoset ->
  Derived GF2 ->
  Derived GF2 ->
  IO ()
assertSameStalkCohomology
  posetValue
  expectedValue
  actualValue =
    traverse_
      compareAtNode
      (V.toList (derivedPosetNodes posetValue))
  where
    compareAtNode (FinObjectId nodeKey) = do
      supportValue <-
        expectRight (mkLocalClosed posetValue (IS.singleton nodeKey))
      expectedPullback <-
        expectRight (prepareProperPullback supportValue expectedValue)
      expectedRestricted <-
        pure (properPullback expectedPullback)
      expectedDimensions <-
        expectRight (hypercohomologyDims expectedRestricted)

      actualPullback <-
        expectRight (prepareProperPullback supportValue actualValue)
      actualRestricted <-
        pure (properPullback actualPullback)
      actualDimensions <-
        expectRight (hypercohomologyDims actualRestricted)

      IM.filter (/= 0) actualDimensions
        @?= IM.filter (/= 0) expectedDimensions

nonPrincipalTensorPoset :: IO DerivedPoset
nonPrincipalTensorPoset =
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

injectiveConcentratedAt ::
  DerivedPoset ->
  FinObjectId ->
  Int ->
  IO (Derived GF2)
injectiveConcentratedAt posetValue nodeValue multiplicityValue =
  injectiveConcentratedAtWithStart posetValue 0 nodeValue multiplicityValue

injectiveConcentratedAtWithStart ::
  DerivedPoset ->
  Int ->
  FinObjectId ->
  Int ->
  IO (Derived GF2)
injectiveConcentratedAtWithStart posetValue startDegree nodeValue multiplicityValue =
  expectRight
    ( mkNormalizedDerived
        posetValue
        InjectiveComplex
          { icStart = startDegree
          , icDiffs =
              V.singleton
                ( zeroBlocked
                    (fromLabels V.empty)
                    (fromLabels (V.replicate multiplicityValue nodeValue))
                    :: BlockedMat GF2
                )
          }
    )

simpleChainComplex :: DerivedPoset -> IO (Derived GF2)
simpleChainComplex posetValue =
  let sourceAxis :: GroupedAxis
      sourceAxis = fromLabels (V.singleton (FinObjectId 1))
      targetAxis :: GroupedAxis
      targetAxis = fromLabels (V.singleton (FinObjectId 0))
      block :: DenseMat GF2
      block = DenseMat 1 1 (V.singleton (V.singleton 1))
      differential :: BlockedMat GF2
      differential = setBlock (FinObjectId 0) (FinObjectId 1) block (zeroBlocked targetAxis sourceAxis)
   in expectRight
        ( mkNormalizedDerived
            posetValue
            InjectiveComplex
              { icStart = 0
              , icDiffs = V.singleton differential
              }
        )

firstAxisLabels :: Derived GF2 -> [FinObjectId]
firstAxisLabels derivedValue =
  case complexObjectAxes (getDerived derivedValue) of
    [] -> []
    firstAxis : _ -> V.toList (gaOrder firstAxis)

firstAxisMultiplicity :: FinObjectId -> Derived GF2 -> Int
firstAxisMultiplicity nodeValue derivedValue =
  case complexObjectAxes (getDerived derivedValue) of
    [] -> 0
    firstAxis : _ -> axisMultiplicity firstAxis nodeValue

expectRight :: Show err => Either err a -> IO a
expectRight =
  either (assertFailure . show) pure
