module InjectiveComplexSpec (tests) where

import Data.List (isInfixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)
import qualified Data.Vector as V
import Moonlight.Core (MoonlightError (..))
import Moonlight.Derived.Pure.Failure (DerivedFailure (..))
import Moonlight.Derived.Pure.Site.Poset (FinObjectId (..), mkDerivedPosetFromOrderEdges)
import Moonlight.Derived.Pure.Site.LabeledMatrix
import Moonlight.Derived.Pure.Site.InjectiveComplex

tests :: TestTree
tests = testGroup "InjectiveComplex"
  [ testCase "zero complex is minimal" $ do
      let ax = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1])
          d0 = zeroBlocked ax ax :: BlockedMat Int
          ic = InjectiveComplex 0 (V.fromList [d0])
      assertBool "zero complex minimal" (isMinimal ic)

  , testCase "non-zero diagonal block is not minimal" $ do
      let ax = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1])
          diagBlk = DenseMat 1 1 (V.fromList [V.fromList [1]]) :: DenseMat Int
          d0 = setBlock (FinObjectId 0) (FinObjectId 0) diagBlk (zeroBlocked ax ax)
          ic = InjectiveComplex 0 (V.fromList [d0])
      assertBool "not minimal" (not (isMinimal ic))

  , testCase "off-diagonal block is minimal" $ do
      let ax = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1])
          offDiag = DenseMat 1 1 (V.fromList [V.fromList [1]]) :: DenseMat Int
          d0 = setBlock (FinObjectId 0) (FinObjectId 1) offDiag (zeroBlocked ax ax)
          ic = InjectiveComplex 0 (V.fromList [d0])
      assertBool "off-diagonal is minimal" (isMinimal ic)

  , testCase "firstNonMinimal finds the culprit" $ do
      let ax = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1])
          diagBlk = DenseMat 1 1 (V.fromList [V.fromList [1]]) :: DenseMat Int
          d0 = zeroBlocked ax ax :: BlockedMat Int
          d1 = setBlock (FinObjectId 1) (FinObjectId 1) diagBlk (zeroBlocked ax ax)
          ic = InjectiveComplex 0 (V.fromList [d0, d1])
      firstNonMinimal ic @?= Just (1, FinObjectId 1)

  , testCase "mkDerived rejects non-minimal" $ do
      siteValue <- expectRight (mkDerivedPosetFromOrderEdges [FinObjectId 0] [])
      let ax = fromLabels (V.fromList [FinObjectId 0])
          diagBlk = DenseMat 1 1 (V.fromList [V.fromList [1]]) :: DenseMat Int
          d0 = setBlock (FinObjectId 0) (FinObjectId 0) diagBlk (zeroBlocked ax ax)
          ic = InjectiveComplex 0 (V.fromList [d0])
      case mkDerived siteValue ic of
        Left (InvariantViolation msg) -> assertBool "error should mention minimality" ("minimal" `isInfixOf` msg)
        Left other -> assertFailure ("expected InvariantViolation about minimality, got: " <> show other)
        Right _ -> assertFailure "should reject non-minimal"

  , testCase "mkDerived rejects empty complexes" $ do
      siteValue <- expectRight (mkDerivedPosetFromOrderEdges [FinObjectId 0] [])
      case mkDerived siteValue (InjectiveComplex 0 V.empty :: InjectiveComplex Int) of
        Left (InvariantViolation msg) -> assertBool "error should mention emptiness" ("empty" `isInfixOf` msg)
        Left other -> assertFailure ("expected InvariantViolation about emptiness, got: " <> show other)
        Right _ -> assertFailure "should reject empty complex"

  , testCase "mkDerived rejects consecutive differentials that do not compose to zero" $ do
      siteValue <- expectRight (mkDerivedPosetFromOrderEdges [FinObjectId 0, FinObjectId 1] [])
      let ax = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1])
          blk = DenseMat 1 1 (V.fromList [V.fromList [1]]) :: DenseMat Int
          d0 = setBlock (FinObjectId 0) (FinObjectId 1) blk (zeroBlocked ax ax)
          d1 = setBlock (FinObjectId 1) (FinObjectId 0) blk (zeroBlocked ax ax)
          ic = InjectiveComplex 0 (V.fromList [d0, d1])
      case mkDerived siteValue ic of
        Left (InvariantViolation msg) -> assertBool "error should mention compose to zero" ("compose to zero" `isInfixOf` msg)
        Left other -> assertFailure ("expected InvariantViolation about chain law, got: " <> show other)
        Right _ -> assertFailure "should reject broken chain law"

  , testCase "composable phase rejects a broken chain at its single structural gate" $ do
      let ax = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1])
          blk = DenseMat 1 1 (V.fromList [V.fromList [1]]) :: DenseMat Int
          d0 = setBlock (FinObjectId 0) (FinObjectId 1) blk (zeroBlocked ax ax)
          d1 = setBlock (FinObjectId 1) (FinObjectId 0) blk (zeroBlocked ax ax)
      mkComposableInjectiveComplex 0 (V.fromList [d0, d1])
        @?= Left DerivedComplexNonzeroAdjacentComposition

  , testCase "final seal retains the order law without rechecking composability" $ do
      siteValue <- expectRight (mkDerivedPosetFromOrderEdges [FinObjectId 0, FinObjectId 1] [])
      let ax = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1])
          blk = DenseMat 1 1 (V.fromList [V.fromList [1]]) :: DenseMat Int
          orderCrossing = setBlock (FinObjectId 0) (FinObjectId 1) blk (zeroBlocked ax ax)
      composableValue <- expectRight (mkComposableInjectiveComplex 0 (V.singleton orderCrossing))
      mkNormalizedDerivedFromComposableChecked siteValue composableValue
        @?= Left DerivedComplexRestrictionUnstable

  , testCase "mkDerived accepts minimal" $ do
      siteValue <- expectRight (mkDerivedPosetFromOrderEdges [FinObjectId 0, FinObjectId 1] [])
      let ax = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1])
          d0 = zeroBlocked ax ax :: BlockedMat Int
          ic = InjectiveComplex 0 (V.fromList [d0])
      case mkDerived siteValue ic of
        Left err -> assertFailure ("unexpected rejection: " <> show err)
        Right derived -> V.length (icDiffs (getDerived derived)) @?= 1

  , testCase "allDiagLabels finds shared labels" $ do
      let rows = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1, FinObjectId 2])
          cols = fromLabels (V.fromList [FinObjectId 1, FinObjectId 2, FinObjectId 3])
          bm = zeroBlocked rows cols :: BlockedMat Int
      allDiagLabels bm @?= [FinObjectId 1, FinObjectId 2]

  , testCase "normalizeBoundaryPresentation advances degree when stripping a leading zero object" $ do
      let sourceAxis = fromLabels (V.fromList [FinObjectId 0])
          targetAxis = fromLabels (V.fromList [FinObjectId 1])
          leadingDifferential = zeroBlocked sourceAxis emptyAxis :: BlockedMat Int
          survivingDifferential = zeroBlocked targetAxis sourceAxis :: BlockedMat Int
          normalized =
            normalizeBoundaryPresentation
              InjectiveComplex
                { icStart = 11
                , icDiffs = V.fromList [leadingDifferential, survivingDifferential]
                }
      icStart normalized @?= 12
      case V.toList (icDiffs normalized) of
        [onlyDifferential] -> do
          bmCols onlyDifferential @?= sourceAxis
          bmRows onlyDifferential @?= targetAxis
        otherDifferentials ->
          assertFailure
            ( "expected exactly one surviving differential, got "
                <> show (length otherDifferentials)
            )

  , testCase "normalizeBoundaryPresentation canonicalizes singleton target-only objects" $ do
      let axisValue = fromLabels (V.fromList [FinObjectId 7])
          targetOnlyDifferential = zeroBlocked axisValue emptyAxis :: BlockedMat Int
          normalized =
            normalizeBoundaryPresentation
              InjectiveComplex
                { icStart = 4
                , icDiffs = V.singleton targetOnlyDifferential
                }
      icStart normalized @?= 5
      case V.toList (icDiffs normalized) of
        [onlyDifferential] -> do
          bmCols onlyDifferential @?= axisValue
          bmRows onlyDifferential @?= emptyAxis
        otherDifferentials ->
          assertFailure
            ( "expected exactly one canonical singleton differential, got "
                <> show (length otherDifferentials)
            )

  , testCase "initialObjectAxis is cols of first diff" $ do
      let rows = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1])
          cols = fromLabels (V.fromList [FinObjectId 2, FinObjectId 3])
          d0 = zeroBlocked rows cols :: BlockedMat Int
          ic = InjectiveComplex 0 (V.fromList [d0])
      initialObjectAxis ic @?= Just cols
  ]

expectRight :: Show errorValue => Either errorValue value -> IO value
expectRight resultValue =
  case resultValue of
    Left errorValue -> assertFailure (show errorValue)
    Right value -> pure value
