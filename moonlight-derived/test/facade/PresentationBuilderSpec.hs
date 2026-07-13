module PresentationBuilderSpec
  ( -- | Agreement of the name-binding builder with the raw specification
    -- dialect, plus one anchor per authoring fault.
    tests
  ) where

import qualified Data.Vector as V
import Moonlight.Derived.Complex (Derived, derivedPoset)
import Moonlight.Derived.Presentation.Builder
  ( DerivedBuildError (..)
  , DerivedBuilder
  , component
  , derivedObject
  , differential
  , differentialDense
  , object
  , objectsFrom
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix (DenseMat (..))
import Moonlight.Derived.Failure (DerivedFailure (..))
import Moonlight.Derived.Site
  ( DerivedPoset
  , FinObjectId (..)
  , mkDerivedPosetFromOrderEdges
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
    "PresentationBuilder"
    [ testCase "dense escape agrees with the component form" $ do
        posetValue <- chainPoset
        componentValue <-
          expectRight
            ( derivedObject posetValue $ do
                (x0, [sourceSummand]) <- object 0 [FinObjectId 1]
                (x1, [targetSummand]) <- object 1 [FinObjectId 0]
                differential x0 x1 [component sourceSummand targetSummand (1 :: GF2)]
            )
        denseValue <-
          expectRight
            ( derivedObject posetValue $ do
                (x0, _) <- object 0 [FinObjectId 1]
                (x1, _) <- object 1 [FinObjectId 0]
                differentialDense x0 x1 (DenseMat 1 1 (V.singleton (V.singleton (1 :: GF2))))
            )
        assertBool "dense escape equals component form" (componentValue == denseValue)

    , testCase "concentrated complex ends with an explicit empty successor" $ do
        posetValue <- singletonPoset
        builtValue <-
          expectRight
            ( derivedObject
                posetValue
                ( do
                    (x0, _) <- object 0 [FinObjectId 0, FinObjectId 0]
                    (x1, _) <- object 1 []
                    differential x0 x1 []
                ) :: Either DerivedBuildError (Derived GF2)
            )
        derivedPoset builtValue @?= posetValue

    , testCase "duplicate degree is rejected" $
        expectFault (DerivedBuildDuplicateDegree 0) $ do
          _ <- object 0 [FinObjectId 0]
          _ <- object 0 [FinObjectId 0]
          pure ()

    , testCase "non-adjacent differential is rejected" $
        expectFault (DerivedBuildNonAdjacentDifferential 0 2) $ do
          (x0, _) <- object 0 [FinObjectId 0]
          (x2, _) <- object 2 [FinObjectId 0]
          differential x0 x2 []

    , testCase "repeated differential is rejected" $
        expectFault (DerivedBuildDuplicateDifferential 0) $ do
          (x0, _) <- object 0 [FinObjectId 0]
          (x1, _) <- object 1 []
          differential x0 x1 []
          differential x0 x1 []

    , testCase "summand used against the wrong endpoint is rejected" $
        expectFault (DerivedBuildComponentDegreeMismatch 1 0) $ do
          (x0, [sourceSummand]) <- object 0 [FinObjectId 0]
          (x1, _) <- object 1 [FinObjectId 0]
          differential x0 x1 [component sourceSummand sourceSummand 1]

    , testCase "duplicate component cell is rejected" $
        expectFault (DerivedBuildDuplicateComponent 0 0 0) $ do
          (x0, [sourceSummand]) <- object 0 [FinObjectId 0]
          (x1, [targetSummand]) <- object 1 [FinObjectId 0]
          differential
            x0
            x1
            [ component sourceSummand targetSummand 1
            , component sourceSummand targetSummand 1
            ]

    , testCase "gapped degree window is rejected" $
        expectFault (DerivedBuildDegreeGap 2) $ do
          (x0, _) <- object 0 [FinObjectId 0]
          (x1, _) <- object 1 [FinObjectId 0]
          (_, _) <- object 3 [FinObjectId 0]
          differential x0 x1 []

    , testCase "missing differential is rejected" $
        expectFault (DerivedBuildMissingDifferential 0) $ do
          _ <- object 0 [FinObjectId 0]
          _ <- object 1 [FinObjectId 0]
          pure ()

    , testCase "dense shape mismatch is rejected" $
        expectFault (DerivedBuildDenseShapeMismatch 0 (1, 1) (2, 2)) $ do
          (x0, _) <- object 0 [FinObjectId 0]
          (x1, _) <- object 1 [FinObjectId 0]
          differentialDense x0 x1 (DenseMat 2 2 (V.replicate 2 (V.replicate 2 1)))

    , testCase "empty and single-degree builders are rejected as empty" $ do
        expectFault DerivedBuildEmpty (pure ())
        expectFault DerivedBuildEmpty $ do
          _ <- object 0 [FinObjectId 0]
          pure ()

    , testCase "refutable pattern failure is recorded as a typed fault" $ do
        posetValue <- singletonPoset
        case derivedObject posetValue builderWithBadPattern of
          Left (DerivedBuildPatternFailure _) -> pure ()
          other -> assertFailure ("expected a pattern-failure fault, got: " <> describe other)

    , testCase "objectsFrom agrees with per-degree declarations" $ do
        posetValue <- singletonPoset
        listedValue <- expectRight (derivedObject posetValue threeDegreeContiguous)
        manualValue <- expectRight (derivedObject posetValue threeDegreeManual)
        assertBool "objectsFrom equals manual declarations" (listedValue == manualValue)

    , testCase "the sole builder entry accepts lawful input" $ do
        posetValue <- chainPoset
        _ <- expectRight (derivedObject posetValue forwardChain)
        pure ()

    , testCase "order-crossing component is refused at authoring time" $ do
        posetValue <- chainPoset
        expectCompileFault
          (DerivedBuildOrderViolation 0 (FinObjectId 1) (FinObjectId 0))
          (derivedObject posetValue reversedChain)

    , testCase "sheaf-lawful entry refuses foreign labels even on zero differentials" $ do
        posetValue <- chainPoset
        expectCompileFault
          (DerivedBuildFailure (DerivedPosetUnknownNode 404))
          (derivedObject posetValue foreignZeroComplex)

    , testCase "authoring-time refusal agrees with the truncation seam gate" $ do
        posetValue <- antichainPoset
        expectCompileFault
          (DerivedBuildOrderViolation 0 (FinObjectId 0) (FinObjectId 1))
          (derivedObject posetValue forwardChain)
    ]

forwardChain :: DerivedBuilder GF2 ()
forwardChain = do
  (x0, [sourceSummand]) <- object 0 [FinObjectId 1]
  (x1, [targetSummand]) <- object 1 [FinObjectId 0]
  differential x0 x1 [component sourceSummand targetSummand 1]

reversedChain :: DerivedBuilder GF2 ()
reversedChain = do
  (x0, [sourceSummand]) <- object 0 [FinObjectId 0]
  (x1, [targetSummand]) <- object 1 [FinObjectId 1]
  differential x0 x1 [component sourceSummand targetSummand 1]

foreignZeroComplex :: DerivedBuilder GF2 ()
foreignZeroComplex = do
  (x0, _) <- object 0 [FinObjectId 404]
  (x1, _) <- object 1 [FinObjectId 404]
  differential x0 x1 []

threeDegreeContiguous :: DerivedBuilder GF2 ()
threeDegreeContiguous = do
  [(bottomObject, _), (middleObject, _), (topObject, _)] <-
    objectsFrom (-1) [[FinObjectId 0], [FinObjectId 0], [FinObjectId 0]]
  differential bottomObject middleObject []
  differential middleObject topObject []

threeDegreeManual :: DerivedBuilder GF2 ()
threeDegreeManual = do
  (bottomObject, _) <- object (-1) [FinObjectId 0]
  (middleObject, _) <- object 0 [FinObjectId 0]
  (topObject, _) <- object 1 [FinObjectId 0]
  differential bottomObject middleObject []
  differential middleObject topObject []

builderWithBadPattern :: DerivedBuilder GF2 ()
builderWithBadPattern = do
  (_, []) <- object 0 [FinObjectId 0]
  pure ()

describe :: Either DerivedBuildError a -> String
describe result =
  case result of
    Left buildError -> show buildError
    Right _ -> "a successful build"

expectFault :: DerivedBuildError -> DerivedBuilder GF2 a -> IO ()
expectFault expected builder = do
  posetValue <- singletonPoset
  case derivedObject posetValue builder of
    Left buildError -> buildError @?= expected
    Right _ -> assertFailure ("expected fault: " <> show expected)

expectCompileFault :: DerivedBuildError -> Either DerivedBuildError a -> IO ()
expectCompileFault expected result =
  case result of
    Left buildError -> buildError @?= expected
    Right _ -> assertFailure ("expected fault: " <> show expected)

expectRight :: Show e => Either e a -> IO a
expectRight = either (assertFailure . show) pure

singletonPoset :: IO DerivedPoset
singletonPoset =
  expectRight (mkDerivedPosetFromOrderEdges [FinObjectId 0] [])

chainPoset :: IO DerivedPoset
chainPoset =
  expectRight (mkDerivedPosetFromOrderEdges [FinObjectId 0, FinObjectId 1] [(FinObjectId 0, FinObjectId 1)])

antichainPoset :: IO DerivedPoset
antichainPoset =
  expectRight (mkDerivedPosetFromOrderEdges [FinObjectId 0, FinObjectId 1] [])
