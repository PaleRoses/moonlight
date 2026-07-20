module ExceptionalPullbackSpec
  ( tests
  ) where

import Data.Either (isLeft)
import Data.Vector qualified as V
import Moonlight.Derived.Pure.Functor.ExceptionalPullback
  ( exceptionalPullback
  )
import Moonlight.Derived.Pure.Functor.Pullback
  ( pullback
  )
import Moonlight.Derived.Pure.Functor.VerdierDual
  ( verdierDualComplex
  )
import Moonlight.Derived.Pure.Site.InjectiveComplex
  ( Derived (..)
  , InjectiveComplex (..)
  , mkNormalizedDerived
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix
  ( BlockedMat
  , DenseMat (..)
  , GroupedAxis
  , emptyAxis
  , fromLabels
  , setBlock
  , zeroBlocked
  )
import Moonlight.Derived.Pure.Site.Poset
  ( FinObjectId (..)
  , DerivedPoset (..)
  , mkDerivedPosetFromCovers
  )
import Moonlight.LinAlg (GF2)
import Moonlight.Derived.Test.Fixture (mkTestFunctor)
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
    "ExceptionalPullback"
    [ testCase "exceptionalPullback is Verdier-conjugated pullback" $ do
        sourcePoset <- expectRight (mkDerivedPosetFromCovers [FinObjectId 10] [])
        targetPoset <- expectRight (mkDerivedPosetFromCovers [FinObjectId 0, FinObjectId 1] [(FinObjectId 0, FinObjectId 1)])
        targetComplex <- offDiagonalComplex targetPoset
        functorValue <- expectRight (mkTestFunctor sourcePoset targetPoset (const (FinObjectId 0)))
        expected <-
          expectRight
            ( do
                dualTarget <- verdierDualComplex targetComplex
                pulledDual <- pullback functorValue dualTarget
                verdierDualComplex pulledDual
            )
        actual <-
          expectRight
            (exceptionalPullback functorValue targetComplex)
        actual @?= expected

    , testCase "exceptionalPullback preserves identity on a self-dual singleton complex" $ do
        posetValue <- expectRight (mkDerivedPosetFromCovers [FinObjectId 0] [])
        targetComplex <- singletonComplex posetValue (FinObjectId 0)
        identityFunctor <- expectRight (mkTestFunctor posetValue posetValue id)
        actual <- expectRight (exceptionalPullback identityFunctor targetComplex)
        actual @?= targetComplex

    , testCase "exceptionalPullback rejects maps whose image leaves the target poset" $ do
        sourcePoset <- expectRight (mkDerivedPosetFromCovers [FinObjectId 0] [])
        targetPoset <- expectRight (mkDerivedPosetFromCovers [FinObjectId 0] [])
        assertBool
          "finite functor construction rejects a foreign target"
          (isLeft (mkTestFunctor sourcePoset targetPoset (const (FinObjectId 404))))
    ]

offDiagonalComplex ::
  DerivedPoset ->
  IO (Derived GF2)
offDiagonalComplex posetValue =
  expectRight
    ( mkNormalizedDerived
        posetValue
        InjectiveComplex
          { icStart = 0
          , icDiffs = V.singleton differential
          }
    )
  where
    axisValue :: GroupedAxis
    axisValue = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1])

    blockValue :: DenseMat GF2
    blockValue = DenseMat 1 1 (V.singleton (V.singleton 1)) :: DenseMat GF2

    differential :: BlockedMat GF2
    differential =
      setBlock
        (FinObjectId 0)
        (FinObjectId 1)
        blockValue
        (zeroBlocked axisValue axisValue)

singletonComplex ::
  DerivedPoset ->
  FinObjectId ->
  IO (Derived GF2)
singletonComplex posetValue nodeValue =
  expectRight
    ( mkNormalizedDerived
        posetValue
        InjectiveComplex
          { icStart = 0
          , icDiffs =
              V.singleton
                (zeroBlocked emptyAxis (fromLabels (V.singleton nodeValue)) :: BlockedMat GF2)
          }
    )

expectRight ::
  Show err =>
  Either err a ->
  IO a
expectRight =
  either (assertFailure . show) pure
