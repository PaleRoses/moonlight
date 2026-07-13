module ExceptionalPushforwardSpec
  ( tests
  ) where

import Data.Either (isLeft)
import Data.Vector qualified as V
import Moonlight.Core (MoonlightError (..))
import Moonlight.Derived.Pure.Functor.ExceptionalPushforward
  ( exceptionalPushforward
  )
import Moonlight.Derived.Pure.Functor.Pushforward
  ( pushforward
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
    "ExceptionalPushforward"
    [ testCase "exceptionalPushforward is Verdier-conjugated pushforward" $ do
        sourcePoset <- expectRight (mkDerivedPosetFromCovers [FinObjectId 0, FinObjectId 1] [(FinObjectId 0, FinObjectId 1)])
        targetPoset <- expectRight (mkDerivedPosetFromCovers [FinObjectId 10] [])
        sourceComplex <- offDiagonalComplex sourcePoset
        functorValue <- expectRight (mkTestFunctor sourcePoset targetPoset (const (FinObjectId 10)))
        expected <-
          expectRight
            ( do
                dualSource <- verdierDualComplex sourceComplex
                pushedDual <- pushforward functorValue dualSource
                verdierDualComplex pushedDual
            )
        actual <-
          expectRight
            (exceptionalPushforward functorValue sourceComplex)
        actual @?= expected

    , testCase "exceptionalPushforward preserves identity on a self-dual singleton complex" $ do
        posetValue <- expectRight (mkDerivedPosetFromCovers [FinObjectId 0] [])
        sourceComplex <- singletonComplex posetValue (FinObjectId 0)
        identityFunctor <- expectRight (mkTestFunctor posetValue posetValue id)
        actual <- expectRight (exceptionalPushforward identityFunctor sourceComplex)
        actual @?= sourceComplex

    , testCase "exceptionalPushforward preserves identity on the chain off-diagonal witness" $ do
        posetValue <- expectRight (mkDerivedPosetFromCovers [FinObjectId 0, FinObjectId 1] [(FinObjectId 0, FinObjectId 1)])
        sourceComplex <- offDiagonalComplex posetValue
        identityFunctor <- expectRight (mkTestFunctor posetValue posetValue id)
        actual <- expectRight (exceptionalPushforward identityFunctor sourceComplex)
        actual @?= sourceComplex

    , testCase "exceptionalPushforward rejects maps whose image leaves the target poset" $ do
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
    axisValue = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1])
    blockValue = DenseMat 1 1 (V.singleton (V.singleton 1)) :: DenseMat GF2
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

expectRight :: Show err => Either err a -> IO a
expectRight =
  either (assertFailure . show) pure
