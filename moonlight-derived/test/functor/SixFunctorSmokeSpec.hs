module SixFunctorSmokeSpec
  ( tests
  ) where

import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.Vector qualified as Vector
import Moonlight.Core (MoonlightError)
import Moonlight.Derived.Pure.Functor.ExceptionalPullback
  ( exceptionalPullback
  )
import Moonlight.Derived.Pure.Functor.ExceptionalPushforward
  ( exceptionalPushforward
  )
import Moonlight.Derived.Pure.Functor.Pullback
  ( pullback
  )
import Moonlight.Derived.Pure.Functor.Pushforward
  ( pushforward
  )
import Moonlight.Derived.Pure.Functor.Tensor
  ( internalHom
  , tensorProduct
  )
import Moonlight.Derived.Pure.Functor.VerdierDual
  ( verdierDualComplex
  )
import Moonlight.Derived.Pure.Morse.Hypercohomology
  ( hypercohomologyDimsWith
  )
import Moonlight.Derived.Pure.LinAlg.Interpreter
  ( gf2PackedRankBackend
  )
import Moonlight.Derived.Pure.Site.InjectiveComplex
  ( Derived (..)
  , InjectiveComplex (..)
  , composesToZero
  , getDerived
  , hasCompatibleObjectAxes
  , isMinimal
  , mkNormalizedDerived
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix
  ( DenseMat (..)
  , emptyAxis
  , fromExpanded
  , fromLabels
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
  ( Assertion
  , assertFailure
  , testCase
  , (@?=)
  )

tests :: TestTree
tests =
  testGroup
    "SixFunctorSmoke"
    [ testCase
        "tensor, Hom, dual, pullback, pushforward, and exceptional functors stay Morse-consumable"
        sixFunctorSmoke
    ]

sixFunctorSmoke :: Assertion
sixFunctorSmoke = do
  ambientPoset <-
    expectRight
      (chainPoset 3)

  targetPoset <-
    expectRight
      (chainPoset 2)

  sourceDerived <-
    expectRight
      (nontrivialChainDerived ambientPoset (FinObjectId 0) (FinObjectId 1))

  secondaryDerived <-
    expectRight
      (concentratedDerived ambientPoset (Vector.fromList [FinObjectId 1, FinObjectId 2]))

  targetDerived <-
    expectRight
      (nontrivialChainDerived targetPoset (FinObjectId 0) (FinObjectId 1))

  functorValue <-
    expectRight (mkTestFunctor ambientPoset targetPoset ambientToTarget)

  tensorDerived <-
    expectRight
      (tensorProduct sourceDerived secondaryDerived)

  internalHomDerived <-
    expectRight
      (internalHom sourceDerived secondaryDerived)

  verdierDualDerived <-
    expectRight
      (verdierDualComplex sourceDerived)

  pushforwardDerived <-
    expectRight
      (pushforward functorValue sourceDerived)

  pullbackDerived <-
    expectRight
      (pullback functorValue targetDerived)

  exceptionalPushforwardDerived <-
    expectRight
      (exceptionalPushforward functorValue sourceDerived)

  exceptionalPullbackDerived <-
    expectRight
      (exceptionalPullback functorValue targetDerived)

  let derivedValues =
        [ tensorDerived
        , internalHomDerived
        , verdierDualDerived
        , pushforwardDerived
        , pullbackDerived
        , exceptionalPushforwardDerived
        , exceptionalPullbackDerived
        ]

  traverse_ assertDerivedInvariants derivedValues
  traverse_ assertMorseConsumable derivedValues

chainPoset :: Int -> Either MoonlightError DerivedPoset
chainPoset nodeCount =
  mkDerivedPosetFromCovers nodesValue coversValue
  where
    nodesValue =
      fmap FinObjectId [0 .. max 0 nodeCount - 1]

    coversValue =
      zip nodesValue (drop 1 nodesValue)

ambientToTarget :: FinObjectId -> FinObjectId
ambientToTarget (FinObjectId nodeKey)
  | nodeKey <= 1 =
      FinObjectId 0
  | otherwise =
      FinObjectId 1

nontrivialChainDerived ::
  DerivedPoset ->
  FinObjectId ->
  FinObjectId ->
  Either MoonlightError (Derived GF2)
nontrivialChainDerived posetValue targetNode sourceNode =
  mkNormalizedDerived
    posetValue
    InjectiveComplex
      { icStart = 0
      , icDiffs =
          Vector.singleton
            ( fromExpanded
                (Vector.singleton targetNode)
                (Vector.singleton sourceNode)
                denseIdentity1
            )
      }

concentratedDerived ::
  DerivedPoset ->
  Vector.Vector FinObjectId ->
  Either MoonlightError (Derived GF2)
concentratedDerived posetValue axisLabels =
  mkNormalizedDerived
    posetValue
    InjectiveComplex
      { icStart = 0
      , icDiffs =
          Vector.singleton
            (zeroBlocked emptyAxis (fromLabels axisLabels))
      }

denseIdentity1 :: DenseMat GF2
denseIdentity1 =
  DenseMat
    { dmRows = 1
    , dmCols = 1
    , dmData = Vector.singleton (Vector.singleton 1)
    }

assertDerivedInvariants :: Derived GF2 -> Assertion
assertDerivedInvariants derivedValue = do
  let injectiveComplex = getDerived derivedValue
  hasCompatibleObjectAxes injectiveComplex @?= True
  composesToZero injectiveComplex @?= True
  isMinimal injectiveComplex @?= True

assertMorseConsumable :: Derived GF2 -> Assertion
assertMorseConsumable derivedValue =
  void
    ( expectRight
        (hypercohomologyDimsWith gf2PackedRankBackend derivedValue)
    )

expectRight :: Show errorValue => Either errorValue value -> IO value
expectRight eitherValue =
  case eitherValue of
    Right value ->
      pure value
    Left errorValue ->
      assertFailure (show errorValue)
