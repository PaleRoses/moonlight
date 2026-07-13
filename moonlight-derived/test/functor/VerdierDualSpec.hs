module VerdierDualSpec
  ( tests
  ) where

import Data.Bifunctor (first)
import Data.IntMap.Strict qualified as IM
import Data.Vector qualified as V
import Moonlight.Derived.Pure.Functor.VerdierDual
  ( dualizingComplex
  , verdierDualComplex
  )
import Moonlight.Derived.Pure.Functor.Presentation.Internal
  ( PreparedVerdierSite (..)
  , prepareVerdierSite
  )
import Moonlight.Derived.Pure.Morse.Hypercohomology (hypercohomologyDims)
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
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertFailure
  , testCase
  , (@?=)
  )

tests :: TestTree
tests =
  testGroup
    "VerdierDual"
    [ testCase "double Verdier dual fixes singleton support on a one-point poset" $ do
        posetValue <- expectRight (mkDerivedPosetFromCovers [FinObjectId 0] [])
        derivedValue <- singletonComplex posetValue (FinObjectId 0)
        doubleDual <- expectRight (doubleVerdierDual posetValue derivedValue)
        doubleDual @?= derivedValue

    , testCase "prepared Verdier site computes absolute topological dimension" $ do
        singletonPoset <- expectRight (mkDerivedPosetFromCovers [FinObjectId 0] [])
        twoChain <- expectRight (mkDerivedPosetFromCovers [FinObjectId 0, FinObjectId 1] [(FinObjectId 0, FinObjectId 1)])
        threeChain <- expectRight (mkDerivedPosetFromCovers [FinObjectId 0, FinObjectId 1, FinObjectId 2] [(FinObjectId 0, FinObjectId 1), (FinObjectId 1, FinObjectId 2)])
        diamondPoset <-
          expectRight
            ( mkDerivedPosetFromCovers
                [FinObjectId 0, FinObjectId 1, FinObjectId 2, FinObjectId 3]
                [(FinObjectId 0, FinObjectId 1), (FinObjectId 0, FinObjectId 2), (FinObjectId 1, FinObjectId 3), (FinObjectId 2, FinObjectId 3)]
            )
        fmap (pvsTopologicalDimension . prepareVerdierSite) [singletonPoset, twoChain, threeChain, diamondPoset]
          @?= [0, 1, 2, 2]

    , testCase "dualizing complex begins in the site's absolute dimension" $ do
        posetValue <- expectRight (mkDerivedPosetFromCovers [FinObjectId 0, FinObjectId 1, FinObjectId 2] [(FinObjectId 0, FinObjectId 1), (FinObjectId 1, FinObjectId 2)])
        dualizingValue <- expectRight (dualizingComplex posetValue)
        icStart (getDerived dualizingValue) @?= 2

    , testCase "double Verdier dual fixes point-supported singleton data on a zero-dimensional family" $ do
        posetValue <- expectRight (mkDerivedPosetFromCovers [FinObjectId 0, FinObjectId 1] [])
        leftPoint <- singletonComplex posetValue (FinObjectId 0)
        rightPoint <- singletonComplex posetValue (FinObjectId 1)
        leftDoubleDual <- expectRight (doubleVerdierDual posetValue leftPoint)
        rightDoubleDual <- expectRight (doubleVerdierDual posetValue rightPoint)
        leftDoubleDual @?= leftPoint
        rightDoubleDual @?= rightPoint

    , testCase "double Verdier dual fixes the discrete two-point dualizing complex" $ do
        posetValue <- expectRight (mkDerivedPosetFromCovers [FinObjectId 0, FinObjectId 1] [])
        dualizingValue <- expectRight (dualizingComplex posetValue)
        doubleDual <- expectRight (doubleVerdierDual posetValue dualizingValue)
        doubleDual @?= dualizingValue

    , testCase "double Verdier dual preserves total hypercohomology rank on the chain off-diagonal witness" $ do
        posetValue <- expectRight (mkDerivedPosetFromCovers [FinObjectId 0, FinObjectId 1] [(FinObjectId 0, FinObjectId 1)])
        derivedValue <- offDiagonalComplex posetValue
        doubleDual <- expectRight (doubleVerdierDual posetValue derivedValue)
        doubleDualRank <- totalHypercohomologyRank doubleDual
        originalRank <- totalHypercohomologyRank derivedValue
        doubleDualRank @?= originalRank

    , testCase "double Verdier dual fixes the chain off-diagonal witness after the normalization repair" $ do
        posetValue <- expectRight (mkDerivedPosetFromCovers [FinObjectId 0, FinObjectId 1] [(FinObjectId 0, FinObjectId 1)])
        derivedValue <- offDiagonalComplex posetValue
        doubleDual <- expectRight (doubleVerdierDual posetValue derivedValue)
        doubleDual @?= derivedValue
    ]

doubleVerdierDual ::
  DerivedPoset ->
  Derived GF2 ->
  Either String (Derived GF2)
doubleVerdierDual posetValue derivedValue =
  first show (verdierDualComplex derivedValue)
    >>= first show . verdierDualComplex

totalHypercohomologyRank :: Derived GF2 -> IO Int
totalHypercohomologyRank derivedValue =
  fmap (sum . IM.elems) (expectRight (hypercohomologyDims derivedValue))

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

expectRight :: Show err => Either err a -> IO a
expectRight =
  either (assertFailure . show) pure
