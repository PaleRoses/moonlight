{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module CategoricalSimplexSpec
  ( tests,
  )
where

import Data.Kind (Type)
import GHC.TypeNats (KnownNat)
import qualified Moonlight.Category.Indexed as Indexed
import Moonlight.Category.Indexed (Category ((.)))
import Moonlight.Category.Simplicial (categoricalSimplexToDeltaMorphism)
import Moonlight.Category.Simplicial
  ( Coface (..),
    Codegeneracy (..),
    DeltaMorphism,
    cofaceMorphism,
    codegeneracyMorphism,
    composeDeltaMorphism,
  )
import Moonlight.Category.Simplicial (Dimension (..), mkFinOffset)
import Numeric.Natural (Natural)
import Prelude hiding ((.))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

type Zero = Indexed.Z

type One = Indexed.S Zero

type Two = Indexed.S One

type Three = Indexed.S Two

zero :: Indexed.Simplex Zero Zero
zero = Indexed.simplexZero

one :: Indexed.Simplex One One
one = Indexed.simplexSucc zero

two :: Indexed.Simplex Two Two
two = Indexed.simplexSucc one

coface0At0 :: Indexed.Simplex Zero One
coface0At0 = Indexed.cofaceFirst zero

coface1At0 :: Indexed.Simplex Zero One
coface1At0 = Indexed.cofaceLast zero

coface0At1 :: Indexed.Simplex One Two
coface0At1 = Indexed.cofaceFirst one

coface1At1 :: Indexed.Simplex One Two
coface1At1 = Indexed.cofaceSucc coface0At0

coface2At1 :: Indexed.Simplex One Two
coface2At1 = Indexed.cofaceLast one

coface0At2 :: Indexed.Simplex Two Three
coface0At2 = Indexed.cofaceFirst two

coface1At2 :: Indexed.Simplex Two Three
coface1At2 = Indexed.cofaceSucc coface0At1

coface2At2 :: Indexed.Simplex Two Three
coface2At2 = Indexed.cofaceSucc coface1At1

coface3At2 :: Indexed.Simplex Two Three
coface3At2 = Indexed.cofaceLast two

codegeneracy0At0 :: Indexed.Simplex One Zero
codegeneracy0At0 = Indexed.codegeneracyFirst zero

codegeneracy0At1 :: Indexed.Simplex Two One
codegeneracy0At1 = Indexed.codegeneracyFirst one

codegeneracy1At1 :: Indexed.Simplex Two One
codegeneracy1At1 = Indexed.codegeneracyLast one

codegeneracy0At2 :: Indexed.Simplex Three Two
codegeneracy0At2 = Indexed.codegeneracyFirst two

codegeneracy1At2 :: Indexed.Simplex Three Two
codegeneracy1At2 = Indexed.codegeneracySucc codegeneracy0At1

codegeneracy2At2 :: Indexed.Simplex Three Two
codegeneracy2At2 = Indexed.codegeneracyLast two

operationalCoface :: forall n. KnownNat n => Dimension n -> Natural -> Maybe DeltaMorphism
operationalCoface dimension indexValue =
  cofaceMorphism . CofaceMap <$> mkFinOffset @n @2 dimension indexValue

operationalCodegeneracy :: forall n. KnownNat n => Dimension n -> Natural -> Maybe DeltaMorphism
operationalCodegeneracy dimension indexValue =
  codegeneracyMorphism . CodegeneracyMap <$> mkFinOffset @n @1 dimension indexValue

assertLowering :: String -> Maybe DeltaMorphism -> DeltaMorphism -> IO ()
assertLowering label expected actual =
  case expected of
    Nothing -> assertFailure ("expected operational generator for " <> label)
    Just expectedMorphism -> actual @?= expectedMorphism

assertLoweredComposition ::
  Indexed.Simplex (b :: Type) (c :: Type) ->
  Indexed.Simplex (a :: Type) b ->
  IO ()
assertLoweredComposition outer inner =
  composeDeltaMorphism
    (categoricalSimplexToDeltaMorphism outer)
    (categoricalSimplexToDeltaMorphism inner)
    @?= Just (categoricalSimplexToDeltaMorphism (outer . inner))

tests :: TestTree
tests =
  testGroup
    "CategoricalSimplex"
    [ testCase "categorical cofaces lower to operational cofaces for dimensions 0, 1, and 2" $ do
        assertLowering "δ0[0]" (operationalCoface (Dimension @0) 0) (categoricalSimplexToDeltaMorphism coface0At0)
        assertLowering "δ1[0]" (operationalCoface (Dimension @0) 1) (categoricalSimplexToDeltaMorphism coface1At0)
        assertLowering "δ0[1]" (operationalCoface (Dimension @1) 0) (categoricalSimplexToDeltaMorphism coface0At1)
        assertLowering "δ1[1]" (operationalCoface (Dimension @1) 1) (categoricalSimplexToDeltaMorphism coface1At1)
        assertLowering "δ2[1]" (operationalCoface (Dimension @1) 2) (categoricalSimplexToDeltaMorphism coface2At1)
        assertLowering "δ0[2]" (operationalCoface (Dimension @2) 0) (categoricalSimplexToDeltaMorphism coface0At2)
        assertLowering "δ1[2]" (operationalCoface (Dimension @2) 1) (categoricalSimplexToDeltaMorphism coface1At2)
        assertLowering "δ2[2]" (operationalCoface (Dimension @2) 2) (categoricalSimplexToDeltaMorphism coface2At2)
        assertLowering "δ3[2]" (operationalCoface (Dimension @2) 3) (categoricalSimplexToDeltaMorphism coface3At2),
      testCase "categorical codegeneracies lower to operational codegeneracies for dimensions 0, 1, and 2" $ do
        assertLowering "σ0[0]" (operationalCodegeneracy (Dimension @0) 0) (categoricalSimplexToDeltaMorphism codegeneracy0At0)
        assertLowering "σ0[1]" (operationalCodegeneracy (Dimension @1) 0) (categoricalSimplexToDeltaMorphism codegeneracy0At1)
        assertLowering "σ1[1]" (operationalCodegeneracy (Dimension @1) 1) (categoricalSimplexToDeltaMorphism codegeneracy1At1)
        assertLowering "σ0[2]" (operationalCodegeneracy (Dimension @2) 0) (categoricalSimplexToDeltaMorphism codegeneracy0At2)
        assertLowering "σ1[2]" (operationalCodegeneracy (Dimension @2) 1) (categoricalSimplexToDeltaMorphism codegeneracy1At2)
        assertLowering "σ2[2]" (operationalCodegeneracy (Dimension @2) 2) (categoricalSimplexToDeltaMorphism codegeneracy2At2),
      testCase "lowering preserves representative categorical composition" $ do
        assertLoweredComposition coface1At1 coface0At0
        assertLoweredComposition codegeneracy0At1 coface2At1
        assertLoweredComposition codegeneracy0At0 codegeneracy1At1
    ]
