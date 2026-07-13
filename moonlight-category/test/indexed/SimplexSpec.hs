{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module SimplexSpec
  ( tests,
  )
where

import Prelude hiding ((.))

import qualified Moonlight.Category.Indexed as Indexed
import Moonlight.Category.Indexed (Category ((.), src, tgt))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

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

three :: Indexed.Simplex Three Three
three = Indexed.simplexSucc two

firstVertex01 :: Indexed.Simplex Zero One
firstVertex01 = Indexed.cofaceLast zero

lastVertex01 :: Indexed.Simplex Zero One
lastVertex01 = Indexed.cofaceFirst zero

collapse10 :: Indexed.Simplex One Zero
collapse10 = Indexed.codegeneracyFirst zero

lastFace12 :: Indexed.Simplex One Two
lastFace12 = Indexed.cofaceFirst one

collapse20 :: Indexed.Simplex Two Zero
collapse20 = Indexed.simplexCollapse two

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

standardZero :: Indexed.StandardSimplex Zero
standardZero = Indexed.Hom_X zero

standardZeroIdentity :: Indexed.SSet (Indexed.StandardSimplex Zero) (Indexed.StandardSimplex Zero)
standardZeroIdentity = Indexed.natId standardZero

tests :: TestTree
tests =
  testGroup
    "Indexed Simplex"
    [ testCase "src and tgt return object identities" $ do
        src lastVertex01 @?= zero
        tgt lastVertex01 @?= one
        src collapse10 @?= one
        tgt collapse10 @?= zero,
      testCase "identity laws hold for concrete simplex arrows" $ do
        lastVertex01 . src lastVertex01 @?= lastVertex01
        tgt lastVertex01 . lastVertex01 @?= lastVertex01
        collapse10 . src collapse10 @?= collapse10
        tgt collapse10 . collapse10 @?= collapse10,
      testCase "composition is associative on concrete simplex arrows" $ do
        (collapse10 . lastVertex01) . zero @?= collapse10 . (lastVertex01 . zero)
        (collapse20 . lastFace12) . firstVertex01 @?= collapse20 . (lastFace12 . firstVertex01),
      testCase "forgetful functor maps arrows to finite-ordinal functions" $ do
        let firstVertexMap = Indexed.ForgetSimplex Indexed.% firstVertex01
            lastVertexMap = Indexed.ForgetSimplex Indexed.% lastVertex01
            collapseMap = Indexed.ForgetSimplex Indexed.% collapse10
        firstVertexMap Indexed.Fz @?= Indexed.Fz
        lastVertexMap Indexed.Fz @?= Indexed.Fs Indexed.Fz
        collapseMap Indexed.Fz @?= Indexed.Fz
        collapseMap (Indexed.Fs Indexed.Fz) @?= Indexed.Fz,
      testCase "simplexValues decodes monotone maps" $ do
        Indexed.simplexValues zero @?= [0]
        Indexed.simplexValues one @?= [0, 1]
        Indexed.simplexValues two @?= [0, 1, 2]
        Indexed.simplexValues three @?= [0, 1, 2, 3]
        Indexed.simplexValues firstVertex01 @?= [0]
        Indexed.simplexValues lastVertex01 @?= [1]
        Indexed.simplexValues collapse10 @?= [0, 0]
        Indexed.simplexValues lastFace12 @?= [1, 2],
      testCase "coface constructors decode to classical skipped-index maps" $ do
        Indexed.simplexValues coface0At0 @?= [1]
        Indexed.simplexValues coface1At0 @?= [0]
        Indexed.simplexValues coface0At1 @?= [1, 2]
        Indexed.simplexValues coface1At1 @?= [0, 2]
        Indexed.simplexValues coface2At1 @?= [0, 1]
        Indexed.simplexValues coface0At2 @?= [1, 2, 3]
        Indexed.simplexValues coface1At2 @?= [0, 2, 3]
        Indexed.simplexValues coface2At2 @?= [0, 1, 3]
        Indexed.simplexValues coface3At2 @?= [0, 1, 2],
      testCase "codegeneracy constructors decode to classical repeated-index maps" $ do
        Indexed.simplexValues codegeneracy0At0 @?= [0, 0]
        Indexed.simplexValues codegeneracy0At1 @?= [0, 0, 1]
        Indexed.simplexValues codegeneracy1At1 @?= [0, 1, 1]
        Indexed.simplexValues codegeneracy0At2 @?= [0, 0, 1, 2]
        Indexed.simplexValues codegeneracy1At2 @?= [0, 1, 1, 2]
        Indexed.simplexValues codegeneracy2At2 @?= [0, 1, 2, 2],
      testCase "coface/coface cosimplicial identity holds concretely" $ do
        coface1At1 . coface0At0 @?= coface0At1 . coface0At0
        coface2At1 . coface0At0 @?= coface0At1 . coface1At0
        coface2At1 . coface1At0 @?= coface1At1 . coface1At0,
      testCase "codegeneracy/codegeneracy cosimplicial identity holds concretely" $ do
        codegeneracy0At0 . codegeneracy0At1 @?= codegeneracy0At0 . codegeneracy1At1
        codegeneracy0At1 . codegeneracy0At2 @?= codegeneracy0At1 . codegeneracy1At2
        codegeneracy1At1 . codegeneracy0At2 @?= codegeneracy0At1 . codegeneracy2At2
        codegeneracy1At1 . codegeneracy1At2 @?= codegeneracy1At1 . codegeneracy2At2,
      testCase "mixed left cosimplicial identity holds concretely" $ do
        codegeneracy1At1 . coface0At1 @?= coface0At0 . codegeneracy0At0
        codegeneracy2At2 . coface0At2 @?= coface0At1 . codegeneracy1At1
        codegeneracy2At2 . coface1At2 @?= coface1At1 . codegeneracy1At1,
      testCase "mixed identity cosimplicial cases hold concretely" $ do
        codegeneracy0At0 . coface0At0 @?= zero
        codegeneracy0At0 . coface1At0 @?= zero
        codegeneracy0At1 . coface0At1 @?= one
        codegeneracy0At1 . coface1At1 @?= one
        codegeneracy1At1 . coface1At1 @?= one
        codegeneracy1At1 . coface2At1 @?= one,
      testCase "mixed right cosimplicial identity holds concretely" $ do
        codegeneracy0At1 . coface2At1 @?= coface1At0 . codegeneracy0At0
        codegeneracy0At2 . coface2At2 @?= coface1At1 . codegeneracy0At1
        codegeneracy0At2 . coface3At2 @?= coface2At1 . codegeneracy0At1
        codegeneracy1At2 . coface3At2 @?= coface2At1 . codegeneracy1At1,
      testCase "domain extension preserves the lower endpoint" $ do
        Indexed.simplexExtendDomain lastVertex01 @?= one,
      testCase "representable standard simplex inhabits SSet" $ do
        let component = standardZeroIdentity Indexed.! Indexed.Op zero
        component zero @?= zero
    ]
