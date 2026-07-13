{-# OPTIONS_GHC -Wno-orphans #-}

module ConstraintArbitrary () where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Moonlight.Constraint (CoFiniteTruth, ConstraintExpr (..), EndoPatch, coFiniteTruth, endoPatch)
import qualified Test.Tasty.QuickCheck as QC

arbitraryConstraintExprSized :: (QC.Arbitrary a, Ord a) => Int -> QC.Gen (ConstraintExpr a)
arbitraryConstraintExprSized size
  | size <= 0 = Atom <$> QC.arbitrary
  | otherwise =
      let childSize = max 0 (size `div` 3)
          maxChildren = max 0 (min 4 childSize)
          genChildren =
            QC.chooseInt (0, maxChildren)
              >>= \count -> QC.vectorOf count (arbitraryConstraintExprSized childSize)
       in QC.oneof
            [ Atom <$> QC.arbitrary,
              And <$> genChildren,
              Or <$> genChildren,
              Not <$> arbitraryConstraintExprSized (size - 1)
            ]

instance (QC.Arbitrary a, Ord a) => QC.Arbitrary (ConstraintExpr a) where
  arbitrary = QC.sized arbitraryConstraintExprSized
  shrink expression =
    case expression of
      Atom _ -> []
      And children -> children <> map And (QC.shrink children)
      Or children -> children <> map Or (QC.shrink children)
      Not inner -> [inner] <> map Not (QC.shrink inner)

instance (QC.Arbitrary k, Ord k) => QC.Arbitrary (CoFiniteTruth k) where
  arbitrary = coFiniteTruth <$> QC.arbitrary <*> (Map.fromList <$> QC.arbitrary)

instance (QC.Arbitrary k, Ord k) => QC.Arbitrary (EndoPatch k) where
  arbitrary = endoPatch <$> (Set.fromList <$> QC.arbitrary) <*> (Set.fromList <$> QC.arbitrary)
