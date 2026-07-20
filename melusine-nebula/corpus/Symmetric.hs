{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module Moonlight.LinAlg.Symmetric
  ( DiagonalizedSymmetric2 (..),
    mapDiagonalizedSymmetric2,
    diagonalizedSymmetric2ToTensor,
    diagonalizedSymmetric2ToVec2,
    eigendecomposeSymmetric2With,
    Symmetric2 (..),
    mapSymmetric2,
    zipSymmetric2With,
    diagonalSymmetric2,
    scaleSymmetric2,
    outerSymmetric2,
    traceSymmetric2,
    applySymmetric2,
    symmetric2Entries,
    symmetric2ToMatrix,
    eigendecomposeSymmetric2,
    DiagonalizedSymmetric3 (..),
    mapDiagonalizedSymmetric3,
    diagonalizedSymmetric3ToTensor,
    diagonalizedSymmetric3ToVec3,
    eigendecomposeSymmetric3With,
    eigendecomposeSymmetric3OrthonormalFrame,
    Symmetric3 (..),
    mapSymmetric3,
    zipSymmetric3With,
    diagonalSymmetric3,
    scaleSymmetric3,
    outerSymmetric3,
    traceSymmetric3,
    applySymmetric3,
    symmetric3Entries,
    symmetric3ToMatrix,
    eigendecomposeSymmetric3,
  )
where

import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Moonlight.Algebra (BilinearSpace (..), Module (..), VectorSpace)
import Moonlight.Core (AdditiveGroup (..), AdditiveMonoid (..), Field, Metric (..), MoonlightError, MultiplicativeMonoid (..), Ring)
import Moonlight.LinAlg.Geometry (OrthonormalFrame, identityOrthonormalFrame, orthonormalFrameFromMatrixEntries)
import Moonlight.LinAlg.Dense (symmetricEigen)
import Moonlight.LinAlg.Dense (Matrix, TensorElement, Vector, fromListMatrix, toListMatrix, toListVector)
import Moonlight.LinAlg.Geometry (Vec2 (..))
import Moonlight.LinAlg.Geometry (Vec3 (..))
import Prelude
  ( Double,
    Either,
    Eq,
    Maybe,
    Monoid (..),
    Ord,
    Read,
    Semigroup (..),
    Show,
    foldr,
    pure,
    sqrt,
    (*),
    (+),
    (.),
    (>>=),
  )

type DiagonalizedSymmetric2 :: Type -> Type -> Type
data DiagonalizedSymmetric2 axes a = DiagonalizedSymmetric2
  { diag2XX :: a,
    diag2YY :: a,
    diag2Axes :: axes
  }
  deriving stock (Eq, Ord, Show, Read)

mapDiagonalizedSymmetric2 ::
  (a -> b) ->
  DiagonalizedSymmetric2 axes a ->
  DiagonalizedSymmetric2 axes b
mapDiagonalizedSymmetric2 transform diagonalizedValue =
  DiagonalizedSymmetric2
    { diag2XX = transform (diag2XX diagonalizedValue),
      diag2YY = transform (diag2YY diagonalizedValue),
      diag2Axes = diag2Axes diagonalizedValue
    }

diagonalizedSymmetric2ToTensor ::
  AdditiveGroup a =>
  DiagonalizedSymmetric2 axes a ->
  Symmetric2 a
diagonalizedSymmetric2ToTensor diagonalizedValue =
  diagonalSymmetric2
    (diag2XX diagonalizedValue)
    (diag2YY diagonalizedValue)

diagonalizedSymmetric2ToVec2 ::
  DiagonalizedSymmetric2 axes Double ->
  Vec2
diagonalizedSymmetric2ToVec2 diagonalizedValue =
  Vec2
    (diag2XX diagonalizedValue)
    (diag2YY diagonalizedValue)

eigendecomposeSymmetric2With ::
  ([Double] -> Maybe axes) ->
  axes ->
  Symmetric2 Double ->
  Either MoonlightError (DiagonalizedSymmetric2 axes Double)
eigendecomposeSymmetric2With decodeAxes fallbackAxes tensorValue = do
  (eigenvalues, eigenvectors) <- eigendecomposeSymmetric2 tensorValue
  let resolvedAxes = fromMaybe fallbackAxes (decodeAxes (toListMatrix eigenvectors))
   in pure
        ( case toListVector eigenvalues of
            lambda1 : lambda2 : _ ->
              DiagonalizedSymmetric2
                { diag2XX = lambda1,
                  diag2YY = lambda2,
                  diag2Axes = resolvedAxes
                }
            lambda1 : _ ->
              DiagonalizedSymmetric2
                { diag2XX = lambda1,
                  diag2YY = 0.0,
                  diag2Axes = resolvedAxes
                }
            [] ->
              DiagonalizedSymmetric2
                { diag2XX = 0.0,
                  diag2YY = 0.0,
                  diag2Axes = resolvedAxes
                }
        )

type Symmetric2 :: Type -> Type
data Symmetric2 a = Symmetric2
  { sym2XX :: a,
    sym2XY :: a,
    sym2YY :: a
  }
  deriving stock (Eq, Ord, Show, Read)

instance AdditiveMonoid a => Semigroup (Symmetric2 a) where
  (<>) = zipSymmetric2With add

instance AdditiveMonoid a => Monoid (Symmetric2 a) where
  mempty = diagonalSymmetric2 zero zero

instance AdditiveMonoid a => AdditiveMonoid (Symmetric2 a) where
  zero = mempty
  add = (<>)

instance AdditiveGroup a => AdditiveGroup (Symmetric2 a) where
  neg = mapSymmetric2 neg

instance Ring a => Module a (Symmetric2 a) where
  scale = scaleSymmetric2

instance Field a => VectorSpace a (Symmetric2 a)

instance Field a => BilinearSpace a (Symmetric2 a) where
  bilinearForm leftValue rightValue =
    let doubledUnit = add one one
     in foldr
          add
          zero
          [ mul (sym2XX leftValue) (sym2XX rightValue),
            mul (sym2YY leftValue) (sym2YY rightValue),
            mul doubledUnit (mul (sym2XY leftValue) (sym2XY rightValue))
          ]

instance Metric (Symmetric2 Double) where
  type Magnitude (Symmetric2 Double) = Double
  magnitude tensorValue = sqrt (bilinearForm tensorValue tensorValue)

mapSymmetric2 :: (a -> b) -> Symmetric2 a -> Symmetric2 b
mapSymmetric2 transform tensorValue =
  Symmetric2
    { sym2XX = transform (sym2XX tensorValue),
      sym2XY = transform (sym2XY tensorValue),
      sym2YY = transform (sym2YY tensorValue)
    }

zipSymmetric2With :: (a -> b -> c) -> Symmetric2 a -> Symmetric2 b -> Symmetric2 c
zipSymmetric2With combine leftValue rightValue =
  Symmetric2
    { sym2XX = combine (sym2XX leftValue) (sym2XX rightValue),
      sym2XY = combine (sym2XY leftValue) (sym2XY rightValue),
      sym2YY = combine (sym2YY leftValue) (sym2YY rightValue)
    }

diagonalSymmetric2 :: AdditiveMonoid a => a -> a -> Symmetric2 a
diagonalSymmetric2 diagonalX diagonalY =
  Symmetric2
    { sym2XX = diagonalX,
      sym2XY = zero,
      sym2YY = diagonalY
    }

scaleSymmetric2 :: MultiplicativeMonoid a => a -> Symmetric2 a -> Symmetric2 a
scaleSymmetric2 = mapSymmetric2 . mul

outerSymmetric2 :: Double -> Vec2 -> Symmetric2 Double
outerSymmetric2 weightValue (Vec2 xValue yValue) =
  Symmetric2
    { sym2XX = weightValue * xValue * xValue,
      sym2XY = weightValue * xValue * yValue,
      sym2YY = weightValue * yValue * yValue
    }

traceSymmetric2 :: AdditiveGroup a => Symmetric2 a -> a
traceSymmetric2 tensorValue =
  add (sym2XX tensorValue) (sym2YY tensorValue)

applySymmetric2 :: Symmetric2 Double -> Vec2 -> Vec2
applySymmetric2 tensorValue (Vec2 xValue yValue) =
  Vec2
    (sym2XX tensorValue * xValue + sym2XY tensorValue * yValue)
    (sym2XY tensorValue * xValue + sym2YY tensorValue * yValue)

symmetric2Entries :: Symmetric2 a -> [a]
symmetric2Entries tensorValue =
  [ sym2XX tensorValue,
    sym2XY tensorValue,
    sym2XY tensorValue,
    sym2YY tensorValue
  ]

symmetric2ToMatrix :: TensorElement a => Symmetric2 a -> Either MoonlightError (Matrix 2 2 a)
symmetric2ToMatrix = fromListMatrix @2 @2 . symmetric2Entries

eigendecomposeSymmetric2 ::
  Symmetric2 Double ->
  Either MoonlightError (Vector 2 Double, Matrix 2 2 Double)
eigendecomposeSymmetric2 tensorValue =
  symmetric2ToMatrix tensorValue >>= symmetricEigen

type DiagonalizedSymmetric3 :: Type -> Type -> Type
data DiagonalizedSymmetric3 axes a = DiagonalizedSymmetric3
  { diag3XX :: a,
    diag3YY :: a,
    diag3ZZ :: a,
    diag3Axes :: axes
  }
  deriving stock (Eq, Ord, Show, Read)

mapDiagonalizedSymmetric3 ::
  (a -> b) ->
  DiagonalizedSymmetric3 axes a ->
  DiagonalizedSymmetric3 axes b
mapDiagonalizedSymmetric3 transform diagonalizedValue =
  DiagonalizedSymmetric3
    { diag3XX = transform (diag3XX diagonalizedValue),
      diag3YY = transform (diag3YY diagonalizedValue),
      diag3ZZ = transform (diag3ZZ diagonalizedValue),
      diag3Axes = diag3Axes diagonalizedValue
    }

diagonalizedSymmetric3ToTensor ::
  AdditiveGroup a =>
  DiagonalizedSymmetric3 axes a ->
  Symmetric3 a
diagonalizedSymmetric3ToTensor diagonalizedValue =
  diagonalSymmetric3
    (diag3XX diagonalizedValue)
    (diag3YY diagonalizedValue)
    (diag3ZZ diagonalizedValue)

diagonalizedSymmetric3ToVec3 ::
  DiagonalizedSymmetric3 axes Double ->
  Vec3
diagonalizedSymmetric3ToVec3 diagonalizedValue =
  Vec3
    (diag3XX diagonalizedValue)
    (diag3YY diagonalizedValue)
    (diag3ZZ diagonalizedValue)

eigendecomposeSymmetric3With ::
  ([Double] -> Maybe axes) ->
  axes ->
  Symmetric3 Double ->
  Either MoonlightError (DiagonalizedSymmetric3 axes Double)
eigendecomposeSymmetric3With decodeAxes fallbackAxes tensorValue = do
  (eigenvalues, eigenvectors) <- eigendecomposeSymmetric3 tensorValue
  let resolvedAxes = fromMaybe fallbackAxes (decodeAxes (toListMatrix eigenvectors))
   in pure
        ( case toListVector eigenvalues of
            lambda1 : lambda2 : lambda3 : _ ->
              DiagonalizedSymmetric3
                { diag3XX = lambda1,
                  diag3YY = lambda2,
                  diag3ZZ = lambda3,
                  diag3Axes = resolvedAxes
                }
            lambda1 : lambda2 : _ ->
              DiagonalizedSymmetric3
                { diag3XX = lambda1,
                  diag3YY = lambda2,
                  diag3ZZ = 0.0,
                  diag3Axes = resolvedAxes
                }
            lambda1 : _ ->
              DiagonalizedSymmetric3
                { diag3XX = lambda1,
                  diag3YY = 0.0,
                  diag3ZZ = 0.0,
                  diag3Axes = resolvedAxes
                }
            [] ->
              DiagonalizedSymmetric3
                { diag3XX = 0.0,
                  diag3YY = 0.0,
                  diag3ZZ = 0.0,
                  diag3Axes = resolvedAxes
                }
        )

eigendecomposeSymmetric3OrthonormalFrame ::
  Symmetric3 Double ->
  Either MoonlightError (DiagonalizedSymmetric3 OrthonormalFrame Double)
eigendecomposeSymmetric3OrthonormalFrame =
  eigendecomposeSymmetric3With
    orthonormalFrameFromMatrixEntries
    identityOrthonormalFrame

type Symmetric3 :: Type -> Type
data Symmetric3 a = Symmetric3
  { sym3XX :: a,
    sym3XY :: a,
    sym3XZ :: a,
    sym3YY :: a,
    sym3YZ :: a,
    sym3ZZ :: a
  }
  deriving stock (Eq, Ord, Show, Read)

instance AdditiveMonoid a => Semigroup (Symmetric3 a) where
  (<>) = zipSymmetric3With add

instance AdditiveMonoid a => Monoid (Symmetric3 a) where
  mempty = diagonalSymmetric3 zero zero zero

instance AdditiveMonoid a => AdditiveMonoid (Symmetric3 a) where
  zero = mempty
  add = (<>)

instance AdditiveGroup a => AdditiveGroup (Symmetric3 a) where
  neg = mapSymmetric3 neg

instance Ring a => Module a (Symmetric3 a) where
  scale = scaleSymmetric3

instance Field a => VectorSpace a (Symmetric3 a)

instance Field a => BilinearSpace a (Symmetric3 a) where
  bilinearForm leftValue rightValue =
    let doubledUnit = add one one
     in foldr
          add
          zero
          [ mul (sym3XX leftValue) (sym3XX rightValue),
            mul (sym3YY leftValue) (sym3YY rightValue),
            mul (sym3ZZ leftValue) (sym3ZZ rightValue),
            mul
              doubledUnit
              ( foldr
                  add
                  zero
                  [ mul (sym3XY leftValue) (sym3XY rightValue),
                    mul (sym3XZ leftValue) (sym3XZ rightValue),
                    mul (sym3YZ leftValue) (sym3YZ rightValue)
                  ]
              )
          ]

instance Metric (Symmetric3 Double) where
  type Magnitude (Symmetric3 Double) = Double
  magnitude tensorValue = sqrt (bilinearForm tensorValue tensorValue)

mapSymmetric3 :: (a -> b) -> Symmetric3 a -> Symmetric3 b
mapSymmetric3 transform tensorValue =
  Symmetric3
    { sym3XX = transform (sym3XX tensorValue),
      sym3XY = transform (sym3XY tensorValue),
      sym3XZ = transform (sym3XZ tensorValue),
      sym3YY = transform (sym3YY tensorValue),
      sym3YZ = transform (sym3YZ tensorValue),
      sym3ZZ = transform (sym3ZZ tensorValue)
    }

zipSymmetric3With :: (a -> b -> c) -> Symmetric3 a -> Symmetric3 b -> Symmetric3 c
zipSymmetric3With combine leftValue rightValue =
  Symmetric3
    { sym3XX = combine (sym3XX leftValue) (sym3XX rightValue),
      sym3XY = combine (sym3XY leftValue) (sym3XY rightValue),
      sym3XZ = combine (sym3XZ leftValue) (sym3XZ rightValue),
      sym3YY = combine (sym3YY leftValue) (sym3YY rightValue),
      sym3YZ = combine (sym3YZ leftValue) (sym3YZ rightValue),
      sym3ZZ = combine (sym3ZZ leftValue) (sym3ZZ rightValue)
    }

diagonalSymmetric3 :: AdditiveMonoid a => a -> a -> a -> Symmetric3 a
diagonalSymmetric3 diagonalX diagonalY diagonalZ =
  Symmetric3
    { sym3XX = diagonalX,
      sym3XY = zero,
      sym3XZ = zero,
      sym3YY = diagonalY,
      sym3YZ = zero,
      sym3ZZ = diagonalZ
    }

scaleSymmetric3 :: MultiplicativeMonoid a => a -> Symmetric3 a -> Symmetric3 a
scaleSymmetric3 = mapSymmetric3 . mul

outerSymmetric3 :: Double -> Vec3 -> Symmetric3 Double
outerSymmetric3 weightValue (Vec3 xValue yValue zValue) =
  Symmetric3
    { sym3XX = weightValue * xValue * xValue,
      sym3XY = weightValue * xValue * yValue,
      sym3XZ = weightValue * xValue * zValue,
      sym3YY = weightValue * yValue * yValue,
      sym3YZ = weightValue * yValue * zValue,
      sym3ZZ = weightValue * zValue * zValue
    }

traceSymmetric3 :: AdditiveGroup a => Symmetric3 a -> a
traceSymmetric3 tensorValue =
  foldr add zero [sym3XX tensorValue, sym3YY tensorValue, sym3ZZ tensorValue]

applySymmetric3 :: Symmetric3 Double -> Vec3 -> Vec3
applySymmetric3 tensorValue (Vec3 xValue yValue zValue) =
  Vec3
    (sym3XX tensorValue * xValue + sym3XY tensorValue * yValue + sym3XZ tensorValue * zValue)
    (sym3XY tensorValue * xValue + sym3YY tensorValue * yValue + sym3YZ tensorValue * zValue)
    (sym3XZ tensorValue * xValue + sym3YZ tensorValue * yValue + sym3ZZ tensorValue * zValue)

symmetric3Entries :: Symmetric3 a -> [a]
symmetric3Entries tensorValue =
  [ sym3XX tensorValue,
    sym3XY tensorValue,
    sym3XZ tensorValue,
    sym3XY tensorValue,
    sym3YY tensorValue,
    sym3YZ tensorValue,
    sym3XZ tensorValue,
    sym3YZ tensorValue,
    sym3ZZ tensorValue
  ]

symmetric3ToMatrix :: TensorElement a => Symmetric3 a -> Either MoonlightError (Matrix 3 3 a)
symmetric3ToMatrix = fromListMatrix @3 @3 . symmetric3Entries

eigendecomposeSymmetric3 ::
  Symmetric3 Double ->
  Either MoonlightError (Vector 3 Double, Matrix 3 3 Double)
eigendecomposeSymmetric3 tensorValue =
  symmetric3ToMatrix tensorValue >>= symmetricEigen
