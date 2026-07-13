{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Derived.Pure.Functor.Tensor.SupportPresentation
  ( TensorSupportPresentation (..)
  , tensorSupportPresentation
  , tensorSupportRestrictionSparse
  , tensorSupportRestrictionDense
  ) where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Moonlight.Core (MoonlightError (..))
import Moonlight.Derived.Pure.Functor.ClosedSupport.Geometry
  ( maximalSupportNodes
  , restrictedClosedPoset
  , validateClosedSupport
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix
  ( DenseMat
  , SparseMat (..)
  , SparseMatrixEntry (..)
  , sparseMatToDense
  )
import Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset
  , FinObjectId
  )
import Moonlight.Derived.Pure.Site.Poset.OrderComplex
  ( PosetChain
  , PreparedOrderComplex
  , facesOfChain
  , prepareOrderComplex
  , preparedOrderComplexChainIndexMaps
  , preparedOrderComplexChainsByDegree
  )

type TensorSupportPresentation :: Type -> Type
data TensorSupportPresentation a = TensorSupportPresentation
  { tspSupportNodes :: !IntSet
  , tspChainsByDegree :: !(Vector [PosetChain])
  , tspChainIndexMaps :: !(Vector (Map PosetChain Int))
  , tspAxes :: !(Vector (Vector FinObjectId))
  , tspDiffs :: !(Vector (SparseMat a))
  }

tensorSupportPresentation ::
  Num a =>
  DerivedPoset ->
  IntSet ->
  Either MoonlightError (TensorSupportPresentation a)
tensorSupportPresentation posetValue supportNodeSet = do
  validatedSupport <-
    validateClosedSupport
      "tensorSupportPresentation"
      posetValue
      supportNodeSet

  if IntSet.null validatedSupport
    then
      Right emptyTensorSupportPresentation
    else
      case principalSupportNode posetValue validatedSupport of
        Just principalNode ->
          Right
            ( principalTensorSupportPresentation
                validatedSupport
                principalNode
            )
        Nothing -> do
          let supportPoset =
                restrictedClosedPoset
                  posetValue
                  validatedSupport
              orderComplex =
                prepareOrderComplex supportPoset

              chainsByDegree =
                preparedOrderComplexChainsByDegree orderComplex

              chainIndexMaps =
                preparedOrderComplexChainIndexMaps orderComplex

          axes <-
            traverse axisFromChains chainsByDegree

          let differentials =
                V.generate
                  (max 0 (V.length chainsByDegree - 1))
                  ( supportDifferentialAt
                      chainsByDegree
                      chainIndexMaps
                  )

          Right
            TensorSupportPresentation
              { tspSupportNodes = validatedSupport
              , tspChainsByDegree = chainsByDegree
              , tspChainIndexMaps = chainIndexMaps
              , tspAxes = axes
              , tspDiffs = differentials
              }

tensorSupportRestrictionDense ::
  Num a =>
  TensorSupportPresentation a ->
  TensorSupportPresentation a ->
  Int ->
  Either MoonlightError (DenseMat a)
tensorSupportRestrictionDense sourcePresentation targetPresentation degreeValue =
  fmap
    sparseMatToDense
    ( tensorSupportRestrictionSparse
        sourcePresentation
        targetPresentation
        degreeValue
    )

tensorSupportRestrictionSparse ::
  Num a =>
  TensorSupportPresentation a ->
  TensorSupportPresentation a ->
  Int ->
  Either MoonlightError (SparseMat a)
tensorSupportRestrictionSparse
  sourcePresentation
  targetPresentation
  degreeValue
    | not
        ( tspSupportNodes targetPresentation
            `IntSet.isSubsetOf` tspSupportNodes sourcePresentation
        ) =
        Left
          ( InvariantViolation
              ( "tensorSupportRestrictionDense: target support is not contained in the source support: "
                  <> show
                    (IntSet.toList (tspSupportNodes targetPresentation))
                  <> " is not contained in "
                  <> show
                    (IntSet.toList (tspSupportNodes sourcePresentation))
              )
          )
    | otherwise =
        Right
          SparseMat
            { smRows = targetCount
            , smCols = sourceCount
            , smEntries = retainedEntries
            }
  where
    sourceChains =
      chainsAtDegree sourcePresentation degreeValue

    targetIndexMap =
      chainIndexMapAtDegree targetPresentation degreeValue

    sourceCount =
      length sourceChains

    targetCount =
      Map.size targetIndexMap

    retainedEntries =
      mapMaybe
        ( \(sourceIndexValue, sourceChain) ->
            fmap
              ( \targetIndexValue ->
                  SparseMatrixEntry
                    { smeRow = targetIndexValue
                    , smeColumn = sourceIndexValue
                    , smeValue = 1
                    }
              )
              (Map.lookup sourceChain targetIndexMap)
        )
        (zip [0 :: Int ..] sourceChains)

emptyTensorSupportPresentation :: TensorSupportPresentation a
emptyTensorSupportPresentation =
  TensorSupportPresentation
    { tspSupportNodes = IntSet.empty
    , tspChainsByDegree = V.singleton []
    , tspChainIndexMaps = V.singleton Map.empty
    , tspAxes = V.singleton V.empty
    , tspDiffs = V.empty
    }

principalTensorSupportPresentation ::
  IntSet ->
  FinObjectId ->
  TensorSupportPresentation a
principalTensorSupportPresentation supportNodeSet principalNode =
  TensorSupportPresentation
    { tspSupportNodes = supportNodeSet
    , tspChainsByDegree = V.singleton [[principalNode]]
    , tspChainIndexMaps =
        V.singleton (Map.singleton [principalNode] 0)
    , tspAxes = V.singleton (V.singleton principalNode)
    , tspDiffs = V.empty
    }

principalSupportNode :: DerivedPoset -> IntSet -> Maybe FinObjectId
principalSupportNode posetValue supportNodeSet =
  case maximalSupportNodes posetValue supportNodeSet of
    [principalNode] ->
      Just principalNode
    _ ->
      Nothing

supportDifferentialAt ::
  Num a =>
  Vector [PosetChain] ->
  Vector (Map PosetChain Int) ->
  Int ->
  SparseMat a
supportDifferentialAt chainsByDegree chainIndexMaps degreeValue =
  SparseMat
    { smRows = targetCount
    , smCols = sourceCount
    , smEntries = differentialEntries
    }
  where
    sourceChains =
      vectorAt degreeValue chainsByDegree []

    targetChains =
      vectorAt (degreeValue + 1) chainsByDegree []

    sourceIndexMap =
      vectorAt degreeValue chainIndexMaps Map.empty

    sourceCount =
      length sourceChains

    targetCount =
      length targetChains

    differentialEntries =
      concatMap
        ( \(targetIndexValue, targetChain) ->
            mapMaybe
              (faceEntry targetIndexValue sourceIndexMap)
              (zip [0 :: Int ..] (facesOfChain targetChain))
        )
        (zip [0 :: Int ..] targetChains)

faceEntry ::
  Num a =>
  Int ->
  Map PosetChain Int ->
  (Int, PosetChain) ->
  Maybe (SparseMatrixEntry a)
faceEntry
  targetIndexValue
  sourceIndexMap
  (faceIndexValue, sourceChain) =
    fmap
      ( \sourceIndexValue ->
          SparseMatrixEntry
            { smeRow = targetIndexValue
            , smeColumn = sourceIndexValue
            , smeValue = faceCoefficient faceIndexValue
            }
      )
      (Map.lookup sourceChain sourceIndexMap)

faceCoefficient :: Num a => Int -> a
faceCoefficient faceIndexValue
  | even faceIndexValue =
      1
  | otherwise =
      negate 1

chainsAtDegree ::
  TensorSupportPresentation a ->
  Int ->
  [PosetChain]
chainsAtDegree
  TensorSupportPresentation {tspChainsByDegree}
  degreeValue =
    vectorAt degreeValue tspChainsByDegree []

chainIndexMapAtDegree ::
  TensorSupportPresentation a ->
  Int ->
  Map PosetChain Int
chainIndexMapAtDegree
  TensorSupportPresentation {tspChainIndexMaps}
  degreeValue =
    vectorAt degreeValue tspChainIndexMaps Map.empty

axisFromChains ::
  [PosetChain] ->
  Either MoonlightError (Vector FinObjectId)
axisFromChains =
  fmap V.fromList . traverse chainAnchor

chainAnchor :: PosetChain -> Either MoonlightError FinObjectId
chainAnchor chainValue =
  case chainValue of
    nodeValue : _ ->
      Right nodeValue
    [] ->
      Left
        ( InvariantViolation
            "tensorSupportPresentation: encountered an empty chain in a nonempty degree"
        )

vectorAt :: Int -> Vector value -> value -> value
vectorAt indexValue vectorValue fallbackValue =
  case vectorValue V.!? indexValue of
    Just value ->
      value
    Nothing ->
      fallbackValue
