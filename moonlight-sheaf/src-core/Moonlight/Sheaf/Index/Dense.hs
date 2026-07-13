{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Index.Dense
  ( DenseIndex,
    mkDenseIndex,
    mkDenseIndexFromDistinct,
    mkDenseIndexByDenseValue,
    denseIndexCount,
    denseIndexValues,
    denseIndexIndexedValues,
    denseIndexKeys,
    denseIndexKeyOf,
    denseIndexKeyOfDenseValue,
    denseIndexValueAt,
    denseIndexContains,
    denseIndexKeyIntSet,
  )
where

import Data.Containers.ListUtils (nubOrd)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Moonlight.Core (DenseKey (..))

type DenseIndex :: Type -> Type -> Type
data DenseIndex key value = DenseIndex
  { diValues :: !(Vector value),
    diLookup :: !(Map value key),
    diDenseLookup :: !(Maybe (IntMap key))
  }
  deriving stock (Eq, Show)

mkDenseIndex :: (DenseKey key, Ord value) => [value] -> DenseIndex key value
mkDenseIndex values =
  mkDenseIndexFromDistinct (nubOrd values)

mkDenseIndexFromDistinct :: (DenseKey key, Ord value) => [value] -> DenseIndex key value
mkDenseIndexFromDistinct values =
  DenseIndex
    { diValues = Vector.fromList values,
      diLookup = Map.fromList (zip values (fmap decodeDenseKey [0 :: Int ..])),
      diDenseLookup = Nothing
    }

mkDenseIndexByDenseValue :: (DenseKey key, DenseKey value) => [value] -> DenseIndex key value
mkDenseIndexByDenseValue values =
  let distinctValues = nubOrd values
      indexedValues = zip distinctValues (fmap decodeDenseKey [0 :: Int ..])
   in DenseIndex
        { diValues = Vector.fromList distinctValues,
          diLookup = Map.fromList indexedValues,
          diDenseLookup =
            Just
              ( IntMap.fromList
                  (fmap (\(value, key) -> (encodeDenseKey value, key)) indexedValues)
              )
        }

denseIndexCount :: DenseIndex key value -> Int
denseIndexCount =
  Vector.length . diValues
{-# INLINE denseIndexCount #-}

denseIndexValues :: DenseIndex key value -> [value]
denseIndexValues =
  Vector.toList . diValues

denseIndexIndexedValues :: DenseKey key => DenseIndex key value -> [(key, value)]
denseIndexIndexedValues indexValue =
  zip (denseIndexKeys indexValue) (denseIndexValues indexValue)

denseIndexKeys :: DenseKey key => DenseIndex key value -> [key]
denseIndexKeys indexValue =
  fmap decodeDenseKey [0 .. denseIndexCount indexValue - 1]

denseIndexKeyOf :: Ord value => value -> DenseIndex key value -> Maybe key
denseIndexKeyOf value =
  Map.lookup value . diLookup
{-# INLINE denseIndexKeyOf #-}

denseIndexKeyOfDenseValue :: (DenseKey value, Ord value) => value -> DenseIndex key value -> Maybe key
denseIndexKeyOfDenseValue value indexValue =
  maybe
    (denseIndexKeyOf value indexValue)
    (IntMap.lookup (encodeDenseKey value))
    (diDenseLookup indexValue)
{-# INLINE denseIndexKeyOfDenseValue #-}

denseIndexValueAt :: DenseKey key => key -> DenseIndex key value -> Maybe value
denseIndexValueAt key indexValue =
  let ordinal = encodeDenseKey key
   in if ordinal < 0
        then Nothing
        else diValues indexValue Vector.!? ordinal
{-# INLINE denseIndexValueAt #-}

denseIndexContains :: Ord value => value -> DenseIndex key value -> Bool
denseIndexContains value =
  Map.member value . diLookup

denseIndexKeyIntSet :: DenseKey key => [key] -> IntSet
denseIndexKeyIntSet =
  IntSet.fromList . fmap encodeDenseKey
