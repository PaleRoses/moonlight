module Moonlight.Rewrite.System.Logic.Delta
  ( differenceAlignedSetMap,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Semialign (alignWith)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.These (These (..))

differenceAlignedSetMap :: (Ord key, Ord value) => Map key (Set value) -> Map key (Set value) -> Map key (Set value)
differenceAlignedSetMap leftMap rightMap =
  Map.mapMaybe nonEmptySet (alignWith differenceAlignedSets leftMap rightMap)

differenceAlignedSets :: Ord value => These (Set value) (Set value) -> Set value
differenceAlignedSets =
  \case
    This leftValues ->
      leftValues
    That _ ->
      Set.empty
    These leftValues rightValues ->
      Set.difference leftValues rightValues

nonEmptySet :: Set value -> Maybe (Set value)
nonEmptySet values
  | Set.null values =
      Nothing
  | otherwise =
      Just values
