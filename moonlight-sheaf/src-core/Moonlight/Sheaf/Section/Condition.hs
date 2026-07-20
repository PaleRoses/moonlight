module Moonlight.Sheaf.Section.Condition
  ( restrictionCheckEntry,
    restrictionConditionAssignmentEntry,
    nonEmptyEntry,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Sheaf.Section.Morphism
  ( Restriction,
    RestrictionCheck,
    checkRestriction,
    rSource,
    rTarget,
    restrictionMismatches,
  )
import Moonlight.Sheaf.Section.Stalk
  ( StalkAlgebra,
  )

restrictionCheckEntry ::
  key ->
  RestrictionCheck stalk mismatch ->
  Maybe (key, [mismatch])
restrictionCheckEntry key =
  nonEmptyEntry key . restrictionMismatches

restrictionConditionAssignmentEntry ::
  Ord cell =>
  (Restriction cell witness -> key) ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  Map cell stalk ->
  Restriction cell witness ->
  Maybe (key, [mismatch])
restrictionConditionAssignmentEntry keyForRestriction stalkAlgebra assignments restriction =
  case (Map.lookup (rSource restriction) assignments, Map.lookup (rTarget restriction) assignments) of
    (Just sourceValue, Just targetValue) ->
      restrictionCheckEntry
        (keyForRestriction restriction)
        (checkRestriction stalkAlgebra restriction sourceValue targetValue)
    _ ->
      Nothing

nonEmptyEntry :: key -> [value] -> Maybe (key, [value])
nonEmptyEntry key values =
  if null values
    then Nothing
    else Just (key, values)
