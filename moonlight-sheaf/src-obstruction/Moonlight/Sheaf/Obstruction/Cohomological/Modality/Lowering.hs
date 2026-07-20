{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Obstruction.Cohomological.Modality.Lowering
  ( equalityConstraintsBy,
    guardEqualityConstraintsBy,
    RelationConstraintBuild (..),
    RelationConstraintPlan (..),
    RelationLoweringAlgebra (..),
    lowerRelationConstraint,
    lowerRelationConstraintPlan,
    zipWithConstraintIds,
  )
where

import Data.Function ((&))
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Moonlight.Sheaf.Obstruction.Cohomological.Modality
  ( LoweringGap,
    data MissingReferenceGap,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Types
  ( Anchor (..),
    ConstraintId (..),
    ExactConstraint (..),
    ExactLabelCode,
    OccurrenceId,
    RelationFlavor,
    anchorDomain,
  )

equalityConstraintsBy ::
  Ord variable =>
  (occurrence -> OccurrenceId) ->
  (occurrence -> Maybe variable) ->
  Map OccurrenceId IntSet.IntSet ->
  [occurrence] ->
  ConstraintId ->
  ([ExactConstraint (Anchor OccurrenceId)], ConstraintId)
equalityConstraintsBy occurrenceIdOf occurrenceVariableOf occurrenceDomains occurrences startingId =
  List.foldl'
    stepGroup
    (startingId, [])
    groupedOccurrenceIds
    & \(nextConstraintId, constraintsRev) ->
      (reverse constraintsRev, nextConstraintId)
  where
    groupedOccurrenceIds =
      Map.elems $
        List.foldl'
          ( \groups occurrence ->
              case occurrenceVariableOf occurrence of
                Nothing ->
                  groups
                Just variable ->
                  Map.insertWith
                    (<>)
                    variable
                    [occurrenceIdOf occurrence]
                    groups
          )
          Map.empty
          occurrences

    stepGroup (nextConstraintId, constraintsRev) occurrenceIds =
      let pairings =
            zip occurrenceIds (drop 1 occurrenceIds)

          newConstraints =
            zipWith
              ( \offset (leftOccurrence, rightOccurrence) ->
                  EqualityConstraint
                    (ConstraintId (unConstraintId nextConstraintId + offset))
                    (OccurrenceAnchor leftOccurrence)
                    (OccurrenceAnchor rightOccurrence)
                    ( IntSet.intersection
                        (lookupDomain leftOccurrence)
                        (lookupDomain rightOccurrence)
                    )
              )
              [0 :: Int ..]
              pairings
       in ( ConstraintId (unConstraintId nextConstraintId + length newConstraints),
            List.foldl' (flip (:)) constraintsRev newConstraints
          )

    lookupDomain occurrenceId =
      Map.findWithDefault IntSet.empty occurrenceId occurrenceDomains

guardEqualityConstraintsBy ::
  Ord guard =>
  Int ->
  Map OccurrenceId IntSet.IntSet ->
  Map guard IntSet.IntSet ->
  (guard -> Maybe (ref, ref)) ->
  (ref -> Maybe (Anchor OccurrenceId)) ->
  [guard] ->
  ConstraintId ->
  ([ExactConstraint (Anchor OccurrenceId)], ConstraintId)
guardEqualityConstraintsBy rootKey occurrenceDomains guardDomains guardReferences anchorForReference guards startingId =
  zipWithConstraintIds
    startingId
    (\constraintId (_, leftAnchor, rightAnchor, supportDomain) ->
       GuardConstraint constraintId leftAnchor rightAnchor supportDomain)
    loweredGuards
  where
    loweredGuards =
      mapMaybe lowerGuard guards

    lowerGuard guard =
      case guardReferences guard of
        Nothing ->
          Nothing
        Just (leftReference, rightReference) -> do
          leftAnchor <- anchorForReference leftReference
          rightAnchor <- anchorForReference rightReference
          let guardDomain =
                Map.findWithDefault IntSet.empty guard guardDomains

              supportDomain =
                IntSet.intersection
                  guardDomain
                  ( IntSet.intersection
                      (anchorDomain rootKey occurrenceDomains leftAnchor)
                      (anchorDomain rootKey occurrenceDomains rightAnchor)
                  )
          if leftAnchor == rightAnchor && not (IntSet.null supportDomain)
            then Nothing
            else Just (guard, leftAnchor, rightAnchor, supportDomain)

type RelationConstraintBuild :: Type -> Type -> Type -> Type
data RelationConstraintBuild anchor ref origin
  = ExactRelation !(ExactConstraint anchor) !(Maybe origin)
  | UnsupportedRelation !ConstraintId ![ref]
  | TrivialRelation
  deriving stock (Eq, Show, Read)

type RelationConstraintPlan :: Type -> Type -> Type -> Type
data RelationConstraintPlan anchor ref origin = RelationConstraintPlan
  { rcpExactConstraints :: ![ExactConstraint anchor],
    rcpUnsupportedConstraints :: ![ConstraintId],
    rcpLoweringGaps :: ![LoweringGap anchor ref],
    rcpOrigins :: !(Map ConstraintId origin),
    rcpNextConstraintId :: !ConstraintId
  }
  deriving stock (Eq, Show, Read)

type RelationLoweringAlgebra :: Type -> Type -> Type -> Type -> Type
data RelationLoweringAlgebra item ref anchor origin = RelationLoweringAlgebra
  { rlaReferencesOf :: item -> Maybe [ref],
    rlaAnchorForReference :: ref -> Maybe anchor,
    rlaSupportActive :: item -> Bool,
    rlaSupportTuples :: item -> [anchor] -> [[ExactLabelCode]],
    rlaOriginOf :: item -> Maybe origin
  }

lowerRelationConstraint ::
  RelationFlavor ->
  RelationLoweringAlgebra item ref anchor origin ->
  ConstraintId ->
  item ->
  RelationConstraintBuild anchor ref origin
lowerRelationConstraint relationFlavor algebra constraintId item =
  case rlaReferencesOf algebra item of
    Nothing ->
      TrivialRelation
    Just references ->
      case traverse (rlaAnchorForReference algebra) references of
        Nothing ->
          UnsupportedRelation constraintId references
        Just anchors ->
          let active =
                rlaSupportActive algebra item

              supportTuples =
                if active
                  then rlaSupportTuples algebra item anchors
                  else []
           in case anchors of
                []
                  | active && not (null supportTuples) ->
                      TrivialRelation
                _ ->
                  ExactRelation
                    (RelationConstraint relationFlavor constraintId anchors supportTuples)
                    (rlaOriginOf algebra item)

lowerRelationConstraintPlan ::
  RelationFlavor ->
  RelationLoweringAlgebra item ref anchor origin ->
  ConstraintId ->
  [item] ->
  RelationConstraintPlan anchor ref origin
lowerRelationConstraintPlan relationFlavor algebra startingId items =
  let (builds, nextConstraintId) =
        zipWithConstraintIds
          startingId
          (lowerRelationConstraint relationFlavor algebra)
          items
   in RelationConstraintPlan
        { rcpExactConstraints =
            mapMaybe exactRelation builds,
          rcpUnsupportedConstraints =
            mapMaybe unsupportedRelation builds,
          rcpLoweringGaps =
            mapMaybe (unsupportedRelationGap relationFlavor) builds,
          rcpOrigins =
            relationConstraintOrigins builds,
          rcpNextConstraintId =
            nextConstraintId
        }

exactRelation ::
  RelationConstraintBuild anchor ref origin ->
  Maybe (ExactConstraint anchor)
exactRelation build =
  case build of
    ExactRelation constraintValue _ -> Just constraintValue
    UnsupportedRelation {} -> Nothing
    TrivialRelation -> Nothing

unsupportedRelation ::
  RelationConstraintBuild anchor ref origin ->
  Maybe ConstraintId
unsupportedRelation build =
  case build of
    UnsupportedRelation constraintId _ -> Just constraintId
    ExactRelation {} -> Nothing
    TrivialRelation -> Nothing

unsupportedRelationGap ::
  RelationFlavor ->
  RelationConstraintBuild anchor ref origin ->
  Maybe (LoweringGap anchor ref)
unsupportedRelationGap relationFlavor build =
  case build of
    UnsupportedRelation constraintId references ->
      Just (MissingReferenceGap relationFlavor constraintId references)
    ExactRelation {} -> Nothing
    TrivialRelation -> Nothing

relationConstraintOrigins ::
  [RelationConstraintBuild anchor ref origin] ->
  Map ConstraintId origin
relationConstraintOrigins =
  Map.fromList . mapMaybe originEntry
  where
    originEntry ::
      RelationConstraintBuild anchor ref origin ->
      Maybe (ConstraintId, origin)
    originEntry build =
      case build of
        ExactRelation (RelationConstraint _ constraintId _ _) (Just origin) ->
          Just (constraintId, origin)
        _ ->
          Nothing

zipWithConstraintIds ::
  ConstraintId ->
  (ConstraintId -> item -> result) ->
  [item] ->
  ([result], ConstraintId)
zipWithConstraintIds startingId make items =
  let results =
        zipWith
          (\offset item -> make (ConstraintId (unConstraintId startingId + offset)) item)
          [0 :: Int ..]
          items
   in (results, ConstraintId (unConstraintId startingId + length results))
