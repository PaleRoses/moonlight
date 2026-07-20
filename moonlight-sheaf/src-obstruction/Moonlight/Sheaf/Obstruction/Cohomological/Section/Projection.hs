{-# LANGUAGE LambdaCase #-}

module Moonlight.Sheaf.Obstruction.Cohomological.Section.Projection
  ( SectionCoordinate (..),
    structuralCoordinate,
    relationCoordinate,
    RelationProjectionMode (..),
    RelationProjectionPolicy,
    RelationProjectionConflict (..),
    emptyRelationProjectionPolicy,
    relationProjectionPolicyFor,
    combineRelationProjectionPolicies,
    resolveRelationProjectionMode,
    separateRelationFlavors,
    SectionProjection,
    projectConstraintCoordinates,
    sectionCoordinateProjection,
    defaultSectionProjection
  )
where

import Data.Kind (Type)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Sheaf.Obstruction.Cohomological.Types
  ( ExactConstraint (..),
    RelationFlavor (..),
  )

type SectionCoordinate :: Type -> Type
data SectionCoordinate anchor
  = StructuralCoordinate !anchor
  | RelationCoordinate !RelationFlavor !anchor
  deriving stock (Eq, Ord, Show, Read)

structuralCoordinate :: anchor -> SectionCoordinate anchor
structuralCoordinate =
  StructuralCoordinate

relationCoordinate :: RelationFlavor -> anchor -> SectionCoordinate anchor
relationCoordinate =
  RelationCoordinate

type RelationProjectionMode :: Type
data RelationProjectionMode
  = StructuralProjection
  | RelationalProjection
  deriving stock (Eq, Ord, Show, Read)

type RelationProjectionPolicy :: Type
newtype RelationProjectionPolicy = RelationProjectionPolicy
  { unRelationProjectionPolicy :: Map RelationFlavor RelationProjectionMode
  }
  deriving stock (Eq, Show, Read)

type RelationProjectionConflict :: Type
data RelationProjectionConflict = RelationProjectionConflict
  { rpcFlavor :: !RelationFlavor,
    rpcExistingMode :: !RelationProjectionMode,
    rpcConflictingMode :: !RelationProjectionMode
  }
  deriving stock (Eq, Show, Read)

emptyRelationProjectionPolicy :: RelationProjectionPolicy
emptyRelationProjectionPolicy =
  RelationProjectionPolicy Map.empty

relationProjectionPolicyFor ::
  RelationFlavor ->
  RelationProjectionMode ->
  RelationProjectionPolicy
relationProjectionPolicyFor relationFlavor projectionMode =
  RelationProjectionPolicy
    (Map.singleton relationFlavor projectionMode)

combineRelationProjectionPolicies ::
  [RelationProjectionPolicy] ->
  Either [RelationProjectionConflict] RelationProjectionPolicy
combineRelationProjectionPolicies policies =
  let (combinedPolicy, conflicts) =
        List.foldl'
          (\(accumulatedPolicy, accumulatedConflicts) relationPolicy ->
             mergeRelationProjectionPolicy
               accumulatedPolicy
               accumulatedConflicts
               relationPolicy
          )
          (Map.empty, [])
          policies
   in if null conflicts
        then Right (RelationProjectionPolicy combinedPolicy)
        else Left (List.reverse conflicts)

resolveRelationProjectionMode :: RelationProjectionPolicy -> RelationFlavor -> RelationProjectionMode
resolveRelationProjectionMode relationPolicy relationFlavor =
  Map.findWithDefault StructuralProjection relationFlavor (unRelationProjectionPolicy relationPolicy)

separateRelationFlavors :: (RelationFlavor -> Bool) -> RelationProjectionPolicy
separateRelationFlavors isRelationalFlavor =
  RelationProjectionPolicy $
    Map.fromList $
      fmap
        ( \relationFlavor ->
            ( relationFlavor,
              if isRelationalFlavor relationFlavor
                then RelationalProjection
                else StructuralProjection
            )
        )
        [FactFlavor, ProvenanceFlavor, ProofFlavor, CapabilityFlavor]

type SectionProjection :: Type -> Type -> Type
newtype SectionProjection anchor coordinate = SectionProjection
  { projectConstraintCoordinates :: ExactConstraint anchor -> [coordinate]
  }

sectionCoordinateProjection :: RelationProjectionPolicy -> SectionProjection anchor (SectionCoordinate anchor)
sectionCoordinateProjection relationPolicy =
  SectionProjection $
    \case
      EqualityConstraint _ leftAnchor rightAnchor _ ->
        fmap StructuralCoordinate [leftAnchor, rightAnchor]
      GuardConstraint _ leftAnchor rightAnchor _ ->
        fmap StructuralCoordinate [leftAnchor, rightAnchor]
      RelationConstraint relationFlavor _ anchorValues _ ->
        fmap (projectRelationCoordinate relationFlavor) anchorValues
  where
    projectRelationCoordinate relationFlavor =
      case resolveRelationProjectionMode relationPolicy relationFlavor of
        StructuralProjection -> StructuralCoordinate
        RelationalProjection -> RelationCoordinate relationFlavor

defaultSectionProjection :: SectionProjection anchor (SectionCoordinate anchor)
defaultSectionProjection =
  sectionCoordinateProjection
    (separateRelationFlavors (== CapabilityFlavor))

mergeRelationProjectionPolicy ::
  Map RelationFlavor RelationProjectionMode ->
  [RelationProjectionConflict] ->
  RelationProjectionPolicy ->
  (Map RelationFlavor RelationProjectionMode, [RelationProjectionConflict])
mergeRelationProjectionPolicy accumulatedPolicy accumulatedConflicts relationPolicy =
  Map.foldlWithKey'
    (\(nextPolicy, nextConflicts) relationFlavor projectionMode ->
       case Map.lookup relationFlavor nextPolicy of
         Nothing ->
           (Map.insert relationFlavor projectionMode nextPolicy, nextConflicts)
         Just existingMode
           | existingMode == projectionMode ->
               (nextPolicy, nextConflicts)
           | otherwise ->
               ( nextPolicy,
                 RelationProjectionConflict relationFlavor existingMode projectionMode : nextConflicts
               )
    )
    (accumulatedPolicy, accumulatedConflicts)
    (unRelationProjectionPolicy relationPolicy)
