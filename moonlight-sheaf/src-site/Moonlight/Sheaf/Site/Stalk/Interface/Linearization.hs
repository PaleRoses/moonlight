module Moonlight.Sheaf.Site.Stalk.Interface.Linearization
  ( LinearizedRestrictionModel,
    InterfaceStalkBasisAtom (..),
    buildLinearizedRestrictionModel,
    linearizedRestrictionComparableRestrictions,
    linearizedRestrictionStalkDimensions,
    interfaceStalkBasisAtoms,
    interfaceStalkBasisLinearization,
  )
where

import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Algebra (Semiring)
import Moonlight.Homology
  ( BoundaryIncidence,
    overlapBoundaryIncidence,
  )
import Moonlight.Sheaf.Section.Linearize
  ( StalkLinearization (..),
  )
import Moonlight.Sheaf.Site.Interface.Types
  ( InterfaceName,
  )
import Moonlight.Sheaf.Site.Stalk.Interface
  ( InterfaceStalk (..),
    WitnessClass (..),
    witnessClass,
  )

type LinearizedRestrictionModel :: Type -> Type -> Type
data LinearizedRestrictionModel node r = LinearizedRestrictionModel
  { linearizedRestrictionStalkDimensionsInternal :: Map.Map node Int,
    linearizedRestrictionComparableRestrictionsInternal :: Map.Map (node, node) (BoundaryIncidence r)
  }

linearizedRestrictionStalkDimensions ::
  LinearizedRestrictionModel node r ->
  Map.Map node Int
linearizedRestrictionStalkDimensions =
  linearizedRestrictionStalkDimensionsInternal

linearizedRestrictionComparableRestrictions ::
  LinearizedRestrictionModel node r ->
  Map.Map (node, node) (BoundaryIncidence r)
linearizedRestrictionComparableRestrictions =
  linearizedRestrictionComparableRestrictionsInternal

buildLinearizedRestrictionModel ::
  Ord node =>
  Map.Map node stalk ->
  (node -> node -> Bool) ->
  StalkLinearization stalk r ->
  LinearizedRestrictionModel node r
buildLinearizedRestrictionModel stalksByNode comparable linearization =
  LinearizedRestrictionModel
    { linearizedRestrictionStalkDimensionsInternal =
        Map.map (slStalkDimension linearization) stalksByNode,
      linearizedRestrictionComparableRestrictionsInternal =
        Map.fromList
          [ ( (upperNode, lowerNode),
              slRestrictionIncidence linearization upperStalk lowerStalk
            )
          | (upperNode, upperStalk) <- Map.toList stalksByNode,
            (lowerNode, lowerStalk) <- Map.toList stalksByNode,
            comparable upperNode lowerNode
          ]
    }

type InterfaceStalkBasisAtom :: Type -> Type
data InterfaceStalkBasisAtom tag
  = BoundNameAtom (InterfaceName tag)
  | DeletedNameAtom (InterfaceName tag)
  | CreatedNameAtom (InterfaceName tag)
  | GuardedAtom
  | WitnessAtom WitnessClass
  deriving stock (Eq, Ord, Show)

interfaceStalkBasisAtoms :: InterfaceStalk tag -> [InterfaceStalkBasisAtom tag]
interfaceStalkBasisAtoms stalkValue =
  fmap BoundNameAtom (Set.toAscList (rsBoundNames stalkValue))
    <> fmap DeletedNameAtom (Set.toAscList (rsDeletedNames stalkValue))
    <> fmap CreatedNameAtom (Set.toAscList (rsCreatedNames stalkValue))
    <> [GuardedAtom | rsGuarded stalkValue]
    <> [WitnessAtom (witnessClass (rsWitness stalkValue))]

interfaceStalkBasisLinearization ::
  (Eq r, Num r, Semiring r) =>
  StalkLinearization (InterfaceStalk tag) r
interfaceStalkBasisLinearization =
  StalkLinearization
    { slStalkDimension = length . interfaceStalkBasisAtoms,
      slRestrictionIncidence =
        \sourceStalk targetStalk ->
          overlapBoundaryIncidence 1
            (interfaceStalkBasisAtoms sourceStalk)
            (interfaceStalkBasisAtoms targetStalk)
    }
