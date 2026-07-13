{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Obstruction.Cohomological.Modality.Standard
  ( SheafModalityTag (..),
    SheafModalityCoverage,
    mkSheafModalityCoverage,
    SheafModalityKey (..),
    sheafModalityTagFromKey,
    validateModalityCoverage,
    validateModalityCoverageWithEnvironmentKeys,
  )
where

import Data.Dependent.Map qualified as DMap
import Data.Dependent.Sum (DSum ((:=>)))
import Data.EqP (EqP (..))
import Data.GADT.Compare
  ( GCompare (..),
    GEq (..),
    GOrdering (..),
  )
import Data.Kind (Type)
import Data.OrdP (OrdP (..))
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:) (Refl))
import Moonlight.Sheaf.Obstruction.Cohomological.Core.Environment
  ( IndexedEnvironmentAlgebra,
    environmentBuilderKeys,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Modality
  ( ModalityRegistry,
    modalityRegistryKeys,
    modalityRegistryProjectionConflicts,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Section.Projection
  ( RelationProjectionConflict,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Types.Capability
  ( ModalityCoverage (..),
  )

type SheafModalityTag :: Type
data SheafModalityTag
  = EqualityModalityTag
  | GuardModalityTag
  | FactModalityTag
  | ProofModalityTag
  | CapabilityModalityTag
  deriving stock (Eq, Ord, Show, Read)

type SheafModalityCoverage :: Type
type SheafModalityCoverage =
  ModalityCoverage SheafModalityTag RelationProjectionConflict

mkSheafModalityCoverage ::
  [SheafModalityTag] ->
  [SheafModalityTag] ->
  [RelationProjectionConflict] ->
  SheafModalityCoverage
mkSheafModalityCoverage =
  ModalityCoverage

type SheafModalityKey :: Type -> Type -> (Type -> Type) -> (Type -> Type) -> Type -> Type -> Type -> Type
data SheafModalityKey equality guard fact proof capability runtime value where
  EqualityModalityKey ::
    SheafModalityKey equality guard fact proof capability runtime equality

  GuardModalityKey ::
    SheafModalityKey equality guard fact proof capability runtime guard

  FactModalityKey ::
    SheafModalityKey equality guard fact proof capability runtime (fact runtime)

  ProofModalityKey ::
    SheafModalityKey equality guard fact proof capability runtime (proof runtime)

  CapabilityModalityKey ::
    SheafModalityKey equality guard fact proof capability runtime capability

eqSheafModalityKey ::
  SheafModalityKey equality guard fact proof capability runtime left ->
  SheafModalityKey equality guard fact proof capability runtime right ->
  Bool
eqSheafModalityKey leftKey rightKey =
  case (leftKey, rightKey) of
    (EqualityModalityKey, EqualityModalityKey) -> True
    (GuardModalityKey, GuardModalityKey) -> True
    (FactModalityKey, FactModalityKey) -> True
    (ProofModalityKey, ProofModalityKey) -> True
    (CapabilityModalityKey, CapabilityModalityKey) -> True
    _ -> False

compareSheafModalityKey ::
  SheafModalityKey equality guard fact proof capability runtime left ->
  SheafModalityKey equality guard fact proof capability runtime right ->
  Ordering
compareSheafModalityKey leftKey rightKey =
  case (leftKey, rightKey) of
    (EqualityModalityKey, EqualityModalityKey) -> EQ
    (EqualityModalityKey, _) -> LT

    (GuardModalityKey, EqualityModalityKey) -> GT
    (GuardModalityKey, GuardModalityKey) -> EQ
    (GuardModalityKey, _) -> LT

    (FactModalityKey, EqualityModalityKey) -> GT
    (FactModalityKey, GuardModalityKey) -> GT
    (FactModalityKey, FactModalityKey) -> EQ
    (FactModalityKey, _) -> LT

    (ProofModalityKey, CapabilityModalityKey) -> LT
    (ProofModalityKey, ProofModalityKey) -> EQ
    (ProofModalityKey, _) -> GT

    (CapabilityModalityKey, CapabilityModalityKey) -> EQ
    (CapabilityModalityKey, _) -> GT

instance Eq (SheafModalityKey equality guard fact proof capability runtime value) where
  (==) =
    eqSheafModalityKey

instance Ord (SheafModalityKey equality guard fact proof capability runtime value) where
  compare =
    compareSheafModalityKey

instance EqP (SheafModalityKey equality guard fact proof capability runtime) where
  eqp =
    eqSheafModalityKey

instance OrdP (SheafModalityKey equality guard fact proof capability runtime) where
  comparep =
    compareSheafModalityKey

instance GEq (SheafModalityKey equality guard fact proof capability runtime) where
  geq leftKey rightKey =
    case (leftKey, rightKey) of
      (EqualityModalityKey, EqualityModalityKey) -> Just Refl
      (GuardModalityKey, GuardModalityKey) -> Just Refl
      (FactModalityKey, FactModalityKey) -> Just Refl
      (ProofModalityKey, ProofModalityKey) -> Just Refl
      (CapabilityModalityKey, CapabilityModalityKey) -> Just Refl
      _ -> Nothing

instance GCompare (SheafModalityKey equality guard fact proof capability runtime) where
  gcompare leftKey rightKey =
    case (leftKey, rightKey) of
      (EqualityModalityKey, EqualityModalityKey) -> GEQ
      (EqualityModalityKey, _) -> GLT

      (GuardModalityKey, EqualityModalityKey) -> GGT
      (GuardModalityKey, GuardModalityKey) -> GEQ
      (GuardModalityKey, _) -> GLT

      (FactModalityKey, EqualityModalityKey) -> GGT
      (FactModalityKey, GuardModalityKey) -> GGT
      (FactModalityKey, FactModalityKey) -> GEQ
      (FactModalityKey, _) -> GLT

      (ProofModalityKey, CapabilityModalityKey) -> GLT
      (ProofModalityKey, ProofModalityKey) -> GEQ
      (ProofModalityKey, _) -> GGT

      (CapabilityModalityKey, CapabilityModalityKey) -> GEQ
      (CapabilityModalityKey, _) -> GGT

sheafModalityTagFromKey ::
  DSum (SheafModalityKey equality guard fact proof capability runtime) witness ->
  SheafModalityTag
sheafModalityTagFromKey (modalityKey :=> _) =
  case modalityKey of
    EqualityModalityKey -> EqualityModalityTag
    GuardModalityKey -> GuardModalityTag
    FactModalityKey -> FactModalityTag
    ProofModalityKey -> ProofModalityTag
    CapabilityModalityKey -> CapabilityModalityTag

validateModalityCoverage ::
  GCompare key =>
  IndexedEnvironmentAlgebra request region occurrence guard key ->
  ModalityRegistry key anchor result ref ->
  (DSum key Proxy -> tag) ->
  ModalityCoverage tag RelationProjectionConflict
validateModalityCoverage environmentAlgebra modalityRegistry tagOf =
  validateModalityCoverageWithEnvironmentKeys
    (environmentBuilderKeys environmentAlgebra)
    modalityRegistry
    tagOf

validateModalityCoverageWithEnvironmentKeys ::
  GCompare key =>
  DMap.DMap key Proxy ->
  ModalityRegistry key anchor result ref ->
  (DSum key Proxy -> tag) ->
  ModalityCoverage tag RelationProjectionConflict
validateModalityCoverageWithEnvironmentKeys environmentKeys modalityRegistry tagOf =
  ModalityCoverage
    { smcMissingEnvironmentBindings =
        fmap
          tagOf
          (DMap.toAscList (DMap.difference (modalityRegistryKeys modalityRegistry) environmentKeys)),
      smcMissingRegisteredModalities =
        fmap
          tagOf
          (DMap.toAscList (DMap.difference environmentKeys (modalityRegistryKeys modalityRegistry))),
      smcProjectionConflicts =
        modalityRegistryProjectionConflicts modalityRegistry
    }
