{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Plan.Compile.Build
  ( QueryPlanInput (..),
    QueryPlanDomain (..),
    PlanOutputBinding (..),
    QueryPlanResidual (..),
    ResidualShape (..),
    queryPlanResidualGuard,
    queryPlanResidualIdentityDigest,
    queryPlanResidualShape,
    QueryPlanError (..),
    mkQueryPlan,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word (Word64)
import Moonlight.Core
  ( duplicateValuesOn,
  )
import Moonlight.Flow.Internal.Digest
  ( fingerprintWord64ToInt,
  )
import Moonlight.Flow.Plan.Query.Core
  ( AtomId,
    AtomSpec,
    OutputVar,
    QueryOutput (..),
    QueryPlan,
    QueryPlanDomain (..),
    SlotId,
    asQueryAtomId,
    asColumns,
    queryAtomAsAtomId,
    queryAtomKey,
    mkAtomId,
    mkQueryId,
    mkSlotId,
    slotIdKey,
    orderedSlotNub,
    unsafeMkQueryPlan,
  )
import Moonlight.Flow.Plan.Physical.Meta
  ( buildJoinMeta,
  )
import Moonlight.Flow.Plan.Residual
  ( QueryPlanResidual (..),
    ResidualShape (..),
    queryPlanResidualGuard,
    queryPlanResidualIdentityDigest,
    queryPlanResidualShape,
  )

type QueryPlanInput :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data QueryPlanInput compiled output guard tag tuple key = QueryPlanInput
  { qpiDomain :: !QueryPlanDomain,
    qpiCompiled :: !compiled,
    qpiDigest :: {-# UNPACK #-} !Word64,
    qpiAtoms :: !(Vector (AtomSpec tag tuple key)),
    qpiSchemaOrder :: !(Maybe (Vector SlotId)),
    qpiRootSlot :: {-# UNPACK #-} !SlotId,
    qpiOutputs :: ![PlanOutputBinding output key],
    qpiResidual :: !(QueryPlanResidual guard)
  }

type PlanOutputBinding :: Type -> Type -> Type
data PlanOutputBinding output key = PlanOutputBinding
  { pobSlot :: {-# UNPACK #-} !SlotId,
    pobVar :: !(OutputVar output key)
  }

type QueryPlanError :: Type
data QueryPlanError
  = QueryPlanEmptyAtoms
  | QueryPlanRootDomainNonEmptyAtoms
  | QueryPlanRootDomainSchemaSlotNotRoot !SlotId
  | QueryPlanRootDomainOutputSlotNotRoot !SlotId
  | QueryPlanDuplicateAtomId !AtomId
  | QueryPlanDuplicateAtomSlot !AtomId !SlotId
  | QueryPlanDuplicateFullSchemaSlot !SlotId
  | QueryPlanAtomSlotOutsideFullSchema !AtomId !SlotId
  | QueryPlanRootSlotOutsideFullSchema !SlotId
  | QueryPlanOutputSlotOutsideFullSchema !SlotId
  deriving stock (Eq, Ord, Show)

mkQueryPlan ::
  QueryOutput output key =>
  QueryPlanInput compiled output guard tag tuple key ->
  Either [QueryPlanError] (QueryPlan compiled output guard tag tuple key)
mkQueryPlan input =
  case validateQueryPlanInput input schemaOrder of
    [] ->
      let !fingerprint =
            fingerprintWord64ToInt (qpiDigest input)
          !atomSchemas =
            atomSchemasFromSpecs (qpiAtoms input)
          !fullSchema =
            Vector.toList schemaOrder
          !joinMeta =
            buildJoinMeta atomSchemas fullSchema
       in Right
            ( unsafeMkQueryPlan
                (qpiDomain input)
                (mkQueryId fingerprint)
                (qpiCompiled input)
                fingerprint
                (qpiAtoms input)
                schemaOrder
                (qpiRootSlot input)
                (outputBindingSlots (qpiOutputs input))
                (mkOutputRecipe (outputBindingVars (qpiOutputs input)))
                (qpiResidual input)
                joinMeta
            )
    errors ->
      Left errors
  where
    !schemaOrder =
      effectiveSchemaOrder input
{-# INLINE mkQueryPlan #-}

effectiveSchemaOrder ::
  QueryPlanInput compiled output guard tag tuple key ->
  Vector SlotId
effectiveSchemaOrder input =
  case qpiSchemaOrder input of
    Just schemaOrder ->
      schemaOrder
    Nothing ->
      inferredSchemaOrder
        (qpiAtoms input)
        (qpiRootSlot input)
        (qpiOutputs input)
{-# INLINE effectiveSchemaOrder #-}

validateQueryPlanInput ::
  QueryPlanInput compiled output guard tag tuple key ->
  Vector SlotId ->
  [QueryPlanError]
validateQueryPlanInput input schemaOrder =
  structuralAtomErrors
    <> rootDomainAtomErrors
    <> rootDomainSchemaErrors
    <> rootDomainOutputErrors
    <> duplicateAtomErrors
    <> duplicateAtomSlotErrors
    <> duplicateFullSchemaErrors
    <> atomOutsideSchemaErrors
    <> rootOutsideSchemaErrors
    <> outputOutsideSchemaErrors
  where
    atoms =
      Vector.toList (qpiAtoms input)

    fullSchemaKeys =
      slotKeySet schemaOrder

    structuralAtomErrors =
      [QueryPlanEmptyAtoms | qpiDomain input == StructuralQueryPlan, Vector.null (qpiAtoms input)]

    rootDomainAtomErrors =
      [QueryPlanRootDomainNonEmptyAtoms | qpiDomain input == RootDomainQueryPlan, not (Vector.null (qpiAtoms input))]

    rootDomainSchemaErrors =
      [ QueryPlanRootDomainSchemaSlotNotRoot schemaSlot
        | qpiDomain input == RootDomainQueryPlan,
          schemaSlot <- Vector.toList schemaOrder,
          schemaSlot /= qpiRootSlot input
      ]

    rootDomainOutputErrors =
      [ QueryPlanRootDomainOutputSlotNotRoot outputSlot
        | qpiDomain input == RootDomainQueryPlan,
          outputSlot <- fmap pobSlot (qpiOutputs input),
          outputSlot /= qpiRootSlot input
      ]

    duplicateAtomErrors =
      fmap
        (QueryPlanDuplicateAtomId . mkAtomId)
        (duplicateInts (fmap (queryAtomKey . asQueryAtomId) atoms))

    duplicateAtomSlotErrors =
      foldMap
        ( \atomSpec ->
            fmap
              (QueryPlanDuplicateAtomSlot (queryAtomAsAtomId (asQueryAtomId atomSpec)) . mkSlotId)
              (duplicateInts (fmap slotIdKey (Vector.toList (asColumns atomSpec))))
        )
        atoms

    duplicateFullSchemaErrors =
      fmap
        (QueryPlanDuplicateFullSchemaSlot . mkSlotId)
        (duplicateInts (fmap slotIdKey (Vector.toList schemaOrder)))

    atomOutsideSchemaErrors =
      foldMap
        ( \atomSpec ->
            fmap
              (QueryPlanAtomSlotOutsideFullSchema (queryAtomAsAtomId (asQueryAtomId atomSpec)))
              ( filter
                  (\slot -> IntSet.notMember (slotIdKey slot) fullSchemaKeys)
                  (Vector.toList (asColumns atomSpec))
              )
        )
        atoms

    rootOutsideSchemaErrors =
      [ QueryPlanRootSlotOutsideFullSchema (qpiRootSlot input)
        | IntSet.notMember (slotIdKey (qpiRootSlot input)) fullSchemaKeys
      ]

    outputOutsideSchemaErrors =
      fmap
        QueryPlanOutputSlotOutsideFullSchema
        ( filter
            (\slot -> IntSet.notMember (slotIdKey slot) fullSchemaKeys)
            (fmap pobSlot (qpiOutputs input))
        )
{-# INLINE validateQueryPlanInput #-}

inferredSchemaOrder ::
  Vector (AtomSpec tag tuple key) ->
  SlotId ->
  [PlanOutputBinding output key] ->
  Vector SlotId
inferredSchemaOrder atoms rootSlot outputBindings =
  Vector.fromList
    ( orderedSlotNub
        ( rootSlot
            : ( foldMap
                  (Vector.toList . asColumns)
                  (Vector.toList atoms)
                  <> fmap pobSlot outputBindings
              )
        )
    )
{-# INLINE inferredSchemaOrder #-}

outputBindingSlots :: [PlanOutputBinding output key] -> Vector SlotId
outputBindingSlots =
  Vector.fromList . fmap pobSlot
{-# INLINE outputBindingSlots #-}

outputBindingVars :: [PlanOutputBinding output key] -> [OutputVar output key]
outputBindingVars =
  fmap pobVar
{-# INLINE outputBindingVars #-}

atomSchemasFromSpecs ::
  Vector (AtomSpec tag tuple key) ->
  IntMap [SlotId]
atomSchemasFromSpecs =
  Vector.foldl'
    ( \acc atomSpec ->
        IntMap.insert
          (queryAtomKey (asQueryAtomId atomSpec))
          (Vector.toList (asColumns atomSpec))
          acc
    )
    IntMap.empty
{-# INLINE atomSchemasFromSpecs #-}

slotKeySet :: Vector SlotId -> IntSet
slotKeySet =
  Vector.foldl'
    (\acc slot -> IntSet.insert (slotIdKey slot) acc)
    IntSet.empty
{-# INLINE slotKeySet #-}

duplicateInts :: [Int] -> [Int]
duplicateInts =
  fmap snd . duplicateValuesOn id
{-# INLINE duplicateInts #-}
