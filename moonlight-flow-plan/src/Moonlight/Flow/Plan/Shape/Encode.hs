{-# LANGUAGE DataKinds #-}

module Moonlight.Flow.Plan.Shape.Encode
  ( logicalPlanTermWords,
    logicalQueryShapeWords,

    factorShapePayloadWords,
    fragmentPayloadWords,
    projectionPayloadWords,
    projectionOperationWords,
    restrictionPayloadWords,
    coverPayloadWords,
    coverageTransformPayloadWords,
    coverageTransformPayloadDigest,

    queryPlanDomainWords,
    rawSlotWords,
    rawAtomTermWords,

    canonicalSlotWords,
    canonAtomWords,
    canonAtomMultisetWords,
    canonStalkRecipeWords,
    canonSlotSourceWords,

    intMapCanonSlotWords,
    setDigestWords,
    intSetWords,
    intMapIntSetWords,
    digestIntSet,
    digestIntMapIntSet,

    shapeMaybeWords,
    shapeListWords,
    shapeSetWords,
    shapeMapSetWords,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Data.Word
  ( Word64,
  )
import Moonlight.Flow.Internal.Digest
  ( wordOfInt,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( boundaryDigest,
  )
import Moonlight.Flow.Plan.Query.Core
  ( QueryPlanDomain,
    queryPlanDomainDigestWord,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
    stableDigest128,
    stableDigestWords,
  )
import Moonlight.Flow.Model.Schema.Digest.Words
  ( digestIntMapIntSetWords,
    digestIntSetWords,
    digestListWords,
    digestMapSetWords,
    digestMaybeWords,
    digestSetWords,
  )
import Moonlight.Flow.Plan.Residual
  ( residualShapeWords,
  )
import Moonlight.Flow.Plan.Shape
  ( CanonAtom (..),
    CanonAtomMultiset,
    CanonBagShape (..),
    CanonSeparator (..),
    FactorShapePayload (..),
    LogicalQueryShape (..),
  )
import Moonlight.Flow.Plan.Shape.Term
  ( CanonSlot (..),
    CanonSlotSource (..),
    CanonStalkRecipe (..),
    CoverPayload (..),
    CoverageTransformPayload (..),
    FragmentPayload (..),
    LogicalPlanTerm (..),
    PlanShape (..),
    ProjectionPayload (..),
    RawAtomTerm (..),
    RawSlot,
    RestrictionPayload (..),
    canonSlotKey,
    rawSlotKey,
  )

logicalPlanTermWords ::
  LogicalPlanTerm ->
  [Word64]
logicalPlanTermWords logical =
  [0x7261774c6f676963]
    <> queryPlanDomainWords (lptDomain logical)
    <> shapeListWords rawAtomTermWords (lptAtoms logical)
    <> rawSlotWords (lptRoot logical)
    <> shapeListWords rawSlotWords (lptOutputs logical)
    <> residualShapeWords (lptResidual logical)
{-# INLINE logicalPlanTermWords #-}

logicalQueryShapeWords ::
  LogicalQueryShape ->
  [Word64]
logicalQueryShapeWords shapeValue =
  [0x706c616e]
    <> queryPlanDomainWords (lqsDomain shapeValue)
    <> canonAtomMultisetWords (lqsAtoms shapeValue)
    <> canonicalSlotWords (lqsRoot shapeValue)
    <> shapeListWords canonicalSlotWords (lqsOutputs shapeValue)
    <> residualShapeWords (lqsResidual shapeValue)
{-# INLINE logicalQueryShapeWords #-}

factorShapePayloadWords ::
  FactorShapePayload ->
  [Word64]
factorShapePayloadWords payload =
  [fromInteger 0x666163746f725465726d]
    <> stableDigestWords (psDigest (fspPlan payload))
    <> stableDigestWords (psDigest (fspFragment payload))
    <> canonAtomMultisetWords (fspAtoms payload)
    <> shapeListWords canonicalSlotWords (fspSourceSchema payload)
    <> shapeListWords canonicalSlotWords (fspOutputSchema payload)
    <> maybeSeparatorWords (fspSeparator payload)
    <> stableDigestWords (boundaryDigest (fspBoundary payload))
    <> residualShapeWords (fspResidual payload)
{-# INLINE factorShapePayloadWords #-}

maybeSeparatorWords ::
  Maybe CanonSeparator ->
  [Word64]
maybeSeparatorWords maybeSeparator =
  case maybeSeparator of
    Nothing ->
      [0x00]
    Just separator ->
      [0x01]
        <> stableDigestWords (csepDigest separator)
        <> stableDigestWords (cbgDigest (csepChild separator))
        <> stableDigestWords (cbgDigest (csepParent separator))
        <> shapeListWords canonicalSlotWords (csepSlots separator)
{-# INLINE maybeSeparatorWords #-}

fragmentPayloadWords ::
  FragmentPayload ->
  [Word64]
fragmentPayloadWords fragment =
  case fragment of
    RootFragmentPayload digestValue ->
      [0x66726f6f74] <> stableDigestWords digestValue
    BagFragmentPayload bagDigest ->
      [0x66626167] <> stableDigestWords bagDigest
    SeparatorFragmentPayload separatorDigest childDigest parentDigest ->
      [0x66736570]
        <> stableDigestWords separatorDigest
        <> stableDigestWords childDigest
        <> stableDigestWords parentDigest
{-# INLINE fragmentPayloadWords #-}

projectionPayloadWords ::
  ProjectionPayload ->
  [Word64]
projectionPayloadWords payload =
  [0x70726f6a5465726d]
    <> stableDigestWords (ppSourceShape payload)
    <> stableDigestWords (ppTargetShape payload)
    <> projectionOperationWords payload
{-# INLINE projectionPayloadWords #-}

projectionOperationWords ::
  ProjectionPayload ->
  [Word64]
projectionOperationWords payload =
  [0x70726f6a4f70]
    <> shapeListWords canonicalSlotWords (ppSourceSchema payload)
    <> shapeListWords canonicalSlotWords (ppTargetSchema payload)
    <> intMapCanonSlotWords (ppSlotMap payload)
{-# INLINE projectionOperationWords #-}

restrictionPayloadWords ::
  RestrictionPayload ->
  [Word64]
restrictionPayloadWords payload =
  [0x7265737445726d]
    <> stableDigestWords (rpSourceShape payload)
    <> stableDigestWords (rpTargetShape payload)
    <> intMapIntSetWords (rpPinnedSlots payload)
{-# INLINE restrictionPayloadWords #-}

coverPayloadWords ::
  CoverPayload ->
  [Word64]
coverPayloadWords payload =
  [fromInteger 0x636f7665725465726d]
    <> stableDigestWords (cpFamilyDigest payload)
    <> stableDigestWords (cpTargetShape payload)
    <> setDigestWords (cpMembers payload)
{-# INLINE coverPayloadWords #-}

coverageTransformPayloadDigest ::
  CoverageTransformPayload ->
  StableDigest128
coverageTransformPayloadDigest =
  stableDigest128 . coverageTransformPayloadWords
{-# INLINE coverageTransformPayloadDigest #-}

coverageTransformPayloadWords ::
  CoverageTransformPayload ->
  [Word64]
coverageTransformPayloadWords payload =
  case payload of
    CoveragePreserveExact ->
      [fromInteger 0x636f765072657365727665]
    CoverageDowngradeLowerBound ->
      [0x636f764c6f776572]
    CoverageExactByCover proofDigest ->
      [fromInteger 0x636f7645786163744279436f766572]
        <> stableDigestWords proofDigest
    CoverageObstructedBy obstructionDigest ->
      [fromInteger 0x636f764f627374727563746564]
        <> stableDigestWords obstructionDigest
{-# INLINE coverageTransformPayloadWords #-}

queryPlanDomainWords ::
  QueryPlanDomain ->
  [Word64]
queryPlanDomainWords domain =
  [0x7175657279446f6d, queryPlanDomainDigestWord domain]
{-# INLINE queryPlanDomainWords #-}

rawAtomTermWords ::
  RawAtomTerm ->
  [Word64]
rawAtomTermWords atomValue =
  [ 0x72617741746f6d,
    wordOfInt (ratRawAtomKey atomValue),
    ratTagDigest atomValue
  ]
    <> shapeListWords rawSlotWords (ratColumns atomValue)
    <> canonStalkRecipeWords (ratRecipe atomValue)
{-# INLINE rawAtomTermWords #-}

rawSlotWords ::
  RawSlot ->
  [Word64]
rawSlotWords rawSlot =
  [0x40, wordOfInt (rawSlotKey rawSlot)]
{-# INLINE rawSlotWords #-}

canonicalSlotWords ::
  CanonSlot ->
  [Word64]
canonicalSlotWords slot =
  [0x01, wordOfInt (canonSlotKey slot)]
{-# INLINE canonicalSlotWords #-}

canonAtomWords ::
  CanonAtom ->
  [Word64]
canonAtomWords atomValue =
  [0x02, caTagDigest atomValue]
    <> shapeListWords canonicalSlotWords (caColumns atomValue)
    <> canonStalkRecipeWords (caRecipe atomValue)
{-# INLINE canonAtomWords #-}

canonAtomMultisetWords ::
  CanonAtomMultiset ->
  [Word64]
canonAtomMultisetWords atoms =
  [0x03, wordOfInt (Map.size atoms)]
    <> foldMap atomMultiplicityWords (Map.toAscList atoms)
  where
    atomMultiplicityWords (atomValue, multiplicity) =
      canonAtomWords atomValue <> [wordOfInt multiplicity]
{-# INLINE canonAtomMultisetWords #-}

canonStalkRecipeWords ::
  CanonStalkRecipe ->
  [Word64]
canonStalkRecipeWords (CanonStalkRecipe columns) =
  [0x04]
    <> shapeListWords (shapeListWords canonSlotSourceWords) columns
{-# INLINE canonStalkRecipeWords #-}

canonSlotSourceWords ::
  CanonSlotSource ->
  [Word64]
canonSlotSourceWords source =
  case source of
    CanonSourceResult ->
      [0x05]
    CanonSourceChild childIndex ->
      [0x06, wordOfInt childIndex]
{-# INLINE canonSlotSourceWords #-}

intMapCanonSlotWords ::
  IntMap CanonSlot ->
  [Word64]
intMapCanonSlotWords slotMap =
  [0x14, wordOfInt (IntMap.size slotMap)]
    <> IntMap.foldrWithKey consEntry [] slotMap
  where
    consEntry targetIndex sourceSlot acc =
      wordOfInt targetIndex : canonicalSlotWords sourceSlot <> acc
{-# INLINE intMapCanonSlotWords #-}

setDigestWords ::
  Set StableDigest128 ->
  [Word64]
setDigestWords digests =
  [0x15, wordOfInt (Set.size digests)]
    <> foldMap stableDigestWords (Set.toAscList digests)
{-# INLINE setDigestWords #-}

digestIntSet ::
  IntSet ->
  StableDigest128
digestIntSet =
  stableDigest128 . intSetWords
{-# INLINE digestIntSet #-}

digestIntMapIntSet ::
  IntMap IntSet ->
  StableDigest128
digestIntMapIntSet =
  stableDigest128 . intMapIntSetWords
{-# INLINE digestIntMapIntSet #-}

intSetWords ::
  IntSet ->
  [Word64]
intSetWords =
  digestIntSetWords 0x0f
{-# INLINE intSetWords #-}

intMapIntSetWords ::
  IntMap IntSet ->
  [Word64]
intMapIntSetWords =
  digestIntMapIntSetWords 0x10 0x0f
{-# INLINE intMapIntSetWords #-}

shapeMaybeWords ::
  (value -> [Word64]) ->
  Maybe value ->
  [Word64]
shapeMaybeWords =
  digestMaybeWords 0x11 0x12
{-# INLINE shapeMaybeWords #-}

shapeListWords ::
  (value -> [Word64]) ->
  [value] ->
  [Word64]
shapeListWords =
  digestListWords 0x13
{-# INLINE shapeListWords #-}

shapeSetWords ::
  (value -> [Word64]) ->
  Set value ->
  [Word64]
shapeSetWords =
  digestSetWords 0x12
{-# INLINE shapeSetWords #-}

shapeMapSetWords ::
  (key -> [Word64]) ->
  (value -> [Word64]) ->
  Map.Map key (Set value) ->
  [Word64]
shapeMapSetWords =
  digestMapSetWords 0x13 0x12
{-# INLINE shapeMapSetWords #-}
