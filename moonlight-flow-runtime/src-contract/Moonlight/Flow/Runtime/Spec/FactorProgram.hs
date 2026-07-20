{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Moonlight.Flow.Runtime.Spec.FactorProgram
  ( RepairProgramKey (..),
    repairProgramKeyDigest,
    ErasedQueryPlanShape (..),
    FactorProgramSpec (..),
    FactorProgramError (..),
    compileFactorProgramSpec,
    validateFactorProgramSpec,
    factorProgramSpecRepairKey,
    factorProgramSpecQueryId,
    factorProgramSpecAtomKeys,
    factorProgramSpecAtomSourceMap,
    factorProgramSpecAtomSchemas,
    factorProgramSpecFactorNodes,
    factorProgramSpecShapeCompatible,
  )
where

import Control.Monad
  ( unless,
  )
import Data.Bifunctor
  ( first,
  )
import Data.Foldable
  ( traverse_,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Word
  ( Word64,
  )
import Moonlight.Flow.Execution.Subsumption.FactorShape
  ( FactorShapeError,
    FactorShapeManifest,
    compileFactorShapeManifest,
    factorShapeManifestNodes,
  )
import Moonlight.Flow.Internal.Digest
  ( wordOfInt,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
    stableDigest128,
  )
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Flow.Plan.Residual
  ( residualShapeWords,
  )
import Moonlight.Flow.Plan.Shape
  ( CanonicalizationResult (..),
    LogicalQueryShape (..),
  )
import Moonlight.Flow.Plan.Shape.CanonicalKey
  ( PlanCanonicalizationError,
    canonicalizationResultFromQueryPlanOutputErased,
  )
import Moonlight.Flow.Plan.Shape.Encode
  ( canonAtomMultisetWords,
    canonicalSlotWords,
    queryPlanDomainWords,
  )
import Moonlight.Flow.Plan.Shape.Term
  ( PlanShape (..),
  )

newtype RepairProgramKey = RepairProgramKey
  { unRepairProgramKey :: StableDigest128
  }
  deriving stock (Eq, Ord, Show, Read)

repairProgramKeyDigest :: RepairProgramKey -> StableDigest128
repairProgramKeyDigest =
  unRepairProgramKey
{-# INLINE repairProgramKeyDigest #-}

data ErasedQueryPlanShape = ErasedQueryPlanShape
  { eqpsPlanCacheKey :: !PlanCacheKey,
    eqpsRepairKey :: !RepairProgramKey,
    eqpsPlanClassDigest :: !StableDigest128
  }
  deriving stock (Eq, Ord, Show)

data FactorProgramSpec = FactorProgramSpec
  { fpsQueryPlan :: !ErasedQueryPlanShape,
    fpsCanonical :: !CanonicalizationResult,
    fpsFactorShapeManifest :: !FactorShapeManifest,
    fpsDecompPlan :: !DecompPlan
  }
  deriving stock (Eq, Show)

data FactorProgramError
  = FactorProgramPlanCanonicalizationFailed !PlanCanonicalizationError
  | FactorProgramErasedShapeMismatch !ErasedQueryPlanShape !ErasedQueryPlanShape
  | FactorProgramManifestCompileFailed !FactorShapeError
  | FactorProgramManifestMismatch !FactorShapeManifest !FactorShapeManifest
  | FactorProgramManifestNodeMissingInDecomp !FactorNode
  | FactorProgramDecompAtomMissingCanonicalOccurrence {-# UNPACK #-} !Int
  | FactorProgramDecompAtomMissingOwner {-# UNPACK #-} !Int
  | FactorProgramRepairKeyCollision !RepairProgramKey !FactorProgramSpec !FactorProgramSpec
  deriving stock (Eq, Show)

compileFactorProgramSpec ::
  QueryPlan compiled output guard tag tuple key ->
  DecompPlan ->
  Either FactorProgramError FactorProgramSpec
compileFactorProgramSpec plan decomp = do
  canonical <-
    first FactorProgramPlanCanonicalizationFailed $
      canonicalizationResultFromQueryPlanOutputErased plan
  manifest <-
    first FactorProgramManifestCompileFailed $
      compileFactorShapeManifest canonical decomp
  let spec =
        FactorProgramSpec
          { fpsQueryPlan =
              erasedQueryPlanShapeFromCanonical
                (queryPlanCacheKey plan)
                canonical
                decomp,
            fpsCanonical = canonical,
            fpsFactorShapeManifest = manifest,
            fpsDecompPlan = decomp
          }
  validateFactorProgramSpec spec
  pure spec
{-# INLINE compileFactorProgramSpec #-}

validateFactorProgramSpec ::
  FactorProgramSpec ->
  Either FactorProgramError ()
validateFactorProgramSpec spec = do
  validateErasedQueryPlanShapeSpec spec
  validateProgramSpecManifest spec
  validateSpecManifestNodesInDecomp spec
  validateSpecDecompAtomsCanonical spec
{-# INLINE validateFactorProgramSpec #-}

factorProgramSpecRepairKey ::
  FactorProgramSpec ->
  RepairProgramKey
factorProgramSpecRepairKey =
  eqpsRepairKey . fpsQueryPlan
{-# INLINE factorProgramSpecRepairKey #-}

factorProgramSpecQueryId :: FactorProgramSpec -> QueryId
factorProgramSpecQueryId =
  qpcdQueryId . pckDescriptor . eqpsPlanCacheKey . fpsQueryPlan
{-# INLINE factorProgramSpecQueryId #-}

factorProgramSpecAtomKeys :: FactorProgramSpec -> IntSet.IntSet
factorProgramSpecAtomKeys =
  decompAtomKeys . fpsDecompPlan
{-# INLINE factorProgramSpecAtomKeys #-}

factorProgramSpecAtomSourceMap ::
  FactorProgramSpec ->
  IntMap.IntMap SourceAtomId
factorProgramSpecAtomSourceMap =
  erasedQueryPlanAtomSourceMap . fpsQueryPlan
{-# INLINE factorProgramSpecAtomSourceMap #-}

factorProgramSpecAtomSchemas ::
  FactorProgramSpec ->
  IntMap.IntMap [SlotId]
factorProgramSpecAtomSchemas =
  erasedQueryPlanAtomSchemas . fpsQueryPlan
{-# INLINE factorProgramSpecAtomSchemas #-}

factorProgramSpecFactorNodes :: FactorProgramSpec -> [FactorNode]
factorProgramSpecFactorNodes =
  decompFactorNodes . fpsDecompPlan
{-# INLINE factorProgramSpecFactorNodes #-}

factorProgramSpecShapeCompatible ::
  FactorProgramSpec ->
  FactorProgramSpec ->
  Bool
factorProgramSpecShapeCompatible left right =
  fpsCanonical left == fpsCanonical right
    && fpsFactorShapeManifest left == fpsFactorShapeManifest right
    && fpsDecompPlan left == fpsDecompPlan right
{-# INLINE factorProgramSpecShapeCompatible #-}

validateProgramSpecManifest ::
  FactorProgramSpec ->
  Either FactorProgramError ()
validateProgramSpecManifest spec =
  let compiled =
        first
          FactorProgramManifestCompileFailed
          (compileFactorShapeManifest (fpsCanonical spec) (fpsDecompPlan spec))
   in compiled >>= \compiledManifest ->
        unless (compiledManifest == fpsFactorShapeManifest spec) $
          Left
            ( FactorProgramManifestMismatch
                (fpsFactorShapeManifest spec)
                compiledManifest
            )
{-# INLINE validateProgramSpecManifest #-}

validateErasedQueryPlanShapeSpec ::
  FactorProgramSpec ->
  Either FactorProgramError ()
validateErasedQueryPlanShapeSpec spec =
  let expected =
        erasedQueryPlanShapeFromCanonical
          (eqpsPlanCacheKey (fpsQueryPlan spec))
          (fpsCanonical spec)
          (fpsDecompPlan spec)
      actual =
        fpsQueryPlan spec
   in unless (actual == expected) $
        Left (FactorProgramErasedShapeMismatch expected actual)
{-# INLINE validateErasedQueryPlanShapeSpec #-}

erasedQueryPlanShapeFromCanonical ::
  PlanCacheKey ->
  CanonicalizationResult ->
  DecompPlan ->
  ErasedQueryPlanShape
erasedQueryPlanShapeFromCanonical planCacheKey canonical decomp =
  let !repairKey =
        repairProgramKeyFromCanonical canonical decomp
      !repairDigest =
        repairProgramKeyDigest repairKey
   in ErasedQueryPlanShape
        { eqpsPlanCacheKey = planCacheKey,
          eqpsRepairKey = repairKey,
          eqpsPlanClassDigest = repairDigest
        }
{-# INLINE erasedQueryPlanShapeFromCanonical #-}

repairProgramKeyFromCanonical ::
  CanonicalizationResult ->
  DecompPlan ->
  RepairProgramKey
repairProgramKeyFromCanonical canonical decomp =
  let !logicalShape =
        psPayload (crPlan canonical)
      !digestValue =
        stableDigest128
          ( [0x7265706169724b65, 0x79]
              <> queryPlanDomainWords (lqsDomain logicalShape)
              <> canonAtomMultisetWords (lqsAtoms logicalShape)
              <> canonicalSlotWords (lqsRoot logicalShape)
              <> residualShapeWords (crResidual canonical)
              <> decompPlanWords decomp
          )
   in RepairProgramKey digestValue
{-# INLINE repairProgramKeyFromCanonical #-}

decompPlanWords :: DecompPlan -> [Word64]
decompPlanWords decomp =
  [0x6465636f6d70]
    <> bagIdWords (dpRoot decomp)
    <> intMapWords decompBagWords (dpBags decomp)
    <> intMapWords bagIdWords (dpParent decomp)
    <> intMapWords (listWords bagIdWords) (dpChildren decomp)
    <> mapWords separatorKeyWords (listWords slotIdWords) (dpSeparator decomp)
    <> intMapWords bagIdWords (dpAtomOwner decomp)
{-# INLINE decompPlanWords #-}

decompBagWords :: DecompBag -> [Word64]
decompBagWords bag =
  bagIdWords (dbBagId bag)
    <> listWords slotIdWords (dbSlots bag)
    <> intSetWords (dbAtoms bag)
{-# INLINE decompBagWords #-}

separatorKeyWords :: (BagId, BagId) -> [Word64]
separatorKeyWords (child, parent) =
  bagIdWords child <> bagIdWords parent
{-# INLINE separatorKeyWords #-}

intMapWords :: (value -> [Word64]) -> IntMap.IntMap value -> [Word64]
intMapWords valueWords values =
  wordOfInt (IntMap.size values)
    : foldMap
      (\(key, value) -> wordOfInt key : valueWords value)
      (IntMap.toAscList values)
{-# INLINE intMapWords #-}

mapWords :: (key -> [Word64]) -> (value -> [Word64]) -> Map.Map key value -> [Word64]
mapWords keyWords valueWords values =
  wordOfInt (Map.size values)
    : foldMap
      (\(key, value) -> keyWords key <> valueWords value)
      (Map.toAscList values)
{-# INLINE mapWords #-}

bagIdWords :: BagId -> [Word64]
bagIdWords (BagId bagKey) =
  [wordOfInt bagKey]
{-# INLINE bagIdWords #-}

listWords :: (value -> [Word64]) -> [value] -> [Word64]
listWords valueWords values =
  wordOfInt (length values) : foldMap valueWords values
{-# INLINE listWords #-}

slotIdWords :: SlotId -> [Word64]
slotIdWords slot =
  [wordOfInt (slotIdKey slot)]
{-# INLINE slotIdWords #-}

intSetWords :: IntSet.IntSet -> [Word64]
intSetWords values =
  wordOfInt (IntSet.size values) : fmap wordOfInt (IntSet.toAscList values)
{-# INLINE intSetWords #-}

validateSpecManifestNodesInDecomp ::
  FactorProgramSpec ->
  Either FactorProgramError ()
validateSpecManifestNodesInDecomp spec =
  traverse_
    validateNode
    (fmap fst (factorShapeManifestNodes (fpsFactorShapeManifest spec)))
  where
    decompNodes =
      decompFactorNodes (fpsDecompPlan spec)

    validateNode node =
      unless (List.elem node decompNodes) $
        Left (FactorProgramManifestNodeMissingInDecomp node)
{-# INLINE validateSpecManifestNodesInDecomp #-}

validateSpecDecompAtomsCanonical ::
  FactorProgramSpec ->
  Either FactorProgramError ()
validateSpecDecompAtomsCanonical spec =
  IntSet.foldl'
    validateAtom
    (Right ())
    decompAtoms
  where
    decomp =
      fpsDecompPlan spec

    decompAtoms =
      factorProgramSpecAtomKeys spec

    canonicalAtomShapes =
      crAtomShapes (fpsCanonical spec)

    validateAtom eitherUnit atomKey = do
      eitherUnit
      unless (IntMap.member atomKey canonicalAtomShapes) $
        Left (FactorProgramDecompAtomMissingCanonicalOccurrence atomKey)
      unless (IntMap.member atomKey (dpAtomOwner decomp)) $
        Left (FactorProgramDecompAtomMissingOwner atomKey)
{-# INLINE validateSpecDecompAtomsCanonical #-}

erasedQueryPlanAtomSourceMap ::
  ErasedQueryPlanShape ->
  IntMap.IntMap SourceAtomId
erasedQueryPlanAtomSourceMap erased =
  IntMap.fromList
    [ (queryAtomKey (apdQueryAtomId descriptor), apdSourceAtomId descriptor)
    | descriptor <- qpcdAtoms (pckDescriptor (eqpsPlanCacheKey erased))
    ]
{-# INLINE erasedQueryPlanAtomSourceMap #-}

erasedQueryPlanAtomSchemas ::
  ErasedQueryPlanShape ->
  IntMap.IntMap [SlotId]
erasedQueryPlanAtomSchemas erased =
  IntMap.fromList
    [ (queryAtomKey (apdQueryAtomId descriptor), apdColumns descriptor)
    | descriptor <- qpcdAtoms (pckDescriptor (eqpsPlanCacheKey erased))
    ]
{-# INLINE erasedQueryPlanAtomSchemas #-}

decompAtomKeys :: DecompPlan -> IntSet.IntSet
decompAtomKeys decomp =
  IntMap.foldl'
    (\acc bag -> IntSet.union acc (dbAtoms bag))
    IntSet.empty
    (dpBags decomp)
{-# INLINE decompAtomKeys #-}

decompFactorNodes :: DecompPlan -> [FactorNode]
decompFactorNodes decomp =
  FactorNodeRoot
    : fmap
      (FactorNodeBag . BagId)
      (IntMap.keys (dpBags decomp))
    <> fmap
      (FactorNodeBagBelief . BagId)
      (IntMap.keys (dpBags decomp))
    <> [ FactorNodeSeparator child parent
       | ((child, parent), _separatorSlots) <- Map.toAscList (dpSeparator decomp)
       ]
{-# INLINE decompFactorNodes #-}
