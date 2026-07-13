{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Model.Schema.Morphism
  ( SchemaProjection,
    spSourceVisibleSchema,
    spTargetVisibleSchema,
    spSourceCanonicalSchema,
    spTargetCanonicalSchema,
    spTargetToSource,

    ProjectionProfile (..),
    BoundaryProjectionProfile (..),
    SchemaProjectionError (..),
    BoundaryProjection (..),
    BoundaryProjectionError (..),

    compileSchemaProjection,
    compileSchemaProjectionByCanonicalSchema,
    compileSchemaProjectionByCanonicalMap,

    projectionProfile,
    projectionProfileWith,

    projectRelationalBoundaryWithProfile,
    projectAtomRow,
    projectAtomRowMapExact,
    projectRowDeltaExact,
    projectRelationalBoundaryExact,
    projectRelationalBoundary,

    projectionIsomorphic,
    sourceToTargetSlotMap,
  )
where

import Control.Monad
  ( foldM,
    unless,
  )
import Data.Bifunctor
  ( first,
  )
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Word
  ( Word64,
  )
import Moonlight.Core
  ( note,
    safeIndex,
  )
import Moonlight.Core
  ( duplicateValuesOn,
  )
import Moonlight.Core
  ( SlotId,
    mkSlotId,
    slotIdKey,
  )
import Moonlight.Flow.Internal.Digest
  ( wordOfInt,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta,
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchChangeMap,
    plainRowPatchFromChangeMap,
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    tupleKeyFromRepKeys,
    tupleKeyIndex,
    tupleKeyWidth,
  )
import Moonlight.Flow.Model.Schema
  ( Schema,
    SchemaError,
    mkSchema,
    schemaIndex,
    schemaSlots,
    schemaWidth,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( BoundaryShape (..),
    RuntimeBoundary,
    RuntimeBoundaryError,
    boundaryShape,
    mkRuntimeBoundary,
    runtimeBoundarySensitiveSlots,
    runtimeBoundarySlotKeys,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128 (..),
    stableDigest128,
    stableDigestWords,
  )
import Moonlight.Flow.Model.Schema.Digest.Words
  ( digestIntMapWords,
    digestIntSetWords,
    digestListWords,
  )

type SchemaProjection :: Type -> Type -> Type
data SchemaProjection visible canonical = SchemaProjection
  { spSourceVisibleSchema :: !(Schema visible),
    spTargetVisibleSchema :: !(Schema visible),
    spSourceCanonicalSchema :: !(Schema canonical),
    spTargetCanonicalSchema :: !(Schema canonical),
    spTargetToSource :: !(IntMap Int)
  }
  deriving stock (Eq, Ord, Show, Read)

type ProjectionProfile :: Type -> Type
data ProjectionProfile canonical = ProjectionProfile
  { ppSourceSchema :: ![canonical],
    ppTargetSchema :: ![canonical],
    ppProjectedColumns :: !(IntMap Int),
    ppDroppedColumns :: !IntSet.IntSet,
    ppDuplicatedTargets :: !IntSet.IntSet,
    ppSensitiveCollision :: !Bool,
    ppBoundaryExact :: !Bool,
    ppCoverageRuleDigest :: !StableDigest128,
    ppDigest :: !StableDigest128
  }
  deriving stock (Eq, Ord, Show, Read)

type BoundaryProjectionProfile :: Type
data BoundaryProjectionProfile = BoundaryProjectionProfile
  { bppProjectedSensitiveSlots :: !IntSet.IntSet,
    bppForgottenSensitiveSlots :: !IntSet.IntSet,
    bppBoundaryExact :: !Bool,
    bppSensitiveCollision :: !Bool,
    bppDigest :: !StableDigest128
  }
  deriving stock (Eq, Ord, Show, Read)

type BoundaryProjection :: Type -> Type
newtype BoundaryProjection canonical = BoundaryProjection
  { bpSchemaProjection :: SchemaProjection SlotId canonical
  }
  deriving stock (Eq, Ord, Show, Read)

type BoundaryProjectionError :: Type
data BoundaryProjectionError
  = BoundaryProjectionSourceSchemaMismatch ![SlotId] ![SlotId]
  | BoundaryProjectionForgottenSensitiveSlot !SlotId
  | BoundaryProjectionBoundaryError !RuntimeBoundaryError
  deriving stock (Eq, Ord, Show)

type SchemaProjectionError :: Type -> Type -> Type
data SchemaProjectionError visible canonical
  = ProjectionSourceVisibleSchemaError !(SchemaError visible)
  | ProjectionTargetVisibleSchemaError !(SchemaError visible)
  | ProjectionSourceCanonicalSchemaError !(SchemaError canonical)
  | ProjectionTargetCanonicalSchemaError !(SchemaError canonical)
  | ProjectionMissingTargetVisibleSlot !visible
  | ProjectionDuplicateSourceCanonicalSlot !canonical
  | ProjectionDuplicateTargetCanonicalSlot !canonical
  | ProjectionMissingTargetCanonicalSlot !canonical
  | ProjectionSourceCanonicalSchemaWidthMismatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | ProjectionTargetCanonicalSchemaWidthMismatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | ProjectionRowWidthMismatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | ProjectionRowCollision !RowTupleKey !RowTupleKey !RowTupleKey
  | ProjectionBoundarySchemaMismatch ![SlotId] ![SlotId]
  | ProjectionForgottenSensitiveSlot !SlotId
  | ProjectionBoundaryError !RuntimeBoundaryError
  | ProjectionMissingTargetIndex {-# UNPACK #-} !Int
  | ProjectionMissingCanonicalMapTarget !canonical
  | ProjectionMappedSourceCanonicalSlotNotVisible !canonical !canonical
  deriving stock (Eq, Ord, Show)

compileSchemaProjection ::
  (Ord visible, Ord canonical) =>
  (visible -> canonical) ->
  [visible] ->
  [visible] ->
  Either (SchemaProjectionError visible canonical) (SchemaProjection visible canonical)
compileSchemaProjection fallbackCanonical sourceVisible targetVisible = do
  let sourceCanonical =
        fmap fallbackCanonical sourceVisible
      targetCanonical =
        fmap fallbackCanonical targetVisible

  sourceVisibleSchema <-
    first ProjectionSourceVisibleSchemaError (mkSchema sourceVisible)
  targetVisibleSchema <-
    first ProjectionTargetVisibleSchemaError (mkSchema targetVisible)
  sourceCanonicalSchema <-
    first ProjectionSourceCanonicalSchemaError (mkSchema sourceCanonical)
  targetCanonicalSchema <-
    first ProjectionTargetCanonicalSchemaError (mkSchema targetCanonical)

  targetToSource <-
    buildVisibleIndexMap
      (schemaIndex sourceVisibleSchema)
      targetVisible

  pure
    SchemaProjection
      { spSourceVisibleSchema = sourceVisibleSchema,
        spTargetVisibleSchema = targetVisibleSchema,
        spSourceCanonicalSchema = sourceCanonicalSchema,
        spTargetCanonicalSchema = targetCanonicalSchema,
        spTargetToSource = targetToSource
      }
{-# INLINE compileSchemaProjection #-}

compileSchemaProjectionByCanonicalSchema ::
  (Ord visible, Ord canonical) =>
  (canonical -> Int) ->
  [canonical] ->
  [visible] ->
  [canonical] ->
  [visible] ->
  Either (SchemaProjectionError visible canonical) (SchemaProjection visible canonical)
compileSchemaProjectionByCanonicalSchema canonicalKey sourceCanonical sourceVisible targetCanonical targetVisible = do
  projection <-
    checkedProjectionShell
      sourceCanonical
      sourceVisible
      targetCanonical
      targetVisible
      IntMap.empty

  sourceCanonicalIndex <-
    keyedUniqueIndex
      canonicalKey
      ProjectionDuplicateSourceCanonicalSlot
      (schemaSlots (spSourceCanonicalSchema projection))

  _targetCanonicalIndex <-
    keyedUniqueIndex
      canonicalKey
      ProjectionDuplicateTargetCanonicalSlot
      (schemaSlots (spTargetCanonicalSchema projection))

  targetToSource <-
    buildCanonicalIndexMap
      canonicalKey
      sourceCanonicalIndex
      (schemaSlots (spTargetCanonicalSchema projection))

  pure projection {spTargetToSource = targetToSource}
{-# INLINE compileSchemaProjectionByCanonicalSchema #-}

compileSchemaProjectionByCanonicalMap ::
  (Ord visible, Ord canonical) =>
  (canonical -> Int) ->
  IntMap canonical ->
  [canonical] ->
  [visible] ->
  [canonical] ->
  [visible] ->
  Either (SchemaProjectionError visible canonical) (SchemaProjection visible canonical)
compileSchemaProjectionByCanonicalMap canonicalKey targetToSourceCanonical sourceCanonical sourceVisible targetCanonical targetVisible = do
  projection <-
    checkedProjectionShell
      sourceCanonical
      sourceVisible
      targetCanonical
      targetVisible
      IntMap.empty

  sourceCanonicalIndex <-
    keyedUniqueIndex
      canonicalKey
      ProjectionDuplicateSourceCanonicalSlot
      (schemaSlots (spSourceCanonicalSchema projection))

  _targetCanonicalIndex <-
    keyedUniqueIndex
      canonicalKey
      ProjectionDuplicateTargetCanonicalSlot
      (schemaSlots (spTargetCanonicalSchema projection))

  targetToSource <-
    buildMappedCanonicalIndexMap
      canonicalKey
      targetToSourceCanonical
      sourceCanonicalIndex
      (schemaSlots (spTargetCanonicalSchema projection))

  pure projection {spTargetToSource = targetToSource}
{-# INLINE compileSchemaProjectionByCanonicalMap #-}

checkedProjectionShell ::
  (Ord visible, Ord canonical) =>
  [canonical] ->
  [visible] ->
  [canonical] ->
  [visible] ->
  IntMap Int ->
  Either (SchemaProjectionError visible canonical) (SchemaProjection visible canonical)
checkedProjectionShell sourceCanonical sourceVisible targetCanonical targetVisible targetToSource = do
  unless (length sourceCanonical == length sourceVisible) $
    Left (ProjectionSourceCanonicalSchemaWidthMismatch (length sourceCanonical) (length sourceVisible))
  unless (length targetCanonical == length targetVisible) $
    Left (ProjectionTargetCanonicalSchemaWidthMismatch (length targetCanonical) (length targetVisible))

  sourceVisibleSchema <-
    first ProjectionSourceVisibleSchemaError (mkSchema sourceVisible)
  targetVisibleSchema <-
    first ProjectionTargetVisibleSchemaError (mkSchema targetVisible)
  sourceCanonicalSchema <-
    first ProjectionSourceCanonicalSchemaError (mkSchema sourceCanonical)
  targetCanonicalSchema <-
    first ProjectionTargetCanonicalSchemaError (mkSchema targetCanonical)

  pure
    SchemaProjection
      { spSourceVisibleSchema = sourceVisibleSchema,
        spTargetVisibleSchema = targetVisibleSchema,
        spSourceCanonicalSchema = sourceCanonicalSchema,
        spTargetCanonicalSchema = targetCanonicalSchema,
        spTargetToSource = targetToSource
      }
{-# INLINE checkedProjectionShell #-}

buildVisibleIndexMap ::
  Ord visible =>
  Map visible Int ->
  [visible] ->
  Either (SchemaProjectionError visible canonical) (IntMap Int)
buildVisibleIndexMap sourceIndex =
  fmap IntMap.fromAscList . traverse readTarget . zip [0 :: Int ..]
  where
    readTarget (targetIx, targetSlot) = do
      sourceIx <-
        note
          (ProjectionMissingTargetVisibleSlot targetSlot)
          (Map.lookup targetSlot sourceIndex)
      pure (targetIx, sourceIx)
{-# INLINE buildVisibleIndexMap #-}

keyedUniqueIndex ::
  (slot -> Int) ->
  (slot -> SchemaProjectionError visible canonical) ->
  [slot] ->
  Either (SchemaProjectionError visible canonical) (IntMap Int)
keyedUniqueIndex keyOf mkDuplicate slots =
  case duplicateValuesOn keyOf slots of
    (_, duplicateSlot) : _ ->
      Left (mkDuplicate duplicateSlot)
    [] ->
      Right
        ( IntMap.fromList
            [ (keyOf slot, ix)
            | (ix, slot) <- zip [0 :: Int ..] slots
            ]
        )
{-# INLINE keyedUniqueIndex #-}

buildCanonicalIndexMap ::
  (canonical -> Int) ->
  IntMap Int ->
  [canonical] ->
  Either (SchemaProjectionError visible canonical) (IntMap Int)
buildCanonicalIndexMap canonicalKey sourceCanonicalIndex =
  fmap IntMap.fromAscList . traverse readTarget . zip [0 :: Int ..]
  where
    readTarget (targetIx, targetCanonicalSlot) = do
      sourceIx <-
        note
          (ProjectionMissingTargetCanonicalSlot targetCanonicalSlot)
          (IntMap.lookup (canonicalKey targetCanonicalSlot) sourceCanonicalIndex)
      pure (targetIx, sourceIx)
{-# INLINE buildCanonicalIndexMap #-}

buildMappedCanonicalIndexMap ::
  (canonical -> Int) ->
  IntMap canonical ->
  IntMap Int ->
  [canonical] ->
  Either (SchemaProjectionError visible canonical) (IntMap Int)
buildMappedCanonicalIndexMap canonicalKey targetToSourceCanonical sourceCanonicalIndex =
  fmap IntMap.fromAscList . traverse readTarget . zip [0 :: Int ..]
  where
    readTarget (targetIx, targetCanonicalSlot) = do
      sourceCanonicalSlot <-
        note
          (ProjectionMissingCanonicalMapTarget targetCanonicalSlot)
          (IntMap.lookup (canonicalKey targetCanonicalSlot) targetToSourceCanonical)
      sourceIx <-
        note
          (ProjectionMappedSourceCanonicalSlotNotVisible targetCanonicalSlot sourceCanonicalSlot)
          (IntMap.lookup (canonicalKey sourceCanonicalSlot) sourceCanonicalIndex)
      pure (targetIx, sourceIx)
{-# INLINE buildMappedCanonicalIndexMap #-}

projectionProfile ::
  (canonical -> Int) ->
  (canonical -> [Word64]) ->
  SchemaProjection visible canonical ->
  ProjectionProfile canonical
projectionProfile canonicalKey canonicalWords =
  projectionProfileWith
    canonicalKey
    canonicalWords
    unspecifiedCoverageDigest
    False
    False
{-# INLINE projectionProfile #-}

projectionProfileWith ::
  (canonical -> Int) ->
  (canonical -> [Word64]) ->
  StableDigest128 ->
  Bool ->
  Bool ->
  SchemaProjection visible canonical ->
  ProjectionProfile canonical
projectionProfileWith canonicalKey canonicalWords coverageDigest boundaryExact sensitiveCollision projection =
  let usedSourceColumns =
        IntSet.fromList (IntMap.elems (spTargetToSource projection))
      sourceColumnUniverse =
        IntSet.fromAscList [0 .. schemaWidth (spSourceVisibleSchema projection) - 1]
      droppedColumns =
        IntSet.difference sourceColumnUniverse usedSourceColumns
      duplicatedTargets =
        duplicateCanonicalKeys canonicalKey (schemaSlots (spTargetCanonicalSchema projection))
      profile0 =
        ProjectionProfile
          { ppSourceSchema = schemaSlots (spSourceCanonicalSchema projection),
            ppTargetSchema = schemaSlots (spTargetCanonicalSchema projection),
            ppProjectedColumns = spTargetToSource projection,
            ppDroppedColumns = droppedColumns,
            ppDuplicatedTargets = duplicatedTargets,
            ppSensitiveCollision = sensitiveCollision,
            ppBoundaryExact = boundaryExact,
            ppCoverageRuleDigest = coverageDigest,
            ppDigest = StableDigest128 0 0
          }
   in profile0 {ppDigest = projectionProfileDigest canonicalWords profile0}
{-# INLINE projectionProfileWith #-}

projectionProfileDigest ::
  (canonical -> [Word64]) ->
  ProjectionProfile canonical ->
  StableDigest128
projectionProfileDigest canonicalWords profile =
  stableDigest128
    ( [0x70726f66696c65]
        <> digestListWords 0x13 canonicalWords (ppSourceSchema profile)
        <> digestListWords 0x13 canonicalWords (ppTargetSchema profile)
        <> digestIntMapWords 0x16 (\mapped -> [wordOfInt mapped]) (ppProjectedColumns profile)
        <> digestIntSetWords 0x0f (ppDroppedColumns profile)
        <> digestIntSetWords 0x0f (ppDuplicatedTargets profile)
        <> [ if ppSensitiveCollision profile then 1 else 0,
             if ppBoundaryExact profile then 1 else 0
           ]
        <> stableDigestWords (ppCoverageRuleDigest profile)
    )
{-# INLINE projectionProfileDigest #-}

projectRelationalBoundaryWithProfile ::
  BoundaryProjection canonical ->
  RuntimeBoundary ->
  Either BoundaryProjectionError (RuntimeBoundary, BoundaryProjectionProfile)
projectRelationalBoundaryWithProfile boundaryProjection@(BoundaryProjection projection) boundary = do
  projectedBoundary <-
    projectRelationalBoundary boundaryProjection boundary

  let sourceSensitive =
        runtimeBoundarySensitiveSlots boundary
      projectedSensitive =
        runtimeBoundarySensitiveSlots projectedBoundary
      sourceToTarget =
        sourceToTargetSlotMap projection
      forgottenSensitive =
        IntSet.filter
          (`IntMap.notMember` sourceToTarget)
          sourceSensitive
      boundaryExact =
        projectedBoundary == boundary
      sensitiveCollision =
        IntSet.size projectedSensitive < IntSet.size sourceSensitive
      profile0 =
        BoundaryProjectionProfile
          { bppProjectedSensitiveSlots = projectedSensitive,
            bppForgottenSensitiveSlots = forgottenSensitive,
            bppBoundaryExact = boundaryExact,
            bppSensitiveCollision = sensitiveCollision,
            bppDigest = StableDigest128 0 0
          }

  pure
    ( projectedBoundary,
      profile0 {bppDigest = boundaryProjectionProfileDigest profile0}
    )
{-# INLINE projectRelationalBoundaryWithProfile #-}

boundaryProjectionProfileDigest ::
  BoundaryProjectionProfile ->
  StableDigest128
boundaryProjectionProfileDigest profile =
  stableDigest128
    ( [fromInteger (0x626f756e6450726f66696c65 :: Integer)]
        <> digestIntSetWords 0x0f (bppProjectedSensitiveSlots profile)
        <> digestIntSetWords 0x0f (bppForgottenSensitiveSlots profile)
        <> [ if bppBoundaryExact profile then 1 else 0,
             if bppSensitiveCollision profile then 1 else 0
           ]
    )
{-# INLINE boundaryProjectionProfileDigest #-}

projectAtomRow ::
  SchemaProjection visible canonical ->
  RowTupleKey ->
  Either (SchemaProjectionError visible canonical) RowTupleKey
projectAtomRow projection row
  | tupleKeyWidth row /= sourceWidth =
      Left (ProjectionRowWidthMismatch sourceWidth (tupleKeyWidth row))
  | otherwise =
      tupleKeyFromRepKeys <$> traverse readTargetIndex [0 .. targetWidth - 1]
  where
    sourceWidth =
      schemaWidth (spSourceVisibleSchema projection)

    targetWidth =
      schemaWidth (spTargetVisibleSchema projection)

    readTargetIndex targetIx = do
      sourceIx <-
        note
          (ProjectionMissingTargetIndex targetIx)
          (IntMap.lookup targetIx (spTargetToSource projection))
      note
        (ProjectionRowWidthMismatch sourceWidth (tupleKeyWidth row))
        (tupleKeyIndex row sourceIx)
{-# INLINE projectAtomRow #-}

projectAtomRowMapExact ::
  SchemaProjection visible canonical ->
  (payload -> payload') ->
  Map RowTupleKey payload ->
  Either (SchemaProjectionError visible canonical) (Map RowTupleKey payload')
projectAtomRowMapExact projection transformPayload =
  fmap finalize
    . Map.foldlWithKey'
      step
      (Right Map.empty)
  where
    step eitherAcc sourceRow payload = do
      acc <- eitherAcc
      targetRow <- projectAtomRow projection sourceRow
      case Map.lookup targetRow acc of
        Nothing ->
          Right (Map.insert targetRow (sourceRow, transformPayload payload) acc)
        Just (existingSourceRow, _existingPayload)
          | existingSourceRow == sourceRow ->
              Right acc
          | otherwise ->
              Left
                ( ProjectionRowCollision
                    existingSourceRow
                    sourceRow
                    targetRow
                )

    finalize :: Map key (sourceRow, payload) -> Map key payload
    finalize =
      Map.map snd
{-# INLINE projectAtomRowMapExact #-}

projectRowDeltaExact ::
  SchemaProjection visible canonical ->
  RowDelta ->
  Either (SchemaProjectionError visible canonical) RowDelta
projectRowDeltaExact projection rows =
  plainRowPatchFromChangeMap
    <$> projectAtomRowMapExact
      projection
      id
      (plainRowPatchChangeMap rows)
{-# INLINE projectRowDeltaExact #-}

projectRelationalBoundaryExact ::
  SchemaProjection SlotId canonical ->
  RuntimeBoundary ->
  Either (SchemaProjectionError SlotId canonical) RuntimeBoundary
projectRelationalBoundaryExact projection boundary =
  first boundaryProjectionErrorToSchemaProjectionError $
    projectRelationalBoundary
      (BoundaryProjection projection)
      boundary
{-# INLINE projectRelationalBoundaryExact #-}

projectRelationalBoundary ::
  BoundaryProjection canonical ->
  RuntimeBoundary ->
  Either BoundaryProjectionError RuntimeBoundary
projectRelationalBoundary (BoundaryProjection projection) boundary = do
  if bsSchema (boundaryShape boundary) == schemaSlots (spSourceVisibleSchema projection)
    then Right ()
    else
      Left
        ( BoundaryProjectionSourceSchemaMismatch
            (schemaSlots (spSourceVisibleSchema projection))
            (bsSchema (boundaryShape boundary))
        )

  let sourceToTarget =
        sourceToTargetSlotMap projection

  sensitiveSlots <-
    foldM
      (projectSensitiveSlotBoundary sourceToTarget)
      IntSet.empty
      (IntSet.toAscList (runtimeBoundarySensitiveSlots boundary))

  let slotKeys =
        IntMap.fromListWith IntSet.union
          [ (slotIdKey targetSlot, representativeKeys)
          | (sourceSlotKey, representativeKeys) <- IntMap.toAscList (runtimeBoundarySlotKeys boundary),
            Just targetSlot <- [IntMap.lookup sourceSlotKey sourceToTarget]
          ]

  first BoundaryProjectionBoundaryError $
    mkRuntimeBoundary
      (schemaSlots (spTargetVisibleSchema projection))
      sensitiveSlots
      slotKeys
{-# INLINE projectRelationalBoundary #-}

projectSensitiveSlotBoundary ::
  IntMap SlotId ->
  IntSet.IntSet ->
  Int ->
  Either BoundaryProjectionError IntSet.IntSet
projectSensitiveSlotBoundary sourceToTarget acc sourceSlotKey =
  case IntMap.lookup sourceSlotKey sourceToTarget of
    Nothing ->
      Left (BoundaryProjectionForgottenSensitiveSlot (mkSlotId sourceSlotKey))
    Just targetSlot ->
      Right (IntSet.insert (slotIdKey targetSlot) acc)
{-# INLINE projectSensitiveSlotBoundary #-}

boundaryProjectionErrorToSchemaProjectionError ::
  BoundaryProjectionError ->
  SchemaProjectionError SlotId canonical
boundaryProjectionErrorToSchemaProjectionError errorValue =
  case errorValue of
    BoundaryProjectionSourceSchemaMismatch expected actual ->
      ProjectionBoundarySchemaMismatch expected actual
    BoundaryProjectionForgottenSensitiveSlot slot ->
      ProjectionForgottenSensitiveSlot slot
    BoundaryProjectionBoundaryError boundaryError ->
      ProjectionBoundaryError boundaryError
{-# INLINE boundaryProjectionErrorToSchemaProjectionError #-}

sourceToTargetSlotMap ::
  SchemaProjection SlotId canonical ->
  IntMap SlotId
sourceToTargetSlotMap projection =
  IntMap.fromList
    [ (slotIdKey sourceSlot, targetSlot)
    | (targetIx, sourceIx) <- IntMap.toAscList (spTargetToSource projection),
      Just sourceSlot <- [safeIndex sourceIx (schemaSlots (spSourceVisibleSchema projection))],
      Just targetSlot <- [safeIndex targetIx (schemaSlots (spTargetVisibleSchema projection))]
    ]
{-# INLINE sourceToTargetSlotMap #-}

projectionIsomorphic ::
  SchemaProjection visible canonical ->
  Bool
projectionIsomorphic projection =
  schemaWidth (spSourceVisibleSchema projection) == schemaWidth (spTargetVisibleSchema projection)
    && IntSet.fromList (IntMap.elems (spTargetToSource projection))
      == IntSet.fromAscList [0 .. schemaWidth (spSourceVisibleSchema projection) - 1]
    && IntMap.keysSet (spTargetToSource projection)
      == IntSet.fromAscList [0 .. schemaWidth (spTargetVisibleSchema projection) - 1]
{-# INLINE projectionIsomorphic #-}

duplicateCanonicalKeys ::
  (canonical -> Int) ->
  [canonical] ->
  IntSet.IntSet
duplicateCanonicalKeys canonicalKey =
  IntSet.fromList
    . fmap (canonicalKey . snd)
    . duplicateValuesOn canonicalKey
{-# INLINE duplicateCanonicalKeys #-}

unspecifiedCoverageDigest :: StableDigest128
unspecifiedCoverageDigest =
  stableDigest128 [fromInteger (0x70726f6a436f766572616765556e73706563 :: Integer)]
{-# INLINE unspecifiedCoverageDigest #-}
