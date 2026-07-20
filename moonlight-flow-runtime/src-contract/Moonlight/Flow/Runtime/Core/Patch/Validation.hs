{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Moonlight.Flow.Runtime.Core.Patch.Validation
  ( CanonicalityOracle (..),
    PatchRowPolarity (..),
    PatchValidationError (..),
    validateQuotientPatch,
  )
where

import Control.Monad
  ( unless,
  )
import Data.Foldable
  ( traverse_,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( AtomId,
    QuotientEpoch,
    mkAtomId,
    nextQuotientEpoch,
  )
import Moonlight.Flow.Model.Delta
  ( AtomPatch,
    AtomPatchPositiveView (..),
    QuotientPatch (..),
    atomPatchPositiveView,
    atomPatchRows
  )
import Moonlight.Differential.Row.Delta
  ( PositiveMultiplicity
  )
import Moonlight.Differential.Row.Patch
  ( EpochTransition (..),
    plainRowPatchNull,
  )
import Moonlight.Flow.Model.Scope
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    tupleKeyWidth,
  )

data CanonicalityOracle row = CanonicalityOracle
  { isCanonicalRowAt :: QuotientEpoch -> row -> Bool,
    canonicalizeRowAt :: QuotientEpoch -> row -> row,
    expectedRowWidthAt :: QuotientEpoch -> AtomId -> Maybe Int,
    dirtyKeysOfRowAt :: QuotientEpoch -> row -> IntSet,
    dirtyTopoForDirtyKey :: Int -> IntSet,
    dirtyTopoForAtom :: AtomId -> IntSet
  }

data PatchRowPolarity
  = PatchRowRemoved
  | PatchRowInserted
  deriving stock (Eq, Ord, Show, Read)

data PatchValidationError
  = StaleQuotientPatch !QuotientEpoch !QuotientEpoch
  | NonSuccessorQuotientPatch !QuotientEpoch !QuotientEpoch
  | EmptyAtomPatch !AtomId
  | PatchRowWidthMismatch !PatchRowPolarity !QuotientEpoch !AtomId !Int !Int !RowTupleKey
  | NonCanonicalPatchRow !PatchRowPolarity !QuotientEpoch !AtomId !RowTupleKey !RowTupleKey
  deriving stock (Eq, Ord, Show)

validateQuotientPatch ::
  CanonicalityOracle RowTupleKey ->
  QuotientEpoch ->
  QuotientPatch ->
  Either PatchValidationError QuotientPatch
validateQuotientPatch oracle expected patch0 = do
  unless (etBefore (qpEpoch patch0) == expected) $
    Left (StaleQuotientPatch expected (etBefore (qpEpoch patch0)))
  let expectedAfter =
        nextQuotientEpoch (etBefore (qpEpoch patch0))
  unless (etAfter (qpEpoch patch0) == expectedAfter) $
    Left (NonSuccessorQuotientPatch expectedAfter (etAfter (qpEpoch patch0)))
  traverse_ validateAtomPatch (IntMap.toList (qpEvents patch0))
  pure (patchWithRuntimeDirtySets oracle patch0)
  where
    validateAtomPatch ::
      (Int, AtomPatch) ->
      Either PatchValidationError ()
    validateAtomPatch (atomKey, patch) = do
      let atomId =
            mkAtomId atomKey
          positiveView =
            atomPatchPositiveView patch
      whenEmptyAtomPatch atomId patch
      validateRows PatchRowRemoved (etBefore (qpEpoch patch0)) atomId (apvRemoved positiveView)
      validateRows PatchRowInserted (etAfter (qpEpoch patch0)) atomId (apvInserted positiveView)

    validateRows ::
      PatchRowPolarity ->
      QuotientEpoch ->
      AtomId ->
      Map RowTupleKey PositiveMultiplicity ->
      Either PatchValidationError ()
    validateRows polarity epoch atomId =
      Map.foldlWithKey'
        ( \eitherUnit rowValue _multiplicity -> do
            eitherUnit
            validatePatchRow oracle polarity epoch atomId rowValue
        )
        (Right ())

whenEmptyAtomPatch ::
  AtomId ->
  AtomPatch ->
  Either PatchValidationError ()
whenEmptyAtomPatch atomId patch =
  if plainRowPatchNull (atomPatchRows patch)
    then Left (EmptyAtomPatch atomId)
    else Right ()
{-# INLINE whenEmptyAtomPatch #-}

validatePatchRow ::
  CanonicalityOracle RowTupleKey ->
  PatchRowPolarity ->
  QuotientEpoch ->
  AtomId ->
  RowTupleKey ->
  Either PatchValidationError ()
validatePatchRow oracle polarity epoch atomId rowValue = do
  case expectedRowWidthAt oracle epoch atomId of
    Nothing ->
      Right ()
    Just expectedWidth -> do
      let actualWidth =
            tupleKeyWidth rowValue
      unless (actualWidth == expectedWidth) $
        Left
          ( PatchRowWidthMismatch
              polarity
              epoch
              atomId
              expectedWidth
              actualWidth
              rowValue
          )
  let canonicalRow =
        canonicalizeRowAt oracle epoch rowValue
  unless (isCanonicalRowAt oracle epoch rowValue && canonicalRow == rowValue) $
    Left
      ( NonCanonicalPatchRow
          polarity
          epoch
          atomId
          rowValue
          canonicalRow
      )
{-# INLINE validatePatchRow #-}

patchWithRuntimeDirtySets ::
  CanonicalityOracle RowTupleKey ->
  QuotientPatch ->
  QuotientPatch
patchWithRuntimeDirtySets oracle patch =
  let oldScope =
        qpScope patch
      atomScopes =
        atomScopeByAtomOfPatch oracle patch
      dirtyDeps =
        IntSet.union
          (scopeDeps oldScope)
          (foldMap scopeDeps atomScopes)
      dirtyTopo =
        IntSet.unions
          [ scopeTopo oldScope,
            dirtyTopoForDirtyKeys oracle (scopeDeps oldScope),
            foldMap scopeTopo atomScopes
          ]
   in patch
        { qpScope =
            oldScope
              { rsDeps = DepsDelta dirtyDeps,
                rsTopo = TopoDelta dirtyTopo
              },
          qpAtomScopeByAtom = atomScopes
        }
{-# INLINE patchWithRuntimeDirtySets #-}

atomScopeByAtomOfPatch ::
  CanonicalityOracle RowTupleKey ->
  QuotientPatch ->
  IntMap.IntMap RelationalScope
atomScopeByAtomOfPatch oracle patch =
  IntMap.mapWithKey
    (atomScopeOfPatch oracle patch)
    (qpEvents patch)
{-# INLINE atomScopeByAtomOfPatch #-}

atomScopeOfPatch ::
  CanonicalityOracle RowTupleKey ->
  QuotientPatch ->
  Int ->
  AtomPatch ->
  RelationalScope
atomScopeOfPatch oracle patch atomKey atomPatch =
  relationalScopeFromSets
    dirtyDeps
    dirtyTopo
    IntSet.empty
    IntSet.empty
    IntSet.empty
  where
    dirtyDeps =
      dirtyDepsOfAtomPatch oracle patch atomPatch

    dirtyTopo =
      IntSet.union
        (dirtyTopoForDirtyKeys oracle dirtyDeps)
        (dirtyTopoForAtom oracle (mkAtomId atomKey))
{-# INLINE atomScopeOfPatch #-}

dirtyDepsOfAtomPatch ::
  CanonicalityOracle RowTupleKey ->
  QuotientPatch ->
  AtomPatch ->
  IntSet
dirtyDepsOfAtomPatch oracle patch atomPatch =
  let positiveView =
        atomPatchPositiveView atomPatch
   in IntSet.union
        (dirtyDepsOfRowsAt oracle (etBefore (qpEpoch patch)) (apvRemoved positiveView))
        (dirtyDepsOfRowsAt oracle (etAfter (qpEpoch patch)) (apvInserted positiveView))
{-# INLINE dirtyDepsOfAtomPatch #-}

dirtyDepsOfRowsAt ::
  CanonicalityOracle RowTupleKey ->
  QuotientEpoch ->
  Map RowTupleKey PositiveMultiplicity ->
  IntSet
dirtyDepsOfRowsAt oracle epoch =
  Map.foldlWithKey'
    ( \acc rowValue _multiplicity ->
        IntSet.union acc (dirtyKeysOfRowAt oracle epoch rowValue)
    )
    IntSet.empty
{-# INLINE dirtyDepsOfRowsAt #-}

dirtyTopoForDirtyKeys ::
  CanonicalityOracle RowTupleKey ->
  IntSet ->
  IntSet
dirtyTopoForDirtyKeys oracle =
  IntSet.foldl'
    ( \acc dirtyKey ->
        IntSet.union acc (dirtyTopoForDirtyKey oracle dirtyKey)
    )
    IntSet.empty
{-# INLINE dirtyTopoForDirtyKeys #-}
