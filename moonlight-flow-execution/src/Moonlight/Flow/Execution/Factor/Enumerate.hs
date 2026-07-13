{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Execution.Factor.Enumerate
  ( enumerateBagRows,
    enumerateBagRowsBounded,
    attachPreparedRowProvenance,
    foldBagRows,
  )
where

import Control.Monad (foldM)
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Moonlight.Flow.Execution.Factor.Core
  ( Factor,
  )
import Moonlight.Flow.Execution.Factor.Types
import Moonlight.Flow.Execution.Prepared.Contract
  ( PreparedProvenanceError (..),
    PreparedProvenanceRow (..),
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Differential.Index.IndexedRows
  ( indexedRowsPayloadMap,
    indexedRowsLayout,
  )

-- | Attach the already-maintained local-factor provenance to complete output
-- assignments. Each atom has exactly one owner bag ('dpAtomOwner'), hence the
-- product of these bag values is precisely the assignment provenance without
-- double-counting separator messages.
attachPreparedRowProvenance ::
  [SlotId] ->
  DecompPlan ->
  FactorCache ->
  [RowTupleKey] ->
  Either PreparedProvenanceError [PreparedProvenanceRow]
attachPreparedRowProvenance fullSchema decomp cache =
  traverse attachRow
  where
    attachRow rowValue = do
      environment <- rowEnvironment fullSchema rowValue
      factors <-
        traverse
          (bagProvenance environment)
          (IntMap.elems (dpBags decomp))
      pure
        PreparedProvenanceRow
          { pprTuple = rowValue,
            pprFactors = factors
          }

    bagProvenance environment bag = do
      factor <-
        maybe
          (Left (PreparedProvenanceFactorMissing (dbBagId bag)))
          Right
          (factorCacheFactorAt (FactorNodeBag (dbBagId bag)) cache)
      assignment <-
        maybe
          (Left (PreparedProvenanceRowArityMismatch (Vector.length (indexedRowsLayout factor)) (IntMap.size environment)))
          Right
          (assignmentKeyFromEnv (Vector.toList (indexedRowsLayout factor)) environment)
      maybe
        (Left (PreparedProvenanceFactorCellMissing (dbBagId bag) assignment))
        Right
        (Map.lookup assignment (indexedRowsPayloadMap factor))
{-# INLINE attachPreparedRowProvenance #-}

rowEnvironment ::
  [SlotId] ->
  RowTupleKey ->
  Either PreparedProvenanceError (IntMap RepKey)
rowEnvironment schema rowValue =
  let values = tupleKeyToRepKeys rowValue
   in if length schema == length values
        then
          Right
            ( IntMap.fromList
                [ (slotIdKey slotId, value)
                  | (slotId, value) <- zip schema values
                ]
            )
        else
          Left (PreparedProvenanceRowArityMismatch (length schema) (length values))
{-# INLINE rowEnvironment #-}

type FoldControl :: Type -> Type
data FoldControl acc
  = FoldContinue !acc
  | FoldStop !acc

foldControlValue :: FoldControl acc -> acc
foldControlValue control =
  case control of
    FoldContinue value ->
      value

    FoldStop value ->
      value
{-# INLINE foldControlValue #-}

enumerateBagRows :: [SlotId] -> DecompPlan -> FactorCache -> [RowTupleKey]
enumerateBagRows fullSchema decomp cache =
  reverse (foldBagRows fullSchema decomp cache [] (:))
{-# INLINE enumerateBagRows #-}

enumerateBagRowsBounded :: Maybe Int -> [SlotId] -> DecompPlan -> FactorCache -> [RowTupleKey]
enumerateBagRowsBounded maybeLimit fullSchema decomp cache =
  case maybeLimit of
    Nothing ->
      enumerateBagRows fullSchema decomp cache

    Just limit
      | limit <= 0 ->
          []
      | otherwise ->
          reverse
            ( brRowsRev
                ( foldBagRowsUntil
                    fullSchema
                    decomp
                    cache
                    BoundedRows
                      { brRemaining = limit,
                        brRowsRev = []
                      }
                    collectBoundedRow
                )
            )
{-# INLINE enumerateBagRowsBounded #-}

type BoundedRows :: Type
data BoundedRows = BoundedRows
  { brRemaining :: {-# UNPACK #-} !Int,
    brRowsRev :: ![RowTupleKey]
  }

collectBoundedRow :: RowTupleKey -> BoundedRows -> FoldControl BoundedRows
collectBoundedRow rowValue rows
  | brRemaining rows <= 1 =
      FoldStop
        rows
          { brRemaining = 0,
            brRowsRev = rowValue : brRowsRev rows
          }
  | otherwise =
      FoldContinue
        rows
          { brRemaining = brRemaining rows - 1,
            brRowsRev = rowValue : brRowsRev rows
          }
{-# INLINE collectBoundedRow #-}

foldBagRows ::
  [SlotId] ->
  DecompPlan ->
  FactorCache ->
  r ->
  (RowTupleKey -> r -> r) ->
  r
foldBagRows fullSchema decomp cache initial step =
  foldBagRowsUntil
    fullSchema
    decomp
    cache
    initial
    (\rowValue acc -> FoldContinue (step rowValue acc))
{-# INLINE foldBagRows #-}

foldBagRowsUntil ::
  [SlotId] ->
  DecompPlan ->
  FactorCache ->
  r ->
  (RowTupleKey -> r -> FoldControl r) ->
  r
foldBagRowsUntil fullSchema decomp cache initial step =
  foldControlValue
    (foldEnumerateFromBagUntil decomp cache (dpRoot decomp) IntMap.empty initial emitRow)
  where
    emitRow env acc =
      case tupleKeyFromSlotEnv fullSchema env of
        Nothing ->
          FoldContinue acc
        Just rowValue ->
          step rowValue acc
{-# INLINE foldBagRowsUntil #-}

foldEnumerateFromBagUntil ::
  DecompPlan ->
  FactorCache ->
  BagId ->
  IntMap RepKey ->
  r ->
  (IntMap RepKey -> r -> FoldControl r) ->
  FoldControl r
foldEnumerateFromBagUntil decomp cache bagId parentEnv initial step =
  case factorCacheFactorAt (FactorNodeBagBelief bagId) cache of
    Nothing ->
      FoldContinue initial
    Just belief ->
      foldCandidateKeysForParentEnvUntil cache bagId belief parentEnv initial $ \key !acc0 ->
        case extendEnvFromKey (Vector.toList (indexedRowsLayout belief)) key parentEnv of
          Nothing ->
            FoldContinue acc0
          Just env1 ->
            foldDownChildrenUntil decomp cache bagId env1 acc0 step
{-# INLINE foldEnumerateFromBagUntil #-}

foldCandidateKeysForParentEnvUntil ::
  FactorCache ->
  BagId ->
  Factor ->
  IntMap RepKey ->
  r ->
  (AssignmentTupleKey -> r -> FoldControl r) ->
  FoldControl r
foldCandidateKeysForParentEnvUntil cache bagId belief env initial step =
  case Map.lookup bagId (fcParentSepIndexes cache) of
    Nothing ->
      Map.foldlWithKey'
        foldCandidate
        (FoldContinue initial)
        (indexedRowsPayloadMap belief)
    Just psi ->
      maybe
        (FoldContinue initial)
        ( \sepKey ->
            Foldable.foldl'
              foldCandidateKey
              (FoldContinue initial)
              (Map.findWithDefault Set.empty sepKey (psiRowsBySeparator psi))
        )
        (assignmentKeyFromEnv (psiSeparator psi) env)
  where
    foldCandidate control key _payload =
      case control of
        FoldStop {} ->
          control

        FoldContinue acc ->
          step key acc

    foldCandidateKey control key =
      case control of
        FoldStop {} ->
          control

        FoldContinue acc ->
          step key acc
{-# INLINE foldCandidateKeysForParentEnvUntil #-}

assignmentKeyFromEnv :: [SlotId] -> IntMap RepKey -> Maybe AssignmentTupleKey
assignmentKeyFromEnv schema env =
  tupleKeyFromRepKeys <$> traverse (\sid -> IntMap.lookup (slotIdKey sid) env) schema
{-# INLINE assignmentKeyFromEnv #-}

foldDownChildrenUntil ::
  DecompPlan ->
  FactorCache ->
  BagId ->
  IntMap RepKey ->
  r ->
  (IntMap RepKey -> r -> FoldControl r) ->
  FoldControl r
foldDownChildrenUntil decomp cache parent env initial step =
  foldChildren (IntMap.findWithDefault [] (unBagId parent) (dpChildren decomp)) env initial
  where
    foldChildren [] finalEnv acc =
      step finalEnv acc
    foldChildren (child : restChildren) parentEnv !acc =
      foldEnumerateFromBagUntil decomp cache child parentEnv acc $ \childEnv !childAcc ->
        foldChildren restChildren childEnv childAcc
{-# INLINE foldDownChildrenUntil #-}

extendEnvFromKey :: [SlotId] -> AssignmentTupleKey -> IntMap RepKey -> Maybe (IntMap RepKey)
extendEnvFromKey schema key env0
  | length schema /= length values = Nothing
  | otherwise = foldM insertOne env0 (zip schema values)
  where
    values = tupleKeyToRepKeys key

    insertOne :: IntMap RepKey -> (SlotId, RepKey) -> Maybe (IntMap RepKey)
    insertOne env (sid, value) =
      case IntMap.lookup (slotIdKey sid) env of
        Nothing -> Just (IntMap.insert (slotIdKey sid) value env)
        Just existing
          | existing == value -> Just env
          | otherwise -> Nothing
{-# INLINE extendEnvFromKey #-}
