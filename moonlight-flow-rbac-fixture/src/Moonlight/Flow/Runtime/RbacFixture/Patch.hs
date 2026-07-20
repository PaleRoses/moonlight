{-# LANGUAGE BangPatterns #-}

module Moonlight.Flow.Runtime.RbacFixture.Patch
  ( patchSchedule,
    mutateRelation,
    generatePatchBatch,
    freshRowsFrom,
    projectRowsByIndices,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.Bits
  ( shiftR,
    xor,
  )
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Data.Word
  ( Word64,
  )
import Moonlight.Delta.Signed
  ( Multiplicity,
    addMultiplicity,
    zeroMultiplicity
  )
import Moonlight.Flow.Patch qualified as R
import Moonlight.Flow.Read qualified as R
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    tupleKeyFromInts,
    tupleKeyToInts,
  )
import Moonlight.Flow.Runtime.RbacFixture.Plans
  ( atomByName,
  )
import Moonlight.Flow.Runtime.RbacFixture.Rows
  ( rowForGlobalOrdinal,
  )
import Moonlight.Flow.Runtime.RbacFixture.Truth
  ( truthRelation,
    writeTruthRelation,
  )
import Moonlight.Flow.Runtime.RbacFixture.Types

generatePatchBatch ::
  RbacAtoms ->
  RbacSize ->
  RbacPatchShape ->
  RbacTruth ->
  Rng ->
  Either RbacFixtureError (RbacTruth, R.Patch, Rng, RbacPatchSummary)
generatePatchBatch atomsValue sizeValue shape truth0 rng0 = do
  (!truth1, !patchesReversed, !rng1, !summary) <-
    Foldable.foldlM
      step
      (truth0, [], rng0, emptyPatchSummary)
      (patchSchedule shape)
  pure (truth1, R.patch (reverse patchesReversed), rng1, summary)
  where
    step (!truth, !patches, !rng, !summary) (!atomName, !moveCount) = do
      (!truthNext, !patchValue, !rngNext, !relationSummary) <-
        mutateRelation atomsValue sizeValue atomName moveCount truth rng
      pure
        ( truthNext,
          patchValue : patches,
          rngNext,
          appendRelationSummary atomName relationSummary summary
        )

patchSchedule :: RbacPatchShape -> [(RbacAtomName, Int)]
patchSchedule shape =
  filter ((> 0) . snd) $
    let (!denyMemberCount, !denyScopeCount, !denyActionCount) = split3 (rpsDenyMoves shape)
     in [ (Member, rpsMemberMoves shape),
          (UserAttr, rpsUserAttrMoves shape),
          (ResourceScope, rpsResourceScopeMoves shape),
          (RoleAction, rpsRoleActionMoves shape),
          (GroupRole, rpsGroupRoleMoves shape),
          (DenyMember, denyMemberCount),
          (DenyGroupScope, denyScopeCount),
          (DenyGroupAction, denyActionCount),
          (GroupScope, rpsGroupScopeMoves shape)
        ]

split3 :: Int -> (Int, Int, Int)
split3 value =
  let !n = max 0 value
      !base = n `quot` 3
      !remValue = n - (base * 3)
      !a = base + if remValue >= 1 then 1 else 0
      !b = base + if remValue >= 2 then 1 else 0
   in (a, b, base)
{-# INLINE split3 #-}

mutateRelation ::
  RbacAtoms ->
  RbacSize ->
  RbacAtomName ->
  Int ->
  RbacTruth ->
  Rng ->
  Either RbacFixtureError (RbacTruth, R.Patch, Rng, RbacRelationPatchSummary)
mutateRelation atomsValue sizeValue atomName requestedMoves truth rng0 =
  let !currentRows = truthRelation atomName truth
      !deleteCount = max 0 (min requestedMoves (Set.size currentRows))
      (!deletedRows, !rng1) = drawRows deleteCount rng0 currentRows
      !afterDelete = Set.difference currentRows deletedRows
      !startOrdinal = IntMap.findWithDefault 0 (rbacAtomKey atomName) (rbtNextOrdinals truth)
   in do
        (!insertedRows, !nextOrdinal) <-
          freshRowsFrom sizeValue atomName deleteCount currentRows startOrdinal
        let !nextRows = Set.union afterDelete insertedRows
            !summary =
              RbacRelationPatchSummary
                { rrpsDeleted = Set.size deletedRows,
                  rrpsInserted = Set.size insertedRows
                }
            !truthNext =
              (writeTruthRelation atomName nextRows truth)
                { rbtNextOrdinals =
                    IntMap.insert
                      (rbacAtomKey atomName)
                      nextOrdinal
                      (rbtNextOrdinals truth)
                }
        patchValue <-
          first RbacFixturePatchError $
            R.replace
              (atomByName atomsValue atomName)
              (Set.toAscList deletedRows)
              (Set.toAscList insertedRows)
        pure (truthNext, patchValue, rng1, summary)

freshRowsFrom :: RbacSize -> RbacAtomName -> Int -> Set RowTupleKey -> Int -> Either RbacFixtureError (Set RowTupleKey, Int)
freshRowsFrom sizeValue atomName requested forbidden startOrdinal =
  if Set.size acceptedRows == target
    then Right (acceptedRows, startOrdinal + attemptLimit)
    else Left (RbacFixtureFreshRowsExhausted atomName target (Set.size acceptedRows))
  where
    !target = max 0 requested
    !attemptLimit = 128 + target * 8
    candidates =
      fmap
        (rowForGlobalOrdinal sizeValue atomName)
        [startOrdinal .. startOrdinal + attemptLimit - 1]
    acceptedRows =
      Foldable.foldl' acceptCandidate Set.empty candidates

    acceptCandidate acc rowValue
      | Set.size acc >= target =
          acc
      | Set.member rowValue forbidden =
          acc
      | Set.member rowValue acc =
          acc
      | otherwise = Set.insert rowValue acc

emptyPatchSummary :: RbacPatchSummary
emptyPatchSummary =
  RbacPatchSummary
    { rpsDeletedRows = 0,
      rpsInsertedRows = 0,
      rpsRelations = Map.empty
    }
{-# INLINE emptyPatchSummary #-}

appendRelationSummary :: RbacAtomName -> RbacRelationPatchSummary -> RbacPatchSummary -> RbacPatchSummary
appendRelationSummary atomName relationSummary summary =
  if rrpsDeleted relationSummary == 0 && rrpsInserted relationSummary == 0
    then summary
    else
      summary
        { rpsDeletedRows = rpsDeletedRows summary + rrpsDeleted relationSummary,
          rpsInsertedRows = rpsInsertedRows summary + rrpsInserted relationSummary,
          rpsRelations =
            Map.insertWith
              mergeRelationSummary
              atomName
              relationSummary
              (rpsRelations summary)
        }
{-# INLINE appendRelationSummary #-}

mergeRelationSummary :: RbacRelationPatchSummary -> RbacRelationPatchSummary -> RbacRelationPatchSummary
mergeRelationSummary newer older =
  RbacRelationPatchSummary
    { rrpsDeleted = rrpsDeleted newer + rrpsDeleted older,
      rrpsInserted = rrpsInserted newer + rrpsInserted older
    }
{-# INLINE mergeRelationSummary #-}

drawRows :: Int -> Rng -> Set RowTupleKey -> (Set RowTupleKey, Rng)
drawRows requested rng0 rows0
  | requested <= 0 || Set.null rows0 =
      (Set.empty, rng0)
  | otherwise =
      let (!offset, !rng1) = chooseInt (Set.size rows0) rng0
          !orderedRows = Set.toAscList rows0
          (!prefix, !suffix) = splitAt offset orderedRows
          !picked = Set.fromList (take requested (suffix <> prefix))
       in (picked, rng1)

projectRowsByIndices :: [Int] -> R.Rows -> Either String (Map RowTupleKey Multiplicity)
projectRowsByIndices indices =
  Foldable.foldlM insertProjectedRow Map.empty . R.rowsToList
  where
    insertProjectedRow acc (rowValue, multiplicity) = do
      projectedRow <- projectRowByIndices indices rowValue
      pure (Map.filter (/= zeroMultiplicity) (Map.insertWith addMultiplicity projectedRow multiplicity acc))
{-# INLINE projectRowsByIndices #-}

projectRowByIndices :: [Int] -> RowTupleKey -> Either String RowTupleKey
projectRowByIndices indices rowValue = do
  let values = tupleKeyToInts rowValue
      env = Map.fromList (zip [0 :: Int ..] values)
  tupleKeyFromInts <$> traverse (lookupIndex env) indices
  where
    lookupIndex env ix =
      maybe (Left ("missing projected index " <> show ix)) Right (Map.lookup ix env)
    lookupIndex :: Map Int Int -> Int -> Either String Int
{-# INLINE projectRowByIndices #-}

chooseInt :: Int -> Rng -> (Int, Rng)
chooseInt bound rng0
  | bound <= 0 =
      (0, rng0)
  | otherwise =
      let (!wordValue, !rng1) = nextWord64 rng0
          !modulus = fromIntegral bound :: Word64
       in (fromIntegral (wordValue `rem` modulus), rng1)
{-# INLINE chooseInt #-}

nextWord64 :: Rng -> (Word64, Rng)
nextWord64 (Rng state0) =
  let !state1 = state0 + 0x9e3779b97f4a7c15
      !z0 = state1
      !z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
      !z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
      !z3 = z2 `xor` (z2 `shiftR` 31)
   in (z3, Rng state1)
{-# INLINE nextWord64 #-}
