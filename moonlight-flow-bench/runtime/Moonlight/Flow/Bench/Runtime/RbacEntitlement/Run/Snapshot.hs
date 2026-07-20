{-# LANGUAGE BangPatterns #-}

module Moonlight.Flow.Bench.Runtime.RbacEntitlement.Run.Snapshot
  ( readCheckedVisibleSnapshot,
    readSnapshotMeasured,
    timedVisibleRead,
    timedReadSnapshot,
    readSnapshot,
    snapshotDigest,
    forceSnapshotDigest,
    forceRowsDigest,
    validateGrantProjectionViews,
  )
where

import Control.Exception
  ( evaluate,
  )
import Control.Monad
  ( unless,
  )
import Data.Bifunctor
  ( first,
  )
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
  ( Multiplicity
  )
import Moonlight.Flow.Read qualified as R
import Moonlight.Flow.Runtime.Types qualified as R
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )
import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Stats
  ( timed,
  )
import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Types
import Moonlight.Flow.Runtime.RbacFixture.Patch
  ( projectRowsByIndices,
  )
import Moonlight.Flow.Runtime.RbacFixture.Truth
  ( rowsDigestWithCount,
  )

readCheckedVisibleSnapshot ::
  RbacPlans ->
  R.Runtime RbacContext RbacProp ->
  IO (Either RbacBenchError (Maybe RbacVisibleRead))
readCheckedVisibleSnapshot plans runtime = do
  readResult <- timedVisibleRead plans runtime
  pure $
    case readResult of
      Left err ->
        Left err
      Right visibleRead ->
        case validateGrantProjectionViews (rvrSnapshot visibleRead) of
          Left err ->
            Left err
          Right () ->
            Right (Just visibleRead)

readSnapshotMeasured ::
  RbacPlans ->
  R.Runtime RbacContext RbacProp ->
  IO (Either RbacBenchError (RbacSnapshot, RbacSnapshotDigest))
readSnapshotMeasured plans runtime = do
  result <- timedReadSnapshot plans runtime
  pure (snd result)

timedVisibleRead ::
  RbacPlans ->
  R.Runtime RbacContext RbacProp ->
  IO (Either RbacBenchError RbacVisibleRead)
timedVisibleRead plans runtime = do
  (!readNs, readResult) <- timedReadSnapshot plans runtime
  pure $
    case readResult of
      Left err ->
        Left err
      Right (!snapshotValue, !digestValue) ->
        Right
          RbacVisibleRead
            { rvrReadNs = readNs,
              rvrSnapshot = snapshotValue,
              rvrDigest = digestValue
            }

timedReadSnapshot ::
  RbacPlans ->
  R.Runtime RbacContext RbacProp ->
  IO (Word64, Either RbacBenchError (RbacSnapshot, RbacSnapshotDigest))
timedReadSnapshot plans runtime =
  timed $
    case readSnapshot plans runtime of
      Left err ->
        pure (Left err)
      Right snapshotValue -> do
        let !digestValue = snapshotDigest snapshotValue
        evaluate (forceSnapshotDigest digestValue)
        pure (Right (snapshotValue, digestValue))

readSnapshot ::
  RbacPlans ->
  R.Runtime RbacContext RbacProp ->
  Either RbacBenchError RbacSnapshot
readSnapshot plans runtime = do
  grantRows <- readPlanRows (rbpGrant plans)
  conditionalRows <- readPlanRows (rbpConditionalGrant plans)
  deniedRows <- readPlanRows (rbpDenied plans)
  grantUserActionRows <- readPlanRows (rbpGrantUserAction plans)
  grantResourceSubjectRows <- readPlanRows (rbpGrantResourceSubject plans)
  grantScopeActionRows <- readPlanRows (rbpGrantScopeAction plans)
  pure
    RbacSnapshot
      { rsGrant = grantRows,
        rsConditionalGrant = conditionalRows,
        rsDenied = deniedRows,
        rsGrantUserAction = grantUserActionRows,
        rsGrantResourceSubject = grantResourceSubjectRows,
        rsGrantScopeAction = grantScopeActionRows
      }
  where
    readPlanRows planValue =
      first RbacReadError (R.readRows planValue runtime)

snapshotDigest :: RbacSnapshot -> RbacSnapshotDigest
snapshotDigest snapshotValue =
  RbacSnapshotDigest
    { rsdGrant = rowsDigestWithCount (rsGrant snapshotValue),
      rsdConditionalGrant = rowsDigestWithCount (rsConditionalGrant snapshotValue),
      rsdDenied = rowsDigestWithCount (rsDenied snapshotValue),
      rsdGrantUserAction = rowsDigestWithCount (rsGrantUserAction snapshotValue),
      rsdGrantResourceSubject = rowsDigestWithCount (rsGrantResourceSubject snapshotValue),
      rsdGrantScopeAction = rowsDigestWithCount (rsGrantScopeAction snapshotValue),
      rsdEffectiveCount = Set.size (effectiveEntitlements snapshotValue)
    }

forceSnapshotDigest :: RbacSnapshotDigest -> ()
forceSnapshotDigest digestValue =
  forceRowsDigest (rsdGrant digestValue)
    `seq` forceRowsDigest (rsdConditionalGrant digestValue)
    `seq` forceRowsDigest (rsdDenied digestValue)
    `seq` forceRowsDigest (rsdGrantUserAction digestValue)
    `seq` forceRowsDigest (rsdGrantResourceSubject digestValue)
    `seq` forceRowsDigest (rsdGrantScopeAction digestValue)
    `seq` rsdEffectiveCount digestValue
    `seq` ()
{-# INLINE forceSnapshotDigest #-}

forceRowsDigest :: RbacRowsDigest -> ()
forceRowsDigest rowsDigest =
  rrdPositiveCount rowsDigest
    `seq` fst (rrdDigest rowsDigest)
    `seq` snd (rrdDigest rowsDigest)
    `seq` ()
{-# INLINE forceRowsDigest #-}

effectiveEntitlements :: RbacSnapshot -> Set RowTupleKey
effectiveEntitlements snapshotValue =
  Set.difference
    ( Set.union
        (positiveRowSet (rsGrant snapshotValue))
        (positiveRowSet (rsConditionalGrant snapshotValue))
    )
    (positiveRowSet (rsDenied snapshotValue))
{-# INLINE effectiveEntitlements #-}

positiveRowSet :: R.Rows -> Set RowTupleKey
positiveRowSet =
  Set.fromList . R.positiveRows
{-# INLINE positiveRowSet #-}

validateGrantProjectionViews :: RbacSnapshot -> Either RbacBenchError ()
validateGrantProjectionViews snapshotValue = do
  assertProjected
    "grant_user_action"
    [0, 1, 3]
    (rsGrant snapshotValue)
    (rsGrantUserAction snapshotValue)
  assertProjected
    "grant_resource_subject"
    [0, 2, 1]
    (rsGrant snapshotValue)
    (rsGrantResourceSubject snapshotValue)

assertProjected :: String -> [Int] -> R.Rows -> R.Rows -> Either RbacBenchError ()
assertProjected name indices sourceRows projectedRows = do
  sourceProjection <- first (RbacProjectionMismatch . ((name <> ": ") <>)) (projectRowsByIndices indices sourceRows)
  let actualProjection = rowsMap projectedRows
  unless (sourceProjection == actualProjection) $
    Left (RbacProjectionMismatch name)

rowsMap :: R.Rows -> Map RowTupleKey Multiplicity
rowsMap =
  Map.fromList . R.rowsToList
{-# INLINE rowsMap #-}
