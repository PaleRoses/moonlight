{-# LANGUAGE BangPatterns #-}

module Moonlight.Flow.Bench.Runtime.RbacEntitlement.Run.Checks
  ( freshCheckVisible,
    freshCheckRuntime,
    conditionalReferenceCheck,
    adversarialCheckVisible,
  )
where

import Control.Exception
  ( evaluate,
  )
import Data.Bifunctor
  ( first,
  )
import Data.Word
  ( Word64,
  )
import Moonlight.Flow.Patch qualified as R
import Moonlight.Flow.Read qualified as R
import Moonlight.Flow.Runtime.Apply qualified as R
import Moonlight.Flow.Runtime.Types qualified as R
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    tupleKeyFromInts,
  )
import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Run.Snapshot
import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Stats
  ( timed,
  )
import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Types
import Moonlight.Flow.Runtime.RbacFixture.Plans
  ( conditionalDecompPlan,
    conditionalReferencePlan,
    conditionalSeedAtoms,
  )
import Moonlight.Flow.Runtime.RbacFixture.Truth
  ( buildRuntimeFromModel,
    buildRuntimeFromTruthForPlans,
    rowsDigestWithCount,
  )
import Moonlight.Flow.Runtime.RbacFixture.Types
  ( rbacAtomKey,
  )

freshCheckVisible ::
  RbacModel ->
  Int ->
  RbacTruth ->
  RbacSnapshot ->
  IO (Either RbacBenchError (Maybe Word64, Maybe Bool))
freshCheckVisible model batch truth currentSnapshot =
  do
    (!freshNs, freshResult) <-
      timed $ do
        let rebuilt = do
              freshRuntime <- fromRbacFixture (buildRuntimeFromModel model truth)
              readSnapshot (rbmPlans model) freshRuntime
        case rebuilt of
          Left err ->
            pure (Left err)
          Right freshSnapshot -> do
            let !freshDigest = snapshotDigest freshSnapshot
                !currentDigest = snapshotDigest currentSnapshot
                !matched = freshSnapshot == currentSnapshot
            _ <- forceSnapshotDigest freshDigest `seq` forceSnapshotDigest currentDigest `seq` evaluate matched
            pure $! if matched
              then Right True
              else Left (RbacFreshMismatch batch freshDigest currentDigest)
    pure ((\matched -> (Just freshNs, Just matched)) <$> freshResult)

freshCheckRuntime ::
  RbacModel ->
  Int ->
  RbacTruth ->
  R.Runtime RbacContext RbacProp ->
  IO (Either RbacBenchError ())
freshCheckRuntime model batch truth runtime =
  case readSnapshot (rbmPlans model) runtime of
    Left err ->
      pure (Left err)
    Right currentSnapshot -> do
      result <- freshCheckVisible model batch truth currentSnapshot
      pure $
        case result of
          Left err ->
            Left err
          Right (_freshNs, _matched) ->
            Right ()

conditionalReferenceCheck ::
  RbacModel ->
  Int ->
  RbacTruth ->
  IO (Either RbacBenchError ())
conditionalReferenceCheck model batch truth =
  case buildConditionalReferenceRows model truth of
    Left err ->
      pure (Left err)
    Right (!referenceRows, !decompRows) -> do
      let !referenceDigest = rowsDigestWithCount referenceRows
          !decompDigest = rowsDigestWithCount decompRows
          !matched = referenceRows == decompRows
      _ <- forceRowsDigest referenceDigest `seq` forceRowsDigest decompDigest `seq` evaluate matched
      pure $
        if matched
          then Right ()
          else Left (RbacConditionalReferenceMismatch batch referenceDigest decompDigest)

buildConditionalReferenceRows ::
  RbacModel ->
  RbacTruth ->
  Either RbacBenchError (R.Rows, R.Rows)
buildConditionalReferenceRows model truth = do
  referencePlan <- fromRbacFixture (conditionalReferencePlan (rbmAtoms model))
  decompPlan <- fromRbacFixture (conditionalDecompPlan (rbmAtoms model))
  referenceRuntime <-
    fromRbacFixture
      ( buildRuntimeFromTruthForPlans
          (rbmAtoms model)
          [referencePlan]
          conditionalSeedAtoms
          truth
      )
  decompRuntime <-
    fromRbacFixture
      ( buildRuntimeFromTruthForPlans
          (rbmAtoms model)
          [decompPlan]
          conditionalSeedAtoms
          truth
      )
  referenceRows <- first RbacReadError (R.readRows referencePlan referenceRuntime)
  decompRows <- first RbacReadError (R.readRows decompPlan decompRuntime)
  pure (referenceRows, decompRows)

adversarialCheckVisible ::
  RbacModel ->
  Int ->
  R.Runtime RbacContext RbacProp ->
  RbacSnapshotDigest ->
  IO (Either RbacBenchError (Maybe RbacAdversarialReport))
adversarialCheckVisible model batch runtime digestBefore =
  do
    let atomsValue = rbmAtoms model
        absentRow = adversarialAbsentRow batch Member
    case cancellationPatch atomsValue absentRow of
      Left err ->
        pure (Left (RbacPatchError err))
      Right cancelPatch ->
        case R.applyPatch cancelPatch runtime of
          Left err ->
            pure (Left (RbacApplyError err))
          Right runtimeAfterCancel -> do
            cancelReadResult <- readSnapshotMeasured (rbmPlans model) runtimeAfterCancel
            case cancelReadResult of
              Left err ->
                pure (Left err)
              Right (_snapshotAfterCancel, digestAfterCancel) ->
                if digestAfterCancel /= digestBefore
                  then pure (Left (RbacCancellationChangedOutput batch digestBefore digestAfterCancel))
                  else
                    case invalidDeletePatch atomsValue absentRow of
                      Left err ->
                        pure (Left (RbacPatchError err))
                      Right badPatch ->
                        case R.applyPatch badPatch runtime of
                          Left _ ->
                            pure
                              ( Right
                                  ( Just
                                      RbacAdversarialReport
                                        { rarCancellationPreservedDigest = True,
                                          rarInvalidDeleteRejected = True
                                        }
                                  )
                              )
                          Right _ ->
                            pure (Left (RbacInvalidDeleteAccepted batch))

cancellationPatch :: RbacAtoms -> RowTupleKey -> Either R.PatchError R.Patch
cancellationPatch atomsValue rowValue = do
  inserted <- R.insert (rbaMember atomsValue) [rowValue, rowValue]
  deleted <- R.delete (rbaMember atomsValue) [rowValue, rowValue]
  pure (R.patch [inserted, deleted])

invalidDeletePatch :: RbacAtoms -> RowTupleKey -> Either R.PatchError R.Patch
invalidDeletePatch atomsValue rowValue =
  R.delete (rbaMember atomsValue) [rowValue]

adversarialAbsentRow :: Int -> RbacAtomName -> RowTupleKey
adversarialAbsentRow batch atomName =
  tupleKeyFromInts
    [ 2000000000 + rbacAtomKey atomName,
      2000000000 + batch,
      2000000000 + batch + rbacAtomKey atomName
    ]
