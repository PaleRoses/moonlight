{-# LANGUAGE DerivingStrategies #-}
module Moonlight.Flow.Runtime.Topology.Site.Quotient.Source
  ( QuotientPatchSource (..),
    QuotientPatchBuildResult (..),
    QuotientPatchBuildError (..),
    nextQuotientEpoch,
    buildQuotientPatchMaybe,
    buildQuotientPatch,
    diffCanonicalAtomRows,
    dirtyKeysOfAtomDelta,
  )
where
import Data.Bifunctor
  ( first,
  )
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Foldable
  ( traverse_,
  )
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
    QueryId,
    QuotientEpoch,
  )
import Moonlight.Core qualified as CoreRelational
import Moonlight.Flow.Model.Delta
  ( AtomPatch,
    atomPatchRows,
    QuotientPatch (..)
  )
import Moonlight.Differential.Row.Delta
  ( rowDeltaBetween
  )
import Moonlight.Flow.Model.Delta
  ( atomPatchFromRowDelta
  )
import Moonlight.Delta.Signed
  ( Multiplicity,
    zeroMultiplicity
  )
import Moonlight.Differential.Row.Patch
  ( EpochTransition (..)
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchNull
  )
import Moonlight.Differential.Row.Delta
  ( rowDeltaAffectedClasses,
  )
import Moonlight.Flow.Model.Scope
import Moonlight.Differential.Row.Tuple
import Moonlight.Flow.Runtime.Topology.Subscription
  ( AtomSubscriptionError,
    QueryAtomSubscription,
    buildAtomSubscribers,
  )
data QuotientPatchSource = QuotientPatchSource
  { qpsEpochBefore :: !QuotientEpoch,
    qpsRowsBefore :: !(IntMap (Map RowTupleKey Multiplicity)),
    qpsRowsAfter :: !(IntMap (Map RowTupleKey Multiplicity)),
    qpsCanonicalRepOf :: Int -> Maybe Int,
    qpsExpectedRowWidth :: Int -> Maybe Int,
    qpsTopoForDirtyKey :: Int -> IntSet,
    qpsTopoForAtomKey :: Int -> IntSet,
    qpsExplicitDirtyTopo :: !IntSet,
    qpsSubscriptions :: ![QueryAtomSubscription]
  }
data QuotientPatchBuildResult = QuotientPatchBuildResult
  { qpbrPatch :: !QuotientPatch,
    qpbrAtomSubscribers :: !(IntMap [(QueryId, AtomId)])
  }
  deriving stock (Eq, Show)
data QuotientPatchBuildError
  = QuotientPatchSubscriptionError !AtomSubscriptionError
  | QuotientPatchMissingCanonicalRepresentative !Int !RowTupleKey !RepKey
  | QuotientPatchNonCanonicalRepresentative !Int !RowTupleKey !RepKey !Int
  | QuotientPatchNegativeRepresentative !Int !RowTupleKey !RepKey
  | QuotientPatchRowWidthMismatch !Int !Int !RowTupleKey
  | QuotientPatchNonPositiveSnapshotMultiplicity !Int !RowTupleKey !Multiplicity
  deriving stock (Eq, Show)
nextQuotientEpoch :: QuotientEpoch -> QuotientEpoch
nextQuotientEpoch =
  CoreRelational.nextQuotientEpoch
buildQuotientPatch ::
  QuotientPatchSource ->
  Either QuotientPatchBuildError QuotientPatchBuildResult
buildQuotientPatch source = do
  maybePatch <- buildQuotientPatchMaybe source
  case maybePatch of
    Nothing ->
      Right
        QuotientPatchBuildResult
          { qpbrPatch =
              QuotientPatch
                { qpEpoch =
                    EpochTransition
                      { etBefore = qpsEpochBefore source,
                        etAfter = nextQuotientEpoch (qpsEpochBefore source)
                      },
                  qpScope =
                    mempty
                      { rsDeps = DepsDelta IntSet.empty,
                        rsTopo = TopoDelta (qpsExplicitDirtyTopo source)
                      },
                  qpAtomScopeByAtom = IntMap.empty,
                  qpEvents = IntMap.empty
                },
            qpbrAtomSubscribers = IntMap.empty
          }
    Just result ->
      Right result
buildQuotientPatchMaybe ::
  QuotientPatchSource ->
  Either QuotientPatchBuildError (Maybe QuotientPatchBuildResult)
buildQuotientPatchMaybe source = do
  subscribers <-
    first QuotientPatchSubscriptionError $
      buildAtomSubscribers (qpsSubscriptions source)
  atomDeltas <-
    buildAtomDeltas source
  if IntMap.null atomDeltas
    then Right Nothing
    else
      let atomScopes =
            IntMap.mapWithKey
              (atomScopeOfDelta source)
              atomDeltas
          dirtyKeys =
            foldMap scopeDeps atomScopes
          dirtyTopo =
            IntSet.union
              (qpsExplicitDirtyTopo source)
              (foldMap scopeTopo atomScopes)
       in Right
            ( Just
                QuotientPatchBuildResult
                  { qpbrPatch =
                      QuotientPatch
                        { qpEpoch =
                            EpochTransition
                              { etBefore = qpsEpochBefore source,
                                etAfter = nextQuotientEpoch (qpsEpochBefore source)
                              },
                          qpScope =
                            mempty
                              { rsDeps = DepsDelta dirtyKeys,
                                rsTopo = TopoDelta dirtyTopo
                              },
                          qpAtomScopeByAtom = atomScopes,
                          qpEvents = atomDeltas
                        },
                    qpbrAtomSubscribers = subscribers
                  }
            )
buildAtomDeltas ::
  QuotientPatchSource ->
  Either QuotientPatchBuildError (IntMap AtomPatch)
buildAtomDeltas source =
  IntSet.foldl'
    step
    (Right IntMap.empty)
    atomKeys
  where
    atomKeys =
      IntSet.union
        (IntMap.keysSet (qpsRowsBefore source))
        (IntMap.keysSet (qpsRowsAfter source))
    step eitherDeltas atomKey = do
      deltas <- eitherDeltas
      let beforeRows =
            IntMap.findWithDefault Map.empty atomKey (qpsRowsBefore source)
          afterRows =
            IntMap.findWithDefault Map.empty atomKey (qpsRowsAfter source)
      validateSnapshotRows source atomKey beforeRows
      validateSnapshotRows source atomKey afterRows
      case diffCanonicalAtomRows beforeRows afterRows of
        Nothing ->
          Right deltas
        Just delta ->
          Right (IntMap.insert atomKey delta deltas)
diffCanonicalAtomRows ::
  Map RowTupleKey Multiplicity ->
  Map RowTupleKey Multiplicity ->
  Maybe AtomPatch
diffCanonicalAtomRows beforeRows afterRows =
  let delta =
        rowDeltaBetween beforeRows afterRows
   in if plainRowPatchNull delta
        then Nothing
        else Just (atomPatchFromRowDelta delta)
{-# INLINE diffCanonicalAtomRows #-}
validateSnapshotRows ::
  QuotientPatchSource ->
  Int ->
  Map RowTupleKey Multiplicity ->
  Either QuotientPatchBuildError ()
validateSnapshotRows source atomKey rows =
  Map.foldlWithKey'
    ( \eitherUnit rowValue multiplicity -> do
        eitherUnit
        validateRowWidth source atomKey rowValue
        validateRowMultiplicity atomKey rowValue multiplicity
        validateCanonicalRow source atomKey rowValue
    )
    (Right ())
    rows
validateRowWidth ::
  QuotientPatchSource ->
  Int ->
  RowTupleKey ->
  Either QuotientPatchBuildError ()
validateRowWidth source atomKey rowValue =
  case qpsExpectedRowWidth source atomKey of
    Nothing ->
      Right ()
    Just expectedWidth ->
      let actualWidth =
            tupleKeyWidth rowValue
       in if actualWidth == expectedWidth
            then Right ()
            else
              Left
                ( QuotientPatchRowWidthMismatch
                    expectedWidth
                    actualWidth
                    rowValue
                )
validateRowMultiplicity ::
  Int ->
  RowTupleKey ->
  Multiplicity ->
  Either QuotientPatchBuildError ()
validateRowMultiplicity atomKey rowValue multiplicity =
  if multiplicity > zeroMultiplicity
    then Right ()
    else
      Left
        ( QuotientPatchNonPositiveSnapshotMultiplicity
            atomKey
            rowValue
            multiplicity
        )
validateCanonicalRow ::
  QuotientPatchSource ->
  Int ->
  RowTupleKey ->
  Either QuotientPatchBuildError ()
validateCanonicalRow source atomKey rowValue =
  traverse_ validateRepresentative (tupleKeyToRepKeys rowValue)
  where
    validateRepresentative rep@(RepKey keyValue) =
      if keyValue < 0
        then
          Left
            ( QuotientPatchNegativeRepresentative
                atomKey
                rowValue
                rep
            )
        else
          case qpsCanonicalRepOf source keyValue of
            Nothing ->
              Left
                ( QuotientPatchMissingCanonicalRepresentative
                    atomKey
                    rowValue
                    rep
                )
            Just canonicalKey
              | canonicalKey == keyValue ->
                  Right ()
              | otherwise ->
                  Left
                    ( QuotientPatchNonCanonicalRepresentative
                        atomKey
                        rowValue
                        rep
                        canonicalKey
                    )
dirtyKeysOfAtomDelta :: AtomPatch -> IntSet
dirtyKeysOfAtomDelta =
  rowDeltaAffectedClasses . atomPatchRows

atomScopeOfDelta ::
  QuotientPatchSource ->
  Int ->
  AtomPatch ->
  RelationalScope
atomScopeOfDelta source atomKey delta =
  relationalScopeFromSets
    dirtyKeys
    dirtyTopo
    IntSet.empty
    IntSet.empty
    IntSet.empty
  where
    dirtyKeys =
      dirtyKeysOfAtomDelta delta

    dirtyTopo =
      IntSet.union
        (topoForDirtyKeys source dirtyKeys)
        (qpsTopoForAtomKey source atomKey)
{-# INLINE atomScopeOfDelta #-}

topoForDirtyKeys ::
  QuotientPatchSource ->
  IntSet ->
  IntSet
topoForDirtyKeys source =
  IntSet.foldl'
    ( \acc dirtyKey ->
        IntSet.union acc (qpsTopoForDirtyKey source dirtyKey)
    )
    IntSet.empty
{-# INLINE topoForDirtyKeys #-}
