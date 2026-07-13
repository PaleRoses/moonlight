{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Sheaf.Section.Repair
  ( PresheafAssignment (..),
    PlainPresheafAssignment,
    RepairDiagnostics (..),
    RepairStatus (..),
    RepairPartialSectionResult (..),
    RepairObstruction (..),
    repairPresheafAssignment,
    repairPartialSection,
    repairPreparedPartialSection,
    emptyRepairDiagnostics,
    repairDiagnosticsAreEmpty,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (asum, foldlM)
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Vector qualified as Vector
import Data.Vector.Unboxed qualified as UVector
import Moonlight.Delta.Scope
  ( Scope,
    dirtyScope,
    foldScope,
  )
import Moonlight.Sheaf.Index.Dense
  ( denseIndexKeyOf,
  )
import Moonlight.Sheaf.Section.Condition
  ( nonEmptyEntry,
    restrictionConditionAssignmentEntry,
  )
import Moonlight.Sheaf.Section.Model
  ( SheafModel,
    sheafModelFingerprint,
    sheafModelObjects,
  )
import Moonlight.Sheaf.Section.Morphism
  ( Restriction,
    RestrictionId (..),
    rSource,
    rTarget,
    rWitness,
    restrictApply,
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( unObjectKey,
  )
import Moonlight.Sheaf.Section.Store.Descent.Prepare
  ( prepareSectionDescent,
  )
import Moonlight.Sheaf.Section.Store.Descent.Rows
  ( objectRestrictionIdsAt,
    preparedRestrictionRowAt,
  )
import Moonlight.Sheaf.Section.Stalk
  ( MergeObstruction (..),
    RepairInput (..),
    StalkAlgebra (..),
    mergeStalks,
    stalkMismatches,
  )
import Moonlight.Sheaf.Section.Store.State
import Moonlight.Sheaf.Section.Store.Types

data PresheafAssignment cell stalk mismatch witness repairObstruction = PresheafAssignment
  { paModel :: !(SheafModel cell witness),
    paStalkAlgebra :: !(StalkAlgebra witness stalk mismatch repairObstruction),
    paAssignment :: !(PartialSectionStore cell stalk)
  }

type PlainPresheafAssignment cell stalk mismatch repairObstruction =
  PresheafAssignment cell stalk mismatch () repairObstruction

data RepairDiagnostics cell mismatch = RepairDiagnostics
  { repairDiagnosticCellMismatches :: !(Map cell [mismatch]),
    repairDiagnosticRestrictionMismatches :: !(Map (cell, cell) [mismatch])
  }
  deriving stock (Eq, Show)

data RepairStatus
  = RepairSettled
  | RepairResidual
  deriving stock (Eq, Ord, Show, Read)

data RepairPartialSectionResult cell stalk mismatch = RepairPartialSectionResult
  { repairedPartialSection :: !(PartialSectionStore cell stalk),
    repairPartialDiagnostics :: !(RepairDiagnostics cell mismatch),
    repairPartialStatus :: !RepairStatus
  }
  deriving stock (Eq, Show)

data RepairObstruction cell repairObstruction
  = RepairStoreObstruction !(SectionStoreError cell)
  | RepairDomainObstruction !cell !repairObstruction
  | RepairDescentPreparationObstruction !SectionDescentPreparationError
  deriving stock (Eq, Show)

emptyRepairDiagnostics :: RepairDiagnostics cell mismatch
emptyRepairDiagnostics =
  RepairDiagnostics
    { repairDiagnosticCellMismatches = Map.empty,
      repairDiagnosticRestrictionMismatches = Map.empty
    }

repairDiagnosticsAreEmpty :: RepairDiagnostics cell mismatch -> Bool
repairDiagnosticsAreEmpty diagnostics =
  all null (repairDiagnosticCellMismatches diagnostics)
    && all null (repairDiagnosticRestrictionMismatches diagnostics)

repairPresheafAssignment ::
  Ord cell =>
  PresheafAssignment cell stalk mismatch witness repairObstruction ->
  Either
    (RepairObstruction cell repairObstruction)
    (RepairPartialSectionResult cell stalk mismatch)
repairPresheafAssignment assignmentValue =
  repairPartialSection
    (paModel assignmentValue)
    (paStalkAlgebra assignmentValue)
    (paAssignment assignmentValue)

repairPartialSection ::
  Ord cell =>
  SheafModel cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  PartialSectionStore cell stalk ->
  Either
    (RepairObstruction cell repairObstruction)
    (RepairPartialSectionResult cell stalk mismatch)
repairPartialSection model stalkAlgebra assignmentValue = do
  preparedDescent <-
    first
      RepairDescentPreparationObstruction
      (prepareSectionDescent model)
  repairPreparedPartialSection model preparedDescent stalkAlgebra assignmentValue

repairPreparedPartialSection ::
  Ord cell =>
  SheafModel cell witness ->
  PreparedSectionDescent cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  PartialSectionStore cell stalk ->
  Either
    (RepairObstruction cell repairObstruction)
    (RepairPartialSectionResult cell stalk mismatch)
repairPreparedPartialSection model preparedDescent stalkAlgebra assignmentValue = do
  validateRepairPreparedSection model preparedDescent assignmentValue
  let assignments =
        partialSectionEntries assignmentValue

  assignmentOrdinals <-
    assignmentOrdinalMap model assignments

  targetOrdinals <-
    repairTargetOrdinalMap preparedDescent assignmentOrdinals

  residualRows <-
    repairResidualRows preparedDescent (Map.elems targetOrdinals)

  repairedCells <-
    traverse
      (repairPreparedCell preparedDescent stalkAlgebra assignments)
      (Map.toAscList targetOrdinals)

  repairedSection <-
    first
      RepairStoreObstruction
      (mkPartialSectionStore model (Map.fromList (mapMaybe repairedCellEntry repairedCells)))

  let diagnostics =
        repairedSectionDiagnosticsPrepared stalkAlgebra repairedCells repairedSection residualRows

  pure
    RepairPartialSectionResult
      { repairedPartialSection = repairedSection,
        repairPartialDiagnostics = diagnostics,
        repairPartialStatus = repairStatusFromDiagnostics diagnostics
      }

data Candidate cell witness stalk
  = DirectCandidate !stalk
  | RestrictedCandidate !cell !witness !stalk

data TargetedDirectRepair cell witness stalk = TargetedDirectRepair
  { tdrDirectValue :: !stalk,
    tdrRetainedCandidates :: ![Candidate cell witness stalk]
  }

data RepairedCell cell stalk mismatch = RepairedCell
  { repairedCellEntry :: !(Maybe (cell, stalk)),
    repairedCellDiagnosticEntry :: !(Maybe (cell, [mismatch]))
  }

repairPreparedCell ::
  Ord cell =>
  PreparedSectionDescent cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  Map cell stalk ->
  (cell, Int) ->
  Either (RepairObstruction cell repairObstruction) (RepairedCell cell stalk mismatch)
repairPreparedCell preparedDescent stalkAlgebra assignments (targetCell, targetOrdinal) = do
  incomingRows <-
    incomingRestrictionRowsForOrdinal preparedDescent targetOrdinal
  let directValue =
        Map.lookup targetCell assignments
      candidates =
        candidateStalksFromRows stalkAlgebra assignments targetCell directValue incomingRows
  case NonEmpty.nonEmpty candidates of
    Nothing ->
      Right
        RepairedCell
          { repairedCellEntry = Nothing,
            repairedCellDiagnosticEntry = Nothing
          }
    Just nonEmptyCandidates -> do
      mergedValue <-
        mergedCandidateValue stalkAlgebra targetCell nonEmptyCandidates
      pure
        RepairedCell
          { repairedCellEntry = Just (targetCell, mergedValue),
            repairedCellDiagnosticEntry =
              nonEmptyEntry
                targetCell
                (maybe [] (stalkMismatches stalkAlgebra mergedValue) directValue)
          }

candidateStalksFromRows ::
  Ord cell =>
  StalkAlgebra witness stalk mismatch repairObstruction ->
  Map cell stalk ->
  cell ->
  Maybe stalk ->
  [SectionDescentRestrictionRow cell witness] ->
  [Candidate cell witness stalk]
candidateStalksFromRows stalkAlgebra assignments targetCell directValue incomingRows =
  mapMaybe
    (restrictionCandidate stalkAlgebra assignments . sdrRestriction)
    (filter (includeRestrictionCandidate targetCell directValue . sdrRestriction) incomingRows)
    <> foldMap (pure . DirectCandidate) directValue

includeRestrictionCandidate ::
  Eq cell =>
  cell ->
  Maybe stalk ->
  Restriction cell witness ->
  Bool
includeRestrictionCandidate targetCell directValue restriction =
  case directValue of
    Just _ | rSource restriction == targetCell ->
      False
    _ ->
      True

restrictionCandidate ::
  Ord cell =>
  StalkAlgebra witness stalk mismatch repairObstruction ->
  Map cell stalk ->
  Restriction cell witness ->
  Maybe (Candidate cell witness stalk)
restrictionCandidate stalkAlgebra assignments restriction =
  fmap
    (RestrictedCandidate (rSource restriction) (rWitness restriction) . restrictApply stalkAlgebra restriction)
    (Map.lookup (rSource restriction) assignments)

mergedCandidateValue ::
  StalkAlgebra witness stalk mismatch repairObstruction ->
  cell ->
  NonEmpty (Candidate cell witness stalk) ->
  Either (RepairObstruction cell repairObstruction) stalk
mergedCandidateValue stalkAlgebra targetCell candidates =
  let firstCandidate :| restCandidates =
        targetedRepairCandidates stalkAlgebra candidates

      candidateValues =
        fmap candidateValue (firstCandidate :| restCandidates)
   in case foldlM (mergeStalks stalkAlgebra) (candidateValue firstCandidate) (fmap candidateValue restCandidates) of
        Right mergedValue ->
          Right mergedValue
        Left (MergeMismatchObstruction mismatches) ->
          case saRepair stalkAlgebra (RepairMergeInput candidateValues mismatches) of
            Right repairedValue ->
              Right repairedValue
            Left repairObstruction ->
              Left (RepairDomainObstruction targetCell repairObstruction)

targetedRepairCandidates ::
  StalkAlgebra witness stalk mismatch repairObstruction ->
  NonEmpty (Candidate cell witness stalk) ->
  NonEmpty (Candidate cell witness stalk)
targetedRepairCandidates stalkAlgebra candidates =
  case directCandidateValue candidates of
    Nothing ->
      candidates
    Just directValue ->
      targetedRepairResult
        ( foldl'
            (targetedRepairStep stalkAlgebra)
            TargetedDirectRepair
              { tdrDirectValue = directValue,
                tdrRetainedCandidates = []
              }
            candidates
        )

directCandidateValue :: NonEmpty (Candidate cell witness stalk) -> Maybe stalk
directCandidateValue =
  asum . fmap candidateDirectValue

candidateDirectValue :: Candidate cell witness stalk -> Maybe stalk
candidateDirectValue candidate =
  case candidate of
    DirectCandidate stalkValue ->
      Just stalkValue
    RestrictedCandidate {} ->
      Nothing

targetedRepairStep ::
  StalkAlgebra witness stalk mismatch repairObstruction ->
  TargetedDirectRepair cell witness stalk ->
  Candidate cell witness stalk ->
  TargetedDirectRepair cell witness stalk
targetedRepairStep stalkAlgebra repairState candidate =
  case candidate of
    DirectCandidate _ ->
      repairState
    RestrictedCandidate _ witness restrictedValue ->
      case repairDirectAgainstRestriction stalkAlgebra (tdrDirectValue repairState) witness restrictedValue of
        Nothing ->
          repairState
            { tdrRetainedCandidates = candidate : tdrRetainedCandidates repairState
            }
        Just repairedDirectValue ->
          repairState
            { tdrDirectValue = repairedDirectValue
            }

targetedRepairResult ::
  TargetedDirectRepair cell witness stalk ->
  NonEmpty (Candidate cell witness stalk)
targetedRepairResult repairState =
  case reverse (tdrRetainedCandidates repairState) <> [DirectCandidate (tdrDirectValue repairState)] of
    firstCandidate : restCandidates ->
      firstCandidate :| restCandidates
    [] ->
      DirectCandidate (tdrDirectValue repairState) :| []

repairDirectAgainstRestriction ::
  StalkAlgebra witness stalk mismatch repairObstruction ->
  stalk ->
  witness ->
  stalk ->
  Maybe stalk
repairDirectAgainstRestriction stalkAlgebra targetValue witness restrictedValue =
  NonEmpty.nonEmpty (stalkMismatches stalkAlgebra restrictedValue targetValue)
    >>= either
      (const Nothing)
      Just
      . saRepair stalkAlgebra
      . RepairRestrictionInput witness restrictedValue targetValue

candidateValue :: Candidate cell witness stalk -> stalk
candidateValue candidate =
  case candidate of
    DirectCandidate stalkValue ->
      stalkValue
    RestrictedCandidate _ _ stalkValue ->
      stalkValue

repairedSectionDiagnosticsPrepared ::
  Ord cell =>
  StalkAlgebra witness stalk mismatch repairObstruction ->
  [RepairedCell cell stalk mismatch] ->
  PartialSectionStore cell stalk ->
  [SectionDescentRestrictionRow cell witness] ->
  RepairDiagnostics cell mismatch
repairedSectionDiagnosticsPrepared stalkAlgebra repairedCells repairedSection residualRows =
  RepairDiagnostics
    { repairDiagnosticCellMismatches =
        Map.fromList (mapMaybe repairedCellDiagnosticEntry repairedCells),
      repairDiagnosticRestrictionMismatches =
        residualPreparedRestrictionDiagnostics stalkAlgebra repairedSection residualRows
    }

repairStatusFromDiagnostics ::
  RepairDiagnostics cell mismatch ->
  RepairStatus
repairStatusFromDiagnostics diagnostics =
  if Map.null (repairDiagnosticRestrictionMismatches diagnostics)
    then RepairSettled
    else RepairResidual

residualPreparedRestrictionDiagnostics ::
  Ord cell =>
  StalkAlgebra witness stalk mismatch repairObstruction ->
  PartialSectionStore cell stalk ->
  [SectionDescentRestrictionRow cell witness] ->
  Map (cell, cell) [mismatch]
residualPreparedRestrictionDiagnostics stalkAlgebra repairedSection rows =
  Map.fromListWith (<>)
    ( mapMaybe
        ( restrictionConditionAssignmentEntry
            (\restriction -> (rSource restriction, rTarget restriction))
            stalkAlgebra
            assignments
            . sdrRestriction
        )
        rows
    )
  where
    assignments =
      partialSectionEntries repairedSection

validateRepairPreparedSection ::
  SheafModel cell witness ->
  PreparedSectionDescent cell witness ->
  PartialSectionStore cell stalk ->
  Either (RepairObstruction cell repairObstruction) ()
validateRepairPreparedSection model preparedDescent assignmentValue
  | psdModelFingerprint preparedDescent /= sheafModelFingerprint model =
      Left
        ( RepairStoreObstruction
            ( SectionStoreModelFingerprintMismatch
                (sheafModelFingerprint model)
                (psdModelFingerprint preparedDescent)
            )
        )
  | partialSectionModelFingerprint assignmentValue /= sheafModelFingerprint model =
      Left
        ( RepairStoreObstruction
            ( SectionStoreModelFingerprintMismatch
                (sheafModelFingerprint model)
                (partialSectionModelFingerprint assignmentValue)
            )
        )
  | otherwise =
      Right ()

assignmentOrdinalMap ::
  Ord cell =>
  SheafModel cell witness ->
  Map cell stalk ->
  Either (RepairObstruction cell repairObstruction) (Map cell Int)
assignmentOrdinalMap model assignments =
  fmap Map.fromAscList
    (traverse assignmentOrdinal (Map.keys assignments))
  where
    assignmentOrdinal cell =
      case denseIndexKeyOf cell (sheafModelObjects model) of
        Just objectKey ->
          Right (cell, unObjectKey objectKey)
        Nothing ->
          Left (RepairStoreObstruction (SectionStoreUnknownCell cell))

repairTargetOrdinalMap ::
  Ord cell =>
  PreparedSectionDescent cell witness ->
  Map cell Int ->
  Either (RepairObstruction cell repairObstruction) (Map cell Int)
repairTargetOrdinalMap preparedDescent assignmentOrdinals =
  fmap
    (Map.union assignmentOrdinals)
    ( foldScope
        (Right Map.empty)
        (repairTargetsFromObjectKeys preparedDescent)
        (repairTargetsFromRestrictionIds preparedDescent (UVector.toList (psdvAllRestrictionIds (psdViews preparedDescent))))
        (dirtyAssignmentScope assignmentOrdinals)
    )

dirtyAssignmentScope :: Map cell Int -> Scope IntSet.IntSet
dirtyAssignmentScope =
  dirtyScope . IntSet.fromList . Map.elems

repairTargetsFromObjectKeys ::
  Ord cell =>
  PreparedSectionDescent cell witness ->
  IntSet.IntSet ->
  Either (RepairObstruction cell repairObstruction) (Map cell Int)
repairTargetsFromObjectKeys preparedDescent objectKeys =
  repairTargetsFromRestrictionIds
    preparedDescent
    ( foldMap
        (restrictionIdsAtObject (psdvOutgoingRestrictionIdsByObject (psdViews preparedDescent)))
        (IntSet.toAscList objectKeys)
    )

repairTargetsFromRestrictionIds ::
  Ord cell =>
  PreparedSectionDescent cell witness ->
  [Int] ->
  Either (RepairObstruction cell repairObstruction) (Map cell Int)
repairTargetsFromRestrictionIds preparedDescent restrictionKeys =
  Map.fromList <$> traverse (repairTargetFromRestrictionId preparedDescent) restrictionKeys

repairTargetFromRestrictionId ::
  PreparedSectionDescent cell witness ->
  Int ->
  Either (RepairObstruction cell repairObstruction) (cell, Int)
repairTargetFromRestrictionId preparedDescent restrictionKey =
  case preparedRestrictionRowAt preparedDescent restrictionKey of
    Just row ->
      Right (rTarget (sdrRestriction row), sdrTargetOrdinal row)
    Nothing ->
      Left (RepairDescentPreparationObstruction (SectionDescentPreparationRestrictionMissing (RestrictionId restrictionKey)))

incomingRestrictionRowsForOrdinal ::
  PreparedSectionDescent cell witness ->
  Int ->
  Either (RepairObstruction cell repairObstruction) [SectionDescentRestrictionRow cell witness]
incomingRestrictionRowsForOrdinal preparedDescent targetOrdinal =
  traverse
    (preparedRestrictionRowForRepair preparedDescent)
    (UVector.toList (objectRestrictionIdsAt targetOrdinal (psdvIncomingRestrictionIdsByObject (psdViews preparedDescent))))

repairResidualRows ::
  PreparedSectionDescent cell witness ->
  [Int] ->
  Either (RepairObstruction cell repairObstruction) [SectionDescentRestrictionRow cell witness]
repairResidualRows preparedDescent targetOrdinals =
  traverse
    (preparedRestrictionRowForRepair preparedDescent)
    ( IntSet.toAscList
        ( foldMap
            (IntSet.fromAscList . restrictionIdsAtObject (psdvIncidentRestrictionIdsByObject (psdViews preparedDescent)))
            targetOrdinals
        )
    )

restrictionIdsAtObject ::
  Vector.Vector (UVector.Vector Int) ->
  Int ->
  [Int]
restrictionIdsAtObject restrictionIdsByObject objectOrdinal =
  UVector.toList (objectRestrictionIdsAt objectOrdinal restrictionIdsByObject)

preparedRestrictionRowForRepair ::
  PreparedSectionDescent cell witness ->
  Int ->
  Either (RepairObstruction cell repairObstruction) (SectionDescentRestrictionRow cell witness)
preparedRestrictionRowForRepair preparedDescent restrictionKey =
  case preparedRestrictionRowAt preparedDescent restrictionKey of
    Just row ->
      Right row
    Nothing ->
      Left (RepairDescentPreparationObstruction (SectionDescentPreparationRestrictionMissing (RestrictionId restrictionKey)))
