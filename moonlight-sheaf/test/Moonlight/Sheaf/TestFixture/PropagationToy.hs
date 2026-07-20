{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}

module Moonlight.Sheaf.TestFixture.PropagationToy
  ( ToyCell (..),
    ToyMorphism (..),
    ToySite (..),
    ToySheaf,
    ToySection,
    ToyStalk (..),
    ToyPatch (..),
    ToyPropagationObstruction (..),
    toyAlgebra,
    withToySheaf,
    toySheafModel,
    toyPreparedDescent,
    initialToySectionWith,
    toyPatch,
    toyKeyedPatch,
    toyBenchmarkKeyedBatchDelta,
    toyBenchmarkPreparedObjectProgram,
    toyBenchmarkPreparedEditProgram,
    toyBenchmarkStalkValues,
    toyBenchmarkKeyedPatches,
    propagateToySectionWith,
    propagateToyKeyedSectionWith,
    propagateToyKeyedBatchWith,
    propagateToyKeyedDeltasWith,
    propagateToyEventStreamWith,
    propagateToyPreparedProgramObservedWith,
    toySectionEntriesWith,
    toyStalkAtWith,
    toyBenchmarkPatches,
  )
where

import Control.Monad (foldM)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Moonlight.Delta.Scope
  ( Scope,
    Scoped (..),
    cleanScope,
    dirtyScope,
    foldScope,
    fullScope,
    scopedDelta,
    unionScope,
  )
import Moonlight.Sheaf.Presheaf.Core
  ( CompiledRestriction (..),
  )
import Moonlight.Sheaf.Section.Certified
  ( SectionCertificationFailure (..),
    SectionCertificationError (..),
  )
import Moonlight.Sheaf.Section.Model
  ( SheafModel,
    SheafModelBuildError,
    sheafModelObjects,
    withPreparedSheafModel,
  )
import Moonlight.Sheaf.Section.Morphism
  ( RestrictionParts (..),
    unitIncidenceRestriction,
  )
import Moonlight.Sheaf.Index.Dense (denseIndexKeyOf)
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectKey,
    initialSheafModelVersion,
    mkObjectIndex,
    unObjectKey,
  )
import Moonlight.Sheaf.Section.Stalk
  ( StalkAlgebra,
  )
import Moonlight.Sheaf.Section.Stalk.Discrete
  ( DiscreteMismatch,
    DiscreteRepairObstruction,
    discreteStalkAlgebra,
  )
import Moonlight.Sheaf.Section.Store.Descent.Execute
  ( descendAlgebraPreparedLocalKeyedBatch,
    descendAlgebraPreparedSectionProgram,
    prepareSectionObjectProgram,
    prepareSectionProgram,
    runAlgebraSectionDescentTransaction,
    transactKeyedSectionDelta,
  )
import Moonlight.Sheaf.Section.Store.Descent.FastPath
  ( prepareAlgebraSectionDescent,
  )
import Moonlight.Sheaf.Section.Store.Descent.Prepare
  ( prepareSectionDescent,
  )
import Moonlight.Sheaf.Section.Store.State
  ( mkTotalSectionStore,
    totalSectionEntries,
    totalStalkAt,
  )
import Moonlight.Sheaf.Section.Store.Types
  ( AlgebraPreparedSectionDescent,
    KeyedSectionDelta (..),
    KeyedSectionEdit (..),
    PreparedSectionDescent,
    PreparedSectionProgram,
    SectionConstructionError,
    SectionDescentError (..),
    SectionDescentObservation (..),
    SectionDescentPreparationError (..),
    SectionLookupError,
    SectionStoreError (..),
    TotalSectionStore,
    apsdPreparedDescent,
    sdrSection,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    CoveringFamily,
    PullbackSquare (..),
    Site (..),
    cmSource,
    cmTarget,
    coveringFamilyFromTargetedWitnesses,
  )
import Moonlight.Sheaf.Site.Class.Validation
  ( siteLawFailures,
  )

data ToyCell
  = ParentCell
  | ChildCell
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

data ToyMorphism
  = ToyIdentity !ToyCell
  | ParentToChild
  deriving stock (Eq, Ord, Show, Read)

data ToySite = ToySite
  deriving stock (Eq, Ord, Show, Read)

newtype ToyStalk = ToyStalk
  { unToyStalk :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

data ToySheaf owner = ToySheaf
  { tsModel :: !(SheafModel owner ToyCell (CompiledRestriction ToySite)),
    tsAlgebraPreparedDescent ::
      !( AlgebraPreparedSectionDescent
          owner
          ToyCell
          (CompiledRestriction ToySite)
          ToyStalk
          (DiscreteMismatch ToyStalk)
          (DiscreteRepairObstruction ToyStalk)
       )
  }

type role ToySheaf nominal

type ToySection owner = TotalSectionStore owner ToyCell ToyStalk

newtype ToyPatch = ToyPatch
  { unToyPatch :: Map ToyCell ToyStalk
  }
  deriving stock (Eq, Show)

data ToyPropagationObstruction
  = ToySheafLawRejected !(NonEmpty String)
  | ToySheafModelRejected !(SheafModelBuildError ToyCell)
  | ToyDescentPreparationFailed !SectionDescentPreparationError
  | ToyInitialSectionRejected !(SectionConstructionError ToyCell)
  | ToyPatchSectionStoreFailed !(SectionStoreError ToyCell)
  | ToyRestrictionLookupFailed !ToyCell !(SectionLookupError ToyCell)
  | ToyPinnedRestrictionConflict !ToyCell !ToyStalk !ToyStalk
  | ToyDescentFrontierDidNotConverge !(Scope IntSet)
  | ToySectionRejected !(SectionCertificationFailure ToyCell (DiscreteMismatch ToyStalk))
  deriving stock (Eq, Show)

instance Site ToySite where
  type SiteObject ToySite = ToyCell
  type SiteMorphism ToySite = ToyMorphism

  siteObjects _ =
    [ParentCell, ChildCell]

  siteMorphisms _ =
    [parentToChild]

  identityAt _ =
    toyIdentity

  coversAt _ cell =
    case cell of
      ParentCell -> []
      ChildCell -> [childCover]

  composeChecked _ outer inner
    | cmTarget inner /= cmSource outer =
        Nothing
    | otherwise =
        composeToyMorphisms outer inner

  pullbackPair _ left right =
    toyPullbackSquare left right

toyAlgebra :: StalkAlgebra (CompiledRestriction ToySite) ToyStalk (DiscreteMismatch ToyStalk) (DiscreteRepairObstruction ToyStalk)
toyAlgebra =
  discreteStalkAlgebra

toySheafModel :: ToySheaf owner -> SheafModel owner ToyCell (CompiledRestriction ToySite)
toySheafModel =
  tsModel

toyPreparedDescent :: ToySheaf owner -> PreparedSectionDescent owner ToyCell (CompiledRestriction ToySite)
toyPreparedDescent =
  apsdPreparedDescent . tsAlgebraPreparedDescent

withToySheaf ::
  (forall owner. ToySheaf owner -> Either ToyPropagationObstruction result) ->
  Either ToyPropagationObstruction result
withToySheaf useSheaf =
  case NonEmpty.nonEmpty (siteLawFailures ToySite) of
    Just failures ->
      Left (ToySheafLawRejected (fmap show failures))
    Nothing ->
      do
        mapSheafModel
          ( withPreparedSheafModel
                initialSheafModelVersion
                (mkObjectIndex (siteObjects ToySite))
                ( \checkedMorphism ->
                    RestrictionParts
                      { partKind = unitIncidenceRestriction,
                        partSource = cmSource checkedMorphism,
                        partTarget = cmTarget checkedMorphism,
                        partWitness = CompiledRestriction ToySite checkedMorphism
                      }
                )
                (siteMorphisms ToySite)
                ( \model -> do
                    preparedDescent <- mapSectionDescentPreparation (prepareSectionDescent model)
                    useSheaf
                      ToySheaf
                        { tsModel = model,
                          tsAlgebraPreparedDescent = prepareAlgebraSectionDescent preparedDescent toyAlgebra
                        }
                )
          )
          >>= id

initialToySectionWith :: ToySheaf owner -> ToyStalk -> Either ToyPropagationObstruction (ToySection owner)
initialToySectionWith sheaf stalkValue =
  mapSectionConstruction
    (mkTotalSectionStore (tsModel sheaf) (Map.fromList [(ParentCell, stalkValue), (ChildCell, stalkValue)]))

toyPatch :: [(ToyCell, ToyStalk)] -> Scoped (Set ToyCell) ToyPatch
toyPatch assignments =
  scopedDelta
    (dirtyScope (Set.fromList (fmap fst assignments)))
    (Just (ToyPatch (Map.fromList assignments)))

toyKeyedPatch :: ToySheaf owner -> [(ToyCell, ToyStalk)] -> Either ToyPropagationObstruction (KeyedSectionDelta owner ToyStalk)
toyKeyedPatch sheaf assignments = do
  keyedAssignments <- IntMap.fromList <$> traverse (keyedToyAssignment sheaf) assignments
  pure
    (keyedSectionDeltaFromAssignments keyedAssignments)

toyBenchmarkKeyedBatchDelta :: ToySheaf owner -> Int -> Either ToyPropagationObstruction (KeyedSectionDelta owner ToyStalk)
toyBenchmarkKeyedBatchDelta sheaf count = do
  parentKey <- keyForToyCell sheaf ParentCell
  let parentOrdinal =
        unObjectKey parentKey
  pure (singletonKeyedSectionDelta parentOrdinal (ToyStalk count))

propagateToySectionWith ::
  ToySheaf owner ->
  ToySection owner ->
  Scoped (Set ToyCell) ToyPatch ->
  Either ToyPropagationObstruction (ToySection owner)
propagateToySectionWith sheaf section0 delta = do
  patchValue <- toyKeyedPatchFromScoped sheaf delta
  propagateToyKeyedSectionWith sheaf section0 patchValue

propagateToyKeyedSectionWith ::
  ToySheaf owner ->
  ToySection owner ->
  KeyedSectionDelta owner ToyStalk ->
  Either ToyPropagationObstruction (ToySection owner)
propagateToyKeyedSectionWith sheaf section0 delta =
  propagateToyKeyedBatchInMode sheaf ObserveFinalSection section0 [delta]

propagateToyKeyedBatchWith ::
  ToySheaf owner ->
  ToySection owner ->
  [KeyedSectionDelta owner ToyStalk] ->
  Either ToyPropagationObstruction (ToySection owner)
propagateToyKeyedBatchWith sheaf section0 deltas =
  propagateToyKeyedBatchInMode sheaf ObserveFinalSection section0 deltas

propagateToyKeyedDeltasWith ::
  ToySheaf owner ->
  ToySection owner ->
  [KeyedSectionDelta owner ToyStalk] ->
  Either ToyPropagationObstruction (ToySection owner)
propagateToyKeyedDeltasWith sheaf section0 deltas =
  sdrSection
    <$> mapSectionDescent
      (descendAlgebraPreparedLocalKeyedBatch (tsAlgebraPreparedDescent sheaf) ObserveFinalSection deltas section0)

propagateToyEventStreamWith ::
  ToySheaf owner ->
  ToySection owner ->
  [KeyedSectionDelta owner ToyStalk] ->
  Either ToyPropagationObstruction (ToySection owner)
propagateToyEventStreamWith sheaf section0 deltas =
  sdrSection . snd
    <$> mapSectionDescent
      ( runAlgebraSectionDescentTransaction (tsAlgebraPreparedDescent sheaf) section0 $ \transaction ->
          foldM
            ( \outcome delta ->
                case outcome of
                  Left descentError ->
                    pure (Left descentError)
                  Right () ->
                    transactKeyedSectionDelta transaction delta
            )
            (Right ())
            deltas
      )

propagateToyPreparedProgramObservedWith ::
  ToySheaf owner ->
  ToySection owner ->
  PreparedSectionProgram owner ToyStalk ->
  Either ToyPropagationObstruction (ToySection owner)
propagateToyPreparedProgramObservedWith sheaf section0 program =
  sdrSection
    <$> mapSectionDescent
      (descendAlgebraPreparedSectionProgram (tsAlgebraPreparedDescent sheaf) program section0)

propagateToyKeyedBatchInMode ::
  ToySheaf owner ->
  SectionDescentObservation ->
  ToySection owner ->
  [KeyedSectionDelta owner ToyStalk] ->
  Either ToyPropagationObstruction (ToySection owner)
propagateToyKeyedBatchInMode sheaf observation section0 deltas =
  sdrSection
    <$> mapSectionDescent
      (descendAlgebraPreparedLocalKeyedBatch (tsAlgebraPreparedDescent sheaf) observation deltas section0)

toySectionEntriesWith :: ToySheaf owner -> ToySection owner -> Either ToyPropagationObstruction (Map ToyCell ToyStalk)
toySectionEntriesWith sheaf sectionValue =
  mapSectionStore (totalSectionEntries (tsModel sheaf) sectionValue)

toyStalkAtWith ::
  ToySheaf owner ->
  ToyCell ->
  ToySection owner ->
  Either ToyPropagationObstruction ToyStalk
toyStalkAtWith sheaf cell sectionValue =
  mapLookup cell (totalStalkAt (tsModel sheaf) cell sectionValue)

toyBenchmarkPatches :: Int -> [Scoped (Set ToyCell) ToyPatch]
toyBenchmarkPatches count =
  fmap
    (\value -> toyPatch [(ParentCell, ToyStalk value)])
    [1 .. count]

toyBenchmarkKeyedPatches :: ToySheaf owner -> Int -> Either ToyPropagationObstruction [KeyedSectionDelta owner ToyStalk]
toyBenchmarkKeyedPatches sheaf count = do
  parentKey <- keyForToyCell sheaf ParentCell
  let parentOrdinal =
        unObjectKey parentKey
  pure
    ( fmap
        (singletonKeyedSectionDelta parentOrdinal . ToyStalk)
        [1 .. count]
    )

toyBenchmarkKeyedEdits :: ToySheaf owner -> Int -> Either ToyPropagationObstruction [KeyedSectionEdit owner ToyStalk]
toyBenchmarkKeyedEdits sheaf count = do
  parentKey <- keyForToyCell sheaf ParentCell
  pure
    ( fmap
        ( \value ->
            KeyedSectionEdit
              { kseObjectKey = parentKey,
                kseValue = ToyStalk value
              }
        )
        [1 .. count]
    )

toyBenchmarkPreparedObjectProgram :: ToySheaf owner -> Int -> Either ToyPropagationObstruction (PreparedSectionProgram owner ToyStalk)
toyBenchmarkPreparedObjectProgram sheaf count =
  keyForToyCell sheaf ParentCell
    >>= \parentKey ->
      mapSectionStore
        (prepareSectionObjectProgram (toyPreparedDescent sheaf) parentKey (toyBenchmarkStalkValues count))

toyBenchmarkPreparedEditProgram :: ToySheaf owner -> Int -> Either ToyPropagationObstruction (PreparedSectionProgram owner ToyStalk)
toyBenchmarkPreparedEditProgram sheaf count =
  toyBenchmarkKeyedEdits sheaf count >>= mapSectionStore . prepareSectionProgram (toyPreparedDescent sheaf)

toyBenchmarkStalkValues :: Int -> Vector ToyStalk
toyBenchmarkStalkValues count =
  Vector.fromList (fmap ToyStalk [1 .. count])

toyKeyedPatchFromScoped ::
  ToySheaf owner ->
  Scoped (Set ToyCell) ToyPatch ->
  Either ToyPropagationObstruction (KeyedSectionDelta owner ToyStalk)
toyKeyedPatchFromScoped sheaf delta = do
  let patchValue = maybe mempty unToyPatch (payload delta)
  keyedAssignments <- IntMap.fromList <$> traverse (keyedToyAssignment sheaf) (Map.toAscList patchValue)
  keyedDirtyScope <- keyedScope sheaf (scope delta)
  pure
    KeyedSectionDelta
      { ksdExtent = unionScope keyedDirtyScope (keyedAssignmentScope keyedAssignments),
        ksdAssignments = keyedAssignments
      }

keyedAssignmentScope :: IntMap ToyStalk -> Scope IntSet
keyedAssignmentScope assignments =
  dirtyScope (IntMap.keysSet assignments)

keyedScope :: ToySheaf owner -> Scope (Set ToyCell) -> Either ToyPropagationObstruction (Scope IntSet)
keyedScope sheaf scopeValue =
  foldScope
    (Right cleanScope)
    ( \cells ->
      dirtyScope . IntSet.fromList . fmap unObjectKey
        <$> traverse (keyForToyCell sheaf) (Set.toAscList cells)
    )
    (Right fullScope)
    scopeValue

mapSectionDescent ::
  Either (SectionDescentError ToyCell ToyStalk (DiscreteMismatch ToyStalk)) value ->
  Either ToyPropagationObstruction value
mapSectionDescent =
  either (Left . toyObstructionFromDescent) Right

mapSectionDescentPreparation ::
  Either SectionDescentPreparationError value ->
  Either ToyPropagationObstruction value
mapSectionDescentPreparation =
  either (Left . toyObstructionFromDescentPreparation) Right

toyObstructionFromDescentPreparation ::
  SectionDescentPreparationError ->
  ToyPropagationObstruction
toyObstructionFromDescentPreparation preparationError =
  ToyDescentPreparationFailed preparationError

toyObstructionFromDescent ::
  SectionDescentError ToyCell ToyStalk (DiscreteMismatch ToyStalk) ->
  ToyPropagationObstruction
toyObstructionFromDescent descentError =
  case descentError of
    SectionDescentStoreFailed storeError ->
      ToyPatchSectionStoreFailed storeError
    SectionDescentRestrictionMissing restrictionId ->
      ToySectionRejected
        (SectionCertificationInfrastructureFailed (SectionCertificationRestrictionMissing restrictionId))
    SectionDescentPinnedConflict cell pinnedValue restrictedValue _ ->
      ToyPinnedRestrictionConflict cell pinnedValue restrictedValue
    SectionDescentRejected mismatches ->
      ToySectionRejected (SectionCertificationSemanticallyRejected mismatches)
    SectionDescentFrontierDidNotConverge frontier ->
      ToyDescentFrontierDidNotConverge frontier

keyedToyAssignment :: ToySheaf owner -> (ToyCell, ToyStalk) -> Either ToyPropagationObstruction (Int, ToyStalk)
keyedToyAssignment sheaf (cell, stalk) =
  fmap (\key -> (unObjectKey key, stalk)) (keyForToyCell sheaf cell)

keyedSectionDeltaFromAssignments ::
  IntMap ToyStalk ->
  KeyedSectionDelta owner ToyStalk
keyedSectionDeltaFromAssignments assignments =
  KeyedSectionDelta
    { ksdExtent = keyedAssignmentScope assignments,
      ksdAssignments = assignments
    }

singletonKeyedSectionDelta ::
  Int ->
  ToyStalk ->
  KeyedSectionDelta owner ToyStalk
singletonKeyedSectionDelta objectOrdinal stalk =
  KeyedSectionDelta
    { ksdExtent = dirtyScope (IntSet.singleton objectOrdinal),
      ksdAssignments = IntMap.singleton objectOrdinal stalk
    }

keyForToyCell :: ToySheaf owner -> ToyCell -> Either ToyPropagationObstruction ObjectKey
keyForToyCell sheaf cell =
  case denseIndexKeyOf cell (sheafModelObjects (toySheafModel sheaf)) of
    Just key ->
      Right key
    Nothing ->
      Left (ToyPatchSectionStoreFailed (SectionStoreUnknownCell cell))

mapSheafModel :: Either (SheafModelBuildError ToyCell) value -> Either ToyPropagationObstruction value
mapSheafModel =
  either (Left . ToySheafModelRejected) Right

mapSectionConstruction :: Either (SectionConstructionError ToyCell) section -> Either ToyPropagationObstruction section
mapSectionConstruction =
  either (Left . ToyInitialSectionRejected) Right

mapSectionStore :: Either (SectionStoreError ToyCell) value -> Either ToyPropagationObstruction value
mapSectionStore =
  either (Left . ToyPatchSectionStoreFailed) Right

mapLookup :: ToyCell -> Either (SectionLookupError ToyCell) value -> Either ToyPropagationObstruction value
mapLookup cell =
  either (Left . ToyRestrictionLookupFailed cell) Right

toyIdentity :: ToyCell -> CheckedMorphism ToyCell ToyMorphism
toyIdentity cell =
  CheckedMorphism
    { cmSource = cell,
      cmTarget = cell,
      cmWitness = ToyIdentity cell
    }

parentToChild :: CheckedMorphism ToyCell ToyMorphism
parentToChild =
  CheckedMorphism
    { cmSource = ParentCell,
      cmTarget = ChildCell,
      cmWitness = ParentToChild
    }

childCover :: CoveringFamily ToyCell ToyMorphism
childCover =
  coveringFamilyFromTargetedWitnesses
    ChildCell
    ( (ParentCell, ParentToChild)
        :| [(ChildCell, ToyIdentity ChildCell)]
    )

composeToyMorphisms ::
  CheckedMorphism ToyCell ToyMorphism ->
  CheckedMorphism ToyCell ToyMorphism ->
  Maybe (CheckedMorphism ToyCell ToyMorphism)
composeToyMorphisms outer inner =
  case (cmWitness outer, cmWitness inner) of
    (ToyIdentity _, _) -> Just inner
    (_, ToyIdentity _) -> Just outer
    (ParentToChild, ParentToChild) -> Nothing

toyPullbackSquare ::
  CheckedMorphism ToyCell ToyMorphism ->
  CheckedMorphism ToyCell ToyMorphism ->
  Maybe (PullbackSquare ToyCell ToyMorphism)
toyPullbackSquare left right =
  case (cmWitness left, cmWitness right) of
    (ToyIdentity _, _) ->
      Just (PullbackSquare left right (cmSource right) right (toyIdentity (cmSource right)))
    (_, ToyIdentity _) ->
      Just (PullbackSquare left right (cmSource left) (toyIdentity (cmSource left)) left)
    (ParentToChild, ParentToChild) ->
      Just (PullbackSquare left right ParentCell (toyIdentity ParentCell) (toyIdentity ParentCell))
