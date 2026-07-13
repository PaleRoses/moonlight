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
    toySheaf,
    toySheafModel,
    toyPreparedDescent,
    initialToySection,
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
    propagateToyEventStream,
    propagateToyEventStreamWith,
    propagateToyPreparedProgramObservedWith,
    toySectionEntries,
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
  ( ModelFingerprint,
    SheafModel,
    SheafModelBuildError,
    prepareSheafModel,
    sheafModelFingerprint,
    sheafModelObjects,
    sheafModelVersion,
  )
import Moonlight.Sheaf.Section.Morphism
  ( RestrictionParts (..),
    unitIncidenceRestriction,
  )
import Moonlight.Sheaf.Index.Dense (denseIndexKeyOf)
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectKey,
    SheafModelVersion,
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
  ( AlgebraPreparedSectionDescent (..),
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
    sdrSection,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    CoverConstructionError,
    CoveringFamily,
    PullbackSquare (..),
    Site (..),
    cmSource,
    cmTarget,
    mkCoveringFamily,
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

data ToySheaf = ToySheaf
  { tsModel :: !(SheafModel ToyCell (CompiledRestriction ToySite)),
    tsAlgebraPreparedDescent ::
      !( AlgebraPreparedSectionDescent
          ToyCell
          (CompiledRestriction ToySite)
          ToyStalk
          (DiscreteMismatch ToyStalk)
          (DiscreteRepairObstruction ToyStalk)
       )
  }

type ToySection = TotalSectionStore ToyCell ToyStalk

newtype ToyPatch = ToyPatch
  { unToyPatch :: Map ToyCell ToyStalk
  }
  deriving stock (Eq, Show)

data ToyPropagationObstruction
  = ToySheafLawRejected !(NonEmpty String)
  | ToySheafModelRejected !(SheafModelBuildError ToyCell)
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
      ChildCell -> either (const []) pure childCover

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

toySheafModel :: ToySheaf -> SheafModel ToyCell (CompiledRestriction ToySite)
toySheafModel =
  tsModel

toyPreparedDescent :: ToySheaf -> PreparedSectionDescent ToyCell (CompiledRestriction ToySite)
toyPreparedDescent =
  apsdPreparedDescent . tsAlgebraPreparedDescent

toySheaf :: Either ToyPropagationObstruction ToySheaf
toySheaf =
  case NonEmpty.nonEmpty (siteLawFailures ToySite) of
    Just failures ->
      Left (ToySheafLawRejected (fmap show failures))
    Nothing ->
      do
        model <-
          mapSheafModel
            ( prepareSheafModel
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
            )
        preparedDescent <- mapSectionDescentPreparation (prepareSectionDescent model)
        pure
          ToySheaf
            { tsModel = model,
              tsAlgebraPreparedDescent = prepareAlgebraSectionDescent preparedDescent toyAlgebra
            }

initialToySection :: ToyStalk -> Either ToyPropagationObstruction ToySection
initialToySection stalkValue = do
  sheaf <- toySheaf
  initialToySectionWith sheaf stalkValue

initialToySectionWith :: ToySheaf -> ToyStalk -> Either ToyPropagationObstruction ToySection
initialToySectionWith sheaf stalkValue =
  mapSectionConstruction
    (mkTotalSectionStore (tsModel sheaf) (Map.fromList [(ParentCell, stalkValue), (ChildCell, stalkValue)]))

toyPatch :: [(ToyCell, ToyStalk)] -> Scoped (Set ToyCell) ToyPatch
toyPatch assignments =
  scopedDelta
    (dirtyScope (Set.fromList (fmap fst assignments)))
    (Just (ToyPatch (Map.fromList assignments)))

toyKeyedPatch :: ToySheaf -> [(ToyCell, ToyStalk)] -> Either ToyPropagationObstruction (KeyedSectionDelta ToyStalk)
toyKeyedPatch sheaf assignments = do
  keyedAssignments <- IntMap.fromList <$> traverse (keyedToyAssignment sheaf) assignments
  pure
    ( keyedSectionDeltaFromAssignments
        (sheafModelFingerprint (toySheafModel sheaf))
        (sheafModelVersion (toySheafModel sheaf))
        keyedAssignments
    )

toyBenchmarkKeyedBatchDelta :: ToySheaf -> Int -> Either ToyPropagationObstruction (KeyedSectionDelta ToyStalk)
toyBenchmarkKeyedBatchDelta sheaf count = do
  parentKey <- keyForToyCell sheaf ParentCell
  let modelVersion =
        sheafModelVersion (toySheafModel sheaf)
      modelFingerprint =
        sheafModelFingerprint (toySheafModel sheaf)
      parentOrdinal =
        unObjectKey parentKey
  pure (singletonKeyedSectionDelta modelFingerprint modelVersion parentOrdinal (ToyStalk count))

propagateToySectionWith ::
  ToySheaf ->
  ToySection ->
  Scoped (Set ToyCell) ToyPatch ->
  Either ToyPropagationObstruction ToySection
propagateToySectionWith sheaf section0 delta = do
  patchValue <- toyKeyedPatchFromScoped sheaf delta
  propagateToyKeyedSectionWith sheaf section0 patchValue

propagateToyKeyedSectionWith ::
  ToySheaf ->
  ToySection ->
  KeyedSectionDelta ToyStalk ->
  Either ToyPropagationObstruction ToySection
propagateToyKeyedSectionWith sheaf section0 delta =
  propagateToyKeyedBatchInMode sheaf ObserveFinalSection section0 [delta]

propagateToyKeyedBatchWith ::
  ToySheaf ->
  ToySection ->
  [KeyedSectionDelta ToyStalk] ->
  Either ToyPropagationObstruction ToySection
propagateToyKeyedBatchWith sheaf section0 deltas =
  propagateToyKeyedBatchInMode sheaf ObserveFinalSection section0 deltas

propagateToyKeyedDeltasWith ::
  ToySheaf ->
  ToySection ->
  [KeyedSectionDelta ToyStalk] ->
  Either ToyPropagationObstruction ToySection
propagateToyKeyedDeltasWith sheaf section0 deltas =
  sdrSection
    <$> mapSectionDescent
      (descendAlgebraPreparedLocalKeyedBatch (tsAlgebraPreparedDescent sheaf) ObserveFinalSection deltas section0)

propagateToyEventStream ::
  ToySection ->
  [KeyedSectionDelta ToyStalk] ->
  Either ToyPropagationObstruction ToySection
propagateToyEventStream section0 deltas = do
  sheaf <- toySheaf
  propagateToyEventStreamWith sheaf section0 deltas

propagateToyEventStreamWith ::
  ToySheaf ->
  ToySection ->
  [KeyedSectionDelta ToyStalk] ->
  Either ToyPropagationObstruction ToySection
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
  ToySheaf ->
  ToySection ->
  PreparedSectionProgram ToyStalk ->
  Either ToyPropagationObstruction ToySection
propagateToyPreparedProgramObservedWith sheaf section0 program =
  sdrSection
    <$> mapSectionDescent
      (descendAlgebraPreparedSectionProgram (tsAlgebraPreparedDescent sheaf) program section0)

propagateToyKeyedBatchInMode ::
  ToySheaf ->
  SectionDescentObservation ->
  ToySection ->
  [KeyedSectionDelta ToyStalk] ->
  Either ToyPropagationObstruction ToySection
propagateToyKeyedBatchInMode sheaf observation section0 deltas =
  sdrSection
    <$> mapSectionDescent
      (descendAlgebraPreparedLocalKeyedBatch (tsAlgebraPreparedDescent sheaf) observation deltas section0)

toySectionEntries :: ToySection -> Either ToyPropagationObstruction (Map ToyCell ToyStalk)
toySectionEntries sectionValue = do
  sheaf <- toySheaf
  toySectionEntriesWith sheaf sectionValue

toySectionEntriesWith :: ToySheaf -> ToySection -> Either ToyPropagationObstruction (Map ToyCell ToyStalk)
toySectionEntriesWith sheaf sectionValue =
  mapSectionStore (totalSectionEntries (tsModel sheaf) sectionValue)

toyStalkAtWith ::
  ToySheaf ->
  ToyCell ->
  ToySection ->
  Either ToyPropagationObstruction ToyStalk
toyStalkAtWith sheaf cell sectionValue =
  mapLookup cell (totalStalkAt (tsModel sheaf) cell sectionValue)

toyBenchmarkPatches :: Int -> [Scoped (Set ToyCell) ToyPatch]
toyBenchmarkPatches count =
  fmap
    (\value -> toyPatch [(ParentCell, ToyStalk value)])
    [1 .. count]

toyBenchmarkKeyedPatches :: ToySheaf -> Int -> Either ToyPropagationObstruction [KeyedSectionDelta ToyStalk]
toyBenchmarkKeyedPatches sheaf count = do
  parentKey <- keyForToyCell sheaf ParentCell
  let modelVersion =
        sheafModelVersion (toySheafModel sheaf)
      modelFingerprint =
        sheafModelFingerprint (toySheafModel sheaf)
      parentOrdinal =
        unObjectKey parentKey
  pure
    ( fmap
        (singletonKeyedSectionDelta modelFingerprint modelVersion parentOrdinal . ToyStalk)
        [1 .. count]
    )

toyBenchmarkKeyedEdits :: ToySheaf -> Int -> Either ToyPropagationObstruction [KeyedSectionEdit ToyStalk]
toyBenchmarkKeyedEdits sheaf count = do
  parentKey <- keyForToyCell sheaf ParentCell
  let modelVersion =
        sheafModelVersion (toySheafModel sheaf)
      modelFingerprint =
        sheafModelFingerprint (toySheafModel sheaf)
  pure
    ( fmap
        ( \value ->
            KeyedSectionEdit
              { kseModelFingerprint = modelFingerprint,
                kseModelVersion = modelVersion,
                kseObjectKey = parentKey,
                kseValue = ToyStalk value
              }
        )
        [1 .. count]
    )

toyBenchmarkPreparedObjectProgram :: ToySheaf -> Int -> Either ToyPropagationObstruction (PreparedSectionProgram ToyStalk)
toyBenchmarkPreparedObjectProgram sheaf count =
  keyForToyCell sheaf ParentCell
    >>= \parentKey ->
      mapSectionStore
        (prepareSectionObjectProgram (toyPreparedDescent sheaf) parentKey (toyBenchmarkStalkValues count))

toyBenchmarkPreparedEditProgram :: ToySheaf -> Int -> Either ToyPropagationObstruction (PreparedSectionProgram ToyStalk)
toyBenchmarkPreparedEditProgram sheaf count =
  toyBenchmarkKeyedEdits sheaf count >>= mapSectionStore . prepareSectionProgram (toyPreparedDescent sheaf)

toyBenchmarkStalkValues :: Int -> Vector ToyStalk
toyBenchmarkStalkValues count =
  Vector.fromList (fmap ToyStalk [1 .. count])

toyKeyedPatchFromScoped ::
  ToySheaf ->
  Scoped (Set ToyCell) ToyPatch ->
  Either ToyPropagationObstruction (KeyedSectionDelta ToyStalk)
toyKeyedPatchFromScoped sheaf delta = do
  let patchValue = maybe mempty unToyPatch (payload delta)
      modelVersion =
        sheafModelVersion (toySheafModel sheaf)
      modelFingerprint =
        sheafModelFingerprint (toySheafModel sheaf)
  keyedAssignments <- IntMap.fromList <$> traverse (keyedToyAssignment sheaf) (Map.toAscList patchValue)
  keyedDirtyScope <- keyedScope sheaf (scope delta)
  pure
    KeyedSectionDelta
      { ksdModelFingerprint = modelFingerprint,
        ksdModelVersion = modelVersion,
        ksdExtent = unionScope keyedDirtyScope (keyedAssignmentScope keyedAssignments),
        ksdAssignments = keyedAssignments
      }

keyedAssignmentScope :: IntMap ToyStalk -> Scope IntSet
keyedAssignmentScope assignments =
  dirtyScope (IntMap.keysSet assignments)

keyedScope :: ToySheaf -> Scope (Set ToyCell) -> Either ToyPropagationObstruction (Scope IntSet)
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
  case preparationError of
    SectionDescentPreparationRestrictionMissing restrictionId ->
      ToySectionRejected
        (SectionCertificationInfrastructureFailed (SectionCertificationRestrictionMissing restrictionId))

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

keyedToyAssignment :: ToySheaf -> (ToyCell, ToyStalk) -> Either ToyPropagationObstruction (Int, ToyStalk)
keyedToyAssignment sheaf (cell, stalk) =
  fmap (\key -> (unObjectKey key, stalk)) (keyForToyCell sheaf cell)

keyedSectionDeltaFromAssignments ::
  ModelFingerprint ->
  SheafModelVersion ->
  IntMap ToyStalk ->
  KeyedSectionDelta ToyStalk
keyedSectionDeltaFromAssignments modelFingerprint modelVersion assignments =
  KeyedSectionDelta
    { ksdModelFingerprint = modelFingerprint,
      ksdModelVersion = modelVersion,
      ksdExtent = keyedAssignmentScope assignments,
      ksdAssignments = assignments
    }

singletonKeyedSectionDelta ::
  ModelFingerprint ->
  SheafModelVersion ->
  Int ->
  ToyStalk ->
  KeyedSectionDelta ToyStalk
singletonKeyedSectionDelta modelFingerprint modelVersion objectOrdinal stalk =
  KeyedSectionDelta
    { ksdModelFingerprint = modelFingerprint,
      ksdModelVersion = modelVersion,
      ksdExtent = dirtyScope (IntSet.singleton objectOrdinal),
      ksdAssignments = IntMap.singleton objectOrdinal stalk
    }

keyForToyCell :: ToySheaf -> ToyCell -> Either ToyPropagationObstruction ObjectKey
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

childCover :: Either (CoverConstructionError ToyCell) (CoveringFamily ToyCell ToyMorphism)
childCover =
  mkCoveringFamily ChildCell (parentToChild :| [toyIdentity ChildCell])

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
