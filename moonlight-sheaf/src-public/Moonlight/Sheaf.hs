{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.Sheaf
  ( PreparedSite,
    Section,
    PartialSection,
    PreparedCover,
    GlobalSection,
    ChangedObjects (..),
    SiteSpec (..),
    GluingAlgebra (..),
    CoverGluingFailure (..),
    CompileError (..),
    PreparedCoversRefusal (..),
    RepairResult (..),
    RepairStatus (..),
    initialSheafModelVersion,
    siteSpec,
    compile,
    preparedCovers,
    preparedCoverTarget,
    preparedCoverSlots,
    preparedCoverSources,
    preparedCoverSize,
    section,
    tabulateSection,
    partial,
    partialEntries,
    assign,
    assignOne,
    stalkAt,
    entries,
    sectionEpoch,
    changedObjects,
    certify,
    sectionCompatibilityVerdict,
    isSectionCompatible,
    globalSection,
    globalSectionUnderlying,
    repair,
    matching,
    matchingTarget,
    matchingCover,
    matchingSections,
    matchingPreparedCover,
    certifyMatching,
    glue,
    Site (..),
    CheckedMorphism (..),
    PullbackSquare (..),
    CompiledRestriction,
    restrictionMorphism,
    CoveringFamily,
    CoverSlotKey,
    CoverSlot,
    coverSlotKey,
    coverSlotArrow,
    CoverConstructionError (..),
    mkCoveringFamily,
    coverTarget,
    coverArrows,
    coverSources,
    coverSize,
    MatchingFamily,
    CompatibleMatchingFamily,
    compatibleMatchingFamilyUnderlying,
    MatchingFamilyConstructionError (..),
    MatchingFailure (..),
    GluingObstruction (..),
    GluingFailure (..),
    AmalgamationLocalityFailure (..),
    Amalgamation,
    amalgamationMatchingFamily,
    amalgamatedStalk,
    certifyAmalgamation,
    CoverStalkUniverse,
    UniverseShapeError (..),
    coverStalkUniverse,
    GhostSection (..),
    SeparatedCover,
    SeparatedCoverRefusal (..),
    separatedCover,
    SeparatedUniquenessRefusal (..),
    UniqueAmalgamation,
    certifyUniqueAmalgamation,
    SeparatedResolutionRefusal (..),
    resolveUniqueAmalgamation,
    SeparatedEqualityRefusal (..),
    SeparatedEqualityVerdict (..),
    separatedLocalEqualityAt,
    uniqueAmalgamationUnderlying,
    FiniteMeetMorphism,
    FiniteMeetSite,
    FiniteMeetSiteBuildError (..),
    FiniteMeetSiteSpec (..),
    finiteMeetMorphism,
    finiteMeetRefines,
    finiteMeetSiteCells,
    finiteMeetSiteCovers,
    finiteMeetSiteMeet,
    finiteMeetSiteRefinements,
    mkFiniteMeetSite,
    RestrictionKind (..),
    IncidenceCoefficient,
    mkIncidenceCoefficient,
    incidenceCoefficientValue,
    mkIncidenceRestriction,
    unitIncidenceRestriction,
    negativeUnitIncidenceRestriction,
    SiteLawFailure (..),
    isIdentityMorphism,
    siteRestrictionMorphisms,
    siteLawFailures,
    Verdict (..),
    SearchVerdict (..),
    decidedSearchVerdict,
    completeSearchVerdict,
    searchVerdictObstructions,
    searchVerdictRefusals,
    searchVerdictDecided,
    CoverSearchBudget (..),
    unboundedCoverSearchBudget,
    CoverSearchCost (..),
    CoverSearchRefusal (..),
    DescentReport (..),
    DescentOutcome (..),
    SectionCertification (..),
    SectionCertificationError (..),
    SectionCertificationFailure (..),
    RepairObstruction (..),
    RepairDiagnostics (..),
    SectionConstructionError (..),
    SectionLookupError (..),
    SectionStoreError (..),
  )
where

import Data.Bifunctor (first)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Moonlight.Delta.Scope
  ( foldScope,
  )
import Moonlight.Sheaf.Descent.Core
  ( DescentOutcome (..),
    DescentReport (..),
  )
import Moonlight.Sheaf.Descent.Kernel
  ( CoverSearchBudget (..),
    CoverSearchCost (..),
    CoverSearchRefusal (..),
    unboundedCoverSearchBudget,
  )
import Moonlight.Sheaf.Index.Dense qualified as DenseIndex
import Moonlight.Sheaf.Section.Certified
  ( SectionCertification (..),
    SectionCertificationError (..),
    SectionCertificationFailure (..),
  )
import Moonlight.Sheaf.Section.Certified qualified as Certified
import Moonlight.Sheaf.Section.Model qualified as Model
import Moonlight.Sheaf.Section.Morphism
  ( IncidenceCoefficient,
    RestrictionId,
    RestrictionKind (..),
    RestrictionParts (..),
    incidenceCoefficientValue,
    mkIncidenceCoefficient,
    mkIncidenceRestriction,
    negativeUnitIncidenceRestriction,
    unitIncidenceRestriction,
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( SheafModelVersion (..),
    initialSheafModelVersion,
  )
import Moonlight.Sheaf.Section.ObjectIndex qualified as ObjectIndex
import Moonlight.Sheaf.Section.Repair
  ( RepairDiagnostics (..),
    RepairObstruction (..),
    RepairStatus (..),
  )
import Moonlight.Sheaf.Section.Repair qualified as Repair
import Moonlight.Sheaf.Section.Restriction
  ( RestrictionIndexError (..),
  )
import Moonlight.Sheaf.Section.Store.Types
import Moonlight.Sheaf.Section.Store.State qualified as Store
import Moonlight.Sheaf.Section.Store.Types qualified as Store
import Moonlight.Sheaf.Verdict
  ( SearchVerdict (..),
    Verdict (..),
    completeSearchVerdict,
    decidedSearchVerdict,
    searchVerdictDecided,
    searchVerdictObstructions,
    searchVerdictRefusals,
    verdictAllowed,
  )
import Moonlight.Sheaf.Internal.PublicModel
  ( Amalgamation (..),
    CompatibleMatchingFamily (..),
    CoverStalkUniverse (..),
    GlobalSection (..),
    MatchingFamily (..),
    PartialSection (..),
    PreparedCover (..),
    PreparedSite (..),
    RepairResult (..),
    Section (..),
    SeparatedCover (..),
    UniqueAmalgamation (..),
  )
import Moonlight.Sheaf.Presheaf.Core
  ( CompiledRestriction (..),
  )
import Moonlight.Sheaf.Section.Stalk
  ( StalkAlgebra,
  )
import Moonlight.Sheaf.Sheaf.Gluing
  ( AmalgamationLocalityFailure (..),
    GhostSection (..),
    GluingFailure (..),
    GluingObstruction (..),
    MatchingFailure (..),
    MatchingFamilyConstructionError (..),
    SeparatedCoverRefusal (..),
    SeparatedEqualityRefusal (..),
    SeparatedEqualityVerdict (..),
    SeparatedResolutionRefusal (..),
    SeparatedUniquenessRefusal (..),
  )
import Moonlight.Sheaf.Sheaf.Gluing qualified as Gluing
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    CoverConstructionError (..),
    CoveringFamily,
    PullbackSquare (..),
    Site (..),
    SiteLawFailure (..),
    coverArrows,
    coverSize,
    coverSources,
    coverTarget,
    isIdentityMorphism,
    mkCoveringFamily,
    siteRestrictionMorphisms,
  )
import Moonlight.Sheaf.Site.Construction.FiniteMeet
  ( FiniteMeetMorphism,
    FiniteMeetSite,
    FiniteMeetSiteBuildError (..),
    FiniteMeetSiteSpec (..),
    finiteMeetMorphism,
    finiteMeetRefines,
    finiteMeetSiteCells,
    finiteMeetSiteCovers,
    finiteMeetSiteMeet,
    finiteMeetSiteRefinements,
    mkFiniteMeetSite,
  )
import Moonlight.Sheaf.Site.Plan
  ( CoverPlan (..),
    CoverSlot (..),
    CoverSlotKey,
    SitePlanBuildError,
    effectiveCoverFamily,
    effectiveCoverSlots,
    effectiveCoverSlotCount,
    prepareSitePlans,
    siteCoverPlansAt,
  )
import Moonlight.Sheaf.Site.Class.Validation
  ( siteLawFailures,
  )

type SiteSpec :: Type -> Type
data SiteSpec site = SiteSpec
  { siteSpecVersion :: !SheafModelVersion,
    siteSpecSite :: !site,
    siteSpecRestrictionKind :: CheckedMorphism (SiteObject site) (SiteMorphism site) -> RestrictionKind
  }

type ChangedObjects :: Type -> Type
data ChangedObjects site
  = NoObjectChanges
  | AllObjectsChanged
  | ChangedObjects !(Set (SiteObject site))

deriving stock instance Eq (SiteObject site) => Eq (ChangedObjects site)

deriving stock instance Show (SiteObject site) => Show (ChangedObjects site)

type CompileError :: Type -> Type -> Type
data CompileError cell morphism
  = SheafRestrictionUnknownSource !cell
  | SheafRestrictionUnknownTarget !cell
  | SheafRestrictionUnknownId !RestrictionId
  | SheafRestrictionDuplicateId !RestrictionId
  | SheafRestrictionNonDenseId !RestrictionId !RestrictionId
  | SheafRestrictionZeroIncidenceCoefficient !cell !cell
  | SheafCoverPreparationFailed !(SitePlanBuildError cell morphism)
  | SheafSiteLawFailed !(NonEmpty (SiteLawFailure cell morphism))
  deriving stock (Eq, Show)

type PreparedCoversRefusal :: Type -> Type
data PreparedCoversRefusal obj
  = PreparedCoversUnknownObject !obj
  deriving stock (Eq, Show)

type CoverGluingFailure :: Type -> Type -> Type -> Type -> Type
data CoverGluingFailure obj mor mismatch gluingFailure
  = CoverMatchingFamilyConstructionFailed !MatchingFamilyConstructionError
  | CoverAmalgamationFailed
      !(GluingFailure obj mor mismatch gluingFailure)
  deriving stock (Eq, Show)

type UniverseShapeError :: Type
data UniverseShapeError = UniverseShapeError
  { universeExpectedSlotCount :: !Int,
    universeActualSlotCount :: !Int
  }
  deriving stock (Eq, Show)

type GluingAlgebra :: Type -> Type -> Type -> Type
data GluingAlgebra site stalk gluingFailure = GluingAlgebra
  { gaAmalgamate ::
      site ->
      CompatibleMatchingFamily site stalk ->
      Either (GluingObstruction (SiteObject site) gluingFailure) stalk
  }

siteSpec :: site -> SiteSpec site
siteSpec site =
  SiteSpec
    { siteSpecVersion = initialSheafModelVersion,
      siteSpecSite = site,
      siteSpecRestrictionKind = const unitIncidenceRestriction
    }

compile ::
  (Site site, Ord (SiteMorphism site)) =>
  SiteSpec site ->
  Either (CompileError (SiteObject site) (SiteMorphism site)) (PreparedSite site)
compile specification = do
  validateSiteSpecSite (siteSpecSite specification)

  let site = siteSpecSite specification
      objects = ObjectIndex.mkObjectIndex (siteObjects site)
  model <-
    mapPreparationError
      ( Model.prepareSheafModel
          (siteSpecVersion specification)
          objects
          ( \checkedMorphism ->
              RestrictionParts
                { partKind = siteSpecRestrictionKind specification checkedMorphism,
                  partSource = cmSource checkedMorphism,
                  partTarget = cmTarget checkedMorphism,
                  partWitness = CompiledRestriction site checkedMorphism
                }
          )
          (siteRestrictionMorphisms site)
      )
  sitePlans <- first SheafCoverPreparationFailed (prepareSitePlans objects site)
  Right
    PreparedSite
      { preparedSiteInternal = site,
        preparedSiteModelInternal = model,
        preparedSitePlansInternal = sitePlans
      }

validateSiteSpecSite ::
  (Site site, Ord (SiteMorphism site)) =>
  site ->
  Either
    (CompileError (SiteObject site) (SiteMorphism site))
    ()
validateSiteSpecSite site =
  case NonEmpty.nonEmpty (siteLawFailures site) of
    Nothing ->
      Right ()
    Just failures ->
      Left (SheafSiteLawFailed failures)

preparedCovers ::
  Site site =>
  PreparedSite site ->
  SiteObject site ->
  Either (PreparedCoversRefusal (SiteObject site)) [PreparedCover site]
preparedCovers preparedSite target =
  case DenseIndex.denseIndexKeyOf target (Model.sheafModelObjects (preparedSiteModelInternal preparedSite)) of
    Nothing -> Left (PreparedCoversUnknownObject target)
    Just targetKey ->
      Right
        ( fmap
            ( \coverPlan ->
                PreparedCover
                  { preparedCoverOwnerInternal = preparedSite,
                    preparedCoverPlanInternal = coverPlan
                  }
            )
            (siteCoverPlansAt targetKey (preparedSitePlansInternal preparedSite))
        )

preparedCoverTarget :: PreparedCover site -> SiteObject site
preparedCoverTarget preparedCover =
  coverTarget (effectiveCoverFamily (cpEffectiveCover (preparedCoverPlanInternal preparedCover)))

preparedCoverSlots ::
  PreparedCover site ->
  Vector (CoverSlot (SiteObject site) (SiteMorphism site))
preparedCoverSlots =
  Vector.fromList
    . IntMap.elems
    . effectiveCoverSlots
    . cpEffectiveCover
    . preparedCoverPlanInternal

preparedCoverSources :: PreparedCover site -> Vector (SiteObject site)
preparedCoverSources =
  Vector.map (cmSource . coverSlotArrow) . preparedCoverSlots

preparedCoverSize :: PreparedCover site -> Int
preparedCoverSize =
  effectiveCoverSlotCount . cpEffectiveCover . preparedCoverPlanInternal

section :: Site site => PreparedSite site -> Map (SiteObject site) stalk -> Either (SectionConstructionError (SiteObject site)) (Section site stalk)
section preparedSite entryMap =
  fmap
    ( \sectionStore ->
        Section
          { sectionOwnerInternal = preparedSite,
            sectionStoreInternal = sectionStore
          }
    )
    (Store.mkTotalSectionStore (preparedSiteModelInternal preparedSite) entryMap)

tabulateSection :: PreparedSite site -> (SiteObject site -> stalk) -> Section site stalk
tabulateSection preparedSite initialize =
  Section
    { sectionOwnerInternal = preparedSite,
      sectionStoreInternal =
        Store.emptyTotalSectionStoreWith (preparedSiteModelInternal preparedSite) initialize
    }

partial :: Site site => PreparedSite site -> Map (SiteObject site) stalk -> Either (SectionStoreError (SiteObject site)) (PartialSection site stalk)
partial preparedSite entryMap =
  fmap
    ( \partialSectionStore ->
        PartialSection
          { partialSectionOwnerInternal = preparedSite,
            partialSectionStoreInternal = partialSectionStore
          }
    )
    (Store.mkPartialSectionStore (preparedSiteModelInternal preparedSite) entryMap)

partialEntries :: PartialSection site stalk -> Map (SiteObject site) stalk
partialEntries =
  Store.partialSectionEntries . partialSectionStoreInternal
{-# INLINE partialEntries #-}

assign :: Site site => Map (SiteObject site) stalk -> Section site stalk -> Either (SectionStoreError (SiteObject site)) (Section site stalk)
assign assignments sectionValue =
  fmap
    ( \updatedSectionStore ->
        sectionValue {sectionStoreInternal = updatedSectionStore}
    )
    ( Store.assignLocal
        (preparedSiteModelInternal preparedSite)
        Store.SectionDelta
          { Store.sdModelFingerprint = Model.sheafModelFingerprint (preparedSiteModelInternal preparedSite),
            Store.sdModelVersion = Model.sheafModelVersion (preparedSiteModelInternal preparedSite),
            Store.sdAssignments = assignments
          }
        sectionStore
    )
  where
    preparedSite = sectionOwnerInternal sectionValue
    sectionStore = sectionStoreInternal sectionValue

assignOne :: Site site => SiteObject site -> stalk -> Section site stalk -> Either (SectionStoreError (SiteObject site)) (Section site stalk)
assignOne cell stalkValue =
  assign (Map.singleton cell stalkValue)

stalkAt :: Site site => SiteObject site -> Section site stalk -> Either (SectionLookupError (SiteObject site)) stalk
stalkAt cell sectionValue =
  Store.totalStalkAt
    (preparedSiteModelInternal (sectionOwnerInternal sectionValue))
    cell
    (sectionStoreInternal sectionValue)

entries :: Site site => Section site stalk -> Map (SiteObject site) stalk
entries sectionValue =
  Map.fromList
    ( zip
        (Model.modelCells (preparedSiteModelInternal (sectionOwnerInternal sectionValue)))
        (Vector.toList (Store.unDenseSection (Store.totalSectionDenseValues (sectionStoreInternal sectionValue))))
    )

sectionEpoch :: Section site stalk -> SectionEpoch
sectionEpoch =
  Store.totalSectionEpoch . sectionStoreInternal

changedObjects :: Ord (SiteObject site) => Section site stalk -> ChangedObjects site
changedObjects sectionValue =
  foldScope
    NoObjectChanges
    ( \keys ->
      ChangedObjects
        ( Set.fromList
            ( mapMaybe
                ( \objectKey ->
                    DenseIndex.denseIndexValueAt
                      (ObjectIndex.ObjectKey objectKey)
                      (Model.sheafModelObjects (preparedSiteModelInternal preparedSite))
                )
                (IntSet.toList keys)
            )
        )
    )
    AllObjectsChanged
    (Store.totalSectionExtent sectionStore)
  where
    preparedSite = sectionOwnerInternal sectionValue
    sectionStore = sectionStoreInternal sectionValue

restrictionMorphism :: CompiledRestriction site -> CheckedMorphism (SiteObject site) (SiteMorphism site)
restrictionMorphism =
  crMorphism

certify ::
  Site site =>
  StalkAlgebra (CompiledRestriction site) stalk mismatch repair ->
  Section site stalk ->
  Either
    (Certified.SectionCertificationError (SiteObject site))
    (Certified.SectionCertification (SiteObject site) mismatch)
certify stalkAlgebra sectionValue =
  Certified.certifySectionCompatibility
    (preparedSiteModelInternal (sectionOwnerInternal sectionValue))
    stalkAlgebra
    (sectionStoreInternal sectionValue)

sectionCompatibilityVerdict ::
  Site site =>
  StalkAlgebra (CompiledRestriction site) stalk mismatch repair ->
  Section site stalk ->
  Verdict () (Certified.SectionCertificationFailure (SiteObject site) mismatch)
sectionCompatibilityVerdict stalkAlgebra sectionValue =
  case certify stalkAlgebra sectionValue of
    Right SectionCertified ->
      Accepted ()
    Right (SectionRejected mismatches) ->
      Rejected (Certified.SectionCertificationSemanticallyRejected mismatches)
    Left certificationError ->
      Rejected (Certified.SectionCertificationInfrastructureFailed certificationError)

isSectionCompatible ::
  Site site =>
  StalkAlgebra (CompiledRestriction site) stalk mismatch repair ->
  Section site stalk ->
  Bool
isSectionCompatible stalkAlgebra =
  verdictAllowed . sectionCompatibilityVerdict stalkAlgebra

globalSection ::
  Site site =>
  StalkAlgebra (CompiledRestriction site) stalk mismatch repair ->
  Section site stalk ->
  Either (Certified.SectionCertificationFailure (SiteObject site) mismatch) (GlobalSection site stalk)
globalSection stalkAlgebra sectionValue =
  fmap
    ( \globalSectionValue ->
        GlobalSection
          { globalSectionOwnerInternal = preparedSite,
            globalSectionValueInternal = globalSectionValue
          }
    )
    (Certified.mkGlobalSection (preparedSiteModelInternal preparedSite) stalkAlgebra sectionStore)
  where
    preparedSite = sectionOwnerInternal sectionValue
    sectionStore = sectionStoreInternal sectionValue

globalSectionUnderlying :: GlobalSection site stalk -> Section site stalk
globalSectionUnderlying globalSectionValue =
  Section
    { sectionOwnerInternal = globalSectionOwnerInternal globalSectionValue,
      sectionStoreInternal =
        Certified.globalUnderlyingSection (globalSectionValueInternal globalSectionValue)
    }

repair :: Site site => StalkAlgebra (CompiledRestriction site) stalk mismatch repair -> PartialSection site stalk -> Either (Repair.RepairObstruction (SiteObject site) repair) (RepairResult site stalk mismatch)
repair stalkAlgebra partialSectionValue =
  fmap
    ( \repairResult ->
        RepairResult
          { repairedAssignment =
              PartialSection
                { partialSectionOwnerInternal = preparedSite,
                  partialSectionStoreInternal = Repair.repairedPartialSection repairResult
                },
            repairDiagnostics =
              Repair.repairPartialDiagnostics repairResult,
            repairStatus =
              Repair.repairPartialStatus repairResult
          }
    )
    (Repair.repairPartialSection (preparedSiteModelInternal preparedSite) stalkAlgebra assignmentValue)
  where
    preparedSite = partialSectionOwnerInternal partialSectionValue
    assignmentValue = partialSectionStoreInternal partialSectionValue

matching ::
  PreparedCover site ->
  Vector stalk ->
  Either MatchingFamilyConstructionError (MatchingFamily site stalk)
matching preparedCover sectionsBySlot =
  fmap
    (MatchingFamily preparedCover)
    ( Gluing.mkMatchingFamily
        (cpEffectiveCover (preparedCoverPlanInternal preparedCover))
        sectionsBySlot
    )

matchingTarget :: MatchingFamily site stalk -> SiteObject site
matchingTarget =
  Gluing.matchingFamilyTarget . matchingFamilyRawInternal

matchingCover :: MatchingFamily site stalk -> CoveringFamily (SiteObject site) (SiteMorphism site)
matchingCover =
  Gluing.matchingFamilyCover . matchingFamilyRawInternal

matchingSections :: MatchingFamily site stalk -> Vector stalk
matchingSections =
  Gluing.matchingFamilySections . matchingFamilyRawInternal

matchingPreparedCover :: MatchingFamily site stalk -> PreparedCover site
matchingPreparedCover =
  matchingFamilyOwnerInternal

certifyMatching ::
  StalkAlgebra (CompiledRestriction site) stalk mismatch repair ->
  MatchingFamily site stalk ->
  Either
    (NonEmpty (MatchingFailure (SiteObject site) (SiteMorphism site) mismatch))
    (CompatibleMatchingFamily site stalk)
certifyMatching stalkAlgebra matchingFamilyValue =
  case
    Gluing.certifyMatchingFamilyCompatibilityFromPlan
      stalkAlgebra
      (preparedSiteInternal (preparedCoverOwnerInternal owner))
      (preparedCoverPlanInternal owner)
      (matchingFamilyRawInternal matchingFamilyValue)
  of
    Right compatibleRaw ->
      Right (CompatibleMatchingFamily owner compatibleRaw)
    Left failures ->
      Left failures
  where
    owner = matchingFamilyOwnerInternal matchingFamilyValue

compatibleMatchingFamilyUnderlying ::
  CompatibleMatchingFamily site stalk ->
  MatchingFamily site stalk
compatibleMatchingFamilyUnderlying compatibleFamily =
  MatchingFamily
    (compatibleMatchingFamilyOwnerInternal compatibleFamily)
    (Gluing.compatibleMatchingFamilyUnderlying (compatibleMatchingFamilyRawInternal compatibleFamily))

glue ::
  StalkAlgebra (CompiledRestriction site) stalk mismatch repair ->
  GluingAlgebra site stalk gluingFailure ->
  MatchingFamily site stalk ->
  Either
    (CoverGluingFailure (SiteObject site) (SiteMorphism site) mismatch gluingFailure)
    (Amalgamation site stalk)
glue stalkAlgebra gluingAlgebra matchingFamilyValue =
  fmap
    (Amalgamation owner)
    ( first
        CoverAmalgamationFailed
        ( Gluing.amalgamateCoverPlanWith
            stalkAlgebra
            ( \siteValue compatibleRaw ->
                gaAmalgamate gluingAlgebra siteValue (CompatibleMatchingFamily owner compatibleRaw)
            )
            (preparedSiteInternal (preparedCoverOwnerInternal owner))
            (preparedCoverPlanInternal owner)
            (matchingFamilyRawInternal matchingFamilyValue)
        )
    )
  where
    owner = matchingFamilyOwnerInternal matchingFamilyValue

certifyAmalgamation ::
  StalkAlgebra (CompiledRestriction site) stalk mismatch repair ->
  CompatibleMatchingFamily site stalk ->
  stalk ->
  Either
    (NonEmpty (AmalgamationLocalityFailure mismatch))
    (Amalgamation site stalk)
certifyAmalgamation stalkAlgebra compatibleFamily stalk =
  fmap
    (Amalgamation owner)
    ( Gluing.certifyAmalgamation
        stalkAlgebra
        (preparedSiteInternal (preparedCoverOwnerInternal owner))
        (compatibleMatchingFamilyRawInternal compatibleFamily)
        stalk
    )
  where
    owner = compatibleMatchingFamilyOwnerInternal compatibleFamily

amalgamationMatchingFamily :: Amalgamation site stalk -> MatchingFamily site stalk
amalgamationMatchingFamily amalgamation =
  MatchingFamily
    (amalgamationOwnerInternal amalgamation)
    (Gluing.amalgamationMatchingFamily (amalgamationRawInternal amalgamation))

amalgamatedStalk :: Amalgamation site stalk -> stalk
amalgamatedStalk =
  Gluing.amalgamatedStalk . amalgamationRawInternal

coverStalkUniverse ::
  PreparedCover site ->
  [stalk] ->
  Vector [stalk] ->
  Either UniverseShapeError (CoverStalkUniverse site stalk)
coverStalkUniverse preparedCover targetStalks slotCandidates
  | Vector.length slotCandidates /= expectedSlotCount =
      Left
        UniverseShapeError
          { universeExpectedSlotCount = expectedSlotCount,
            universeActualSlotCount = Vector.length slotCandidates
          }
  | otherwise =
      Right
        CoverStalkUniverse
          { coverStalkUniverseOwnerInternal = preparedCover,
            coverStalkUniverseRawInternal =
              Gluing.CoverStalkUniverse
                { Gluing.csuTargetStalks = targetStalks,
                  Gluing.csuSlotStalks =
                    IntMap.fromList (zip slotKeys (Vector.toList slotCandidates))
                }
          }
  where
    effectivePlan = cpEffectiveCover (preparedCoverPlanInternal preparedCover)
    slotKeys = IntMap.keys (effectiveCoverSlots effectivePlan)
    expectedSlotCount = effectiveCoverSlotCount effectivePlan

separatedCover ::
  Eq stalk =>
  StalkAlgebra (CompiledRestriction site) stalk mismatch repair ->
  CoverStalkUniverse site stalk ->
  Either (SeparatedCoverRefusal stalk mismatch) (SeparatedCover site stalk)
separatedCover stalkAlgebra universe =
  fmap
    (SeparatedCover owner)
    ( Gluing.certifySeparatedCover
        stalkAlgebra
        (preparedSiteInternal (preparedCoverOwnerInternal owner))
        (cpEffectiveCover (preparedCoverPlanInternal owner))
        (coverStalkUniverseRawInternal universe)
    )
  where
    owner = coverStalkUniverseOwnerInternal universe

certifyUniqueAmalgamation ::
  (Eq stalk, Eq (SiteObject site), Eq (SiteMorphism site)) =>
  StalkAlgebra (CompiledRestriction site) stalk mismatch repair ->
  SeparatedCover site stalk ->
  CompatibleMatchingFamily site stalk ->
  stalk ->
  Either (SeparatedUniquenessRefusal mismatch) (UniqueAmalgamation site stalk)
certifyUniqueAmalgamation stalkAlgebra separated compatibleFamily stalk
  | not (ownersAgree separatedOwner familyOwner) =
      Left UniquenessCoverPlanMismatch
  | otherwise =
      fmap
        (UniqueAmalgamation separatedOwner)
        ( Gluing.certifyUniqueAmalgamation
            stalkAlgebra
            (preparedSiteInternal (preparedCoverOwnerInternal separatedOwner))
            (separatedCoverRawInternal separated)
            (compatibleMatchingFamilyRawInternal compatibleFamily)
            stalk
        )
  where
    separatedOwner = separatedCoverOwnerInternal separated
    familyOwner = compatibleMatchingFamilyOwnerInternal compatibleFamily

resolveUniqueAmalgamation ::
  (Eq stalk, Eq (SiteObject site), Eq (SiteMorphism site)) =>
  StalkAlgebra (CompiledRestriction site) stalk mismatch repair ->
  SeparatedCover site stalk ->
  CompatibleMatchingFamily site stalk ->
  Either (SeparatedResolutionRefusal mismatch) (UniqueAmalgamation site stalk)
resolveUniqueAmalgamation stalkAlgebra separated compatibleFamily
  | not (ownersAgree separatedOwner familyOwner) =
      Left ResolutionCoverPlanMismatch
  | otherwise =
      fmap
        (UniqueAmalgamation separatedOwner)
        ( Gluing.resolveUniqueAmalgamation
            stalkAlgebra
            (separatedCoverRawInternal separated)
            (compatibleMatchingFamilyRawInternal compatibleFamily)
        )
  where
    separatedOwner = separatedCoverOwnerInternal separated
    familyOwner = compatibleMatchingFamilyOwnerInternal compatibleFamily

separatedLocalEqualityAt ::
  StalkAlgebra (CompiledRestriction site) stalk mismatch repair ->
  SeparatedCover site stalk ->
  Int ->
  Int ->
  Either SeparatedEqualityRefusal SeparatedEqualityVerdict
separatedLocalEqualityAt stalkAlgebra separated leftIndex rightIndex =
  Gluing.separatedLocalEqualityAt
    stalkAlgebra
    (separatedCoverRawInternal separated)
    leftIndex
    rightIndex

uniqueAmalgamationUnderlying :: UniqueAmalgamation site stalk -> Amalgamation site stalk
uniqueAmalgamationUnderlying uniqueAmalgamation =
  Amalgamation
    (uniqueAmalgamationOwnerInternal uniqueAmalgamation)
    (Gluing.uniqueAmalgamationUnderlying (uniqueAmalgamationRawInternal uniqueAmalgamation))

ownersAgree ::
  (Eq (SiteObject site), Eq (SiteMorphism site)) =>
  PreparedCover site ->
  PreparedCover site ->
  Bool
ownersAgree leftOwner rightOwner =
  Model.sheafModelFingerprint (preparedSiteModelInternal (preparedCoverOwnerInternal leftOwner))
    == Model.sheafModelFingerprint (preparedSiteModelInternal (preparedCoverOwnerInternal rightOwner))
    && preparedCoverPlanInternal leftOwner == preparedCoverPlanInternal rightOwner

mapPreparationError :: Either (Model.SheafModelBuildError cell) value -> Either (CompileError cell morphism) value
mapPreparationError =
  first $ \buildError ->
    case buildError of
      Model.SheafModelRestrictionBuildError restrictionError ->
        fromRestrictionIndexError restrictionError

fromRestrictionIndexError :: RestrictionIndexError cell -> CompileError cell morphism
fromRestrictionIndexError restrictionError =
  case restrictionError of
    RestrictionUnknownSource cell ->
      SheafRestrictionUnknownSource cell
    RestrictionUnknownTarget cell ->
      SheafRestrictionUnknownTarget cell
    RestrictionUnknownId restrictionId ->
      SheafRestrictionUnknownId restrictionId
    RestrictionDuplicateId restrictionId ->
      SheafRestrictionDuplicateId restrictionId
    RestrictionNonDenseId expectedId actualId ->
      SheafRestrictionNonDenseId expectedId actualId
    RestrictionZeroIncidenceCoefficient source target ->
      SheafRestrictionZeroIncidenceCoefficient source target
