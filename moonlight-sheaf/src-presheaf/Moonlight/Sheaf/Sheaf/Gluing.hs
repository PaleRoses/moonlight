{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Sheaf.Gluing
  ( MatchingFamily,
    CompatibleMatchingFamily,
    GluingAlgebra (..),
    MatchingFamilyConstructionError (..),
    MatchingFailure (..),
    GluingObstruction (..),
    GluingFailure (..),
    AmalgamationLocalityFailure (..),
    Amalgamation,
    mkMatchingFamily,
    matchingFamilyTarget,
    matchingFamilyCover,
    matchingFamilySections,
    matchingFamilySectionAt,
    compatibleMatchingFamilyUnderlying,
    certifyMatchingFamilyCompatibility,
    certifyMatchingFamilyCompatibilityFromPlan,
    certifyMatchingFamilyCompatibilityFirstObstruction,
    certifyMatchingFamilyCompatibilityFirstObstructionFromPlan,
    amalgamationMatchingFamily,
    amalgamatedStalk,
    amalgamationLocalityFailures,
    certifyAmalgamation,
    amalgamateCompatibleMatchingFamilyWith,
    CoverStalkUniverse (..),
    GhostSection (..),
    SeparatedCoverRefusal (..),
    SeparatedCover,
    separatedCoverPlan,
    separatedCoverUniverse,
    certifySeparatedCover,
    SeparatedUniquenessRefusal (..),
    UniqueAmalgamation,
    uniqueAmalgamationUnderlying,
    certifyUniqueAmalgamation,
    SeparatedResolutionRefusal (..),
    resolveUniqueAmalgamation,
    SeparatedEqualityRefusal (..),
    SeparatedEqualityVerdict (..),
    separatedLocalEqualityAt,
    pairwiseCompatibilityFailures,
    pairwiseCompatibilityFailuresFromPlan,
    pairwiseCompatibilityFailuresFromEffectivePlan,
    MatchingFamilyPruningObstruction (..),
    matchingFamilyPruningVerdict,
    matchingFamilyPruningVerdictFromPlan,
    amalgamateCoverPlanWith,
    amalgamateMatchingFamilyWith,
  )
where

import Data.Bifunctor (bimap, first)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List (find, tails)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Monoid (First (..))
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Moonlight.Sheaf.Presheaf.Core
  ( CompiledRestriction (..),
  )
import Moonlight.Sheaf.Section.Stalk
  ( StalkAlgebra,
    restrictStalk,
    stalkMismatches,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism,
    CoveringFamily,
    PullbackSquare (..),
    Site (..),
    coverTarget,
  )
import Moonlight.Sheaf.Site.Plan
  ( CoverPlan (..),
    CoverSlot (..),
    CoverSlotKey (..),
    EffectiveCoverPlan,
    OverlapPlan (..),
    effectiveCoverFamily,
    effectiveCoverOverlapPlans,
    effectiveCoverSlots,
  )
import Moonlight.Sheaf.Verdict
  ( ObstructionVerdict,
    rejectedFromList,
  )

type MatchingFamily :: Type -> Type -> Type
data MatchingFamily site stalk = MatchingFamily
  { mfCoverPlan :: !(EffectiveCoverPlan (SiteObject site) (SiteMorphism site)),
    mfSections :: !(Vector stalk)
  }

type CompatibleMatchingFamily :: Type -> Type -> Type
newtype CompatibleMatchingFamily site stalk = CompatibleMatchingFamily
  { compatibleMatchingFamilyUnderlying :: MatchingFamily site stalk
  }

type GluingAlgebra :: Type -> Type -> Type -> Type
data GluingAlgebra site stalk gluingFailure = GluingAlgebra
  { gaAmalgamate ::
      site ->
      CompatibleMatchingFamily site stalk ->
      Either (GluingObstruction (SiteObject site) gluingFailure) stalk
  }

type MatchingFamilyConstructionError :: Type
data MatchingFamilyConstructionError = MatchingFamilyArityMismatch
  { expectedSectionCount :: !Int,
    actualSectionCount :: !Int
  }
  deriving stock (Eq, Show)

type MatchingFailure :: Type -> Type -> Type -> Type
data MatchingFailure obj mor mismatch
  = MissingPullback
      !(CheckedMorphism obj mor)
      !(CheckedMorphism obj mor)
  -- A foreign plan passed to pairwiseCompatibilityFailuresFromPlan may name
  -- a slot outside the matching family's total vector.
  | MissingLocalSection
      !CoverSlotKey
  | PullbackDisagreement
      !(PullbackSquare obj mor)
      ![mismatch]
  deriving stock (Eq, Show)

type GluingObstruction :: Type -> Type -> Type
data GluingObstruction obj gluingFailure
  = GluingUnavailable !obj
  | GluingRejected !gluingFailure
  deriving stock (Eq, Show)

type GluingFailure :: Type -> Type -> Type -> Type -> Type
data GluingFailure obj mor mismatch gluingFailure
  = IncompatibleMatchingFamily !(NonEmpty (MatchingFailure obj mor mismatch))
  | GluingObstructed !(GluingObstruction obj gluingFailure)
  | AmalgamationNotLocal !(NonEmpty (AmalgamationLocalityFailure mismatch))
  deriving stock (Eq, Show)

type AmalgamationLocalityFailure :: Type -> Type
data AmalgamationLocalityFailure mismatch
  = AmalgamationLocalSectionMissing !CoverSlotKey
  | AmalgamationLocalityMismatch !CoverSlotKey ![mismatch]
  deriving stock (Eq, Show)

type MatchingFamilyPruningObstruction :: Type -> Type -> Type -> Type
data MatchingFamilyPruningObstruction obj mor mismatch
  = MatchingFamilyIncompatible !(MatchingFailure obj mor mismatch)
  deriving stock (Eq, Show)

mkMatchingFamily ::
  EffectiveCoverPlan (SiteObject site) (SiteMorphism site) ->
  Vector stalk ->
  Either
    MatchingFamilyConstructionError
    (MatchingFamily site stalk)
mkMatchingFamily coverPlan sections =
  let expectedCount = IntMap.size (effectiveCoverSlots coverPlan)
      actualCount = Vector.length sections
   in if actualCount == expectedCount
        then
          Right
            MatchingFamily
              { mfCoverPlan = coverPlan,
                mfSections = sections
              }
        else
          Left
            MatchingFamilyArityMismatch
              { expectedSectionCount = expectedCount,
                actualSectionCount = actualCount
              }

matchingFamilyTarget ::
  MatchingFamily site stalk ->
  SiteObject site
matchingFamilyTarget =
  coverTarget . effectiveCoverFamily . mfCoverPlan

matchingFamilyCover ::
  MatchingFamily site stalk ->
  CoveringFamily (SiteObject site) (SiteMorphism site)
matchingFamilyCover =
  effectiveCoverFamily . mfCoverPlan

matchingFamilySections ::
  MatchingFamily site stalk ->
  Vector stalk
matchingFamilySections =
  mfSections

matchingFamilySectionAt ::
  CoverSlotKey ->
  MatchingFamily site stalk ->
  Maybe stalk
matchingFamilySectionAt (CoverSlotKey slotIndex) matchingFamily =
  mfSections matchingFamily Vector.!? slotIndex
{-# INLINE matchingFamilySectionAt #-}

pairwiseCompatibilityFailures ::
  StalkAlgebra (CompiledRestriction site) stalk mismatch repairObstruction ->
  site ->
  MatchingFamily site stalk ->
  [MatchingFailure (SiteObject site) (SiteMorphism site) mismatch]
pairwiseCompatibilityFailures stalkAlgebra site matchingFamily =
  pairwiseCompatibilityFailuresFromEffectivePlan
    stalkAlgebra
    site
    (mfCoverPlan matchingFamily)
    matchingFamily

pairwiseCompatibilityFailuresFromPlan ::
  StalkAlgebra (CompiledRestriction site) stalk mismatch repairObstruction ->
  site ->
  CoverPlan (SiteObject site) (SiteMorphism site) ->
  MatchingFamily site stalk ->
  [MatchingFailure (SiteObject site) (SiteMorphism site) mismatch]
pairwiseCompatibilityFailuresFromPlan stalkAlgebra site coverPlan =
  pairwiseCompatibilityFailuresFromEffectivePlan
    stalkAlgebra
    site
    (cpEffectiveCover coverPlan)

pairwiseCompatibilityFailuresFromEffectivePlan ::
  StalkAlgebra (CompiledRestriction site) stalk mismatch repairObstruction ->
  site ->
  EffectiveCoverPlan (SiteObject site) (SiteMorphism site) ->
  MatchingFamily site stalk ->
  [MatchingFailure (SiteObject site) (SiteMorphism site) mismatch]
pairwiseCompatibilityFailuresFromEffectivePlan stalkAlgebra site coverPlan matchingFamily =
  concatMap compatibilityFailuresForOverlap (effectiveCoverOverlapPlans coverPlan)
  where
    compatibilityFailuresForOverlap overlapPlan =
      case
        ( matchingFamilySectionAt (opLeftSlot overlapPlan) matchingFamily,
          matchingFamilySectionAt (opRightSlot overlapPlan) matchingFamily
        )
      of
        (Nothing, Nothing) ->
          [ MissingLocalSection (opLeftSlot overlapPlan),
            MissingLocalSection (opRightSlot overlapPlan)
          ]
        (Nothing, Just _) ->
          [MissingLocalSection (opLeftSlot overlapPlan)]
        (Just _, Nothing) ->
          [MissingLocalSection (opRightSlot overlapPlan)]
        (Just leftSectionValue, Just rightSectionValue) ->
          let square = opPullbackSquare overlapPlan
              leftRestricted =
                restrictStalk stalkAlgebra (CompiledRestriction site (psToLeft square)) leftSectionValue
              rightRestricted =
                restrictStalk stalkAlgebra (CompiledRestriction site (psToRight square)) rightSectionValue
              mismatches =
                stalkMismatches stalkAlgebra leftRestricted rightRestricted
           in [PullbackDisagreement square mismatches | not (null mismatches)]

firstCompatibilityFailureFromEffectivePlan ::
  StalkAlgebra (CompiledRestriction site) stalk mismatch repairObstruction ->
  site ->
  EffectiveCoverPlan (SiteObject site) (SiteMorphism site) ->
  MatchingFamily site stalk ->
  Maybe (MatchingFailure (SiteObject site) (SiteMorphism site) mismatch)
firstCompatibilityFailureFromEffectivePlan stalkAlgebra site coverPlan matchingFamily =
  getFirst (foldMap (First . firstCompatibilityFailureForOverlap) (effectiveCoverOverlapPlans coverPlan))
  where
    firstCompatibilityFailureForOverlap overlapPlan =
      case
        ( matchingFamilySectionAt (opLeftSlot overlapPlan) matchingFamily,
          matchingFamilySectionAt (opRightSlot overlapPlan) matchingFamily
        )
      of
        (Nothing, _) ->
          Just (MissingLocalSection (opLeftSlot overlapPlan))
        (Just _, Nothing) ->
          Just (MissingLocalSection (opRightSlot overlapPlan))
        (Just leftSectionValue, Just rightSectionValue) ->
          let square = opPullbackSquare overlapPlan
              leftRestricted =
                restrictStalk stalkAlgebra (CompiledRestriction site (psToLeft square)) leftSectionValue
              rightRestricted =
                restrictStalk stalkAlgebra (CompiledRestriction site (psToRight square)) rightSectionValue
              mismatches =
                stalkMismatches stalkAlgebra leftRestricted rightRestricted
           in if null mismatches
                then Nothing
                else Just (PullbackDisagreement square mismatches)

certifyMatchingFamilyCompatibility ::
  StalkAlgebra (CompiledRestriction site) stalk mismatch repairObstruction ->
  site ->
  MatchingFamily site stalk ->
  Either
    (NonEmpty (MatchingFailure (SiteObject site) (SiteMorphism site) mismatch))
    (CompatibleMatchingFamily site stalk)
certifyMatchingFamilyCompatibility stalkAlgebra site matchingFamily =
  certifyMatchingFamilyCompatibilityFromEffectivePlan
    stalkAlgebra
    site
    (mfCoverPlan matchingFamily)
    matchingFamily

certifyMatchingFamilyCompatibilityFromPlan ::
  StalkAlgebra (CompiledRestriction site) stalk mismatch repairObstruction ->
  site ->
  CoverPlan (SiteObject site) (SiteMorphism site) ->
  MatchingFamily site stalk ->
  Either
    (NonEmpty (MatchingFailure (SiteObject site) (SiteMorphism site) mismatch))
    (CompatibleMatchingFamily site stalk)
certifyMatchingFamilyCompatibilityFromPlan stalkAlgebra site coverPlan =
  certifyMatchingFamilyCompatibilityFromEffectivePlan
    stalkAlgebra
    site
    (cpEffectiveCover coverPlan)

certifyMatchingFamilyCompatibilityFromEffectivePlan ::
  StalkAlgebra (CompiledRestriction site) stalk mismatch repairObstruction ->
  site ->
  EffectiveCoverPlan (SiteObject site) (SiteMorphism site) ->
  MatchingFamily site stalk ->
  Either
    (NonEmpty (MatchingFailure (SiteObject site) (SiteMorphism site) mismatch))
    (CompatibleMatchingFamily site stalk)
certifyMatchingFamilyCompatibilityFromEffectivePlan stalkAlgebra site coverPlan matchingFamily =
  case pairwiseCompatibilityFailuresFromEffectivePlan stalkAlgebra site coverPlan matchingFamily of
    [] ->
      Right (CompatibleMatchingFamily matchingFamily)
    failure : rest ->
      Left (failure :| rest)

certifyMatchingFamilyCompatibilityFirstObstruction ::
  StalkAlgebra (CompiledRestriction site) stalk mismatch repairObstruction ->
  site ->
  MatchingFamily site stalk ->
  Either
    (MatchingFailure (SiteObject site) (SiteMorphism site) mismatch)
    (CompatibleMatchingFamily site stalk)
certifyMatchingFamilyCompatibilityFirstObstruction stalkAlgebra site matchingFamily =
  certifyMatchingFamilyCompatibilityFirstObstructionFromEffectivePlan
    stalkAlgebra
    site
    (mfCoverPlan matchingFamily)
    matchingFamily

certifyMatchingFamilyCompatibilityFirstObstructionFromPlan ::
  StalkAlgebra (CompiledRestriction site) stalk mismatch repairObstruction ->
  site ->
  CoverPlan (SiteObject site) (SiteMorphism site) ->
  MatchingFamily site stalk ->
  Either
    (MatchingFailure (SiteObject site) (SiteMorphism site) mismatch)
    (CompatibleMatchingFamily site stalk)
certifyMatchingFamilyCompatibilityFirstObstructionFromPlan stalkAlgebra site coverPlan =
  certifyMatchingFamilyCompatibilityFirstObstructionFromEffectivePlan
    stalkAlgebra
    site
    (cpEffectiveCover coverPlan)

certifyMatchingFamilyCompatibilityFirstObstructionFromEffectivePlan ::
  StalkAlgebra (CompiledRestriction site) stalk mismatch repairObstruction ->
  site ->
  EffectiveCoverPlan (SiteObject site) (SiteMorphism site) ->
  MatchingFamily site stalk ->
  Either
    (MatchingFailure (SiteObject site) (SiteMorphism site) mismatch)
    (CompatibleMatchingFamily site stalk)
certifyMatchingFamilyCompatibilityFirstObstructionFromEffectivePlan stalkAlgebra site coverPlan matchingFamily =
  case firstCompatibilityFailureFromEffectivePlan stalkAlgebra site coverPlan matchingFamily of
    Nothing ->
      Right (CompatibleMatchingFamily matchingFamily)
    Just failure ->
      Left failure

matchingFamilyPruningVerdict ::
  StalkAlgebra (CompiledRestriction site) stalk mismatch repairObstruction ->
  site ->
  MatchingFamily site stalk ->
  ObstructionVerdict
    (MatchingFamilyPruningObstruction (SiteObject site) (SiteMorphism site) mismatch)
matchingFamilyPruningVerdict stalkAlgebra site =
  rejectedFromList
    . mapMaybe matchingFamilyPruningObstruction
    . pairwiseCompatibilityFailures stalkAlgebra site

matchingFamilyPruningVerdictFromPlan ::
  StalkAlgebra (CompiledRestriction site) stalk mismatch repairObstruction ->
  site ->
  CoverPlan (SiteObject site) (SiteMorphism site) ->
  MatchingFamily site stalk ->
  ObstructionVerdict
    (MatchingFamilyPruningObstruction (SiteObject site) (SiteMorphism site) mismatch)
matchingFamilyPruningVerdictFromPlan stalkAlgebra site coverPlan =
  rejectedFromList
    . mapMaybe matchingFamilyPruningObstruction
    . pairwiseCompatibilityFailuresFromPlan stalkAlgebra site coverPlan

matchingFamilyPruningObstruction ::
  MatchingFailure obj mor mismatch ->
  Maybe (MatchingFamilyPruningObstruction obj mor mismatch)
matchingFamilyPruningObstruction failure =
  case failure of
    MissingPullback _ _ ->
      Nothing
    MissingLocalSection _ ->
      Just (MatchingFamilyIncompatible failure)
    PullbackDisagreement _ _ ->
      Just (MatchingFamilyIncompatible failure)

type Amalgamation :: Type -> Type -> Type
data Amalgamation site stalk = Amalgamation
  { amFamily :: !(MatchingFamily site stalk),
    amStalk :: !stalk
  }

amalgamationMatchingFamily :: Amalgamation site stalk -> MatchingFamily site stalk
amalgamationMatchingFamily =
  amFamily

amalgamatedStalk :: Amalgamation site stalk -> stalk
amalgamatedStalk =
  amStalk

certifyAmalgamation ::
  StalkAlgebra (CompiledRestriction site) stalk mismatch repairObstruction ->
  site ->
  CompatibleMatchingFamily site stalk ->
  stalk ->
  Either
    (NonEmpty (AmalgamationLocalityFailure mismatch))
    (Amalgamation site stalk)
certifyAmalgamation stalkAlgebra site compatibleFamily stalk =
  case
    NonEmpty.nonEmpty
      ( amalgamationLocalityFailures
          stalkAlgebra
          site
          (mfCoverPlan matchingFamily)
          (mfSections matchingFamily)
          stalk
      )
  of
    Nothing ->
      Right (Amalgamation matchingFamily stalk)
    Just failures ->
      Left failures
  where
    matchingFamily =
      compatibleMatchingFamilyUnderlying compatibleFamily

amalgamationLocalityFailures ::
  StalkAlgebra (CompiledRestriction site) stalk mismatch repairObstruction ->
  site ->
  EffectiveCoverPlan (SiteObject site) (SiteMorphism site) ->
  Vector stalk ->
  stalk ->
  [AmalgamationLocalityFailure mismatch]
amalgamationLocalityFailures stalkAlgebra site coverPlan localSections stalk =
  concatMap slotLocalityFailures (IntMap.elems (effectiveCoverSlots coverPlan))
  where
    slotLocalityFailures slot =
      case localSections Vector.!? (unCoverSlotKey (coverSlotKey slot)) of
        Nothing ->
          [AmalgamationLocalSectionMissing (coverSlotKey slot)]
        Just localSection ->
          let restricted =
                restrictStalk stalkAlgebra (CompiledRestriction site (coverSlotArrow slot)) stalk
              mismatches =
                stalkMismatches stalkAlgebra restricted localSection
           in [ AmalgamationLocalityMismatch
                  (coverSlotKey slot)
                  mismatches
                | not (null mismatches)
              ]

amalgamateCompatibleMatchingFamilyWith ::
  StalkAlgebra (CompiledRestriction site) stalk mismatch repairObstruction ->
  ( site ->
    CompatibleMatchingFamily site stalk ->
    Either (GluingObstruction (SiteObject site) gluingFailure) stalk
  ) ->
  site ->
  CompatibleMatchingFamily site stalk ->
  Either
    (GluingFailure (SiteObject site) (SiteMorphism site) mismatch gluingFailure)
    (Amalgamation site stalk)
amalgamateCompatibleMatchingFamilyWith stalkAlgebra glueFamily site compatibleFamily =
  case glueFamily site compatibleFamily of
    Left obstruction ->
      Left (GluingObstructed obstruction)
    Right stalk ->
      first AmalgamationNotLocal (certifyAmalgamation stalkAlgebra site compatibleFamily stalk)

amalgamateMatchingFamilyWith ::
  StalkAlgebra (CompiledRestriction site) stalk mismatch repairObstruction ->
  ( site ->
    CompatibleMatchingFamily site stalk ->
    Either (GluingObstruction (SiteObject site) gluingFailure) stalk
  ) ->
  site ->
  MatchingFamily site stalk ->
  Either
    (GluingFailure (SiteObject site) (SiteMorphism site) mismatch gluingFailure)
    (Amalgamation site stalk)
amalgamateMatchingFamilyWith stalkAlgebra glueFamily site matchingFamily =
  case certifyMatchingFamilyCompatibility stalkAlgebra site matchingFamily of
    Right compatibleFamily ->
      amalgamateCompatibleMatchingFamilyWith stalkAlgebra glueFamily site compatibleFamily
    Left failures ->
      Left (IncompatibleMatchingFamily failures)

amalgamateCoverPlanWith ::
  StalkAlgebra (CompiledRestriction site) stalk mismatch repairObstruction ->
  ( site ->
    CompatibleMatchingFamily site stalk ->
    Either (GluingObstruction (SiteObject site) gluingFailure) stalk
  ) ->
  site ->
  CoverPlan (SiteObject site) (SiteMorphism site) ->
  MatchingFamily site stalk ->
  Either
    (GluingFailure (SiteObject site) (SiteMorphism site) mismatch gluingFailure)
    (Amalgamation site stalk)
amalgamateCoverPlanWith stalkAlgebra glueFamily site coverPlan matchingFamily =
  case certifyMatchingFamilyCompatibilityFromPlan stalkAlgebra site coverPlan matchingFamily of
    Right compatibleFamily ->
      amalgamateCompatibleMatchingFamilyWith stalkAlgebra glueFamily site compatibleFamily
    Left failures ->
      Left (IncompatibleMatchingFamily failures)

type CoverStalkUniverse :: Type -> Type
data CoverStalkUniverse stalk = CoverStalkUniverse
  { csuTargetStalks :: ![stalk],
    csuSlotStalks :: !(IntMap [stalk])
  }
  deriving stock (Eq, Show)

type GhostSection :: Type -> Type -> Type
data GhostSection stalk mismatch = GhostSection
  { gsLeftStalk :: !stalk,
    gsRightStalk :: !stalk,
    gsMismatches :: ![mismatch],
    gsSlotComparands :: !(IntMap stalk)
  }
  deriving stock (Eq, Show)

type SeparatedCoverRefusal :: Type -> Type -> Type
data SeparatedCoverRefusal stalk mismatch
  = SeparatedCoverUniverseIncomplete !IntSet
  | SeparatedCoverGhostSections !(NonEmpty (GhostSection stalk mismatch))
  deriving stock (Eq, Show)

type SeparatedCover :: Type -> Type -> Type
data SeparatedCover site stalk = SeparatedCover
  { scPlan :: !(EffectiveCoverPlan (SiteObject site) (SiteMorphism site)),
    scUniverse :: !(CoverStalkUniverse stalk),
    scIndexedTargets :: !(IntMap stalk),
    scRestrictedTargets :: !(IntMap (IntMap stalk))
  }

separatedCoverPlan ::
  SeparatedCover site stalk ->
  EffectiveCoverPlan (SiteObject site) (SiteMorphism site)
separatedCoverPlan =
  scPlan

separatedCoverUniverse :: SeparatedCover site stalk -> CoverStalkUniverse stalk
separatedCoverUniverse =
  scUniverse

certifySeparatedCover ::
  Eq stalk =>
  StalkAlgebra (CompiledRestriction site) stalk mismatch repairObstruction ->
  site ->
  EffectiveCoverPlan (SiteObject site) (SiteMorphism site) ->
  CoverStalkUniverse stalk ->
  Either (SeparatedCoverRefusal stalk mismatch) (SeparatedCover site stalk)
certifySeparatedCover stalkAlgebra site coverPlan universe
  | not (IntSet.null incompleteSlotKeys) =
      Left (SeparatedCoverUniverseIncomplete incompleteSlotKeys)
  | otherwise =
      maybe
        ( Right
            SeparatedCover
              { scPlan = coverPlan,
                scUniverse = universe,
                scIndexedTargets = IntMap.fromList indexedTargets,
                scRestrictedTargets = restrictedTargetsBySlot
              }
        )
        (Left . SeparatedCoverGhostSections)
        (NonEmpty.nonEmpty ghostSections)
  where
    slotStalksAt slotKeyInt =
      IntMap.findWithDefault [] slotKeyInt (csuSlotStalks universe)

    incompleteSlotKeys =
      IntSet.fromList
        [ slotKeyInt
        | slotKeyInt <- IntMap.keys (effectiveCoverSlots coverPlan),
          null (slotStalksAt slotKeyInt)
        ]

    indexedTargets =
      zip [0 :: Int ..] (csuTargetStalks universe)

    restrictedTargetsBySlot =
      fmap
        ( \slot ->
            IntMap.fromList
              [ (targetIndex, restrictStalk stalkAlgebra (CompiledRestriction site (coverSlotArrow slot)) targetStalk)
              | (targetIndex, targetStalk) <- indexedTargets
              ]
        )
        (effectiveCoverSlots coverPlan)

    ghostSections =
      mapMaybe
        ghostSection
        [ (leftEntry, rightEntry)
        | leftEntry : laterEntries <- tails indexedTargets,
          rightEntry <- laterEntries,
          snd leftEntry /= snd rightEntry
        ]

    ghostSection ((leftIndex, leftStalk), (rightIndex, rightStalk)) = do
      comparands <- IntMap.traverseWithKey (slotComparand leftIndex rightIndex) (effectiveCoverSlots coverPlan)
      pure
        GhostSection
          { gsLeftStalk = leftStalk,
            gsRightStalk = rightStalk,
            gsMismatches = stalkMismatches stalkAlgebra leftStalk rightStalk,
            gsSlotComparands = comparands
          }

    slotComparand leftIndex rightIndex slotKeyInt _slot = do
      restrictedRow <- IntMap.lookup slotKeyInt restrictedTargetsBySlot
      leftRestricted <- IntMap.lookup leftIndex restrictedRow
      rightRestricted <- IntMap.lookup rightIndex restrictedRow
      find
        ( \candidate ->
            null (stalkMismatches stalkAlgebra leftRestricted candidate)
              && null (stalkMismatches stalkAlgebra rightRestricted candidate)
        )
        (slotStalksAt slotKeyInt)

type SeparatedUniquenessRefusal :: Type -> Type
data SeparatedUniquenessRefusal mismatch
  = UniquenessCoverPlanMismatch
  | UniquenessAmalgamatedStalkOutsideUniverse
  | UniquenessFamilySectionsOutsideUniverse !IntSet
  | UniquenessNotLocal !(NonEmpty (AmalgamationLocalityFailure mismatch))
  deriving stock (Eq, Show)

type UniqueAmalgamation :: Type -> Type -> Type
newtype UniqueAmalgamation site stalk = UniqueAmalgamation
  { uniqueAmalgamationUnderlying :: Amalgamation site stalk
  }

certifyUniqueAmalgamation ::
  (Eq stalk, Eq (SiteObject site), Eq (SiteMorphism site)) =>
  StalkAlgebra (CompiledRestriction site) stalk mismatch repairObstruction ->
  site ->
  SeparatedCover site stalk ->
  CompatibleMatchingFamily site stalk ->
  stalk ->
  Either (SeparatedUniquenessRefusal mismatch) (UniqueAmalgamation site stalk)
certifyUniqueAmalgamation stalkAlgebra site separatedCover compatibleFamily stalk
  | mfCoverPlan family /= scPlan separatedCover =
      Left UniquenessCoverPlanMismatch
  | stalk `notElem` csuTargetStalks certifiedUniverse =
      Left UniquenessAmalgamatedStalkOutsideUniverse
  | not (IntSet.null foreignSectionSlots) =
      Left (UniquenessFamilySectionsOutsideUniverse foreignSectionSlots)
  | otherwise =
      bimap
        UniquenessNotLocal
        UniqueAmalgamation
        (certifyAmalgamation stalkAlgebra site compatibleFamily stalk)
  where
    family =
      compatibleMatchingFamilyUnderlying compatibleFamily

    certifiedUniverse =
      scUniverse separatedCover

    foreignSectionSlots =
      IntSet.fromList
        [ slotKeyInt
        | (slotKeyInt, sectionStalk) <- Vector.toList (Vector.indexed (mfSections family)),
          sectionStalk `notElem` IntMap.findWithDefault [] slotKeyInt (csuSlotStalks certifiedUniverse)
        ]

type SeparatedResolutionRefusal :: Type -> Type
data SeparatedResolutionRefusal mismatch
  = ResolutionCoverPlanMismatch
  | ResolutionFamilySectionsOutsideUniverse !IntSet
  | ResolutionNoLocalTarget !(IntMap (NonEmpty (AmalgamationLocalityFailure mismatch)))
  deriving stock (Eq, Show)

resolveUniqueAmalgamation ::
  (Eq stalk, Eq (SiteObject site), Eq (SiteMorphism site)) =>
  StalkAlgebra (CompiledRestriction site) stalk mismatch repairObstruction ->
  SeparatedCover site stalk ->
  CompatibleMatchingFamily site stalk ->
  Either (SeparatedResolutionRefusal mismatch) (UniqueAmalgamation site stalk)
resolveUniqueAmalgamation stalkAlgebra separatedCover compatibleFamily
  | mfCoverPlan family /= scPlan separatedCover =
      Left ResolutionCoverPlanMismatch
  | not (IntSet.null foreignSectionSlots) =
      Left (ResolutionFamilySectionsOutsideUniverse foreignSectionSlots)
  | otherwise =
      case IntMap.lookupMin (IntMap.difference (scIndexedTargets separatedCover) failuresByTarget) of
        Nothing ->
          Left (ResolutionNoLocalTarget failuresByTarget)
        Just (_, localTarget) ->
          Right (UniqueAmalgamation (Amalgamation family localTarget))
  where
    failuresByTarget =
      IntMap.mapMaybeWithKey
        (\targetIndex _ -> NonEmpty.nonEmpty (targetLocalityFailures targetIndex))
        (scIndexedTargets separatedCover)

    family =
      compatibleMatchingFamilyUnderlying compatibleFamily

    foreignSectionSlots =
      IntSet.fromList
        [ slotKeyInt
        | (slotKeyInt, sectionStalk) <- Vector.toList (Vector.indexed (mfSections family)),
          sectionStalk `notElem` IntMap.findWithDefault [] slotKeyInt (csuSlotStalks (scUniverse separatedCover))
        ]

    targetLocalityFailures targetIndex =
      [ AmalgamationLocalityMismatch
          (CoverSlotKey slotKeyInt)
          mismatches
      | (slotKeyInt, sectionStalk) <- Vector.toList (Vector.indexed (mfSections family)),
        Just restrictedRow <- [IntMap.lookup slotKeyInt (scRestrictedTargets separatedCover)],
        Just restricted <- [IntMap.lookup targetIndex restrictedRow],
        let mismatches = stalkMismatches stalkAlgebra restricted sectionStalk,
        not (null mismatches)
      ]

type SeparatedEqualityRefusal :: Type
newtype SeparatedEqualityRefusal = EqualityTargetIndexOutOfRange IntSet
  deriving stock (Eq, Show)

type SeparatedEqualityVerdict :: Type
data SeparatedEqualityVerdict
  = SeparatedStalksEqual
  | SeparatedStalksDistinguished !CoverSlotKey
  deriving stock (Eq, Show)

separatedLocalEqualityAt ::
  StalkAlgebra (CompiledRestriction site) stalk mismatch repairObstruction ->
  SeparatedCover site stalk ->
  Int ->
  Int ->
  Either SeparatedEqualityRefusal SeparatedEqualityVerdict
separatedLocalEqualityAt stalkAlgebra separatedCover leftIndex rightIndex
  | not (IntSet.null invalidIndices) =
      Left (EqualityTargetIndexOutOfRange invalidIndices)
  | leftIndex == rightIndex =
      Right SeparatedStalksEqual
  | otherwise =
      maybe
        (Right SeparatedStalksEqual)
        (Right . SeparatedStalksDistinguished . CoverSlotKey)
        (find separatesPair (IntMap.keys (scRestrictedTargets separatedCover)))
  where
    invalidIndices =
      IntSet.fromList
        [ targetIndex
        | targetIndex <- [leftIndex, rightIndex],
          not (IntMap.member targetIndex (scIndexedTargets separatedCover))
        ]

    separatesPair slotKeyInt =
      case IntMap.lookup slotKeyInt (scRestrictedTargets separatedCover) of
        Nothing ->
          True
        Just restrictedRow ->
          case (IntMap.lookup leftIndex restrictedRow, IntMap.lookup rightIndex restrictedRow) of
            (Just leftRestricted, Just rightRestricted) ->
              not
                ( any
                    ( \candidate ->
                        null (stalkMismatches stalkAlgebra leftRestricted candidate)
                          && null (stalkMismatches stalkAlgebra rightRestricted candidate)
                    )
                    (IntMap.findWithDefault [] slotKeyInt (csuSlotStalks (scUniverse separatedCover)))
                )
            _ ->
              True
