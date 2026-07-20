{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

-- | Finite sheafification: the separated reflection and plus-construction
-- reflector, with unit morphisms and sheaf witnesses.
module Moonlight.Sheaf.Sheafification.Finite
  ( AssociatedFinitePresheaf,
    AssociatedPresheafFailure,
    AssociatedRestrictionFailure,
    SeparatedRestrictionFailure,
    SeparatedFinitePresheaf,
    SecondPlusConstruction,
    FinitePlusUnitMorphism,
    FinitePlusUnitMorphismFailure,
    FinitePlusUnitEvidence (..),
    FiniteSheafificationUnitMorphism,
    FiniteSheafificationUnitCompositionFailure,
    FiniteSheafWitness,
    FiniteSheafWitnessFailure (..),
    finiteSheafWitnessPresheaf,
    finiteSheafWitnessReport,
    FiniteSheafificationReflectorResult,
    finiteSheafificationBase,
    finiteSheafificationSeparated,
    finiteSheafificationAssociated,
    finiteSheafificationUnit,
    finiteSheafificationAssociatedWitness,
    FiniteSheafificationReflectorFailure (..),
    Sheafification (..),
    SheafificationFailure (..),
    SheafificationUnitEvidence (..),
    SheafificationUnitEvidenceFailure (..),
    SheafConditionBuildFailure (..),
    UnitInjectivityFailure (..),
    UnitSurjectivityFailure (..),
    SheafConditionReport (..),
    sheafifyFinitePresheaf,
    finiteSheafificationReflectorResult,
    sheafificationUnitEvidence,
    associatedSheafificationReport,
    checkFiniteSheafCondition,
    sheafConditionReportAccepted,
    isFiniteSheaf,
    finiteSheafWitness,
  )
where

import Data.Bifunctor (first)
import Data.List (tails)
import Data.Set qualified as Set
import Moonlight.Sheaf.Presheaf.Enumeration (FiniteEnumerationBudget)
import Moonlight.Sheaf.Presheaf.Finite
  ( FinitePresheaf (..),
    FinitePresheafFailure,
    finiteFiberAt,
    finiteFiberValues,
  )
import Moonlight.Sheaf.Presheaf.Morphism
  ( FinitePresheafMorphism,
    FinitePresheafMorphismCompositionFailure,
    FinitePresheafMorphismFailure,
    composeFinitePresheafMorphisms,
    mkFinitePresheafMorphism,
  )
import Moonlight.Sheaf.Presheaf.Plus
  ( PlusClass,
    PlusClassMismatch,
    PlusConstruction,
    PlusConstructionFailure,
    PlusRestrictionFailure,
    PlusUnitFailure,
    plusAsFinitePresheaf,
    plusConstruction,
    plusUnitClass,
  )
import Moonlight.Sheaf.Presheaf.Separation
  ( SeparationConditionFailure,
    SeparationFailure,
    checkSeparated,
    separateFinitePresheaf,
  )
import Moonlight.Sheaf.Site.Class (Site (..))
import Moonlight.Sheaf.Site.CoverBasis.Finite (FiniteCoverBasis)

type SeparatedRestrictionFailure obj mor value mismatch restrictionFailure =
  PlusRestrictionFailure obj mor value mismatch restrictionFailure

type AssociatedRestrictionFailure obj mor value mismatch restrictionFailure =
  PlusRestrictionFailure
    obj
    mor
    (PlusClass obj)
    (PlusClassMismatch obj)
    (SeparatedRestrictionFailure obj mor value mismatch restrictionFailure)

type AssociatedPresheafFailure obj mor value mismatch restrictionFailure =
  FinitePresheafFailure
    obj
    mor
    (PlusClass obj)
    (PlusClassMismatch obj)
    (AssociatedRestrictionFailure obj mor value mismatch restrictionFailure)

type SecondPlusConstruction site value mismatch restrictionFailure =
  PlusConstruction
    site
    (PlusClass (SiteObject site))
    (PlusClassMismatch (SiteObject site))
    (SeparatedRestrictionFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)

type AssociatedFinitePresheaf site value mismatch restrictionFailure =
  FinitePresheaf
    site
    (PlusClass (SiteObject site))
    (PlusClassMismatch (SiteObject site))
    (AssociatedRestrictionFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)

type SeparatedFinitePresheaf site value mismatch restrictionFailure =
  FinitePresheaf
    site
    (PlusClass (SiteObject site))
    (PlusClassMismatch (SiteObject site))
    (SeparatedRestrictionFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)

type FinitePlusUnitMorphism site value mismatch restrictionFailure =
  FinitePresheafMorphism
    site
    value
    (PlusClass (SiteObject site))
    mismatch
    (PlusClassMismatch (SiteObject site))
    restrictionFailure
    (SeparatedRestrictionFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)

type FinitePlusUnitMorphismFailure obj mor value mismatch restrictionFailure =
  FinitePresheafMorphismFailure
    obj
    mor
    value
    (PlusClass obj)
    restrictionFailure
    (SeparatedRestrictionFailure obj mor value mismatch restrictionFailure)
    (PlusClassMismatch obj)
    (PlusUnitFailure obj mor value mismatch restrictionFailure)

type FiniteSheafificationUnitMorphism site value mismatch restrictionFailure =
  FinitePresheafMorphism
    site
    value
    (PlusClass (SiteObject site))
    mismatch
    (PlusClassMismatch (SiteObject site))
    restrictionFailure
    (AssociatedRestrictionFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)

type FiniteSheafificationUnitCompositionFailure obj mor value mismatch restrictionFailure =
  FinitePresheafMorphismCompositionFailure
    obj
    mor
    value
    (PlusClass obj)
    (PlusClass obj)
    (PlusClassMismatch obj)
    restrictionFailure
    (SeparatedRestrictionFailure obj mor value mismatch restrictionFailure)
    (AssociatedRestrictionFailure obj mor value mismatch restrictionFailure)
    (PlusClassMismatch obj)

type SecondFinitePlusUnitEvidence site value mismatch restrictionFailure =
  FinitePlusUnitEvidence
    site
    (PlusClass (SiteObject site))
    (PlusClassMismatch (SiteObject site))
    (SeparatedRestrictionFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)

type AssociatedFiniteSheafWitness site value mismatch restrictionFailure =
  FiniteSheafWitness
    site
    (PlusClass (SiteObject site))
    (PlusClassMismatch (SiteObject site))
    (AssociatedRestrictionFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)

data FinitePlusUnitEvidence site value mismatch restrictionFailure = FinitePlusUnitEvidence
  { finitePlusUnitMorphism :: !(FinitePlusUnitMorphism site value mismatch restrictionFailure),
    finitePlusUnitReport :: !(SheafConditionReport (SiteObject site) value mismatch)
  }

data FiniteSheafWitness site value mismatch restrictionFailure = FiniteSheafWitness
  { finiteSheafWitnessPresheaf :: !(FinitePresheaf site value mismatch restrictionFailure),
    finiteSheafWitnessReport :: !(SheafConditionReport (SiteObject site) value mismatch)
  }

data FiniteSheafWitnessFailure obj mor value mismatch restrictionFailure
  = FiniteSheafWitnessBuildFailed !(SheafConditionBuildFailure obj mor value mismatch restrictionFailure)
  | FiniteSheafWitnessRejected !(SheafConditionReport obj value mismatch)
  deriving stock (Eq, Show)

data FiniteSheafificationReflectorResult site value mismatch restrictionFailure = FiniteSheafificationReflectorResult
  { fsrSheafification :: !(Sheafification site value mismatch restrictionFailure),
    fsrUnit :: !(FiniteSheafificationUnitMorphism site value mismatch restrictionFailure),
    fsrAssociatedWitness :: !(AssociatedFiniteSheafWitness site value mismatch restrictionFailure)
  }

data FiniteSheafificationReflectorFailure obj mor value mismatch restrictionFailure
  = FiniteSheafificationReflectorConstructionFailed !(SheafificationFailure obj mor value mismatch restrictionFailure)
  | FiniteSheafificationReflectorUnitEvidenceFailed !(SheafificationUnitEvidenceFailure obj mor value mismatch restrictionFailure)
  | FiniteSheafificationReflectorUnitCompositionFailed
      !(FiniteSheafificationUnitCompositionFailure obj mor value mismatch restrictionFailure)
  | FiniteSheafificationReflectorAssociatedWitnessFailed
      !( FiniteSheafWitnessFailure
           obj
           mor
           (PlusClass obj)
           (PlusClassMismatch obj)
           (AssociatedRestrictionFailure obj mor value mismatch restrictionFailure)
       )
  deriving stock (Eq, Show)

data Sheafification site value mismatch restrictionFailure = Sheafification
  { sheafificationBase :: !(FinitePresheaf site value mismatch restrictionFailure),
    sheafificationFirstPlusConstruction :: !(PlusConstruction site value mismatch restrictionFailure),
    sheafificationSeparated :: !(SeparatedFinitePresheaf site value mismatch restrictionFailure),
    sheafificationSecondPlusConstruction :: !(SecondPlusConstruction site value mismatch restrictionFailure),
    sheafificationAssociated :: !(AssociatedFinitePresheaf site value mismatch restrictionFailure)
  }

data SheafificationFailure obj mor value mismatch restrictionFailure
  = SheafificationFirstPlusFailed !(PlusConstructionFailure obj mor value mismatch restrictionFailure)
  | SheafificationSeparatedPresheafFailed
      !( FinitePresheafFailure
           obj
           mor
           (PlusClass obj)
           (PlusClassMismatch obj)
           (SeparatedRestrictionFailure obj mor value mismatch restrictionFailure)
       )
  | SheafificationSecondPlusFailed
      !( PlusConstructionFailure
           obj
           mor
           (PlusClass obj)
           (PlusClassMismatch obj)
           (SeparatedRestrictionFailure obj mor value mismatch restrictionFailure)
       )
  | SheafificationAssociatedPresheafFailed !(AssociatedPresheafFailure obj mor value mismatch restrictionFailure)
  deriving stock (Eq, Show)

data SheafificationUnitEvidence site value mismatch restrictionFailure = SheafificationUnitEvidence
  { sheafificationFirstUnit :: !(FinitePlusUnitEvidence site value mismatch restrictionFailure),
    sheafificationSecondUnit :: !(SecondFinitePlusUnitEvidence site value mismatch restrictionFailure)
  }

data SheafificationUnitEvidenceFailure obj mor value mismatch restrictionFailure
  = SheafificationFirstUnitEvidenceFailed !(SheafConditionBuildFailure obj mor value mismatch restrictionFailure)
  | SheafificationSecondUnitEvidenceFailed
      !( SheafConditionBuildFailure
           obj
           mor
           (PlusClass obj)
           (PlusClassMismatch obj)
           (SeparatedRestrictionFailure obj mor value mismatch restrictionFailure)
       )
  deriving stock (Eq, Show)

data SheafConditionBuildFailure obj mor value mismatch restrictionFailure
  = SheafConditionFirstPlusFailed !(PlusConstructionFailure obj mor value mismatch restrictionFailure)
  | SheafConditionAssociatedPresheafFailed
      !( FinitePresheafFailure
           obj
           mor
           (PlusClass obj)
           (PlusClassMismatch obj)
           (SeparatedRestrictionFailure obj mor value mismatch restrictionFailure)
       )
  | SheafConditionSeparationFailed !(SeparationFailure obj mor value mismatch restrictionFailure)
  | SheafConditionUnitFailed !(PlusUnitFailure obj mor value mismatch restrictionFailure)
  | SheafConditionUnitMorphismInvalid !(FinitePlusUnitMorphismFailure obj mor value mismatch restrictionFailure)
  | SheafConditionBaseFiberMissing !obj
  | SheafConditionPlusFiberMissing !obj
  deriving stock (Eq, Show)

data UnitInjectivityFailure obj value mismatch = UnitInjectivityFailure
  { uifObject :: !obj,
    uifLeftValue :: !value,
    uifRightValue :: !value,
    uifGlobalMismatches :: ![mismatch]
  }
  deriving stock (Eq, Show)

data UnitSurjectivityFailure obj = UnitSurjectivityFailure
  { usfObject :: !obj,
    usfUnrepresentedClass :: !(PlusClass obj)
  }
  deriving stock (Eq, Show)

data SheafConditionReport obj value mismatch = SheafConditionReport
  { scrInjectivityFailures :: ![UnitInjectivityFailure obj value mismatch],
    scrSurjectivityFailures :: ![UnitSurjectivityFailure obj],
    scrSeparationFailures :: ![SeparationConditionFailure obj value mismatch]
  }
  deriving stock (Eq, Show)

sheafifyFinitePresheaf ::
  (Site site, Ord (SiteMorphism site)) =>
  FiniteEnumerationBudget ->
  FiniteCoverBasis site ->
  FinitePresheaf site value mismatch restrictionFailure ->
  Either
    (SheafificationFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)
    (Sheafification site value mismatch restrictionFailure)
sheafifyFinitePresheaf budget basis presheaf = do
  firstPlusValue <-
    first
      SheafificationFirstPlusFailed
      (plusConstruction budget basis presheaf)
  separatedPresheaf <-
    first
      SheafificationSeparatedPresheafFailed
      (plusAsFinitePresheaf firstPlusValue)
  secondPlusValue <-
    first
      SheafificationSecondPlusFailed
      (plusConstruction budget basis separatedPresheaf)
  associatedPresheaf <-
    first
      SheafificationAssociatedPresheafFailed
      (plusAsFinitePresheaf secondPlusValue)
  pure
    Sheafification
      { sheafificationBase = presheaf,
        sheafificationFirstPlusConstruction = firstPlusValue,
        sheafificationSeparated = separatedPresheaf,
        sheafificationSecondPlusConstruction = secondPlusValue,
        sheafificationAssociated = associatedPresheaf
      }

finiteSheafificationBase ::
  FiniteSheafificationReflectorResult site value mismatch restrictionFailure ->
  FinitePresheaf site value mismatch restrictionFailure
finiteSheafificationBase =
  sheafificationBase . fsrSheafification
{-# INLINE finiteSheafificationBase #-}

finiteSheafificationSeparated ::
  FiniteSheafificationReflectorResult site value mismatch restrictionFailure ->
  SeparatedFinitePresheaf site value mismatch restrictionFailure
finiteSheafificationSeparated =
  sheafificationSeparated . fsrSheafification
{-# INLINE finiteSheafificationSeparated #-}

finiteSheafificationAssociated ::
  FiniteSheafificationReflectorResult site value mismatch restrictionFailure ->
  AssociatedFinitePresheaf site value mismatch restrictionFailure
finiteSheafificationAssociated =
  sheafificationAssociated . fsrSheafification
{-# INLINE finiteSheafificationAssociated #-}

finiteSheafificationUnit ::
  FiniteSheafificationReflectorResult site value mismatch restrictionFailure ->
  FiniteSheafificationUnitMorphism site value mismatch restrictionFailure
finiteSheafificationUnit =
  fsrUnit
{-# INLINE finiteSheafificationUnit #-}

finiteSheafificationAssociatedWitness ::
  FiniteSheafificationReflectorResult site value mismatch restrictionFailure ->
  AssociatedFiniteSheafWitness site value mismatch restrictionFailure
finiteSheafificationAssociatedWitness =
  fsrAssociatedWitness
{-# INLINE finiteSheafificationAssociatedWitness #-}

finiteSheafificationReflectorResult ::
  forall site value mismatch restrictionFailure.
  (Site site, Ord (SiteMorphism site), Ord value) =>
  FiniteEnumerationBudget ->
  FiniteCoverBasis site ->
  FinitePresheaf site value mismatch restrictionFailure ->
  Either
    (FiniteSheafificationReflectorFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)
    (FiniteSheafificationReflectorResult site value mismatch restrictionFailure)
finiteSheafificationReflectorResult budget basis presheaf = do
  sheafification <-
    first
      FiniteSheafificationReflectorConstructionFailed
      (sheafifyFinitePresheaf budget basis presheaf)
  unitEvidence <-
    first
      FiniteSheafificationReflectorUnitEvidenceFailed
      (sheafificationUnitEvidence basis sheafification)
  compositeUnit <-
    first
      FiniteSheafificationReflectorUnitCompositionFailed
      ( composeFinitePresheafMorphisms
          (finitePlusUnitMorphism (sheafificationSecondUnit unitEvidence))
          (finitePlusUnitMorphism (sheafificationFirstUnit unitEvidence))
      )
  associatedWitness <-
    first
      FiniteSheafificationReflectorAssociatedWitnessFailed
      (finiteSheafWitness budget basis (sheafificationAssociated sheafification))
  pure
    FiniteSheafificationReflectorResult
      { fsrSheafification = sheafification,
        fsrUnit = compositeUnit,
        fsrAssociatedWitness = associatedWitness
      }

sheafificationUnitEvidence ::
  forall site value mismatch restrictionFailure.
  (Site site, Ord (SiteMorphism site), Ord value) =>
  FiniteCoverBasis site ->
  Sheafification site value mismatch restrictionFailure ->
  Either
    (SheafificationUnitEvidenceFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)
    (SheafificationUnitEvidence site value mismatch restrictionFailure)
sheafificationUnitEvidence basis sheafification = do
  firstUnit <-
    first
      SheafificationFirstUnitEvidenceFailed
      ( finitePlusUnitEvidenceFromConstruction
          basis
          (sheafificationBase sheafification)
          (sheafificationFirstPlusConstruction sheafification)
          (sheafificationSeparated sheafification)
      )
  secondUnit <-
    first
      SheafificationSecondUnitEvidenceFailed
      ( finitePlusUnitEvidenceFromConstruction
          basis
          (sheafificationSeparated sheafification)
          (sheafificationSecondPlusConstruction sheafification)
          (sheafificationAssociated sheafification)
      )
  pure
    SheafificationUnitEvidence
      { sheafificationFirstUnit = firstUnit,
        sheafificationSecondUnit = secondUnit
      }

associatedSheafificationReport ::
  (Site site, Ord (SiteMorphism site)) =>
  FiniteEnumerationBudget ->
  FiniteCoverBasis site ->
  Sheafification site value mismatch restrictionFailure ->
  Either
    ( SheafConditionBuildFailure
        (SiteObject site)
        (SiteMorphism site)
        (PlusClass (SiteObject site))
        (PlusClassMismatch (SiteObject site))
        (AssociatedRestrictionFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)
    )
    (SheafConditionReport (SiteObject site) (PlusClass (SiteObject site)) (PlusClassMismatch (SiteObject site)))
associatedSheafificationReport budget basis =
  checkFiniteSheafCondition budget basis . sheafificationAssociated

checkFiniteSheafCondition ::
  forall site value mismatch restrictionFailure.
  (Site site, Ord (SiteMorphism site), Ord value) =>
  FiniteEnumerationBudget ->
  FiniteCoverBasis site ->
  FinitePresheaf site value mismatch restrictionFailure ->
  Either
    (SheafConditionBuildFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)
    (SheafConditionReport (SiteObject site) value mismatch)
checkFiniteSheafCondition budget basis presheaf = do
  plusValue <-
    first
      SheafConditionFirstPlusFailed
      (plusConstruction budget basis presheaf)
  plusPresheaf <-
    first
      SheafConditionAssociatedPresheafFailed
      (plusAsFinitePresheaf plusValue)
  sheafConditionReportFromPlus basis presheaf plusValue plusPresheaf

sheafConditionReportAccepted ::
  SheafConditionReport obj value mismatch ->
  Bool
sheafConditionReportAccepted report =
  null (scrInjectivityFailures report)
    && null (scrSurjectivityFailures report)
    && null (scrSeparationFailures report)
{-# INLINE sheafConditionReportAccepted #-}

isFiniteSheaf ::
  (Site site, Ord (SiteMorphism site), Ord value) =>
  FiniteEnumerationBudget ->
  FiniteCoverBasis site ->
  FinitePresheaf site value mismatch restrictionFailure ->
  Either
    (SheafConditionBuildFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)
    Bool
isFiniteSheaf budget basis presheaf =
  sheafConditionReportAccepted <$> checkFiniteSheafCondition budget basis presheaf

finiteSheafWitness ::
  (Site site, Ord (SiteMorphism site), Ord value) =>
  FiniteEnumerationBudget ->
  FiniteCoverBasis site ->
  FinitePresheaf site value mismatch restrictionFailure ->
  Either
    (FiniteSheafWitnessFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)
    (FiniteSheafWitness site value mismatch restrictionFailure)
finiteSheafWitness budget basis presheaf = do
  reportValue <-
    first
      FiniteSheafWitnessBuildFailed
      (checkFiniteSheafCondition budget basis presheaf)
  if sheafConditionReportAccepted reportValue
    then
      Right
        FiniteSheafWitness
          { finiteSheafWitnessPresheaf = presheaf,
            finiteSheafWitnessReport = reportValue
          }
    else Left (FiniteSheafWitnessRejected reportValue)

finitePlusUnitEvidenceFromConstruction ::
  forall site value mismatch restrictionFailure.
  (Site site, Ord (SiteMorphism site), Ord value) =>
  FiniteCoverBasis site ->
  FinitePresheaf site value mismatch restrictionFailure ->
  PlusConstruction site value mismatch restrictionFailure ->
  SeparatedFinitePresheaf site value mismatch restrictionFailure ->
  Either
    (SheafConditionBuildFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)
    (FinitePlusUnitEvidence site value mismatch restrictionFailure)
finitePlusUnitEvidenceFromConstruction basis presheaf plusValue plusPresheaf = do
  unitMorphismValue <-
    finitePlusUnitMorphismFromConstruction presheaf plusValue plusPresheaf
  unitReportValue <-
    sheafConditionReportFromPlus basis presheaf plusValue plusPresheaf
  pure
    FinitePlusUnitEvidence
      { finitePlusUnitMorphism = unitMorphismValue,
        finitePlusUnitReport = unitReportValue
      }

finitePlusUnitMorphismFromConstruction ::
  forall site value mismatch restrictionFailure.
  (Site site, Ord (SiteMorphism site), Ord value) =>
  FinitePresheaf site value mismatch restrictionFailure ->
  PlusConstruction site value mismatch restrictionFailure ->
  SeparatedFinitePresheaf site value mismatch restrictionFailure ->
  Either
    (SheafConditionBuildFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)
    (FinitePlusUnitMorphism site value mismatch restrictionFailure)
finitePlusUnitMorphismFromConstruction presheaf plusValue plusPresheaf =
  first
    SheafConditionUnitMorphismInvalid
    ( mkFinitePresheafMorphism
        presheaf
        plusPresheaf
        (\objectValue value -> plusUnitClass plusValue objectValue value)
    )

sheafConditionReportFromPlus ::
  forall site value mismatch restrictionFailure.
  (Site site, Ord (SiteMorphism site), Ord value) =>
  FiniteCoverBasis site ->
  FinitePresheaf site value mismatch restrictionFailure ->
  PlusConstruction site value mismatch restrictionFailure ->
  SeparatedFinitePresheaf site value mismatch restrictionFailure ->
  Either
    (SheafConditionBuildFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)
    (SheafConditionReport (SiteObject site) value mismatch)
sheafConditionReportFromPlus basis presheaf plusValue plusPresheaf = do
  separated <-
    first
      SheafConditionSeparationFailed
      (separateFinitePresheaf basis presheaf)
  objectReports <-
    traverse
      sheafConditionReportAtObject
      (siteObjects (fpSite presheaf))
  pure
    SheafConditionReport
      { scrInjectivityFailures = foldMap scrInjectivityFailures objectReports,
        scrSurjectivityFailures = foldMap scrSurjectivityFailures objectReports,
        scrSeparationFailures = checkSeparated separated
      }
  where
    sheafConditionReportAtObject objectValue = do
      baseFiber <-
        maybe
          (Left (SheafConditionBaseFiberMissing objectValue))
          Right
          (finiteFiberAt objectValue presheaf)
      plusFiber <-
        maybe
          (Left (SheafConditionPlusFiberMissing objectValue))
          Right
          (finiteFiberAt objectValue plusPresheaf)
      unitClassEntries <-
        traverse
          (unitClassEntry objectValue)
          (finiteFiberValues baseFiber)
      let unitClassSet =
            Set.fromList (fmap snd unitClassEntries)
      pure
        SheafConditionReport
          { scrInjectivityFailures =
              concatMap
                (injectivityFailureForPair objectValue)
                (distinctPairs unitClassEntries),
            scrSurjectivityFailures =
              [ UnitSurjectivityFailure
                  { usfObject = objectValue,
                    usfUnrepresentedClass = plusClass
                  }
              | plusClass <- finiteFiberValues plusFiber,
                not (Set.member plusClass unitClassSet)
              ],
            scrSeparationFailures = []
          }

    unitClassEntry objectValue value = do
      unitClass <-
        unitClassInReport plusValue objectValue value
      pure (value, unitClass)

    injectivityFailureForPair objectValue ((leftValue, leftClass), (rightValue, rightClass)) =
      let mismatches = fpMismatches presheaf objectValue leftValue rightValue
       in [ UnitInjectivityFailure
              { uifObject = objectValue,
                uifLeftValue = leftValue,
                uifRightValue = rightValue,
                uifGlobalMismatches = mismatches
              }
          | leftClass == rightClass,
            not (null mismatches)
          ]

    unitClassInReport ::
      PlusConstruction site value mismatch restrictionFailure ->
      SiteObject site ->
      value ->
      Either
        (SheafConditionBuildFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)
        (PlusClass (SiteObject site))
    unitClassInReport construction objectValue value =
      first
        SheafConditionUnitFailed
        (plusUnitClass construction objectValue value)

distinctPairs ::
  [value] ->
  [(value, value)]
distinctPairs values =
  [ (leftValue, rightValue)
  | leftValue : rightValues <- tails values,
    rightValue <- rightValues
  ]
