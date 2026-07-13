{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.Sheaf.Presheaf.Plus
  ( PlusKey (..),
    PlusClass (..),
    PlusRepresentative (..),
    PlusFiber (..),
    PlusRestrictionTable (..),
    PlusRepresentativeSignature (..),
    PlusConstruction (..),
    PlusRestrictionFailure (..),
    PlusUnitFailure (..),
    PlusEnumerationCost (..),
    PlusClassMismatch (..),
    PlusConstructionFailure (..),
    plusConstruction,
    plusAsFinitePresheaf,
    plusUnitClass,
    plusFiberAt,
    plusRepresentativeAt,
    plusCanonicalClass,
  )
where

import Data.Bifunctor (first)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Set qualified as Set
import Numeric.Natural (Natural)
import Moonlight.Core (DenseKey (..))
import Moonlight.Sheaf.Index.Dense (mkDenseIndex)
import Moonlight.Sheaf.Presheaf.Finite
  ( FiniteFiber (..),
    FinitePresheaf (..),
    FinitePresheafFailure (..),
    finiteFiberAt,
    finiteFiberContains,
    finiteFiberValues,
  )
import Moonlight.Sheaf.Presheaf.Enumeration
  ( FiniteEnumerationBudget,
    assignmentUpperBound,
    guardEnumerationBudget,
  )
import Moonlight.Sheaf.Presheaf.Transport
  ( CoverSectionTransport (..),
    CoverSectionTransportFailure (..),
    pullCoverSectionsAlong,
  )
import Moonlight.Sheaf.Section.ObjectIndex (mkObjectIndex)
import Moonlight.Sheaf.Section.Congruence.Equivalence.Relation
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism,
    CoveringFamily,
    PullbackSquare (..),
    Site (..),
    cmSource,
    cmTarget,
    siteMorphismUniverse,
  )
import Moonlight.Sheaf.Site.CoverBasis.Finite
  ( FiniteCoverBasis,
    FiniteCoverBasisFailure,
    finiteCanonicalCoverPlan,
    finiteCommonRefinementPlan,
    finiteCoversAt,
    finiteIdentityCoverAt,
  )
import Moonlight.Sheaf.Site.Plan
  ( CommonRefinementPlan (..),
    CoverSlot (..),
    CoverSlotKey (..),
    CrossCoverOverlapPlan (..),
    EffectiveCoverPlan,
    EffectiveCoverPlanFailure,
    OverlapPlan (..),
    effectiveCoverFamily,
    effectiveCoverOverlapPlans,
    effectiveCoverSlots,
  )

newtype PlusKey = PlusKey
  { unPlusKey :: Int
  }
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum)

instance DenseKey PlusKey where
  encodeDenseKey =
    unPlusKey
  {-# INLINE encodeDenseKey #-}

  decodeDenseKey =
    PlusKey
  {-# INLINE decodeDenseKey #-}

data PlusClass obj = PlusClass
  { plusClassTarget :: !obj,
    plusClassKey :: !PlusKey
  }
  deriving stock (Eq, Ord, Show)

data PlusRepresentative site value = PlusRepresentative
  { plusRepTarget :: !(SiteObject site),
    plusRepCover :: !(EffectiveCoverPlan (SiteObject site) (SiteMorphism site)),
    plusRepSections :: !(IntMap value)
  }


deriving stock instance
  (Eq (SiteObject site), Eq (SiteMorphism site), Eq value) =>
  Eq (PlusRepresentative site value)

deriving stock instance
  (Show (SiteObject site), Show (SiteMorphism site), Show value) =>
  Show (PlusRepresentative site value)

data PlusFiber site value = PlusFiber
  { plusFiberTarget :: !(SiteObject site),
    plusFiberRepresentatives :: !(IntMap (PlusRepresentative site value)),
    plusFiberCanonicalKeys :: ![PlusKey],
    plusFiberEquivalence :: !(EquivalenceRelation PlusKey)
  }


deriving stock instance
  (Eq (SiteObject site), Eq (SiteMorphism site), Eq value) =>
  Eq (PlusFiber site value)

deriving stock instance
  (Show (SiteObject site), Show (SiteMorphism site), Show value) =>
  Show (PlusFiber site value)

data PlusRestrictionTable obj mor = PlusRestrictionTable
  { plusRestrictionRows :: !(Map (CheckedMorphism obj mor) (IntMap PlusKey))
  }
  deriving stock (Eq, Show)

data PlusRepresentativeSignature obj mor = PlusRepresentativeSignature
  { prsCover :: !(CoveringFamily obj mor),
    prsSections :: !(IntMap Int)
  }
  deriving stock (Eq, Ord, Show)

newtype PlusCommonSignature = PlusCommonSignature
  { unPlusCommonSignature :: [Int]
  }
  deriving stock (Eq, Ord, Show)

data CoverAssignmentDomain value = CoverAssignmentDomain
  { cadSlotKey :: !CoverSlotKey,
    cadValues :: ![(Int, value)]
  }

data PartialCoverAssignment value = PartialCoverAssignment
  { pcaSections :: !(IntMap value),
    pcaPositions :: !(IntMap Int)
  }

data SignedPlusRepresentative site value = SignedPlusRepresentative
  { sprRepresentative :: !(PlusRepresentative site value),
    sprSignature :: !(PlusRepresentativeSignature (SiteObject site) (SiteMorphism site))
  }

data PlusConstruction site value mismatch restrictionFailure = PlusConstruction
  { plusBase :: !(FinitePresheaf site value mismatch restrictionFailure),
    plusBasis :: !(FiniteCoverBasis site),
    plusFibers :: !(Map (SiteObject site) (PlusFiber site value)),
    plusRepresentativeIndex ::
      !( Map
           (SiteObject site)
           (Map (PlusRepresentativeSignature (SiteObject site) (SiteMorphism site)) [PlusKey])
       ),
    plusRestrictions :: !(PlusRestrictionTable (SiteObject site) (SiteMorphism site))
  }

data PlusRestrictionFailure obj mor value mismatch restrictionFailure
  = PlusRestrictionClassTargetMismatch !(CheckedMorphism obj mor) !(PlusClass obj)
  | PlusRestrictionFiberMissing !obj
  | PlusRestrictionRepresentativeMissing !obj !PlusKey
  | PlusRestrictionCoverUnavailable
      !(CheckedMorphism obj mor)
      !(EffectiveCoverPlan obj mor)
      !(FiniteCoverBasisFailure obj mor)
  | PlusRestrictionSectionMissing !CoverSlotKey
  | PlusRestrictionSectionRestrictFailed !(CheckedMorphism obj mor) !value !restrictionFailure
  | PlusRestrictionCommonRefinementFailed
      !(EffectiveCoverPlan obj mor)
      !(EffectiveCoverPlan obj mor)
      !(EffectiveCoverPlanFailure obj mor)
  | PlusRestrictionSectionMismatch !(PullbackSquare obj mor) ![mismatch]
  | PlusRestrictionRepresentativeNotIndexed !obj !(EffectiveCoverPlan obj mor) !(IntMap value)
  | PlusRestrictionAmbiguousRepresentative !obj ![PlusKey]
  | PlusRestrictionTableMorphismMissing !(CheckedMorphism obj mor)
  | PlusRestrictionTableClassMissing !(CheckedMorphism obj mor) !(PlusClass obj)
  deriving stock (Eq, Show)

data PlusUnitFailure obj mor value mismatch restrictionFailure
  = PlusUnitFiberMissing !obj
  | PlusUnitValueOutsideFiber !obj !value
  | PlusUnitIdentityCoverUnavailable !obj !(FiniteCoverBasisFailure obj mor)
  | PlusUnitIndexFailed !(PlusConstructionFailure obj mor value mismatch restrictionFailure)
  deriving stock (Eq, Show)

data PlusEnumerationCost = PlusEnumerationCost
  { pecCoverCount :: !Natural,
    pecAssignmentUpperBound :: !Natural
  }
  deriving stock (Eq, Show)

data PlusClassMismatch obj =
  PlusClassMismatch !(PlusClass obj) !(PlusClass obj)
  deriving stock (Eq, Ord, Show)

data PlusConstructionFailure obj mor value mismatch restrictionFailure
  = PlusFiberMissing !obj
  | PlusCoverSourceFiberMissing !obj
  | PlusClassKeyMissing !obj !PlusKey
  | PlusRepresentativeMissing !obj !PlusKey
  | PlusRepresentativeSectionMissing !CoverSlotKey
  | PlusRepresentativeNotIndexed !obj !(EffectiveCoverPlan obj mor) !(IntMap value)
  | PlusRepresentativeAmbiguousClass !obj ![PlusKey]
  | PlusRestrictionFailed !(CheckedMorphism obj mor) !value !restrictionFailure
  | PlusRestrictionMismatch !(PullbackSquare obj mor) ![mismatch]
  | PlusCommonRefinementFailed
      !(EffectiveCoverPlan obj mor)
      !(EffectiveCoverPlan obj mor)
      !(EffectiveCoverPlanFailure obj mor)
  | PlusEquivalenceInvalid !obj !EquivalenceRelationError
  | PlusEnumerationBudgetExceeded !obj !PlusEnumerationCost
  | PlusRestrictionTableBuildFailed
      !(CheckedMorphism obj mor)
      !(PlusClass obj)
      !(PlusRestrictionFailure obj mor value mismatch restrictionFailure)
  deriving stock (Eq, Show)

plusConstruction ::
  (Site site, Ord (SiteMorphism site)) =>
  FiniteEnumerationBudget ->
  FiniteCoverBasis site ->
  FinitePresheaf site value mismatch restrictionFailure ->
  Either
    (PlusConstructionFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)
    (PlusConstruction site value mismatch restrictionFailure)
plusConstruction budget basis presheaf = do
  fiberEntries <-
    traverse
      buildPlusFiber
      (siteObjects (fpSite presheaf))
  let fibers =
        Map.fromList
          [ (objectValue, fiberValue)
          | (objectValue, fiberValue, _indexValue) <- fiberEntries
          ]
      representativeIndexes =
        Map.fromList
          [ (objectValue, indexValue)
          | (objectValue, _fiberValue, indexValue) <- fiberEntries
          ]
  restrictionTable <-
    buildPlusRestrictionTable fibers representativeIndexes
  pure
    PlusConstruction
      { plusBase = presheaf,
        plusBasis = basis,
        plusFibers = fibers,
        plusRepresentativeIndex = representativeIndexes,
        plusRestrictions = restrictionTable
      }
  where
    buildPlusFiber objectValue = do
      let covers = finiteCoversAt basis objectValue
      coverDomains <-
        traverse
          coverAssignmentDomains
          covers
      let cost =
            PlusEnumerationCost
              { pecCoverCount = fromIntegral (length covers),
                pecAssignmentUpperBound =
                  sum (assignmentUpperBound . denseDomain <$> coverDomains)
              }
      validateBudget objectValue cost
      signedRepresentatives <-
        concat
          <$> traverse
            (representativesForCoverDomains objectValue)
            coverDomains
      let indexedRepresentatives =
            IntMap.fromList
              [ (indexValue, sprRepresentative representativeValue)
              | (indexValue, representativeValue) <- zip [0 :: Int ..] signedRepresentatives
              ]
          representativeIndex =
            representativeSignatureIndex
              [ (PlusKey indexValue, sprSignature representativeValue)
              | (indexValue, representativeValue) <- zip [0 :: Int ..] signedRepresentatives
              ]
      equivalentPairs <-
        equivalentRepresentativePairs indexedRepresentatives
      relationValue <-
        first
          (PlusEquivalenceInvalid objectValue)
          ( equivalenceFromPairs
              (IntSet.fromList (IntMap.keys indexedRepresentatives))
              equivalentPairs
          )
      let canonicalKeys =
            canonicalRepresentativeKeys relationValue indexedRepresentatives
      pure
        ( objectValue,
          PlusFiber
            { plusFiberTarget = objectValue,
              plusFiberRepresentatives = indexedRepresentatives,
              plusFiberCanonicalKeys = canonicalKeys,
              plusFiberEquivalence = relationValue
            },
          representativeIndex
        )

    coverAssignmentDomains coverPlan = do
      domains <-
        traverse
          slotDomain
          (IntMap.elems (effectiveCoverSlots coverPlan))
      pure (coverPlan, domains)

    denseDomain (_coverPlan, domains) =
      [ (cadSlotKey domainValue, snd <$> cadValues domainValue)
      | domainValue <- domains
      ]

    representativesForCoverDomains objectValue (coverPlan, domains) =
      pure
        ( signedRepresentative objectValue coverPlan
            <$> compatibleCoverAssignments coverPlan domains
        )

    slotDomain slot = do
      fiberValue <-
        note
          (PlusCoverSourceFiberMissing (cmSource (coverSlotArrow slot)))
          (finiteFiberAt (cmSource (coverSlotArrow slot)) presheaf)
      pure
        CoverAssignmentDomain
          { cadSlotKey = coverSlotKey slot,
            cadValues = zip [0 :: Int ..] (finiteFiberValues fiberValue)
          }

    compatibleCoverAssignments coverPlan domains =
      List.sortOn
        (assignmentOriginalOrder domains)
        ( List.foldl'
            extendAssignments
            [ PartialCoverAssignment
                { pcaSections = IntMap.empty,
                  pcaPositions = IntMap.empty
                }
            ]
            (orderedCoverDomains constraintsBySlot domains)
        )
      where
        constraintsBySlot =
          coverConstraintsBySlot coverPlan

        extendAssignments assignments domainValue =
          concatMap
            (extendAssignment domainValue)
            assignments

        extendAssignment domainValue partialAssignment =
          [ nextAssignment
          | (valueIndex, value) <- cadValues domainValue,
            let slotKey = cadSlotKey domainValue,
            let nextAssignment =
                  PartialCoverAssignment
                    { pcaSections =
                        IntMap.insert
                          (unCoverSlotKey slotKey)
                          value
                          (pcaSections partialAssignment),
                      pcaPositions =
                        IntMap.insert
                          (unCoverSlotKey slotKey)
                          valueIndex
                          (pcaPositions partialAssignment)
                    },
            assignmentSatisfiesSlotConstraints
              nextAssignment
              (IntMap.findWithDefault [] (unCoverSlotKey slotKey) constraintsBySlot)
          ]

    orderedCoverDomains constraintsBySlot =
      List.sortOn
        ( \domainValue ->
            ( negate
                (length (IntMap.findWithDefault [] (unCoverSlotKey (cadSlotKey domainValue)) constraintsBySlot)),
              unCoverSlotKey (cadSlotKey domainValue)
            )
        )

    coverConstraintsBySlot coverPlan =
      IntMap.fromListWith
        (flip (<>))
        [ (slotKey, [overlapPlan])
        | overlapPlan <- effectiveCoverOverlapPlans coverPlan,
          slotKey <- [unCoverSlotKey (opLeftSlot overlapPlan), unCoverSlotKey (opRightSlot overlapPlan)]
        ]

    assignmentSatisfiesSlotConstraints partialAssignment =
      all (assignmentSatisfiesConstraint partialAssignment)

    assignmentSatisfiesConstraint partialAssignment overlapPlan =
      case
        ( IntMap.lookup (unCoverSlotKey (opLeftSlot overlapPlan)) (pcaSections partialAssignment),
          IntMap.lookup (unCoverSlotKey (opRightSlot overlapPlan)) (pcaSections partialAssignment)
        )
      of
        (Just _leftValue, Just _rightValue) ->
          null
            ( representativeComparisonFailures
                presheaf
                (opLeftSlot overlapPlan)
                (opRightSlot overlapPlan)
                (opPullbackSquare overlapPlan)
                (pcaSections partialAssignment)
                (pcaSections partialAssignment)
            )
        _ ->
          True

    assignmentOriginalOrder domains partialAssignment =
      [ valueIndex
      | domainValue <- domains,
        Just valueIndex <- [IntMap.lookup (unCoverSlotKey (cadSlotKey domainValue)) (pcaPositions partialAssignment)]
      ]

    signedRepresentative objectValue coverPlan assignment =
      let representative =
            PlusRepresentative
              { plusRepTarget = objectValue,
                plusRepCover = coverPlan,
                plusRepSections = pcaSections assignment
              }
       in SignedPlusRepresentative
            { sprRepresentative = representative,
              sprSignature =
                PlusRepresentativeSignature
                  { prsCover = effectiveCoverFamily coverPlan,
                    prsSections = pcaPositions assignment
                  }
            }

    representativeSignatureIndex indexedRepresentatives =
      Map.fromListWith
        (<>)
        [ (signatureValue, [representativeKey])
        | (representativeKey, signatureValue) <- indexedRepresentatives
        ]

    equivalentRepresentativePairs representatives =
      fmap concat $
        traverse
          equivalentCoverPair
          (representativeCoverPairRequests representatives)

    equivalentCoverPair (_firstPair, leftPlan, rightPlan, leftRepresentatives, rightRepresentatives) = do
      commonRefinement <-
        first
          (PlusCommonRefinementFailed leftPlan rightPlan)
          (finiteCommonRefinementPlan basis leftPlan rightPlan)
      let leftSignatures =
            representativeCommonSignatureGroups
              (leftCommonSignature commonRefinement)
              leftRepresentatives
          rightSignatures =
            representativeCommonSignatureGroups
              (rightCommonSignature commonRefinement)
              rightRepresentatives
      pure
        [ (PlusKey leftKey, PlusKey rightKey)
        | (signatureValue, leftKeys) <- Map.toAscList leftSignatures,
          rightKeys <- maybeToList (Map.lookup signatureValue rightSignatures),
          leftKey <- leftKeys,
          rightKey <- rightKeys,
          leftKey < rightKey
        ]

    representativeCoverPairRequests representatives =
      List.sortOn
        (\(firstPair, _leftPlan, _rightPlan, _leftRepresentatives, _rightRepresentatives) -> firstPair)
        [ (firstPair, leftPlan, rightPlan, leftRepresentatives, rightRepresentatives)
        | (leftCover, rawLeftRepresentatives) <- Map.toAscList representativesByCover,
          (rightCover, rawRightRepresentatives) <- Map.toAscList representativesByCover,
          let leftRepresentatives = List.sortOn fst rawLeftRepresentatives,
          let rightRepresentatives = List.sortOn fst rawRightRepresentatives,
          Just firstPair <- [firstOrderedKeyPair (fst <$> leftRepresentatives) (fst <$> rightRepresentatives)],
          Just leftPlan <- [Map.lookup leftCover plansByCover],
          Just rightPlan <- [Map.lookup rightCover plansByCover]
        ]
      where
        representativeEntries =
          IntMap.toAscList representatives

        representativesByCover =
          Map.fromListWith
            (<>)
            [ (effectiveCoverFamily (plusRepCover representativeValue), [(representativeKey, representativeValue)])
            | (representativeKey, representativeValue) <- representativeEntries
            ]

        plansByCover =
          Map.fromList
            [ (effectiveCoverFamily (plusRepCover representativeValue), plusRepCover representativeValue)
            | (_representativeKey, representativeValue) <- representativeEntries
            ]

    firstOrderedKeyPair leftKeys rightKeys = do
      rightMaximum <- lastMaybe rightKeys
      leftKey <- List.find (< rightMaximum) leftKeys
      rightKey <- List.find (> leftKey) rightKeys
      pure (leftKey, rightKey)

    lastMaybe values =
      case reverse values of
        [] ->
          Nothing
        value : _ ->
          Just value

    representativeCommonSignatureGroups signatureFor representatives =
      Map.fromListWith
        (<>)
        [ (signatureValue, [representativeKey])
        | (representativeKey, representativeValue) <- representatives,
          Just signatureValue <- [signatureFor representativeValue]
        ]

    leftCommonSignature commonRefinement representative =
      PlusCommonSignature
        <$> traverse
          (commonOverlapSignature representative ccopLeftSlot (psToLeft . ccopPullbackSquare))
          (crpCrossOverlaps commonRefinement)

    rightCommonSignature commonRefinement representative =
      PlusCommonSignature
        <$> traverse
          (commonOverlapSignature representative ccopRightSlot (psToRight . ccopPullbackSquare))
          (crpCrossOverlaps commonRefinement)

    commonOverlapSignature representative slotKey projection crossOverlap = do
      sectionValue <-
        IntMap.lookup
          (unCoverSlotKey (slotKey crossOverlap))
          (plusRepSections representative)
      restrictedValue <-
        either
          (const Nothing)
          Just
          (fpRestrict presheaf (projection crossOverlap) sectionValue)
      valueSignatureAtFor presheaf (psApex (ccopPullbackSquare crossOverlap)) restrictedValue

    validateBudget objectValue cost =
      guardEnumerationBudget
        budget
        (pecAssignmentUpperBound cost)
        (PlusEnumerationBudgetExceeded objectValue cost)

    buildPlusRestrictionTable fibers representativeIndexes =
      PlusRestrictionTable . Map.fromList
        <$> traverse
          (restrictionRow fibers representativeIndexes)
          (siteMorphismUniverse (fpSite presheaf))

    restrictionRow fibers representativeIndexes morphismValue = do
      targetFiber <-
        note
          (PlusFiberMissing (cmTarget morphismValue))
          (Map.lookup (cmTarget morphismValue) fibers)
      row <-
        IntMap.fromList
          <$> traverse
            (restrictionEntry fibers representativeIndexes morphismValue targetFiber)
            (plusFiberCanonicalKeys targetFiber)
      pure (morphismValue, row)

    restrictionEntry fibers representativeIndexes morphismValue _targetFiber targetKey = do
      let targetClass =
            PlusClass
              { plusClassTarget = cmTarget morphismValue,
                plusClassKey = targetKey
              }
      restrictedClass <-
        first
          (PlusRestrictionTableBuildFailed morphismValue targetClass)
          (restrictRepresentativeByIndex fibers representativeIndexes morphismValue targetClass)
      pure (unPlusKey targetKey, plusClassKey restrictedClass)

    restrictRepresentativeByIndex fibers representativeIndexes morphismValue classValue
      | plusClassTarget classValue /= cmTarget morphismValue =
          Left (PlusRestrictionClassTargetMismatch morphismValue classValue)
      | otherwise = do
          targetFiber <-
            note
              (PlusRestrictionFiberMissing (cmTarget morphismValue))
              (Map.lookup (cmTarget morphismValue) fibers)
          representative <-
            note
              (PlusRestrictionRepresentativeMissing (cmTarget morphismValue) (plusClassKey classValue))
              (plusRepresentativeAt (plusClassKey classValue) targetFiber)
          transported <-
            first
              transportFailureToPlusRestriction
              ( pullCoverSectionsAlong
                  basis
                  (fpRestrict presheaf)
                  morphismValue
                  (plusRepCover representative)
                  (plusRepSections representative)
              )
          findRestrictedClassByIndex
            fibers
            representativeIndexes
            (cmSource morphismValue)
            PlusRepresentative
              { plusRepTarget = cmSource morphismValue,
                plusRepCover = cstCoverPlan transported,
                plusRepSections = cstSections transported
              }

    findRestrictedClassByIndex fibers representativeIndexes objectValue representative = do
      sourceFiber <-
        note
          (PlusRestrictionFiberMissing objectValue)
          (Map.lookup objectValue fibers)
      let matchingKeys =
            representativeMatchingKeys representativeIndexes objectValue representative
      case canonicalKeysInFiber sourceFiber matchingKeys of
        [] ->
          Left
            ( PlusRestrictionRepresentativeNotIndexed
                objectValue
                (plusRepCover representative)
                (plusRepSections representative)
            )
        [canonicalKey] ->
          Right
            PlusClass
              { plusClassTarget = objectValue,
                plusClassKey = canonicalKey
              }
        canonicalKeys ->
          Left (PlusRestrictionAmbiguousRepresentative objectValue canonicalKeys)

    representativeMatchingKeys representativeIndexes objectValue representative =
      case representativeExactSignatureFor presheaf representative of
        Nothing ->
          []
        Just signatureValue ->
          Map.findWithDefault
            []
            signatureValue
            (Map.findWithDefault Map.empty objectValue representativeIndexes)


plusAsFinitePresheaf ::
  forall site value mismatch restrictionFailure.
  (Site site, Ord (SiteMorphism site)) =>
  PlusConstruction site value mismatch restrictionFailure ->
  Either
    ( FinitePresheafFailure
        (SiteObject site)
        (SiteMorphism site)
        (PlusClass (SiteObject site))
        (PlusClassMismatch (SiteObject site))
        (PlusRestrictionFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)
    )
    ( FinitePresheaf
        site
        (PlusClass (SiteObject site))
        (PlusClassMismatch (SiteObject site))
        (PlusRestrictionFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)
    )
plusAsFinitePresheaf plusValue = do
  fibers <-
    Map.fromList
      <$> traverse
        finiteFiberForObject
        (siteObjects (fpSite presheaf))
  pure
    FinitePresheaf
      { fpSite = fpSite presheaf,
        fpObjectIndex = mkObjectIndex (siteObjects (fpSite presheaf)),
        fpFibers = fibers,
        fpRestrict = restrictPlusClass,
        fpMismatches = plusClassMismatches,
        fpNormalize = \_objectValue classValue -> classValue
      }
  where
    presheaf =
      plusBase plusValue

    finiteFiberForObject objectValue = do
      fiberValue <-
        note
          (FiniteFiberMissing objectValue)
          (plusFiberAt objectValue plusValue)
      pure
        ( objectValue,
          FiniteFiber
            { ffObject = objectValue,
              ffValues = mkDenseIndex (canonicalClassesForFiber objectValue fiberValue)
            }
        )

    canonicalClassesForFiber ::
      SiteObject site ->
      PlusFiber site value ->
      [PlusClass (SiteObject site)]
    canonicalClassesForFiber objectValue fiberValue =
      [ PlusClass objectValue key
      | key <- plusFiberCanonicalKeys fiberValue
      ]

    restrictPlusClass morphismValue classValue
      | plusClassTarget classValue /= cmTarget morphismValue =
          Left (PlusRestrictionClassTargetMismatch morphismValue classValue)
      | otherwise = do
          targetKey <-
            note
              (PlusRestrictionRepresentativeMissing (cmTarget morphismValue) (plusClassKey classValue))
              (canonicalClassKeyAt (cmTarget morphismValue) classValue)
          row <-
            note
              (PlusRestrictionTableMorphismMissing morphismValue)
              (Map.lookup morphismValue (plusRestrictionRows (plusRestrictions plusValue)))
          sourceKey <-
            note
              (PlusRestrictionTableClassMissing morphismValue classValue)
              (IntMap.lookup (unPlusKey targetKey) row)
          pure
            PlusClass
              { plusClassTarget = cmSource morphismValue,
                plusClassKey = sourceKey
              }

    plusClassMismatches objectValue leftClass rightClass =
      case (canonicalClassKeyAt objectValue leftClass, canonicalClassKeyAt objectValue rightClass) of
        (Just leftKey, Just rightKey)
          | leftKey == rightKey ->
              []
        _ ->
          [PlusClassMismatch leftClass rightClass]

    canonicalClassKeyAt objectValue classValue = do
      fiberValue <- plusFiberAt objectValue plusValue
      if plusClassTarget classValue == objectValue
        then equivalenceRepresentative (plusFiberEquivalence fiberValue) (plusClassKey classValue)
        else Nothing

transportFailureToPlusRestriction ::
  CoverSectionTransportFailure obj mor value restrictionFailure ->
  PlusRestrictionFailure obj mor value mismatch restrictionFailure
transportFailureToPlusRestriction failure =
  case failure of
    CoverSectionTransportCoverUnavailable morphismValue coverPlan basisFailure ->
      PlusRestrictionCoverUnavailable morphismValue coverPlan basisFailure
    CoverSectionTransportSectionMissing slotKey ->
      PlusRestrictionSectionMissing slotKey
    CoverSectionTransportRestrictionFailed morphismValue value restrictionFailure ->
      PlusRestrictionSectionRestrictFailed morphismValue value restrictionFailure

plusUnitClass ::
  (Site site, Ord (SiteMorphism site), Ord value) =>
  PlusConstruction site value mismatch restrictionFailure ->
  SiteObject site ->
  value ->
  Either
    (PlusUnitFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)
    (PlusClass (SiteObject site))
plusUnitClass plusValue objectValue value = do
  fiberValue <-
    note
      (PlusUnitFiberMissing objectValue)
      (finiteFiberAt objectValue (plusBase plusValue))
  if finiteFiberContains value fiberValue
    then pure ()
    else Left (PlusUnitValueOutsideFiber objectValue value)
  identityPlan <-
    first
      (PlusUnitIdentityCoverUnavailable objectValue)
      (finiteIdentityCoverAt (plusBasis plusValue) objectValue)
  canonicalIdentityPlan <-
    first
      (PlusUnitIdentityCoverUnavailable objectValue)
      (finiteCanonicalCoverPlan (plusBasis plusValue) identityPlan)
  first
    PlusUnitIndexFailed
    ( plusClassForRepresentative
        plusValue
        objectValue
        PlusRepresentative
          { plusRepTarget = objectValue,
            plusRepCover = canonicalIdentityPlan,
            plusRepSections = IntMap.singleton 0 value
          }
    )

plusClassForRepresentative ::
  (Site site, Ord (SiteMorphism site)) =>
  PlusConstruction site value mismatch restrictionFailure ->
  SiteObject site ->
  PlusRepresentative site value ->
  Either
    (PlusConstructionFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)
    (PlusClass (SiteObject site))
plusClassForRepresentative plusValue objectValue representative = do
  fiberValue <-
    note
      (PlusFiberMissing objectValue)
      (plusFiberAt objectValue plusValue)
  let matchingKeys =
        case representativeExactSignatureFor (plusBase plusValue) representative of
          Nothing ->
            []
          Just signatureValue ->
            Map.findWithDefault
              []
              signatureValue
              (Map.findWithDefault Map.empty objectValue (plusRepresentativeIndex plusValue))
  case canonicalKeysInFiber fiberValue matchingKeys of
    [] ->
      Left
        ( PlusRepresentativeNotIndexed
            objectValue
            (plusRepCover representative)
            (plusRepSections representative)
        )
    [canonicalKey] ->
      Right
        PlusClass
          { plusClassTarget = objectValue,
            plusClassKey = canonicalKey
          }
    canonicalKeys ->
      Left (PlusRepresentativeAmbiguousClass objectValue canonicalKeys)

representativeExactSignatureFor ::
  Site site =>
  FinitePresheaf site value mismatch restrictionFailure ->
  PlusRepresentative site value ->
  Maybe (PlusRepresentativeSignature (SiteObject site) (SiteMorphism site))
representativeExactSignatureFor presheaf representative =
  PlusRepresentativeSignature
    (effectiveCoverFamily (plusRepCover representative))
    <$> traverseCoverSectionSignaturesFor presheaf representative

traverseCoverSectionSignaturesFor ::
  Site site =>
  FinitePresheaf site value mismatch restrictionFailure ->
  PlusRepresentative site value ->
  Maybe (IntMap Int)
traverseCoverSectionSignaturesFor presheaf representative =
  IntMap.fromList
    <$> traverse
      sectionSignature
      (IntMap.elems (effectiveCoverSlots (plusRepCover representative)))
  where
    sectionSignature slot = do
      sectionValue <-
        IntMap.lookup
          (unCoverSlotKey (coverSlotKey slot))
          (plusRepSections representative)
      valueIndex <-
        valueSignatureAtFor presheaf (cmSource (coverSlotArrow slot)) sectionValue
      pure (unCoverSlotKey (coverSlotKey slot), valueIndex)

valueSignatureAtFor ::
  Site site =>
  FinitePresheaf site value mismatch restrictionFailure ->
  SiteObject site ->
  value ->
  Maybe Int
valueSignatureAtFor presheaf objectValue value =
  do
    fiberValue <- finiteFiberAt objectValue presheaf
    fst
      <$> List.find
        ( \(_valueIndex, candidateValue) ->
            null (fpMismatches presheaf objectValue value candidateValue)
        )
        (zip [0 :: Int ..] (finiteFiberValues fiberValue))

canonicalKeysInFiber ::
  PlusFiber site value ->
  [PlusKey] ->
  [PlusKey]
canonicalKeysInFiber fiberValue keys =
  Set.toAscList
    ( Set.fromList
        [ canonicalKey
        | key <- keys,
          Just canonicalKey <- [equivalenceRepresentative (plusFiberEquivalence fiberValue) key]
        ]
    )

canonicalRepresentativeKeys ::
  EquivalenceRelation PlusKey ->
  IntMap (PlusRepresentative site value) ->
  [PlusKey]
canonicalRepresentativeKeys relationValue representatives =
  [ key
  | rawKey <- IntMap.keys representatives,
    let key = PlusKey rawKey,
    equivalenceRepresentative relationValue key == Just key
  ]

representativeComparisonFailures ::
  FinitePresheaf site value mismatch restrictionFailure ->
  CoverSlotKey ->
  CoverSlotKey ->
  PullbackSquare (SiteObject site) (SiteMorphism site) ->
  IntMap value ->
  IntMap value ->
  [PlusConstructionFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure]
representativeComparisonFailures presheaf leftSlot rightSlot square leftSections rightSections =
  case (IntMap.lookup (unCoverSlotKey leftSlot) leftSections, IntMap.lookup (unCoverSlotKey rightSlot) rightSections) of
    (Nothing, _) ->
      [PlusRepresentativeSectionMissing leftSlot]
    (_, Nothing) ->
      [PlusRepresentativeSectionMissing rightSlot]
    (Just leftSection, Just rightSection) ->
      case (fpRestrict presheaf (psToLeft square) leftSection, fpRestrict presheaf (psToRight square) rightSection) of
        (Left failure, _) ->
          [PlusRestrictionFailed (psToLeft square) leftSection failure]
        (_, Left failure) ->
          [PlusRestrictionFailed (psToRight square) rightSection failure]
        (Right leftRestricted, Right rightRestricted) ->
          let mismatches = fpMismatches presheaf (psApex square) leftRestricted rightRestricted
           in [PlusRestrictionMismatch square mismatches | not (null mismatches)]

plusFiberAt ::
  Site site =>
  SiteObject site ->
  PlusConstruction site value mismatch restrictionFailure ->
  Maybe (PlusFiber site value)
plusFiberAt objectValue =
  Map.lookup objectValue . plusFibers
{-# INLINE plusFiberAt #-}

plusRepresentativeAt ::
  PlusKey ->
  PlusFiber site value ->
  Maybe (PlusRepresentative site value)
plusRepresentativeAt key =
  IntMap.lookup (encodeDenseKey key) . plusFiberRepresentatives
{-# INLINE plusRepresentativeAt #-}

plusCanonicalClass ::
  Site site =>
  PlusConstruction site value mismatch restrictionFailure ->
  SiteObject site ->
  PlusKey ->
  Either
    (PlusConstructionFailure (SiteObject site) (SiteMorphism site) value mismatch restrictionFailure)
    (PlusClass (SiteObject site))
plusCanonicalClass plusValue objectValue key = do
  fiberValue <-
    note
      (PlusFiberMissing objectValue)
      (plusFiberAt objectValue plusValue)
  canonicalKey <-
    note
      (PlusClassKeyMissing objectValue key)
      (equivalenceRepresentative (plusFiberEquivalence fiberValue) key)
  pure
    PlusClass
      { plusClassTarget = objectValue,
        plusClassKey = canonicalKey
      }

note :: failure -> Maybe value -> Either failure value
note failure =
  maybe (Left failure) Right
{-# INLINE note #-}
