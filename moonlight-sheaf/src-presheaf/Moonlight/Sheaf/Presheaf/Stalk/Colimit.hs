{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Presheaf.Stalk.Colimit
  ( NeighborhoodFilter (..),
    NeighborhoodFilterFailure (..),
    ColimitStalkRepKey (..),
    ColimitStalkRepresentative (..),
    FiniteColimitStalk (..),
    ColimitStalkFailure (..),
    ColimitFactorFailure (..),
    finiteColimitStalkAt,
    colimitStalkRepresentatives,
    colimitStalkClassOf,
    colimitStalkEquivalent,
    factorFiniteColimitStalk,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core (DenseKey (..))
import Moonlight.Sheaf.Index.Dense
  ( DenseIndex,
    denseIndexIndexedValues,
    denseIndexKeyIntSet,
    denseIndexKeyOf,
    denseIndexKeys,
    denseIndexValueAt,
    mkDenseIndex,
  )
import Moonlight.Sheaf.Presheaf.Finite
  ( FinitePresheaf (..),
    finiteFiberAt,
    finiteFiberValues,
  )
import Moonlight.Sheaf.Section.Congruence.Equivalence.Relation
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    Site (..),
    siteMorphismUniverse,
  )

-- | A concrete finite neighborhood filter for a site point.
--
-- The predicate names exactly which site objects contain the point; the
-- constructor validates that this chosen local cover is closed upward along
-- refinements and directed by common refinements before quotienting sections.
type NeighborhoodFilter :: Type -> Type -> Type
data NeighborhoodFilter point obj = NeighborhoodFilter
  { neighborhoodPoint :: !point,
    neighborhoodContains :: point -> obj -> Bool
  }

type NeighborhoodFilterFailure :: Type -> Type -> Type
data NeighborhoodFilterFailure obj mor
  = NeighborhoodFilterEmpty
  | NeighborhoodFilterNotUpwardClosed !(CheckedMorphism obj mor)
  | NeighborhoodFilterNotDirected !obj !obj
  deriving stock (Eq, Ord, Show)

type ColimitStalkRepKey :: Type
newtype ColimitStalkRepKey = ColimitStalkRepKey
  { unColimitStalkRepKey :: Int
  }
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum)

instance DenseKey ColimitStalkRepKey where
  encodeDenseKey =
    unColimitStalkRepKey
  {-# INLINE encodeDenseKey #-}

  decodeDenseKey =
    ColimitStalkRepKey
  {-# INLINE decodeDenseKey #-}

type ColimitStalkRepresentative :: Type -> Type -> Type
data ColimitStalkRepresentative obj value = ColimitStalkRepresentative
  { colimitRepObject :: !obj,
    colimitRepValue :: !value
  }
  deriving stock (Eq, Ord, Show)

type FiniteColimitStalk :: Type -> Type -> Type -> Type
data FiniteColimitStalk point obj value = FiniteColimitStalk
  { finiteColimitStalkPoint :: !point,
    finiteColimitStalkNeighborhoods :: ![obj],
    finiteColimitStalkRepresentativeIndex :: !(DenseIndex ColimitStalkRepKey (ColimitStalkRepresentative obj value)),
    finiteColimitStalkEquivalence :: !(EquivalenceRelation ColimitStalkRepKey)
  }
  deriving stock (Eq, Show)

type ColimitStalkFailure :: Type -> Type -> Type -> Type -> Type
data ColimitStalkFailure obj mor value restrictionFailure
  = ColimitNeighborhoodInvalid !(NeighborhoodFilterFailure obj mor)
  | ColimitFiberMissing !obj
  | ColimitRepresentativeMissing !(ColimitStalkRepresentative obj value)
  | ColimitRestrictionFailed !(CheckedMorphism obj mor) !value !restrictionFailure
  | ColimitRestrictedRepresentativeMissing !(CheckedMorphism obj mor) !value !value
  | ColimitEquivalenceInvalid !EquivalenceRelationError
  deriving stock (Eq, Show)

type ColimitFactorFailure :: Type -> Type -> Type -> Type
data ColimitFactorFailure obj value target
  = ColimitFactorRepresentativeMissing !ColimitStalkRepKey
  | ColimitFactorEmptyClass !ColimitStalkRepKey
  | ColimitFactorIncompatible
      !ColimitStalkRepKey
      !ColimitStalkRepKey
      !(ColimitStalkRepresentative obj value)
      !(ColimitStalkRepresentative obj value)
      !target
      !target
  deriving stock (Eq, Show)

finiteColimitStalkAt ::
  (Site site, Ord (SiteMorphism site), Ord value) =>
  NeighborhoodFilter point (SiteObject site) ->
  FinitePresheaf site value mismatch restrictionFailure ->
  Either
    (ColimitStalkFailure (SiteObject site) (SiteMorphism site) value restrictionFailure)
    (FiniteColimitStalk point (SiteObject site) value)
finiteColimitStalkAt filterValue presheaf = do
  neighborhoods <-
    first ColimitNeighborhoodInvalid $
      validateNeighborhoodFilter siteValue filterValue
  representatives <-
    fmap concat $
      traverse representativesAt neighborhoods
  let representativeIndex =
        mkDenseIndex representatives
  colimitEquivalencePairs <-
    fmap concat $
      traverse
        (restrictionPairs representativeIndex)
        (neighborhoodMorphisms siteValue (Set.fromList neighborhoods))
  relationValue <-
    first ColimitEquivalenceInvalid $
      equivalenceFromPairs
        (denseIndexKeyIntSet (denseIndexKeys representativeIndex))
        colimitEquivalencePairs
  pure
    FiniteColimitStalk
      { finiteColimitStalkPoint = neighborhoodPoint filterValue,
        finiteColimitStalkNeighborhoods = neighborhoods,
        finiteColimitStalkRepresentativeIndex = representativeIndex,
        finiteColimitStalkEquivalence = relationValue
      }
  where
    siteValue =
      fpSite presheaf

    representativesAt objectValue = do
      fiberValue <-
        maybe
          (Left (ColimitFiberMissing objectValue))
          Right
          (finiteFiberAt objectValue presheaf)
      pure
        [ ColimitStalkRepresentative
            { colimitRepObject = objectValue,
              colimitRepValue = sectionValue
            }
        | sectionValue <- finiteFiberValues fiberValue
        ]

    restrictionPairs representativeIndex morphismValue = do
      targetFiber <-
        maybe
          (Left (ColimitFiberMissing (cmTarget morphismValue)))
          Right
          (finiteFiberAt (cmTarget morphismValue) presheaf)
      traverse
        (restrictionPair representativeIndex morphismValue)
        (finiteFiberValues targetFiber)

    restrictionPair representativeIndex morphismValue sectionValue = do
      restrictedValue <-
        first
          (ColimitRestrictionFailed morphismValue sectionValue)
          (fpRestrict presheaf morphismValue sectionValue)
      targetKey <-
        representativeKey
          representativeIndex
          ColimitStalkRepresentative
            { colimitRepObject = cmTarget morphismValue,
              colimitRepValue = sectionValue
            }
      sourceKey <-
        maybe
          (Left (ColimitRestrictedRepresentativeMissing morphismValue sectionValue restrictedValue))
          Right
          ( denseIndexKeyOf
              ColimitStalkRepresentative
                { colimitRepObject = cmSource morphismValue,
                  colimitRepValue = restrictedValue
                }
              representativeIndex
          )
      pure (targetKey, sourceKey)

colimitStalkRepresentatives ::
  FiniteColimitStalk point obj value ->
  [(ColimitStalkRepKey, ColimitStalkRepresentative obj value)]
colimitStalkRepresentatives =
  denseIndexIndexedValues . finiteColimitStalkRepresentativeIndex
{-# INLINE colimitStalkRepresentatives #-}

colimitStalkClassOf ::
  (Ord obj, Ord value) =>
  FiniteColimitStalk point obj value ->
  ColimitStalkRepresentative obj value ->
  Maybe ColimitStalkRepKey
colimitStalkClassOf stalkValue representativeValue = do
  key <-
    denseIndexKeyOf
      representativeValue
      (finiteColimitStalkRepresentativeIndex stalkValue)
  equivalenceRepresentative (finiteColimitStalkEquivalence stalkValue) key
{-# INLINEABLE colimitStalkClassOf #-}

colimitStalkEquivalent ::
  (Ord obj, Ord value) =>
  FiniteColimitStalk point obj value ->
  ColimitStalkRepresentative obj value ->
  ColimitStalkRepresentative obj value ->
  Bool
colimitStalkEquivalent stalkValue leftRepresentative rightRepresentative =
  case
    ( colimitStalkClassOf stalkValue leftRepresentative,
      colimitStalkClassOf stalkValue rightRepresentative
    )
  of
    (Just leftKey, Just rightKey) ->
      equivalenceEquivalent
        (finiteColimitStalkEquivalence stalkValue)
        leftKey
        rightKey
    _ ->
      False
{-# INLINEABLE colimitStalkEquivalent #-}

factorFiniteColimitStalk ::
  Eq target =>
  FiniteColimitStalk point obj value ->
  (ColimitStalkRepresentative obj value -> target) ->
  Either
    (ColimitFactorFailure obj value target)
    (Map ColimitStalkRepKey target)
factorFiniteColimitStalk stalkValue mapRepresentative =
  Map.fromList
    <$> traverse
      factorClass
      (IntMap.toAscList (equivalenceMembersByRep (finiteColimitStalkEquivalence stalkValue)))
  where
    representativeIndex =
      finiteColimitStalkRepresentativeIndex stalkValue

    factorClass (classKey, members) = do
      memberValues <-
        traverse memberValue (IntSet.toAscList members)
      case memberValues of
        [] ->
          Left (ColimitFactorEmptyClass (decodeDenseKey classKey))
        firstMember : remainingMembers -> do
          traverse_ (validateFactorMember firstMember) remainingMembers
          let (_firstKey, _firstRepresentative, firstTarget) = firstMember
          pure (decodeDenseKey classKey, firstTarget)

    memberValue memberKey = do
      let representativeKeyValue = decodeDenseKey memberKey
      representativeValue <-
        maybe
          (Left (ColimitFactorRepresentativeMissing representativeKeyValue))
          Right
          (denseIndexValueAt representativeKeyValue representativeIndex)
      pure
        ( representativeKeyValue,
          representativeValue,
          mapRepresentative representativeValue
        )

validateNeighborhoodFilter ::
  (Site site, Ord (SiteMorphism site)) =>
  site ->
  NeighborhoodFilter point (SiteObject site) ->
  Either
    (NeighborhoodFilterFailure (SiteObject site) (SiteMorphism site))
    [SiteObject site]
validateNeighborhoodFilter siteValue filterValue = do
  case neighborhoods of
    [] ->
      Left NeighborhoodFilterEmpty
    _ -> do
      traverse_ validateUpwardClosure allMorphisms
      traverse_ validateDirected neighborhoodPairs
      pure neighborhoods
  where
    neighborhoods =
      filter
        (neighborhoodContains filterValue (neighborhoodPoint filterValue))
        (siteObjects siteValue)

    neighborhoodSet =
      Set.fromList neighborhoods

    allMorphisms =
      siteMorphismUniverse siteValue

    reachableTargetsBySource =
      Map.fromListWith
        Set.union
        [ (cmSource morphismValue, Set.singleton (cmTarget morphismValue))
        | morphismValue <- allMorphisms
        ]

    neighborhoodPairs =
      [ (leftObject, rightObject)
      | leftObject <- neighborhoods,
        rightObject <- neighborhoods
      ]

    validateUpwardClosure morphismValue =
      if Set.member (cmSource morphismValue) neighborhoodSet
        && not (Set.member (cmTarget morphismValue) neighborhoodSet)
        then Left (NeighborhoodFilterNotUpwardClosed morphismValue)
        else Right ()

    validateDirected (leftObject, rightObject) =
      if hasCommonRefinement leftObject rightObject
        then Right ()
        else Left (NeighborhoodFilterNotDirected leftObject rightObject)

    hasCommonRefinement leftObject rightObject =
      or
        [ morphismExists candidateObject leftObject
            && morphismExists candidateObject rightObject
        | candidateObject <- neighborhoods
        ]

    morphismExists sourceObject targetObject =
      Set.member
        targetObject
        (Map.findWithDefault Set.empty sourceObject reachableTargetsBySource)

neighborhoodMorphisms ::
  (Site site, Ord (SiteMorphism site)) =>
  site ->
  Set (SiteObject site) ->
  [CheckedMorphism (SiteObject site) (SiteMorphism site)]
neighborhoodMorphisms siteValue neighborhoodSet =
  [ morphismValue
  | morphismValue <- siteMorphismUniverse siteValue,
    Set.member (cmSource morphismValue) neighborhoodSet,
    Set.member (cmTarget morphismValue) neighborhoodSet
  ]

representativeKey ::
  (Ord obj, Ord value) =>
  DenseIndex ColimitStalkRepKey (ColimitStalkRepresentative obj value) ->
  ColimitStalkRepresentative obj value ->
  Either (ColimitStalkFailure obj mor value restrictionFailure) ColimitStalkRepKey
representativeKey representativeIndex representativeValue =
  maybe
    (Left (ColimitRepresentativeMissing representativeValue))
    Right
    (denseIndexKeyOf representativeValue representativeIndex)

validateFactorMember ::
  Eq target =>
  (ColimitStalkRepKey, ColimitStalkRepresentative obj value, target) ->
  (ColimitStalkRepKey, ColimitStalkRepresentative obj value, target) ->
  Either (ColimitFactorFailure obj value target) ()
validateFactorMember (expectedKey, expectedRepresentative, expectedTarget) (actualKey, actualRepresentative, actualTarget) =
  if actualTarget == expectedTarget
    then Right ()
    else
      Left
        ( ColimitFactorIncompatible
            expectedKey
            actualKey
            expectedRepresentative
            actualRepresentative
            expectedTarget
            actualTarget
        )
