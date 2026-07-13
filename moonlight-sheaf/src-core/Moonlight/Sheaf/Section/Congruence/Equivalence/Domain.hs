module Moonlight.Sheaf.Section.Congruence.Equivalence.Domain
  ( EquivalenceDomain,
    DomainEquivalence,
    DomainEndomap,
    withEquivalenceDomain,
    mkDomainEquivalence,
    domainEquivalenceRaw,
    mkDomainEndomap,
    applyDomainEndomap,
    applyCheckedDomainEndomap,
    mergeDomainEquivalence,
    mergeCheckedDomainEquivalence,
    normalizeDomainEquivalence,
    applyDomainEquivalenceMergesCounted,
  )
where

import Control.Monad (unless)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.Kind (Type)
import Moonlight.Algebra
  ( JoinSemilattice (..),
  )
import Moonlight.Core (DenseKey (..))
import Moonlight.Sheaf.Section.Congruence.Equivalence.Canonicalization
  ( applyEquivalenceEndomap,
    imageAtValidatedEndomapKey,
    mkEquivalenceEndomap,
    normalizeEquivalenceRelation,
  )
import Moonlight.Sheaf.Section.Congruence.Equivalence.Relation
  ( EquivalenceEndomap (..),
    EquivalenceMergeDelta (..),
    EquivalenceRelation (..),
    EquivalenceRelationError (..),
    applyCanonicalEquivalenceSeeds,
    applyEquivalenceMergesCounted,
    equivalencePairs,
    unsafeImageEquivalenceFromPairs,
    validateDomainKeys,
    validateEquivalenceRelation,
  )

type EquivalenceDomain :: Type -> Type -> Type
type role EquivalenceDomain nominal nominal
newtype EquivalenceDomain carrier rep = EquivalenceDomain
  { edDomain :: IntSet
  }
  deriving stock (Eq, Ord, Show)

type DomainEquivalence :: Type -> Type -> Type
type role DomainEquivalence nominal nominal
newtype DomainEquivalence carrier rep = DomainEquivalence
  { deRelation :: EquivalenceRelation rep
  }
  deriving stock (Eq, Ord, Show)

type DomainEndomap :: Type -> Type -> Type
type role DomainEndomap nominal nominal
newtype DomainEndomap carrier rep = DomainEndomap
  { demEndomap :: EquivalenceEndomap rep
  }
  deriving stock (Eq, Ord, Show)

withEquivalenceDomain ::
  IntSet ->
  (forall carrier. EquivalenceDomain carrier rep -> result) ->
  Either EquivalenceRelationError result
withEquivalenceDomain domainKeys continue = do
  validateDomainKeys domainKeys
  pure (continue (EquivalenceDomain domainKeys))

mkDomainEquivalence ::
  DenseKey rep =>
  EquivalenceDomain carrier rep ->
  EquivalenceRelation rep ->
  Either EquivalenceRelationError (DomainEquivalence carrier rep)
mkDomainEquivalence domain relationValue = do
  validateEquivalenceRelation relationValue
  unless (edDomain domain == erDomain relationValue) $
    Left (EquivalenceDomainMismatch (edDomain domain) (erDomain relationValue))
  pure
    DomainEquivalence
      { deRelation = relationValue
      }

domainEquivalenceRaw :: DomainEquivalence carrier rep -> EquivalenceRelation rep
domainEquivalenceRaw =
  deRelation

mkDomainEndomap ::
  DenseKey rep =>
  EquivalenceDomain carrier rep ->
  IntMap rep ->
  Either EquivalenceRelationError (DomainEndomap carrier rep)
mkDomainEndomap domain sourceToTarget = do
  endomap <- mkEquivalenceEndomap (edDomain domain) sourceToTarget
  pure
    DomainEndomap
      { demEndomap = endomap
      }

applyDomainEndomap ::
  DenseKey rep =>
  DomainEndomap carrier rep ->
  DomainEquivalence carrier rep ->
  DomainEquivalence carrier rep
applyDomainEndomap (DomainEndomap endomap) (DomainEquivalence relationValue) =
  DomainEquivalence
    { deRelation =
        unsafeImageEquivalenceFromPairs
          (eeDomain endomap)
          [ (targetBaseRep, targetRepresentativeRep)
            | (sourceKey, sourceRep) <- IntMap.toAscList (erRepOfBase relationValue),
              let targetBaseRep = imageAtValidatedEndomapKey endomap sourceKey,
              let targetRepresentativeRep = imageAtValidatedEndomapKey endomap (encodeDenseKey sourceRep)
          ]
    }

applyCheckedDomainEndomap ::
  DenseKey rep =>
  EquivalenceEndomap rep ->
  DomainEquivalence carrier rep ->
  Either EquivalenceRelationError (DomainEquivalence carrier rep)
applyCheckedDomainEndomap endomap (DomainEquivalence relationValue) =
  fmap DomainEquivalence (applyEquivalenceEndomap endomap relationValue)
{-# INLINEABLE applyCheckedDomainEndomap #-}

mergeDomainEquivalence ::
  DenseKey rep =>
  DomainEquivalence carrier rep ->
  DomainEquivalence carrier rep ->
  DomainEquivalence carrier rep
mergeDomainEquivalence leftRelation rightRelation =
  DomainEquivalence
    { deRelation =
        emdRelation $
          applyCanonicalEquivalenceSeeds
            (equivalencePairs (deRelation rightRelation))
            (deRelation leftRelation)
    }

mergeCheckedDomainEquivalence ::
  DenseKey rep =>
  DomainEquivalence leftCarrier rep ->
  DomainEquivalence rightCarrier rep ->
  Either EquivalenceRelationError (DomainEquivalence leftCarrier rep)
mergeCheckedDomainEquivalence (DomainEquivalence leftRelation) (DomainEquivalence rightRelation) = do
  unless (erDomain leftRelation == erDomain rightRelation) $
    Left (EquivalenceDomainMismatch (erDomain leftRelation) (erDomain rightRelation))
  pure (mergeDomainEquivalence (DomainEquivalence leftRelation) (DomainEquivalence rightRelation))
{-# INLINEABLE mergeCheckedDomainEquivalence #-}

instance DenseKey rep => JoinSemilattice (DomainEquivalence carrier rep) where
  join =
    mergeDomainEquivalence
  {-# INLINEABLE join #-}

normalizeDomainEquivalence ::
  DenseKey rep =>
  DomainEquivalence carrier rep ->
  DomainEquivalence carrier rep
normalizeDomainEquivalence relationValue =
  relationValue
    { deRelation =
        normalizeEquivalenceRelation (deRelation relationValue)
    }

applyDomainEquivalenceMergesCounted ::
  DenseKey rep =>
  [(rep, rep)] ->
  DomainEquivalence carrier rep ->
  (DomainEquivalence carrier rep, IntSet, Int)
applyDomainEquivalenceMergesCounted repPairs relationValue =
  let (mergeDelta, mergeCount) =
        applyEquivalenceMergesCounted repPairs (deRelation relationValue)
   in ( DomainEquivalence
          { deRelation = emdRelation mergeDelta
          },
        emdChanged mergeDelta,
        mergeCount
      )
{-# INLINEABLE applyDomainEquivalenceMergesCounted #-}
