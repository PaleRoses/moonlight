module Moonlight.Sheaf.Section.Stalk.Congruence.Mismatch
  ( CongruenceMismatch (..),
    carrierMismatchPair,
    visibleMismatchPair,
    representativeMismatchPair,
  )
where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Moonlight.Core (DenseKey (..))
import Moonlight.Sheaf.Section.Congruence.Equivalence.Relation
import Moonlight.Sheaf.Section.Stalk.Congruence.Carrier
  ( CarrierId,
    GlobalCarrier,
    carrierIndexedValues,
    globalCarrierId,
    sameCarrier,
  )

type CongruenceMismatch :: Type -> Type -> Type
data CongruenceMismatch rep atom
  = CongruenceCarrierMismatch
      !CarrierId
      !CarrierId
      ![(rep, atom)]
      ![(rep, atom)]
  | CongruenceVisibleMismatch
      !IntSet
      !IntSet
  | CongruenceRelationMismatch
      !EquivalenceRelationError
  | CongruenceRepresentativeMismatch
      !rep
      !rep
      !rep
  deriving stock (Eq, Ord, Show)

carrierMismatchPair ::
  (DenseKey rep, Eq atom) =>
  GlobalCarrier rep atom ->
  GlobalCarrier rep atom ->
  [CongruenceMismatch rep atom]
carrierMismatchPair leftCarrier rightCarrier =
  [ CongruenceCarrierMismatch
      (globalCarrierId leftCarrier)
      (globalCarrierId rightCarrier)
      (carrierIndexedValues leftCarrier)
      (carrierIndexedValues rightCarrier)
    | not (sameCarrier leftCarrier rightCarrier)
  ]
{-# INLINEABLE carrierMismatchPair #-}

visibleMismatchPair ::
  IntSet ->
  IntSet ->
  [CongruenceMismatch rep atom]
visibleMismatchPair leftVisible rightVisible =
  [ CongruenceVisibleMismatch leftVisible rightVisible
    | leftVisible /= rightVisible
  ]
{-# INLINE visibleMismatchPair #-}

representativeMismatchPair ::
  DenseKey rep =>
  (IntSet, EquivalenceRelation rep) ->
  (IntSet, EquivalenceRelation rep) ->
  [CongruenceMismatch rep atom]
representativeMismatchPair (leftVisible, leftRelation) (rightVisible, rightRelation) =
  [ CongruenceRepresentativeMismatch
      (decodeDenseKey key)
      leftRep
      rightRep
    | key <- IntSet.toAscList (IntSet.union leftVisible rightVisible),
      let leftRep = equivalenceRepAtBaseKeyOrSelf leftRelation key,
      let rightRep = equivalenceRepAtBaseKeyOrSelf rightRelation key,
      leftRep /= rightRep
  ]
{-# INLINEABLE representativeMismatchPair #-}
