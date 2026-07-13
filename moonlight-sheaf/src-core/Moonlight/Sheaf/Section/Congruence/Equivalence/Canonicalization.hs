module Moonlight.Sheaf.Section.Congruence.Equivalence.Canonicalization
  ( mkEquivalenceEndomap,
    applyEquivalenceEndomap,
    imageAtValidatedEndomapKey,
    equivalenceImage,
    canonicalizeEquivalence,
    canonicalizeEquivalenceUnions,
    canonicalEquivalenceUnionClosure,
    normalizeEquivalenceRelation,
    touchedEquivalenceReps,
    expandSupportToTouchedEquivalenceBlocks,
  )
where

import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Moonlight.Core (DenseKey (..))
import Moonlight.Sheaf.Section.Congruence.Equivalence.Relation
  ( EquivalenceEndomap (..),
    EquivalenceMergeDelta (..),
    EquivalenceRelation (..),
    EquivalenceRelationError (..),
    applyCanonicalEquivalenceSeeds,
    chooseLowerRep,
    equivalenceFromPairs,
    equivalenceFromPartialRepMap,
    equivalencePairs,
    equivalenceRepAtBaseKeyOrSelf,
    equivalenceRepresentativeAtKey,
    normalizeEquivalentPairUnder,
    unsafeImageEquivalenceFromPairs,
    unsafeRelationFromTotalRepMap,
    validateDomainKeys,
    validateEquivalenceRelation,
  )

mkEquivalenceEndomap ::
  DenseKey rep =>
  IntSet ->
  IntMap rep ->
  Either EquivalenceRelationError (EquivalenceEndomap rep)
mkEquivalenceEndomap domainKeys sourceToTarget = do
  validateDomainKeys domainKeys
  traverse_ requireMapKeyInDomain (IntMap.keys sourceToTarget)
  traverse_ requireDomainKeyMapped (IntSet.toAscList domainKeys)
  traverse_ requireImageInDomain (IntMap.toAscList sourceToTarget)
  pure
    EquivalenceEndomap
      { eeDomain = domainKeys,
        eeMap = sourceToTarget
      }
  where
    requireMapKeyInDomain key =
      unless (IntSet.member key domainKeys) $
        Left (EquivalenceEndomapMapKeyOutsideDomain key)

    requireDomainKeyMapped key =
      unless (IntMap.member key sourceToTarget) $
        Left (EquivalenceEndomapMissingDomainKey key)

    requireImageInDomain (sourceKey, targetRep) = do
      let targetKey = encodeDenseKey targetRep
      unless (IntSet.member targetKey domainKeys) $
        Left (EquivalenceEndomapImageOutsideDomain sourceKey targetKey)

applyEquivalenceEndomap ::
  DenseKey rep =>
  EquivalenceEndomap rep ->
  EquivalenceRelation rep ->
  Either EquivalenceRelationError (EquivalenceRelation rep)
applyEquivalenceEndomap endomap sourceRelation = do
  validateEquivalenceRelation sourceRelation
  unless (eeDomain endomap == erDomain sourceRelation) $
    Left (EquivalenceEndomapDomainMismatch (eeDomain endomap) (erDomain sourceRelation))
  projectedPairs <-
    projectEquivalenceImagePairs
      imageOf
      (eeDomain endomap)
      sourceRelation
  equivalenceFromPairs (eeDomain endomap) projectedPairs
  where
    imageOf sourceKey =
      case IntMap.lookup sourceKey (eeMap endomap) of
        Nothing ->
          Left (EquivalenceEndomapMissingDomainKey sourceKey)
        Just targetRep ->
          Right targetRep

imageAtValidatedEndomapKey ::
  DenseKey rep =>
  EquivalenceEndomap rep ->
  Int ->
  rep
imageAtValidatedEndomapKey endomap key =
  IntMap.findWithDefault (decodeDenseKey key) key (eeMap endomap)

equivalenceImage ::
  DenseKey rep =>
  IntMap rep ->
  IntSet ->
  EquivalenceRelation rep ->
  Either EquivalenceRelationError (EquivalenceRelation rep)
equivalenceImage sourceToTarget targetDomain sourceRelation = do
  validateEquivalenceRelation sourceRelation
  validateDomainKeys targetDomain
  projectedPairs <-
    projectEquivalenceImagePairs
      imageOf
      targetDomain
      sourceRelation
  equivalenceFromPairs targetDomain projectedPairs
  where
    imageOf sourceKey =
      case IntMap.lookup sourceKey sourceToTarget of
        Nothing ->
          Left (EquivalenceImageMissingSourceKey sourceKey)
        Just targetRep ->
          Right targetRep

canonicalizeEquivalence ::
  DenseKey rep =>
  IntMap Int ->
  IntSet ->
  EquivalenceRelation rep ->
  Either EquivalenceRelationError (EquivalenceMergeDelta rep)
canonicalizeEquivalence oldToNew newDomain oldRelation = do
  validateEquivalenceRelation oldRelation
  projectedEntries <- traverse projectEntry (IntMap.toAscList (erRepOfBase oldRelation))
  let projectedRepOfBase = IntMap.fromListWith chooseLowerRep (catMaybes projectedEntries)
  projectedRelation <- equivalenceFromPartialRepMap newDomain projectedRepOfBase
  let changed =
        IntSet.fromList
          [ key
            | key <- IntSet.toAscList newDomain,
              equivalenceRepresentativeAtKey projectedRelation key
                /= equivalenceRepresentativeAtKey discreteProjectedRelation key
          ]
      introduced =
        IntSet.difference newDomain (IntMap.keysSet projectedRepOfBase)
  pure
    EquivalenceMergeDelta
      { emdRelation = projectedRelation,
        emdChanged = IntSet.union changed introduced
      }
  where
    remapKey key =
      IntMap.findWithDefault key key oldToNew

    projectEntry ::
      DenseKey rep =>
      (Int, rep) ->
      Either EquivalenceRelationError (Maybe (Int, rep))
    projectEntry (key, repValue) =
      let projectedKey = remapKey key
          projectedRepKey = remapKey (encodeDenseKey repValue)
       in if IntSet.member projectedKey newDomain
            then
              if IntSet.member projectedRepKey newDomain
                then Right (Just (projectedKey, decodeDenseKey projectedRepKey))
                else Left (EquivalenceRepresentativeOutsideDomain projectedKey projectedRepKey)
            else Right Nothing

    discreteProjectedRelation =
      unsafeRelationFromTotalRepMap
        newDomain
        (IntMap.fromSet decodeDenseKey newDomain)

canonicalizeEquivalenceUnions ::
  DenseKey rep =>
  EquivalenceRelation rep ->
  [(rep, rep)] ->
  [(rep, rep)]
canonicalizeEquivalenceUnions relationValue =
  catMaybes . snd . List.mapAccumL keepFirst Set.empty
  where
    keepFirst seen pairValue =
      case normalizeEquivalentPairUnder relationValue pairValue of
        Nothing ->
          (seen, Nothing)
        Just normalizedPair@(leftRep, rightRep) ->
          let pairKey = (encodeDenseKey leftRep, encodeDenseKey rightRep)
           in if Set.member pairKey seen
                then (seen, Nothing)
                else (Set.insert pairKey seen, Just normalizedPair)
{-# INLINEABLE canonicalizeEquivalenceUnions #-}

canonicalEquivalenceUnionClosure ::
  DenseKey rep =>
  EquivalenceRelation rep ->
  [(rep, rep)] ->
  [(rep, rep)]
canonicalEquivalenceUnionClosure relationValue rawUnions =
  let seeds =
        canonicalizeEquivalenceUnions relationValue rawUnions
      touchedKeys =
        IntSet.fromList
          [ encodeDenseKey repValue
            | (leftRep, rightRep) <- seeds,
              repValue <- [leftRep, rightRep]
          ]
      seedRelation =
        unsafeRelationFromTotalRepMap
          touchedKeys
          (IntMap.fromSet decodeDenseKey touchedKeys)
      merged =
        applyCanonicalEquivalenceSeeds seeds seedRelation
      componentMembers =
        erMembersByRep (emdRelation merged)
      rootedComponents =
        IntMap.fromList
          [ (rootKey, IntSet.toAscList rest)
            | members <- IntMap.elems componentMembers,
              Just (rootKey, rest) <- [IntSet.minView members]
          ]
   in [ (decodeDenseKey rootKey, decodeDenseKey memberKey)
        | (rootKey, rest) <- IntMap.toAscList rootedComponents,
          memberKey <- rest
      ]

normalizeEquivalenceRelation ::
  DenseKey rep =>
  EquivalenceRelation rep ->
  EquivalenceRelation rep
normalizeEquivalenceRelation relationValue =
  unsafeImageEquivalenceFromPairs
    (erDomain relationValue)
    (equivalencePairs relationValue)

touchedEquivalenceReps ::
  DenseKey rep =>
  EquivalenceRelation rep ->
  IntSet ->
  IntSet
touchedEquivalenceReps relationValue =
  IntSet.foldl'
    (\acc key -> IntSet.insert (encodeDenseKey (equivalenceRepAtBaseKeyOrSelf relationValue key)) acc)
    IntSet.empty
{-# INLINEABLE touchedEquivalenceReps #-}

expandSupportToTouchedEquivalenceBlocks ::
  DenseKey rep =>
  EquivalenceRelation rep ->
  IntSet ->
  IntSet
expandSupportToTouchedEquivalenceBlocks relationValue support =
  let touchedReps = touchedEquivalenceReps relationValue support
   in IntSet.foldl'
        (\acc repKey -> IntSet.union acc (IntMap.findWithDefault IntSet.empty repKey (erMembersByRep relationValue)))
        support
        touchedReps
{-# INLINEABLE expandSupportToTouchedEquivalenceBlocks #-}

projectEquivalenceImagePairs ::
  DenseKey rep =>
  (Int -> Either EquivalenceRelationError rep) ->
  IntSet ->
  EquivalenceRelation rep ->
  Either EquivalenceRelationError [(rep, rep)]
projectEquivalenceImagePairs imageOf targetDomain sourceRelation =
  traverse projectEntry (IntMap.toAscList (erRepOfBase sourceRelation))
  where
    projectEntry (sourceKey, sourceRep) = do
      let sourceRepKey = encodeDenseKey sourceRep
      targetBaseRep <- imageOf sourceKey
      targetRep <- imageOf sourceRepKey
      requireTargetKey sourceKey targetBaseRep
      requireTargetKey sourceRepKey targetRep
      pure (targetBaseRep, targetRep)

    requireTargetKey sourceKey targetRep = do
      let targetKey = encodeDenseKey targetRep
      unless (IntSet.member targetKey targetDomain) $
        Left (EquivalenceImageOutsideTarget sourceKey targetKey)
