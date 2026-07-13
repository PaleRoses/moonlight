module Moonlight.Sheaf.Section.Congruence.Equivalence.Relation
  ( EquivalenceRelation (EquivalenceRelation, erDomain, erRepOfBase, erMembersByRep),
    EquivalenceEndomap (..),
    EquivalenceMergeDelta (..),
    EquivalenceRelationError (..),
    validateDomainKeys,
    validateEquivalenceRelation,
    discreteEquivalence,
    equivalenceFromPartialRepMap,
    equivalenceFromPairs,
    extendEquivalenceDomain,
    equivalenceDomain,
    equivalenceRepOfBase,
    equivalenceMembersByRep,
    equivalencePairs,
    equivalenceRepresentativeAtKey,
    equivalenceRepresentative,
    equivalenceRepAtBaseKeyOrSelf,
    equivalenceRepresentativeOrSelf,
    equivalenceEquivalent,
    normalizeEquivalentPairUnder,
    applyEquivalenceMergesCounted,
    applyCanonicalEquivalenceSeeds,
    chooseLowerRep,
    rebuildEquivalenceMembers,
    unsafeRelationFromTotalRepMap,
    unsafeImageEquivalenceFromPairs,
  )
where

import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Moonlight.Core (DenseKey (..))

type EquivalenceRelation :: Type -> Type
data EquivalenceRelation rep = EquivalenceRelation
  { erDomain :: !IntSet,
    erRepOfBase :: IntMap rep,
    erMembersByRep :: IntMap IntSet,
    erClassOfBase :: !(IntMap Int),
    erLeaderByClass :: !(IntMap rep),
    erMembersByClass :: !(IntMap IntSet)
  }

instance Eq rep => Eq (EquivalenceRelation rep) where
  leftRelation == rightRelation =
    (erDomain leftRelation, erRepOfBase leftRelation, erMembersByRep leftRelation)
      == (erDomain rightRelation, erRepOfBase rightRelation, erMembersByRep rightRelation)

instance Ord rep => Ord (EquivalenceRelation rep) where
  compare leftRelation rightRelation =
    compare
      (erDomain leftRelation, erRepOfBase leftRelation, erMembersByRep leftRelation)
      (erDomain rightRelation, erRepOfBase rightRelation, erMembersByRep rightRelation)

instance Show rep => Show (EquivalenceRelation rep) where
  showsPrec precedence relationValue =
    showParen (precedence > 10) $
      showString "EquivalenceRelation {erDomain = "
        . shows (erDomain relationValue)
        . showString ", erRepOfBase = "
        . shows (erRepOfBase relationValue)
        . showString ", erMembersByRep = "
        . shows (erMembersByRep relationValue)
        . showString "}"

type EquivalenceEndomap :: Type -> Type
data EquivalenceEndomap rep = EquivalenceEndomap
  { eeDomain :: !IntSet,
    eeMap :: !(IntMap rep)
  }
  deriving stock (Eq, Ord, Show)

type EquivalenceMergeDelta :: Type -> Type
data EquivalenceMergeDelta rep = EquivalenceMergeDelta
  { emdRelation :: !(EquivalenceRelation rep),
    emdChanged :: !IntSet
  }
  deriving stock (Eq, Ord, Show)

type EquivalenceRelationError :: Type
data EquivalenceRelationError
  = EquivalenceDomainContainsNegativeKey !Int
  | EquivalenceRepMapKeyOutsideDomain !Int
  | EquivalenceRepresentativeOutsideDomain !Int !Int
  | EquivalenceRepresentativeNotCanonical !Int !Int !Int
  | EquivalencePairOutsideDomain !Int
  | EquivalenceMembersInverseMismatch
  | EquivalenceRepMapMissingDomainKey !Int
  | EquivalenceImageMissingSourceKey !Int
  | EquivalenceImageOutsideTarget !Int !Int
  | EquivalenceEndomapMapKeyOutsideDomain !Int
  | EquivalenceEndomapMissingDomainKey !Int
  | EquivalenceEndomapImageOutsideDomain !Int !Int
  | EquivalenceEndomapDomainMismatch !IntSet !IntSet
  | EquivalenceDomainMismatch !IntSet !IntSet
  deriving stock (Eq, Ord, Show)

validateDomainKeys :: IntSet -> Either EquivalenceRelationError ()
validateDomainKeys domainKeys =
  case IntSet.lookupMin domainKeys of
    Just key | key < 0 ->
      Left (EquivalenceDomainContainsNegativeKey key)
    _ ->
      Right ()

validateEquivalenceRelation ::
  DenseKey rep =>
  EquivalenceRelation rep ->
  Either EquivalenceRelationError ()
validateEquivalenceRelation relationValue = do
  validateDomainKeys (erDomain relationValue)
  let repKeys = IntMap.keysSet (erRepOfBase relationValue)
  traverse_ requireRepMapKeyInDomain (IntSet.toAscList repKeys)
  traverse_ requireDomainKeyMapped (IntSet.toAscList (erDomain relationValue))
  traverse_ validateRepresentative (IntMap.toAscList (erRepOfBase relationValue))
  unless (rebuildEquivalenceMembers (erRepOfBase relationValue) == erMembersByRep relationValue) $
    Left EquivalenceMembersInverseMismatch
  where
    requireRepMapKeyInDomain key =
      unless (IntSet.member key (erDomain relationValue)) $
        Left (EquivalenceRepMapKeyOutsideDomain key)

    requireDomainKeyMapped key =
      unless (IntMap.member key (erRepOfBase relationValue)) $
        Left (EquivalenceRepMapMissingDomainKey key)

    validateRepresentative (key, repValue) = do
      let repKey = encodeDenseKey repValue
      unless (IntSet.member repKey (erDomain relationValue)) $
        Left (EquivalenceRepresentativeOutsideDomain key repKey)
      case IntMap.lookup repKey (erRepOfBase relationValue) of
        Nothing ->
          Left (EquivalenceRepMapMissingDomainKey repKey)
        Just ownerRep ->
          let ownerKey = encodeDenseKey ownerRep
           in unless (ownerKey == repKey) $
                Left (EquivalenceRepresentativeNotCanonical key repKey ownerKey)

discreteEquivalence ::
  DenseKey rep =>
  IntSet ->
  Either EquivalenceRelationError (EquivalenceRelation rep)
discreteEquivalence domainKeys = do
  validateDomainKeys domainKeys
  pure (unsafeRelationFromTotalRepMap domainKeys (IntMap.fromSet decodeDenseKey domainKeys))

equivalenceFromPartialRepMap ::
  DenseKey rep =>
  IntSet ->
  IntMap rep ->
  Either EquivalenceRelationError (EquivalenceRelation rep)
equivalenceFromPartialRepMap domainKeys rawRepOfBase = do
  validateDomainKeys domainKeys
  traverse_ validateEntry (IntMap.toAscList rawRepOfBase)
  relation0 <- discreteEquivalence domainKeys
  pure (applyEquivalenceMerges relationPairs relation0)
  where
    relationPairs =
      [ (decodeDenseKey key, repValue)
        | (key, repValue) <- IntMap.toAscList rawRepOfBase
      ]

    validateEntry (key, repValue) = do
      unless (IntSet.member key domainKeys) $
        Left (EquivalenceRepMapKeyOutsideDomain key)
      let repKey = encodeDenseKey repValue
      unless (IntSet.member repKey domainKeys) $
        Left (EquivalenceRepresentativeOutsideDomain key repKey)

equivalenceFromPairs ::
  DenseKey rep =>
  IntSet ->
  [(rep, rep)] ->
  Either EquivalenceRelationError (EquivalenceRelation rep)
equivalenceFromPairs domainKeys pairs = do
  validateDomainKeys domainKeys
  traverse_ validatePair pairs
  relation0 <- discreteEquivalence domainKeys
  pure (applyEquivalenceMerges pairs relation0)
  where
    validatePair (leftRep, rightRep) = do
      requirePairKey (encodeDenseKey leftRep)
      requirePairKey (encodeDenseKey rightRep)

    requirePairKey key =
      unless (IntSet.member key domainKeys) $
        Left (EquivalencePairOutsideDomain key)

-- | Extend the relation's domain with the given keys, each entering as its
-- own singleton class. Keys already in the domain are untouched.
extendEquivalenceDomain ::
  DenseKey rep =>
  IntSet ->
  EquivalenceRelation rep ->
  EquivalenceRelation rep
extendEquivalenceDomain newKeys relationValue =
  let freshKeys =
        IntSet.difference newKeys (erDomain relationValue)
   in if IntSet.null freshKeys
        then relationValue
        else
          EquivalenceRelation
            { erDomain = IntSet.union (erDomain relationValue) freshKeys,
              erRepOfBase =
                IntSet.foldl'
                  (\repMap key -> IntMap.insert key (decodeDenseKey key) repMap)
                  (erRepOfBase relationValue)
                  freshKeys,
              erMembersByRep =
                IntSet.foldl'
                  (\members key -> IntMap.insert key (IntSet.singleton key) members)
                  (erMembersByRep relationValue)
                  freshKeys,
              erClassOfBase =
                IntSet.foldl'
                  (\classMap key -> IntMap.insert key key classMap)
                  (erClassOfBase relationValue)
                  freshKeys,
              erLeaderByClass =
                IntSet.foldl'
                  (\leaders key -> IntMap.insert key (decodeDenseKey key) leaders)
                  (erLeaderByClass relationValue)
                  freshKeys,
              erMembersByClass =
                IntSet.foldl'
                  (\members key -> IntMap.insert key (IntSet.singleton key) members)
                  (erMembersByClass relationValue)
                  freshKeys
            }
{-# INLINEABLE extendEquivalenceDomain #-}

equivalenceDomain :: EquivalenceRelation rep -> IntSet
equivalenceDomain =
  erDomain

equivalenceRepOfBase :: EquivalenceRelation rep -> IntMap rep
equivalenceRepOfBase =
  erRepOfBase

equivalenceMembersByRep :: EquivalenceRelation rep -> IntMap IntSet
equivalenceMembersByRep =
  erMembersByRep

equivalencePairs ::
  DenseKey rep =>
  EquivalenceRelation rep ->
  [(rep, rep)]
equivalencePairs relationValue =
  [ (decodeDenseKey key, repValue)
    | (key, repValue) <- IntMap.toAscList (erRepOfBase relationValue),
      key /= encodeDenseKey repValue
  ]
{-# INLINEABLE equivalencePairs #-}

equivalenceRepresentativeAtKey ::
  EquivalenceRelation rep ->
  Int ->
  Maybe rep
equivalenceRepresentativeAtKey relationValue key =
  if IntSet.member key (erDomain relationValue)
    then do
      classId <- IntMap.lookup key (erClassOfBase relationValue)
      IntMap.lookup classId (erLeaderByClass relationValue)
    else Nothing
{-# INLINE equivalenceRepresentativeAtKey #-}

equivalenceRepresentative ::
  DenseKey rep =>
  EquivalenceRelation rep ->
  rep ->
  Maybe rep
equivalenceRepresentative relationValue repValue =
  equivalenceRepresentativeAtKey relationValue (encodeDenseKey repValue)
{-# INLINE equivalenceRepresentative #-}

equivalenceRepAtBaseKeyOrSelf ::
  DenseKey rep =>
  EquivalenceRelation rep ->
  Int ->
  rep
equivalenceRepAtBaseKeyOrSelf relationValue key =
  fromMaybe (decodeDenseKey key) (equivalenceRepresentativeAtKey relationValue key)
{-# INLINE equivalenceRepAtBaseKeyOrSelf #-}

equivalenceRepresentativeOrSelf ::
  DenseKey rep =>
  EquivalenceRelation rep ->
  rep ->
  rep
equivalenceRepresentativeOrSelf relationValue repValue =
  fromMaybe repValue (equivalenceRepresentative relationValue repValue)
{-# INLINE equivalenceRepresentativeOrSelf #-}

equivalenceEquivalent ::
  DenseKey rep =>
  EquivalenceRelation rep ->
  rep ->
  rep ->
  Bool
equivalenceEquivalent relationValue leftRep rightRep =
  case (equivalenceRepresentative relationValue leftRep, equivalenceRepresentative relationValue rightRep) of
    (Just leftRepresentative, Just rightRepresentative) ->
      leftRepresentative == rightRepresentative
    _ ->
      False
{-# INLINE equivalenceEquivalent #-}

normalizeEquivalentPairUnder ::
  DenseKey rep =>
  EquivalenceRelation rep ->
  (rep, rep) ->
  Maybe (rep, rep)
normalizeEquivalentPairUnder relationValue (leftValue, rightValue) =
  let leftRep = equivalenceRepresentativeOrSelf relationValue leftValue
      rightRep = equivalenceRepresentativeOrSelf relationValue rightValue
      leftKey = encodeDenseKey leftRep
      rightKey = encodeDenseKey rightRep
   in if leftKey == rightKey
        then Nothing
        else
          Just $
            if leftKey <= rightKey
              then (leftRep, rightRep)
              else (rightRep, leftRep)
{-# INLINEABLE normalizeEquivalentPairUnder #-}

applyEquivalenceMergesCounted ::
  DenseKey rep =>
  [(rep, rep)] ->
  EquivalenceRelation rep ->
  (EquivalenceMergeDelta rep, Int)
applyEquivalenceMergesCounted repPairs relationValue =
  List.foldl'
    step
    (EquivalenceMergeDelta relationValue IntSet.empty, 0)
    repPairs
  where
    step ::
      DenseKey rep =>
      (EquivalenceMergeDelta rep, Int) ->
      (rep, rep) ->
      (EquivalenceMergeDelta rep, Int)
    step (delta, mergeCount) pairValue =
      case normalizeEquivalentPairUnder (emdRelation delta) pairValue of
        Nothing ->
          (delta, mergeCount)
        Just normalizedPair ->
          let mergeDelta = mergeEquivalenceRep (emdRelation delta) normalizedPair
           in ( mergeDelta
                  { emdChanged = IntSet.union (emdChanged delta) (emdChanged mergeDelta)
                  },
                mergeCount + 1
              )
{-# INLINEABLE applyEquivalenceMergesCounted #-}

applyEquivalenceMerges ::
  DenseKey rep =>
  [(rep, rep)] ->
  EquivalenceRelation rep ->
  EquivalenceRelation rep
applyEquivalenceMerges repPairs relationValue =
  List.foldl' step relationValue repPairs
  where
    step ::
      DenseKey rep =>
      EquivalenceRelation rep ->
      (rep, rep) ->
      EquivalenceRelation rep
    step currentRelation pairValue =
      case normalizeEquivalentPairUnder currentRelation pairValue of
        Nothing ->
          currentRelation
        Just normalizedPair ->
          emdRelation (mergeEquivalenceRep currentRelation normalizedPair)
{-# INLINEABLE applyEquivalenceMerges #-}

applyCanonicalEquivalenceSeeds ::
  DenseKey rep =>
  [(rep, rep)] ->
  EquivalenceRelation rep ->
  EquivalenceMergeDelta rep
applyCanonicalEquivalenceSeeds canonicalizedUnions relationValue =
  fst (applyEquivalenceMergesCounted canonicalizedUnions relationValue)
{-# INLINE applyCanonicalEquivalenceSeeds #-}

mergeEquivalenceRep ::
  DenseKey rep =>
  EquivalenceRelation rep ->
  (rep, rep) ->
  EquivalenceMergeDelta rep
mergeEquivalenceRep relationValue (leaderRep, loserRep) =
  let leaderKey = encodeDenseKey leaderRep
      loserKey = encodeDenseKey loserRep
   in if leaderKey == loserKey
        then EquivalenceMergeDelta relationValue IntSet.empty
        else
          case (IntMap.lookup leaderKey (erClassOfBase relationValue), IntMap.lookup loserKey (erClassOfBase relationValue)) of
            (Just leaderClass, Just loserClass)
              | leaderClass /= loserClass ->
                  let leaderMembers =
                        IntMap.findWithDefault IntSet.empty leaderClass (erMembersByClass relationValue)
                      loserMembers =
                        IntMap.findWithDefault IntSet.empty loserClass (erMembersByClass relationValue)
                      leaderClassValue =
                        IntMap.findWithDefault leaderRep leaderClass (erLeaderByClass relationValue)
                      loserClassValue =
                        IntMap.findWithDefault loserRep loserClass (erLeaderByClass relationValue)
                      survivingLeader =
                        chooseLowerRep leaderClassValue loserClassValue
                      changedMembers =
                        if encodeDenseKey survivingLeader == encodeDenseKey leaderClassValue
                          then loserMembers
                          else leaderMembers
                      (survivingClass, absorbedClass, absorbedMembers) =
                        if IntSet.size leaderMembers >= IntSet.size loserMembers
                          then (leaderClass, loserClass, loserMembers)
                          else (loserClass, leaderClass, leaderMembers)
                      classOfBase' =
                        IntSet.foldl'
                          (\classMap memberKey -> IntMap.insert memberKey survivingClass classMap)
                          (erClassOfBase relationValue)
                          absorbedMembers
                      membersByClass' =
                        IntMap.insert survivingClass (IntSet.union leaderMembers loserMembers) $
                          IntMap.delete absorbedClass (erMembersByClass relationValue)
                      leaderByClass' =
                        IntMap.insert survivingClass survivingLeader $
                          IntMap.delete absorbedClass (erLeaderByClass relationValue)
                   in EquivalenceMergeDelta
                        { emdRelation =
                            relationFromClassState
                              (erDomain relationValue)
                              classOfBase'
                              leaderByClass'
                              membersByClass',
                          emdChanged = changedMembers
                        }
            _ ->
              EquivalenceMergeDelta relationValue IntSet.empty

rebuildEquivalenceMembers :: DenseKey rep => IntMap rep -> IntMap IntSet
rebuildEquivalenceMembers =
  IntMap.foldlWithKey'
    ( \acc key repValue ->
        IntMap.insertWith IntSet.union
          (encodeDenseKey repValue)
          (IntSet.singleton key)
          acc
    )
    IntMap.empty

unsafeRelationFromTotalRepMap :: DenseKey rep => IntSet -> IntMap rep -> EquivalenceRelation rep
unsafeRelationFromTotalRepMap domainKeys repOfBase =
  let membersByClass =
        rebuildEquivalenceMembers repOfBase
      leaderByClass =
        IntMap.mapMaybeWithKey
          (\classId _ -> IntMap.lookup classId repOfBase)
          membersByClass
   in EquivalenceRelation
        { erDomain = domainKeys,
          erRepOfBase = repOfBase,
          erMembersByRep = membersByClass,
          erClassOfBase = fmap encodeDenseKey repOfBase,
          erLeaderByClass = leaderByClass,
          erMembersByClass = membersByClass
        }

relationFromClassState ::
  DenseKey rep =>
  IntSet ->
  IntMap Int ->
  IntMap rep ->
  IntMap IntSet ->
  EquivalenceRelation rep
relationFromClassState domainKeys classOfBase leaderByClass membersByClass =
  EquivalenceRelation
    { erDomain = domainKeys,
      erRepOfBase = materializeRepOfBase classOfBase leaderByClass,
      erMembersByRep = materializeMembersByRep leaderByClass membersByClass,
      erClassOfBase = classOfBase,
      erLeaderByClass = leaderByClass,
      erMembersByClass = membersByClass
    }

materializeRepOfBase :: IntMap Int -> IntMap rep -> IntMap rep
materializeRepOfBase classOfBase leaderByClass =
  IntMap.mapMaybe (`IntMap.lookup` leaderByClass) classOfBase

materializeMembersByRep :: DenseKey rep => IntMap rep -> IntMap IntSet -> IntMap IntSet
materializeMembersByRep leaderByClass membersByClass =
  IntMap.fromListWith
    IntSet.union
    [ (encodeDenseKey leaderValue, members)
      | (classId, members) <- IntMap.toAscList membersByClass,
        Just leaderValue <- [IntMap.lookup classId leaderByClass]
    ]

unsafeImageEquivalenceFromPairs ::
  DenseKey rep =>
  IntSet ->
  [(rep, rep)] ->
  EquivalenceRelation rep
unsafeImageEquivalenceFromPairs targetDomain projectedPairs =
  applyEquivalenceMerges
    projectedPairs
    (unsafeRelationFromTotalRepMap targetDomain (IntMap.fromSet decodeDenseKey targetDomain))

chooseLowerRep :: DenseKey rep => rep -> rep -> rep
chooseLowerRep leftRep rightRep =
  if encodeDenseKey leftRep <= encodeDenseKey rightRep
    then leftRep
    else rightRep
