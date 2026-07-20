module Moonlight.Sheaf.Section.Congruence.Equivalence.Relation
  ( EquivalenceRelation,
    EquivalenceEndomap,
    EquivalenceMergeDelta,
    EquivalenceDomain,
    DomainEquivalence,
    DomainEndomap,
    EquivalenceRelationError (..),
    validateDomainKeys,
    validateEquivalenceRelation,
    discreteEquivalence,
    equivalenceFromPartialRepMap,
    equivalenceFromPairs,
    mkEquivalenceEndomap,
    equivalenceEndomapDomain,
    equivalenceEndomapImageAtKey,
    extendEquivalenceDomain,
    equivalenceDomain,
    equivalenceRepOfBase,
    equivalenceMembersByRep,
    equivalenceMergeRelation,
    equivalenceMergeChanged,
    withEquivalenceDomain,
    mkDomainEquivalence,
    domainEquivalenceRaw,
    mkDomainEndomap,
    applyDomainEndomap,
    applyCheckedDomainEndomap,
    mergeDomainEquivalence,
    mergeCheckedDomainEquivalence,
    applyDomainEquivalenceMergesCounted,
    applyEquivalenceEndomap,
    equivalenceImage,
    canonicalizeEquivalence,
    canonicalizeEquivalenceUnions,
    canonicalEquivalenceUnionClosure,
    touchedEquivalenceReps,
    expandSupportToTouchedEquivalenceBlocks,
    equivalencePairs,
    equivalenceRepresentativeAtKey,
    equivalenceRepresentative,
    equivalenceRepAtBaseKeyOrSelf,
    equivalenceRepresentativeOrSelf,
    equivalenceEquivalent,
    normalizeEquivalentPairUnder,
    applyEquivalenceMergesCounted,
    applyCanonicalEquivalenceSeeds,
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
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as Set
import Moonlight.Core (DenseKey (..))
import Moonlight.Algebra (JoinSemilattice (..))

type EquivalenceRelation :: Type -> Type
data EquivalenceRelation rep = EquivalenceRelation
  { equivalenceDomainInternal :: !IntSet,
    equivalenceRepOfBaseInternal :: IntMap rep,
    equivalenceMembersByRepInternal :: IntMap IntSet,
    equivalenceClassOfBaseInternal :: !(IntMap Int),
    equivalenceLeaderByClassInternal :: !(IntMap rep),
    equivalenceMembersByClassInternal :: !(IntMap IntSet)
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
  { equivalenceEndomapDomainInternal :: !IntSet,
    equivalenceEndomapMapInternal :: !(IntMap rep)
  }
  deriving stock (Eq, Ord, Show)

type EquivalenceMergeDelta :: Type -> Type
data EquivalenceMergeDelta rep = EquivalenceMergeDelta
  { equivalenceMergeRelationInternal :: !(EquivalenceRelation rep),
    equivalenceMergeChangedInternal :: !IntSet
  }
  deriving stock (Eq, Ord, Show)

-- | A generative witness for one finite congruence domain.  The carrier
-- parameter is deliberately nominal: values minted by separate invocations
-- of 'withEquivalenceDomain' cannot be combined accidentally.
type EquivalenceDomain :: Type -> Type -> Type
type role EquivalenceDomain nominal nominal
newtype EquivalenceDomain carrier rep = EquivalenceDomain
  { equivalenceDomainKeysInternal :: IntSet
  }
  deriving stock (Eq, Ord, Show)

type DomainEquivalence :: Type -> Type -> Type
type role DomainEquivalence nominal nominal
newtype DomainEquivalence carrier rep = DomainEquivalence
  { domainEquivalenceRelationInternal :: EquivalenceRelation rep
  }
  deriving stock (Eq, Ord, Show)

type DomainEndomap :: Type -> Type -> Type
type role DomainEndomap nominal nominal
newtype DomainEndomap carrier rep = DomainEndomap
  { domainEndomapInternal :: EquivalenceEndomap rep
  }
  deriving stock (Eq, Ord, Show)

erDomain :: EquivalenceRelation rep -> IntSet
erDomain = equivalenceDomainInternal

erRepOfBase :: EquivalenceRelation rep -> IntMap rep
erRepOfBase = equivalenceRepOfBaseInternal

erMembersByRep :: EquivalenceRelation rep -> IntMap IntSet
erMembersByRep = equivalenceMembersByRepInternal

erClassOfBase :: EquivalenceRelation rep -> IntMap Int
erClassOfBase = equivalenceClassOfBaseInternal

erLeaderByClass :: EquivalenceRelation rep -> IntMap rep
erLeaderByClass = equivalenceLeaderByClassInternal

erMembersByClass :: EquivalenceRelation rep -> IntMap IntSet
erMembersByClass = equivalenceMembersByClassInternal

eeDomain :: EquivalenceEndomap rep -> IntSet
eeDomain = equivalenceEndomapDomainInternal

eeMap :: EquivalenceEndomap rep -> IntMap rep
eeMap = equivalenceEndomapMapInternal

emdRelation :: EquivalenceMergeDelta rep -> EquivalenceRelation rep
emdRelation = equivalenceMergeRelationInternal

emdChanged :: EquivalenceMergeDelta rep -> IntSet
emdChanged = equivalenceMergeChangedInternal

type EquivalenceRelationError :: Type
data EquivalenceRelationError
  = EquivalenceDomainContainsNegativeKey !Int
  | EquivalenceRepMapKeyOutsideDomain !Int
  | EquivalenceRepresentativeOutsideDomain !Int !Int
  | EquivalenceRepresentativeNotCanonical !Int !Int !Int
  | EquivalencePairOutsideDomain !Int
  | EquivalenceMembersInverseMismatch
  | EquivalenceDerivedClassStateMismatch
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
      { equivalenceEndomapDomainInternal = domainKeys,
        equivalenceEndomapMapInternal = sourceToTarget
      }
  where
    requireMapKeyInDomain key =
      unless (IntSet.member key domainKeys) $
        Left (EquivalenceEndomapMapKeyOutsideDomain key)

    requireDomainKeyMapped key =
      unless (IntMap.member key sourceToTarget) $
        Left (EquivalenceEndomapMissingDomainKey key)

    requireImageInDomain (sourceKey, targetRep) =
      unless (IntSet.member (encodeDenseKey targetRep) domainKeys) $
        Left (EquivalenceEndomapImageOutsideDomain sourceKey (encodeDenseKey targetRep))

equivalenceEndomapDomain :: EquivalenceEndomap rep -> IntSet
equivalenceEndomapDomain = eeDomain

equivalenceEndomapImageAtKey :: EquivalenceEndomap rep -> Int -> Maybe rep
equivalenceEndomapImageAtKey endomap key = IntMap.lookup key (eeMap endomap)

equivalenceMergeRelation :: EquivalenceMergeDelta rep -> EquivalenceRelation rep
equivalenceMergeRelation = emdRelation

equivalenceMergeChanged :: EquivalenceMergeDelta rep -> IntSet
equivalenceMergeChanged = emdChanged

-- | Mint a fresh nominal owner for a checked finite domain.
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
  unless (equivalenceDomainKeysInternal domain == erDomain relationValue) $
    Left
      ( EquivalenceDomainMismatch
          (equivalenceDomainKeysInternal domain)
          (erDomain relationValue)
      )
  pure (DomainEquivalence relationValue)

domainEquivalenceRaw :: DomainEquivalence carrier rep -> EquivalenceRelation rep
domainEquivalenceRaw =
  domainEquivalenceRelationInternal

mkDomainEndomap ::
  DenseKey rep =>
  EquivalenceDomain carrier rep ->
  IntMap rep ->
  Either EquivalenceRelationError (DomainEndomap carrier rep)
mkDomainEndomap domain sourceToTarget =
  fmap DomainEndomap $
    mkEquivalenceEndomap
      (equivalenceDomainKeysInternal domain)
      sourceToTarget

-- | Apply a total endomap minted for the same generative domain.  The owner
-- index is the compatibility proof; no runtime validation is repeated in the
-- restriction hot path.
applyDomainEndomap ::
  DenseKey rep =>
  DomainEndomap carrier rep ->
  DomainEquivalence carrier rep ->
  DomainEquivalence carrier rep
applyDomainEndomap
  (DomainEndomap endomap)
  (DomainEquivalence relationValue) =
    DomainEquivalence $
      imageEquivalenceFromPairsInternal
        (eeDomain endomap)
        (projectOwnedEndomapPairs endomap relationValue)
{-# INLINEABLE applyDomainEndomap #-}

-- | Dynamically reconcile an endomap with an existentially owned relation.
-- Prepared paths use 'applyDomainEndomap' and pay no repeated validation.
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
mergeDomainEquivalence
  (DomainEquivalence leftRelation)
  (DomainEquivalence rightRelation) =
    DomainEquivalence $
      applyEquivalenceMerges
        (equivalencePairs rightRelation)
        leftRelation
{-# INLINEABLE mergeDomainEquivalence #-}

-- | Dynamically reconcile two existential domain owners before gluing their
-- partitions.  Equal owner indices use 'mergeDomainEquivalence' directly.
mergeCheckedDomainEquivalence ::
  DenseKey rep =>
  DomainEquivalence leftCarrier rep ->
  DomainEquivalence rightCarrier rep ->
  Either EquivalenceRelationError (DomainEquivalence leftCarrier rep)
mergeCheckedDomainEquivalence
  (DomainEquivalence leftRelation)
  (DomainEquivalence rightRelation) = do
    unless (erDomain leftRelation == erDomain rightRelation) $
      Left (EquivalenceDomainMismatch (erDomain leftRelation) (erDomain rightRelation))
    pure $
      DomainEquivalence $
        applyEquivalenceMerges
          (equivalencePairs rightRelation)
          leftRelation
{-# INLINEABLE mergeCheckedDomainEquivalence #-}

instance DenseKey rep => JoinSemilattice (DomainEquivalence carrier rep) where
  join =
    mergeDomainEquivalence
  {-# INLINEABLE join #-}

applyDomainEquivalenceMergesCounted ::
  DenseKey rep =>
  [(rep, rep)] ->
  DomainEquivalence carrier rep ->
  Either
    EquivalenceRelationError
    (DomainEquivalence carrier rep, IntSet, Int)
applyDomainEquivalenceMergesCounted repPairs (DomainEquivalence relationValue) = do
  (mergeDelta, mergeCount) <-
    applyEquivalenceMergesCounted repPairs relationValue
  pure
    ( DomainEquivalence (emdRelation mergeDelta),
      emdChanged mergeDelta,
      mergeCount
    )
{-# INLINEABLE applyDomainEquivalenceMergesCounted #-}

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
  unless (derivedClassStateMatches relationValue) $
    Left EquivalenceDerivedClassStateMismatch
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

derivedClassStateMatches ::
  DenseKey rep =>
  EquivalenceRelation rep ->
  Bool
derivedClassStateMatches relationValue =
  IntMap.keysSet (erClassOfBase relationValue) == erDomain relationValue
    && IntMap.keysSet (erLeaderByClass relationValue) == classIds
    && all classMatches (IntMap.toAscList (erMembersByClass relationValue))
  where
    classIds =
      IntMap.keysSet (erMembersByClass relationValue)

    classMatches (classId, members) =
      case IntMap.lookup classId (erLeaderByClass relationValue) of
        Nothing ->
          False
        Just leader ->
          let leaderKey = encodeDenseKey leader
           in IntMap.lookup leaderKey (erMembersByRep relationValue) == Just members
                && all
                  (\memberKey -> IntMap.lookup memberKey (erClassOfBase relationValue) == Just classId)
                  (IntSet.toAscList members)
                && all
                  (\memberKey -> fmap encodeDenseKey (IntMap.lookup memberKey (erRepOfBase relationValue)) == Just leaderKey)
                  (IntSet.toAscList members)

discreteEquivalence ::
  DenseKey rep =>
  IntSet ->
  Either EquivalenceRelationError (EquivalenceRelation rep)
discreteEquivalence domainKeys = do
  validateDomainKeys domainKeys
  pure (relationFromTotalRepMapInternal domainKeys (IntMap.fromSet decodeDenseKey domainKeys))

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
  Either EquivalenceRelationError (EquivalenceRelation rep)
extendEquivalenceDomain newKeys relationValue = do
  validateDomainKeys newKeys
  let freshKeys =
        IntSet.difference newKeys (erDomain relationValue)
  pure $
    if IntSet.null freshKeys
      then relationValue
      else
        EquivalenceRelation
            { equivalenceDomainInternal = IntSet.union (erDomain relationValue) freshKeys,
              equivalenceRepOfBaseInternal =
                IntSet.foldl'
                  (\repMap key -> IntMap.insert key (decodeDenseKey key) repMap)
                  (erRepOfBase relationValue)
                  freshKeys,
              equivalenceMembersByRepInternal =
                IntSet.foldl'
                  (\members key -> IntMap.insert key (IntSet.singleton key) members)
                  (erMembersByRep relationValue)
                  freshKeys,
              equivalenceClassOfBaseInternal =
                IntSet.foldl'
                  (\classMap key -> IntMap.insert key key classMap)
                  (erClassOfBase relationValue)
                  freshKeys,
              equivalenceLeaderByClassInternal =
                IntSet.foldl'
                  (\leaders key -> IntMap.insert key (decodeDenseKey key) leaders)
                  (erLeaderByClass relationValue)
                  freshKeys,
              equivalenceMembersByClassInternal =
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
  Either EquivalenceRelationError (Maybe (rep, rep))
normalizeEquivalentPairUnder relationValue pairValue@(leftValue, rightValue) = do
  requireMember leftValue
  requireMember rightValue
  pure (normalizeEquivalentPairUnderInternal relationValue pairValue)
  where
    requireMember repValue =
      unless (IntSet.member (encodeDenseKey repValue) (erDomain relationValue)) $
        Left (EquivalencePairOutsideDomain (encodeDenseKey repValue))
{-# INLINEABLE normalizeEquivalentPairUnder #-}

normalizeEquivalentPairUnderInternal ::
  DenseKey rep =>
  EquivalenceRelation rep ->
  (rep, rep) ->
  Maybe (rep, rep)
normalizeEquivalentPairUnderInternal relationValue (leftValue, rightValue) =
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
{-# INLINEABLE normalizeEquivalentPairUnderInternal #-}

applyEquivalenceMergesCounted ::
  DenseKey rep =>
  [(rep, rep)] ->
  EquivalenceRelation rep ->
  Either EquivalenceRelationError (EquivalenceMergeDelta rep, Int)
applyEquivalenceMergesCounted repPairs relationValue = do
  traverse_ validatePair repPairs
  pure (applyEquivalenceMergesCountedInternal repPairs relationValue)
  where
    validatePair (leftRep, rightRep) = do
      validateRep leftRep
      validateRep rightRep

    validateRep repValue =
      unless (IntSet.member (encodeDenseKey repValue) (erDomain relationValue)) $
        Left (EquivalencePairOutsideDomain (encodeDenseKey repValue))

applyEquivalenceMergesCountedInternal ::
  DenseKey rep =>
  [(rep, rep)] ->
  EquivalenceRelation rep ->
  (EquivalenceMergeDelta rep, Int)
applyEquivalenceMergesCountedInternal repPairs relationValue =
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
      case normalizeEquivalentPairUnderInternal (emdRelation delta) pairValue of
        Nothing ->
          (delta, mergeCount)
        Just normalizedPair ->
          let mergeDelta = mergeEquivalenceRep (emdRelation delta) normalizedPair
           in ( mergeDelta
                  { equivalenceMergeChangedInternal = IntSet.union (emdChanged delta) (emdChanged mergeDelta)
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
      case normalizeEquivalentPairUnderInternal currentRelation pairValue of
        Nothing ->
          currentRelation
        Just normalizedPair ->
          emdRelation (mergeEquivalenceRep currentRelation normalizedPair)
{-# INLINEABLE applyEquivalenceMerges #-}

applyCanonicalEquivalenceSeeds ::
  DenseKey rep =>
  [(rep, rep)] ->
  EquivalenceRelation rep ->
  Either EquivalenceRelationError (EquivalenceMergeDelta rep)
applyCanonicalEquivalenceSeeds canonicalizedUnions relationValue =
  fst <$> applyEquivalenceMergesCounted canonicalizedUnions relationValue
{-# INLINE applyCanonicalEquivalenceSeeds #-}

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
      maybe
        (Left (EquivalenceEndomapMissingDomainKey sourceKey))
        Right
        (IntMap.lookup sourceKey (eeMap endomap))

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
      maybe
        (Left (EquivalenceImageMissingSourceKey sourceKey))
        Right
        (IntMap.lookup sourceKey sourceToTarget)

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
  discreteProjectedRelation <- discreteEquivalence newDomain
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
      { equivalenceMergeRelationInternal = projectedRelation,
        equivalenceMergeChangedInternal = IntSet.union changed introduced
      }
  where
    remapKey key =
      IntMap.findWithDefault key key oldToNew

    projectEntry (key, repValue) =
      let projectedKey = remapKey key
          projectedRepKey = remapKey (encodeDenseKey repValue)
       in if IntSet.member projectedKey newDomain
            then
              if IntSet.member projectedRepKey newDomain
                then Right (Just (projectedKey, decodeDenseKey projectedRepKey))
                else Left (EquivalenceRepresentativeOutsideDomain projectedKey projectedRepKey)
            else Right Nothing

canonicalizeEquivalenceUnions ::
  DenseKey rep =>
  EquivalenceRelation rep ->
  [(rep, rep)] ->
  Either EquivalenceRelationError [(rep, rep)]
canonicalizeEquivalenceUnions relationValue rawUnions =
  fmap (catMaybes . snd) $
    List.mapAccumL keepFirst Set.empty <$> traverse normalize rawUnions
  where
    normalize pairValue =
      normalizeEquivalentPairUnder relationValue pairValue

    keepFirst ::
      DenseKey key =>
      Set.Set (Int, Int) ->
      Maybe (key, key) ->
      (Set.Set (Int, Int), Maybe (key, key))
    keepFirst seen normalizedPair =
      case normalizedPair of
        Nothing ->
          (seen, Nothing)
        Just normalizedPairValue@(leftRep, rightRep) ->
          let pairKey = (encodeDenseKey leftRep, encodeDenseKey rightRep)
           in if Set.member pairKey seen
                then (seen, Nothing)
                else (Set.insert pairKey seen, Just normalizedPairValue)
{-# INLINEABLE canonicalizeEquivalenceUnions #-}

canonicalEquivalenceUnionClosure ::
  DenseKey rep =>
  EquivalenceRelation rep ->
  [(rep, rep)] ->
  Either EquivalenceRelationError [(rep, rep)]
canonicalEquivalenceUnionClosure relationValue rawUnions = do
  seeds <- canonicalizeEquivalenceUnions relationValue rawUnions
  let touchedKeys =
        IntSet.fromList
          [ encodeDenseKey repValue
            | (leftRep, rightRep) <- seeds,
              repValue <- [leftRep, rightRep]
          ]
  seedRelation <- discreteEquivalence touchedKeys
  merged <- applyCanonicalEquivalenceSeeds seeds seedRelation
  let componentMembers = erMembersByRep (emdRelation merged)
      rootedComponents =
        IntMap.fromList
          [ (rootKey, IntSet.toAscList rest)
            | members <- IntMap.elems componentMembers,
              Just (rootKey, rest) <- [IntSet.minView members]
          ]
  pure
    [ (decodeDenseKey rootKey, decodeDenseKey memberKey)
      | (rootKey, rest) <- IntMap.toAscList rootedComponents,
        memberKey <- rest
    ]

-- | Every public constructor and merge keeps the least dense key as class
-- leader, so a checked relation is already normalized.
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

    requireTargetKey sourceKey targetRep =
      unless (IntSet.member (encodeDenseKey targetRep) targetDomain) $
        Left (EquivalenceImageOutsideTarget sourceKey (encodeDenseKey targetRep))

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
                        { equivalenceMergeRelationInternal =
                            relationFromClassState
                              (erDomain relationValue)
                              classOfBase'
                              leaderByClass'
                              membersByClass',
                          equivalenceMergeChangedInternal = changedMembers
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

relationFromTotalRepMapInternal :: DenseKey rep => IntSet -> IntMap rep -> EquivalenceRelation rep
relationFromTotalRepMapInternal domainKeys repOfBase =
  let membersByClass =
        rebuildEquivalenceMembers repOfBase
      leaderByClass =
        IntMap.mapMaybeWithKey
          (\classId _ -> IntMap.lookup classId repOfBase)
          membersByClass
   in EquivalenceRelation
        { equivalenceDomainInternal = domainKeys,
          equivalenceRepOfBaseInternal = repOfBase,
          equivalenceMembersByRepInternal = membersByClass,
          equivalenceClassOfBaseInternal = fmap encodeDenseKey repOfBase,
          equivalenceLeaderByClassInternal = leaderByClass,
          equivalenceMembersByClassInternal = membersByClass
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
    { equivalenceDomainInternal = domainKeys,
      equivalenceRepOfBaseInternal = materializeRepOfBase classOfBase leaderByClass,
      equivalenceMembersByRepInternal = materializeMembersByRep leaderByClass membersByClass,
      equivalenceClassOfBaseInternal = classOfBase,
      equivalenceLeaderByClassInternal = leaderByClass,
      equivalenceMembersByClassInternal = membersByClass
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

imageEquivalenceFromPairsInternal ::
  DenseKey rep =>
  IntSet ->
  [(rep, rep)] ->
  EquivalenceRelation rep
imageEquivalenceFromPairsInternal targetDomain projectedPairs =
  applyEquivalenceMerges
    projectedPairs
    (relationFromTotalRepMapInternal targetDomain (IntMap.fromSet decodeDenseKey targetDomain))

projectOwnedEndomapPairs ::
  DenseKey rep =>
  EquivalenceEndomap rep ->
  EquivalenceRelation rep ->
  [(rep, rep)]
projectOwnedEndomapPairs endomap relationValue =
  IntMap.elems $
    IntMap.intersectionWith
      (,)
      (eeMap endomap)
      representativeImages
  where
    -- Both maps are total over the same nominally owned domain.  The
    -- mapMaybe is therefore only the representation-level composition of two
    -- checked finite maps; no public malformed value can make it discard an
    -- entry.
    representativeImages =
      IntMap.mapMaybe
        (\representative -> IntMap.lookup (encodeDenseKey representative) (eeMap endomap))
        (erRepOfBase relationValue)
{-# INLINEABLE projectOwnedEndomapPairs #-}

chooseLowerRep :: DenseKey rep => rep -> rep -> rep
chooseLowerRep leftRep rightRep =
  if encodeDenseKey leftRep <= encodeDenseKey rightRep
    then leftRep
    else rightRep
