-- | Quotient fact store and guard-evidence vocabulary.
-- Owns fact ids, class tuples, witnesses, typed present/absent/equality/
-- capability evidence, and set-map store algebra.
-- Contracts: stores are functions from fact ids to canonical class tuples on
-- the quotient; canonicalization rewrites tuple classes without new facts.
module Moonlight.Rewrite.System.Logic.Store
  ( FactId (..),
    FactTuple (..),
    FactWitness (..),
    GuardLiteralEvidence (..),
    GuardClauseEvidence (..),
    GuardEvidence,
    geClauses,
    geFactWitnesses,
    guardEvidenceFromClauses,
    FactStore,
    emptyFactStore,
    nullFactStore,
    factStoreSize,
    factsFor,
    factWitnesses,
    factTupleClassKeys,
    factWitnessClassKeys,
    factStoreClassKeys,
    changedScopedFactStoreClassKeys,
    hasFact,
    insertFact,
    insertFacts,
    unionFactStores,
    differenceFactStores,
    canonicalizeFactWitness,
    canonicalizeFactTuple,
    canonicalizeFactStore,
  )
where

import Data.Kind (Type)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core (ClassId, classIdKey)
import Moonlight.Rewrite.System.Logic.Delta
  ( differenceAlignedSetMap,
  )

type FactId :: Type
newtype FactId = FactId
  { unFactId :: Int
  }
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum)

type FactTuple :: Type
newtype FactTuple = FactTuple
  { unFactTuple :: [ClassId]
  }
  deriving stock (Eq, Ord, Show, Read)

type FactWitness :: Type
data FactWitness = FactWitness
  { fwFactId :: FactId,
    fwTuple :: FactTuple
  }
  deriving stock (Eq, Ord, Show, Read)

type GuardLiteralEvidence :: Type
data GuardLiteralEvidence
  = GuardFactPresent !FactWitness
  | GuardFactAbsent !FactId !FactTuple
  | GuardClassesEqual !ClassId !ClassId
  | GuardClassesDistinct !ClassId !ClassId
  | GuardCapabilityHeld
  | GuardCapabilityMissing
  | GuardAtomUnresolved
  deriving stock (Eq, Ord, Show, Read)

type GuardClauseEvidence :: Type
newtype GuardClauseEvidence = GuardClauseEvidence
  { gceSatisfiedLiterals :: [GuardLiteralEvidence]
  }
  deriving stock (Eq, Ord, Show, Read)

type GuardEvidence :: Type
data GuardEvidence = GuardEvidence ![GuardClauseEvidence] !(Set FactWitness)
  deriving stock (Eq, Ord)

instance Show GuardEvidence where
  showsPrec precedence guardEvidence =
    showParen (precedence > 10)
      ( showString "GuardEvidence "
          . showsPrec 11 (geClauses guardEvidence)
      )

instance Read GuardEvidence where
  readsPrec precedence =
    readParen (precedence > 10) $ \source ->
      [ (guardEvidenceFromClauses clauseEvidences, suffix)
        | ("GuardEvidence", clausesSource) <- lex source,
          (clauseEvidences, suffix) <- readsPrec 11 clausesSource
      ]

geClauses :: GuardEvidence -> [GuardClauseEvidence]
geClauses (GuardEvidence clauses _factWitnesses) =
  clauses
{-# INLINE geClauses #-}

geFactWitnesses :: GuardEvidence -> Set FactWitness
geFactWitnesses (GuardEvidence _clauses witnesses) =
  witnesses
{-# INLINE geFactWitnesses #-}

guardEvidenceFromClauses :: [GuardClauseEvidence] -> GuardEvidence
guardEvidenceFromClauses clauseEvidences =
  GuardEvidence clauseEvidences (foldMap guardClauseFactWitnesses clauseEvidences)
  where
    guardClauseFactWitnesses (GuardClauseEvidence literalEvidences) =
      Set.fromList
        [ factWitness
          | GuardFactPresent factWitness <- literalEvidences
        ]

type FactStore :: Type
newtype FactStore = FactStore
  { unFactStore :: Map FactId (Set FactTuple)
  }
  deriving stock (Eq, Show)

emptyFactStore :: FactStore
emptyFactStore =
  FactStore Map.empty

nullFactStore :: FactStore -> Bool
nullFactStore =
  Map.null . unFactStore

factStoreSize :: FactStore -> Int
factStoreSize =
  Map.foldr ((+) . Set.size) 0 . unFactStore

factsFor :: FactId -> FactStore -> Set FactTuple
factsFor factId =
  Map.findWithDefault Set.empty factId . unFactStore

factWitnesses :: FactStore -> Set FactWitness
factWitnesses (FactStore storeEntries) =
  Map.foldMapWithKey
    (\factId -> Set.map (FactWitness factId))
    storeEntries

factTupleClassKeys :: FactTuple -> IntSet
factTupleClassKeys (FactTuple classIds) =
  IntSet.fromList (fmap classIdKey classIds)
{-# INLINE factTupleClassKeys #-}

factWitnessClassKeys :: FactWitness -> IntSet
factWitnessClassKeys =
  factTupleClassKeys . fwTuple
{-# INLINE factWitnessClassKeys #-}

factStoreClassKeys :: FactStore -> IntSet
factStoreClassKeys =
  foldMap factWitnessClassKeys . factWitnesses
{-# INLINE factStoreClassKeys #-}

changedScopedFactStoreClassKeys ::
  Ord scope =>
  Map scope FactStore ->
  Map scope FactStore ->
  IntSet
changedScopedFactStoreClassKeys oldFacts newFacts =
  foldMap changedContextKeys (Map.keysSet oldFacts <> Map.keysSet newFacts)
  where
    changedContextKeys contextValue =
      let oldStore =
            Map.findWithDefault emptyFactStore contextValue oldFacts
          newStore =
            Map.findWithDefault emptyFactStore contextValue newFacts
       in factStoreClassKeys (differenceFactStores oldStore newStore)
            <> factStoreClassKeys (differenceFactStores newStore oldStore)
{-# INLINE changedScopedFactStoreClassKeys #-}

hasFact :: FactId -> FactTuple -> FactStore -> Bool
hasFact factId factTuple =
  Set.member factTuple . factsFor factId

insertFact :: FactId -> FactTuple -> FactStore -> FactStore
insertFact factId factTuple (FactStore storeEntries) =
  FactStore (Map.insertWith (<>) factId (Set.singleton factTuple) storeEntries)

insertFacts :: FactId -> Set FactTuple -> FactStore -> FactStore
insertFacts factId factTuples factStore@(FactStore storeEntries)
  | Set.null factTuples =
      factStore
  | otherwise =
      FactStore (Map.insertWith (<>) factId factTuples storeEntries)

unionFactStores :: FactStore -> FactStore -> FactStore
unionFactStores (FactStore leftEntries) (FactStore rightEntries) =
  FactStore (Map.unionWith (<>) leftEntries rightEntries)

differenceFactStores :: FactStore -> FactStore -> FactStore
differenceFactStores (FactStore leftEntries) (FactStore rightEntries) =
  FactStore (differenceAlignedSetMap leftEntries rightEntries)

canonicalizeFactWitness :: (ClassId -> ClassId) -> FactWitness -> FactWitness
canonicalizeFactWitness canonicalizeClassId factWitness =
  factWitness
    { fwTuple = canonicalizeFactTuple canonicalizeClassId (fwTuple factWitness)
    }

canonicalizeFactStore :: (ClassId -> ClassId) -> FactStore -> FactStore
canonicalizeFactStore canonicalizeClassId (FactStore storeEntries) =
  FactStore
    ( Map.map
        (Set.map (canonicalizeFactTuple canonicalizeClassId))
        storeEntries
    )

canonicalizeFactTuple :: (ClassId -> ClassId) -> FactTuple -> FactTuple
canonicalizeFactTuple canonicalizeClassId (FactTuple classIds) =
  FactTuple (fmap canonicalizeClassId classIds)
