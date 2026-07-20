{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Moonlight.Flow.Carrier.Reuse.Internal.Index.Shape
  ( SubsumptionIndex (..),
    ContainmentTrie (..),
    ContainmentSignature (..),
    SubsumptionIndexInvariantError (..),
    emptySubsumptionIndex,
    lookupEquivalentFactorShape,
    lookupContainmentCandidates,
    lookupSubsumptionEntryByCarrier,
    lookupRegisteredEntry,
    insertEntryIndex,
    dropSubsumptionCarrier,
    subsumptionIndexEntries,
    subsumptionIndexSize,
    validateSubsumptionIndex,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caCarrier,
  )
import Moonlight.Differential.Index.Reverse
  ( finishInvariantErrors,
    validateIntReverseIndex,
    validateMapReverseIndex,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Shape (RequestedFactorShape (..), SubsumptionEntry (..))
import Moonlight.Flow.Carrier.Reuse.Internal.Validity (ReuseValidity (..), ReuseValidityRequest (..), reuseExactValidityMatchesRequest, reuseTemporalViewMatchesRequest)
import Moonlight.Flow.Execution.Subsumption.CQContainment (CanonAtomPredicateKey, canonAtomPredicateKey)
import Moonlight.Flow.Internal.Digest (wordOfInt)
import Moonlight.Flow.Model.Schema.Digest (StableDigest128 (..), stableDigest128, stableDigestWords)
import Moonlight.Flow.Plan.Residual (ResidualCandidateKey, ResidualShape, ResidualTheoryRegistry, residualCandidateKey, residualCandidateKeysForRequest, residualShapeWords)
import Moonlight.Flow.Plan.Rewrite (PlanReuseShapeKey, planReuseShapeKeyWords)
import Moonlight.Flow.Plan.Shape (CanonAtom, CanonAtomMultiset, cbsDigest, factorShapeAtoms, factorShapeBoundary, factorShapeFragmentPayload, factorShapeOutputSchema, factorShapeResidual, factorShapeSourceSchema)
import Moonlight.Flow.Plan.Shape.Encode (canonAtomMultisetWords)
import Moonlight.Flow.Plan.Shape.Term (FragmentPayload (..), canonSlotKey)
import Moonlight.Differential.Index.Reverse.Batch
  ( addMembership,
    dropMapAxis,
    dropMembership,
    insertMapAxis,
  )

data ContainmentSignature = ContainmentSignature
  { csigShapeKey :: !PlanReuseShapeKey,
    csigFragmentKind :: {-# UNPACK #-} !Int,
    csigTagMultisetDigest :: !StableDigest128,
    csigSourceSchemaSlotCount :: {-# UNPACK #-} !Int,
    csigOutputSchemaSlotCount :: {-# UNPACK #-} !Int,
    csigResidualShape :: !ResidualShape,
    csigBoundaryDigest :: !StableDigest128,
    csigDigest :: !StableDigest128
  }
  deriving stock (Eq, Ord, Show, Read)

data ContainmentPlanResidualKey = ContainmentPlanResidualKey
  { cprShapeKey :: !PlanReuseShapeKey,
    cprResidualKey :: !ResidualCandidateKey,
    cprViewDigest :: !(Maybe StableDigest128)
  }
  deriving stock (Eq, Ord, Show, Read)

data ContainmentResidualViewKey = ContainmentResidualViewKey
  { crvResidualKey :: !ResidualCandidateKey,
    crvViewDigest :: !(Maybe StableDigest128)
  }
  deriving stock (Eq, Ord, Show, Read)

data ContainmentTrie ctx prop = ContainmentTrie
  { ctByPlanResidual :: !(Map ContainmentPlanResidualKey (Set (CarrierAddr ctx Carrier prop))),
    ctByResidualView :: !(Map ContainmentResidualViewKey (Set (CarrierAddr ctx Carrier prop))),
    ctByOutputSlot :: !(IntMap (Set (CarrierAddr ctx Carrier prop))),
    ctByOutputWidth :: !(IntMap (Set (CarrierAddr ctx Carrier prop))),
    ctByAtom :: !(Map CanonAtom (Set (CarrierAddr ctx Carrier prop))),
    ctByAtomPredicate :: !(Map CanonAtomPredicateKey (Set (CarrierAddr ctx Carrier prop))),
    ctEntries :: !(Set (CarrierAddr ctx Carrier prop))
  }
  deriving stock (Eq, Show)

data SubsumptionIndex ctx prop = SubsumptionIndex
  { siByCarrier ::
      !(Map (CarrierAddr ctx Carrier prop) (SubsumptionEntry ctx prop)),
    siFactorShapes ::
      !(Map PlanReuseShapeKey (Set (CarrierAddr ctx Carrier prop))),
    siByDep ::
      !(IntMap (Set (CarrierAddr ctx Carrier prop))),
    siByTopo ::
      !(IntMap (Set (CarrierAddr ctx Carrier prop))),
    siContainmentTrie :: !(ContainmentTrie ctx prop)
  }
  deriving stock (Eq, Show)

data SubsumptionIndexInvariantError ctx prop
  = SubsumptionCarrierReverseMissing !(CarrierAddr ctx Carrier prop)
  | SubsumptionCarrierReverseStale !(CarrierAddr ctx Carrier prop)
  | SubsumptionShapeReverseMissing !(CarrierAddr ctx Carrier prop) !PlanReuseShapeKey
  | SubsumptionShapeReverseStale !(CarrierAddr ctx Carrier prop) !PlanReuseShapeKey
  | SubsumptionDepReverseMissing !(CarrierAddr ctx Carrier prop) !Int
  | SubsumptionTopoReverseMissing !(CarrierAddr ctx Carrier prop) !Int
  | SubsumptionDepReverseStale !(CarrierAddr ctx Carrier prop) !Int
  | SubsumptionTopoReverseStale !(CarrierAddr ctx Carrier prop) !Int
  | SubsumptionEntryStoredUnderWrongShapeKey !PlanReuseShapeKey !PlanReuseShapeKey
  | SubsumptionContainmentTrieMissing !(CarrierAddr ctx Carrier prop)
  | SubsumptionContainmentTrieStale !(CarrierAddr ctx Carrier prop)
  | SubsumptionDerivedCarrierRegisteredAsSource !(CarrierAddr ctx Carrier prop)
  deriving stock (Eq, Show)

emptySubsumptionIndex :: SubsumptionIndex ctx prop
emptySubsumptionIndex =
  SubsumptionIndex
    { siByCarrier = Map.empty,
      siFactorShapes = Map.empty,
      siByDep = IntMap.empty,
      siByTopo = IntMap.empty,
      siContainmentTrie = emptyContainmentTrie
    }
{-# INLINE emptySubsumptionIndex #-}

emptyContainmentTrie :: ContainmentTrie ctx prop
emptyContainmentTrie =
  ContainmentTrie
    { ctByPlanResidual = Map.empty,
      ctByResidualView = Map.empty,
      ctByOutputSlot = IntMap.empty,
      ctByOutputWidth = IntMap.empty,
      ctByAtom = Map.empty,
      ctByAtomPredicate = Map.empty,
      ctEntries = Set.empty
    }
{-# INLINE emptyContainmentTrie #-}

lookupEquivalentFactorShape ::
  (Ord ctx, Ord prop) =>
  RequestedFactorShape ctx prop ->
  SubsumptionIndex ctx prop ->
  [SubsumptionEntry ctx prop]
lookupEquivalentFactorShape request index =
  [ entry
  | addr <-
      Set.toAscList
        (Map.findWithDefault Set.empty (rfsShapeKey request) (siFactorShapes index)),
    Just entry <- [Map.lookup addr (siByCarrier index)],
    seShapeKey entry == rfsShapeKey request,
    reuseExactValidityMatchesRequest (rfsValidity request) (seValidity entry)
  ]
{-# INLINE lookupEquivalentFactorShape #-}

lookupContainmentCandidates ::
  Ord ctx =>
  Ord prop =>
  ResidualTheoryRegistry ->
  SubsumptionIndex ctx prop ->
  RequestedFactorShape ctx prop ->
  [SubsumptionEntry ctx prop]
lookupContainmentCandidates registry index request =
  fmap snd $
    List.sortOn
      (containmentCandidateRank request . snd)
      [ (addr, entry)
      | addr <- Set.toAscList candidateAddrs,
        Set.member addr (ctEntries trie),
        Just entry <- [Map.lookup addr (siByCarrier index)],
        reuseTemporalViewMatchesRequest (rfsValidity request) (seValidity entry)
      ]
  where
    trie =
      siContainmentTrie index
    candidateAddrs =
      Set.union structuralCandidateAddrs homomorphicCandidateAddrs
    structuralCandidateAddrs =
      intersectCandidateSets
        ( structuralBase
            : structuralOutputFilters
              <> structuralAtomFilters
        )
    structuralBase =
      unionCandidateSets
        [ Map.findWithDefault
            Set.empty
            key
            (ctByPlanResidual trie)
        | key <- containmentPlanResidualKeysForRequest registry request
        ]
    structuralOutputFilters =
      [ IntMap.findWithDefault Set.empty (canonSlotKey slot) (ctByOutputSlot trie)
      | slot <- factorShapeOutputSchema (rfsShape request)
      ]
    structuralAtomFilters =
      [ Map.findWithDefault Set.empty atomValue (ctByAtom trie)
      | (atomValue, multiplicity) <- Map.toAscList (factorShapeAtoms (rfsShape request)),
        multiplicity > 0
      ]
    homomorphicCandidateAddrs =
      intersectCandidateSets
        ( homomorphicBase
            : homomorphicOutputWidthFilter
              : homomorphicPredicateFilters
        )
    homomorphicBase =
      unionCandidateSets
        [ Map.findWithDefault
            Set.empty
            key
            (ctByResidualView trie)
        | key <- containmentResidualViewKeysForRequest registry request
        ]
    homomorphicOutputWidthFilter =
      outputWidthAtLeast
        (length (factorShapeOutputSchema (rfsShape request)))
        (ctByOutputWidth trie)
    homomorphicPredicateFilters =
      [ Map.findWithDefault Set.empty predicateKey (ctByAtomPredicate trie)
      | predicateKey <- Set.toAscList (positivePredicateKeys (factorShapeAtoms (rfsShape request)))
      ]
{-# INLINE lookupContainmentCandidates #-}

lookupSubsumptionEntryByCarrier ::
  (Ord ctx, Ord prop) =>
  CarrierAddr ctx Carrier prop ->
  SubsumptionIndex ctx prop ->
  Maybe (SubsumptionEntry ctx prop)
lookupSubsumptionEntryByCarrier addr =
  Map.lookup addr . siByCarrier
{-# INLINE lookupSubsumptionEntryByCarrier #-}

lookupRegisteredEntry ::
  Ord ctx =>
  Ord prop =>
  PlanReuseShapeKey ->
  CarrierAddr ctx Carrier prop ->
  SubsumptionIndex ctx prop ->
  Maybe (SubsumptionEntry ctx prop)
lookupRegisteredEntry shapeKey addr index =
  case Map.lookup addr (siByCarrier index) of
    Just entry
      | seShapeKey entry == shapeKey
          && Set.member
            addr
            (Map.findWithDefault Set.empty shapeKey (siFactorShapes index)) ->
          Just entry
    _ ->
      Nothing
{-# INLINE lookupRegisteredEntry #-}

insertEntryIndex ::
  Ord ctx =>
  Ord prop =>
  SubsumptionEntry ctx prop ->
  SubsumptionIndex ctx prop ->
  SubsumptionIndex ctx prop
insertEntryIndex entry index0 =
  let index =
        dropSubsumptionCarrier (seCarrier entry) index0
   in index
        { siByCarrier =
            Map.insert (seCarrier entry) entry (siByCarrier index),
          siFactorShapes =
            insertMapAxis
              (seCarrier entry)
              (Set.singleton (seShapeKey entry))
              (siFactorShapes index),
          siByDep =
            addMembership (seCarrier entry) (seDeps entry) (siByDep index),
          siByTopo =
            addMembership (seCarrier entry) (seTopo entry) (siByTopo index),
          siContainmentTrie =
            insertContainmentTrieEntry entry (siContainmentTrie index)
        }
{-# INLINE insertEntryIndex #-}

dropSubsumptionCarrier ::
  Ord ctx =>
  Ord prop =>
  CarrierAddr ctx Carrier prop ->
  SubsumptionIndex ctx prop ->
  SubsumptionIndex ctx prop
dropSubsumptionCarrier addr index =
  case Map.lookup addr (siByCarrier index) of
    Nothing ->
      index
    Just entry ->
      removeEntry entry index
{-# INLINE dropSubsumptionCarrier #-}

subsumptionIndexEntries ::
  SubsumptionIndex ctx prop ->
  [SubsumptionEntry ctx prop]
subsumptionIndexEntries =
  Map.elems . siByCarrier
{-# INLINE subsumptionIndexEntries #-}

subsumptionIndexSize :: SubsumptionIndex ctx prop -> Int
subsumptionIndexSize =
  Map.size . siByCarrier
{-# INLINE subsumptionIndexSize #-}

validateSubsumptionIndex ::
  (Ord ctx, Ord prop) =>
  SubsumptionIndex ctx prop ->
  Either [SubsumptionIndexInvariantError ctx prop] ()
validateSubsumptionIndex index =
  finishInvariantErrors $
    carrierReverseMissingErrors
      <> carrierReverseStaleErrors
      <> validateMapReverseIndex
        subsumptionShapeKeys
        SubsumptionShapeReverseMissing
        SubsumptionShapeReverseStale
        rows
        (siFactorShapes index)
      <> storedShapeErrors
      <> validateIntReverseIndex
        subsumptionDepKeys
        SubsumptionDepReverseMissing
        SubsumptionDepReverseStale
        rows
        (siByDep index)
      <> validateIntReverseIndex
        subsumptionTopoKeys
        SubsumptionTopoReverseMissing
        SubsumptionTopoReverseStale
        rows
        (siByTopo index)
      <> containmentTrieMissingErrors
      <> containmentTrieStaleErrors
      <> validateMapReverseIndex
        containmentPlanResidualIndexKeys
        (\addr _key -> SubsumptionContainmentTrieMissing addr)
        (\addr _key -> SubsumptionContainmentTrieStale addr)
        rows
        (ctByPlanResidual trie)
      <> validateMapReverseIndex
        containmentResidualViewIndexKeys
        (\addr _key -> SubsumptionContainmentTrieMissing addr)
        (\addr _key -> SubsumptionContainmentTrieStale addr)
        rows
        (ctByResidualView trie)
      <> validateIntReverseIndex
        containmentOutputSlotIndexKeys
        (\addr _key -> SubsumptionContainmentTrieMissing addr)
        (\addr _key -> SubsumptionContainmentTrieStale addr)
        rows
        (ctByOutputSlot trie)
      <> validateIntReverseIndex
        containmentOutputWidthIndexKeys
        (\addr _key -> SubsumptionContainmentTrieMissing addr)
        (\addr _key -> SubsumptionContainmentTrieStale addr)
        rows
        (ctByOutputWidth trie)
      <> validateMapReverseIndex
        containmentAtomIndexKeys
        (\addr _key -> SubsumptionContainmentTrieMissing addr)
        (\addr _key -> SubsumptionContainmentTrieStale addr)
        rows
        (ctByAtom trie)
      <> validateMapReverseIndex
        containmentPredicateIndexKeys
        (\addr _key -> SubsumptionContainmentTrieMissing addr)
        (\addr _key -> SubsumptionContainmentTrieStale addr)
        rows
        (ctByAtomPredicate trie)
      <> derivedCarrierErrors
  where
    rows =
      siByCarrier index
    trie =
      siContainmentTrie index
    carrierReverseMissingErrors =
      [ SubsumptionCarrierReverseMissing (seCarrier entry)
      | (_addr, entry) <- Map.toAscList rows,
        Map.lookup (seCarrier entry) rows /= Just entry
      ]
    carrierReverseStaleErrors =
      [ SubsumptionCarrierReverseStale addr
      | (addr, entry) <- Map.toAscList rows,
        seCarrier entry /= addr
      ]
    storedShapeErrors =
      [ SubsumptionEntryStoredUnderWrongShapeKey shapeKey (seShapeKey entry)
      | (shapeKey, addrs) <- Map.toAscList (siFactorShapes index),
        addr <- Set.toAscList addrs,
        Just entry <- [Map.lookup addr rows],
        shapeKey /= seShapeKey entry
      ]
    containmentTrieMissingErrors =
      [ SubsumptionContainmentTrieMissing addr
      | addr <- Map.keys rows,
        not (Set.member addr (ctEntries trie))
      ]
    containmentTrieStaleErrors =
      [ SubsumptionContainmentTrieStale addr
      | addr <- Set.toAscList (ctEntries trie),
        not (Map.member addr rows)
      ]
    derivedCarrierErrors =
      [ SubsumptionDerivedCarrierRegisteredAsSource (seCarrier entry)
      | (_addr, entry) <- Map.toAscList rows,
        case caCarrier (seCarrier entry) of
          DerivedCarrier {} -> True
          _ -> False
      ]
{-# INLINE validateSubsumptionIndex #-}

subsumptionShapeKeys ::
  CarrierAddr ctx Carrier prop ->
  SubsumptionEntry ctx prop ->
  Set PlanReuseShapeKey
subsumptionShapeKeys _addr entry =
  Set.singleton (seShapeKey entry)
{-# INLINE subsumptionShapeKeys #-}

subsumptionDepKeys ::
  CarrierAddr ctx Carrier prop ->
  SubsumptionEntry ctx prop ->
  IntSet
subsumptionDepKeys _addr =
  seDeps
{-# INLINE subsumptionDepKeys #-}

subsumptionTopoKeys ::
  CarrierAddr ctx Carrier prop ->
  SubsumptionEntry ctx prop ->
  IntSet
subsumptionTopoKeys _addr =
  seTopo
{-# INLINE subsumptionTopoKeys #-}

containmentPlanResidualIndexKeys ::
  CarrierAddr ctx Carrier prop ->
  SubsumptionEntry ctx prop ->
  Set ContainmentPlanResidualKey
containmentPlanResidualIndexKeys _addr entry =
  Set.singleton (containmentPlanResidualKey entry)
{-# INLINE containmentPlanResidualIndexKeys #-}

containmentResidualViewIndexKeys ::
  CarrierAddr ctx Carrier prop ->
  SubsumptionEntry ctx prop ->
  Set ContainmentResidualViewKey
containmentResidualViewIndexKeys _addr entry =
  Set.singleton (containmentResidualViewKey entry)
{-# INLINE containmentResidualViewIndexKeys #-}

containmentOutputSlotIndexKeys ::
  CarrierAddr ctx Carrier prop ->
  SubsumptionEntry ctx prop ->
  IntSet
containmentOutputSlotIndexKeys _addr =
  containmentOutputSlots
{-# INLINE containmentOutputSlotIndexKeys #-}

containmentOutputWidthIndexKeys ::
  CarrierAddr ctx Carrier prop ->
  SubsumptionEntry ctx prop ->
  IntSet
containmentOutputWidthIndexKeys _addr entry =
  IntSet.singleton (containmentOutputWidth entry)
{-# INLINE containmentOutputWidthIndexKeys #-}

containmentAtomIndexKeys ::
  CarrierAddr ctx Carrier prop ->
  SubsumptionEntry ctx prop ->
  Set CanonAtom
containmentAtomIndexKeys _addr =
  containmentAtomsPresent
{-# INLINE containmentAtomIndexKeys #-}

containmentPredicateIndexKeys ::
  CarrierAddr ctx Carrier prop ->
  SubsumptionEntry ctx prop ->
  Set CanonAtomPredicateKey
containmentPredicateIndexKeys _addr =
  containmentPredicatesPresent
{-# INLINE containmentPredicateIndexKeys #-}

removeEntry ::
  Ord ctx =>
  Ord prop =>
  SubsumptionEntry ctx prop ->
  SubsumptionIndex ctx prop ->
  SubsumptionIndex ctx prop
removeEntry entry index =
  index
    { siByCarrier =
        Map.delete (seCarrier entry) (siByCarrier index),
      siFactorShapes =
        dropMapAxis
          (seCarrier entry)
          (Set.singleton (seShapeKey entry))
          (siFactorShapes index),
      siByDep =
        dropMembership (seCarrier entry) (seDeps entry) (siByDep index),
      siByTopo =
        dropMembership (seCarrier entry) (seTopo entry) (siByTopo index),
      siContainmentTrie =
        dropContainmentTrieEntry entry (siContainmentTrie index)
    }
{-# INLINE removeEntry #-}

unionCandidateSets ::
  Ord value =>
  [Set value] ->
  Set value
unionCandidateSets =
  Set.unions
{-# INLINE unionCandidateSets #-}

intersectCandidateSets ::
  Ord value =>
  [Set value] ->
  Set value
intersectCandidateSets sets
  | null sets =
      Set.empty
  | any Set.null sets =
      Set.empty
  | otherwise =
      case List.sortOn Set.size sets of
        [] ->
          Set.empty
        firstSet : restSets ->
          foldr Set.intersection firstSet restSets
{-# INLINE intersectCandidateSets #-}

outputWidthAtLeast ::
  Ord value =>
  Int ->
  IntMap (Set value) ->
  Set value
outputWidthAtLeast minimumWidth =
  Set.unions
    . fmap snd
    . filter ((>= minimumWidth) . fst)
    . IntMap.toAscList
{-# INLINE outputWidthAtLeast #-}

containmentCandidateRank ::
  RequestedFactorShape ctx prop ->
  SubsumptionEntry ctx prop ->
  (Bool, Bool, Int, Int, Int, Int)
containmentCandidateRank request entry =
  let source =
        seShape entry
      target =
        rfsShape request
      planClassMismatch =
        seShapeKey entry /= rfsShapeKey request
      boundaryMismatch =
        cbsDigest (factorShapeBoundary source) /= cbsDigest (factorShapeBoundary target)
      outputOverhang =
        length (factorShapeOutputSchema source) - length (factorShapeOutputSchema target)
      predicateOverhang =
        positivePredicateCount (factorShapeAtoms source) - positivePredicateCount (factorShapeAtoms target)
      atomOverhang =
        positiveAtomCount (factorShapeAtoms source) - positiveAtomCount (factorShapeAtoms target)
   in ( planClassMismatch,
        boundaryMismatch,
        abs outputOverhang,
        abs predicateOverhang,
        abs atomOverhang,
        csigSourceSchemaSlotCount (containmentSignature entry)
      )
{-# INLINE containmentCandidateRank #-}

positiveAtomCount ::
  CanonAtomMultiset ->
  Int
positiveAtomCount =
  Map.foldl' (\acc multiplicity -> if multiplicity > 0 then acc + multiplicity else acc) 0
{-# INLINE positiveAtomCount #-}

positivePredicateCount ::
  CanonAtomMultiset ->
  Int
positivePredicateCount =
  Set.size . positivePredicateKeys
{-# INLINE positivePredicateCount #-}

positivePredicateKeys ::
  CanonAtomMultiset ->
  Set CanonAtomPredicateKey
positivePredicateKeys atoms =
  Set.fromList
    [ canonAtomPredicateKey atomValue
    | (atomValue, multiplicity) <- Map.toAscList atoms,
      multiplicity > 0
    ]
{-# INLINE positivePredicateKeys #-}

insertContainmentTrieEntry ::
  Ord ctx =>
  Ord prop =>
  SubsumptionEntry ctx prop ->
  ContainmentTrie ctx prop ->
  ContainmentTrie ctx prop
insertContainmentTrieEntry entry trie0 =
  let addr =
        seCarrier entry
   in trie0
        { ctByPlanResidual =
            insertMapAxis
              addr
              (Set.singleton (containmentPlanResidualKey entry))
              (ctByPlanResidual trie0),
          ctByResidualView =
            insertMapAxis
              addr
              (Set.singleton (containmentResidualViewKey entry))
              (ctByResidualView trie0),
          ctByOutputSlot =
            addMembership addr (containmentOutputSlots entry) (ctByOutputSlot trie0),
          ctByOutputWidth =
            addMembership
              addr
              (IntSet.singleton (containmentOutputWidth entry))
              (ctByOutputWidth trie0),
          ctByAtom =
            insertMapAxis addr (containmentAtomsPresent entry) (ctByAtom trie0),
          ctByAtomPredicate =
            insertMapAxis addr (containmentPredicatesPresent entry) (ctByAtomPredicate trie0),
          ctEntries =
            Set.insert addr (ctEntries trie0)
        }
{-# INLINE insertContainmentTrieEntry #-}

dropContainmentTrieEntry ::
  Ord ctx =>
  Ord prop =>
  SubsumptionEntry ctx prop ->
  ContainmentTrie ctx prop ->
  ContainmentTrie ctx prop
dropContainmentTrieEntry entry trie0 =
  let addr =
        seCarrier entry
   in trie0
        { ctByPlanResidual =
            dropMapAxis
              addr
              (Set.singleton (containmentPlanResidualKey entry))
              (ctByPlanResidual trie0),
          ctByResidualView =
            dropMapAxis
              addr
              (Set.singleton (containmentResidualViewKey entry))
              (ctByResidualView trie0),
          ctByOutputSlot =
            dropMembership addr (containmentOutputSlots entry) (ctByOutputSlot trie0),
          ctByOutputWidth =
            dropMembership
              addr
              (IntSet.singleton (containmentOutputWidth entry))
              (ctByOutputWidth trie0),
          ctByAtom =
            dropMapAxis addr (containmentAtomsPresent entry) (ctByAtom trie0),
          ctByAtomPredicate =
            dropMapAxis addr (containmentPredicatesPresent entry) (ctByAtomPredicate trie0),
          ctEntries =
            Set.delete addr (ctEntries trie0)
        }
{-# INLINE dropContainmentTrieEntry #-}

containmentOutputWidth ::
  SubsumptionEntry ctx prop ->
  Int
containmentOutputWidth =
  length . factorShapeOutputSchema . seShape
{-# INLINE containmentOutputWidth #-}

containmentOutputSlots ::
  SubsumptionEntry ctx prop ->
  IntSet
containmentOutputSlots entry =
  IntSet.fromList
    [ canonSlotKey slot
    | slot <- factorShapeOutputSchema (seShape entry)
    ]
{-# INLINE containmentOutputSlots #-}

containmentAtomsPresent ::
  SubsumptionEntry ctx prop ->
  Set CanonAtom
containmentAtomsPresent entry =
  Set.fromList
    [ atomValue
    | (atomValue, multiplicity) <- Map.toAscList (factorShapeAtoms (seShape entry)),
      multiplicity > 0
    ]
{-# INLINE containmentAtomsPresent #-}

containmentPredicatesPresent ::
  SubsumptionEntry ctx prop ->
  Set CanonAtomPredicateKey
containmentPredicatesPresent =
  positivePredicateKeys . factorShapeAtoms . seShape
{-# INLINE containmentPredicatesPresent #-}

containmentResidualViewKey ::
  SubsumptionEntry ctx prop ->
  ContainmentResidualViewKey
containmentResidualViewKey entry =
  ContainmentResidualViewKey
    { crvResidualKey = residualCandidateKey (factorShapeResidual (seShape entry)),
      crvViewDigest = rvViewDigest (seValidity entry)
    }
{-# INLINE containmentResidualViewKey #-}

containmentResidualViewKeysForRequest ::
  ResidualTheoryRegistry ->
  RequestedFactorShape ctx prop ->
  [ContainmentResidualViewKey]
containmentResidualViewKeysForRequest residualRegistry request =
  [ ContainmentResidualViewKey
      { crvResidualKey = residualKey,
        crvViewDigest = rvrViewDigest (rfsValidity request)
      }
  | residualKey <-
      residualCandidateKeysForRequest
        residualRegistry
        (factorShapeResidual (rfsShape request))
  ]
{-# INLINE containmentResidualViewKeysForRequest #-}

containmentPlanResidualKey ::
  SubsumptionEntry ctx prop ->
  ContainmentPlanResidualKey
containmentPlanResidualKey entry =
  ContainmentPlanResidualKey
    { cprShapeKey = seShapeKey entry,
      cprResidualKey = residualCandidateKey (factorShapeResidual (seShape entry)),
      cprViewDigest = rvViewDigest (seValidity entry)
    }
{-# INLINE containmentPlanResidualKey #-}

containmentPlanResidualKeysForRequest ::
  ResidualTheoryRegistry ->
  RequestedFactorShape ctx prop ->
  [ContainmentPlanResidualKey]
containmentPlanResidualKeysForRequest residualRegistry request =
  [ ContainmentPlanResidualKey
      { cprShapeKey = rfsShapeKey request,
        cprResidualKey = residualKey,
        cprViewDigest = rvrViewDigest (rfsValidity request)
      }
  | residualKey <-
      residualCandidateKeysForRequest
        residualRegistry
        (factorShapeResidual (rfsShape request))
  ]
{-# INLINE containmentPlanResidualKeysForRequest #-}

containmentSignature ::
  SubsumptionEntry ctx prop ->
  ContainmentSignature
containmentSignature entry =
  let shape =
        seShape entry
      tagDigest =
        stableDigest128 (canonAtomMultisetWords (factorShapeAtoms shape))
      signatureNoDigest =
        ContainmentSignature
          { csigShapeKey = seShapeKey entry,
            csigFragmentKind = fragmentKind (factorShapeFragmentPayload shape),
            csigTagMultisetDigest = tagDigest,
            csigSourceSchemaSlotCount = length (factorShapeSourceSchema shape),
            csigOutputSchemaSlotCount = length (factorShapeOutputSchema shape),
            csigResidualShape = factorShapeResidual shape,
            csigBoundaryDigest = cbsDigest (factorShapeBoundary shape),
            csigDigest = StableDigest128 0 0
          }
      digestValue =
        stableDigest128
          ( [0x636f6e74536967]
              <> planReuseShapeKeyWords (csigShapeKey signatureNoDigest)
              <> [wordOfInt (csigFragmentKind signatureNoDigest)]
              <> stableDigestWords (csigTagMultisetDigest signatureNoDigest)
              <> [ wordOfInt (csigSourceSchemaSlotCount signatureNoDigest),
                   wordOfInt (csigOutputSchemaSlotCount signatureNoDigest)
                 ]
              <> residualShapeWords (csigResidualShape signatureNoDigest)
              <> stableDigestWords (csigBoundaryDigest signatureNoDigest)
          )
   in signatureNoDigest {csigDigest = digestValue}
{-# INLINE containmentSignature #-}

fragmentKind ::
  FragmentPayload ->
  Int
fragmentKind fragment =
  case fragment of
    RootFragmentPayload {} ->
      0
    BagFragmentPayload {} ->
      1
    SeparatorFragmentPayload {} ->
      2
{-# INLINE fragmentKind #-}
