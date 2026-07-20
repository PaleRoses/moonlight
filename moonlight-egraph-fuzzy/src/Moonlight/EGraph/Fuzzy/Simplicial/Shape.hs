{-# LANGUAGE StrictData #-}

module Moonlight.EGraph.Fuzzy.Simplicial.Shape
  ( ParallelTagFingerprint (..),
    ParallelFaceKey (..),
    TriangleRequirement (..),
    ParallelRequirement (..),
    ParallelEvidence (..),
    ParallelShapeValidationError (..),
    ParallelBlockRegistryValidationError (..),
    BlockCanonicalization (..),
    ParallelBlockRegistry,
    mkParallelBlockRegistry,
    CanonicalizationRegistry,
    mkCanonicalizationRegistry,
    ParallelShapeAlgebra (..),
    mkParallelShapeAlgebra,
    parallelBlocksFor,
    canonicalPatternBlockOrder,
    canonicalClassBlockOrder,
    parallelFaceKey,
    tagFingerprintOf,
    validateParallelPatternShape,
    validateParallelClassShape,
  )
where

import Data.Function ((&))
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.EGraph.Fuzzy.Simplicial.Complex.Internal
  ( ParallelTagFingerprint (..),
    SimplexId,
    orderedPair,
    pairwise,
    safeIndex,
  )
import Moonlight.Core (HasConstructorTag (..))
import Moonlight.Core
  ( Pattern
  )
import Moonlight.EGraph.Pure.Types (ClassId)
import Prelude

type ParallelFaceKey :: Type
data ParallelFaceKey = ParallelFaceKey
  { pfkRootClass :: !ClassId,
    pfkTagFingerprint :: !ParallelTagFingerprint,
    pfkSlots :: !(Int, Int),
    pfkChildren :: !(ClassId, ClassId)
  }
  deriving stock (Eq, Ord, Show)

type TriangleRequirement :: Type
data TriangleRequirement = TriangleRequirement
  { trLeftSlot :: !Int,
    trRightSlot :: !Int,
    trFaceId :: !SimplexId,
    trBoundary0 :: !SimplexId,
    trBoundary1 :: !SimplexId,
    trBoundary2 :: !SimplexId
  }
  deriving stock (Eq, Ord, Show)

type ParallelRequirement :: Type
data ParallelRequirement = ParallelRequirement
  { prRawSlots :: ![Int],
    prCanonicalPatternSlots :: ![Int],
    prTriangles :: ![TriangleRequirement]
  }
  deriving stock (Eq, Ord, Show)

type ParallelEvidence :: Type
data ParallelEvidence = ParallelEvidence
  { peFaceKey :: !ParallelFaceKey,
    peFaceSimplex :: !(Maybe SimplexId),
    peLeftSlot :: !Int,
    peRightSlot :: !Int,
    peLeftChild :: !ClassId,
    peRightChild :: !ClassId
  }
  deriving stock (Show)

instance Eq ParallelEvidence where
  left == right =
    parallelEvidenceIdentity left == parallelEvidenceIdentity right

instance Ord ParallelEvidence where
  compare left right =
    compare (parallelEvidenceIdentity left) (parallelEvidenceIdentity right)

type ParallelShapeValidationError :: Type -> Type
data ParallelShapeValidationError tag
  = ParallelBlockSlotOutOfRange !tag !Int !Int
  | ParallelBlocksOverlap !tag !Int !IntSet !IntSet
  | CanonicalizationChangesMembership ![Int] ![Int]
  | CanonicalizationChangesCardinality ![Int] ![Int]
  deriving stock (Eq, Ord, Show)

type ParallelBlockRegistryValidationError :: Type -> Type
data ParallelBlockRegistryValidationError tag
  = RegistryBlockSlotOutOfRange !tag !Int !Int
  | RegistryBlocksOverlap !tag !Int !IntSet !IntSet
  deriving stock (Eq, Ord, Show)

type ParallelBlockRegistry :: Type -> Type
newtype ParallelBlockRegistry tag = ParallelBlockRegistry (Map tag (Map Int [IntSet]))

type BlockCanonicalization :: Type
data BlockCanonicalization
  = PreserveBlockOrder
  | SortBlockAscending
  | ExplicitBlockOrder [Int]
  deriving stock (Eq, Ord, Show)

type CanonicalizationRegistry :: Type -> Type
newtype CanonicalizationRegistry tag = CanonicalizationRegistry (Map tag BlockCanonicalization)

type ParallelShapeAlgebra :: (Type -> Type) -> Type
data ParallelShapeAlgebra f = ParallelShapeAlgebra
  { psaParallelBlockRegistry :: ParallelBlockRegistry (ConstructorTag f),
    psaCanonicalizationRegistry :: CanonicalizationRegistry (ConstructorTag f),
    psaTagFingerprint :: ConstructorTag f -> ParallelTagFingerprint
  }

mkParallelBlockRegistry ::
  Ord tag =>
  [(tag, Int, [IntSet])] ->
  Either [ParallelBlockRegistryValidationError tag] (ParallelBlockRegistry tag)
mkParallelBlockRegistry declarations =
  let registryMap =
        Map.fromListWith (Map.unionWith (<>)) [(tag, Map.singleton arity slotSets) | (tag, arity, slotSets) <- declarations]
      validationErrors =
        registryMap
          & Map.foldMapWithKey
            ( \tag arityMap ->
                arityMap
                  & Map.foldMapWithKey
                    (\arity slotSets -> validateRegistryDeclaration tag arity slotSets)
            )
   in if null validationErrors
        then Right (ParallelBlockRegistry registryMap)
        else Left validationErrors

mkCanonicalizationRegistry :: Ord tag => [(tag, BlockCanonicalization)] -> CanonicalizationRegistry tag
mkCanonicalizationRegistry declarations =
  CanonicalizationRegistry (Map.fromList declarations)

mkParallelShapeAlgebra ::
  ParallelBlockRegistry (ConstructorTag f) ->
  CanonicalizationRegistry (ConstructorTag f) ->
  (ConstructorTag f -> ParallelTagFingerprint) ->
  ParallelShapeAlgebra f
mkParallelShapeAlgebra parallelBlockRegistry canonicalizationRegistry tagFingerprint =
  ParallelShapeAlgebra
    { psaParallelBlockRegistry = parallelBlockRegistry,
      psaCanonicalizationRegistry = canonicalizationRegistry,
      psaTagFingerprint = tagFingerprint
    }

parallelBlocksFor :: HasConstructorTag f => ParallelShapeAlgebra f -> ConstructorTag f -> Int -> [IntSet]
parallelBlocksFor parallelShape tag arity =
  registryBlocksFor (psaParallelBlockRegistry parallelShape) tag arity
    & filter ((>= 2) . IntSet.size)

canonicalPatternBlockOrder :: HasConstructorTag f => ParallelShapeAlgebra f -> f (Pattern f) -> [Int] -> [Int]
canonicalPatternBlockOrder parallelShape node =
  applyCanonicalization
    (canonicalizationFor (psaCanonicalizationRegistry parallelShape) (constructorTag node))
    node

canonicalClassBlockOrder :: HasConstructorTag f => ParallelShapeAlgebra f -> f ClassId -> [Int] -> [Int]
canonicalClassBlockOrder parallelShape node =
  applyCanonicalization
    (canonicalizationFor (psaCanonicalizationRegistry parallelShape) (constructorTag node))
    node

applyCanonicalization :: (Foldable f, Ord a) => BlockCanonicalization -> f a -> [Int] -> [Int]
applyCanonicalization canonicalization node rawSlots =
  case canonicalization of
    PreserveBlockOrder ->
      rawSlots
    SortBlockAscending ->
      rawSlots & sortOn (\slot -> safeIndex slot (toList node))
    ExplicitBlockOrder proposedSlots ->
      if null (validateCanonicalization rawSlots proposedSlots)
        then canonicalBlockOrder proposedSlots rawSlots
        else rawSlots

canonicalBlockOrder :: [Int] -> [Int] -> [Int]
canonicalBlockOrder proposedSlots rawSlots =
  let rawSlotSet = IntSet.fromList rawSlots
      normalizedProposed =
        proposedSlots
          & filter (`IntSet.member` rawSlotSet)
          & dedupeSlots
      proposedSet = IntSet.fromList normalizedProposed
   in normalizedProposed <> filter (`IntSet.notMember` proposedSet) rawSlots

dedupeSlots :: [Int] -> [Int]
dedupeSlots slots =
  reverse
    ( snd
        ( foldl'
            ( \(seen, kept) slot ->
                if IntSet.member slot seen
                  then (seen, kept)
                  else (IntSet.insert slot seen, slot : kept)
            )
            (IntSet.empty, [])
            slots
        )
    )

parallelFaceKey ::
  ParallelTagFingerprint ->
  ClassId ->
  Int ->
  Int ->
  ClassId ->
  ClassId ->
  ParallelFaceKey
parallelFaceKey tagFingerprint rootClass leftSlot rightSlot leftChild rightChild =
  ParallelFaceKey
    { pfkRootClass = rootClass,
      pfkTagFingerprint = tagFingerprint,
      pfkSlots = orderedPair leftSlot rightSlot,
      pfkChildren = orderedPair leftChild rightChild
    }

parallelEvidenceIdentity :: ParallelEvidence -> (ParallelFaceKey, Int, Int, ClassId, ClassId)
parallelEvidenceIdentity parallelEvidence =
  ( peFaceKey parallelEvidence,
    peLeftSlot parallelEvidence,
    peRightSlot parallelEvidence,
    peLeftChild parallelEvidence,
    peRightChild parallelEvidence
  )

tagFingerprintOf :: ParallelShapeAlgebra f -> ConstructorTag f -> ParallelTagFingerprint
tagFingerprintOf =
  psaTagFingerprint

canonicalizationFor :: Ord tag => CanonicalizationRegistry tag -> tag -> BlockCanonicalization
canonicalizationFor (CanonicalizationRegistry registryMap) tag =
  Map.findWithDefault PreserveBlockOrder tag registryMap

validateParallelPatternShape ::
  HasConstructorTag f =>
  ParallelShapeAlgebra f ->
  f (Pattern f) ->
  [ParallelShapeValidationError (ConstructorTag f)]
validateParallelPatternShape parallelShape node =
  let tag = constructorTag node
      arity = length (toList node)
      declaredBlocks = registryBlocksFor (psaParallelBlockRegistry parallelShape) tag arity
      canonicalization = canonicalizationFor (psaCanonicalizationRegistry parallelShape) tag
      canonicalizationErrors =
        declaredBlocks
          & foldMap
            ( \slotSet ->
                validateCanonicalizationStrategy canonicalization (IntSet.toAscList slotSet)
            )
   in canonicalizationErrors

validateParallelClassShape ::
  HasConstructorTag f =>
  ParallelShapeAlgebra f ->
  f ClassId ->
  [ParallelShapeValidationError (ConstructorTag f)]
validateParallelClassShape parallelShape node =
  let tag = constructorTag node
      arity = length (toList node)
      declaredBlocks = registryBlocksFor (psaParallelBlockRegistry parallelShape) tag arity
      canonicalization = canonicalizationFor (psaCanonicalizationRegistry parallelShape) tag
      canonicalizationErrors =
        declaredBlocks
          & foldMap
            ( \slotSet ->
                validateCanonicalizationStrategy canonicalization (IntSet.toAscList slotSet)
            )
   in canonicalizationErrors

validateCanonicalizationStrategy :: BlockCanonicalization -> [Int] -> [ParallelShapeValidationError tag]
validateCanonicalizationStrategy canonicalization rawSlots =
  case canonicalization of
    PreserveBlockOrder ->
      []
    SortBlockAscending ->
      []
    ExplicitBlockOrder proposedSlots ->
      validateCanonicalization rawSlots proposedSlots

validateCanonicalization :: [Int] -> [Int] -> [ParallelShapeValidationError tag]
validateCanonicalization rawSlots proposedSlots =
  let rawSet = IntSet.fromList rawSlots
      proposedSet = IntSet.fromList proposedSlots
   in membershipErrors rawSet proposedSet rawSlots proposedSlots
        <> cardinalityErrors rawSlots proposedSlots

membershipErrors :: IntSet -> IntSet -> [Int] -> [Int] -> [ParallelShapeValidationError tag]
membershipErrors rawSet proposedSet rawSlots proposedSlots =
  if rawSet == proposedSet
    then []
    else [CanonicalizationChangesMembership rawSlots proposedSlots]

cardinalityErrors :: [Int] -> [Int] -> [ParallelShapeValidationError tag]
cardinalityErrors rawSlots proposedSlots =
  if length rawSlots == length proposedSlots
    then []
    else [CanonicalizationChangesCardinality rawSlots proposedSlots]

blockWithinArity :: Int -> IntSet -> Bool
blockWithinArity arity slotSet =
  IntSet.toAscList slotSet
    & all (\slot -> slot >= 0 && slot < arity)

validateRegistryDeclaration ::
  tag ->
  Int ->
  [IntSet] ->
  [ParallelBlockRegistryValidationError tag]
validateRegistryDeclaration tag arity slotSets =
  foldMap (validateRegistryBlockRange tag arity) slotSets
    <> validateRegistryBlockDisjointness tag arity (filter (blockWithinArity arity) slotSets)

validateRegistryBlockRange ::
  tag ->
  Int ->
  IntSet ->
  [ParallelBlockRegistryValidationError tag]
validateRegistryBlockRange tag arity slotSet =
  IntSet.toAscList slotSet
    & foldMap
      ( \slot ->
          if slot >= 0 && slot < arity
            then []
            else [RegistryBlockSlotOutOfRange tag arity slot]
      )

validateRegistryBlockDisjointness ::
  tag ->
  Int ->
  [IntSet] ->
  [ParallelBlockRegistryValidationError tag]
validateRegistryBlockDisjointness tag arity slotSets =
  slotSets
    & pairwise
    & foldMap
      ( \(leftBlock, rightBlock) ->
          if IntSet.null (IntSet.intersection leftBlock rightBlock)
            then []
            else [RegistryBlocksOverlap tag arity leftBlock rightBlock]
      )

registryBlocksFor :: Ord tag => ParallelBlockRegistry tag -> tag -> Int -> [IntSet]
registryBlocksFor (ParallelBlockRegistry registryMap) tag arity =
  Map.findWithDefault Map.empty tag registryMap
    & Map.findWithDefault [] arity
