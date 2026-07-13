{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Pure.Relational.Direct
  ( DirectPatternShape (..),
    classifyCompiledPatternQuery,
    directPatternMatches,
    directPatternDeltaMatches,
  )
where

import Data.Foldable (toList)
import Data.Foldable qualified as Foldable
import Data.Functor (void)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core
  ( Language,
    Pattern (..),
    PatternVar,
    Substitution,
    emptySubstitution,
    extendSubst,
  )
import Moonlight.EGraph.Pure.Query.RootFilter
  ( RootClassFilter,
    canonicalRootKeys,
    rootClassAllowed,
  )
import Moonlight.EGraph.Pure.Relational.Source (structuralRowsFromBucket)
import Moonlight.EGraph.Pure.Structural.Store
  ( structuralParentKeysOf,
    structuralRowBucketForTag,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    EGraph,
    canonicalizeClassId,
    classIdKey,
    eGraphStore,
  )
import Data.List.NonEmpty (NonEmpty (..))
import Moonlight.Rewrite.Algebra
  ( CompiledPatternQuery,
    cpqQuery,
    patternQueryPatterns,
  )

data DirectPatternShape f
  = DirectSingleAtomTree !(Pattern f)
  | DirectHierarchicalTree !(Pattern f)
  | DirectRelationalJoin

deriving stock instance Eq (Pattern f) => Eq (DirectPatternShape f)

deriving stock instance Ord (Pattern f) => Ord (DirectPatternShape f)

deriving stock instance Show (Pattern f) => Show (DirectPatternShape f)

classifyCompiledPatternQuery :: Foldable f => CompiledPatternQuery guard f -> DirectPatternShape f
classifyCompiledPatternQuery compiledQuery =
  case patternQueryPatterns (cpqQuery compiledQuery) of
    patternValue :| [] ->
      classifySinglePattern patternValue
    _ ->
      DirectRelationalJoin
{-# INLINE classifyCompiledPatternQuery #-}

classifySinglePattern :: Foldable f => Pattern f -> DirectPatternShape f
classifySinglePattern patternValue =
  case patternValue of
    PatternVar _ ->
      DirectRelationalJoin
    PatternNode patternNode
      | all patternChildIsVariable patternNode ->
          DirectSingleAtomTree patternValue
      | otherwise ->
          DirectHierarchicalTree patternValue
{-# INLINE classifySinglePattern #-}

patternChildIsVariable :: Pattern f -> Bool
patternChildIsVariable =
  \case
    PatternVar _ ->
      True
    PatternNode _ ->
      False
{-# INLINE patternChildIsVariable #-}

data BucketPattern
  = BucketVar !PatternVar
  | BucketNode !(IntMap (Set [Int])) ![BucketPattern]

bucketPattern :: Language f => EGraph f a -> Pattern f -> BucketPattern
bucketPattern graph =
  \case
    PatternVar patternVar ->
      BucketVar patternVar
    PatternNode patternNode ->
      BucketNode
        (structuralRowBucketForTag (void patternNode) (eGraphStore graph))
        (fmap (bucketPattern graph) (toList patternNode))

directPatternMatches ::
  Language f =>
  RootClassFilter ->
  EGraph f a ->
  DirectPatternShape f ->
  [(ClassId, Substitution)]
directPatternMatches rootClassFilter graph shape =
  case directPatternShapePattern shape of
    Just patternValue ->
      case bucketPattern graph patternValue of
        BucketNode rootBucket childPlans ->
          directPatternMatchesFromRows
            (const True)
            IntSet.empty
            rootClassFilter
            graph
            childPlans
            (structuralRowsFromBucket rootBucket)
        BucketVar _ ->
          []
    Nothing ->
      []
{-# INLINE directPatternMatches #-}

directPatternDeltaMatches ::
  Language f =>
  RootClassFilter ->
  IntSet ->
  EGraph f a ->
  DirectPatternShape f ->
  [(ClassId, Substitution)]
directPatternDeltaMatches rootClassFilter dirtyResults graph shape =
  case directPatternShapePattern shape of
    Just patternValue ->
      case bucketPattern graph patternValue of
        BucketNode rootBucket childPlans ->
          let dirtyKeys =
                dirtyStructuralRootKeys graph dirtyResults
              rootKeys =
                directDeltaRootKeys graph (patternNodeDepth patternValue) dirtyKeys
           in directPatternMatchesFromRows
                id
                dirtyKeys
                rootClassFilter
                graph
                childPlans
                (structuralRowsFromBucket (IntMap.restrictKeys rootBucket rootKeys))
        BucketVar _ ->
          []
    Nothing ->
      []
{-# INLINE directPatternDeltaMatches #-}

directPatternShapePattern :: DirectPatternShape f -> Maybe (Pattern f)
directPatternShapePattern =
  \case
    DirectSingleAtomTree patternValue ->
      Just patternValue
    DirectHierarchicalTree patternValue ->
      Just patternValue
    DirectRelationalJoin ->
      Nothing
{-# INLINE directPatternShapePattern #-}

patternNodeDepth :: Foldable f => Pattern f -> Int
patternNodeDepth =
  \case
    PatternVar _ ->
      0
    PatternNode patternNode ->
      1 + Foldable.foldl' (\depthAcc child -> max depthAcc (patternNodeDepth child)) 0 patternNode

directDeltaRootKeys :: EGraph f a -> Int -> IntSet -> IntSet
directDeltaRootKeys graph patternDepth dirtyKeys =
  go (patternDepth - 1) dirtyKeys dirtyKeys
  where
    go steps frontier acc
      | steps <= 0 || IntSet.null frontier =
          acc
      | otherwise =
          let parentKeys =
                structuralParentKeysOf (eGraphStore graph) frontier
              widened =
                IntSet.union parentKeys (canonicalRootKeys graph parentKeys)
              fresh =
                IntSet.difference widened acc
           in go (steps - 1) fresh (IntSet.union acc fresh)
{-# INLINE directDeltaRootKeys #-}

directPatternMatchesFromRows ::
  Language f =>
  (Bool -> Bool) ->
  IntSet ->
  RootClassFilter ->
  EGraph f a ->
  [BucketPattern] ->
  [(Int, [Int])] ->
  [(ClassId, Substitution)]
directPatternMatchesFromRows keepMatch dirtyKeys rootClassFilter graph childPlans rows =
  Set.toAscList $
    Set.fromList $
      foldMap
        (directPatternMatchFromRow keepMatch dirtyKeys rootClassFilter graph childPlans)
        rows
{-# INLINE directPatternMatchesFromRows #-}

directPatternMatchFromRow ::
  Language f =>
  (Bool -> Bool) ->
  IntSet ->
  RootClassFilter ->
  EGraph f a ->
  [BucketPattern] ->
  (Int, [Int]) ->
  [(ClassId, Substitution)]
directPatternMatchFromRow keepMatch dirtyKeys rootClassFilter graph childPlans (rootKey, childKeys)
  | rootClassAllowed rootClassFilter graph rootClass =
      [ (rootClass, substitutionValue)
        | (substitutionValue, dirtySeen) <-
            matchPatternChildren
              graph
              dirtyKeys
              childPlans
              childKeys
              (emptySubstitution, rowIsDirty graph dirtyKeys rootKey),
          keepMatch dirtySeen
      ]
  | otherwise =
      []
  where
    rootClass =
      canonicalizeClassId graph (ClassId rootKey)
{-# INLINE directPatternMatchFromRow #-}

rowIsDirty :: EGraph f a -> IntSet -> Int -> Bool
rowIsDirty graph dirtyKeys resultKey =
  not (IntSet.null dirtyKeys)
    && IntSet.member
      (classIdKey (canonicalizeClassId graph (ClassId resultKey)))
      dirtyKeys
{-# INLINE rowIsDirty #-}

matchPatternChildren ::
  Language f =>
  EGraph f a ->
  IntSet ->
  [BucketPattern] ->
  [Int] ->
  (Substitution, Bool) ->
  [(Substitution, Bool)]
matchPatternChildren graph dirtyKeys childPlans childKeys stateValue =
  case zipSameLength childPlans childKeys of
    Just childPlanPairs ->
      Foldable.foldl'
        (\states childPlanPair -> states >>= uncurry (matchPatternAtKey graph dirtyKeys) childPlanPair)
        [stateValue]
        childPlanPairs
    Nothing ->
      []
{-# INLINE matchPatternChildren #-}

matchPatternAtKey ::
  Language f =>
  EGraph f a ->
  IntSet ->
  BucketPattern ->
  Int ->
  (Substitution, Bool) ->
  [(Substitution, Bool)]
matchPatternAtKey graph dirtyKeys bucketPlan classKey (substitutionValue, dirtySeen) =
  case bucketPlan of
    BucketVar patternVar ->
      maybe
        []
        (\extended -> [(extended, dirtySeen)])
        (extendSubst patternVar (canonicalizeClassId graph (ClassId classKey)) substitutionValue)
    BucketNode bucket childPlans ->
      foldMap
        ( \(resultKey, innerChildKeys) ->
            matchPatternChildren
              graph
              dirtyKeys
              childPlans
              innerChildKeys
              (substitutionValue, dirtySeen || rowIsDirty graph dirtyKeys resultKey)
        )
        (structuralRowsFromBucket (IntMap.restrictKeys bucket (rawAndCanonicalKeys graph classKey)))
{-# INLINE matchPatternAtKey #-}

rawAndCanonicalKeys :: EGraph f a -> Int -> IntSet
rawAndCanonicalKeys graph classKey =
  IntSet.fromList
    [ classKey,
      classIdKey (canonicalizeClassId graph (ClassId classKey))
    ]
{-# INLINE rawAndCanonicalKeys #-}

zipSameLength :: [a] -> [b] -> Maybe [(a, b)]
zipSameLength leftValues rightValues =
  if length leftValues == length rightValues
    then Just (zip leftValues rightValues)
    else Nothing
{-# INLINE zipSameLength #-}

dirtyStructuralRootKeys :: EGraph f a -> IntSet -> IntSet
dirtyStructuralRootKeys graph dirtyResults =
  dirtyResults <> canonicalRootKeys graph dirtyResults
{-# INLINE dirtyStructuralRootKeys #-}
