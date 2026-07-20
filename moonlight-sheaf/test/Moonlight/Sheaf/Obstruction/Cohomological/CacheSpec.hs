module Moonlight.Sheaf.Obstruction.Cohomological.CacheSpec
  ( tests,
  )
where

import Data.Foldable (traverse_)
import Data.IntSet qualified as IntSet
import Moonlight.Sheaf.Obstruction.Cohomological.Core.Cache
  ( CohomologicalCache,
    ObstructionCacheKey (..),
    emptyCohomologicalCache,
    insertCachedObstructionForDependencies,
    invalidateCachedObstructions,
    lookupCachedObstruction,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Types
  ( RegionScale (CoarseRegion),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

data CachePurpose = CachePurpose Int
  deriving stock (Eq, Ord, Show)

tests :: TestTree
tests =
  testGroup
    "cohomological cache"
    [ testCase "entry dependencies and reverse invalidation agree" testDependencyInverse,
      testCase "replacement removes every old reverse edge" testReplacementRemovesOldEdges,
      testCase "invalidation removes every dependent entry" testInvalidationRemovesEveryDependent,
      testCase "invalidation preserves unrelated entries" testInvalidationPreservesUnrelated
    ]

cacheKey :: Int -> ObstructionCacheKey CachePurpose
cacheKey keyValue =
  ObstructionCacheKey
    { ockQueryFingerprint = keyValue,
      ockRegionFingerprint = keyValue * 17,
      ockScale = CoarseRegion,
      ockPurpose = CachePurpose keyValue,
      ockEnvironmentFingerprint = Nothing
    }

insertEntry ::
  IntSet.IntSet ->
  Int ->
  String ->
  CohomologicalCache CachePurpose String ->
  CohomologicalCache CachePurpose String
insertEntry dependencies keyValue =
  insertCachedObstructionForDependencies dependencies (cacheKey keyValue)

testDependencyInverse :: Assertion
testDependencyInverse =
  let dependencies = IntSet.fromList [3, 5, 8]
      cache = insertEntry dependencies 1 "dependent" emptyCohomologicalCache
   in traverse_
        ( \dependency ->
            lookupCachedObstruction
              (cacheKey 1)
              (invalidateCachedObstructions (IntSet.singleton dependency) cache)
              @?= Nothing
        )
        (IntSet.toAscList dependencies)

testReplacementRemovesOldEdges :: Assertion
testReplacementRemovesOldEdges =
  let originalCache =
        insertEntry (IntSet.fromList [1, 2]) 1 "original" emptyCohomologicalCache
      replacedCache =
        insertEntry (IntSet.singleton 9) 1 "replacement" originalCache
      invalidatedAtOldDependency =
        invalidateCachedObstructions (IntSet.singleton 1) replacedCache
      invalidatedAtNewDependency =
        invalidateCachedObstructions (IntSet.singleton 9) replacedCache
   in do
        lookupCachedObstruction (cacheKey 1) invalidatedAtOldDependency
          @?= Just "replacement"
        lookupCachedObstruction (cacheKey 1) invalidatedAtNewDependency
          @?= Nothing

testInvalidationRemovesEveryDependent :: Assertion
testInvalidationRemovesEveryDependent =
  let cache =
        insertEntry (IntSet.fromList [4, 7]) 1 "first" $
          insertEntry (IntSet.singleton 4) 2 "second" $
            insertEntry (IntSet.singleton 7) 3 "third" emptyCohomologicalCache
      invalidated =
        invalidateCachedObstructions (IntSet.singleton 4) cache
   in do
        lookupCachedObstruction (cacheKey 1) invalidated @?= Nothing
        lookupCachedObstruction (cacheKey 2) invalidated @?= Nothing

testInvalidationPreservesUnrelated :: Assertion
testInvalidationPreservesUnrelated =
  let cache =
        insertEntry (IntSet.singleton 4) 1 "dependent" $
          insertEntry (IntSet.singleton 7) 2 "unrelated" emptyCohomologicalCache
      invalidated =
        invalidateCachedObstructions (IntSet.singleton 4) cache
   in lookupCachedObstruction (cacheKey 2) invalidated @?= Just "unrelated"
