{-# LANGUAGE DerivingStrategies #-}

module RowsCacheSpec
  ( tests,
  )
where

import Control.Monad.Trans.State.Strict
  ( State,
    StateT,
    modify',
    runState,
    runStateT,
  )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Differential.Context.RowsCache
  ( ContextRowsCache,
    ContextRowsRuntime (..),
    ContextRowsSourceSelectionError (..),
    cachedContextSet,
    contextRowsKey,
    crkBaseRevision,
    crkContext,
    crkOverlayEpoch,
    crkPlanFingerprint,
    crcCurrentBytes,
    crcEntries,
    crcPinned,
    dropContextRowsFor,
    emptyContextRowsCache,
    getContextRows,
    insertContextRows,
    withPinnedContext,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    testCase,
  )

data TestRows = TestRows
  { testRowsLabel :: ![String],
    testRowsBytes :: !Int
  }
  deriving stock (Eq, Show)

data CacheEvent
  = Materialized !Int
  | Restricted !Int !Int
  deriving stock (Eq, Show)

type TestCache = ContextRowsCache Int TestRows

type CacheAction a = StateT TestCache (State [CacheEvent]) a

tests :: TestTree
tests =
  testGroup
    "context rows cache laws"
    [ testCase "context rows key preserves authority dimensions" contextRowsKeyAccessors,
      testCase "miss materializes, restriction source derives, hit reuses" hitMissRestrictionReuse,
      testCase "revision and fingerprint isolate cache keys" revisionFingerprintIsolation,
      testCase "stale authority keys are not offered as restriction sources" staleAuthorityKeysDoNotSeedRestrictionSources,
      testCase "self restriction source is rejected" selfRestrictionSourceRejected,
      testCase "alternating off-domain restriction source is rejected without recursion" alternatingOffDomainSourceRejected,
      testCase "LRU eviction respects byte budget" lruEvictionRespectsBudget,
      testCase "pin survives over-budget insertion and post-unpin eviction closes budget" pinSurvivalAndPostUnpinEviction,
      testCase "dirty context invalidation drops only selected contexts" dirtyContextInvalidation
    ]

contextRowsKeyAccessors :: Assertion
contextRowsKeyAccessors = do
  let keyValue = contextRowsKey 11 22 33 "ctx"
  assertEqual "base revision" 11 (crkBaseRevision keyValue)
  assertEqual "overlay epoch" 22 (crkOverlayEpoch keyValue)
  assertEqual "plan fingerprint" 33 (crkPlanFingerprint keyValue)
  assertEqual "context" "ctx" (crkContext keyValue)

hitMissRestrictionReuse :: Assertion
hitMissRestrictionReuse = do
  let ((rows, cache), events) =
        runCache
          ( do
              _ <- getContextRows (testRuntime 0 0 0) 1
              derived <- getContextRows (testRuntime 0 0 0) 2
              cached <- getContextRows (testRuntime 0 0 0) 2
              pure (derived, cached)
          )
          (emptyContextRowsCache 100)
  assertEqual
    "derived rows are reused on the second read"
    ( Right (TestRows ["root-1", "restrict-1-2"] 8),
      Right (TestRows ["root-1", "restrict-1-2"] 8)
    )
    rows
  assertEqual
    "only the miss and restriction derivation invoke runtime effects"
    [Materialized 1, Restricted 1 2]
    events
  assertEqual
    "both source and restricted target are cached"
    (Set.fromList [1, 2])
    (cachedContextSet cache)

revisionFingerprintIsolation :: Assertion
revisionFingerprintIsolation = do
  let (((), cache), events) =
        runCache
          ( do
              _ <- getContextRows (testRuntime 1 0 10) 7
              _ <- getContextRows (testRuntime 2 0 10) 7
              _ <- getContextRows (testRuntime 2 0 11) 7
              pure ()
          )
          (emptyContextRowsCache 100)
  assertEqual
    "revision/fingerprint changes force distinct materializations"
    [Materialized 7, Materialized 7, Materialized 7]
    events
  assertEqual
    "same context can have distinct authority keys"
    ( Set.fromList
        [ contextRowsKey 1 0 10 7,
          contextRowsKey 2 0 10 7,
          contextRowsKey 2 0 11 7
        ]
    )
    (Map.keysSet (crcEntries cache))

staleAuthorityKeysDoNotSeedRestrictionSources :: Assertion
staleAuthorityKeysDoNotSeedRestrictionSources = do
  let ((rows, cache), events) =
        runCache
          ( do
              insertContextRows (fixedRowsRuntime 0 0 0 (TestRows ["stale-one"] 5)) 1 (TestRows ["stale-one"] 5)
              getContextRows (testRuntime 1 0 0) 2
          )
          (emptyContextRowsCache 100)
  assertEqual
    "stale source is not used for restriction derivation"
    (Right (TestRows ["root-2"] 5))
    rows
  assertEqual
    "only the requested current key materializes"
    [Materialized 2]
    events
  assertEqual
    "stale and current keys coexist without aliasing"
    (Set.fromList [contextRowsKey 0 0 0 1, contextRowsKey 1 0 0 2])
    (Map.keysSet (crcEntries cache))

selfRestrictionSourceRejected :: Assertion
selfRestrictionSourceRejected = do
  let ((result, cache), events) =
        runCache
          (getContextRows (sourceSelectingRuntime (\targetContext -> targetContext)) 1)
          (emptyContextRowsCache 100)
  assertEqual
    "target cannot masquerade as its own cached restriction source"
    (Left (ContextRowsRestrictionSourceIsTarget 1))
    result
  assertEqual "rejected self-selection performs no materialization" [] events
  assertEqual "rejected self-selection inserts no cache entry" Map.empty (crcEntries cache)

alternatingOffDomainSourceRejected :: Assertion
alternatingOffDomainSourceRejected = do
  let alternatingSource :: Int -> Int
      alternatingSource targetContext =
        if targetContext == 1 then 2 else 1
      ((result, cache), events) =
        runCache
          (getContextRows (sourceSelectingRuntime alternatingSource) 1)
          (emptyContextRowsCache 100)
  assertEqual
    "the first off-domain edge obstructs an alternating source cycle"
    (Left (ContextRowsRestrictionSourceOutsideCachedDomain 2 1))
    result
  assertEqual "off-domain selection performs no recursive effects" [] events
  assertEqual "off-domain selection inserts no cache entry" Map.empty (crcEntries cache)

lruEvictionRespectsBudget :: Assertion
lruEvictionRespectsBudget = do
  let (((), cache), _events) =
        runCache
          ( do
              insertContextRows (fixedRowsRuntime 0 0 0 (TestRows ["one"] 6)) 1 (TestRows ["one"] 6)
              insertContextRows (fixedRowsRuntime 0 0 0 (TestRows ["two"] 6)) 2 (TestRows ["two"] 6)
          )
          (emptyContextRowsCache 10)
  assertEqual
    "oldest unpinned row is evicted"
    (Set.singleton (contextRowsKey 0 0 0 2))
    (Map.keysSet (crcEntries cache))
  assertEqual
    "current bytes account for eviction"
    6
    (crcCurrentBytes cache)

pinSurvivalAndPostUnpinEviction :: Assertion
pinSurvivalAndPostUnpinEviction = do
  let oversizedRows = TestRows ["oversized"] 12
      (((), cache), _events) =
        runCache
          ( withPinnedContext (fixedRowsRuntime 0 0 0 oversizedRows) 1 $
              insertContextRows (fixedRowsRuntime 0 0 0 oversizedRows) 1 oversizedRows
          )
          (emptyContextRowsCache 10)
  assertEqual
    "pin count is released"
    Set.empty
    (crcPinned cache)
  assertEqual
    "over-budget row becomes evictable immediately after unpin"
    Map.empty
    (crcEntries cache)
  assertEqual
    "post-unpin eviction closes the byte budget"
    0
    (crcCurrentBytes cache)

dirtyContextInvalidation :: Assertion
dirtyContextInvalidation = do
  let cache0 :: TestCache
      cache0 = emptyContextRowsCache 100
      (((), cache1), _events) =
        runCache
          ( do
              insertContextRows (fixedRowsRuntime 0 0 0 (TestRows ["one"] 5)) 1 (TestRows ["one"] 5)
              insertContextRows (fixedRowsRuntime 0 0 0 (TestRows ["two"] 7)) 2 (TestRows ["two"] 7)
          )
          cache0
      (refusedKeys, cache2) = dropContextRowsFor (Set.singleton 1) cache1
  assertEqual
    "no pinned entry refuses an unpinned invalidation"
    Set.empty
    refusedKeys
  assertEqual
    "dirty context is removed without touching unrelated rows"
    (Set.singleton (contextRowsKey 0 0 0 2))
    (Map.keysSet (crcEntries cache2))
  assertEqual
    "current bytes subtract invalidated entries"
    7
    (crcCurrentBytes cache2)

runCache :: CacheAction a -> TestCache -> ((a, TestCache), [CacheEvent])
runCache action cache =
  runState (runStateT action cache) []

testRuntime :: Int -> Int -> Int -> ContextRowsRuntime (State [CacheEvent]) Int TestRows
testRuntime baseRevision overlayEpoch planFingerprint =
  ContextRowsRuntime
    { crrKeyFor = contextRowsKey baseRevision overlayEpoch planFingerprint,
      crrChooseRestrictionSource = \availableContexts targetContext ->
        pure (Set.lookupMax (Set.filter (< targetContext) availableContexts)),
      crrMaterializeRootRows = \contextValue -> do
        modify' (<> [Materialized contextValue])
        pure (TestRows ["root-" <> show contextValue] 5),
      crrDeriveByRestriction = \sourceContext targetContext sourceRows -> do
        modify' (<> [Restricted sourceContext targetContext])
        pure
          sourceRows
            { testRowsLabel =
                testRowsLabel sourceRows
                  <> ["restrict-" <> show sourceContext <> "-" <> show targetContext],
              testRowsBytes = testRowsBytes sourceRows + 3
            },
      crrRowsBytes = fromIntegral . testRowsBytes
    }

fixedRowsRuntime :: Int -> Int -> Int -> TestRows -> ContextRowsRuntime (State [CacheEvent]) Int TestRows
fixedRowsRuntime baseRevision overlayEpoch planFingerprint rows =
  ContextRowsRuntime
    { crrKeyFor = contextRowsKey baseRevision overlayEpoch planFingerprint,
      crrChooseRestrictionSource = \_availableContexts _targetContext -> pure Nothing,
      crrMaterializeRootRows = \contextValue -> do
        modify' (<> [Materialized contextValue])
        pure rows,
      crrDeriveByRestriction = \sourceContext targetContext _sourceRows -> do
        modify' (<> [Restricted sourceContext targetContext])
        pure rows,
      crrRowsBytes = fromIntegral . testRowsBytes
    }

sourceSelectingRuntime :: (Int -> Int) -> ContextRowsRuntime (State [CacheEvent]) Int TestRows
sourceSelectingRuntime selectSource =
  (testRuntime 0 0 0)
    { crrChooseRestrictionSource =
        \_availableContexts targetContext ->
          pure (Just (selectSource targetContext))
    }
