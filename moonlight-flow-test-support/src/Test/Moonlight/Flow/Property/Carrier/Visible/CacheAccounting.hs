module Test.Moonlight.Flow.Property.Carrier.Visible.CacheAccounting
  ( tests,
  )
where

import Moonlight.Core
  ( mkLiveEpoch,
    mkQuotientEpoch,
  )
import Moonlight.Flow.Carrier.View.Cache
  ( VisibleCacheAccountingError (..),
    VisibleContextKey (..),
    VisibleSectionCache (..),
    dropLazyVisibleContext,
    dropVisibleContext,
    emptyVisibleSectionCache,
    insertPinnedVisibleContext,
    insertVisibleContext,
    lookupPinnedVisibleContext,
    lookupVisibleContext,
    validateVisibleCacheAccounting,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    testCase,
    (@?=),
  )

tests :: TestTree
tests =
  testGroup
    "visible section cache accounting"
    [ testCase "tracks bytes exactly across insert and replace" insertReplaceAssertion,
      testCase "tracks bytes exactly across touch" touchAssertion,
      testCase "tracks pinned contexts as maintained entries" pinnedAssertion,
      testCase "lazy context drop preserves pinned demand" lazyDropPreservesPinnedAssertion,
      testCase "tracks bytes exactly across context drop" contextDropAssertion,
      testCase "detects corrupted accounting" corruptedAccountingAssertion
    ]

insertReplaceAssertion :: Assertion
insertReplaceAssertion = do
  let cache0 =
        emptyStringCache 1024
      cache1 =
        insertVisibleContext length keyA "aaaa" cache0
      cache2 =
        insertVisibleContext length keyA "aa" cache1
  vscCurrentBytes cache2 @?= 2
  validateVisibleCacheAccounting cache2 @?= Right ()

touchAssertion :: Assertion
touchAssertion = do
  let cache0 =
        insertVisibleContext length keyA "aaaa" (emptyVisibleSectionCache 1024)
      (cache1, value) =
        lookupVisibleContext keyA cache0
  value @?= Just "aaaa"
  vscCurrentBytes cache1 @?= 4
  validateVisibleCacheAccounting cache1 @?= Right ()

pinnedAssertion :: Assertion
pinnedAssertion = do
  let cache0 =
        insertVisibleContext length keyA "lazy" (emptyVisibleSectionCache 1024)
      cache1 =
        insertPinnedVisibleContext length 1 "pinned" cache0
      (_cacheTouched, pinnedValue) =
        lookupPinnedVisibleContext 1 cache1
      (_cacheLazyTouched, lazyValue) =
        lookupVisibleContext keyA cache1
  pinnedValue @?= Just "pinned"
  lazyValue @?= Nothing
  vscCurrentBytes cache1 @?= 6
  validateVisibleCacheAccounting cache1 @?= Right ()

lazyDropPreservesPinnedAssertion :: Assertion
lazyDropPreservesPinnedAssertion = do
  let cache0 =
        emptyStringCache 1024
      cache1 =
        insertPinnedVisibleContext length 1 "aaaa" cache0
      cache2 =
        insertVisibleContext length keyB "bb" cache1
      cache3 =
        dropLazyVisibleContext 1 cache2
      (_cacheTouched, pinnedValue) =
        lookupPinnedVisibleContext 1 cache3
  pinnedValue @?= Just "aaaa"
  vscCurrentBytes cache3 @?= 6
  validateVisibleCacheAccounting cache3 @?= Right ()

contextDropAssertion :: Assertion
contextDropAssertion = do
  let cache0 =
        emptyStringCache 1024
      cache1 =
        insertVisibleContext length keyA "aaaa" cache0
      cache2 =
        insertVisibleContext length keyB "bbbbbb" cache1
      cache3 =
        dropVisibleContext 1 cache2
  vscCurrentBytes cache3 @?= 6
  validateVisibleCacheAccounting cache3 @?= Right ()

corruptedAccountingAssertion :: Assertion
corruptedAccountingAssertion = do
  let cache0 =
        insertVisibleContext length keyA "aaaa" (emptyVisibleSectionCache 1024)
      corrupted =
        cache0 {vscCurrentBytes = 9}
  validateVisibleCacheAccounting corrupted
    @?= Left (VisibleCacheBytesMismatch 4 9)

keyA :: VisibleContextKey Int
keyA =
  VisibleContextKey
    { vckQuotientEpoch = mkQuotientEpoch 1,
      vckLiveEpoch = mkLiveEpoch 1,
      vckContext = 1
    }

keyB :: VisibleContextKey Int
keyB =
  VisibleContextKey
    { vckQuotientEpoch = mkQuotientEpoch 1,
      vckLiveEpoch = mkLiveEpoch 1,
      vckContext = 2
    }

emptyStringCache :: Int -> VisibleSectionCache Int String
emptyStringCache =
  emptyVisibleSectionCache
