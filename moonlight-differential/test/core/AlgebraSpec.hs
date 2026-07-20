module AlgebraSpec
  ( tests,
  )
where

import Moonlight.Differential.Algebra.FiniteMap qualified as FiniteMap
import Moonlight.Core (AdditiveGroup (..), AdditiveMonoid (..))
import Moonlight.Differential.Algebra.ZSet qualified as ZSet
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertEqual,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "algebra laws"
    [ testCase "FiniteMap owns zero-pruned Abelian group support" finiteMapOwnsCanonicalSupport,
      testCase "ZSet and IndexedZSet inherit nested group cancellation" zsetNestedCancellation
    ]

finiteMapOwnsCanonicalSupport :: IO ()
finiteMapOwnsCanonicalSupport = do
  let values =
        FiniteMap.fromList
          [ ("a", 1 :: Int),
            ("a", 4),
            ("b", 7),
            ("b", -7),
            ("c", 0)
          ]
  assertEqual
    "finite support map consolidates by key and removes zero support"
    [("a", 5)]
    (FiniteMap.toAscList values)
  assertEqual "left identity" values (zero <> values)
  assertEqual "right identity" values (values <> zero)
  assertEqual "inverse cancels support" zero (values <> neg values)

zsetNestedCancellation :: IO ()
zsetNestedCancellation = do
  let rows =
        ZSet.indexedZSetInsert "outer" 'x' (3 :: Int) $
          ZSet.indexedZSetInsert "outer" 'x' (-3) $
            ZSet.indexedZSetInsert "kept" 'y' 5 ZSet.indexedZSetEmpty
  assertEqual
    "IndexedZSet inherits cancellation from FiniteMap k (ZSet v w)"
    [("kept", ZSet.zsetFromList [('y', 5 :: Int)])]
    (ZSet.indexedZSetToAscList rows)
