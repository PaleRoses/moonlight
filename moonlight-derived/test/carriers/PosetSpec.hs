module PosetSpec (tests) where

import Data.List (isInfixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import Moonlight.Core (MoonlightError (..))
import Moonlight.Derived.Pure.Site.Poset

expectPoset :: [FinObjectId] -> [(FinObjectId, FinObjectId)] -> DerivedPoset
expectPoset ns cs = either (error . show) id (mkDerivedPosetFromCovers ns cs)

tests :: TestTree
tests = testGroup "DerivedPoset"
  [ testGroup "mkDerivedPosetFromCovers"
    [ testCase "singleton poset" $ do
        let p = expectPoset [FinObjectId 0] []
        V.length (derivedPosetNodes p) @?= 1

    , testCase "linear chain 0 < 1 < 2" $ do
        let p = expectPoset
                  [FinObjectId 0, FinObjectId 1, FinObjectId 2]
                  [(FinObjectId 0, FinObjectId 1), (FinObjectId 1, FinObjectId 2)]
        V.length (derivedPosetNodes p) @?= 3
        assertBool "0 <= 1" (leq p (FinObjectId 0) (FinObjectId 1))
        assertBool "1 <= 2" (leq p (FinObjectId 1) (FinObjectId 2))
        assertBool "0 <= 2 (transitive)" (leq p (FinObjectId 0) (FinObjectId 2))
        assertBool "not 2 <= 0" (not (leq p (FinObjectId 2) (FinObjectId 0)))
        assertBool "not 1 <= 0" (not (leq p (FinObjectId 1) (FinObjectId 0)))

    , testCase "diamond 0 < {1,2} < 3" $ do
        let p = expectPoset
                  [FinObjectId 0, FinObjectId 1, FinObjectId 2, FinObjectId 3]
                  [ (FinObjectId 0, FinObjectId 1), (FinObjectId 0, FinObjectId 2)
                  , (FinObjectId 1, FinObjectId 3), (FinObjectId 2, FinObjectId 3) ]
        assertBool "0 <= 3 (both paths)" (leq p (FinObjectId 0) (FinObjectId 3))
        assertBool "not 1 <= 2" (not (leq p (FinObjectId 1) (FinObjectId 2)))
        assertBool "not 2 <= 1" (not (leq p (FinObjectId 2) (FinObjectId 1)))

    , testCase "topological order respects covers" $ do
        let p = expectPoset
                  [FinObjectId 0, FinObjectId 1, FinObjectId 2]
                  [(FinObjectId 0, FinObjectId 1), (FinObjectId 1, FinObjectId 2)]
            asc = V.toList (derivedPosetTopoAsc p)
            desc = V.toList (derivedPosetTopoDesc p)
        assertBool "ascending: 0 before 1" (elemIndex' (FinObjectId 0) asc < elemIndex' (FinObjectId 1) asc)
        assertBool "ascending: 1 before 2" (elemIndex' (FinObjectId 1) asc < elemIndex' (FinObjectId 2) asc)
        desc @?= reverse asc

    , testCase "duplicate nodes deduplicated" $ do
        let p = expectPoset
                  [FinObjectId 0, FinObjectId 0, FinObjectId 1, FinObjectId 1]
                  [(FinObjectId 0, FinObjectId 1)]
        V.length (derivedPosetNodes p) @?= 2

    , testCase "duplicate covers canonicalize instead of fabricating a cycle" $ do
        let duplicateCoverPoset =
              expectPoset
                [FinObjectId 0, FinObjectId 1]
                [(FinObjectId 0, FinObjectId 1), (FinObjectId 0, FinObjectId 1)]
            canonicalPoset =
              expectPoset [FinObjectId 0, FinObjectId 1] [(FinObjectId 0, FinObjectId 1)]
        duplicateCoverPoset @?= canonicalPoset

    , testCase "redundant transitive edges and input order canonicalize to one Hasse presentation" $ do
        let canonicalPoset =
              expectPoset
                [FinObjectId 0, FinObjectId 1, FinObjectId 2]
                [(FinObjectId 0, FinObjectId 1), (FinObjectId 1, FinObjectId 2)]
            redundantPermutedPoset =
              expectPoset
                [FinObjectId 2, FinObjectId 0, FinObjectId 1]
                [(FinObjectId 0, FinObjectId 2), (FinObjectId 1, FinObjectId 2), (FinObjectId 0, FinObjectId 1)]
        redundantPermutedPoset @?= canonicalPoset
        derivedPosetCoversUp redundantPermutedPoset @?= derivedPosetCoversUp canonicalPoset

    , testCase "self-loop rejected" $ do
        case mkDerivedPosetFromCovers [FinObjectId 0] [(FinObjectId 0, FinObjectId 0)] of
          Left (InvariantViolation msg) -> assertBool "error should mention self-loop" ("self-loop" `isInfixOf` msg)
          Left other -> assertFailure ("expected InvariantViolation about self-loop, got: " <> show other)
          Right _ -> assertFailure "should reject self-loop"

    , testCase "cycle rejected" $ do
        case mkDerivedPosetFromCovers [FinObjectId 0, FinObjectId 1] [(FinObjectId 0, FinObjectId 1), (FinObjectId 1, FinObjectId 0)] of
          Left (InvariantViolation msg) -> assertBool "error should mention cycle" ("cycle" `isInfixOf` msg)
          Left other -> assertFailure ("expected InvariantViolation about cycle, got: " <> show other)
          Right _ -> assertFailure "should reject cycle"
    ]

  , testGroup "star"
    [ testCase "star of bottom in chain" $ do
        let p = expectPoset
                  [FinObjectId 0, FinObjectId 1, FinObjectId 2]
                  [(FinObjectId 0, FinObjectId 1), (FinObjectId 1, FinObjectId 2)]
        star p (FinObjectId 0) @?= IS.fromList [0, 1, 2]

    , testCase "star of top is singleton" $ do
        let p = expectPoset
                  [FinObjectId 0, FinObjectId 1, FinObjectId 2]
                  [(FinObjectId 0, FinObjectId 1), (FinObjectId 1, FinObjectId 2)]
        star p (FinObjectId 2) @?= IS.fromList [2]

    , testCase "star in diamond" $ do
        let p = expectPoset
                  [FinObjectId 0, FinObjectId 1, FinObjectId 2, FinObjectId 3]
                  [ (FinObjectId 0, FinObjectId 1), (FinObjectId 0, FinObjectId 2)
                  , (FinObjectId 1, FinObjectId 3), (FinObjectId 2, FinObjectId 3) ]
        star p (FinObjectId 0) @?= IS.fromList [0, 1, 2, 3]
        star p (FinObjectId 1) @?= IS.fromList [1, 3]
        star p (FinObjectId 2) @?= IS.fromList [2, 3]
    ]

  , testGroup "leq"
    [ testCase "reflexive" $ do
        let p = expectPoset [FinObjectId 0] []
        assertBool "0 <= 0" (leq p (FinObjectId 0) (FinObjectId 0))

    , testCase "antisymmetric" $ do
        let p = expectPoset
                  [FinObjectId 0, FinObjectId 1]
                  [(FinObjectId 0, FinObjectId 1)]
        assertBool "0 <= 1" (leq p (FinObjectId 0) (FinObjectId 1))
        assertBool "not 1 <= 0" (not (leq p (FinObjectId 1) (FinObjectId 0)))
    ]

  , testGroup "closureOf"
    [ testCase "closure of top in chain is everything" $ do
        let p = expectPoset
                  [FinObjectId 0, FinObjectId 1, FinObjectId 2]
                  [(FinObjectId 0, FinObjectId 1), (FinObjectId 1, FinObjectId 2)]
        closureOf p (IS.singleton 2) @?= IS.fromList [0, 1, 2]

    , testCase "closure of bottom is just bottom" $ do
        let p = expectPoset
                  [FinObjectId 0, FinObjectId 1, FinObjectId 2]
                  [(FinObjectId 0, FinObjectId 1), (FinObjectId 1, FinObjectId 2)]
        closureOf p (IS.singleton 0) @?= IS.fromList [0]
    ]
  ]

elemIndex' :: Eq a => a -> [a] -> Int
elemIndex' x xs = go 0 xs
  where
    go _ [] = error "elemIndex': not found"
    go n (y:ys)
      | x == y    = n
      | otherwise = go (n + 1) ys
