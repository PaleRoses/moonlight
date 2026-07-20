{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wmissing-local-signatures #-}

module Moonlight.Pale.Test.Laws.LatticeSpec
  ( tests,
  )
where

import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Moonlight.Pale.Test.Laws.Lattice
  ( LatticeLawSeed,
    LatticeLawSeedError,
    latticeLawSeed,
    unfoldLatticeLaws,
    withBounded,
  )
import Moonlight.Pale.Test.LawSuite (lawGroup, renderLawSuite)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

data Diamond
  = DiamondBottom
  | DiamondLeft
  | DiamondRight
  | DiamondTop
  deriving stock (Eq, Show)

tests :: TestTree
tests =
  testGroup
    "Moonlight.Pale.Test.Laws.Lattice"
    [ renderLatticeSeed "Bool bounded lattice" boolSeed,
      renderLatticeSeed "diamond bounded lattice" diamondSeed,
      testCase "invalid bounded lattice seed returns typed Left" $
        assertBool "expected bounded seed construction to reject a top outside the universe" $
          isLeft invalidBoundedSeed
    ]

boolSeed :: Either (NonEmpty (LatticeLawSeedError Bool)) (LatticeLawSeed Bool)
boolSeed =
  latticeLawSeed "Bool" (||) (&&) (False :| [True]) >>= withBounded False True

diamondSeed :: Either (NonEmpty (LatticeLawSeedError Diamond)) (LatticeLawSeed Diamond)
diamondSeed =
  latticeLawSeed "diamond" diamondJoin diamondMeet diamondUniverse >>= withBounded DiamondBottom DiamondTop

invalidBoundedSeed :: Either (NonEmpty (LatticeLawSeedError Bool)) (LatticeLawSeed Bool)
invalidBoundedSeed =
  latticeLawSeed "invalid Bool" (||) (&&) (False :| []) >>= withBounded False True

diamondUniverse :: NonEmpty Diamond
diamondUniverse = DiamondBottom :| [DiamondLeft, DiamondRight, DiamondTop]

diamondJoin :: Diamond -> Diamond -> Diamond
diamondJoin x y
  | diamondLeq x y = y
  | diamondLeq y x = x
  | otherwise = DiamondTop

diamondMeet :: Diamond -> Diamond -> Diamond
diamondMeet x y
  | diamondLeq x y = x
  | diamondLeq y x = y
  | otherwise = DiamondBottom

diamondLeq :: Diamond -> Diamond -> Bool
diamondLeq x y = x == y || x == DiamondBottom || y == DiamondTop

renderLatticeSeed :: (Show a, Eq a) => String -> Either (NonEmpty (LatticeLawSeedError a)) (LatticeLawSeed a) -> TestTree
renderLatticeSeed label seedResult =
  case seedResult of
    Left errors ->
      testCase (label <> " seed is valid") $
        assertFailure ("expected valid lattice seed: " <> show errors)
    Right seed ->
      renderLawSuite (lawGroup label (unfoldLatticeLaws seed))
