{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wmissing-local-signatures #-}

module Moonlight.Pale.Test.Laws.RestrictionSpec
  ( tests,
  )
where

import Moonlight.Pale.Test.Laws.Restriction
  ( RestrictionLawSeed,
    restrictionLawSeed,
    unfoldRestrictionLaws,
    withComparablePairs,
    withComparableTriples,
  )
import Moonlight.Pale.Test.LawSuite (lawGroup, renderLawSuite)
import Test.Tasty (TestTree, testGroup)

data ChainCell
  = ChainBottom
  | ChainMiddle
  | ChainTop
  deriving stock (Eq, Show)

newtype Section = Section ChainCell
  deriving stock (Eq, Show)

tests :: TestTree
tests =
  testGroup
    "Moonlight.Pale.Test.Laws.Restriction"
    [ renderLawSuite (lawGroup "chain restriction suite" (unfoldRestrictionLaws chainRestrictionSeed))
    ]

chainRestrictionSeed :: RestrictionLawSeed ChainCell Section
chainRestrictionSeed =
  withComparableTriples chainComparableTriples $
    withComparablePairs chainComparablePairs $
      restrictionLawSeed "chain" restrictSection chainLeq chainSections

chainSections :: [(ChainCell, Section)]
chainSections = fmap sectionAt chainCells

sectionAt :: ChainCell -> (ChainCell, Section)
sectionAt cell = (cell, Section cell)

chainCells :: [ChainCell]
chainCells = [ChainBottom, ChainMiddle, ChainTop]

chainComparablePairs :: [(ChainCell, ChainCell)]
chainComparablePairs = filter (uncurry chainLeq) ((,) <$> chainCells <*> chainCells)

chainComparableTriples :: [(ChainCell, ChainCell, ChainCell)]
chainComparableTriples = filter chainLeqTriple ((,,) <$> chainCells <*> chainCells <*> chainCells)

chainLeqTriple :: (ChainCell, ChainCell, ChainCell) -> Bool
chainLeqTriple (x, y, z) = chainLeq x y && chainLeq y z

restrictSection :: ChainCell -> ChainCell -> Section -> Section
restrictSection _ target (Section cell) = Section (chainMeet cell target)

chainMeet :: ChainCell -> ChainCell -> ChainCell
chainMeet x y
  | chainLeq x y = x
  | otherwise = y

chainLeq :: ChainCell -> ChainCell -> Bool
chainLeq x y = chainRank x <= chainRank y

chainRank :: ChainCell -> Int
chainRank cell =
  case cell of
    ChainBottom -> 0
    ChainMiddle -> 1
    ChainTop -> 2
