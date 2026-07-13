{-# LANGUAGE TypeFamilies #-}

module Moonlight.Cosheaf.Test.Homology.Expect
  ( expectRight,
    shouldHaveHomologyGroup,
    shouldHaveCellCounts,
  )
where

import Data.Foldable (traverse_)
import Moonlight.Cosheaf.Chain
  ( PreparedFiniteCosheafChain,
    cosheafChainCellsAtDegree,
  )
import Moonlight.Homology
  ( HomologicalDegree (..),
    HomologyGroup (..),
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
  )

expectRight ::
  Show failure =>
  Either failure value ->
  IO value
expectRight result =
  case result of
    Right value ->
      pure value
    Left failure ->
      assertFailure ("expected Right, got Left: " <> show failure)

shouldHaveHomologyGroup ::
  [HomologyGroup Integer] ->
  Int ->
  Int ->
  [Integer] ->
  Assertion
shouldHaveHomologyGroup groups degreeInt expectedRank expectedTorsion =
  case homologyGroupAt degreeInt groups of
    Nothing ->
      assertFailure ("missing homology group at degree " <> show degreeInt)
    Just groupValue -> do
      assertEqual ("free rank at degree " <> show degreeInt) expectedRank (freeRank groupValue)
      assertEqual ("torsion at degree " <> show degreeInt) expectedTorsion (torsionInvariants groupValue)

shouldHaveCellCounts ::
  PreparedFiniteCosheafChain site value ->
  [(Int, Int)] ->
  Assertion
shouldHaveCellCounts plan expectedCounts =
  traverse_
    (\(degreeInt, expectedCount) ->
      assertEqual
        ("cell count at degree " <> show degreeInt)
        expectedCount
        (length (cosheafChainCellsAtDegree (HomologicalDegree degreeInt) plan))
    )
    expectedCounts

homologyGroupAt :: Int -> [HomologyGroup Integer] -> Maybe (HomologyGroup Integer)
homologyGroupAt degreeInt groups
  | degreeInt < 0 =
      Nothing
  | otherwise =
      case drop degreeInt groups of
        groupValue : _ ->
          Just groupValue
        [] ->
          Nothing
