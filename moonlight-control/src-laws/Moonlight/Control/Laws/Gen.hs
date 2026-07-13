-- | Sized generators and structural shrinkers over the raw 'Program'
-- representation.
--
-- Generation deliberately uses the exact constructors of
-- "Moonlight.Control.Program.Internal", not the smart constructors of the
-- class: the law kit must exercise the full syntax space — including
-- redundant skips, nested spines, and fusible scopes that smart
-- construction would already have reduced.
module Moonlight.Control.Laws.Gen
  ( genProgram,
    shrinkProgram,
    genRepeatCount,
    shrinkRepeatCount,
  )
where

import Numeric.Natural (Natural)
import Test.QuickCheck
  ( Gen,
    chooseInt,
    frequency,
    shrinkIntegral,
    sized,
  )

import Moonlight.Control.Program.Internal
  ( Program (..),
  )

-- | A sized program over the given context and phase generators. Leaves are
-- skips and phases; interior nodes split the size budget between children.
genProgram :: Gen ctx -> Gen p -> Gen (Program ctx p)
genProgram genContext genPhase =
  sized go
  where
    go size
      | size <= 0 =
          frequency
            [ (1, pure Skip),
              (3, Phase <$> genPhase)
            ]
      | otherwise =
          frequency
            [ (1, pure Skip),
              (3, Phase <$> genPhase),
              (4, Seq <$> sub <*> sub),
              (3, Or <$> sub <*> sub),
              (2, UpTo <$> genRepeatCount <*> sub),
              (2, Attempt <$> sub),
              (2, Scoped <$> genContext <*> sub)
            ]
      where
        sub = go (size `div` 2)

-- | A small repetition count, including the degenerate zero. O(1).
genRepeatCount :: Gen Natural
genRepeatCount =
  fromIntegral <$> chooseInt (0, 5)

shrinkRepeatCount :: Natural -> [Natural]
shrinkRepeatCount = shrinkIntegral

-- | Structural shrinking: replace a node by 'Skip' or by a subterm, then
-- shrink components pointwise.
shrinkProgram ::
  (ctx -> [ctx]) ->
  (p -> [p]) ->
  Program ctx p ->
  [Program ctx p]
shrinkProgram shrinkContext shrinkPhase =
  go
  where
    go program =
      case program of
        Skip ->
          []
        Phase phaseValue ->
          Skip : fmap Phase (shrinkPhase phaseValue)
        Seq left right ->
          Skip
            : left
            : right
            : fmap (`Seq` right) (go left)
            <> fmap (Seq left) (go right)
        Or left right ->
          Skip
            : left
            : right
            : fmap (`Or` right) (go left)
            <> fmap (Or left) (go right)
        UpTo repeatCount body ->
          Skip
            : body
            : fmap (`UpTo` body) (shrinkRepeatCount repeatCount)
            <> fmap (UpTo repeatCount) (go body)
        Attempt body ->
          Skip : body : fmap Attempt (go body)
        Scoped context body ->
          Skip
            : body
            : fmap (`Scoped` body) (shrinkContext context)
            <> fmap (Scoped context) (go body)
