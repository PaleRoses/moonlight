{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

-- | The sole physical planner for binary and n-ary joins. It derives exact
-- local compatibility facts, selects one schedule, and leaves execution to a
-- consumer; sequential and concurrent interpreters share the same tournament
-- tree rather than inventing pairing policies of their own.
module Moonlight.Triangulation.Internal.Join.Plan
  ( PairPlan (..)
  , planPair
  , TournamentPlan (..)
  , planTournament
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import Moonlight.Triangulation.Dcel (numVertices)
import Moonlight.Triangulation.Internal.Join.Seam (SeamPlan, planSeam)
import Moonlight.Triangulation.Internal.Join.SiteSet
  ( SiteSet
  , siteSetFromTriangulation
  , siteSetRelation
  , siteSetUnionWith
  )
import Moonlight.Triangulation.Internal.Representation (Triangulation)
import Moonlight.Triangulation.JoinSemilattice (JoinSemilattice (joinAnnotations))
import Moonlight.Triangulation.Internal.Types
  ( ConstraintMode (Unconstrained)
  , SiteRelation (..)
  )

data PairPlan annotation
  = ReturnLeftOperand
  | ReturnRightOperand
  | InsertLeftIntoRight !(SiteSet annotation)
  | InsertRightIntoLeft !(SiteSet annotation)
  | MergeSeparated !SeamPlan
  | RebuildCanonicalUnion !(SiteSet annotation)

-- | Stage cheap facts before exact set classification. Empty and structurally
-- identical operands return verbatim; skewed pairs preserve the larger value
-- through local insertion before seam planning is considered.
planPair
  :: JoinSemilattice annotation
  => Triangulation 'Unconstrained annotation () () ()
  -> Triangulation 'Unconstrained annotation () () ()
  -> PairPlan annotation
planPair left right
  | leftCount == 0 = ReturnRightOperand
  | rightCount == 0 = ReturnLeftOperand
  | left == right = ReturnLeftOperand
  | insertionIsCheaper leftCount rightCount = InsertLeftIntoRight leftSites
  | insertionIsCheaper rightCount leftCount = InsertRightIntoLeft rightSites
  | Just seamPlan <- planSeam left right = MergeSeparated seamPlan
  | otherwise =
      case siteSetRelation leftSites rightSites of
        EqualSites -> InsertLeftIntoRight leftSites
        LeftProperSubset -> InsertLeftIntoRight leftSites
        RightProperSubset -> InsertRightIntoLeft rightSites
        DisjointSites -> rebuildUnion
        PartialOverlap _ -> rebuildUnion
 where
  !leftCount = numVertices left
  !rightCount = numVertices right
  leftSites = siteSetFromTriangulation left
  rightSites = siteSetFromTriangulation right
  rebuildUnion =
    RebuildCanonicalUnion
      (siteSetUnionWith joinAnnotations leftSites rightSites)

-- A transaction reuses an existing topology only when the added side is small
-- enough that its expected local cavities beat one bulk sweep. This is an
-- internal cost estimate, deliberately not a caller-controlled threshold.
insertionIsCheaper :: Int -> Int -> Bool
insertionIsCheaper addition base = addition <= 64 || addition <= base `quot` 8
{-# INLINE insertionIsCheaper #-}

-- | A deterministic dependency graph. Leaves retain meshes as values; no site
-- flattening occurs, so singleton and repeated-value shortcut semantics remain
-- those of the binary operation. Duplicate operands need no planning pass:
-- every adjacent pair reaches the binary operation, whose structural-equality
-- shortcut already returns the operand verbatim, so a dedup here would buy a
-- quadratic scan of whole meshes to skip work the executor skips anyway.
data TournamentPlan mesh
  = TournamentLeaf !mesh
  | TournamentNode !(TournamentPlan mesh) !(TournamentPlan mesh)

planTournament :: NonEmpty mesh -> TournamentPlan mesh
planTournament = buildBalanced . fmap TournamentLeaf
 where
  buildBalanced :: NonEmpty (TournamentPlan value) -> TournamentPlan value
  buildBalanced (single :| []) = single
  buildBalanced plans = buildBalanced (pairRound plans)

  pairRound :: NonEmpty (TournamentPlan value) -> NonEmpty (TournamentPlan value)
  pairRound (left :| right : rest) =
    TournamentNode left right :| pairTail rest
  pairRound (single :| []) = single :| []

  pairTail :: [TournamentPlan value] -> [TournamentPlan value]
  pairTail (left : right : rest) = TournamentNode left right : pairTail rest
  pairTail rest = rest
