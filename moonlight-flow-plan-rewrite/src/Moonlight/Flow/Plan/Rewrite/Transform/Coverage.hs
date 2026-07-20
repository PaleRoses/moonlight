module Moonlight.Flow.Plan.Rewrite.Transform.Coverage
  ( coverageTransformCompose,
    coverSingletonEliminates,
  )
where

import Data.Set qualified as Set
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )
import Moonlight.Flow.Plan.Shape.Term
  ( CoverPayload (..),
    CoverageTransformPayload (..),
  )

coverageTransformCompose ::
  CoverageTransformPayload ->
  CoverageTransformPayload ->
  Maybe CoverageTransformPayload
coverageTransformCompose outer inner =
  case (outer, inner) of
    (CoverageObstructedBy obstruction, _) ->
      Just (CoverageObstructedBy obstruction)
    (_, CoverageObstructedBy obstruction) ->
      Just (CoverageObstructedBy obstruction)
    (CoveragePreserveExact, transform) ->
      Just transform
    (transform, CoveragePreserveExact) ->
      Just transform
    (CoverageDowngradeLowerBound, _) ->
      Just CoverageDowngradeLowerBound
    (_, CoverageDowngradeLowerBound) ->
      Just CoverageDowngradeLowerBound
    (CoverageExactByCover leftProof, CoverageExactByCover rightProof)
      | leftProof == rightProof ->
          Just (CoverageExactByCover leftProof)
      | otherwise ->
          Nothing
{-# INLINE coverageTransformCompose #-}

coverSingletonEliminates ::
  CoverPayload ->
  Maybe StableDigest128
coverSingletonEliminates payload =
  case Set.toAscList (cpMembers payload) of
    [memberDigest]
      | memberDigest == cpTargetShape payload ->
          Just memberDigest
    _ ->
      Nothing
{-# INLINE coverSingletonEliminates #-}
