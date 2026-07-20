{-# LANGUAGE PatternSynonyms #-}

module Moonlight.EGraph.Saturation.Cohomological.Backend.Instance.Internal.Frontier
  ( filterRegionsByMatchingFrontier,
    regionTouchesMatchingDelta,
    seedSurvivesMatchingFrontier,
  )
where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Moonlight.Delta.Scope qualified as Delta
import Moonlight.EGraph.Pure.Saturation.Matching
  ( MatchingFrontier,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    classIdKey,
  )
import Moonlight.Sheaf.Obstruction
  ( CandidateRegion (crRoot, crMembers),
    CandidateRegionSeed (crsRoot),
  )

seedSurvivesMatchingFrontier ::
  (ClassId -> ClassId) ->
  MatchingFrontier ->
  CandidateRegionSeed ClassId ->
  Bool
seedSurvivesMatchingFrontier canonicalize matchingFrontier seedValue =
  maybe
    True
    (\impactedClassKeys -> IntSet.member (canonicalClassKey canonicalize (crsRoot seedValue)) impactedClassKeys)
    (Delta.scopeKeys matchingFrontier)

filterRegionsByMatchingFrontier ::
  (ClassId -> ClassId) ->
  MatchingFrontier ->
  [CandidateRegion ClassId] ->
  [CandidateRegion ClassId]
filterRegionsByMatchingFrontier canonicalize matchingFrontier =
  maybe
    id
    (\impactedClassKeys -> filter (regionTouchesMatchingDelta canonicalize impactedClassKeys))
    (Delta.scopeKeys matchingFrontier)

regionTouchesMatchingDelta ::
  (ClassId -> ClassId) ->
  IntSet ->
  CandidateRegion ClassId ->
  Bool
regionTouchesMatchingDelta canonicalize impactedClassKeys regionValue =
  let canonicalRootKey =
        canonicalClassKey canonicalize (crRoot regionValue)
      canonicalMemberKeys =
        IntSet.map
          (canonicalMemberKey canonicalize)
          (crMembers regionValue)
   in IntSet.member canonicalRootKey impactedClassKeys
        || not (IntSet.null (IntSet.intersection canonicalMemberKeys impactedClassKeys))

canonicalClassKey :: (ClassId -> ClassId) -> ClassId -> Int
canonicalClassKey canonicalize =
  classIdKey . canonicalize

canonicalMemberKey :: (ClassId -> ClassId) -> Int -> Int
canonicalMemberKey canonicalize =
  canonicalClassKey canonicalize . ClassId
