module Moonlight.EGraph.Saturation.CohomologicalSpec.Site
  ( tests
  )
where

import Moonlight.EGraph.Saturation.CohomologicalSpec.Prelude
import Moonlight.Saturation.Matching qualified as GenericMatching
import Data.IntSet qualified as IntSet
import Moonlight.EGraph.Saturation.Cohomological.Backend.Modality
  ( validateSheafModalityCoverage,
  )
import Moonlight.Sheaf.Obstruction
  ( CohomologicalPolicy (..),
    CohomologicalProfile (..),
    LaplacianPruning (..),
    defaultExactCoverageBudget,
    laplacianGapThresholdValue,
    profileExactCoverageBudget,
    profileLaplacianPruning,
    profilePolicy,
    mkSheafModalityCoverage,
  )
import Moonlight.Sheaf.Obstruction
  ( RegionScale (..),
  )

tests :: TestTree
tests =
  testGroup
    "site"
    [
      testCase "sheaf modality coverage is closed and complete" $
              validateSheafModalityCoverage (propertyContext (ClassId 11) (ClassId 22))
                @?= mkSheafModalityCoverage [] [] [],
      testCase "profile policies install named exact-search and laplacian defaults except for exact witness mode" $
              do
                let conservativePolicy = profilePolicy ConservativeProfile
                    aggressivePolicy = profilePolicy AggressivePruningProfile
                    balancedPolicy = profilePolicy BalancedPruningProfile
                    exactPolicy = exactWitnessPolicy
                    thresholdOf =
                      fmap (laplacianGapThresholdValue . lpGapThreshold)
                        . cpLaplacianPruning
                cpLaplacianPruning conservativePolicy @?= profileLaplacianPruning ConservativeProfile
                cpLaplacianPruning aggressivePolicy @?= profileLaplacianPruning AggressivePruningProfile
                cpLaplacianPruning balancedPolicy @?= profileLaplacianPruning BalancedPruningProfile
                cpLaplacianPruning exactPolicy @?= Nothing
                cpExactCoverageBudget conservativePolicy @?= Just defaultExactCoverageBudget
                cpExactCoverageBudget aggressivePolicy @?= profileExactCoverageBudget AggressivePruningProfile
                cpExactCoverageBudget balancedPolicy @?= profileExactCoverageBudget BalancedPruningProfile
                cpExactCoverageBudget exactPolicy @?= Nothing
                thresholdOf conservativePolicy @?= Just 0.05
                thresholdOf balancedPolicy @?= Just 0.08
                thresholdOf aggressivePolicy @?= Just 0.12,
      testCase "obstruction cache round-trips keys" $
              let cacheKey =
                    ObstructionCacheKey
                      { ockQueryFingerprint = 13,
                        ockRegionFingerprint = 21,
                        ockScale = CoarseRegion,
                        ockPurpose = GenericMatching.RewritePurpose (RewriteRuleId 8),
                        ockEnvironmentFingerprint = Just 34
                      }
                  cacheKey :: ObstructionCacheKey SaturationPurpose
                  cache :: CohomologicalCache SaturationPurpose ()
                  cache =
                    insertCachedObstruction
                      cacheKey
                      ()
                      emptyCohomologicalCache
               in lookupCachedObstruction cacheKey cache @?= Just (),
      testCase "obstruction cache invalidates impacted dependency classes" $
              let impactedCacheKey =
                    ObstructionCacheKey
                      { ockQueryFingerprint = 1,
                        ockRegionFingerprint = 2,
                        ockScale = CoarseRegion,
                        ockPurpose = GenericMatching.RewritePurpose (RewriteRuleId 4),
                        ockEnvironmentFingerprint = Nothing
                      }
                  impactedCacheKey :: ObstructionCacheKey SaturationPurpose
                  retainedCacheKey =
                    ObstructionCacheKey
                      { ockQueryFingerprint = 3,
                        ockRegionFingerprint = 5,
                        ockScale = FineRegion,
                        ockPurpose = GenericMatching.RewritePurpose (RewriteRuleId 6),
                        ockEnvironmentFingerprint = Nothing
                      }
                  retainedCacheKey :: ObstructionCacheKey SaturationPurpose
                  cache :: CohomologicalCache SaturationPurpose ()
                  cache =
                    insertCachedObstructionForDependencies
                      (IntSet.singleton 17)
                      retainedCacheKey
                      ()
                      ( insertCachedObstructionForDependencies
                          (IntSet.fromList [11, 13])
                          impactedCacheKey
                          ()
                          emptyCohomologicalCache
                      )
                  rebuildDelta =
                    EGraphRebuildDelta
                      { erdImpactedClassKeys = IntSet.singleton 13
                      , erdDirtyResultKeys = IntSet.empty
                      , erdTopologyClassKeys = IntSet.empty
                      }
                  invalidatedCache =
                    invalidateCachedObstructions
                      (erdImpactedClassKeys rebuildDelta)
                      cache
               in do
                    lookupCachedObstruction impactedCacheKey invalidatedCache @?= Nothing
                    lookupCachedObstruction retainedCacheKey invalidatedCache @?= Just ()
    ]
