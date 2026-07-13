{-# LANGUAGE RankNTypes #-}

module Moonlight.Sheaf.Obstruction.Cohomological.Analysis
  ( analyzeCohomologicalRegion,
    kernelVerdictSummary,
    analyzeKernelAcceptedRegion,
    analyzeFreshCohomologicalRegion,
    analyzeExactSummary,
    analyzeRefinedSummary,
    cohomologicalWitnessFromLift,
    emptyObstructionWitness,
    modalityCoverageWitness,
    obstructionWitnessFor,
  )
where

import Data.GADT.Compare (GCompare)
import Data.List (mapAccumL)
import Data.Maybe (listToMaybe, mapMaybe)
import Moonlight.Homology
  ( HomologicalDegree (..),
    RepresentativeChain (..),
    representativeCocyclesOverQ,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Algebra
  ( rankGapLowerBound,
    rankUpperBoundary2,
    supportCellsFromBasis,
    supportCellsFromRepresentatives,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Analysis.Exact
  ( exactCoverageFromLift,
    exactCoverageSupportsObstruction,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Analysis.Lift
  ( buildCohomologicalLift,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Core.Cache
  ( insertCachedObstructionForDependencies,
    lookupCachedObstruction,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Evidence.Certification
  ( CachePolicy (..),
    SectionCertificationAlgebra (..),
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Evidence.Pruning
  ( CohomologicalPruningCertificate,
    CohomologicalPruningGates (cpgRegionDecision),
    RetainedCohomologicalRegion (..),
    cohomologicalFootprintMeasures,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Section.Region
  ( RegionAnalysisOutcome (..),
    RegionExactness (..),
    RegionTraversalSummary (..),
    recCoverage,
    recExactness,
    regionCoverageFromSectionCoverage,
    skippedRegionCoverage,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Substrate
  ( CohomologicalLift (..),
    CohomologicalSubstrate (..),
    SubstrateCache,
    SubstrateRegion,
    SubstrateRegionCoverage,
    SubstrateRegionSummary,
    SubstrateWitness,
    cacheKeyForRegion,
    emptyObstructionWitness,
    modalityCoverageWitness,
    normalizeCohomologicalRegion,
    obstructionWitnessFor,
    regionDependencyKeys,
    substratePolicy,
    substrateRefinedRegions,
    substrateShouldRefine,
    substrateCertification,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Types
  ( ObstructionLift (..),
    ObstructionReason (..),
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Core.Policy
  ( CohomologicalPolicy (..),
  )
import Moonlight.Sheaf.Pruning
  ( PruningCertificate (pcFootprint),
    PruningDecision (..),
  )
import Moonlight.Sheaf.Verdict
  ( Verdict (..),
  )

-- | Analyse a region, normalising it first and checking the kernel verdict
-- before committing to a full lift computation.
analyzeCohomologicalRegion ::
  ( CohomologicalSubstrate substrate,
    GCompare (SubstrateModalityKey substrate runtime),
    Ord (SubstrateRoot substrate),
    Ord (SubstrateResult substrate),
    Ord (SubstrateGap substrate),
    Ord (SubstratePurpose substrate)
  ) =>
  Bool ->
  CohomologicalPruningGates (SubstrateRoot substrate) ->
  substrate ->
  SubstrateCache substrate ->
  SubstrateRequest substrate runtime ->
  SubstrateRegion substrate ->
  (SubstrateCache substrate, SubstrateRegionSummary substrate)
analyzeCohomologicalRegion preferCoarse pruningGates substrate cache request region0 =
  let region =
        normalizeCohomologicalRegion substrate request region0
   in case kernelVerdictSummary substrate request region of
        Just summary ->
          (cache, summary)
        Nothing ->
          case cpgRegionDecision pruningGates region of
            PruningRejected certificate ->
              prunedRegionResult substrate cache region certificate
            PruningAccepted footprint ->
              analyzeKernelAcceptedRegion
                preferCoarse
                pruningGates
                substrate
                cache
                request
                ( RetainedCohomologicalRegion
                    { rcrRegion = region,
                      rcrFootprint = footprint
                    }
                )

-- | Check the substrate kernel verdict. Returns Just a summary when the
-- verdict rejects the region; Nothing when it is accepted.
kernelVerdictSummary ::
  CohomologicalSubstrate substrate =>
  substrate ->
  SubstrateRequest substrate runtime ->
  SubstrateRegion substrate ->
  Maybe (SubstrateRegionSummary substrate)
kernelVerdictSummary substrate request region =
  case socKernelVerdict (substrateCertification substrate) request region of
    Accepted () ->
      Nothing
    Rejected _ ->
      Just
        RegionTraversalSummary
          { rtsRegion = region,
            rtsOutcome =
              RegionAnalysisObstructed
                ( emptyObstructionWitness
                    substrate
                    request
                    region
                    []
                    KernelVerdictObstructed
                ),
            rtsCoverage = skippedRegionCoverage,
            rtsMeasures = []
          }

-- | Cache lookup / insertion wrapper for a kernel-accepted region.
analyzeKernelAcceptedRegion ::
  ( CohomologicalSubstrate substrate,
    GCompare (SubstrateModalityKey substrate runtime),
    Ord (SubstrateRoot substrate),
    Ord (SubstrateResult substrate),
    Ord (SubstrateGap substrate),
    Ord (SubstratePurpose substrate)
  ) =>
  Bool ->
  CohomologicalPruningGates (SubstrateRoot substrate) ->
  substrate ->
  SubstrateCache substrate ->
  SubstrateRequest substrate runtime ->
  RetainedCohomologicalRegion (SubstrateRoot substrate) ->
  (SubstrateCache substrate, SubstrateRegionSummary substrate)
analyzeKernelAcceptedRegion preferCoarse pruningGates substrate cache request retainedRegion =
  let region =
        rcrRegion retainedRegion
   in case prefilteredRegionSummary preferCoarse pruningGates substrate cache request region of
        Just result ->
          result
        Nothing ->
          let key =
                cacheKeyForRegion substrate request region

              useCache =
                regionCacheEnabled substrate request
           in case if useCache then lookupCachedObstruction key cache else Nothing of
                Just summary ->
                  (cache, summary)
                Nothing ->
                  let (cache1, summary) =
                        analyzeFreshCohomologicalRegion
                          preferCoarse
                          pruningGates
                          substrate
                          cache
                          request
                          retainedRegion

                      deps =
                        regionDependencyKeys substrate request region

                      cache2 =
                        if useCache
                          then insertCachedObstructionForDependencies deps key summary cache1
                          else cache1
                   in (cache2, summary)

-- | Analyse a fresh region (cache miss). Build the lift and decide on
-- obstruction vs exact coverage.
analyzeFreshCohomologicalRegion ::
  ( CohomologicalSubstrate substrate,
    GCompare (SubstrateModalityKey substrate runtime),
    Ord (SubstrateRoot substrate),
    Ord (SubstrateResult substrate),
    Ord (SubstrateGap substrate),
    Ord (SubstratePurpose substrate)
  ) =>
  Bool ->
  CohomologicalPruningGates (SubstrateRoot substrate) ->
  substrate ->
  SubstrateCache substrate ->
  SubstrateRequest substrate runtime ->
  RetainedCohomologicalRegion (SubstrateRoot substrate) ->
  (SubstrateCache substrate, SubstrateRegionSummary substrate)
analyzeFreshCohomologicalRegion preferCoarse pruningGates substrate cache request retainedRegion =
  let region =
        rcrRegion retainedRegion
   in case buildCohomologicalLift substrate request retainedRegion of
    Left witness ->
      ( cache,
        RegionTraversalSummary
          { rtsRegion = region,
            rtsOutcome = RegionAnalysisObstructed witness,
            rtsCoverage = skippedRegionCoverage,
            rtsMeasures = []
          }
      )

    Right lift ->
      let exactCoverage =
            exactCoverageFromLift substrate request region lift
       in case cohomologicalWitnessFromLift substrate request lift of
            Just witness
              | exactCoverageSupportsObstruction exactCoverage ->
                  obstructionRegionResult substrate cache region witness
              | ExactCoverageSkipped <- recExactness exactCoverage ->
                  ( cache,
                    RegionTraversalSummary
                      { rtsRegion = region,
                        rtsOutcome = RegionAnalysisTruncated witness,
                        rtsCoverage = exactCoverage,
                        rtsMeasures = []
                      }
                  )
            _ ->
              analyzeExactSummary
                preferCoarse
                pruningGates
                substrate
                cache
                request
                region
                exactCoverage

obstructionRegionResult ::
  substrate ->
  SubstrateCache substrate ->
  SubstrateRegion substrate ->
  SubstrateWitness substrate ->
  (SubstrateCache substrate, SubstrateRegionSummary substrate)
obstructionRegionResult _substrate cache region witness =
  ( cache,
    RegionTraversalSummary
      { rtsRegion = region,
        rtsOutcome = RegionAnalysisObstructed witness,
        rtsCoverage = skippedRegionCoverage,
        rtsMeasures = []
      }
  )

-- | Decide whether to refine or finalise a region given its exact coverage.
analyzeExactSummary ::
  ( CohomologicalSubstrate substrate,
    GCompare (SubstrateModalityKey substrate runtime),
    Ord (SubstrateRoot substrate),
    Ord (SubstrateResult substrate),
    Ord (SubstrateGap substrate),
    Ord (SubstratePurpose substrate)
  ) =>
  Bool ->
  CohomologicalPruningGates (SubstrateRoot substrate) ->
  substrate ->
  SubstrateCache substrate ->
  SubstrateRequest substrate runtime ->
  SubstrateRegion substrate ->
  SubstrateRegionCoverage substrate ->
  (SubstrateCache substrate, SubstrateRegionSummary substrate)
analyzeExactSummary preferCoarse pruningGates substrate cache request region leafCoverage
  | substrateShouldRefine substrate region =
      case substrateRefinedRegions substrate pruningGates request region of
        [] ->
          exactRegionResult substrate cache region leafCoverage
        refinedRegions ->
          analyzeRefinedSummary preferCoarse pruningGates substrate cache request region refinedRegions
  | otherwise =
      exactRegionResult substrate cache region leafCoverage

-- | Recursively analyse refined sub-regions and combine their summaries.
analyzeRefinedSummary ::
  ( CohomologicalSubstrate substrate,
    GCompare (SubstrateModalityKey substrate runtime),
    Ord (SubstrateRoot substrate),
    Ord (SubstrateResult substrate),
    Ord (SubstrateGap substrate),
    Ord (SubstratePurpose substrate)
  ) =>
  Bool ->
  CohomologicalPruningGates (SubstrateRoot substrate) ->
  substrate ->
  SubstrateCache substrate ->
  SubstrateRequest substrate runtime ->
  SubstrateRegion substrate ->
  [SubstrateRegion substrate] ->
  (SubstrateCache substrate, SubstrateRegionSummary substrate)
analyzeRefinedSummary preferCoarse pruningGates substrate cache request region refinedRegions =
  let (cache1, summaries) =
        mapAccumL
          (\cacheN refined ->
            analyzeCohomologicalRegion
              preferCoarse
              pruningGates
              substrate
              cacheN
              request
              refined)
          cache
          refinedRegions
   in (cache1, refinedTraversalSummary substrate region summaries)

-- | Build a witness from a completed lift, using rank-gap short-circuit when
-- the policy permits and representative cocycles otherwise.
cohomologicalWitnessFromLift ::
  CohomologicalSubstrate substrate =>
  substrate ->
  SubstrateRequest substrate runtime ->
  CohomologicalLift substrate ->
  Maybe (SubstrateWitness substrate)
cohomologicalWitnessFromLift substrate request lift
  | not (olExactH1Eligible obstructionLift) =
      Nothing

  | cpShortCircuitRankGap (substratePolicy substrate) && rankGap > 0 =
      Just
        ( obstructionWitnessFor
            substrate
            request
            (olRegion obstructionLift)
            (olRoot obstructionLift)
            (supportCellsFromBasis (clOneBasis lift))
            (PositiveFirstCohomology rankGap)
            rankGap
            (rankUpperBoundary2 (olExpandedComplex obstructionLift))
            0
        )

  | otherwise =
      case representativeBasis of
        [] ->
          Nothing
        _ ->
          Just
            ( obstructionWitnessFor
                substrate
                request
                (olRegion obstructionLift)
                (olRoot obstructionLift)
                (supportCellsFromRepresentatives (clOneBasis lift) representativeBasis)
                (PositiveFirstCohomology (length representativeBasis))
                (length representativeBasis)
                (rankUpperBoundary2 (olExpandedComplex obstructionLift))
                (length representativeBasis)
            )
  where
    obstructionLift =
      clObstructionLift lift

    rankGap =
      rankGapLowerBound (olExpandedComplex obstructionLift)

    representativeBasis =
      filter
        ((== HomologicalDegree 1) . representativeDegree)
        (representativeCocyclesOverQ (olExpandedComplex obstructionLift))

-- Internal helpers.

regionCacheEnabled ::
  CohomologicalSubstrate substrate =>
  substrate ->
  SubstrateRequest substrate runtime ->
  Bool
regionCacheEnabled substrate request =
  case socQueryCachePolicy (substrateCertification substrate) request of
    DoNotCache ->
      False
    SharedAcrossEnvironments ->
      True
    EnvironmentScoped _ ->
      True

prefilteredRegionSummary ::
  ( CohomologicalSubstrate substrate,
    GCompare (SubstrateModalityKey substrate runtime),
    Ord (SubstrateRoot substrate),
    Ord (SubstrateResult substrate),
    Ord (SubstrateGap substrate),
    Ord (SubstratePurpose substrate)
  ) =>
  Bool ->
  CohomologicalPruningGates (SubstrateRoot substrate) ->
  substrate ->
  SubstrateCache substrate ->
  SubstrateRequest substrate runtime ->
  SubstrateRegion substrate ->
  Maybe (SubstrateCache substrate, SubstrateRegionSummary substrate)
prefilteredRegionSummary preferCoarse pruningGates substrate cache request region =
  if preferCoarse && substrateShouldRefine substrate region
    then
      case substrateRefinedRegions substrate pruningGates request region of
        [] ->
          Nothing
        refinedRegions ->
          Just
            (analyzeRefinedSummary preferCoarse pruningGates substrate cache request region refinedRegions)
    else Nothing

prunedRegionResult ::
  substrate ->
  SubstrateCache substrate ->
  SubstrateRegion substrate ->
  CohomologicalPruningCertificate ->
  (SubstrateCache substrate, SubstrateRegionSummary substrate)
prunedRegionResult _ cache region certificate =
  ( cache,
    RegionTraversalSummary
      { rtsRegion = region,
        rtsOutcome = RegionAnalysisPruned certificate,
        rtsCoverage = skippedRegionCoverage,
        rtsMeasures = cohomologicalFootprintMeasures (pcFootprint certificate)
      }
  )

exactRegionResult ::
  substrate ->
  SubstrateCache substrate ->
  SubstrateRegion substrate ->
  SubstrateRegionCoverage substrate ->
  (SubstrateCache substrate, SubstrateRegionSummary substrate)
exactRegionResult substrate cache region exactCoverage =
  ( cache,
    RegionTraversalSummary
      { rtsRegion = region,
        rtsOutcome = regionOutcomeFromCoverage substrate exactCoverage,
        rtsCoverage = exactCoverage,
        rtsMeasures = []
      }
  )

regionOutcomeFromCoverage ::
  substrate ->
  SubstrateRegionCoverage substrate ->
  RegionAnalysisOutcome
    (SubstrateGap substrate)
    (SubstrateWitness substrate)
    CohomologicalPruningCertificate
regionOutcomeFromCoverage _ exactCoverage =
  RegionAnalysisExact (recExactness exactCoverage)

refinedTraversalSummary ::
  substrate ->
  SubstrateRegion substrate ->
  [SubstrateRegionSummary substrate] ->
  SubstrateRegionSummary substrate
refinedTraversalSummary substrate region summaries =
  let combinedCoverage =
        foldMap (recCoverage . rtsCoverage) summaries

      finalCoverage =
        regionCoverageFromSectionCoverage combinedCoverage
      combinedMeasures =
        foldMap rtsMeasures summaries

      outcome =
        if all (isObstructed . rtsOutcome) summaries
          then
            maybe
              ( maybe
                  (regionOutcomeFromCoverage substrate finalCoverage)
                  RegionAnalysisPruned
                  (firstPruningCertificate substrate summaries)
              )
              RegionAnalysisObstructed
              (firstObstructionWitness substrate summaries)
          else
            maybe
              (regionOutcomeFromCoverage substrate finalCoverage)
              RegionAnalysisTruncated
              (firstTruncatedWitness substrate summaries)
   in RegionTraversalSummary
        { rtsRegion = region,
          rtsOutcome = outcome,
          rtsCoverage = finalCoverage,
          rtsMeasures = combinedMeasures
        }

isObstructed ::
  RegionAnalysisOutcome gap witness pruning ->
  Bool
isObstructed outcome =
  case outcome of
    RegionAnalysisPruned {} -> True
    RegionAnalysisObstructed {} -> True
    RegionAnalysisTruncated {} -> False
    RegionAnalysisExact {} -> False

firstObstructionWitness ::
  substrate ->
  [SubstrateRegionSummary substrate] ->
  Maybe (SubstrateWitness substrate)
firstObstructionWitness _ =
  listToMaybe
    . mapMaybe
      ( \summary ->
          case rtsOutcome summary of
            RegionAnalysisPruned _ -> Nothing
            RegionAnalysisObstructed witness -> Just witness
            RegionAnalysisTruncated _ -> Nothing
            RegionAnalysisExact _ -> Nothing
      )

firstTruncatedWitness ::
  substrate ->
  [SubstrateRegionSummary substrate] ->
  Maybe (SubstrateWitness substrate)
firstTruncatedWitness _ =
  listToMaybe
    . mapMaybe
      ( \summary ->
          case rtsOutcome summary of
            RegionAnalysisPruned _ -> Nothing
            RegionAnalysisObstructed _ -> Nothing
            RegionAnalysisTruncated witness -> Just witness
            RegionAnalysisExact _ -> Nothing
      )

firstPruningCertificate ::
  substrate ->
  [SubstrateRegionSummary substrate] ->
  Maybe CohomologicalPruningCertificate
firstPruningCertificate _ =
  listToMaybe
    . mapMaybe
      ( \summary ->
          case rtsOutcome summary of
            RegionAnalysisPruned certificate -> Just certificate
            RegionAnalysisObstructed _ -> Nothing
            RegionAnalysisTruncated _ -> Nothing
            RegionAnalysisExact _ -> Nothing
      )
