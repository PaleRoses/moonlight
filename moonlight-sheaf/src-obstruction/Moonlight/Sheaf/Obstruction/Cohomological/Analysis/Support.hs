{-# LANGUAGE RankNTypes #-}

module Moonlight.Sheaf.Obstruction.Cohomological.Analysis.Support
  ( buildCohomologicalRegionSupport,
    validateOccurrenceDomains,
  )
where

import Data.IntSet qualified as IntSet
import Data.Foldable (traverse_)
import Data.Map.Strict qualified as Map
import Data.GADT.Compare (GCompare)
import Moonlight.Sheaf.Obstruction.Cohomological.Core.Environment
  ( buildIndexedEnvironment,
    oeaIndexedEnvironmentAlgebra,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Modality
  ( ModalityContribution (..),
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Substrate
  ( CohomologicalRegionSupport (..),
    CohomologicalSubstrate (..),
    SubstrateRegion,
    SubstrateWitness,
    csaCoverage,
    csaEvaluateSupport,
    csaMissingOccurrenceDomainCoverage,
    csaMapGap,
    csaOccurrenceDomains,
    csaSectionReification,
    substrateSupportAlgebra,
    substrateEnvironment,
    substrateOccurrenceId,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Types
  ( ModalityCoverage,
    ObstructionCell (..),
    ObstructionReason (..),
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Section.Projection
  ( RelationProjectionConflict,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Types.Capability
  ( isCompleteModalityCoverage,
  )

-- | Build the region support from occurrences and guards, evaluating modality
-- coverage and collecting exact constraints and lowering gaps.
buildCohomologicalRegionSupport ::
  ( CohomologicalSubstrate substrate,
    GCompare (SubstrateModalityKey substrate runtime)
  ) =>
  substrate ->
  SubstrateRequest substrate runtime ->
  SubstrateRegion substrate ->
  [SubstrateOccurrence substrate] ->
  [SubstrateGuard substrate] ->
  Either
    (ModalityCoverage
      (SubstrateModalityTag substrate)
      RelationProjectionConflict)
    ( CohomologicalRegionSupport
        (SubstrateOccurrence substrate)
        (SubstrateGuard substrate)
        (SubstrateSupportEvidence substrate)
        (SubstrateResult substrate)
        (SubstrateGap substrate)
    )
buildCohomologicalRegionSupport substrate request region occurrences guards =
  let supportAlgebra =
        substrateSupportAlgebra substrate

      environmentAlgebra =
        substrateEnvironment substrate

      indexedEnvironment =
        buildIndexedEnvironment
          request
          region
          occurrences
          guards
          (oeaIndexedEnvironmentAlgebra environmentAlgebra)

      coverage =
        csaCoverage supportAlgebra
   in if not (isCompleteModalityCoverage coverage)
        then Left coverage
        else
          case csaOccurrenceDomains supportAlgebra indexedEnvironment of
            Nothing ->
              Left (csaMissingOccurrenceDomainCoverage supportAlgebra)
            Just occurrenceDomains ->
              let (contribution, evidencePayload) =
                    csaEvaluateSupport supportAlgebra indexedEnvironment
               in Right
                    CohomologicalRegionSupport
                      { crsOccurrences = occurrences,
                        crsGuards = guards,
                        crsOccurrenceDomains = occurrenceDomains,
                        crsExactConstraints = mcExactConstraints contribution,
                        crsExactLoweringGaps =
                          fmap
                            (csaMapGap supportAlgebra)
                            (mcLoweringGaps contribution),
                        crsSectionReification =
                          csaSectionReification supportAlgebra indexedEnvironment,
                        crsEvidence = evidencePayload
                      }

-- | Check that every occurrence has a non-empty domain. Returns Left with an
-- obstruction witness built by the supplied builder on the first empty domain
-- found.
--
-- The witness builder is passed as a parameter to break the dependency on the
-- full witness-constructor module (which depends back on this module via
-- Analysis.hs). Callers in Analysis.Lift wire in 'emptyObstructionWitness'.
validateOccurrenceDomains ::
  CohomologicalSubstrate substrate =>
  substrate ->
  SubstrateRequest substrate runtime ->
  SubstrateRegion substrate ->
  ( [ObstructionCell] ->
    ObstructionReason
      (SubstrateRegion substrate)
      (ModalityCoverage (SubstrateModalityTag substrate) RelationProjectionConflict) ->
    SubstrateWitness substrate
  ) ->
  CohomologicalRegionSupport
    (SubstrateOccurrence substrate)
    guard
    evidence
    result
    gap ->
  Either (SubstrateWitness substrate) ()
validateOccurrenceDomains substrate _request _region mkWitness support =
  traverse_ validateOccurrence (crsOccurrences support)
  where
    validateOccurrence occurrence =
      let occurrenceId =
            substrateOccurrenceId substrate occurrence

          domain =
            Map.findWithDefault
              IntSet.empty
              occurrenceId
              (crsOccurrenceDomains support)
       in if IntSet.null domain
            then
              Left
                ( mkWitness
                    [OccurrenceCell occurrenceId]
                    (EmptyLocalDomain (OccurrenceCell occurrenceId))
                )
            else Right ()
