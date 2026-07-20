module Moonlight.Sheaf.Obstruction.Cohomological.Analysis.Exact
  ( matchesFromCohomologicalLift,
    exactMatchFromSection,
    occurrenceDomainConstraints,
    syntheticOccurrenceConstraintId,
    exactCoverageFromLift,
    exactCoverageSupportsObstruction,
  )
where

import Data.Map.Strict qualified as Map
import Moonlight.Sheaf.Obstruction.Cohomological.Section.Exact
  ( CohomologicalExactMatch (..),
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Core.Policy
  ( CohomologicalPolicy (..),
    ExactCoverageBudget (..),
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Section.Region
  ( RegionExactCoverage,
    RegionExactness (..),
    recExactness,
    regionCoverageFromSectionCoverage,
    skippedRegionCoverage,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Section
  ( ExactSearchCost,
    SectionCoverage (..),
    SectionMatch (..),
    enumerateSectionMatchesWithinBudgetWithSeed,
    enumerateSectionMatchesWithSeed,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Substrate
  ( CohomologicalAnchor,
    CohomologicalCoordinate,
    CohomologicalLift (..),
    CohomologicalSubstrate (..),
    SubstrateExactMatch,
    SubstrateRegion,
    SubstrateRegionCoverage,
    csaSectionProjection,
    substrateSupportAlgebra,
    substrateCanonicalRoot,
    substrateEmptyResult,
    substrateExactEvidence,
    substratePolicy,
    substrateRootKey,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Types
  ( Anchor (..),
    CandidateRegion (..),
    ConstraintId (..),
    ExactConstraint (..),
    ExactLabelCode (..),
    ObstructionLift (..),
    OccurrenceId (..),
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Section.Projection
  ( SectionCoordinate (..),
    SectionProjection,
  )

-- | Build exact coverage from a completed lift. If H1 is eligible (no lowering
-- gaps and coboundary nilpotence holds), enumerate section matches; otherwise
-- report the lowering gaps as a coverage failure.
exactCoverageFromLift ::
  CohomologicalSubstrate substrate =>
  substrate ->
  SubstrateRequest substrate runtime ->
  SubstrateRegion substrate ->
  CohomologicalLift substrate ->
  SubstrateRegionCoverage substrate
exactCoverageFromLift substrate request region lift
  | not (olExactH1Eligible (clObstructionLift lift)) =
      regionCoverageFromSectionCoverage (SectionCoverage [] (clExactLoweringGaps lift))
  | otherwise =
      case exactLiftPreflight substrate request region lift of
        Nothing ->
          regionCoverageFromSectionCoverage mempty
        Just preflight ->
          either
            (const skippedRegionCoverage)
            ( \sectionMatches ->
                regionCoverageFromSectionCoverage
                  ( SectionCoverage
                      (fmap (exactMatchFromSection substrate request lift (elpRoot preflight)) sectionMatches)
                      []
                  )
            )
            (exactSectionMatchesWithinBudget substrate lift preflight)

-- | Enumerate exact matches by projecting sections over the combined domain
-- constraints and occurrence-domain equality constraints.
matchesFromCohomologicalLift ::
  CohomologicalSubstrate substrate =>
  substrate ->
  SubstrateRequest substrate runtime ->
  SubstrateRegion substrate ->
  CohomologicalLift substrate ->
  [SubstrateExactMatch substrate]
matchesFromCohomologicalLift substrate request region lift =
  maybe
    []
    ( \preflight ->
        fmap
          (exactMatchFromSection substrate request lift (elpRoot preflight))
          (exactSectionMatches substrate lift preflight)
    )
    (exactLiftPreflight substrate request region lift)

data ExactLiftPreflight substrate = ExactLiftPreflight
  { elpSectionProjection :: !(SectionProjection CohomologicalAnchor CohomologicalCoordinate),
    elpRoot :: !(SubstrateRoot substrate),
    elpSeededBindings :: !(Map.Map CohomologicalCoordinate ExactLabelCode),
    elpConstraints :: ![ExactConstraint CohomologicalAnchor]
  }

exactLiftPreflight ::
  CohomologicalSubstrate substrate =>
  substrate ->
  SubstrateRequest substrate runtime ->
  SubstrateRegion substrate ->
  CohomologicalLift substrate ->
  Maybe (ExactLiftPreflight substrate)
exactLiftPreflight substrate request region lift =
  fmap
    ( \sectionProjection ->
        let root = substrateCanonicalRoot substrate request (crRoot region)
         in ExactLiftPreflight
              { elpSectionProjection = sectionProjection,
                elpRoot = root,
                elpSeededBindings =
                  Map.singleton
                    (StructuralCoordinate RootAnchor)
                    (ClassLabelCode (substrateRootKey substrate request root)),
                elpConstraints = exactConstraintsForLift lift
              }
    )
    (either (const Nothing) Just (csaSectionProjection (substrateSupportAlgebra substrate)))

exactSectionMatches ::
  CohomologicalSubstrate substrate =>
  substrate ->
  CohomologicalLift substrate ->
  ExactLiftPreflight substrate ->
  [SectionMatch CohomologicalCoordinate (SubstrateResult substrate)]
exactSectionMatches substrate lift preflight =
  enumerateSectionMatchesWithSeed
    (elpSectionProjection preflight)
    (clSectionReification lift)
    (elpSeededBindings preflight)
    (substrateEmptyResult substrate)
    (elpConstraints preflight)

exactSectionMatchesWithinBudget ::
  CohomologicalSubstrate substrate =>
  substrate ->
  CohomologicalLift substrate ->
  ExactLiftPreflight substrate ->
  Either
    (ExactSearchCost CohomologicalCoordinate)
    [SectionMatch CohomologicalCoordinate (SubstrateResult substrate)]
exactSectionMatchesWithinBudget substrate lift preflight =
  enumerateSectionMatchesWithinBudgetWithSeed
    (ecbMaxAssignments <$> cpExactCoverageBudget (substratePolicy substrate))
    (elpSectionProjection preflight)
    (clSectionReification lift)
    (elpSeededBindings preflight)
    (substrateEmptyResult substrate)
    (elpConstraints preflight)

exactConstraintsForLift ::
  CohomologicalLift substrate ->
  [ExactConstraint CohomologicalAnchor]
exactConstraintsForLift lift =
  occurrenceDomainConstraints lift <> clExactConstraints lift

-- | Construct a single exact match from a section match.
exactMatchFromSection ::
  CohomologicalSubstrate substrate =>
  substrate ->
  SubstrateRequest substrate runtime ->
  CohomologicalLift substrate ->
  SubstrateRoot substrate ->
  SectionMatch CohomologicalCoordinate (SubstrateResult substrate) ->
  SubstrateExactMatch substrate
exactMatchFromSection substrate request lift root sectionMatch =
  CohomologicalExactMatch
    { cemRootClass = root,
      cemSubstitution = smResult sectionMatch,
      cemEvidence =
        substrateExactEvidence
          substrate
          request
          lift
          root
          (smRelationEvidence sectionMatch)
    }

-- | Synthesise equality constraints from the occurrence-domain map. These pin
-- each occurrence variable to its candidate class set.
occurrenceDomainConstraints ::
  CohomologicalLift substrate ->
  [ExactConstraint CohomologicalAnchor]
occurrenceDomainConstraints lift =
  fmap
    (\(occurrenceId, occurrenceDomain) ->
      EqualityConstraint
        (syntheticOccurrenceConstraintId occurrenceId)
        (OccurrenceAnchor occurrenceId)
        (OccurrenceAnchor occurrenceId)
        occurrenceDomain)
    (Map.toAscList (clOccurrenceDomains lift))

-- | Produce a synthetic constraint id for an occurrence domain constraint.
-- Uses a negative offset so it cannot clash with real constraint ids.
syntheticOccurrenceConstraintId :: OccurrenceId -> ConstraintId
syntheticOccurrenceConstraintId occurrenceId =
  ConstraintId (negate (unOccurrenceId occurrenceId + 1))

-- | True only when the exact coverage is feasible (has matches). When exact
-- coverage is skipped or infeasible, an H1 obstruction should not be raised.
exactCoverageSupportsObstruction ::
  RegionExactCoverage match gap ->
  Bool
exactCoverageSupportsObstruction coverage =
  case recExactness coverage of
    ExactCoverageFeasible -> True
    ExactCoverageSkipped -> False
    ExactCoverageInfeasible _ -> False
