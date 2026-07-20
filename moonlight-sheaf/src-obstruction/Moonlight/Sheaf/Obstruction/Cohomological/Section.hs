module Moonlight.Sheaf.Obstruction.Cohomological.Section
  ( SectionCoordinate (..),
    structuralCoordinate,
    relationCoordinate,
    RelationEvidence (..),
    SectionMatch (..),
    SectionCoverage (..),
    SectionFeasibilityFailure (..),
    SectionCoverageSummary (..),
    sectionCoverageFeasibility,
    mapSectionCoverage,
    mapSectionCoverageGaps,
    bimapSectionCoverage,
    firstSectionCoverageGap,
    foldSectionCoverage,
    summarizeSectionCoverage,
    renderSectionCoverage,
    projectSectionCoverageWith,
    projectSectionCoverage,
    SectionAssignment (..),
    sectionBindings,
    sectionBinding,
    SectionReification (..),
    emptySectionReification,
    sectionReification,
    reifySectionAssignment,
    relationEvidenceFromAssignment,
    ExactSearchCost (..),
    exactSearchCostWithSeed,
    exactSearchCostWithin,
    enumerateProjectedSectionsWithSeed,
    enumerateSectionMatchesWithSeed,
    enumerateSectionMatchesWithinBudgetWithSeed,
  )
where

import Data.Function ((&))
import Data.Kind (Type)
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Sheaf.Obstruction.Cohomological.Section.Projection
  ( SectionCoordinate (..),
    SectionProjection (..),
    relationCoordinate,
    structuralCoordinate,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Types
  ( ConstraintId,
    ExactConstraint (..),
    ExactLabelCode (..),
    RelationFlavor,
  )
import Numeric.Natural (Natural)

type SectionAssignment :: Type -> Type
newtype SectionAssignment coordinate = SectionAssignment
  { unSectionAssignment :: Map coordinate ExactLabelCode
  }
  deriving stock (Eq, Ord, Show, Read)

type SectionReification :: Type -> Type -> Type
newtype SectionReification coordinate result = SectionReification
  { runSectionReification :: SectionAssignment coordinate -> result -> Maybe result
  }

instance Semigroup (SectionReification coordinate result) where
  leftReification <> rightReification =
    SectionReification
      (\sectionAssignment seedResult ->
         runSectionReification leftReification sectionAssignment seedResult
           >>= runSectionReification rightReification sectionAssignment
      )

instance Monoid (SectionReification coordinate result) where
  mempty =
    emptySectionReification

sectionBindings :: SectionAssignment coordinate -> Map coordinate ExactLabelCode
sectionBindings =
  unSectionAssignment

sectionBinding :: Ord coordinate => coordinate -> SectionAssignment coordinate -> Maybe ExactLabelCode
sectionBinding coordinateValue =
  Map.lookup coordinateValue . unSectionAssignment

emptySectionReification :: SectionReification coordinate result
emptySectionReification =
  SectionReification (const Just)

sectionReification ::
  (SectionAssignment coordinate -> result -> Maybe result) ->
  SectionReification coordinate result
sectionReification =
  SectionReification

reifySectionAssignment ::
  SectionReification coordinate result ->
  result ->
  SectionAssignment coordinate ->
  Maybe result
reifySectionAssignment sectionReificationValue seedResult sectionAssignment =
  runSectionReification sectionReificationValue sectionAssignment seedResult

type RelationAtom :: Type -> Type
data RelationAtom coordinate = RelationAtom
  { raCoordinates :: ![coordinate],
    raTuples :: ![[ExactLabelCode]]
  }

type TupleCoordinateSupport :: Type
data TupleCoordinateSupport
  = TupleCoordinateAbsent
  | TupleCoordinateConflict
  | TupleCoordinateLabel !ExactLabelCode

type CompiledRelation :: Type -> Type
data CompiledRelation coordinate = CompiledRelation
  { crAllTupleIds :: !IntSet.IntSet,
    crMentionedCoordinates :: !(Set coordinate),
    crTupleIdsByCoordinateValue :: !(Map coordinate (Map ExactLabelCode IntSet.IntSet)),
    crAbsentTupleIdsByCoordinate :: !(Map coordinate IntSet.IntSet)
  }

type ExactSearchPlan :: Type -> Type
data ExactSearchPlan coordinate = ExactSearchPlan
  { espRelationAtoms :: ![RelationAtom coordinate],
    espCoordinateOrder :: ![coordinate],
    espCompiledRelations :: ![CompiledRelation coordinate]
  }

type ExactSearchState :: Type -> Type
data ExactSearchState coordinate = ExactSearchState
  { essBindings :: !(Map coordinate ExactLabelCode),
    essCandidateTupleIds :: ![IntSet.IntSet],
    essCoordinateDomains :: !(Map coordinate (Set ExactLabelCode))
  }

type RelationEvidence :: Type -> Type
data RelationEvidence coordinate = RelationEvidence
  { reFlavor :: !RelationFlavor,
    reConstraintId :: !ConstraintId,
    reCoordinates :: ![coordinate],
    reMatchingTuples :: ![[ExactLabelCode]]
  }
  deriving stock (Eq, Ord, Show, Read)

type SectionMatch :: Type -> Type -> Type
data SectionMatch coordinate result = SectionMatch
  { smAssignment :: !(SectionAssignment coordinate),
    smResult :: !result,
    smRelationEvidence :: ![RelationEvidence coordinate]
  }
  deriving stock (Eq, Ord, Show, Read)

type SectionCoverage :: Type -> Type -> Type
data SectionCoverage match gap = SectionCoverage
  { scMatches :: ![match],
    scLoweringGaps :: ![gap]
  }
  deriving stock (Eq, Ord, Show, Read)

type SectionFeasibilityFailure :: Type -> Type
data SectionFeasibilityFailure gap
  = CoverageGap !gap
  | EmptySupport
  deriving stock (Eq, Ord, Show, Read)

type SectionCoverageSummary :: Type -> Type
data SectionCoverageSummary gap = SectionCoverageSummary
  { scsMatchCount :: !Int,
    scsGapCount :: !Int,
    scsFirstGap :: !(Maybe gap)
  }
  deriving stock (Eq, Ord, Show, Read)

instance Semigroup (SectionCoverage match gap) where
  leftCoverage <> rightCoverage =
    SectionCoverage
      { scMatches =
          scMatches leftCoverage <> scMatches rightCoverage,
        scLoweringGaps =
          scLoweringGaps leftCoverage <> scLoweringGaps rightCoverage
      }

instance Monoid (SectionCoverage match gap) where
  mempty =
    SectionCoverage
      { scMatches = [],
        scLoweringGaps = []
      }

mapSectionCoverage ::
  (leftMatch -> rightMatch) ->
  SectionCoverage leftMatch gap ->
  SectionCoverage rightMatch gap
mapSectionCoverage reifyMatch coverage =
  SectionCoverage
    { scMatches = fmap reifyMatch (scMatches coverage),
      scLoweringGaps = scLoweringGaps coverage
    }

mapSectionCoverageGaps ::
  (leftGap -> rightGap) ->
  SectionCoverage match leftGap ->
  SectionCoverage match rightGap
mapSectionCoverageGaps reifyGap coverage =
  SectionCoverage
    { scMatches = scMatches coverage,
      scLoweringGaps = fmap reifyGap (scLoweringGaps coverage)
    }

bimapSectionCoverage ::
  (leftMatch -> rightMatch) ->
  (leftGap -> rightGap) ->
  SectionCoverage leftMatch leftGap ->
  SectionCoverage rightMatch rightGap
bimapSectionCoverage reifyMatch reifyGap =
  mapSectionCoverage reifyMatch . mapSectionCoverageGaps reifyGap

firstSectionCoverageGap :: SectionCoverage match gap -> Maybe gap
firstSectionCoverageGap =
  listToMaybe . scLoweringGaps

sectionCoverageFeasibility ::
  SectionCoverage match gap ->
  Either (SectionFeasibilityFailure gap) (NonEmpty match)
sectionCoverageFeasibility coverage =
  case projectSectionCoverage (\_ gapValue -> CoverageGap gapValue) coverage of
    Left rejection -> Left rejection
    Right coverageValue ->
      maybe
        (Left EmptySupport)
        Right
        (NonEmpty.nonEmpty (scMatches coverageValue))

foldSectionCoverage ::
  (SectionCoverage match gap -> accepted) ->
  (SectionCoverage match gap -> NonEmpty gap -> rejected) ->
  SectionCoverage match gap ->
  Either rejected accepted
foldSectionCoverage onAccepted onRejected coverage =
  maybe
    (Right (onAccepted coverage))
    (\gapValues -> Left (onRejected coverage gapValues))
    (NonEmpty.nonEmpty (scLoweringGaps coverage))

summarizeSectionCoverage ::
  SectionCoverage match gap ->
  SectionCoverageSummary gap
summarizeSectionCoverage coverage =
  either
    id
    id
    ( foldSectionCoverage
        (\coverageValue ->
           SectionCoverageSummary
             { scsMatchCount = length (scMatches coverageValue),
               scsGapCount = 0,
               scsFirstGap = Nothing
             }
        )
        (\coverageValue gapValues ->
           SectionCoverageSummary
             { scsMatchCount = length (scMatches coverageValue),
               scsGapCount = length (scLoweringGaps coverageValue),
               scsFirstGap = Just (NonEmpty.head gapValues)
             }
        )
        coverage
    )

renderSectionCoverage ::
  (gap -> renderedGap) ->
  SectionCoverage match gap ->
  SectionCoverageSummary renderedGap
renderSectionCoverage renderGap coverage =
  let summary = summarizeSectionCoverage coverage
   in SectionCoverageSummary
        { scsMatchCount = scsMatchCount summary,
          scsGapCount = scsGapCount summary,
          scsFirstGap = fmap renderGap (scsFirstGap summary)
        }

projectSectionCoverageWith ::
  (SectionCoverage match gap -> gap -> rejection) ->
  (SectionCoverage match gap -> accepted) ->
  SectionCoverage match gap ->
  Either rejection accepted
projectSectionCoverageWith mkRejection mkAccepted = foldSectionCoverage
    mkAccepted
    ( \coverageValue gapValues ->
        mkRejection coverageValue (NonEmpty.head gapValues)
    )

projectSectionCoverage ::
  (SectionCoverage match gap -> gap -> rejection) ->
  SectionCoverage match gap ->
  Either rejection (SectionCoverage match gap)
projectSectionCoverage mkRejection =
  projectSectionCoverageWith mkRejection id

relationEvidenceFromAssignment ::
  Ord coordinate =>
  SectionProjection anchor coordinate ->
  [ExactConstraint anchor] ->
  SectionAssignment coordinate ->
  [RelationEvidence coordinate]
relationEvidenceFromAssignment projectCoordinates constraints sectionAssignment =
  mapMaybe
    (\constraintValue -> relationEvidenceForConstraint projectCoordinates constraintValue sectionAssignment)
    constraints

type ExactSearchCost :: Type -> Type
data ExactSearchCost coordinate = ExactSearchCost
  { escUnseededCoordinates :: ![coordinate],
    escDomainSizes :: !(Map coordinate Natural),
    escAssignmentUpperBound :: !Natural
  }
  deriving stock (Eq, Show, Read)

exactSearchCostWithSeed ::
  Ord coordinate =>
  SectionProjection anchor coordinate ->
  Map coordinate ExactLabelCode ->
  [ExactConstraint anchor] ->
  ExactSearchCost coordinate
exactSearchCostWithSeed projectCoordinates seededBindings constraints =
  exactSearchCostFromAtomsWithSeed seededBindings (fmap (relationAtomOf projectCoordinates) constraints)

exactSearchCostWithin ::
  Maybe Natural ->
  ExactSearchCost coordinate ->
  Bool
exactSearchCostWithin maybeMaxAssignments searchCost =
  maybe True (escAssignmentUpperBound searchCost <=) maybeMaxAssignments

enumerateProjectedSectionsWithSeed ::
  Ord coordinate =>
  SectionProjection anchor coordinate ->
  Map coordinate ExactLabelCode ->
  [ExactConstraint anchor] ->
  [SectionAssignment coordinate]
enumerateProjectedSectionsWithSeed projectCoordinates seededBindings constraints =
  fmap SectionAssignment
    (enumerateProjectedBindingsWithSeed seededBindings (fmap (relationAtomOf projectCoordinates) constraints))

unseededOrderedAnchors ::
  Ord coordinate =>
  Map coordinate ExactLabelCode ->
  [RelationAtom coordinate] ->
  [coordinate]
unseededOrderedAnchors seededBindings = filter (`Map.notMember` seededBindings) . orderedAnchors

exactSearchCostFromAtomsWithSeed ::
  Ord coordinate =>
  Map coordinate ExactLabelCode ->
  [RelationAtom coordinate] ->
  ExactSearchCost coordinate
exactSearchCostFromAtomsWithSeed seededBindings relationAtoms =
  let seedCompatible =
        all (relationSeedCompatible seededBindings) relationAtoms

      unseededCoordinates =
        [ coordinateValue
        | seedCompatible,
          coordinateValue <- unseededOrderedAnchors seededBindings relationAtoms
        ]

      domainSizes =
        Map.fromList
          [ ( coordinateValue,
              fromIntegral (Set.size (domainForAnchor relationAtoms seededBindings coordinateValue))
            )
          | coordinateValue <- unseededCoordinates
          ]
   in ExactSearchCost
        { escUnseededCoordinates = unseededCoordinates,
          escDomainSizes = domainSizes,
          escAssignmentUpperBound =
            if seedCompatible
              then List.foldl' (*) 1 (Map.elems domainSizes)
              else 0
        }

enumerateProjectedBindingsWithSeed ::
  Ord coordinate =>
  Map coordinate ExactLabelCode ->
  [RelationAtom coordinate] ->
  [Map coordinate ExactLabelCode]
enumerateProjectedBindingsWithSeed seededBindings relationAtoms =
  case initialExactSearchState seededBindings relationAtoms of
    Nothing ->
      []
    Just (searchPlan, searchState) ->
      enumerateBindingsFromState searchPlan searchState

initialExactSearchState ::
  Ord coordinate =>
  Map coordinate ExactLabelCode ->
  [RelationAtom coordinate] ->
  Maybe (ExactSearchPlan coordinate, ExactSearchState coordinate)
initialExactSearchState seededBindings relationAtoms =
  let compiledRelations =
        fmap compileRelationAtom relationAtoms
      searchPlan =
        ExactSearchPlan
          { espRelationAtoms = relationAtoms,
            espCoordinateOrder = unseededOrderedAnchors seededBindings relationAtoms,
            espCompiledRelations = compiledRelations
          }
      candidateTupleIds =
        fmap (relationCandidateTupleIds seededBindings) compiledRelations
   in fmap
        (\searchState -> (searchPlan, searchState))
        ( enforceArcConsistency
            (espCoordinateOrder searchPlan)
            compiledRelations
            seededBindings
            candidateTupleIds
        )

compileRelationAtom :: Ord coordinate => RelationAtom coordinate -> CompiledRelation coordinate
compileRelationAtom relationAtom =
  CompiledRelation
    { crAllTupleIds =
        IntSet.fromDistinctAscList (fmap fst indexedTuples),
      crMentionedCoordinates =
        relationCoordinateSet,
      crTupleIdsByCoordinateValue =
        Map.fromListWith
          (Map.unionWith IntSet.union)
          [ (coordinateValue, Map.singleton labelCode (IntSet.singleton tupleId))
          | (tupleId, tupleValue) <- indexedTuples,
            coordinateValue <- relationCoordinates,
            TupleCoordinateLabel labelCode <- [tupleCoordinateSupport coordinateValue relationAtom tupleValue]
          ],
      crAbsentTupleIdsByCoordinate =
        Map.fromListWith
          IntSet.union
          [ (coordinateValue, IntSet.singleton tupleId)
          | (tupleId, tupleValue) <- indexedTuples,
            coordinateValue <- relationCoordinates,
            TupleCoordinateAbsent <- [tupleCoordinateSupport coordinateValue relationAtom tupleValue]
          ]
    }
  where
    indexedTuples =
      zip [0 :: Int ..] (raTuples relationAtom)

    relationCoordinates =
      Set.toAscList relationCoordinateSet

    relationCoordinateSet =
      Set.fromList (raCoordinates relationAtom)

tupleCoordinateSupport ::
  Eq coordinate =>
  coordinate ->
  RelationAtom coordinate ->
  [ExactLabelCode] ->
  TupleCoordinateSupport
tupleCoordinateSupport coordinateValue relationAtom tupleValue =
  case [labelCode | (candidate, labelCode) <- zip (raCoordinates relationAtom) tupleValue, candidate == coordinateValue] of
    [] ->
      TupleCoordinateAbsent
    firstLabel : remainingLabels
      | all (== firstLabel) remainingLabels ->
          TupleCoordinateLabel firstLabel
      | otherwise ->
          TupleCoordinateConflict

enforceArcConsistency ::
  Ord coordinate =>
  [coordinate] ->
  [CompiledRelation coordinate] ->
  Map coordinate ExactLabelCode ->
  [IntSet.IntSet] ->
  Maybe (ExactSearchState coordinate)
enforceArcConsistency coordinateOrder compiledRelations currentBindings candidateTupleIds =
  let coordinateDomains =
        coordinateDomainsFromCandidates coordinateOrder compiledRelations currentBindings candidateTupleIds
      blockedCoordinate =
        any
          ( \coordinateValue ->
              Map.notMember coordinateValue currentBindings
                && Set.null (Map.findWithDefault Set.empty coordinateValue coordinateDomains)
          )
          coordinateOrder
      prunedCandidateTupleIds =
        zipWith
          (pruneRelationCandidates currentBindings coordinateDomains)
          compiledRelations
          candidateTupleIds
   in if blockedCoordinate
        then Nothing
        else
          if prunedCandidateTupleIds == candidateTupleIds
            then
              Just
                ExactSearchState
                  { essBindings = currentBindings,
                    essCandidateTupleIds = candidateTupleIds,
                    essCoordinateDomains = coordinateDomains
                  }
            else
              enforceArcConsistency
                coordinateOrder
                compiledRelations
                currentBindings
                prunedCandidateTupleIds

coordinateDomainsFromCandidates ::
  Ord coordinate =>
  [coordinate] ->
  [CompiledRelation coordinate] ->
  Map coordinate ExactLabelCode ->
  [IntSet.IntSet] ->
  Map coordinate (Set ExactLabelCode)
coordinateDomainsFromCandidates coordinateOrder compiledRelations currentBindings candidateTupleIds =
  Map.fromList
    [ (coordinateValue, intersectDomains relationDomains)
    | coordinateValue <- coordinateOrder,
      Map.notMember coordinateValue currentBindings,
      let relationDomains =
            [ relationDomainFromCandidates compiledRelation candidateIds coordinateValue
            | (compiledRelation, candidateIds) <- zip compiledRelations candidateTupleIds,
              compiledRelationMentions coordinateValue compiledRelation
            ]
    ]

intersectDomains :: Ord label => [Set label] -> Set label
intersectDomains domains =
  case domains of
    [] ->
      Set.empty
    firstDomain : remainingDomains ->
      foldr Set.intersection firstDomain remainingDomains

relationDomainFromCandidates ::
  Ord coordinate =>
  CompiledRelation coordinate ->
  IntSet.IntSet ->
  coordinate ->
  Set ExactLabelCode
relationDomainFromCandidates compiledRelation candidateTupleIds coordinateValue =
  case Map.lookup coordinateValue (crTupleIdsByCoordinateValue compiledRelation) of
    Nothing ->
      Set.empty
    Just labelTupleIds ->
      Set.fromList
        [ labelCode
        | (labelCode, tupleIds) <- Map.toAscList labelTupleIds,
          not (IntSet.null (IntSet.intersection candidateTupleIds tupleIds))
        ]

pruneRelationCandidates ::
  Ord coordinate =>
  Map coordinate ExactLabelCode ->
  Map coordinate (Set ExactLabelCode) ->
  CompiledRelation coordinate ->
  IntSet.IntSet ->
  IntSet.IntSet
pruneRelationCandidates currentBindings coordinateDomains compiledRelation candidateTupleIds =
  List.foldl'
    ( \remainingTupleIds (coordinateValue, coordinateDomain) ->
        if Map.member coordinateValue currentBindings
          then remainingTupleIds
          else
            IntSet.intersection
              remainingTupleIds
              (relationTupleIdsForDomain compiledRelation coordinateValue coordinateDomain)
    )
    candidateTupleIds
    (Map.toAscList coordinateDomains)

relationCandidateTupleIds ::
  Ord coordinate =>
  Map coordinate ExactLabelCode ->
  CompiledRelation coordinate ->
  IntSet.IntSet
relationCandidateTupleIds currentBindings compiledRelation =
  List.foldl'
    ( \candidateTupleIds (coordinateValue, labelCode) ->
        IntSet.intersection
          candidateTupleIds
          (relationTupleIdsForBinding compiledRelation coordinateValue labelCode)
    )
    (crAllTupleIds compiledRelation)
    (Map.toAscList currentBindings)

relationTupleIdsForBinding ::
  Ord coordinate =>
  CompiledRelation coordinate ->
  coordinate ->
  ExactLabelCode ->
  IntSet.IntSet
relationTupleIdsForBinding compiledRelation coordinateValue labelCode =
  if compiledRelationMentions coordinateValue compiledRelation
    then
      IntSet.union
        (Map.findWithDefault IntSet.empty coordinateValue (crAbsentTupleIdsByCoordinate compiledRelation))
        ( maybe
            IntSet.empty
            id
            (Map.lookup coordinateValue (crTupleIdsByCoordinateValue compiledRelation) >>= Map.lookup labelCode)
        )
    else
      crAllTupleIds compiledRelation

relationTupleIdsForDomain ::
  Ord coordinate =>
  CompiledRelation coordinate ->
  coordinate ->
  Set ExactLabelCode ->
  IntSet.IntSet
relationTupleIdsForDomain compiledRelation coordinateValue coordinateDomain =
  if compiledRelationMentions coordinateValue compiledRelation
    then
      IntSet.union
        (Map.findWithDefault IntSet.empty coordinateValue (crAbsentTupleIdsByCoordinate compiledRelation))
        ( List.foldl'
            IntSet.union
            IntSet.empty
            [ tupleIds
            | labelCode <- Set.toAscList coordinateDomain,
              tupleIds <-
                maybeToList
                  (Map.lookup coordinateValue (crTupleIdsByCoordinateValue compiledRelation) >>= Map.lookup labelCode)
            ]
        )
    else
      crAllTupleIds compiledRelation

compiledRelationMentions :: Ord coordinate => coordinate -> CompiledRelation coordinate -> Bool
compiledRelationMentions coordinateValue =
  Set.member coordinateValue . crMentionedCoordinates

enumerateBindingsFromState ::
  Ord coordinate =>
  ExactSearchPlan coordinate ->
  ExactSearchState coordinate ->
  [Map coordinate ExactLabelCode]
enumerateBindingsFromState searchPlan searchState =
  case List.find (`Map.notMember` essBindings searchState) (espCoordinateOrder searchPlan) of
    Nothing ->
      [ essBindings searchState
      | all (relationSatisfied (essBindings searchState)) (espRelationAtoms searchPlan)
      ]
    Just coordinateValue ->
      [ projectedBindings
      | labelCode <- Set.toAscList (Map.findWithDefault Set.empty coordinateValue (essCoordinateDomains searchState)),
        nextState <- maybeToList (extendExactSearchState searchPlan searchState coordinateValue labelCode),
        projectedBindings <- enumerateBindingsFromState searchPlan nextState
      ]

extendExactSearchState ::
  Ord coordinate =>
  ExactSearchPlan coordinate ->
  ExactSearchState coordinate ->
  coordinate ->
  ExactLabelCode ->
  Maybe (ExactSearchState coordinate)
extendExactSearchState searchPlan searchState coordinateValue labelCode =
  enforceArcConsistency
    (espCoordinateOrder searchPlan)
    (espCompiledRelations searchPlan)
    nextBindings
    nextCandidateTupleIds
  where
    nextBindings =
      Map.insert coordinateValue labelCode (essBindings searchState)

    nextCandidateTupleIds =
      zipWith
        ( \compiledRelation candidateTupleIds ->
            IntSet.intersection
              candidateTupleIds
              (relationTupleIdsForBinding compiledRelation coordinateValue labelCode)
        )
        (espCompiledRelations searchPlan)
        (essCandidateTupleIds searchState)

enumerateSectionMatchesWithSeed ::
  Ord coordinate =>
  SectionProjection anchor coordinate ->
  SectionReification coordinate result ->
  Map coordinate ExactLabelCode ->
  result ->
  [ExactConstraint anchor] ->
  [SectionMatch coordinate result]
enumerateSectionMatchesWithSeed projectCoordinates sectionReificationValue seededBindings seedResult constraints =
  enumerateSectionMatchesFromAtomsWithSeed
    projectCoordinates
    sectionReificationValue
    seededBindings
    seedResult
    constraints
    (fmap (relationAtomOf projectCoordinates) constraints)

enumerateSectionMatchesWithinBudgetWithSeed ::
  Ord coordinate =>
  Maybe Natural ->
  SectionProjection anchor coordinate ->
  SectionReification coordinate result ->
  Map coordinate ExactLabelCode ->
  result ->
  [ExactConstraint anchor] ->
  Either (ExactSearchCost coordinate) [SectionMatch coordinate result]
enumerateSectionMatchesWithinBudgetWithSeed maybeMaxAssignments projectCoordinates sectionReificationValue seededBindings seedResult constraints =
  let relationAtoms =
        fmap (relationAtomOf projectCoordinates) constraints

      searchCost =
        exactSearchCostFromAtomsWithSeed seededBindings relationAtoms
   in if exactSearchCostWithin maybeMaxAssignments searchCost
        then
          Right
            ( enumerateSectionMatchesFromAtomsWithSeed
                projectCoordinates
                sectionReificationValue
                seededBindings
                seedResult
                constraints
                relationAtoms
            )
        else
          Left searchCost

enumerateSectionMatchesFromAtomsWithSeed ::
  Ord coordinate =>
  SectionProjection anchor coordinate ->
  SectionReification coordinate result ->
  Map coordinate ExactLabelCode ->
  result ->
  [ExactConstraint anchor] ->
  [RelationAtom coordinate] ->
  [SectionMatch coordinate result]
enumerateSectionMatchesFromAtomsWithSeed projectCoordinates sectionReificationValue seededBindings seedResult constraints relationAtoms =
  mapMaybe buildMatch (SectionAssignment <$> enumerateProjectedBindingsWithSeed seededBindings relationAtoms)
  where
    buildMatch sectionAssignment = do
      result <- reifySectionAssignment sectionReificationValue seedResult sectionAssignment
      pure
        SectionMatch
          { smAssignment = sectionAssignment,
            smResult = result,
            smRelationEvidence =
              relationEvidenceFromAssignment projectCoordinates constraints sectionAssignment
          }

relationEvidenceForConstraint ::
  Ord coordinate =>
  SectionProjection anchor coordinate ->
  ExactConstraint anchor ->
  SectionAssignment coordinate ->
  Maybe (RelationEvidence coordinate)
relationEvidenceForConstraint projectCoordinates constraintValue sectionAssignment =
  case constraintValue of
    RelationConstraint relationFlavor constraintId _ supportTuples ->
      let projectedCoordinates =
            projectConstraintCoordinates projectCoordinates constraintValue
          matchingTuples =
            supportTuples
              & filter
                ( tupleConsistent
                    (sectionBindings sectionAssignment)
                    (RelationAtom projectedCoordinates supportTuples)
                )
       in if null matchingTuples
            then Nothing
            else
              Just
                RelationEvidence
                  { reFlavor = relationFlavor,
                    reConstraintId = constraintId,
                    reCoordinates = projectedCoordinates,
                    reMatchingTuples = matchingTuples
                  }
    _ -> Nothing

relationAtomOf ::
  SectionProjection anchor coordinate ->
  ExactConstraint anchor ->
  RelationAtom coordinate
relationAtomOf projectCoordinates constraintValue =
  case constraintValue of
    EqualityConstraint _ _ _ supportDomain ->
      RelationAtom
        (projectConstraintCoordinates projectCoordinates constraintValue)
        ( IntSet.toAscList supportDomain
            & fmap (\classKey -> [ClassLabelCode classKey, ClassLabelCode classKey])
        )
    GuardConstraint _ _ _ supportDomain ->
      RelationAtom
        (projectConstraintCoordinates projectCoordinates constraintValue)
        ( IntSet.toAscList supportDomain
            & fmap (\classKey -> [ClassLabelCode classKey, ClassLabelCode classKey])
        )
    RelationConstraint _ _ _ supportTuples ->
      RelationAtom (projectConstraintCoordinates projectCoordinates constraintValue) supportTuples

orderedAnchors :: Ord coordinate => [RelationAtom coordinate] -> [coordinate]
orderedAnchors relationAtoms =
  relationAtoms
    & foldr
      (\relationAtom accumulatedAnchors ->
         foldr
           (\coordinateValue -> Map.insertWith (+) coordinateValue (relationCost relationAtom))
           accumulatedAnchors
           (raCoordinates relationAtom)
      )
      Map.empty
    & Map.toList
    & List.sortOn snd
    & fmap fst

relationCost :: RelationAtom coordinate -> Int
relationCost relationAtom =
  length (raCoordinates relationAtom) * max 1 (length (raTuples relationAtom))

domainForAnchor ::
  Ord coordinate =>
  [RelationAtom coordinate] ->
  Map coordinate ExactLabelCode ->
  coordinate ->
  Set ExactLabelCode
domainForAnchor relationAtoms currentBindings coordinateValue =
  case
    relationAtoms
      & filter (anchorAppearsIn coordinateValue)
      & fmap (domainInRelation currentBindings coordinateValue) of
    [] -> Set.empty
    firstDomain : remainingDomains ->
      foldr Set.intersection firstDomain remainingDomains

domainInRelation ::
  Ord coordinate =>
  Map coordinate ExactLabelCode ->
  coordinate ->
  RelationAtom coordinate ->
  Set ExactLabelCode
domainInRelation currentBindings coordinateValue relationAtom =
  Set.fromList
    ( mapMaybe
        (tupleComponent coordinateValue relationAtom)
        (consistentTuples currentBindings relationAtom)
    )

tupleComponent :: Eq coordinate => coordinate -> RelationAtom coordinate -> [ExactLabelCode] -> Maybe ExactLabelCode
tupleComponent coordinateValue relationAtom tupleValue =
  lookup coordinateValue (zip (raCoordinates relationAtom) tupleValue)

consistentTuples ::
  Ord coordinate =>
  Map coordinate ExactLabelCode ->
  RelationAtom coordinate ->
  [[ExactLabelCode]]
consistentTuples currentBindings relationAtom =
  raTuples relationAtom
    & filter (tupleConsistent currentBindings relationAtom)

tupleConsistent ::
  Ord coordinate =>
  Map coordinate ExactLabelCode ->
  RelationAtom coordinate ->
  [ExactLabelCode] ->
  Bool
tupleConsistent currentBindings relationAtom tupleValue =
  all (\(coordinateValue, labelCode) ->
             maybe True (== labelCode) (Map.lookup coordinateValue currentBindings)
          ) (zip (raCoordinates relationAtom) tupleValue)

relationSeedCompatible ::
  Ord coordinate =>
  Map coordinate ExactLabelCode ->
  RelationAtom coordinate ->
  Bool
relationSeedCompatible seededBindings relationAtom =
  not (null (consistentTuples seededBindings relationAtom))

relationSatisfied ::
  Ord coordinate =>
  Map coordinate ExactLabelCode ->
  RelationAtom coordinate ->
  Bool
relationSatisfied currentBindings relationAtom =
  any (tupleConsistent currentBindings relationAtom) (raTuples relationAtom)

anchorAppearsIn :: Eq coordinate => coordinate -> RelationAtom coordinate -> Bool
anchorAppearsIn coordinateValue =
  elem coordinateValue . raCoordinates
