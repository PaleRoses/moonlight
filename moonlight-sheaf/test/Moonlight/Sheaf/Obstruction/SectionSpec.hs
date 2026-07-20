module Moonlight.Sheaf.Obstruction.SectionSpec
  ( sectionTests,
  )
where

import Data.IntSet qualified as IntSet
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Moonlight.Sheaf.Obstruction
  ( SectionCoordinate,
    defaultSectionProjection,
    sectionCoordinateProjection,
    separateRelationFlavors,
  )
import Moonlight.Sheaf.Obstruction
  ( enumerateProjectedSectionsWithSeed,
    exactSearchCostWithSeed,
    exactSearchCostWithin,
    firstSectionCoverageGap,
    foldSectionCoverage,
    mapSectionCoverageGaps,
    renderSectionCoverage,
    projectSectionCoverage,
    projectSectionCoverageWith,
    relationEvidenceFromAssignment,
    sectionCoverageFeasibility,
    enumerateSectionMatchesWithSeed,
    RelationEvidence (..),
    SectionCoverage (..),
    SectionFeasibilityFailure (..),
    SectionCoverageSummary (..),
    ExactSearchCost (..),
    SectionAssignment (..),
    SectionMatch (..),
    relationCoordinate,
    sectionBinding,
    sectionBindings,
    sectionReification,
    structuralCoordinate,
  )
import Moonlight.Sheaf.Obstruction
  ( Anchor (..),
    ConstraintId (..),
    ExactConstraint (..),
    ExactLabelCode (..),
    OccurrenceId (..),
    RelationFlavor (..),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertEqual, testCase)

enumerateSections :: Ord anchor => [ExactConstraint anchor] -> [SectionAssignment (SectionCoordinate anchor)]
enumerateSections =
  enumerateProjectedSectionsWithSeed defaultSectionProjection Map.empty

enumerateSectionsWithSeed ::
  Ord anchor =>
  Map.Map (SectionCoordinate anchor) ExactLabelCode ->
  [ExactConstraint anchor] ->
  [SectionAssignment (SectionCoordinate anchor)]
enumerateSectionsWithSeed =
  enumerateProjectedSectionsWithSeed defaultSectionProjection

sectionTests :: TestTree
sectionTests =
  testGroup
    "section"
    [ testCase "enumerateSections solves binary equality plus ternary relation constraints" $
        let sections =
              enumerateSections
                [ EqualityConstraint
                    (ConstraintId 0)
                    RootAnchor
                    (OccurrenceAnchor (OccurrenceId 0))
                    (IntSet.fromList [3, 5]),
                  RelationConstraint
                    FactFlavor
                    (ConstraintId 1)
                    [RootAnchor, OccurrenceAnchor (OccurrenceId 0), OccurrenceAnchor (OccurrenceId 1)]
                    [ [ClassLabelCode 3, ClassLabelCode 3, ClassLabelCode 7],
                      [ClassLabelCode 5, ClassLabelCode 5, ClassLabelCode 11]
                    ]
                ]
         in assertEqual
              "global sections respect both constraints"
              [ Map.fromList
                  [ (structuralCoordinate RootAnchor, ClassLabelCode 3),
                    (structuralCoordinate (OccurrenceAnchor (OccurrenceId 0)), ClassLabelCode 3),
                    (structuralCoordinate (OccurrenceAnchor (OccurrenceId 1)), ClassLabelCode 7)
                  ],
                Map.fromList
                  [ (structuralCoordinate RootAnchor, ClassLabelCode 5),
                    (structuralCoordinate (OccurrenceAnchor (OccurrenceId 0)), ClassLabelCode 5),
                    (structuralCoordinate (OccurrenceAnchor (OccurrenceId 1)), ClassLabelCode 11)
                  ]
              ]
              (fmap sectionBindings sections)
    , testCase "relationEvidenceFromAssignment retains structured relation witnesses" $
        let constraints =
              [ RelationConstraint
                  FactFlavor
                  (ConstraintId 1)
                  [RootAnchor, OccurrenceAnchor (OccurrenceId 0), OccurrenceAnchor (OccurrenceId 1)]
                  [ [ClassLabelCode 3, ClassLabelCode 3, ClassLabelCode 7],
                    [ClassLabelCode 5, ClassLabelCode 5, ClassLabelCode 11]
                  ]
              ]
            sections =
              enumerateSectionsWithSeed
                (Map.singleton (structuralCoordinate RootAnchor) (ClassLabelCode 3))
                constraints
         in case sections of
              [sectionAssignment] ->
                assertEqual
                  "matching tuples are preserved for diagnostics"
                  [ RelationEvidence
                      { reFlavor = FactFlavor,
                        reConstraintId = ConstraintId 1,
                        reCoordinates =
                          [ structuralCoordinate RootAnchor,
                            structuralCoordinate (OccurrenceAnchor (OccurrenceId 0)),
                            structuralCoordinate (OccurrenceAnchor (OccurrenceId 1))
                          ],
                        reMatchingTuples =
                          [[ClassLabelCode 3, ClassLabelCode 3, ClassLabelCode 7]]
                      }
                  ]
                  (relationEvidenceFromAssignment (sectionCoordinateProjection (separateRelationFlavors (== CapabilityFlavor))) constraints sectionAssignment)
              _ ->
                assertEqual "expected a single seeded section" 1 (length sections)
    , testCase "enumerateSectionsWithSeed rejects inconsistent seeded assignments" $
        let sections =
              enumerateSectionsWithSeed
                (Map.singleton (structuralCoordinate RootAnchor) (ClassLabelCode 9))
                [ EqualityConstraint
                    (ConstraintId 0)
                    RootAnchor
                    (OccurrenceAnchor (OccurrenceId 0))
                    (IntSet.fromList [3, 5])
                ]
         in assertEqual
              "inconsistent seeds admit no sections"
              []
              sections
    , testCase "exactSearchCostWithSeed computes a seed-domain leaf upper bound" $
        let unary occurrenceId labels =
              RelationConstraint
                FactFlavor
                (ConstraintId occurrenceId)
                [OccurrenceAnchor (OccurrenceId occurrenceId)]
                (fmap (pure . ClassLabelCode) labels)
            cost =
              exactSearchCostWithSeed
                defaultSectionProjection
                (Map.singleton (structuralCoordinate RootAnchor) (ClassLabelCode 7))
                [unary 0 [1, 2], unary 1 [3, 4, 5]]
         in do
              escUnseededCoordinates cost
                @?= [ structuralCoordinate (OccurrenceAnchor (OccurrenceId 0)),
                      structuralCoordinate (OccurrenceAnchor (OccurrenceId 1))
                    ]
              escAssignmentUpperBound cost @?= 6
              exactSearchCostWithin (Just 6) cost @?= True
              exactSearchCostWithin (Just 5) cost @?= False
    , testCase "enumerateSectionMatchesWithSeed carries reified results and relation evidence" $
        let constraints =
              [ RelationConstraint
                  FactFlavor
                  (ConstraintId 3)
                  [RootAnchor, OccurrenceAnchor (OccurrenceId 0)]
                  [[ClassLabelCode 5, ClassLabelCode 8]]
              ]
            sectionMatches =
              enumerateSectionMatchesWithSeed
                (sectionCoordinateProjection (separateRelationFlavors (== CapabilityFlavor)))
                ( sectionReification
                    (\sectionAssignment _ ->
                       sectionBinding (structuralCoordinate RootAnchor) sectionAssignment
                    )
                )
                Map.empty
                (ClassLabelCode 0)
                constraints
         in assertEqual
              "section matches retain both result and supporting evidence"
              [ SectionMatch
                  { smAssignment =
                      SectionAssignment
                        ( Map.fromList
                            [ (structuralCoordinate RootAnchor, ClassLabelCode 5),
                              (structuralCoordinate (OccurrenceAnchor (OccurrenceId 0)), ClassLabelCode 8)
                            ]
                        ),
                    smResult = ClassLabelCode 5,
                    smRelationEvidence =
                      [ RelationEvidence
                          { reFlavor = FactFlavor,
                            reConstraintId = ConstraintId 3,
                            reCoordinates =
                              [ structuralCoordinate RootAnchor,
                                structuralCoordinate (OccurrenceAnchor (OccurrenceId 0))
                              ],
                            reMatchingTuples =
                              [[ClassLabelCode 5, ClassLabelCode 8]]
                          }
                      ]
                  }
              ]
              sectionMatches
    , testCase "enumerateSectionsWithSeed collapses repeated anchors through tuple consistency" $
        let sections =
              enumerateSectionsWithSeed
                Map.empty
                [ RelationConstraint
                    CapabilityFlavor
                    (ConstraintId 2)
                    [RootAnchor, RootAnchor, OccurrenceAnchor (OccurrenceId 0)]
                    [ [FiniteLabelCode 1, FiniteLabelCode 1, FiniteLabelCode 8],
                      [FiniteLabelCode 1, FiniteLabelCode 2, FiniteLabelCode 13],
                      [FiniteLabelCode 3, FiniteLabelCode 3, FiniteLabelCode 21]
                    ]
                ]
         in assertEqual
              "only tuples consistent on repeated anchors survive"
              [ Map.fromList
                  [ (relationCoordinate CapabilityFlavor RootAnchor, FiniteLabelCode 1),
                    (relationCoordinate CapabilityFlavor (OccurrenceAnchor (OccurrenceId 0)), FiniteLabelCode 8)
                  ],
                Map.fromList
                  [ (relationCoordinate CapabilityFlavor RootAnchor, FiniteLabelCode 3),
                    (relationCoordinate CapabilityFlavor (OccurrenceAnchor (OccurrenceId 0)), FiniteLabelCode 21)
                  ]
              ]
              (fmap sectionBindings sections)
    , testCase "projected enumeration separates structural and capability coordinates" $
        let sections =
              enumerateProjectedSectionsWithSeed
                (sectionCoordinateProjection (separateRelationFlavors (== CapabilityFlavor)))
                (Map.singleton (structuralCoordinate RootAnchor) (ClassLabelCode 3))
                [ EqualityConstraint
                    (ConstraintId 0)
                    RootAnchor
                    (OccurrenceAnchor (OccurrenceId 0))
                    (IntSet.singleton 3),
                  RelationConstraint
                    CapabilityFlavor
                    (ConstraintId 1)
                    [RootAnchor, OccurrenceAnchor (OccurrenceId 0)]
                    [[FiniteLabelCode 1, FiniteLabelCode 2]]
                ]
         in assertEqual
              "structural class bindings coexist with capability bindings"
              [ Map.fromList
                  [ (structuralCoordinate RootAnchor, ClassLabelCode 3),
                    (structuralCoordinate (OccurrenceAnchor (OccurrenceId 0)), ClassLabelCode 3),
                    (relationCoordinate CapabilityFlavor RootAnchor, FiniteLabelCode 1),
                    (relationCoordinate CapabilityFlavor (OccurrenceAnchor (OccurrenceId 0)), FiniteLabelCode 2)
                  ]
              ]
              (fmap sectionBindings sections)
    , testCase "projectSectionCoverage preserves successful coverage" $
        let coverage =
              ( SectionCoverage
                { scMatches = [ClassLabelCode 5],
                  scLoweringGaps = []
                } ::
                  SectionCoverage ExactLabelCode ConstraintId
              )
         in assertEqual
              "successful coverage passes through untouched"
              (Right coverage)
              (projectSectionCoverage (\_ gapValue -> gapValue) coverage)
    , testCase "projectSectionCoverage selects the first lowering gap" $
        let coverage =
              ( SectionCoverage
                { scMatches = [],
                  scLoweringGaps = [ConstraintId 3, ConstraintId 5]
                } ::
                  SectionCoverage ExactLabelCode ConstraintId
              )
         in do
              assertEqual
                "first lowering gap drives the compatibility projection"
                (Left (ConstraintId 3))
                (projectSectionCoverage (\_ gapValue -> gapValue) coverage)
              assertEqual
                "first lowering gap is exposed directly"
                (Just (ConstraintId 3))
                (firstSectionCoverageGap coverage)
    , testCase "projectSectionCoverageWith reifies successful coverage" $
        let coverage =
              ( SectionCoverage
                  { scMatches = [ClassLabelCode 2, ClassLabelCode 5],
                    scLoweringGaps = []
                  } ::
                  SectionCoverage ExactLabelCode ConstraintId
              )
         in assertEqual
              "successful coverage can project to an arbitrary accepted value"
              (Right 2)
              (projectSectionCoverageWith (\_ gapValue -> gapValue) (length . scMatches) coverage)
    , testCase "sectionCoverageFeasibility accepts non-empty gap-free support" $
        let coverage =
              ( SectionCoverage
                  { scMatches = [ClassLabelCode 2, ClassLabelCode 5],
                    scLoweringGaps = []
                  } ::
                  SectionCoverage ExactLabelCode ConstraintId
              )
         in assertEqual
              "feasibility requires non-empty support"
              (Right (ClassLabelCode 2 NonEmpty.:| [ClassLabelCode 5]))
              (sectionCoverageFeasibility coverage)
    , testCase "sectionCoverageFeasibility rejects lowering gaps before support emptiness" $
        let coverage =
              ( SectionCoverage
                  { scMatches = [ClassLabelCode 2],
                    scLoweringGaps = [ConstraintId 3]
                  } ::
                  SectionCoverage ExactLabelCode ConstraintId
              )
         in assertEqual
              "first lowering gap drives infeasibility"
              (Left (CoverageGap (ConstraintId 3)))
              (sectionCoverageFeasibility coverage)
    , testCase "sectionCoverageFeasibility rejects gap-free empty support" $
        let coverage =
              ( SectionCoverage
                  { scMatches = [],
                    scLoweringGaps = []
                  } ::
                  SectionCoverage ExactLabelCode ConstraintId
              )
         in assertEqual
              "empty support is possibilistically infeasible"
              (Left EmptySupport)
              (sectionCoverageFeasibility coverage)
    , testCase "foldSectionCoverage exposes all lowering gaps to the reject branch" $
        let coverage =
              ( SectionCoverage
                  { scMatches = [ClassLabelCode 2],
                    scLoweringGaps = [ConstraintId 3, ConstraintId 5]
                  } ::
                  SectionCoverage ExactLabelCode ConstraintId
              )
         in assertEqual
              "reject branch sees the full non-empty gap list"
              (Left [ConstraintId 3, ConstraintId 5])
              ( foldSectionCoverage
                  (const ([] :: [ConstraintId]))
                  (\_ gapValues -> NonEmpty.toList gapValues)
                  coverage
              )
    , testCase "mapSectionCoverageGaps relabels lowering diagnostics without touching matches" $
        let coverage =
              ( SectionCoverage
                  { scMatches = [ClassLabelCode 2],
                    scLoweringGaps = [ConstraintId 3]
                  } ::
                  SectionCoverage ExactLabelCode ConstraintId
              )
         in assertEqual
              "gap relabeling preserves accepted payloads"
              ( SectionCoverage
                  { scMatches = [ClassLabelCode 2],
                    scLoweringGaps = ["ConstraintId {unConstraintId = 3}"]
                  }
              )
              (mapSectionCoverageGaps show coverage)
    , testCase "enumerateSectionsWithSeed solves arity-3 with only unary domain constraints (no guard)" $
        let domain = IntSet.singleton 42
            sections =
              enumerateSectionsWithSeed
                (Map.singleton (structuralCoordinate RootAnchor) (ClassLabelCode 99))
                [ EqualityConstraint (ConstraintId 0) (OccurrenceAnchor (OccurrenceId 0)) (OccurrenceAnchor (OccurrenceId 0)) domain,
                  EqualityConstraint (ConstraintId 1) (OccurrenceAnchor (OccurrenceId 1)) (OccurrenceAnchor (OccurrenceId 1)) domain,
                  EqualityConstraint (ConstraintId 2) (OccurrenceAnchor (OccurrenceId 2)) (OccurrenceAnchor (OccurrenceId 2)) domain
                ]
         in assertEqual
              "unary-only arity-3 constraints must produce one section"
              [ Map.fromList
                  [ (structuralCoordinate RootAnchor, ClassLabelCode 99),
                    (structuralCoordinate (OccurrenceAnchor (OccurrenceId 0)), ClassLabelCode 42),
                    (structuralCoordinate (OccurrenceAnchor (OccurrenceId 1)), ClassLabelCode 42),
                    (structuralCoordinate (OccurrenceAnchor (OccurrenceId 2)), ClassLabelCode 42)
                  ]
              ]
              (fmap sectionBindings sections)
    , testCase "enumerateProjectedSectionsWithSeed solves arity-3 unary-only with projection" $
        let domain = IntSet.singleton 42
            sections =
              enumerateProjectedSectionsWithSeed
                (sectionCoordinateProjection (separateRelationFlavors (== CapabilityFlavor)))
                (Map.singleton (structuralCoordinate RootAnchor) (ClassLabelCode 99))
                [ EqualityConstraint (ConstraintId 0) (OccurrenceAnchor (OccurrenceId 0)) (OccurrenceAnchor (OccurrenceId 0)) domain,
                  EqualityConstraint (ConstraintId 1) (OccurrenceAnchor (OccurrenceId 1)) (OccurrenceAnchor (OccurrenceId 1)) domain,
                  EqualityConstraint (ConstraintId 2) (OccurrenceAnchor (OccurrenceId 2)) (OccurrenceAnchor (OccurrenceId 2)) domain
                ]
         in assertEqual
              "projected arity-3 unary constraints must produce one section"
              1
              (length sections)
    , testCase "enumerateSectionMatchesWithSeed reifies arity-3 unary-only to result" $
        let domain = IntSet.singleton 42
            sectionMatches =
              enumerateSectionMatchesWithSeed
                (sectionCoordinateProjection (separateRelationFlavors (== CapabilityFlavor)))
                ( sectionReification
                    (\sectionAssignment _ ->
                       sectionBinding (structuralCoordinate RootAnchor) sectionAssignment
                    )
                )
                (Map.singleton (structuralCoordinate RootAnchor) (ClassLabelCode 99))
                (ClassLabelCode 0)
                [ EqualityConstraint (ConstraintId 0) (OccurrenceAnchor (OccurrenceId 0)) (OccurrenceAnchor (OccurrenceId 0)) domain,
                  EqualityConstraint (ConstraintId 1) (OccurrenceAnchor (OccurrenceId 1)) (OccurrenceAnchor (OccurrenceId 1)) domain,
                  EqualityConstraint (ConstraintId 2) (OccurrenceAnchor (OccurrenceId 2)) (OccurrenceAnchor (OccurrenceId 2)) domain
                ]
         in assertEqual
              "arity-3 section match with reification must produce one match"
              1
              (length sectionMatches)
    , testCase "enumerateSectionsWithSeed solves arity-3 equality guard constraints" $
        let domain = IntSet.singleton 42
            sections =
              enumerateSectionsWithSeed
                (Map.singleton (structuralCoordinate RootAnchor) (ClassLabelCode 99))
                [ EqualityConstraint (ConstraintId 0) (OccurrenceAnchor (OccurrenceId 0)) (OccurrenceAnchor (OccurrenceId 0)) domain,
                  EqualityConstraint (ConstraintId 1) (OccurrenceAnchor (OccurrenceId 1)) (OccurrenceAnchor (OccurrenceId 1)) domain,
                  EqualityConstraint (ConstraintId 2) (OccurrenceAnchor (OccurrenceId 2)) (OccurrenceAnchor (OccurrenceId 2)) domain,
                  GuardConstraint (ConstraintId 3) (OccurrenceAnchor (OccurrenceId 0)) (OccurrenceAnchor (OccurrenceId 1)) domain,
                  GuardConstraint (ConstraintId 4) (OccurrenceAnchor (OccurrenceId 1)) (OccurrenceAnchor (OccurrenceId 2)) domain
                ]
         in assertEqual
              "arity-3 diagonal assignment must produce exactly one section"
              [ Map.fromList
                  [ (structuralCoordinate RootAnchor, ClassLabelCode 99),
                    (structuralCoordinate (OccurrenceAnchor (OccurrenceId 0)), ClassLabelCode 42),
                    (structuralCoordinate (OccurrenceAnchor (OccurrenceId 1)), ClassLabelCode 42),
                    (structuralCoordinate (OccurrenceAnchor (OccurrenceId 2)), ClassLabelCode 42)
                  ]
              ]
              (fmap sectionBindings sections)
    , testCase "renderSectionCoverage summarizes counts and first rendered gap" $
        let coverage =
              ( SectionCoverage
                  { scMatches = [ClassLabelCode 2],
                    scLoweringGaps = [ConstraintId 3, ConstraintId 5]
                  } ::
                  SectionCoverage ExactLabelCode ConstraintId
              )
         in assertEqual
              "rendered summary preserves counts and renders the first gap"
              ( SectionCoverageSummary
                  { scsMatchCount = 1,
                    scsGapCount = 2,
                    scsFirstGap = Just "ConstraintId {unConstraintId = 3}"
                  }
              )
              (renderSectionCoverage show coverage)
    ]
