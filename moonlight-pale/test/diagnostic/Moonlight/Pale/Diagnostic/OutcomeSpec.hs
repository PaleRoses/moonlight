module Moonlight.Pale.Diagnostic.OutcomeSpec
  ( tests,
  )
where

import Data.Set qualified as Set
import Moonlight.Pale.Diagnostic.Gluing.Algebra
  ( outcomeSummaryFromProjectionOutcome,
    outcomeSummaryFromRestrictionOutcome,
    summarizeRestrictionOutcomes,
    topRestrictionHotspots,
  )
import Moonlight.Pale.Diagnostic.Gluing.Propagation (OutcomeSummary)
import Moonlight.Pale.Diagnostic.Section.Propagation
  ( ProjectionRunOutcome (..),
    RestrictionOutcomeStat (..),
    RestrictionRunOutcome (..),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

type Cell = String

type ProjectionKey = String

type ProjectionValue = String

type ProjectionFailure = String

type Diagnostic = String

data Mismatch
  = ContextMismatch
  | PhaseMismatch
  | ShapeMismatch
  deriving stock (Eq, Ord, Show)

tests :: TestTree
tests =
  testGroup
    "pale.diagnostic.outcome"
    [ testCase "OutcomeSummary has mempty as a left identity" $
        assertEqual
          "left identity"
          outcomeSummaryA
          (mempty <> outcomeSummaryA),
      testCase "OutcomeSummary has mempty as a right identity" $
        assertEqual
          "right identity"
          outcomeSummaryA
          (outcomeSummaryA <> mempty),
      testCase "OutcomeSummary composition is associative" $
        assertEqual
          "associativity"
          ((outcomeSummaryA <> outcomeSummaryB) <> outcomeSummaryC)
          (outcomeSummaryA <> (outcomeSummaryB <> outcomeSummaryC)),
      testCase "restriction hotspot fold ranks structural mismatch membership" $
        assertEqual
          "ranked hotspot structure"
          [ ("alpha", "omega", PhaseMismatch),
            ("beta", "omega", ShapeMismatch)
          ]
          (hotspotKey <$> topRestrictionHotspots 2 (summarizeRestrictionOutcomes knownRestrictionOutcomes))
    ]

outcomeSummaryA :: OutcomeSummary Cell Mismatch ProjectionKey ProjectionValue ProjectionFailure Diagnostic
outcomeSummaryA =
  outcomeSummaryFromProjectionOutcome projectionAppliedA

outcomeSummaryB :: OutcomeSummary Cell Mismatch ProjectionKey ProjectionValue ProjectionFailure Diagnostic
outcomeSummaryB =
  outcomeSummaryFromProjectionOutcome projectionSkippedB

outcomeSummaryC :: OutcomeSummary Cell Mismatch ProjectionKey ProjectionValue ProjectionFailure Diagnostic
outcomeSummaryC =
  outcomeSummaryFromRestrictionOutcome restrictionC

projectionAppliedA :: ProjectionRunOutcome Cell ProjectionKey ProjectionValue ProjectionFailure Diagnostic
projectionAppliedA =
  ProjectionApplied "project-alpha" (Set.fromList ["alpha", "beta"]) "projected" 0.25 ["alpha adjusted"]

projectionSkippedB :: ProjectionRunOutcome Cell ProjectionKey ProjectionValue ProjectionFailure Diagnostic
projectionSkippedB =
  ProjectionSkipped "project-beta" "already stable"

restrictionC :: RestrictionRunOutcome Cell Mismatch
restrictionC =
  RestrictionMismatch "beta" "omega" [ShapeMismatch]

knownRestrictionOutcomes :: [RestrictionRunOutcome Cell Mismatch]
knownRestrictionOutcomes =
  [ RestrictionMismatch "alpha" "omega" [PhaseMismatch, PhaseMismatch, PhaseMismatch],
    RestrictionMismatch "beta" "omega" [ShapeMismatch, ShapeMismatch],
    RestrictionMismatch "gamma" "omega" [ContextMismatch]
  ]

hotspotKey :: RestrictionOutcomeStat Cell Mismatch -> (Cell, Cell, Mismatch)
hotspotKey stat =
  (rosSourceCell stat, rosTargetCell stat, rosMismatch stat)
