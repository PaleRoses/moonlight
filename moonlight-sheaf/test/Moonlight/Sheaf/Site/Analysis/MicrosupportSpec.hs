{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Site.Analysis.MicrosupportSpec
  ( tests,
  )
where

import Data.Bits ((.&.), (.|.))
import Data.Kind (Type)
import Data.Set qualified as Set
import Moonlight.Algebra (JoinSemilattice (..), Lattice, MeetSemilattice (..))
import Moonlight.Derived.Morse (MicrosupportResult (..))
import Moonlight.Derived.Site (Criticality (..))
import Moonlight.Derived.Site (FinObjectId (..))
import Moonlight.Sheaf.Site.Analysis.Microsupport
  ( localMicrosupport,
    localMicrosupportForContexts,
    localMicrosupportFromGenerators,
    localMicrosupportPairwiseMeets,
  )
import Moonlight.Sheaf.Site.Analysis.Microsupport.Footprint
  ( MicrosupportFootprint (..),
    MicrosupportFootprintMeasure (..),
    MicrosupportFootprintReduction (..),
    materializeMicrosupportPlan,
    microsupportFootprintReduction,
    microsupportMaterializationPlan,
    microsupportPlanPrunedNodes,
    microsupportPlanRetainedNodes,
    microsupportStrictlyReducesFootprint,
    microsupportUnitFootprintReduction,
  )
import Moonlight.Sheaf.Site.Context.GeneratorCover (ContextGeneratorCover (..))
import Moonlight.Homology (HomologyFailure)
import Moonlight.Sheaf.Site.Context.Presentation
  ( ContextPresentationSystem,
  )
import Moonlight.Sheaf.Site.Interface.Types
  ( InterfaceDirectionEstimate (..),
    MorphismInterface (..),
  )
import Moonlight.Sheaf.Site.System
  ( AnalyzableSystem (..),
    ContextOrdinalSystem (..),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "parametric microsupport"
    [ testCase "explicit contexts classify one contractible fiber as noncritical" testSingleContractibleFiber,
      testCase "explicit contexts expose the critical top fiber and exact pruning count" testExplicitBranchingCounts,
      testCase "generator cover delegates to the same parametric microsupport kernel" testGeneratorCoverMatchesExplicitContexts,
      testCase "pairwise meet closure exposes exact critical and prunable fibers" testPairwiseMeetClosureCounts,
      testCase "full presentation agrees with explicit meet-closed contexts" testFullPresentationMatchesExplicitMeetClosure,
      testCase "materialization plan exposes only critical payload demand" testMaterializationPlanOnlyDemandsCriticalFibers,
      testCase "unit footprint proof records exact microsupport reduction" testUnitFootprintReduction,
      testCase "weighted footprint proof is monotone and strict exactly when noncritical weight is positive" testWeightedFootprintReduction
    ]

testSingleContractibleFiber :: Assertion
testSingleContractibleFiber =
  assertMicrosupport
    "single contractible generator"
    (MicrosupportCounts 0 1 [(FinObjectId 1, NonCritical)])
    (localMicrosupportForContexts singleContextSystem [ctxA])

testExplicitBranchingCounts :: Assertion
testExplicitBranchingCounts =
  assertMicrosupport
    "branching generator basis"
    ( MicrosupportCounts
        1
        3
        [ (FinObjectId 7, Critical),
          (FinObjectId 3, NonCritical),
          (FinObjectId 5, NonCritical),
          (FinObjectId 6, NonCritical)
        ]
    )
    (localMicrosupportForContexts branchingSystem branchingGenerators)

testGeneratorCoverMatchesExplicitContexts :: Assertion
testGeneratorCoverMatchesExplicitContexts =
  assertEqual
    "generator-cover microsupport must be just the explicit-context kernel with the declared generators"
    (microsupportSummary <$> localMicrosupportForContexts branchingSystem branchingGenerators)
    (microsupportSummary <$> localMicrosupportFromGenerators branchingSystem)

testPairwiseMeetClosureCounts :: Assertion
testPairwiseMeetClosureCounts =
  assertMicrosupport
    "pairwise meet closure"
    ( MicrosupportCounts
        4
        3
        [ (FinObjectId 7, Critical),
          (FinObjectId 3, Critical),
          (FinObjectId 5, Critical),
          (FinObjectId 6, Critical),
          (FinObjectId 1, NonCritical),
          (FinObjectId 2, NonCritical),
          (FinObjectId 4, NonCritical)
        ]
    )
    (localMicrosupportPairwiseMeets branchingSystem)

testFullPresentationMatchesExplicitMeetClosure :: Assertion
testFullPresentationMatchesExplicitMeetClosure =
  assertEqual
    "full context presentation should not add e-graph-introspection-only semantics"
    (microsupportSummary <$> localMicrosupportForContexts meetClosedSystem meetClosedContexts)
    (microsupportSummary <$> localMicrosupport meetClosedSystem)

testMaterializationPlanOnlyDemandsCriticalFibers :: Assertion
testMaterializationPlanOnlyDemandsCriticalFibers =
  case localMicrosupportPairwiseMeets branchingSystem of
    Left failure ->
      assertFailure ("pairwise microsupport failed: " <> show failure)
    Right microsupportValue -> do
      let planValue =
            microsupportMaterializationPlan
              (MicrosupportFootprintMeasure (\(FinObjectId nodeOrdinal) -> MicrosupportFootprint (fromIntegral nodeOrdinal)))
              microsupportValue
      assertEqual
        "retained materialization frontier is exactly the critical microsupport"
        [FinObjectId 7, FinObjectId 3, FinObjectId 5, FinObjectId 6]
        (microsupportPlanRetainedNodes planValue)
      assertEqual
        "pruned materialization frontier is exactly the noncritical complement"
        [FinObjectId 1, FinObjectId 2, FinObjectId 4]
        (microsupportPlanPrunedNodes planValue)
      assertEqual
        "payload materialization is requested only for retained critical fibers"
        [(FinObjectId 7, 7), (FinObjectId 3, 3), (FinObjectId 5, 5), (FinObjectId 6, 6)]
        (materializeMicrosupportPlan (\(FinObjectId nodeOrdinal) -> nodeOrdinal) planValue)

testUnitFootprintReduction :: Assertion
testUnitFootprintReduction =
  case localMicrosupportForContexts branchingSystem branchingGenerators of
    Left failure ->
      assertFailure ("branching microsupport failed: " <> show failure)
    Right microsupportValue -> do
      let reduction = microsupportUnitFootprintReduction microsupportValue
      assertEqual
        "total unit footprint"
        (MicrosupportFootprint 4)
        (mfrTotalFootprint reduction)
      assertEqual
        "retained unit footprint"
        (MicrosupportFootprint 1)
        (mfrRetainedFootprint reduction)
      assertEqual
        "pruned unit footprint"
        (MicrosupportFootprint 3)
        (mfrPrunedFootprint reduction)
      assertEqual
        "retained fibers are exactly the critical microsupport"
        [(FinObjectId 7, MicrosupportFootprint 1)]
        (mfrRetainedFibers reduction)
      assertEqual
        "pruned fibers are exactly the noncritical complement"
        [(FinObjectId 3, MicrosupportFootprint 1), (FinObjectId 5, MicrosupportFootprint 1), (FinObjectId 6, MicrosupportFootprint 1)]
        (mfrPrunedFibers reduction)
      assertEqual
        "positive noncritical footprint proves strict footprint reduction"
        True
        (microsupportStrictlyReducesFootprint reduction)

testWeightedFootprintReduction :: Assertion
testWeightedFootprintReduction =
  case localMicrosupportPairwiseMeets branchingSystem of
    Left failure ->
      assertFailure ("pairwise microsupport failed: " <> show failure)
    Right microsupportValue -> do
      let reduction =
            microsupportFootprintReduction
              (\(FinObjectId nodeOrdinal) -> MicrosupportFootprint (fromIntegral nodeOrdinal))
              microsupportValue
      assertEqual
        "total weighted footprint"
        (MicrosupportFootprint 28)
        (mfrTotalFootprint reduction)
      assertEqual
        "retained weighted footprint"
        (MicrosupportFootprint 21)
        (mfrRetainedFootprint reduction)
      assertEqual
        "pruned weighted footprint"
        (MicrosupportFootprint 7)
        (mfrPrunedFootprint reduction)
      assertEqual
        "footprint reduction is strict exactly because noncritical cells carry positive weight"
        True
        (microsupportStrictlyReducesFootprint reduction)

type MicrosupportCounts :: Type
data MicrosupportCounts = MicrosupportCounts
  { expectedCriticalCount :: !Int,
    expectedNoncriticalCount :: !Int,
    expectedFibers :: ![(FinObjectId, Criticality)]
  }
  deriving stock (Eq, Show)

assertMicrosupport :: String -> MicrosupportCounts -> Either HomologyFailure MicrosupportResult -> Assertion
assertMicrosupport label expectedCounts resultValue =
  case resultValue of
    Left failure ->
      assertFailure (label <> " microsupport failed: " <> show failure)
    Right microsupportValue ->
      assertEqual label expectedCounts (microsupportSummary microsupportValue)

microsupportSummary :: MicrosupportResult -> MicrosupportCounts
microsupportSummary microsupportValue =
  MicrosupportCounts
    { expectedCriticalCount = mrCriticalCount microsupportValue,
      expectedNoncriticalCount = mrNoncriticalCount microsupportValue,
      expectedFibers = mrCriticalFibers microsupportValue
    }

type FiniteMicrosupportSystem :: Type
data FiniteMicrosupportSystem = FiniteMicrosupportSystem
  { finiteContexts :: ![MicroContext],
    finiteGenerators :: ![MicroContext]
  }
  deriving stock (Eq, Ord, Show)

type MicroContext :: Type
newtype MicroContext = MicroContext
  { microContextBits :: Int
  }
  deriving stock (Eq, Ord, Show)

type MicroObject :: Type
data MicroObject = MicroObject
  deriving stock (Eq, Ord, Show)

type MicroMorphism :: Type
data MicroMorphism = MicroIdentity
  deriving stock (Eq, Ord, Show)

type MicroTag :: Type
data MicroTag
  deriving stock (Eq, Ord, Show)

type MicroMismatch :: Type
data MicroMismatch
  deriving stock (Eq, Ord, Show)

instance JoinSemilattice MicroContext where
  join leftContext rightContext =
    MicroContext (microContextBits leftContext .|. microContextBits rightContext)

instance MeetSemilattice MicroContext where
  meet leftContext rightContext =
    MicroContext (microContextBits leftContext .&. microContextBits rightContext)

instance Lattice MicroContext

instance AnalyzableSystem FiniteMicrosupportSystem where
  type SystemTag FiniteMicrosupportSystem = MicroTag
  type SystemOb FiniteMicrosupportSystem = MicroObject
  type SystemMor FiniteMicrosupportSystem = MicroMorphism
  type SystemCtx FiniteMicrosupportSystem = MicroContext
  type SystemMismatch FiniteMicrosupportSystem = MicroMismatch

  allContexts =
    finiteContexts

  contextLeq _ leftContext rightContext =
    (microContextBits leftContext .&. microContextBits rightContext) == microContextBits leftContext

  systemObjectsInContext _ _ =
    [MicroObject]

  systemMorphismsInContext _ _ =
    []

  restrictObject systemValue sourceContext targetContext objectValue =
    if contextLeq systemValue targetContext sourceContext
      then Just objectValue
      else Nothing

  restrictMorphism systemValue sourceContext targetContext morphismValue =
    if contextLeq systemValue targetContext sourceContext
      then Just morphismValue
      else Nothing

  identityMorphism _ _ _ =
    MicroIdentity

  morphismSource _ _ =
    MicroObject

  morphismTarget _ _ =
    MicroObject

  composeMorphisms _ _ MicroIdentity MicroIdentity =
    Right MicroIdentity

  morphismInterface _ _ =
    MorphismInterface
      { miBoundNames = Set.empty,
        miDeletedNames = Set.empty,
        miCreatedNames = Set.empty,
        miGuarded = False,
        miDirectionEstimate = InterfaceDirectionEstimate 0
      }

  normalizeMorphism _ _ =
    id

instance ContextOrdinalSystem FiniteMicrosupportSystem where
  contextOrdinal _ =
    microContextBits

instance ContextPresentationSystem FiniteMicrosupportSystem

instance ContextGeneratorCover FiniteMicrosupportSystem where
  contextGenerators =
    finiteGenerators

  contextIsBottom _ contextValue =
    microContextBits contextValue == 0

singleContextSystem :: FiniteMicrosupportSystem
singleContextSystem =
  FiniteMicrosupportSystem
    { finiteContexts = [ctxA],
      finiteGenerators = [ctxA]
    }

branchingSystem :: FiniteMicrosupportSystem
branchingSystem =
  FiniteMicrosupportSystem
    { finiteContexts = branchingGenerators,
      finiteGenerators = branchingGenerators
    }

meetClosedSystem :: FiniteMicrosupportSystem
meetClosedSystem =
  FiniteMicrosupportSystem
    { finiteContexts = meetClosedContexts,
      finiteGenerators = branchingGenerators
    }

branchingGenerators :: [MicroContext]
branchingGenerators =
  [ctxABC, ctxAB, ctxAC, ctxBC]

meetClosedContexts :: [MicroContext]
meetClosedContexts =
  [ctxABC, ctxAB, ctxAC, ctxBC, ctxA, ctxB, ctxC]

ctxA :: MicroContext
ctxA =
  MicroContext 1

ctxB :: MicroContext
ctxB =
  MicroContext 2

ctxC :: MicroContext
ctxC =
  MicroContext 4

ctxAB :: MicroContext
ctxAB =
  join ctxA ctxB

ctxAC :: MicroContext
ctxAC =
  join ctxA ctxC

ctxBC :: MicroContext
ctxBC =
  join ctxB ctxC

ctxABC :: MicroContext
ctxABC =
  join ctxAB ctxC
