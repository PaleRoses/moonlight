{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Site.Analysis.Microsupport.SheafSubstrateSpec
  ( tests,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Stats (RTSStats (..), getRTSStats, getRTSStatsEnabled)
import Moonlight.Algebra (JoinSemilattice (..), Lattice, MeetSemilattice (..))
import Moonlight.Derived.Morse (MicrosupportResult (..))
import Moonlight.Derived.Site (Criticality (..))
import Moonlight.Derived.Site (FinObjectId (..))
import Moonlight.Sheaf.Presheaf.Core (CompiledRestriction (..), Presheaf (..), restrictAlong)
import Moonlight.Sheaf.Section.Stalk (StalkAlgebra (..), StalkRestrictionKernel (..), mismatchObstruction)
import Moonlight.Sheaf.Sheaf.Gluing
  ( GluingAlgebra (..),
    GluingFailure (..),
    GluingObstruction (..),
    MatchingFamily,
    MatchingFamilyConstructionError,
    MatchingFailure (..),
    amalgamatedStalk,
    amalgamateMatchingFamilyWith,
    compatibleMatchingFamilyUnderlying,
    matchingFamilySections,
    matchingFamilyTarget,
    mkMatchingFamily,
    pairwiseCompatibilityFailures,
  )
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
import Moonlight.Sheaf.Site.Analysis.Microsupport (localMicrosupport)
import Moonlight.Sheaf.Site.Analysis.Microsupport.Footprint
  ( MicrosupportFootprint (..),
    MicrosupportFootprintMeasure (..),
    MicrosupportFootprintReduction,
    MicrosupportMaterializationPlan,
    materializeMicrosupportPlan,
    mfrPrunedFootprint,
    mfrPrunedFibers,
    mfrRetainedFibers,
    mfrRetainedFootprint,
    mfrTotalFootprint,
    microsupportFootprintReduction,
    microsupportMaterializationPlan,
    microsupportPlanFootprintReduction,
    microsupportPlanPrunedNodes,
    microsupportPlanRetainedNodes,
    microsupportStrictlyReducesFootprint,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    CoveringFamily,
    PullbackSquare (..),
    Site (..),
    coveringFamilyFromTargetedWitnesses,
  )
import Moonlight.Sheaf.Site.Plan
  ( EffectiveCoverPlanFailure,
    prepareEffectiveCoverPlan,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
    testCaseSteps,
  )
import Control.DeepSeq (NFData (..), rnf)
import Control.Exception (evaluate)
import System.Mem (performMajorGC)

tests :: TestTree
tests =
  testGroup
    "sheaf substrate microsupport"
    [ testCase "compatible branch cover glues before microlocal footprint pruning" testCompatibleBranchCoverPrunesSheafFootprint,
      testCase "overlap disagreement remains a typed sheaf obstruction before pruning" testOverlapDisagreementBlocksSheafPruning,
      testCaseSteps "substrate profiling starts at the sheaf materialization boundary" testSheafSubstrateProfilingStartsAtMaterialization
    ]

testCompatibleBranchCoverPrunesSheafFootprint :: Assertion
testCompatibleBranchCoverPrunesSheafFootprint = do
  let coverValue = branchRootCover
  matchingFamily <-
    expectRight
      ( mkMatchingFamilyForCover
          branchSite
          coverValue
          (Vector.fromList [branchLeftCompatibleStalk, branchRightCompatibleStalk])
      )
  leftArrow <- expectJust (branchArrow BranchLeft BranchBase)
  rightArrow <- expectJust (branchArrow BranchRight BranchBase)
  case amalgamateMatchingFamilyWith branchCompiledStalkAlgebra (gaAmalgamate branchGluingAlgebra) branchSite matchingFamily of
    Left failure ->
      assertFailure ("expected compatible branch cover to glue before pruning, received " <> show failure)
    Right amalgamationValue -> do
      let gluedStalk = amalgamatedStalk amalgamationValue
      assertEqual
        "compatible local sections glue to the branch sheaf global stalk"
        branchCompatibleAmalgamatedStalk
        gluedStalk
      assertEqual
        "glued branch stalk restricts back to the left local section"
        branchLeftCompatibleStalk
        (restrictAlong branchSite leftArrow gluedStalk)
      assertEqual
        "glued branch stalk restricts back to the right local section"
        branchRightCompatibleStalk
        (restrictAlong branchSite rightArrow gluedStalk)
      case localMicrosupport branchSubstrateSystem of
        Left failure ->
          assertFailure ("branch sheaf substrate microsupport failed: " <> show failure)
        Right microsupportValue -> do
          assertEqual
            "branch cover nerve is contractible, so every sheaf-substrate fiber is noncritical"
            ( MicrosupportCounts
                0
                4
                [ (FinObjectId 3, NonCritical),
                  (FinObjectId 1, NonCritical),
                  (FinObjectId 2, NonCritical),
                  (FinObjectId 0, NonCritical)
                ]
            )
            (microsupportSummary microsupportValue)
          let reduction = microsupportFootprintReduction branchCoverPayloadFootprint microsupportValue
          assertEqual
            "no critical sheaf payload is retained by the microlocal support"
            (MicrosupportFootprint 0)
            (mfrRetainedFootprint reduction)
          assertEqual
            "left, right, and overlap payload are the exact prunable sheaf materialization footprint"
            (MicrosupportFootprint 5)
            (mfrPrunedFootprint reduction)
          assertEqual
            "positive noncritical sheaf payload proves strict substrate footprint reduction"
            True
            (microsupportStrictlyReducesFootprint reduction)

testOverlapDisagreementBlocksSheafPruning :: Assertion
testOverlapDisagreementBlocksSheafPruning = do
  let coverValue = branchRootCover
  matchingFamily <-
    expectRight
      ( mkMatchingFamilyForCover
          branchSite
          coverValue
          (Vector.fromList [branchLeftCompatibleStalk, branchRightIncompatibleStalk])
      )
  case pairwiseCompatibilityFailures branchCompiledStalkAlgebra branchSite matchingFamily of
    [PullbackDisagreement squareValue [BranchCoordinateConflict BranchApex 7 8]] -> do
      assertEqual "left overlap leg lands at the apex" BranchApex (cmSource (psToLeft squareValue))
      assertEqual "right overlap leg lands at the apex" BranchApex (cmSource (psToRight squareValue))
    otherFailures ->
      assertFailure ("expected one typed apex disagreement before pruning, received " <> show otherFailures)
  case amalgamateMatchingFamilyWith branchCompiledStalkAlgebra (gaAmalgamate branchGluingAlgebra) branchSite matchingFamily of
    Left (IncompatibleMatchingFamily (_ :| [])) ->
      pure ()
    Left failure ->
      assertFailure ("expected incompatible matching family, received " <> show failure)
    Right _ ->
      assertFailure "expected sheaf overlap obstruction, received successful amalgamation"

testSheafSubstrateProfilingStartsAtMaterialization :: (String -> IO ()) -> Assertion
testSheafSubstrateProfilingStartsAtMaterialization step =
  case localMicrosupport branchSubstrateSystem of
    Left failure ->
      assertFailure ("branch sheaf substrate microsupport failed: " <> show failure)
    Right microsupportValue -> do
      let planValue =
            microsupportMaterializationPlan
              (MicrosupportFootprintMeasure profiledBranchPayloadFootprint)
              microsupportValue
          reduction = microsupportPlanFootprintReduction planValue
      assertEqual
        "the sheaf substrate candidate materialization footprint is exact"
        (MicrosupportFootprint 10240)
        (mfrTotalFootprint reduction)
      assertEqual
        "the entire profiled branch-cover payload is prunable at the sheaf substrate"
        (MicrosupportFootprint 10240)
        (mfrPrunedFootprint reduction)
      assertEqual
        "microsupport retains no branch-cover payload on a contractible sheaf substrate"
        (MicrosupportFootprint 0)
        (mfrRetainedFootprint reduction)
      assertEqual
        "the retained materialization frontier is empty, so payload construction has no noncritical path"
        []
        (microsupportPlanRetainedNodes planValue)
      assertEqual
        "the pruned frontier records the noncritical sheaf substrate without materializing payloads"
        [FinObjectId 3, FinObjectId 1, FinObjectId 2, FinObjectId 0]
        (microsupportPlanPrunedNodes planValue)
      footprintAllocatedBytes <-
        measureAllocatedBytes
          (forcePure (profiledFootprintBudgetSummary planValue))
      fullAllocatedBytes <-
        measureAllocatedBytes
          (forcePure (materializeFullProfiledPayloads microsupportValue))
      retainedAllocatedBytes <-
        measureAllocatedBytes
          (forcePure (materializeRetainedProfiledPayloads planValue))
      step (footprintBudgetSavingsReport footprintAllocatedBytes fullAllocatedBytes)
      step (allocationSavingsReport fullAllocatedBytes retainedAllocatedBytes)
      assertBool
        ( "expected sheaf footprint budgeting to allocate less than full payload materialization; footprint="
            <> show footprintAllocatedBytes
            <> " full="
            <> show fullAllocatedBytes
        )
        (footprintAllocatedBytes < fullAllocatedBytes)
      assertBool
        ( "expected sheaf-level retained materialization to allocate less than full substrate materialization; full="
            <> show fullAllocatedBytes
            <> " retained="
            <> show retainedAllocatedBytes
        )
        (fullAllocatedBytes > retainedAllocatedBytes)

newtype BranchSubstrateContext = BranchSubstrateContext
  { unBranchSubstrateContext :: BranchContext
  }
  deriving stock (Eq, Ord, Show)

data BranchSubstrateSystem = BranchSubstrateSystem
  deriving stock (Eq, Ord, Show)

data BranchSubstrateTag
  deriving stock (Eq, Ord, Show)

data BranchSubstrateMismatch = BranchSubstrateCompositionMismatch
  deriving stock (Eq, Ord, Show)

branchSubstrateSystem :: BranchSubstrateSystem
branchSubstrateSystem =
  BranchSubstrateSystem

instance JoinSemilattice BranchSubstrateContext where
  join leftContext rightContext =
    BranchSubstrateContext
      (branchJoin (unBranchSubstrateContext leftContext) (unBranchSubstrateContext rightContext))

instance MeetSemilattice BranchSubstrateContext where
  meet leftContext rightContext =
    BranchSubstrateContext
      (branchMeet (unBranchSubstrateContext leftContext) (unBranchSubstrateContext rightContext))

instance Lattice BranchSubstrateContext

instance AnalyzableSystem BranchSubstrateSystem where
  type SystemTag BranchSubstrateSystem = BranchSubstrateTag
  type SystemOb BranchSubstrateSystem = BranchContext
  type SystemMor BranchSubstrateSystem = BranchMorphism
  type SystemCtx BranchSubstrateSystem = BranchSubstrateContext
  type SystemMismatch BranchSubstrateSystem = BranchSubstrateMismatch

  allContexts _ =
    fmap BranchSubstrateContext [BranchApex, BranchLeft, BranchRight, BranchBase]

  contextLeq _ leftContext rightContext =
    branchLeq (unBranchSubstrateContext leftContext) (unBranchSubstrateContext rightContext)

  systemObjectsInContext _ contextValue =
    [unBranchSubstrateContext contextValue]

  systemMorphismsInContext _ _ =
    []

  restrictObject _ sourceContext targetContext objectValue =
    let sourceBranchContext = unBranchSubstrateContext sourceContext
        targetBranchContext = unBranchSubstrateContext targetContext
     in if objectValue == sourceBranchContext && branchLeq targetBranchContext sourceBranchContext
          then Just targetBranchContext
          else Nothing

  restrictMorphism _ sourceContext targetContext morphismValue =
    let sourceBranchContext = unBranchSubstrateContext sourceContext
        targetBranchContext = unBranchSubstrateContext targetContext
     in case morphismValue of
          BranchMorphism morphismSourceContext morphismTargetContext
            | morphismSourceContext == sourceBranchContext,
              morphismTargetContext == sourceBranchContext,
              branchLeq targetBranchContext sourceBranchContext ->
                Just (BranchMorphism targetBranchContext targetBranchContext)
          _ ->
            Nothing

  identityMorphism _ _ objectValue =
    BranchMorphism objectValue objectValue

  morphismSource _ (BranchMorphism sourceContext _) =
    sourceContext

  morphismTarget _ (BranchMorphism _ targetContext) =
    targetContext

  composeMorphisms _ _ leftMorphism rightMorphism =
    case (leftMorphism, rightMorphism) of
      (BranchMorphism leftSource leftTarget, BranchMorphism rightSource rightTarget)
        | rightTarget == leftSource ->
            Right (BranchMorphism rightSource leftTarget)
      _ ->
        Left BranchSubstrateCompositionMismatch

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

instance ContextOrdinalSystem BranchSubstrateSystem where
  contextOrdinal _ =
    fromEnum . unBranchSubstrateContext

instance ContextPresentationSystem BranchSubstrateSystem

data MicrosupportCountsRecord = MicrosupportCounts
  { expectedCriticalCount :: !Int,
    expectedNoncriticalCount :: !Int,
    expectedFibers :: ![(FinObjectId, Criticality)]
  }
  deriving stock (Eq, Show)

microsupportSummary :: MicrosupportResult -> MicrosupportCountsRecord
microsupportSummary microsupportValue =
  MicrosupportCounts
    { expectedCriticalCount = mrCriticalCount microsupportValue,
      expectedNoncriticalCount = mrNoncriticalCount microsupportValue,
      expectedFibers = mrCriticalFibers microsupportValue
    }

branchCoverPayloadFootprint :: FinObjectId -> MicrosupportFootprint
branchCoverPayloadFootprint nodeValue =
  maybe mempty branchStalkFootprint (Map.lookup nodeValue branchCoverPayloadByNode)

branchCoverPayloadByNode :: Map FinObjectId BranchStalk
branchCoverPayloadByNode =
  Map.fromList
    [ (FinObjectId 1, branchLeftCompatibleStalk),
      (FinObjectId 2, branchRightCompatibleStalk),
      (FinObjectId 3, branchStalk [(BranchApex, 7)])
    ]

branchStalkFootprint :: BranchStalk -> MicrosupportFootprint
branchStalkFootprint =
  MicrosupportFootprint . fromIntegral . Map.size . branchStalkEntries

newtype ProfiledBranchPayload = ProfiledBranchPayload
  { profiledBranchPayloadCells :: Map Int Int
  }
  deriving stock (Eq, Show)

instance NFData ProfiledBranchPayload where
  rnf =
    rnf . profiledBranchPayloadCells

profiledBranchPayloadFootprint :: FinObjectId -> MicrosupportFootprint
profiledBranchPayloadFootprint nodeValue =
  MicrosupportFootprint (fromIntegral (Map.findWithDefault 0 nodeValue profiledBranchPayloadWeights))

profiledBranchPayloadWeights :: Map FinObjectId Int
profiledBranchPayloadWeights =
  Map.fromList
    [ (FinObjectId 1, 4096),
      (FinObjectId 2, 4096),
      (FinObjectId 3, 2048)
    ]

type FootprintBudgetSummary = (Integer, Integer, Integer, Int, Int)

profiledFootprintBudgetSummary :: MicrosupportMaterializationPlan -> FootprintBudgetSummary
profiledFootprintBudgetSummary =
  footprintBudgetSummary . microsupportPlanFootprintReduction

footprintBudgetSummary :: MicrosupportFootprintReduction -> FootprintBudgetSummary
footprintBudgetSummary reduction =
  ( footprintBytes (mfrTotalFootprint reduction),
    footprintBytes (mfrRetainedFootprint reduction),
    footprintBytes (mfrPrunedFootprint reduction),
    length (mfrRetainedFibers reduction),
    length (mfrPrunedFibers reduction)
  )

footprintBytes :: MicrosupportFootprint -> Integer
footprintBytes =
  toInteger . unMicrosupportFootprint

materializeFullProfiledPayloads :: MicrosupportResult -> [ProfiledBranchPayload]
materializeFullProfiledPayloads =
  fmap (materializeProfiledPayload . fst) . mrCriticalFibers

materializeRetainedProfiledPayloads :: MicrosupportMaterializationPlan -> [ProfiledBranchPayload]
materializeRetainedProfiledPayloads =
  fmap snd . materializeMicrosupportPlan materializeProfiledPayload

materializeProfiledPayload :: FinObjectId -> ProfiledBranchPayload
materializeProfiledPayload nodeValue@(FinObjectId nodeOrdinal) =
  let cellCount = Map.findWithDefault 0 nodeValue profiledBranchPayloadWeights
   in ProfiledBranchPayload
        ( Map.fromList
            ( fmap
                (\cellIndex -> (cellIndex, nodeOrdinal + cellIndex))
                [1 .. cellCount]
            )
        )

measureAllocatedBytes :: IO () -> IO Word64
measureAllocatedBytes action = do
  performMajorGC
  beforeStats <- requireRtsStats
  action
  performMajorGC
  afterStats <- requireRtsStats
  pure (allocated_bytes afterStats - allocated_bytes beforeStats)

allocationSavingsReport :: Word64 -> Word64 -> String
allocationSavingsReport fullAllocatedBytes retainedAllocatedBytes =
  "sheaf materialization allocation: full="
    <> show fullAllocatedBytes
    <> "B retained="
    <> show retainedAllocatedBytes
    <> "B "
    <> allocationSavingsSuffix fullAllocatedBytes retainedAllocatedBytes

footprintBudgetSavingsReport :: Word64 -> Word64 -> String
footprintBudgetSavingsReport footprintAllocatedBytes fullAllocatedBytes =
  "sheaf footprint budgeting allocation: footprint="
    <> show footprintAllocatedBytes
    <> "B full-materialization-control="
    <> show fullAllocatedBytes
    <> "B "
    <> allocationSavingsSuffix fullAllocatedBytes footprintAllocatedBytes

allocationSavingsSuffix :: Word64 -> Word64 -> String
allocationSavingsSuffix fullAllocatedBytes retainedAllocatedBytes
  | fullAllocatedBytes > retainedAllocatedBytes =
      "saved=" <> show (fullAllocatedBytes - retainedAllocatedBytes) <> "B"
  | otherwise =
      "no allocation reduction"

requireRtsStats :: IO RTSStats
requireRtsStats = do
  enabled <- getRTSStatsEnabled
  if enabled
    then getRTSStats
    else assertFailure "RTS stats are disabled; run the sheaf substrate microsupport test with +RTS -T"

forcePure :: NFData value => value -> IO ()
forcePure =
  evaluate . rnf

mkMatchingFamilyForCover ::
  Site site =>
  site ->
  CoveringFamily (SiteObject site) (SiteMorphism site) ->
  Vector stalk ->
  Either
    (Either (EffectiveCoverPlanFailure (SiteObject site) (SiteMorphism site)) MatchingFamilyConstructionError)
    (MatchingFamily site stalk)
mkMatchingFamilyForCover site coverValue sections = do
  effectiveCover <-
    case prepareEffectiveCoverPlan site coverValue of
      Left failure ->
        Left (Left failure)
      Right planValue ->
        Right planValue
  case mkMatchingFamily effectiveCover sections of
    Left failure ->
      Left (Right failure)
    Right matchingFamily ->
      Right matchingFamily

expectRight :: Show failure => Either failure value -> IO value
expectRight =
  either
    (\failure -> assertFailure ("expected Right, received " <> show failure))
    pure

expectJust :: Maybe value -> IO value
expectJust =
  maybe (assertFailure "expected Just") pure

data BranchContext
  = BranchBase
  | BranchLeft
  | BranchRight
  | BranchApex
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data BranchMorphism = BranchMorphism BranchContext BranchContext
  deriving stock (Eq, Ord, Show)

data BranchSite = BranchSite
  deriving stock (Eq, Ord, Show)

newtype BranchStalk = BranchStalk (Map BranchContext Int)
  deriving stock (Eq, Show)

data BranchMismatch
  = BranchMissingCoordinate !BranchContext !(Maybe Int) !(Maybe Int)
  | BranchCoordinateConflict !BranchContext !Int !Int
  deriving stock (Eq, Show)

branchSite :: BranchSite
branchSite =
  BranchSite

branchContexts :: [BranchContext]
branchContexts =
  [minBound .. maxBound]

branchLeq :: BranchContext -> BranchContext -> Bool
branchLeq leftContext rightContext
  | leftContext == rightContext = True
  | leftContext == BranchBase = True
  | rightContext == BranchApex = True
  | otherwise = False

branchJoin :: BranchContext -> BranchContext -> BranchContext
branchJoin leftContext rightContext
  | branchLeq leftContext rightContext = rightContext
  | branchLeq rightContext leftContext = leftContext
  | otherwise = BranchApex

branchMeet :: BranchContext -> BranchContext -> BranchContext
branchMeet leftContext rightContext
  | branchLeq leftContext rightContext = leftContext
  | branchLeq rightContext leftContext = rightContext
  | otherwise = BranchBase

branchArrow ::
  BranchContext ->
  BranchContext ->
  Maybe (CheckedMorphism BranchContext BranchMorphism)
branchArrow sourceContext targetContext =
  if branchLeq targetContext sourceContext
    then Just (checkedBranchArrow sourceContext targetContext)
    else Nothing

checkedBranchArrow :: BranchContext -> BranchContext -> CheckedMorphism BranchContext BranchMorphism
checkedBranchArrow sourceContext targetContext =
  CheckedMorphism
    { cmSource = sourceContext,
      cmTarget = targetContext,
      cmWitness = BranchMorphism sourceContext targetContext
    }

branchRootCover ::
  CoveringFamily BranchContext BranchMorphism
branchRootCover =
  coveringFamilyFromTargetedWitnesses
    BranchBase
    ( (BranchLeft, BranchMorphism BranchLeft BranchBase)
        :| [(BranchRight, BranchMorphism BranchRight BranchBase)]
    )

singletonCover ::
  BranchContext ->
  BranchContext ->
  [CoveringFamily BranchContext BranchMorphism]
singletonCover targetContext sourceContext =
  [ coveringFamilyFromTargetedWitnesses
      targetContext
      ((sourceContext, BranchMorphism sourceContext targetContext) :| [])
  ]

branchStalk :: [(BranchContext, Int)] -> BranchStalk
branchStalk =
  BranchStalk . Map.fromList

branchStalkEntries :: BranchStalk -> Map BranchContext Int
branchStalkEntries (BranchStalk entries) =
  entries

branchLeftCompatibleStalk :: BranchStalk
branchLeftCompatibleStalk =
  branchStalk [(BranchLeft, 10), (BranchApex, 7)]

branchRightCompatibleStalk :: BranchStalk
branchRightCompatibleStalk =
  branchStalk [(BranchRight, 20), (BranchApex, 7)]

branchRightIncompatibleStalk :: BranchStalk
branchRightIncompatibleStalk =
  branchStalk [(BranchRight, 20), (BranchApex, 8)]

branchCompatibleAmalgamatedStalk :: BranchStalk
branchCompatibleAmalgamatedStalk =
  branchStalk [(BranchLeft, 10), (BranchRight, 20), (BranchApex, 7)]

instance Site BranchSite where
  type SiteObject BranchSite = BranchContext
  type SiteMorphism BranchSite = BranchMorphism

  siteObjects _ =
    branchContexts

  siteMorphisms _ =
    mapMaybe
      (uncurry branchArrow)
      ((,) <$> branchContexts <*> branchContexts)

  identityAt _ contextValue =
    checkedBranchArrow contextValue contextValue

  coversAt _ contextValue =
    case contextValue of
      BranchBase ->
        [branchRootCover]
      BranchLeft ->
        singletonCover BranchLeft BranchApex
      BranchRight ->
        singletonCover BranchRight BranchApex
      BranchApex ->
        []

  composeChecked _ outerMorphism innerMorphism =
    if cmTarget innerMorphism == cmSource outerMorphism
      then branchArrow (cmSource innerMorphism) (cmTarget outerMorphism)
      else Nothing

  pullbackPair _ leftMorphism rightMorphism =
    if cmTarget leftMorphism == cmTarget rightMorphism
      then
        let apexContext = branchJoin (cmSource leftMorphism) (cmSource rightMorphism)
         in Just
              PullbackSquare
                { psLeftBase = leftMorphism,
                  psRightBase = rightMorphism,
                  psApex = apexContext,
                  psToLeft = checkedBranchArrow apexContext (cmSource leftMorphism),
                  psToRight = checkedBranchArrow apexContext (cmSource rightMorphism)
                }
      else Nothing

instance Presheaf BranchSite BranchStalk where
  restrictAlong _ morphismValue (BranchStalk entries) =
    BranchStalk
      ( Map.filterWithKey
          (\contextValue _ -> branchLeq (cmSource morphismValue) contextValue)
          entries
      )

branchStalkAlgebra :: StalkAlgebra (CheckedMorphism BranchContext BranchMorphism) BranchStalk BranchMismatch ()
branchStalkAlgebra =
  StalkAlgebra
    { saRestrictionKernel = StalkRestrictionMap . restrictAlong branchSite,
      saMismatches =
        \(BranchStalk leftEntries) (BranchStalk rightEntries) ->
          mapMaybe
            (branchMismatchAt leftEntries rightEntries)
            (Map.keys (Map.union leftEntries rightEntries)),
      saMerge =
        \leftStalk@(BranchStalk leftEntries) rightStalk@(BranchStalk rightEntries) ->
          case mismatchObstruction (branchMergeMismatches leftStalk rightStalk) of
            Just obstruction -> Left obstruction
            Nothing -> Right (BranchStalk (Map.union leftEntries rightEntries)),
      saRepair = const (Left ()),
      saNormalize = id
    }

branchCompiledStalkAlgebra :: StalkAlgebra (CompiledRestriction BranchSite) BranchStalk BranchMismatch ()
branchCompiledStalkAlgebra =
  StalkAlgebra
    { saRestrictionKernel = \restrictionValue -> StalkRestrictionMap (restrictAlong (crSite restrictionValue) (crMorphism restrictionValue)),
      saMismatches = saMismatches branchStalkAlgebra,
      saMerge = saMerge branchStalkAlgebra,
      saRepair = const (Left ()),
      saNormalize = saNormalize branchStalkAlgebra
    }

branchMergeMismatches :: BranchStalk -> BranchStalk -> [BranchMismatch]
branchMergeMismatches (BranchStalk leftEntries) (BranchStalk rightEntries) =
  mapMaybe
    (branchMergeConflictAt leftEntries rightEntries)
    (Map.keys (Map.intersection leftEntries rightEntries))

branchMismatchAt ::
  Map BranchContext Int ->
  Map BranchContext Int ->
  BranchContext ->
  Maybe BranchMismatch
branchMismatchAt leftEntries rightEntries contextValue =
  case (Map.lookup contextValue leftEntries, Map.lookup contextValue rightEntries) of
    (Just leftValue, Just rightValue)
      | leftValue == rightValue ->
          Nothing
      | otherwise ->
          Just (BranchCoordinateConflict contextValue leftValue rightValue)
    (Nothing, Nothing) ->
      Nothing
    (leftValue, rightValue) ->
      Just (BranchMissingCoordinate contextValue leftValue rightValue)

branchMergeConflictAt ::
  Map BranchContext Int ->
  Map BranchContext Int ->
  BranchContext ->
  Maybe BranchMismatch
branchMergeConflictAt leftEntries rightEntries contextValue =
  case (Map.lookup contextValue leftEntries, Map.lookup contextValue rightEntries) of
    (Just leftValue, Just rightValue)
      | leftValue /= rightValue ->
          Just (BranchCoordinateConflict contextValue leftValue rightValue)
    _ ->
      Nothing

branchGluingAlgebra :: GluingAlgebra BranchSite BranchStalk Void
branchGluingAlgebra =
  GluingAlgebra
    { gaAmalgamate = \_ compatibleFamily ->
        let matchingFamily = compatibleMatchingFamilyUnderlying compatibleFamily
         in case matchingFamilyTarget matchingFamily of
              BranchBase ->
                Right
                  ( BranchStalk
                      (Map.unions (fmap branchStalkEntries (Vector.toList (matchingFamilySections matchingFamily))))
                  )
              _ ->
                Left (GluingUnavailable (matchingFamilyTarget matchingFamily))
    }
