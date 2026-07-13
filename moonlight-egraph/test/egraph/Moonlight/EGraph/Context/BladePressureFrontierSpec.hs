{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Context.BladePressureFrontierSpec
  ( tests,
  )
where

import Moonlight.EGraph.Effect.CoveringSurface (SurfaceKind)
import Moonlight.Core (ZipMatch (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Word (Word64)
import GHC.TypeLits (Symbol)
import Moonlight.Algebra (JoinSemilattice (..))
import Moonlight.FiniteLattice (singletonContextLattice)
import Moonlight.Core (emptyTheorySpec)
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec, semilatticeAnalysis)
import Moonlight.EGraph.Pure.Context (emptyContextEGraph)
import Moonlight.EGraph.Pure.Extraction (AnalysisCostAlgebra (..), ExtractionResult (..))
import Moonlight.EGraph.Pure.Saturation.Front
  ( EGraphFront,
    EGraphFrontError,
    FrontPhase (Authored),
    RulesetM,
    SaturationBudget (..),
    Term,
    def,
    efrResult,
    egraph,
    extract,
    frontErrorMessage,
    node,
    rewrite,
    ruleset,
    run,
    runEGraphFront,
    runFor,
    (==>),
  )
import Moonlight.EGraph.Pure.Saturation.Front.PackedNode
  ( PackedNode,
    packAnalysisSpec,
  )
import Moonlight.EGraph.Pure.Types (emptyEGraphWithTheory)
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingContextEGraph,
    emptySaturatingContextEGraph,
  )
import Data.Fix (Fix (..))
import Moonlight.Rewrite.DSL
  ( HTraversable (..),
    K (..),
    Node (..),
    RewriteSignature (..),
    SortWitness (..),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

data BladeSeam
  = AbyssRidge
  | OrchardRoad
  | ArchiveGate
  deriving stock (Eq, Ord, Show)

data CyclePatch
  = PatchA
  | PatchB
  | PatchC
  deriving stock (Eq, Ord, Show)

data CycleOverlap
  = OverlapAB
  | OverlapBC
  | OverlapCA
  deriving stock (Eq, Ord, Show)

data SeamValue
  = SeamRed
  | SeamBlue
  deriving stock (Eq, Ord, Show)

data PatchVariant
  = LocalSection
  | RepairSection
  deriving stock (Eq, Ord, Show)

data CycleRepairPlan
  = GreedyLocal
  | RepairPatchA
  | RepairPatchB
  | RepairPatchC
  deriving stock (Eq, Ord, Show)

data BladeDecision
  = ExtractMerge !BladeSeam
  | ExtractFrontier !BladeSeam
  deriving stock (Eq, Show)

newtype PressureCost = PressureCost Int
  deriving stock (Eq, Ord, Show)

data GluingCost = GluingCost
  { gcVerdict :: !GluingVerdict,
    gcLocalCost :: !Int,
    gcPlan :: !(Maybe CycleRepairPlan)
  }
  deriving stock (Eq, Show)

data GluingVerdict
  = GluingFragment
  | GluingPending
  | GluingAccepted
  | GluingRejected !CycleObstruction
  deriving stock (Eq, Show)

data PatchSection = PatchSection
  { psPatch :: !CyclePatch,
    psVariant :: !PatchVariant,
    psRestrictions :: !(Map CycleOverlap SeamValue),
    psLocalCost :: !Int
  }
  deriving stock (Eq, Show)

data CycleDescentVerdict
  = CycleAccepted
  | CycleRejected !CycleObstruction
  deriving stock (Eq, Show)

data CycleObstruction
  = OverlapMismatch !CycleOverlap !CyclePatch !SeamValue !CyclePatch !SeamValue
  | MissingRestriction !CyclePatch !CycleOverlap
  deriving stock (Eq, Ord, Show)

data SeamObservation = SeamObservation
  { soMergeAffinity :: !Int,
    soBoundaryDrama :: !Int,
    soCompatibilityDissonance :: !Int,
    soBundlePressure :: !Int
  }
  deriving stock (Eq, Show)

data BladeChoiceTag
  = RootTag
  | UnitTag
  | UndecidedTag !BladeSeam
  | MergeTag !BladeSeam
  | FrontierTag !BladeSeam
  | CycleCoverTag
  | PatchLocalTag !CyclePatch
  | PatchRepairTag !CyclePatch
  | CycleLayoutTag
  | AcceptedLayoutTag !CycleRepairPlan
  | ObstructedLayoutTag !CycleRepairPlan !CycleObstruction
  deriving stock (Eq, Ord, Show)

data BladeChoiceSig (result :: Symbol) r where
  BladeRootNode :: r "Expr" -> BladeChoiceSig "Expr" r
  BladeUnitNode :: BladeChoiceSig "Expr" r
  BladeUndecidedNode :: BladeSeam -> r "Expr" -> BladeChoiceSig "Expr" r
  BladeMergeNode :: BladeSeam -> r "Expr" -> BladeChoiceSig "Expr" r
  BladeFrontierNode :: BladeSeam -> r "Expr" -> BladeChoiceSig "Expr" r
  CycleCoverNode :: BladeChoiceSig "Expr" r
  PatchLocalNode :: CyclePatch -> BladeChoiceSig "Expr" r
  PatchRepairNode :: CyclePatch -> BladeChoiceSig "Expr" r
  CycleLayoutNode :: r "Expr" -> r "Expr" -> r "Expr" -> BladeChoiceSig "Expr" r
  AcceptedLayoutNode :: CycleRepairPlan -> BladeChoiceSig "Expr" r
  ObstructedLayoutNode :: CycleRepairPlan -> CycleObstruction -> BladeChoiceSig "Expr" r

instance HTraversable BladeChoiceSig where
  htraverseWithSort transform =
    \case
      BladeRootNode child ->
        BladeRootNode <$> transform SortWitness child
      BladeUnitNode ->
        pure BladeUnitNode
      BladeUndecidedNode seam child ->
        BladeUndecidedNode seam <$> transform SortWitness child
      BladeMergeNode seam child ->
        BladeMergeNode seam <$> transform SortWitness child
      BladeFrontierNode seam child ->
        BladeFrontierNode seam <$> transform SortWitness child
      CycleCoverNode ->
        pure CycleCoverNode
      PatchLocalNode patch ->
        pure (PatchLocalNode patch)
      PatchRepairNode patch ->
        pure (PatchRepairNode patch)
      CycleLayoutNode patchA patchB patchC ->
        CycleLayoutNode
          <$> transform SortWitness patchA
          <*> transform SortWitness patchB
          <*> transform SortWitness patchC
      AcceptedLayoutNode plan ->
        pure (AcceptedLayoutNode plan)
      ObstructedLayoutNode plan obstruction ->
        pure (ObstructedLayoutNode plan obstruction)

instance RewriteSignature BladeChoiceSig where
  type NodeTag BladeChoiceSig = BladeChoiceTag

  nodeTag =
    \case
      BladeRootNode {} -> RootTag
      BladeUnitNode -> UnitTag
      BladeUndecidedNode seam _ -> UndecidedTag seam
      BladeMergeNode seam _ -> MergeTag seam
      BladeFrontierNode seam _ -> FrontierTag seam
      CycleCoverNode -> CycleCoverTag
      PatchLocalNode patch -> PatchLocalTag patch
      PatchRepairNode patch -> PatchRepairTag patch
      CycleLayoutNode {} -> CycleLayoutTag
      AcceptedLayoutNode plan -> AcceptedLayoutTag plan
      ObstructedLayoutNode plan obstruction -> ObstructedLayoutTag plan obstruction

  nodeTagDigest _ =
    \case
      RootTag -> 1
      UnitTag -> 2
      UndecidedTag seam -> 100 + 3 * seamDigest seam
      MergeTag seam -> 101 + 3 * seamDigest seam
      FrontierTag seam -> 102 + 3 * seamDigest seam
      CycleCoverTag -> 290
      PatchLocalTag patch -> 301 + 3 * patchDigest patch
      PatchRepairTag patch -> 302 + 3 * patchDigest patch
      CycleLayoutTag -> 400
      AcceptedLayoutTag plan -> 500 + planDigest plan
      ObstructedLayoutTag plan obstruction -> 600 + 17 * planDigest plan + obstructionDigest obstruction

  nodeResultSort =
    \case
      BladeRootNode {} -> SortWitness
      BladeUnitNode -> SortWitness
      BladeUndecidedNode {} -> SortWitness
      BladeMergeNode {} -> SortWitness
      BladeFrontierNode {} -> SortWitness
      CycleCoverNode -> SortWitness
      PatchLocalNode {} -> SortWitness
      PatchRepairNode {} -> SortWitness
      CycleLayoutNode {} -> SortWitness
      AcceptedLayoutNode {} -> SortWitness
      ObstructedLayoutNode {} -> SortWitness

instance ZipMatch (Node BladeChoiceSig) where
  zipMatch leftNode rightNode =
    case (leftNode, rightNode) of
      (Node (BladeRootNode left), Node (BladeRootNode right)) ->
        Just (Node (BladeRootNode (zipChild left right)))
      (Node BladeUnitNode, Node BladeUnitNode) ->
        Just (Node BladeUnitNode)
      (Node (BladeUndecidedNode leftSeam left), Node (BladeUndecidedNode rightSeam right))
        | leftSeam == rightSeam ->
            Just (Node (BladeUndecidedNode leftSeam (zipChild left right)))
      (Node (BladeMergeNode leftSeam left), Node (BladeMergeNode rightSeam right))
        | leftSeam == rightSeam ->
            Just (Node (BladeMergeNode leftSeam (zipChild left right)))
      (Node (BladeFrontierNode leftSeam left), Node (BladeFrontierNode rightSeam right))
        | leftSeam == rightSeam ->
            Just (Node (BladeFrontierNode leftSeam (zipChild left right)))
      (Node CycleCoverNode, Node CycleCoverNode) ->
        Just (Node CycleCoverNode)
      (Node (PatchLocalNode leftPatch), Node (PatchLocalNode rightPatch))
        | leftPatch == rightPatch ->
            Just (Node (PatchLocalNode leftPatch))
      (Node (PatchRepairNode leftPatch), Node (PatchRepairNode rightPatch))
        | leftPatch == rightPatch ->
            Just (Node (PatchRepairNode leftPatch))
      (Node (CycleLayoutNode leftA leftB leftC), Node (CycleLayoutNode rightA rightB rightC)) ->
        Just
          ( Node
              ( CycleLayoutNode
                  (zipChild leftA rightA)
                  (zipChild leftB rightB)
                  (zipChild leftC rightC)
              )
          )
      (Node (AcceptedLayoutNode leftPlan), Node (AcceptedLayoutNode rightPlan))
        | leftPlan == rightPlan ->
            Just (Node (AcceptedLayoutNode leftPlan))
      (Node (ObstructedLayoutNode leftPlan leftObstruction), Node (ObstructedLayoutNode rightPlan rightObstruction))
        | leftPlan == rightPlan && leftObstruction == rightObstruction ->
            Just (Node (ObstructedLayoutNode leftPlan leftObstruction))
      _ ->
        Nothing
    where
      zipChild :: K child sortLeft -> K child sortRight -> K (child, child) sortResult
      zipChild leftChild rightChild =
        K (unK leftChild, unK rightChild)

newtype ChoiceDepth = ChoiceDepth Int
  deriving stock (Eq, Ord, Show)

instance JoinSemilattice ChoiceDepth where
  join (ChoiceDepth left) (ChoiceDepth right) =
    ChoiceDepth (max left right)

tests :: TestTree
tests =
  testGroup
    "blade pressure frontier egraph"
    [ testCase "extraction cuts the high-pressure seam while merging low-pressure seams" testPressureFrontierExtraction,
      testCase "extraction rejects the locally cheapest cycle and glues the coherent seam layout" testCycleGluingExtraction
    ]

testPressureFrontierExtraction :: Assertion
testPressureFrontierExtraction = do
  report <- expectBladeFront (runEGraphFront pressureFrontierProgram emptyBladeChoiceGraph)
  case efrResult report of
    Nothing ->
      assertFailure "expected blade-choice extraction result"
    Just extractionResult -> do
      erCost extractionResult @?= PressureCost 31
      decodeBladeChoices (erTerm extractionResult)
        @?= Right
          [ ExtractFrontier AbyssRidge,
            ExtractMerge OrchardRoad,
            ExtractMerge ArchiveGate
          ]

testCycleGluingExtraction :: Assertion
testCycleGluingExtraction = do
  planLocalCost GreedyLocal @?= 3
  planLocalCost RepairPatchC @?= 10
  cycleDescentVerdict (layoutForPlan GreedyLocal)
    @?= CycleRejected (OverlapMismatch OverlapCA PatchA SeamRed PatchC SeamBlue)
  cycleDescentVerdict (layoutForPlan RepairPatchC) @?= CycleAccepted
  report <- expectBladeFront (runEGraphFront cycleGluingProgram emptyBladeChoiceGraph)
  case efrResult report of
    Nothing ->
      assertFailure "expected cycle-gluing extraction result"
    Just extractionResult -> do
      erCost extractionResult
        @?= GluingCost
          { gcVerdict = GluingAccepted,
            gcLocalCost = 10,
            gcPlan = Just RepairPatchC
          }
      decodeCycleLayout (erTerm extractionResult) @?= Right RepairPatchC

pressureFrontierProgram ::
  EGraphFront
    'Authored
    BladeChoiceSig
    ChoiceDepth
    ()
    (Maybe (ExtractionResult (Node BladeChoiceSig) PressureCost))
pressureFrontierProgram =
  egraph $ do
    bladeRules <- ruleset @"blade-pressure-frontier" bladeChoiceRules
    start <- def @"candidate-region-cover" pressureFrontierSeed

    run $
      runFor pressureFrontierSaturationBudget bladeRules

    extract @"lowest-pressure-frontier-cost" pressureCostAlgebra start

cycleGluingProgram ::
  EGraphFront
    'Authored
    BladeChoiceSig
    ChoiceDepth
    ()
    (Maybe (ExtractionResult (Node BladeChoiceSig) GluingCost))
cycleGluingProgram =
  egraph $ do
    gluingRules <- ruleset @"cycle-gluing" cycleGluingRules
    start <- def @"cyclic-patch-cover" cycleGluingSeed

    run $
      runFor pressureFrontierSaturationBudget gluingRules

    extract @"best-glued-cycle-layout" gluingCostAlgebra start

bladeChoiceRules :: RulesetM BladeChoiceSig ()
bladeChoiceRules = do
  rewrite @"abyss-ridge-merge" $
    undecided AbyssRidge #tail ==> mergeDecision AbyssRidge #tail
  rewrite @"abyss-ridge-frontier" $
    undecided AbyssRidge #tail ==> frontierDecision AbyssRidge #tail
  rewrite @"orchard-road-merge" $
    undecided OrchardRoad #tail ==> mergeDecision OrchardRoad #tail
  rewrite @"orchard-road-frontier" $
    undecided OrchardRoad #tail ==> frontierDecision OrchardRoad #tail
  rewrite @"archive-gate-merge" $
    undecided ArchiveGate #tail ==> mergeDecision ArchiveGate #tail
  rewrite @"archive-gate-frontier" $
    undecided ArchiveGate #tail ==> frontierDecision ArchiveGate #tail

cycleGluingRules :: RulesetM BladeChoiceSig ()
cycleGluingRules = do
  rewrite @"cover-greedy-local-sections" $
    cycleCover ==> cycleLayout (patchLocal PatchA) (patchLocal PatchB) (patchLocal PatchC)
  rewrite @"cover-patch-a-repair-section" $
    cycleCover ==> cycleLayout (patchRepair PatchA) (patchLocal PatchB) (patchLocal PatchC)
  rewrite @"cover-patch-b-repair-section" $
    cycleCover ==> cycleLayout (patchLocal PatchA) (patchRepair PatchB) (patchLocal PatchC)
  rewrite @"cover-patch-c-repair-section" $
    cycleCover ==> cycleLayout (patchLocal PatchA) (patchLocal PatchB) (patchRepair PatchC)
  rewrite @"greedy-cycle-witnesses-ca-twist" $
    cycleLayout (patchLocal PatchA) (patchLocal PatchB) (patchLocal PatchC)
      ==> obstructedLayout GreedyLocal greedyCycleObstruction
  rewrite @"patch-a-repair-glues-cycle" $
    cycleLayout (patchRepair PatchA) (patchLocal PatchB) (patchLocal PatchC)
      ==> acceptedLayout RepairPatchA
  rewrite @"patch-b-repair-still-obstructs-cycle" $
    cycleLayout (patchLocal PatchA) (patchRepair PatchB) (patchLocal PatchC)
      ==> obstructedLayout RepairPatchB repairPatchBObstruction
  rewrite @"patch-c-repair-glues-cycle" $
    cycleLayout (patchLocal PatchA) (patchLocal PatchB) (patchRepair PatchC)
      ==> acceptedLayout RepairPatchC

pressureFrontierSeed :: Term BladeChoiceSig "Expr"
pressureFrontierSeed =
  bladeRoot
    ( undecided
        AbyssRidge
        ( undecided
            OrchardRoad
            (undecided ArchiveGate bladeUnit)
        )
    )

cycleGluingSeed :: Term BladeChoiceSig "Expr"
cycleGluingSeed =
  cycleCover

pressureCostAlgebra :: AnalysisCostAlgebra (Node BladeChoiceSig) ChoiceDepth PressureCost
pressureCostAlgebra =
  AnalysisCostAlgebra $
    \_analysis ->
      \case
        Node (BladeRootNode (K (_, childCost))) ->
          childCost
        Node BladeUnitNode ->
          PressureCost 0
        Node (BladeUndecidedNode _ (K (_, childCost))) ->
          childCost <> PressureCost 200
        Node (BladeMergeNode seam (K (_, childCost))) ->
          childCost <> mergeCost seam
        Node (BladeFrontierNode seam (K (_, childCost))) ->
          childCost <> frontierCost seam
        Node CycleCoverNode ->
          PressureCost 200
        Node PatchLocalNode {} ->
          PressureCost 200
        Node PatchRepairNode {} ->
          PressureCost 200
        Node CycleLayoutNode {} ->
          PressureCost 200
        Node AcceptedLayoutNode {} ->
          PressureCost 200
        Node ObstructedLayoutNode {} ->
          PressureCost 200

gluingCostAlgebra :: AnalysisCostAlgebra (Node BladeChoiceSig) ChoiceDepth GluingCost
gluingCostAlgebra =
  AnalysisCostAlgebra $
    \_analysis ->
      \case
        Node (BladeRootNode (K (_, childCost))) ->
          childCost
        Node BladeUnitNode ->
          GluingCost GluingPending 0 Nothing
        Node (BladeUndecidedNode _ (K (_, childCost))) ->
          childCost {gcLocalCost = gcLocalCost childCost + 500}
        Node (BladeMergeNode _ (K (_, childCost))) ->
          childCost
        Node (BladeFrontierNode _ (K (_, childCost))) ->
          childCost
        Node CycleCoverNode ->
          GluingCost GluingPending 500 Nothing
        Node (PatchLocalNode patch) ->
          GluingCost GluingFragment (patchSectionCost patch LocalSection) Nothing
        Node (PatchRepairNode patch) ->
          GluingCost GluingFragment (patchSectionCost patch RepairSection) Nothing
        Node (CycleLayoutNode (K (_, patchA)) (K (_, patchB)) (K (_, patchC))) ->
          GluingCost GluingPending (sum (fmap gcLocalCost [patchA, patchB, patchC])) Nothing
        Node (AcceptedLayoutNode plan) ->
          acceptedPlanCost plan
        Node (ObstructedLayoutNode plan obstruction) ->
          obstructedPlanCost plan obstruction

mergeCost :: BladeSeam -> PressureCost
mergeCost =
  PressureCost . (100 -) . mergeScore . seamObservation

frontierCost :: BladeSeam -> PressureCost
frontierCost =
  PressureCost . (100 -) . frontierScore . seamObservation

seamObservation :: BladeSeam -> SeamObservation
seamObservation =
  \case
    AbyssRidge ->
      SeamObservation
        { soMergeAffinity = 95,
          soBoundaryDrama = 95,
          soCompatibilityDissonance = 95,
          soBundlePressure = 95
        }
    OrchardRoad ->
      SeamObservation
        { soMergeAffinity = 95,
          soBoundaryDrama = 5,
          soCompatibilityDissonance = 5,
          soBundlePressure = 5
        }
    ArchiveGate ->
      SeamObservation
        { soMergeAffinity = 90,
          soBoundaryDrama = 10,
          soCompatibilityDissonance = 10,
          soBundlePressure = 10
        }

mergeScore :: SeamObservation -> Int
mergeScore observation =
  weightedScore
    [ (52, soMergeAffinity observation),
      (18, 100 - soCompatibilityDissonance observation),
      (15, 100 - soBoundaryDrama observation),
      (15, 100 - soBundlePressure observation)
    ]

frontierScore :: SeamObservation -> Int
frontierScore observation =
  weightedScore
    [ (34, soCompatibilityDissonance observation),
      (30, soBoundaryDrama observation),
      (24, soBundlePressure observation),
      (12, 100 - soMergeAffinity observation)
    ]

weightedScore :: [(Int, Int)] -> Int
weightedScore =
  (`div` 100) . sum . fmap (uncurry (*))

instance Semigroup PressureCost where
  PressureCost left <> PressureCost right =
    PressureCost (left + right)

instance Monoid PressureCost where
  mempty =
    PressureCost 0

instance Ord GluingCost where
  compare leftCost rightCost =
    compare (gluingCostKey leftCost) (gluingCostKey rightCost)

gluingCostKey :: GluingCost -> (Int, Int, Maybe CycleRepairPlan)
gluingCostKey cost =
  (gluingVerdictRank (gcVerdict cost), gcLocalCost cost, gcPlan cost)

gluingVerdictRank :: GluingVerdict -> Int
gluingVerdictRank =
  \case
    GluingAccepted -> 0
    GluingFragment -> 1
    GluingPending -> 2
    GluingRejected {} -> 3

acceptedPlanCost :: CycleRepairPlan -> GluingCost
acceptedPlanCost plan =
  case cycleDescentVerdict (layoutForPlan plan) of
    CycleAccepted ->
      GluingCost
        { gcVerdict = GluingAccepted,
          gcLocalCost = planLocalCost plan,
          gcPlan = Just plan
        }
    CycleRejected obstruction ->
      obstructedPlanCost plan obstruction

obstructedPlanCost :: CycleRepairPlan -> CycleObstruction -> GluingCost
obstructedPlanCost plan obstruction =
  GluingCost
    { gcVerdict = GluingRejected obstruction,
      gcLocalCost = planLocalCost plan + 1000,
      gcPlan = Just plan
    }

bladeChoiceAnalysis :: AnalysisSpec (Node BladeChoiceSig) ChoiceDepth
bladeChoiceAnalysis =
  semilatticeAnalysis $
    \case
      Node (BladeRootNode (K (ChoiceDepth childDepth))) ->
        ChoiceDepth (childDepth + 1)
      Node BladeUnitNode ->
        ChoiceDepth 0
      Node (BladeUndecidedNode _ (K (ChoiceDepth childDepth))) ->
        ChoiceDepth (childDepth + 1)
      Node (BladeMergeNode _ (K (ChoiceDepth childDepth))) ->
        ChoiceDepth (childDepth + 1)
      Node (BladeFrontierNode _ (K (ChoiceDepth childDepth))) ->
        ChoiceDepth (childDepth + 1)
      Node CycleCoverNode ->
        ChoiceDepth 0
      Node PatchLocalNode {} ->
        ChoiceDepth 0
      Node PatchRepairNode {} ->
        ChoiceDepth 0
      Node (CycleLayoutNode (K (ChoiceDepth leftDepth)) (K (ChoiceDepth middleDepth)) (K (ChoiceDepth rightDepth))) ->
        ChoiceDepth (max leftDepth (max middleDepth rightDepth) + 1)
      Node AcceptedLayoutNode {} ->
        ChoiceDepth 0
      Node ObstructedLayoutNode {} ->
        ChoiceDepth 0

emptyBladeChoiceGraph :: SaturatingContextEGraph SurfaceKind (PackedNode BladeChoiceSig) ChoiceDepth ()
emptyBladeChoiceGraph =
  emptySaturatingContextEGraph $
    emptyContextEGraph (singletonContextLattice ()) $
      emptyEGraphWithTheory (packAnalysisSpec bladeChoiceAnalysis) emptyTheorySpec

pressureFrontierSaturationBudget :: SaturationBudget
pressureFrontierSaturationBudget =
  SaturationBudget
    { sbMaxIterations = 32,
      sbMaxNodes = 2048
    }

bladeRoot :: Term BladeChoiceSig "Expr" -> Term BladeChoiceSig "Expr"
bladeRoot child =
  node (BladeRootNode child)

bladeUnit :: Term BladeChoiceSig "Expr"
bladeUnit =
  node BladeUnitNode

undecided :: BladeSeam -> Term BladeChoiceSig "Expr" -> Term BladeChoiceSig "Expr"
undecided seam child =
  node (BladeUndecidedNode seam child)

mergeDecision :: BladeSeam -> Term BladeChoiceSig "Expr" -> Term BladeChoiceSig "Expr"
mergeDecision seam child =
  node (BladeMergeNode seam child)

frontierDecision :: BladeSeam -> Term BladeChoiceSig "Expr" -> Term BladeChoiceSig "Expr"
frontierDecision seam child =
  node (BladeFrontierNode seam child)

cycleCover :: Term BladeChoiceSig "Expr"
cycleCover =
  node CycleCoverNode

patchLocal :: CyclePatch -> Term BladeChoiceSig "Expr"
patchLocal patch =
  node (PatchLocalNode patch)

patchRepair :: CyclePatch -> Term BladeChoiceSig "Expr"
patchRepair patch =
  node (PatchRepairNode patch)

cycleLayout ::
  Term BladeChoiceSig "Expr" ->
  Term BladeChoiceSig "Expr" ->
  Term BladeChoiceSig "Expr" ->
  Term BladeChoiceSig "Expr"
cycleLayout patchA patchB patchC =
  node (CycleLayoutNode patchA patchB patchC)

acceptedLayout :: CycleRepairPlan -> Term BladeChoiceSig "Expr"
acceptedLayout plan =
  node (AcceptedLayoutNode plan)

obstructedLayout :: CycleRepairPlan -> CycleObstruction -> Term BladeChoiceSig "Expr"
obstructedLayout plan obstruction =
  node (ObstructedLayoutNode plan obstruction)

decodeBladeChoices :: Fix (Node BladeChoiceSig) -> Either String [BladeDecision]
decodeBladeChoices (Fix (Node sigNode)) =
  case sigNode of
    BladeRootNode (K child) ->
      decodeBladeChoiceChain child
    _ ->
      Left "expected extracted term to be rooted"

decodeBladeChoiceChain :: Fix (Node BladeChoiceSig) -> Either String [BladeDecision]
decodeBladeChoiceChain (Fix (Node sigNode)) =
  case sigNode of
    BladeUnitNode ->
      Right []
    BladeMergeNode seam (K child) ->
      (ExtractMerge seam :) <$> decodeBladeChoiceChain child
    BladeFrontierNode seam (K child) ->
      (ExtractFrontier seam :) <$> decodeBladeChoiceChain child
    BladeUndecidedNode seam _ ->
      Left ("extraction left seam undecided: " <> show seam)
    BladeRootNode {} ->
      Left "nested blade root is not a decision chain"
    CycleCoverNode ->
      Left "cycle cover is not a frontier decision"
    PatchLocalNode {} ->
      Left "local patch section is not a frontier decision"
    PatchRepairNode {} ->
      Left "repair patch section is not a frontier decision"
    CycleLayoutNode {} ->
      Left "cycle layout is not a frontier decision"
    AcceptedLayoutNode {} ->
      Left "accepted cycle layout is not a frontier decision"
    ObstructedLayoutNode {} ->
      Left "obstructed cycle layout is not a frontier decision"

decodeCycleLayout :: Fix (Node BladeChoiceSig) -> Either String CycleRepairPlan
decodeCycleLayout (Fix (Node sigNode)) =
  case sigNode of
    AcceptedLayoutNode plan ->
      Right plan
    ObstructedLayoutNode plan obstruction ->
      Left ("extracted obstructed cycle layout " <> show plan <> ": " <> show obstruction)
    CycleLayoutNode {} ->
      Left "extracted raw cycle layout without descent witness"
    _ ->
      Left "expected extracted cycle layout witness"

layoutForPlan :: CycleRepairPlan -> Map CyclePatch PatchSection
layoutForPlan plan =
  Map.fromList
    [ (PatchA, patchSection PatchA (variantForPlan PatchA plan)),
      (PatchB, patchSection PatchB (variantForPlan PatchB plan)),
      (PatchC, patchSection PatchC (variantForPlan PatchC plan))
    ]

variantForPlan :: CyclePatch -> CycleRepairPlan -> PatchVariant
variantForPlan patch plan =
  case (patch, plan) of
    (PatchA, RepairPatchA) ->
      RepairSection
    (PatchB, RepairPatchB) ->
      RepairSection
    (PatchC, RepairPatchC) ->
      RepairSection
    _ ->
      LocalSection

patchSection :: CyclePatch -> PatchVariant -> PatchSection
patchSection patch variant =
  PatchSection
    { psPatch = patch,
      psVariant = variant,
      psRestrictions = patchRestrictions patch variant,
      psLocalCost = patchSectionCost patch variant
    }

patchRestrictions :: CyclePatch -> PatchVariant -> Map CycleOverlap SeamValue
patchRestrictions patch variant =
  Map.fromList $
    case (patch, variant) of
      (PatchA, LocalSection) ->
        [(OverlapCA, SeamRed), (OverlapAB, SeamRed)]
      (PatchA, RepairSection) ->
        [(OverlapCA, SeamBlue), (OverlapAB, SeamRed)]
      (PatchB, LocalSection) ->
        [(OverlapAB, SeamRed), (OverlapBC, SeamRed)]
      (PatchB, RepairSection) ->
        [(OverlapAB, SeamRed), (OverlapBC, SeamBlue)]
      (PatchC, LocalSection) ->
        [(OverlapBC, SeamRed), (OverlapCA, SeamBlue)]
      (PatchC, RepairSection) ->
        [(OverlapBC, SeamRed), (OverlapCA, SeamRed)]

patchSectionCost :: CyclePatch -> PatchVariant -> Int
patchSectionCost patch variant =
  case (patch, variant) of
    (_, LocalSection) ->
      1
    (PatchA, RepairSection) ->
      12
    (PatchB, RepairSection) ->
      12
    (PatchC, RepairSection) ->
      8

planLocalCost :: CycleRepairPlan -> Int
planLocalCost =
  sum . fmap psLocalCost . Map.elems . layoutForPlan

cycleDescentVerdict :: Map CyclePatch PatchSection -> CycleDescentVerdict
cycleDescentVerdict sections =
  case mapMaybe (overlapMismatch sections) [OverlapAB, OverlapBC, OverlapCA] of
    [] ->
      CycleAccepted
    obstruction : _ ->
      CycleRejected obstruction

overlapMismatch :: Map CyclePatch PatchSection -> CycleOverlap -> Maybe CycleObstruction
overlapMismatch sections overlap =
  let (leftPatch, rightPatch) = overlapOwners overlap
      leftRestriction = restrictionAt sections leftPatch overlap
      rightRestriction = restrictionAt sections rightPatch overlap
   in case (leftRestriction, rightRestriction) of
        (Right leftValue, Right rightValue)
          | leftValue == rightValue ->
              Nothing
          | otherwise ->
              Just (OverlapMismatch overlap leftPatch leftValue rightPatch rightValue)
        (Left obstruction, _) ->
          Just obstruction
        (_, Left obstruction) ->
          Just obstruction

restrictionAt :: Map CyclePatch PatchSection -> CyclePatch -> CycleOverlap -> Either CycleObstruction SeamValue
restrictionAt sections patch overlap =
  case Map.lookup patch sections >>= Map.lookup overlap . psRestrictions of
    Just seamValue ->
      Right seamValue
    Nothing ->
      Left (MissingRestriction patch overlap)

overlapOwners :: CycleOverlap -> (CyclePatch, CyclePatch)
overlapOwners =
  \case
    OverlapAB -> (PatchA, PatchB)
    OverlapBC -> (PatchB, PatchC)
    OverlapCA -> (PatchA, PatchC)

greedyCycleObstruction :: CycleObstruction
greedyCycleObstruction =
  OverlapMismatch OverlapCA PatchA SeamRed PatchC SeamBlue

repairPatchBObstruction :: CycleObstruction
repairPatchBObstruction =
  OverlapMismatch OverlapBC PatchB SeamBlue PatchC SeamRed

seamDigest :: BladeSeam -> Word64
seamDigest =
  \case
    AbyssRidge -> 0
    OrchardRoad -> 1
    ArchiveGate -> 2

patchDigest :: CyclePatch -> Word64
patchDigest =
  \case
    PatchA -> 0
    PatchB -> 1
    PatchC -> 2

overlapDigest :: CycleOverlap -> Word64
overlapDigest =
  \case
    OverlapAB -> 0
    OverlapBC -> 1
    OverlapCA -> 2

seamValueDigest :: SeamValue -> Word64
seamValueDigest =
  \case
    SeamRed -> 0
    SeamBlue -> 1

planDigest :: CycleRepairPlan -> Word64
planDigest =
  \case
    GreedyLocal -> 0
    RepairPatchA -> 1
    RepairPatchB -> 2
    RepairPatchC -> 3

obstructionDigest :: CycleObstruction -> Word64
obstructionDigest =
  \case
    OverlapMismatch overlap leftPatch leftValue rightPatch rightValue ->
      10
        + 97 * overlapDigest overlap
        + 17 * patchDigest leftPatch
        + 5 * seamValueDigest leftValue
        + 29 * patchDigest rightPatch
        + 7 * seamValueDigest rightValue
    MissingRestriction patch overlap ->
      400 + 19 * patchDigest patch + overlapDigest overlap

expectBladeFront ::
  Either (EGraphFrontError BladeChoiceSig ChoiceDepth ()) value ->
  IO value
expectBladeFront =
  \case
    Right value ->
      pure value
    Left err ->
      assertFailure (frontErrorMessage err)
