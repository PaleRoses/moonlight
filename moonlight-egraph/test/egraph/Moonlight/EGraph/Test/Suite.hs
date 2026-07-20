{-# LANGUAGE LambdaCase #-}

module Moonlight.EGraph.Test.Suite
  ( egraphSuite,
  )
where

import qualified Moonlight.EGraph.Core.AnalysisSpec as AnalysisSpec
import qualified Moonlight.EGraph.Core.AntiUnifySpec as AntiUnifySpec
import qualified Moonlight.EGraph.Boundary.BlueRoseSpec as BlueRoseSpec
import qualified Moonlight.EGraph.Context.BladePressureFrontierSpec as BladePressureFrontierSpec
import qualified Moonlight.EGraph.Context.BodyRegionContextSpec as BodyRegionContextSpec
import qualified Moonlight.EGraph.Context.ColoredVsSheafSpec as ColoredVsSheafSpec
import qualified Moonlight.EGraph.Context.ContextSpec as ContextSpec
import qualified Moonlight.EGraph.Context.ContextualSectionCacheSpec as ContextualSectionCacheSpec
import qualified Moonlight.EGraph.Context.ControlledInductionSpec as ControlledInductionSpec
import qualified Moonlight.EGraph.Context.DescentSpec as DescentSpec
import qualified Moonlight.EGraph.Diagnostics.DiagnosticSpec as DiagnosticSpec
import qualified Moonlight.EGraph.Context.EGraphIncidenceSiteLawSpec as EGraphIncidenceSiteLawSpec
import qualified Moonlight.EGraph.Context.MeshSeamDescentSpec as MeshSeamDescentSpec
import qualified Moonlight.EGraph.Context.PowersetTwinSpec as PowersetTwinSpec
import qualified Moonlight.EGraph.Context.AnnotatedDeltaSpec as AnnotatedDeltaSpec
import qualified Moonlight.EGraph.Context.RegionalUnionFindSpec as RegionalUnionFindSpec
import qualified Moonlight.EGraph.Context.VirtualFiberSpec as VirtualFiberSpec
import qualified Moonlight.EGraph.Effect.Laws as EffectLaws
import qualified Moonlight.EGraph.Egg.GroupSpec as EggGroupSpec
import qualified Moonlight.EGraph.Egg.MathSpec as EggMathSpec
import qualified Moonlight.EGraph.Egg.SimpleSpec as EggSimpleSpec
import qualified Moonlight.EGraph.Extraction.ExtractionSpec as ExtractionSpec
import qualified Moonlight.EGraph.Extraction.IncrementalExtractionSpec as IncrementalExtractionSpec
import qualified Moonlight.EGraph.Extraction.WorklistSpec as WorklistSpec
import qualified Moonlight.EGraph.Core.GraphSpec as GraphSpec
import qualified Moonlight.EGraph.Extraction.GuideSpec as GuideSpec
import qualified Moonlight.EGraph.Core.HashConsSpec as HashConsSpec
import qualified Moonlight.EGraph.Boundary.LeanKernelSpec as LeanKernelSpec
import qualified Moonlight.EGraph.Boundary.ObstructionSpec as ObstructionSpec
import qualified Moonlight.EGraph.Core.PatternSpec as PatternSpec
import qualified Moonlight.EGraph.Rewrite.PresheafPathSpec as PresheafPathSpec
import qualified Moonlight.EGraph.Boundary.ProofBoundarySpec as ProofBoundarySpec
import qualified Moonlight.EGraph.Boundary.ProofSpec as ProofSpec
import qualified Moonlight.EGraph.Pure.Saturation.FrontSpec as FrontSpec
import qualified Moonlight.EGraph.Pure.Saturation.LogicSpec as LogicSpec
import qualified Moonlight.EGraph.Pure.Saturation.MatchingSpec as MatchingSpec
import qualified Moonlight.EGraph.Core.QuotientPatchSpec as QuotientPatchSpec
import qualified Moonlight.EGraph.Core.RebuildPropertySpec as RebuildPropertySpec
import qualified Moonlight.EGraph.Core.RebuildSpec as RebuildSpec
import qualified Moonlight.EGraph.Boundary.RegistryConsistencySpec as RegistryConsistencySpec
import qualified Moonlight.EGraph.Rewrite.StructuralStoreSpec as StructuralStoreSpec
import qualified Moonlight.EGraph.Rewrite.RewriteSpec as RewriteSpec
import qualified Moonlight.EGraph.Front.PackedNodeOracleSpec as PackedNodeOracleSpec
import qualified Moonlight.EGraph.Front.SDFAlgebraSpec as SDFAlgebraSpec
import qualified Moonlight.EGraph.Front.SDFPropertySpec as SDFPropertySpec
import qualified Moonlight.EGraph.Binding.ScopedLambdaSaturationSpec as ScopedLambdaSaturationSpec
import qualified Moonlight.EGraph.Binding.SlottedLambdaBindingGoalSpec as SlottedLambdaBindingGoalSpec
import qualified Moonlight.EGraph.Binding.SlottedUpstreamCoverageSpec as SlottedUpstreamCoverageSpec
import qualified Moonlight.EGraph.Spec.LambdaBindingGoal as LambdaBindingGoal
import qualified Moonlight.EGraph.Rewrite.TraceLawSpec as TraceLawSpec
import qualified Moonlight.EGraph.Core.UnionFindSpec as UnionFindSpec
import qualified Moonlight.EGraph.Diagnostics.VertexSetBenchSpec as VertexSetBenchSpec
import Test.Tasty (TestTree, testGroup)

data SuiteGroup
  = FrontAuthoring
  | CoreEGraph
  | RewriteSaturation
  | ExtractionGuidance
  | ContextDescent
  | ProofEffectBoundary
  | BindingSemantics
  | DiagnosticsPressure
  deriving stock (Eq, Ord, Show)

newtype SuiteElement = SuiteElement TestTree

data SemanticSuite = SemanticSuite SuiteGroup [SuiteElement]

egraphSuite :: TestTree
egraphSuite =
  testGroup "moonlight-egraph" $
    fmap semanticSuiteTree semanticSuites

semanticSuites :: [SemanticSuite]
semanticSuites =
  [ SemanticSuite
      FrontAuthoring
      [ SuiteElement FrontSpec.tests,
        SuiteElement EggSimpleSpec.tests,
        SuiteElement EggGroupSpec.tests,
        SuiteElement EggMathSpec.tests,
        SuiteElement SDFAlgebraSpec.tests,
        SuiteElement SDFPropertySpec.tests,
        SuiteElement PackedNodeOracleSpec.tests
      ],
    SemanticSuite
      CoreEGraph
      [ SuiteElement UnionFindSpec.tests,
        SuiteElement HashConsSpec.tests,
        SuiteElement GraphSpec.tests,
        SuiteElement RebuildSpec.tests,
        SuiteElement RebuildPropertySpec.tests,
        SuiteElement QuotientPatchSpec.tests,
        SuiteElement AnalysisSpec.tests,
        SuiteElement AntiUnifySpec.tests,
        SuiteElement PatternSpec.tests
      ],
    SemanticSuite
      RewriteSaturation
      [ SuiteElement RewriteSpec.tests,
        SuiteElement LogicSpec.tests,
        SuiteElement PresheafPathSpec.tests,
        SuiteElement StructuralStoreSpec.tests,
        SuiteElement TraceLawSpec.tests,
        SuiteElement (testGroup "SEL-10" [MatchingSpec.tests])
      ],
    SemanticSuite
      ExtractionGuidance
      [ SuiteElement ExtractionSpec.tests,
        SuiteElement WorklistSpec.tests,
        SuiteElement IncrementalExtractionSpec.tests,
        SuiteElement GuideSpec.tests
      ],
    SemanticSuite
      ContextDescent
      [ SuiteElement ContextSpec.tests,
        SuiteElement BodyRegionContextSpec.tests,
        SuiteElement ColoredVsSheafSpec.tests,
        SuiteElement ContextualSectionCacheSpec.tests,
        SuiteElement ControlledInductionSpec.tests,
        SuiteElement DescentSpec.tests,
        SuiteElement EGraphIncidenceSiteLawSpec.tests,
        SuiteElement BladePressureFrontierSpec.tests,
        SuiteElement MeshSeamDescentSpec.tests,
        SuiteElement PowersetTwinSpec.tests,
        SuiteElement VirtualFiberSpec.tests,
        SuiteElement AnnotatedDeltaSpec.tests,
        SuiteElement RegionalUnionFindSpec.tests
      ],
    SemanticSuite
      ProofEffectBoundary
      [ SuiteElement ProofSpec.tests,
        SuiteElement ProofBoundarySpec.tests,
        SuiteElement ObstructionSpec.tests,
        SuiteElement EffectLaws.tests,
        SuiteElement RegistryConsistencySpec.tests,
        SuiteElement BlueRoseSpec.tests,
        SuiteElement LeanKernelSpec.tests
      ],
    SemanticSuite
      BindingSemantics
      [ SuiteElement ScopedLambdaSaturationSpec.tests,
        SuiteElement LambdaBindingGoal.tests,
        SuiteElement SlottedLambdaBindingGoalSpec.tests,
        SuiteElement SlottedUpstreamCoverageSpec.tests
      ],
    SemanticSuite
      DiagnosticsPressure
      [ SuiteElement DiagnosticSpec.tests,
        SuiteElement VertexSetBenchSpec.tests
      ]
  ]

semanticSuiteTree :: SemanticSuite -> TestTree
semanticSuiteTree (SemanticSuite groupName elements) =
  testGroup (suiteGroupName groupName) (fmap suiteElementTree elements)

suiteGroupName :: SuiteGroup -> String
suiteGroupName =
  \case
    FrontAuthoring -> "front-authoring-and-semantic-programs"
    CoreEGraph -> "core-egraph-store-and-rebuild"
    RewriteSaturation -> "rewrite-saturation-and-matching-substrate"
    ExtractionGuidance -> "extraction-and-guidance"
    ContextDescent -> "context-descent-and-sheaf-boundary"
    ProofEffectBoundary -> "proof-effect-and-obstruction-boundary"
    BindingSemantics -> "binding-semantics"
    DiagnosticsPressure -> "diagnostics-and-pressure"

suiteElementTree :: SuiteElement -> TestTree
suiteElementTree (SuiteElement tree) =
  tree
