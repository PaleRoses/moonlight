{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Test.Scale.Run
  ( AtlasRunObstruction (..),
    AtlasReferenceObstruction (..),
    AtlasExtractionObstruction (..),
    AtlasContextExtractionObstruction (..),
    AtlasAgreementObstruction (..),
    AtlasExtraction,
    atlasExtractionCost,
    atlasExtractionTerm,
    runAtlasProgram,
    runAtlasReferences,
    extractAtlasFromGraph,
    extractAtlasAtContext,
    assertReferenceAgreement,
  )
where

import Data.Bifunctor (first)
import Data.Fix (Fix)
import Data.Foldable (foldlM, traverse_)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Algebra (JoinSemilattice)
import Moonlight.Control.Schedule (identitySchedulerRefinement)
import Moonlight.Core
  ( ClassId,
    ConstructorTag,
    HasConstructorTag,
    Language,
    OrderedFix (..),
    RewriteRuleId,
  )
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    activateContext,
  )
import Moonlight.EGraph.Pure.Context (cegSite)
import Moonlight.EGraph.Pure.Context.Proof
  ( ProofGraph (pgGraph),
  )
import Moonlight.EGraph.Pure.Extraction
  ( CostAlgebra,
    ExtractionResult (..),
    extract,
    stableExtractionSnapshotFromEGraph,
  )
import Moonlight.EGraph.Pure.Saturation.Substrate (EGraphU)
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingProofEGraph,
    mapSaturatingContextGraph,
    sceContextGraph,
  )
import Moonlight.EGraph.Pure.Types
  ( EGraph,
    canonicalizeClassId,
  )
import Moonlight.EGraph.Test.Saturation
  ( saturationReportBaseGraph,
    saturateWithSchedulerRefinement,
    srResult,
  )
import Moonlight.EGraph.Test.Context.MaterializedOracle
  ( materializedContextGraphAt,
  )
import Moonlight.Rewrite.System (RewriteCondition)
import Moonlight.Rewrite.System (FactRule)
import Moonlight.Rewrite.System (RawRewriteRule)
import Moonlight.Saturation.Context.Error (SaturationError)
import Moonlight.Saturation.Context.Program.Spec (PlanSpec)
import Moonlight.Saturation.Core
  ( SaturationTermination (..),
  )
import Moonlight.Saturation.Substrate
  ( SatGraph,
    TrivialContext,
  )
import Moonlight.Saturation.Support.Core (SupportSaturationReportFor)
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSupportError,
    UnitContextSiteOwner,
  )
import Moonlight.Sheaf.Twist.SupportedRuleSpec qualified as SheafTwist

type AtlasRunObstruction :: Type -> Type -> Type
data AtlasRunObstruction context errorValue
  = AtlasDriverContextActivationFailed !(PreparedContextSupportError context)
  | AtlasDriverFailed !errorValue
  | AtlasDriverDidNotReachFixedPoint !SaturationTermination
  deriving stock (Eq, Show)

type AtlasReferenceObstruction :: Type -> Type -> Type
data AtlasReferenceObstruction context saturationError
  = AtlasReferenceRuleLookupFailed !context !(PreparedContextSupportError context)
  | AtlasReferenceFactLookupFailed !context !(PreparedContextSupportError context)
  | AtlasReferenceMaterializationFailed !context !(PreparedContextSupportError context)
  | AtlasReferenceSaturationFailed !context !saturationError
  | AtlasReferenceDidNotReachFixedPoint !context !SaturationTermination
  deriving stock (Eq, Show)

type AtlasExtractionObstruction :: Type
data AtlasExtractionObstruction
  = AtlasExtractionGraphDirty
  | AtlasExtractionRootMissing !ClassId
  deriving stock (Eq, Show)

type AtlasContextExtractionObstruction :: Type -> Type
data AtlasContextExtractionObstruction context
  = AtlasContextMaterializationFailed !(PreparedContextSupportError context)
  | AtlasContextGraphExtractionFailed !AtlasExtractionObstruction
  deriving stock (Eq, Show)

type AtlasAgreementObstruction :: Type -> Type
data AtlasAgreementObstruction context
  = AtlasReferenceGraphMissing !context
  | AtlasProductionMaterializationFailed !context !(PreparedContextSupportError context)
  | AtlasEquivalenceMismatch !context !ClassId !ClassId
  | AtlasProductionExtractionFailed !context !ClassId !AtlasExtractionObstruction
  | AtlasReferenceExtractionFailed !context !ClassId !AtlasExtractionObstruction
  | AtlasExtractionMismatch !context !ClassId
  deriving stock (Eq, Show)

type AtlasExtraction :: (Type -> Type) -> Type -> Type
data AtlasExtraction f cost = AtlasExtraction
  { aeCost :: !cost,
    aeTerm :: !(Fix f)
  }

instance (Language f, Eq cost) => Eq (AtlasExtraction f cost) where
  leftExtraction == rightExtraction =
    aeCost leftExtraction == aeCost rightExtraction
      && OrderedFix (aeTerm leftExtraction) == OrderedFix (aeTerm rightExtraction)

atlasExtractionCost :: AtlasExtraction f cost -> cost
atlasExtractionCost =
  aeCost

atlasExtractionTerm :: AtlasExtraction f cost -> Fix f
atlasExtractionTerm =
  aeTerm

runAtlasProgram ::
  (Language f, Ord context) =>
  [context] ->
  ( SaturatingProofEGraph owner capability f analysis context proof ->
    Either
      errorValue
      (SupportSaturationReportFor universe (SaturatingProofEGraph owner capability f analysis context proof))
  ) ->
  SaturatingProofEGraph owner capability f analysis context proof ->
  Either
    (AtlasRunObstruction context errorValue)
    (SupportSaturationReportFor universe (SaturatingProofEGraph owner capability f analysis context proof))
runAtlasProgram activeContexts runProgram proofGraph = do
  activatedContextGraph <-
    first AtlasDriverContextActivationFailed
      ( foldlM
          (\contextGraph contextValue -> activateContext contextValue contextGraph)
          (sceContextGraph (pgGraph proofGraph))
          activeContexts
      )
  report <-
    first AtlasDriverFailed $
      runProgram
        proofGraph
          { pgGraph =
              mapSaturatingContextGraph
                (const activatedContextGraph)
                (pgGraph proofGraph)
          }
  if srResult report == ReachedFixedPoint
    then Right report
    else Left (AtlasDriverDidNotReachFixedPoint (srResult report))

runAtlasReferences ::
  forall owner capability f analysis context.
  ( Ord capability,
    Show capability,
    HasConstructorTag f,
    Show (ConstructorTag f),
    Show (f ()),
    Ord analysis,
    JoinSemilattice analysis,
    Ord context
  ) =>
  PlanSpec
    (EGraphU UnitContextSiteOwner capability f analysis TrivialContext)
    (SatGraph (EGraphU UnitContextSiteOwner capability f analysis TrivialContext))
    RewriteRuleId ->
  SheafTwist.SupportedRuleBook owner context (RawRewriteRule (RewriteCondition capability f) f) ->
  SheafTwist.SupportedFactBook owner context (FactRule capability f) ->
  ContextEGraph owner f analysis context ->
  [context] ->
  Either
    ( AtlasReferenceObstruction
        context
        (SaturationError (EGraphU UnitContextSiteOwner capability f analysis TrivialContext) RewriteRuleId)
    )
    (Map context (EGraph f analysis))
runAtlasReferences planSpecValue ruleBook factBook initialContextGraph contexts =
  Map.fromList
    <$> traverse
      ( \contextValue ->
          fmap ((,) contextValue) $
            runAtlasReferenceAt
              planSpecValue
              ruleBook
              factBook
              initialContextGraph
              contextValue
      )
      contexts

runAtlasReferenceAt ::
  forall owner capability f analysis context.
  ( Ord capability,
    Show capability,
    HasConstructorTag f,
    Show (ConstructorTag f),
    Show (f ()),
    Ord analysis,
    JoinSemilattice analysis,
    Ord context
  ) =>
  PlanSpec
    (EGraphU UnitContextSiteOwner capability f analysis TrivialContext)
    (SatGraph (EGraphU UnitContextSiteOwner capability f analysis TrivialContext))
    RewriteRuleId ->
  SheafTwist.SupportedRuleBook owner context (RawRewriteRule (RewriteCondition capability f) f) ->
  SheafTwist.SupportedFactBook owner context (FactRule capability f) ->
  ContextEGraph owner f analysis context ->
  context ->
  Either
    ( AtlasReferenceObstruction
        context
        (SaturationError (EGraphU UnitContextSiteOwner capability f analysis TrivialContext) RewriteRuleId)
    )
    (EGraph f analysis)
runAtlasReferenceAt planSpecValue ruleBook factBook initialContextGraph contextValue = do
  rules <-
    first (AtlasReferenceRuleLookupFailed contextValue) $
      SheafTwist.rulesActiveAt (cegSite initialContextGraph) contextValue ruleBook
  facts <-
    first (AtlasReferenceFactLookupFailed contextValue) $
      SheafTwist.factRulesActiveAt (cegSite initialContextGraph) contextValue factBook
  localizedGraph <-
    first (AtlasReferenceMaterializationFailed contextValue)
      (materializedContextGraphAt contextValue initialContextGraph)
  report <-
    first (AtlasReferenceSaturationFailed contextValue) $
      saturateWithSchedulerRefinement
        identitySchedulerRefinement
        planSpecValue
        facts
        rules
        localizedGraph
  if srResult report == ReachedFixedPoint
    then Right (saturationReportBaseGraph report)
    else Left (AtlasReferenceDidNotReachFixedPoint contextValue (srResult report))

extractAtlasFromGraph ::
  (Language f, Ord cost) =>
  CostAlgebra f cost ->
  ClassId ->
  EGraph f analysis ->
  Either AtlasExtractionObstruction (AtlasExtraction f cost)
extractAtlasFromGraph costAlgebra rootClass graph = do
  snapshot <-
    maybe
      (Left AtlasExtractionGraphDirty)
      Right
      (stableExtractionSnapshotFromEGraph graph)
  extractionResult <-
    maybe
      (Left (AtlasExtractionRootMissing rootClass))
      Right
      (extract costAlgebra rootClass snapshot)
  pure
    AtlasExtraction
      { aeCost = erCost extractionResult,
        aeTerm = erTerm extractionResult
      }

extractAtlasAtContext ::
  (Language f, Ord context, Ord cost) =>
  CostAlgebra f cost ->
  context ->
  ClassId ->
  ContextEGraph owner f analysis context ->
  Either (AtlasContextExtractionObstruction context) (AtlasExtraction f cost)
extractAtlasAtContext costAlgebra contextValue rootClass contextGraph = do
  materializedGraph <-
    first AtlasContextMaterializationFailed
      (materializedContextGraphAt contextValue contextGraph)
  first AtlasContextGraphExtractionFailed
    (extractAtlasFromGraph costAlgebra rootClass materializedGraph)

assertReferenceAgreement ::
  forall owner f analysis context cost.
  (Language f, Ord context, Ord cost) =>
  CostAlgebra f cost ->
  [context] ->
  [ClassId] ->
  ContextEGraph owner f analysis context ->
  Map context (EGraph f analysis) ->
  Either (AtlasAgreementObstruction context) ()
assertReferenceAgreement costAlgebra contexts rootClasses productionContextGraph referenceGraphs =
  traverse_ agreeAtContext contexts
  where
    classPairs = liftA2 (,) rootClasses rootClasses

    agreeAtContext :: context -> Either (AtlasAgreementObstruction context) ()
    agreeAtContext contextValue = do
      referenceGraph <-
        maybe
          (Left (AtlasReferenceGraphMissing contextValue))
          Right
          (Map.lookup contextValue referenceGraphs)
      productionGraph <-
        first (AtlasProductionMaterializationFailed contextValue)
          (materializedContextGraphAt contextValue productionContextGraph)
      traverse_ (agreeEquivalence contextValue productionGraph referenceGraph) classPairs
      traverse_ (agreeExtraction contextValue productionGraph referenceGraph) rootClasses

    agreeEquivalence ::
      context ->
      EGraph f analysis ->
      EGraph f analysis ->
      (ClassId, ClassId) ->
      Either (AtlasAgreementObstruction context) ()
    agreeEquivalence contextValue productionGraph referenceGraph (leftClass, rightClass) =
      if equivalent productionGraph leftClass rightClass == equivalent referenceGraph leftClass rightClass
        then Right ()
        else Left (AtlasEquivalenceMismatch contextValue leftClass rightClass)

    agreeExtraction ::
      context ->
      EGraph f analysis ->
      EGraph f analysis ->
      ClassId ->
      Either (AtlasAgreementObstruction context) ()
    agreeExtraction contextValue productionGraph referenceGraph rootClass = do
      productionExtraction <-
        first (AtlasProductionExtractionFailed contextValue rootClass) $
          extractAtlasFromGraph costAlgebra rootClass productionGraph
      referenceExtraction <-
        first (AtlasReferenceExtractionFailed contextValue rootClass) $
          extractAtlasFromGraph costAlgebra rootClass referenceGraph
      if productionExtraction == referenceExtraction
        then Right ()
        else Left (AtlasExtractionMismatch contextValue rootClass)

    equivalent :: EGraph f analysis -> ClassId -> ClassId -> Bool
    equivalent graph leftClass rightClass =
      canonicalizeClassId graph leftClass == canonicalizeClassId graph rightClass
