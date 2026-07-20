{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Bench.Pipeline.ProductSubstrate
  ( ContextEqualityGoal (..),
    ExternalConjunctionGoal (..),
    ProductGoalSpec (..),
    ProductGoalSearch (..),
    InternalGoalResult (..),
    ExternalGoalResult (..),
    ProductGoalResult (..),
    ProductContextReport (..),
    ProductAggregateReport (..),
    ProductSubstrateReport (..),
    ProductGoalClassSide (..),
    ProductSubstrateError (..),
    resolveProductContextEqualityGoal,
    runProductSubstrate,
    runProductSubstrateFromFixture,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Melusine.Nebula.Core
  ( ModuleWorkload (..),
    NebulaAnalysis,
    NebulaConfig (..),
    NebulaError (..),
    NebulaRule,
    workloadOracle,
  )
import Melusine.Nebula.Discovery.Choose (resolvePatternClass)
import Melusine.Nebula.Rewrite.Corpus
  ( RuleCorpus,
    deriveRuleCorpusWithOracleKeysAndReason,
    rcFactBook,
    rcRuleBook,
  )
import Melusine.Nebula.Source.Ingest
  ( IngestedModule (..),
    ingestModule,
  )
import Melusine.Nebula.Source.Workspace (enumerateModuleWorkloads)
import Moonlight.Core (ClassId, Pattern, RewriteRuleId)
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( HsExprF,
    ScopeCtx,
    hsExprCapabilityGenerationForContextGraph,
    hsExprOracleKeyTable,
    hsExprRuntimeCapabilitiesForContextGraph,
  )
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    contextPreparedObjects,
    emptyContextEGraphFromSite,
  )
import Moonlight.EGraph.Pure.Context
  ( cegBase,
    cegSite,
    contextAuthoredUnionPairs,
  )
import Moonlight.EGraph.Pure.Saturation.Matching
  ( MatchingStrategy (GenericJoinMatching),
  )
import Moonlight.EGraph.Pure.Saturation.Substrate (EGraphU)
import Moonlight.EGraph.Pure.Types
  ( EGraph,
    canonicalizeClassId,
    eGraphClassCount,
    eGraphNodeCount,
    lookupEClass,
  )
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingContextEGraph,
    sceContextGraph,
  )
import Moonlight.Rewrite.System (OracleKey)
import Moonlight.Rewrite.System (FactRule)
import Moonlight.Saturation.Context.Driver
  ( ContextRunResult (..),
    plainContextRunSpec,
    runContextProgram,
  )
import Moonlight.Saturation.Context.Error (SaturationError)
import Moonlight.Saturation.Context.Program.Source
  ( ProgramM,
    facts,
    rewrites,
  )
import Moonlight.Saturation.Context.Program.Spec
  ( PlanSpec,
    RewriteContextSnapshot (..),
    deterministicSchedulerConfig,
    planSpec,
    withRewriteContext,
    withSchedulerConfig,
  )
import Moonlight.Saturation.Context.Runtime.Report
  ( SaturationReport,
    reportIterationCount,
    saturationReportBaseGraph,
    srResult,
  )
import Moonlight.Saturation.Core
  ( SaturationTermination (..),
    TerminationGoal,
    goal,
  )
import Moonlight.Saturation
  ( TrivialContext,
    embedBaseGraph,
  )
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite,
    PreparedContextSupportError,
    preparedContextRestrictsTo,
  )
import Moonlight.Sheaf.Twist.SupportedRuleSpec
  ( factRulesActiveAt,
    rulesActiveAt,
  )
import Moonlight.Pale.Ghc.Hie.Oracle
  ( ModuleNameOracle,
    occResolvesUniquely,
  )
import Moonlight.Pale.Ghc.Hie.SourceKey (oracleAttachFailure)

type ContextEqualityGoal :: Type
data ContextEqualityGoal = ContextEqualityGoal
  { cegContext :: !ScopeCtx,
    cegLeftClass :: !ClassId,
    cegRightClass :: !ClassId
  }
  deriving stock (Eq, Ord, Show)

type ExternalConjunctionGoal :: Type
data ExternalConjunctionGoal = ExternalConjunctionGoal
  { ecgTargetContext :: !ScopeCtx,
    ecgCoveringEqualities :: !(NonEmpty ContextEqualityGoal)
  }
  deriving stock (Eq, Ord, Show)

type ProductGoalSpec :: Type
data ProductGoalSpec
  = InternalEqualityGoal !ContextEqualityGoal
  | ExternalCoveringConjunctionGoal !ExternalConjunctionGoal
  deriving stock (Eq, Ord, Show)

type ProductGoalSearch :: Type
data ProductGoalSearch
  = ProductGoalNotFound
  | ProductGoalFoundAtRound !Int
  deriving stock (Eq, Ord, Show)

type InternalGoalResult :: Type
data InternalGoalResult = InternalGoalResult
  { igrContext :: !ScopeCtx,
    igrSearch :: !ProductGoalSearch
  }
  deriving stock (Eq, Ord, Show)

type ExternalGoalResult :: Type
data ExternalGoalResult = ExternalGoalResult
  { egrTargetContext :: !ScopeCtx,
    egrCoveringContexts :: !(NonEmpty ScopeCtx),
    egrSearch :: !ProductGoalSearch
  }
  deriving stock (Eq, Ord, Show)

type ProductGoalResult :: Type
data ProductGoalResult
  = NoProductGoal
  | InternalProductGoalResult !InternalGoalResult
  | ExternalConjunctionGoalResult !ExternalGoalResult
  deriving stock (Eq, Ord, Show)

type ProductContextReport :: Type
data ProductContextReport = ProductContextReport
  { pcrContext :: !ScopeCtx,
    pcrRounds :: !Int,
    pcrNodes :: !Int,
    pcrClasses :: !Int,
    pcrTermination :: !SaturationTermination
  }
  deriving stock (Eq, Ord, Show)

type ProductAggregateReport :: Type
data ProductAggregateReport = ProductAggregateReport
  { parRounds :: !Int,
    parNodes :: !Int,
    parClasses :: !Int,
    parCompleted :: !Bool,
    parGoalResult :: !ProductGoalResult
  }
  deriving stock (Eq, Ord, Show)

type ProductSubstrateReport :: Type
data ProductSubstrateReport = ProductSubstrateReport
  { psrPath :: !FilePath,
    psrContexts :: ![ProductContextReport],
    psrAggregate :: !ProductAggregateReport
  }
  deriving stock (Eq, Ord, Show)

type ProductGoalClassSide :: Type
data ProductGoalClassSide
  = ProductGoalLeftClass
  | ProductGoalRightClass
  deriving stock (Eq, Ord, Show)

type ProductUniverse :: Type
type ProductUniverse = EGraphU ScopeCtx HsExprF NebulaAnalysis TrivialContext

type ProductCarrier :: Type
type ProductCarrier = SaturatingContextEGraph ScopeCtx HsExprF NebulaAnalysis TrivialContext

type ProductSubstrateError :: Type
data ProductSubstrateError
  = ProductWorkspaceFailure ![NebulaError]
  | ProductFixtureWorkloadCardinality !FilePath !Int
  | ProductIngestFailure !NebulaError
  | ProductCorpusFailure !NebulaError
  | ProductContextualEqualityUnsupported !ScopeCtx !Int
  | ProductRuleProjectionFailure !ScopeCtx !(PreparedContextSupportError ScopeCtx)
  | ProductFactProjectionFailure !ScopeCtx !(PreparedContextSupportError ScopeCtx)
  | ProductGoalContextMissing !ScopeCtx
  | ProductExternalConjunctionRequiresMultipleCovers !Int
  | ProductExternalConjunctionDuplicateCovers ![ScopeCtx]
  | ProductGoalCoverEqualsTarget !ScopeCtx
  | ProductGoalCoverProjectionFailure !ScopeCtx !ScopeCtx !(PreparedContextSupportError ScopeCtx)
  | ProductGoalCoverDoesNotRestrictToTarget !ScopeCtx !ScopeCtx
  | ProductGoalClassMissing !ScopeCtx !ProductGoalClassSide !ClassId
  | ProductSaturationFailure !ScopeCtx !(SaturationError ProductUniverse RewriteRuleId)
  | ProductContextReportMissing !ScopeCtx
  deriving stock (Show)

type ProductSection :: Type
data ProductSection = ProductSection
  { productSectionContext :: !ScopeCtx,
    productSectionGraph :: !(EGraph HsExprF NebulaAnalysis),
    productSectionRules :: ![NebulaRule],
    productSectionFacts :: ![FactRule ScopeCtx HsExprF],
    productSectionGoal :: !(Maybe ContextEqualityGoal)
  }

runProductSubstrate ::
  NebulaConfig ->
  ModuleWorkload ->
  Maybe ProductGoalSpec ->
  Either ProductSubstrateError ProductSubstrateReport
runProductSubstrate config workload maybeGoalSpec = do
  ingested <- first ProductIngestFailure (ingestModule workload)
  corpus <- deriveProductionCorpus config workload ingested
  let contextGraph = imContextGraph ingested
      site = cegSite contextGraph
      contexts = contextPreparedObjects contextGraph
  productBaseGraph <- independentProductBaseGraph contextGraph
  validatedGoal <- validateProductGoal site contexts maybeGoalSpec
  let goalAssignments = productGoalAssignments validatedGoal
  sections <-
    traverse
      (assembleProductSection site productBaseGraph corpus goalAssignments)
      contexts
  contextReports <- traverse (runProductSection config site) sections
  aggregate <- aggregateProductReports validatedGoal contextReports
  pure
    ProductSubstrateReport
      { psrPath = mwPath workload,
        psrContexts = contextReports,
        psrAggregate = aggregate
      }

runProductSubstrateFromFixture ::
  NebulaConfig ->
  FilePath ->
  Maybe ProductGoalSpec ->
  IO (Either ProductSubstrateError ProductSubstrateReport)
runProductSubstrateFromFixture config fixturePath maybeGoalSpec = do
  (workspaceFailures, workloads) <- enumerateModuleWorkloads [fixturePath] []
  pure $
    case workspaceFailures of
      [] ->
        case workloads of
          [workload] ->
            runProductSubstrate config workload maybeGoalSpec
          _ ->
            Left (ProductFixtureWorkloadCardinality fixturePath (length workloads))
      _ ->
        Left (ProductWorkspaceFailure workspaceFailures)

deriveProductionCorpus ::
  NebulaConfig ->
  ModuleWorkload ->
  IngestedModule ->
  Either ProductSubstrateError RuleCorpus
deriveProductionCorpus config workload ingested = do
  satisfiedOracleKeys <-
    first ProductCorpusFailure (oracleKeysForProductWorkload (workloadOracle workload))
  first ProductCorpusFailure $
    deriveRuleCorpusWithOracleKeysAndReason
      config
      satisfiedOracleKeys
      (oracleAttachFailure (mwOracleLookup workload))
      (imSpanRows ingested)
      (workloadOracle workload)
      (imConverted ingested)

oracleKeysForProductWorkload ::
  Maybe ModuleNameOracle ->
  Either NebulaError (Set.Set OracleKey)
oracleKeysForProductWorkload =
  maybe
    (Right Set.empty)
    ( \oracle ->
        first (NebulaRuleDerivationError . ("oracle key table parse failed: " <>) . show) $
          Set.fromList
            . fmap (\(oracleKey, _, _) -> oracleKey)
            . filter (\(_, occurrence, acceptedOrigins) -> occResolvesUniquely oracle occurrence acceptedOrigins)
            <$> hsExprOracleKeyTable
    )

validateProductGoal ::
  PreparedContextSite ScopeCtx ->
  [ScopeCtx] ->
  Maybe ProductGoalSpec ->
  Either ProductSubstrateError (Maybe ProductGoalSpec)
validateProductGoal site contexts maybeGoalSpec = do
  traverse_ (validateGoalSpec site (Set.fromList contexts)) maybeGoalSpec
  pure maybeGoalSpec

validateGoalSpec ::
  PreparedContextSite ScopeCtx ->
  Set.Set ScopeCtx ->
  ProductGoalSpec ->
  Either ProductSubstrateError ()
validateGoalSpec site contexts = \case
  InternalEqualityGoal equalityGoal ->
    requireGoalContext contexts (cegContext equalityGoal)
  ExternalCoveringConjunctionGoal conjunctionGoal -> do
    let targetContext = ecgTargetContext conjunctionGoal
        coveringGoals = NonEmpty.toList (ecgCoveringEqualities conjunctionGoal)
        coveringContexts = fmap cegContext coveringGoals
        duplicateCovers =
          Map.keys
            ( Map.filter
                (> 1)
                (Map.fromListWith (+) (fmap (\contextValue -> (contextValue, 1 :: Int)) coveringContexts))
            )
    requireGoalContext contexts targetContext
    traverse_ (requireGoalContext contexts) coveringContexts
    if length coveringGoals < 2
      then Left (ProductExternalConjunctionRequiresMultipleCovers (length coveringGoals))
      else pure ()
    if null duplicateCovers
      then pure ()
      else Left (ProductExternalConjunctionDuplicateCovers duplicateCovers)
    traverse_ (validateCoveringContext site targetContext) coveringContexts

requireGoalContext ::
  Set.Set ScopeCtx ->
  ScopeCtx ->
  Either ProductSubstrateError ()
requireGoalContext contexts contextValue =
  if Set.member contextValue contexts
    then Right ()
    else Left (ProductGoalContextMissing contextValue)

validateCoveringContext ::
  PreparedContextSite ScopeCtx ->
  ScopeCtx ->
  ScopeCtx ->
  Either ProductSubstrateError ()
validateCoveringContext site targetContext coverContext =
  if coverContext == targetContext
    then Left (ProductGoalCoverEqualsTarget targetContext)
    else do
      restrictsToTarget <-
        first
          (ProductGoalCoverProjectionFailure targetContext coverContext)
          (preparedContextRestrictsTo site coverContext targetContext)
      if restrictsToTarget
        then Right ()
        else Left (ProductGoalCoverDoesNotRestrictToTarget targetContext coverContext)

productGoalAssignments ::
  Maybe ProductGoalSpec ->
  Map ScopeCtx ContextEqualityGoal
productGoalAssignments = \case
  Nothing ->
    Map.empty
  Just (InternalEqualityGoal equalityGoal) ->
    Map.singleton (cegContext equalityGoal) equalityGoal
  Just (ExternalCoveringConjunctionGoal conjunctionGoal) ->
    Map.fromList
      [ (cegContext equalityGoal, equalityGoal)
      | equalityGoal <- NonEmpty.toList (ecgCoveringEqualities conjunctionGoal)
      ]

assembleProductSection ::
  PreparedContextSite ScopeCtx ->
  EGraph HsExprF NebulaAnalysis ->
  RuleCorpus ->
  Map ScopeCtx ContextEqualityGoal ->
  ScopeCtx ->
  Either ProductSubstrateError ProductSection
assembleProductSection site productBaseGraph corpus goalAssignments contextValue = do
  activeRules <-
    first
      (ProductRuleProjectionFailure contextValue)
      (rulesActiveAt site contextValue (rcRuleBook corpus))
  activeFacts <-
    first
      (ProductFactProjectionFailure contextValue)
      (factRulesActiveAt site contextValue (rcFactBook corpus))
  let maybeEqualityGoal = Map.lookup contextValue goalAssignments
  traverse_ (validateGoalClasses productBaseGraph) maybeEqualityGoal
  pure
    ProductSection
      { productSectionContext = contextValue,
        productSectionGraph = productBaseGraph,
        productSectionRules = activeRules,
        productSectionFacts = activeFacts,
        productSectionGoal = maybeEqualityGoal
      }

resolveProductContextEqualityGoal ::
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  ScopeCtx ->
  Pattern HsExprF ->
  Pattern HsExprF ->
  Either ProductSubstrateError (Maybe ContextEqualityGoal)
resolveProductContextEqualityGoal contextGraph contextValue leftPattern rightPattern = do
  productBaseGraph <- independentProductBaseGraph contextGraph
  pure
    ( ContextEqualityGoal contextValue
        <$> resolvePatternClass productBaseGraph leftPattern
        <*> resolvePatternClass productBaseGraph rightPattern
    )

independentProductBaseGraph ::
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  Either ProductSubstrateError (EGraph HsExprF NebulaAnalysis)
independentProductBaseGraph contextGraph =
  case mapMaybe authoredEqualityCount (contextPreparedObjects contextGraph) of
    [] -> Right (cegBase contextGraph)
    (contextValue, equalityCount) : _ ->
      Left (ProductContextualEqualityUnsupported contextValue equalityCount)
  where
    authoredEqualityCount contextValue =
      case length (contextAuthoredUnionPairs contextValue contextGraph) of
        0 -> Nothing
        equalityCount -> Just (contextValue, equalityCount)

validateGoalClasses ::
  EGraph HsExprF NebulaAnalysis ->
  ContextEqualityGoal ->
  Either ProductSubstrateError ()
validateGoalClasses graph equalityGoal = do
  requireGoalClass graph equalityGoal ProductGoalLeftClass (cegLeftClass equalityGoal)
  requireGoalClass graph equalityGoal ProductGoalRightClass (cegRightClass equalityGoal)

requireGoalClass ::
  EGraph HsExprF NebulaAnalysis ->
  ContextEqualityGoal ->
  ProductGoalClassSide ->
  ClassId ->
  Either ProductSubstrateError ()
requireGoalClass graph equalityGoal classSide classId =
  case lookupEClass graph classId of
    Nothing ->
      Left (ProductGoalClassMissing (cegContext equalityGoal) classSide classId)
    Just _ ->
      Right ()

runProductSection ::
  NebulaConfig ->
  PreparedContextSite ScopeCtx ->
  ProductSection ->
  Either ProductSubstrateError ProductContextReport
runProductSection config sourceSite sectionValue = do
  let contextValue = productSectionContext sectionValue
      initialCarrier :: ProductCarrier
      initialCarrier = embedBaseGraph @ProductUniverse (productSectionGraph sectionValue)
      planSpecValue = productPlanSpec config sourceSite initialCarrier
      runSpec =
        plainContextRunSpec @ProductUniverse
          planSpecValue
          (productTerminationGoal (productSectionGoal sectionValue))
  runResult <-
    first
      (ProductSaturationFailure contextValue)
      ( runContextProgram @ProductUniverse
          runSpec
          (productProgram sectionValue)
          initialCarrier
      )
  pure (productContextReport contextValue (crrResult runResult))

productProgram :: ProductSection -> ProgramM ProductUniverse ()
productProgram sectionValue =
  void (facts @ProductUniverse (productSectionFacts sectionValue))
    *> void (rewrites @ProductUniverse (productSectionRules sectionValue))

productPlanSpec ::
  NebulaConfig ->
  PreparedContextSite ScopeCtx ->
  ProductCarrier ->
  PlanSpec ProductUniverse ProductCarrier RewriteRuleId
productPlanSpec config sourceSite initialCarrier =
  let initialSnapshot = productRewriteContextSnapshot sourceSite initialCarrier
   in withRewriteContext
        (productRewriteContextSnapshot sourceSite)
        ( withSchedulerConfig
            deterministicSchedulerConfig
            ( planSpec
                (ncSaturationBudget config)
                GenericJoinMatching
                (rcsRewriteContext initialSnapshot)
            )
        )

productRewriteContextSnapshot ::
  PreparedContextSite ScopeCtx ->
  ProductCarrier ->
  RewriteContextSnapshot ProductUniverse
productRewriteContextSnapshot sourceSite carrier =
  let capabilityGraph =
        emptyContextEGraphFromSite
          sourceSite
          (cegBase (sceContextGraph carrier))
   in RewriteContextSnapshot
        { rcsCapabilityGeneration = hsExprCapabilityGenerationForContextGraph capabilityGraph,
          rcsRewriteContext = hsExprRuntimeCapabilitiesForContextGraph capabilityGraph
        }

productTerminationGoal ::
  Maybe ContextEqualityGoal ->
  TerminationGoal ProductCarrier
productTerminationGoal = \case
  Nothing ->
    mempty
  Just equalityGoal ->
    goal
      ( \carrier ->
          let graph = cegBase (sceContextGraph carrier)
           in canonicalizeClassId graph (cegLeftClass equalityGoal)
                == canonicalizeClassId graph (cegRightClass equalityGoal)
      )

productContextReport ::
  ScopeCtx ->
  SaturationReport ProductUniverse ->
  ProductContextReport
productContextReport contextValue report =
  let finalGraph = saturationReportBaseGraph @ProductUniverse report
   in ProductContextReport
        { pcrContext = contextValue,
          pcrRounds = reportIterationCount report,
          pcrNodes = eGraphNodeCount finalGraph,
          pcrClasses = eGraphClassCount finalGraph,
          pcrTermination = srResult report
        }

aggregateProductReports ::
  Maybe ProductGoalSpec ->
  [ProductContextReport] ->
  Either ProductSubstrateError ProductAggregateReport
aggregateProductReports maybeGoalSpec contextReports = do
  goalResult <- productGoalResult maybeGoalSpec contextReports
  pure
    ProductAggregateReport
      { parRounds = foldl' max 0 (fmap pcrRounds contextReports),
        parNodes = sum (fmap pcrNodes contextReports),
        parClasses = sum (fmap pcrClasses contextReports),
        parCompleted = all (completedTermination . pcrTermination) contextReports,
        parGoalResult = goalResult
      }

completedTermination :: SaturationTermination -> Bool
completedTermination = \case
  ReachedFixedPoint ->
    True
  ReachedGoal ->
    True
  HitIterationLimit ->
    False
  HitNodeLimit ->
    False

productGoalResult ::
  Maybe ProductGoalSpec ->
  [ProductContextReport] ->
  Either ProductSubstrateError ProductGoalResult
productGoalResult maybeGoalSpec contextReports =
  let reportsByContext =
        Map.fromList
          [ (pcrContext contextReport, contextReport)
          | contextReport <- contextReports
          ]
      reportFor contextValue =
        maybe
          (Left (ProductContextReportMissing contextValue))
          Right
          (Map.lookup contextValue reportsByContext)
   in case maybeGoalSpec of
        Nothing ->
          Right NoProductGoal
        Just (InternalEqualityGoal equalityGoal) -> do
          contextReport <- reportFor (cegContext equalityGoal)
          pure
            ( InternalProductGoalResult
                InternalGoalResult
                  { igrContext = cegContext equalityGoal,
                    igrSearch = goalSearchFromReports [contextReport]
                  }
            )
        Just (ExternalCoveringConjunctionGoal conjunctionGoal) -> do
          let coveringContexts = fmap cegContext (ecgCoveringEqualities conjunctionGoal)
          coveringReports <- traverse reportFor coveringContexts
          pure
            ( ExternalConjunctionGoalResult
                ExternalGoalResult
                  { egrTargetContext = ecgTargetContext conjunctionGoal,
                    egrCoveringContexts = coveringContexts,
                    egrSearch = goalSearchFromReports (NonEmpty.toList coveringReports)
                  }
            )

goalSearchFromReports :: [ProductContextReport] -> ProductGoalSearch
goalSearchFromReports reports =
  if all ((== ReachedGoal) . pcrTermination) reports
    then ProductGoalFoundAtRound (foldl' max 0 (fmap pcrRounds reports))
    else ProductGoalNotFound
