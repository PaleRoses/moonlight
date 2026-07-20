{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.Nebula.Source.Ingest
  ( IngestedModule (..),
    ingestModule,
    bindingDisplayName,
    patternNodeCount,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Monoid (Sum (..))
import Data.Set qualified as Set
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import Melusine.Nebula.Core
  ( ModuleWorkload (..),
    NebulaAnalysis (..),
    NebulaError (..),
    nebulaAnalysisSpec,
    typeObservations,
    workloadOracle,
  )
import Moonlight.Algebra (JoinSemilattice (..))
import Moonlight.Core (Pattern (..))
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( ConvertedModule (..),
    HsExprF,
    HsExprInsertionMetrics,
    InsertionSeeding (..),
    SpanClassRow,
    ScopeCtx (ActualScope),
    ScopedExpr (..),
    TopLevelBinding (..),
    convertHaskellSource,
    convertedModuleContextLattice,
    insertConvertedModuleWithMetrics,
  )
import Moonlight.EGraph.Pure.Context (ContextEGraph, emptyContextEGraph)
import Moonlight.EGraph.Pure.Types (ClassId, emptyEGraph)
import Moonlight.Flow.Model.Schema.Digest (stableDigest128)
import Moonlight.Pale.Ghc.Hie.Oracle (ModuleNameOracle (..))
import Moonlight.Pale.Ghc.Hie.TypeWords (typeWordsList)

type IngestedModule :: Type
data IngestedModule = IngestedModule
  { imPath :: !FilePath,
    imSource :: !String,
    imConverted :: !ConvertedModule,
    imBindingNames :: ![String],
    imBindingContexts :: ![ScopeCtx],
    imOriginalSizes :: ![Int],
    imSeedClasses :: ![ClassId],
    imSpanRows :: ![SpanClassRow],
    imInsertionMetrics :: !HsExprInsertionMetrics,
    imContextGraph :: !(ContextEGraph HsExprF NebulaAnalysis ScopeCtx)
  }

ingestModule :: ModuleWorkload -> Either NebulaError IngestedModule
ingestModule workload = do
  convertedModule <-
    first (NebulaParseError . show) (convertHaskellSource (mwPath workload) (mwSource workload))
  latticeValue <-
    first (NebulaLatticeError . show) (convertedModuleContextLattice convertedModule)
  let contextGraph0 = emptyContextEGraph latticeValue (emptyEGraph (nebulaAnalysisSpec (cmScopeIndex convertedModule)))
  (seedClasses, spanRows, insertionMetrics, contextGraph1) <-
    first (NebulaInsertionError . show) (insertConvertedModuleWithMetrics (typeSeeding (workloadOracle workload)) convertedModule contextGraph0)
  let bindings = cmBindings convertedModule
      bindingNames = fmap bindingDisplayName bindings
      bindingContexts = fmap (ActualScope . seOccScope . tlbScopedTerm) bindings
      nameCount = length bindingNames
      contextCount = length bindingContexts
      seedCount = length seedClasses
  if nameCount == seedCount && contextCount == seedCount
    then
      Right
        IngestedModule
          { imPath = mwPath workload,
            imSource = mwSource workload,
            imConverted = convertedModule,
            imBindingNames = bindingNames,
            imBindingContexts = bindingContexts,
            imOriginalSizes = fmap (patternNodeCount . tlbTerm) bindings,
            imSeedClasses = seedClasses,
            imSpanRows = spanRows,
            imInsertionMetrics = insertionMetrics,
            imContextGraph = contextGraph1
          }
    else Left (NebulaArityMismatch nameCount contextCount seedCount)

bindingDisplayName :: TopLevelBinding -> String
bindingDisplayName bindingValue =
  case tlbNames bindingValue of
    [] ->
      "_unnamed"
    bindingName : _ ->
      occNameString (rdrNameOcc bindingName)

patternNodeCount :: Pattern HsExprF -> Int
patternNodeCount = \case
  PatternVar {} ->
    1
  PatternNode nodeValue ->
    1 + getSum (foldMap (Sum . patternNodeCount) nodeValue)

typeSeeding :: Maybe ModuleNameOracle -> InsertionSeeding NebulaAnalysis
typeSeeding maybeOracle =
  InsertionSeeding
    ( \maybeRegion analysisValue ->
        case maybeRegion >>= \region -> Map.lookup region =<< fmap mnoTypeAtSpan maybeOracle of
          Nothing ->
            analysisValue
          Just typeWords ->
            analysisValue
              { naType =
                  join
                    (naType analysisValue)
                    (typeObservations (Set.map (stableDigest128 . typeWordsList) typeWords))
              }
    )
