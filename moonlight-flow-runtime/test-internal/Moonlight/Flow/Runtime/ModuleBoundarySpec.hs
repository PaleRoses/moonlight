module Moonlight.Flow.Runtime.ModuleBoundarySpec
  ( tests,
  )
where

import Control.Monad
  ( filterM,
  )
import Data.Foldable
  ( traverse_,
  )
import Data.List
  ( isInfixOf,
  )
import Data.Maybe
  ( listToMaybe,
  )
import System.Directory
  ( doesFileExist,
  )
import System.FilePath
  ( (</>),
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "runtime module boundaries"
    [ testCase "canonical IR is not coupled to engine or lowering" canonicalIRBoundaryAssertion,
      testCase "schedule and dispatch do not import topology lowering" engineBoundaryAssertion,
      testCase "visible context maintenance is carrier-cache owned, not factor or trace-payload owned" visibleContextMaintenanceBoundaryAssertion,
      testCase "carrier operators are one runtime bundle across backend, config, and env" carrierOperatorBundleAssertion,
      testCase "runtime dataflow op is abstract and topology lowering is split" runtimeDataflowOpLoweringShapeAssertion,
      testCase "old split-authority names are gone from runtime sources" staleAuthorityNameAssertion
    ]

canonicalIRBoundaryAssertion :: Assertion
canonicalIRBoundaryAssertion = do
  source <- readRuntimeSource "src-contract/Moonlight/Flow/Runtime/Execution/IR.hs"
  assertNotContaining
    "Runtime.Execution.IR must not import topology lowering"
    "Moonlight.Flow.Runtime.Topology.Lowering"
    source
  assertNotContaining
    "Runtime.Execution.IR must not import engine modules"
    "Moonlight.Flow.Runtime.Engine."
    source

engineBoundaryAssertion :: Assertion
engineBoundaryAssertion = do
  traverse_
    assertEngineBoundary
    [ "src/Moonlight/Flow/Runtime/Engine/Schedule/Enqueue.hs",
      "src/Moonlight/Flow/Runtime/Engine/Schedule/Feedback.hs",
      "src/Moonlight/Flow/Runtime/Engine/Schedule/Time.hs",
      "src/Moonlight/Flow/Runtime/Engine/Dispatch/Carrier.hs",
      "src/Moonlight/Flow/Runtime/Engine/Dispatch/Core.hs",
      "src/Moonlight/Flow/Runtime/Engine/Dispatch/Shard.hs"
    ]

assertEngineBoundary :: FilePath -> Assertion
assertEngineBoundary path = do
  source <- readRuntimeSource path
  assertNotContaining
    (path <> " must not import topology lowering")
    "Moonlight.Flow.Runtime.Topology.Lowering"
    source
  assertNotContaining
    (path <> " must not import carrier topology")
    "Moonlight.Flow.Carrier.Core.Topology"
    source

visibleContextMaintenanceBoundaryAssertion :: Assertion
visibleContextMaintenanceBoundaryAssertion = do
  touchSource <-
    readRuntimeSource "src/Moonlight/Flow/Runtime/Carrier/Store/Touch.hs"
  traceSource <-
    readRuntimeSource "src/Moonlight/Flow/Runtime/Carrier/Core/Types.hs"
  cacheSource <-
    readRuntimeSource "../carrier/src/Moonlight/Flow/Carrier/View/Cache.hs"

  assertContaining
    "touch path must own pinned visible-context cache maintenance"
    "updatePinnedVisibleContext"
    touchSource
  assertNotContaining
    "visible-context maintenance must not reuse factor demand machinery"
    "Moonlight.Flow.Runtime.Factor"
    touchSource
  assertNotContaining
    "visible-context maintenance must not route through FactorDemand"
    "FactorDemand"
    touchSource
  assertNotContaining
    "CarrierCommitTrace must remain touched-address metadata, not a row-delta payload carrier"
    "RowDelta"
    traceSource
  assertNotContaining
    "CarrierCommitTrace must not carry carrier deltas as a shadow maintenance language"
    "RelationalCarrierDelta"
    traceSource
  assertContaining
    "pinned visible contexts must be keyed by context, not by epoch"
    "vscPinned :: !(Map ctx (CachedVisibleEntry section))"
    cacheSource
  assertNotContaining
    "pinned visible contexts must not regress to epoch-keyed entries"
    "Set (VisibleContextKey"
    cacheSource

carrierOperatorBundleAssertion :: Assertion
carrierOperatorBundleAssertion = do
  backendSource <- readRuntimeSource "src/Moonlight/Flow/Runtime/Backend.hs"
  configSource <- readRuntimeSource "src/Moonlight/Flow/Runtime/Kernel/Config.hs"
  kernelSource <- readRuntimeSource "src/Moonlight/Flow/Runtime/Kernel.hs"

  assertContaining
    "backend must own one carrier operator bundle"
    "rbCarrierOperators"
    backendSource
  assertContaining
    "config must carry one carrier operator bundle"
    "rcCarrierOperators"
    configSource
  assertContaining
    "runtime env must expose one carrier operator bundle"
    "reCarrierOperators"
    kernelSource
  assertContaining
    "bundle type must be the named runtime operator product"
    "RuntimeCarrierOperators"
    (backendSource <> configSource <> kernelSource)


runtimeDataflowOpLoweringShapeAssertion :: Assertion
runtimeDataflowOpLoweringShapeAssertion = do
  irSource <- readRuntimeSource "src-contract/Moonlight/Flow/Runtime/Execution/IR.hs"
  unsplitLoweringExists <- doesRuntimeSourceExist "src/Moonlight/Flow/Runtime/Topology/Lowering.hs"
  assertNotContaining
    "RuntimeDataflowOp constructors must not be exported directly"
    "RuntimeDataflowOp (..)"
    irSource
  assertContaining
    "runtime dataflow ops must glue leaf vocabularies through a typed sum"
    "data (f :+: g)"
    irSource
  assertContaining
    "runtime dataflow metadata must live on the op algebra"
    "class RuntimeDataflowOpMetadata"
    irSource
  assertNotContaining
    "RuntimeDataflowOpKind must not regress to a closed constructor sum"
    "data RuntimeDataflowOpKind ctx prop boundary evidence"
    irSource
  assertBool
    "unsplit Runtime.Topology.Lowering module must stay deleted"
    (not unsplitLoweringExists)

staleAuthorityNameAssertion :: Assertion
staleAuthorityNameAssertion = do
  traverse_
    (\needle -> assertRuntimeSourcesDoNotContain needle)
    [ "Moonlight.Flow.Runtime.Topology.Program",
      "RuntimeTopologyProgram",
      "Runtime" <> "Contract",
      "executionOpForEdge",
      "executionEdgeContract",
      "executionOpContract",
      "Engine.Types",
      "Engine.Plan"
    ]

assertRuntimeSourcesDoNotContain :: String -> Assertion
assertRuntimeSourcesDoNotContain needle = do
  sourceFiles <- runtimeSourceFiles
  offenders <- filterM (sourceContains needle) sourceFiles
  assertBool
    ("stale runtime authority name remains: " <> needle <> " in " <> show offenders)
    (null offenders)

sourceContains :: String -> FilePath -> IO Bool
sourceContains needle path =
  isInfixOf needle <$> readFile path

assertNotContaining :: String -> String -> String -> Assertion
assertNotContaining label needle source =
  assertBool label (not (isInfixOf needle source))

assertContaining :: String -> String -> String -> Assertion
assertContaining label needle source =
  assertBool label (isInfixOf needle source)


doesRuntimeSourceExist :: FilePath -> IO Bool
doesRuntimeSourceExist relativePath = do
  root <- runtimePackageRoot
  doesFileExist (root </> relativePath)

readRuntimeSource :: FilePath -> IO String
readRuntimeSource relativePath = do
  root <- runtimePackageRoot
  readFile (root </> relativePath)

runtimeSourceFiles :: IO [FilePath]
runtimeSourceFiles = do
  root <- runtimePackageRoot
  existing <- filterM doesFileExist (fmap (root </>) checkedRuntimeSourcePaths)
  pure existing

runtimePackageRoot :: IO FilePath
runtimePackageRoot = do
  roots <- filterM doesFileExist candidateCabalFiles
  case listToMaybe roots of
    Nothing ->
      assertFailure "could not locate moonlight-flow-runtime package root" *> pure "."
    Just cabalFile ->
      pure (dropCabalFile cabalFile)

candidateCabalFiles :: [FilePath]
candidateCabalFiles =
  [ "moonlight-flow-runtime.cabal",
    "foundation/moonlight-flow/runtime/moonlight-flow-runtime.cabal",
    "compiler/foundation/moonlight-flow/runtime/moonlight-flow-runtime.cabal"
  ]

dropCabalFile :: FilePath -> FilePath
dropCabalFile cabalFile =
  case cabalFile of
    "moonlight-flow-runtime.cabal" ->
      "."
    "foundation/moonlight-flow/runtime/moonlight-flow-runtime.cabal" ->
      "foundation/moonlight-flow/runtime"
    "compiler/foundation/moonlight-flow/runtime/moonlight-flow-runtime.cabal" ->
      "compiler/foundation/moonlight-flow/runtime"
    _ ->
      "."

checkedRuntimeSourcePaths :: [FilePath]
checkedRuntimeSourcePaths =
  [ "src-contract/Moonlight/Flow/Runtime/Execution/IR.hs",
    "src/Moonlight/Flow/Runtime/Topology/Lowering/Edge.hs",
    "src/Moonlight/Flow/Runtime/Topology/Lowering/GeneratedSite.hs",
    "src/Moonlight/Flow/Runtime/Topology/Lowering/Impact.hs",
    "src/Moonlight/Flow/Runtime/Topology/Lowering/Types.hs",
    "src/Moonlight/Flow/Runtime/Engine.hs",
    "src-dataflow/Moonlight/Flow/Runtime/Engine/Dataflow.hs",
    "src/Moonlight/Flow/Runtime/Engine/Diagnostics.hs",
    "src/Moonlight/Flow/Runtime/Engine/Dispatch.hs",
    "src/Moonlight/Flow/Runtime/Engine/GeneratedSite.hs",
    "src/Moonlight/Flow/Runtime/Engine/Patch.hs",
    "src/Moonlight/Flow/Runtime/Engine/Queue.hs",
    "src/Moonlight/Flow/Runtime/Engine/Schedule.hs",
    "src/Moonlight/Flow/Runtime/Engine/State.hs",
    "src/Moonlight/Flow/Runtime/Engine/Step.hs",
    "src/Moonlight/Flow/Runtime/Engine/Touch.hs",
    "src/Moonlight/Flow/Runtime/Kernel.hs",
    "src/Moonlight/Flow/Runtime/Topology.hs"
  ]
