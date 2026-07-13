{-# LANGUAGE DerivingStrategies #-}

-- | Product-vs-region scaling CSV generator for paper evidence.
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Fix (Fix)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (foldl', intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Word (Word64)
import qualified GHC.Clock as Clock
import qualified GHC.Stats as Stats
import Moonlight.Core (UnionFindAllocationError)
import Moonlight.EGraph.Pure.Context
  ( ContextDeltaError,
    ContextEGraph,
    contextAnalysisValueAt,
    contextMerge,
    contextRepresentativeAt,
    contextVisibleClassKeys,
    emptyContextEGraph,
  )
import Moonlight.EGraph.Pure.Context.Core
  ( cegBase,
    cegContextRevision,
    contextAuthoredUnionPairs,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.EGraph.Pure.Rebuild (rebuild)
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    EGraph,
    classIdKey,
    classUnionsDelta,
    eGraphAnalysis,
    eGraphClassCount,
    eGraphNodeCount,
    eGraphPendingClassUnions,
    emptyEGraph,
    enqueueEditDelta,
  )
import Moonlight.EGraph.Test.Arith.Core
  ( ArithF,
    NodeCount (..),
    addTermNode,
    analysisSpec,
    numTerm,
  )
import Moonlight.FiniteLattice
  ( ContextLattice,
    ContextLatticeCompileError,
    compileContextLattice,
    contextOrderDecl,
  )
import Moonlight.Sheaf.Context.Site (PreparedContextSupportError)
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Mem (performMajorGC)

type ArithTerm = Fix ArithF

type ArithGraph = EGraph ArithF NodeCount

type RegionGraph = ContextEGraph ArithF NodeCount ContextIndex

newtype ContextIndex = ContextIndex
  { contextIndexValue :: Int
  }
  deriving stock (Eq, Ord, Show)

data GridConfig = GridConfig
  { gridK :: !Int,
    gridN :: !Int
  }
  deriving stock (Eq, Ord, Show)

data WorkloadSpec = WorkloadSpec
  { workloadConfig :: !GridConfig,
    workloadContexts :: ![ContextIndex],
    workloadLattice :: !(ContextLattice ContextIndex),
    workloadTermPairs :: ![AuthoredTermPair]
  }

data AuthoredTermPair = AuthoredTermPair
  { authoredTermContext :: !ContextIndex,
    authoredLeftTerm :: !ArithTerm,
    authoredRightTerm :: !ArithTerm
  }

data AuthoredMerge = AuthoredMerge
  { authoredMergeContext :: !ContextIndex,
    authoredMergeLeft :: !ClassId,
    authoredMergeRight :: !ClassId
  }

data BuiltGraph = BuiltGraph
  { builtGraphMerges :: ![AuthoredMerge],
    builtGraphValue :: !ArithGraph
  }

data ProductArm = ProductArm
  { productArmConfig :: !GridConfig,
    productArmContexts :: ![ContextIndex],
    productArmProbeMerge :: !AuthoredMerge,
    productArmMerges :: ![AuthoredMerge],
    productArmGraphs :: !(Map ContextIndex ArithGraph)
  }

data RegionArm = RegionArm
  { regionArmConfig :: !GridConfig,
    regionArmContexts :: ![ContextIndex],
    regionArmProbeMerge :: !AuthoredMerge,
    regionArmMerges :: ![AuthoredMerge],
    regionArmGraph :: !RegionGraph
  }

data ArmName
  = ProductArmName
  | RegionArmName

data BaselineRow = BaselineRow
  { baselineRowArm :: !ArmName,
    baselineRowConfig :: !GridConfig,
    baselineRowMemBytes :: !Word64,
    baselineRowCommitUs :: !Integer,
    baselineRowRoundUs :: !Integer,
    baselineRowColdstartUs :: !Integer
  }

data Timed a = Timed
  { timedValue :: !a,
    timedMicros :: !Integer
  }

data BaselineError
  = InvalidGrid !GridConfig
  | ContextLatticeFailed !(ContextLatticeCompileError ContextIndex)
  | EmptyProductArm !GridConfig
  | EmptyRegionArm !GridConfig
  | RegionCommitFailed !GridConfig !(ContextDeltaError ArithF ContextIndex)
  | RegionSemanticQueryFailed !GridConfig !ContextIndex !(PreparedContextSupportError ContextIndex)
  | GraphClassIdAllocationFailed !GridConfig !UnionFindAllocationError
  | RtsStatsUnavailable
  deriving stock (Show)

main :: IO ()
main = do
  result <- runBaselineCsv
  case result of
    Right outputPath ->
      putStrLn ("wrote " <> outputPath)
    Left failure -> do
      hPutStrLn stderr (renderBaselineError failure)
      exitFailure

runBaselineCsv :: IO (Either BaselineError FilePath)
runBaselineCsv = do
  statsEnabled <- Stats.getRTSStatsEnabled
  if statsEnabled
    then do
      measuredRows <- traverse measureConfiguration baselineGrid
      case sequenceA measuredRows of
        Left failure ->
          pure (Left failure)
        Right rowGroups -> do
          createDirectoryIfMissing True baselineOutputDirectory
          writeFile baselineOutputPath (renderCsv (concat rowGroups))
          pure (Right baselineOutputPath)
    else pure (Left RtsStatsUnavailable)

baselineGrid :: [GridConfig]
baselineGrid =
  GridConfig <$> [1, 2, 4, 8] <*> [16, 64, 256]

baselineOutputDirectory :: FilePath
baselineOutputDirectory =
  "artifacts/paper/baselines"

baselineOutputPath :: FilePath
baselineOutputPath =
  "artifacts/paper/baselines/product-baseline.csv"

measureConfiguration :: GridConfig -> IO (Either BaselineError [BaselineRow])
measureConfiguration config =
  case buildWorkloadSpec config of
    Left failure ->
      pure (Left failure)
    Right workloadSpec -> do
      productMeasurement <- measureProductArm workloadSpec
      regionMeasurement <- measureRegionArm workloadSpec
      pure ((\productRow regionRow -> [productRow, regionRow]) <$> productMeasurement <*> regionMeasurement)

measureProductArm :: WorkloadSpec -> IO (Either BaselineError BaselineRow)
measureProductArm workloadSpec = do
  coldstart <- timePhase productColdDigest (pure (buildProductArm workloadSpec))
  case coldstart of
    Left failure ->
      pure (Left failure)
    Right timedColdstart -> do
      memBytes <- sampleLiveBytes productColdDigest (timedValue timedColdstart)
      commit <- timePhase productCommitDigest (pure (Right (commitProductProbe (timedValue timedColdstart))))
      roundValue <- timePhase productRoundDigest (pure (Right (runProductRound (timedValue timedColdstart))))
      pure
        ( productBaselineRow
            (timedValue timedColdstart)
            memBytes
            <$> fmap timedMicros commit
            <*> fmap timedMicros roundValue
            <*> Right (timedMicros timedColdstart)
        )

measureRegionArm :: WorkloadSpec -> IO (Either BaselineError BaselineRow)
measureRegionArm workloadSpec = do
  coldstart <- timePhase regionColdDigest (pure (buildRegionArm workloadSpec))
  case coldstart of
    Left failure ->
      pure (Left failure)
    Right timedColdstart -> do
      memBytes <- sampleLiveBytes regionColdDigest (timedValue timedColdstart)
      commit <- timePhase regionCommitDigest (pure (commitRegionProbe (timedValue timedColdstart)))
      roundValue <- timePhase regionRoundDigest (pure (runRegionRound (timedValue timedColdstart)))
      pure
        ( regionBaselineRow
            (timedValue timedColdstart)
            memBytes
            <$> fmap timedMicros commit
            <*> fmap timedMicros roundValue
            <*> Right (timedMicros timedColdstart)
        )

productBaselineRow ::
  ProductArm ->
  Word64 ->
  Integer ->
  Integer ->
  Integer ->
  BaselineRow
productBaselineRow productArm memBytes commitUs roundUs coldstartUs =
  BaselineRow
    { baselineRowArm = ProductArmName,
      baselineRowConfig = productArmConfig productArm,
      baselineRowMemBytes = memBytes,
      baselineRowCommitUs = commitUs,
      baselineRowRoundUs = roundUs,
      baselineRowColdstartUs = coldstartUs
    }

regionBaselineRow ::
  RegionArm ->
  Word64 ->
  Integer ->
  Integer ->
  Integer ->
  BaselineRow
regionBaselineRow regionArm memBytes commitUs roundUs coldstartUs =
  BaselineRow
    { baselineRowArm = RegionArmName,
      baselineRowConfig = regionArmConfig regionArm,
      baselineRowMemBytes = memBytes,
      baselineRowCommitUs = commitUs,
      baselineRowRoundUs = roundUs,
      baselineRowColdstartUs = coldstartUs
    }

buildWorkloadSpec :: GridConfig -> Either BaselineError WorkloadSpec
buildWorkloadSpec config
  | gridK config <= 0 || gridN config <= 0 =
      Left (InvalidGrid config)
  | otherwise =
      fmap
        ( \lattice ->
            WorkloadSpec
              { workloadConfig = config,
                workloadContexts = contexts,
                workloadLattice = lattice,
                workloadTermPairs = authoredTermPairs config
              }
        )
        (buildChainLattice config contexts)
  where
    contexts =
      fmap ContextIndex [0 .. gridK config - 1]

buildChainLattice ::
  GridConfig ->
  [ContextIndex] ->
  Either BaselineError (ContextLattice ContextIndex)
buildChainLattice config contexts =
  first ContextLatticeFailed $
    compileContextLattice
      (Set.fromList contexts)
      ( contextOrderDecl
          (ContextIndex (gridK config - 1))
          (ContextIndex 0)
          (zip contexts (drop 1 contexts))
      )

authoredTermPairs :: GridConfig -> [AuthoredTermPair]
authoredTermPairs config =
  fmap (authoredTermPairAt config) [0 .. gridN config - 1]

authoredTermPairAt :: GridConfig -> Int -> AuthoredTermPair
authoredTermPairAt config termIndex =
  AuthoredTermPair
    { authoredTermContext = ContextIndex (termIndex `mod` gridK config),
      authoredLeftTerm =
        addTermNode
          (numTerm termBase)
          (addTermNode (numTerm (termBase + 1)) (numTerm (termBase + 2))),
      authoredRightTerm =
        addTermNode
          (addTermNode (numTerm termBase) (numTerm (termBase + 1)))
          (numTerm (termBase + 3))
    }
  where
    termBase =
      termIndex * 4

buildProductArm :: WorkloadSpec -> Either BaselineError ProductArm
buildProductArm workloadSpec = do
  builtCopies <-
    first (GraphClassIdAllocationFailed (workloadConfig workloadSpec)) $
      traverse
        (\contextValue -> fmap ((,) contextValue) (buildArithGraph (workloadTermPairs workloadSpec)))
        (workloadContexts workloadSpec)
  case builtCopies of
    [] ->
      Left (EmptyProductArm (workloadConfig workloadSpec))
    (_, firstBuiltGraph) : _ ->
      case builtGraphMerges firstBuiltGraph of
        [] ->
          Left (EmptyProductArm (workloadConfig workloadSpec))
        probeMerge : _ ->
          Right
            ProductArm
              { productArmConfig = workloadConfig workloadSpec,
                productArmContexts = workloadContexts workloadSpec,
                productArmProbeMerge = probeMerge,
                productArmMerges = builtGraphMerges firstBuiltGraph,
                productArmGraphs = Map.fromList (fmap graphEntry builtCopies)
              }
  where
    graphEntry (contextValue, builtGraph) =
      (contextValue, builtGraphValue builtGraph)

buildRegionArm :: WorkloadSpec -> Either BaselineError RegionArm
buildRegionArm workloadSpec = do
  builtGraph <-
    first (GraphClassIdAllocationFailed (workloadConfig workloadSpec)) $
      buildArithGraph (workloadTermPairs workloadSpec)
  case builtGraphMerges builtGraph of
    [] ->
      Left (EmptyRegionArm (workloadConfig workloadSpec))
    probeMerge : _ ->
      Right
        RegionArm
          { regionArmConfig = workloadConfig workloadSpec,
            regionArmContexts = workloadContexts workloadSpec,
            regionArmProbeMerge = probeMerge,
            regionArmMerges = builtGraphMerges builtGraph,
            regionArmGraph =
              emptyContextEGraph
                (workloadLattice workloadSpec)
                (builtGraphValue builtGraph)
          }
buildArithGraph :: [AuthoredTermPair] -> Either UnionFindAllocationError BuiltGraph
buildArithGraph termPairs =
  fmap
    ( \(graphValue, reversedMergePairs) ->
        BuiltGraph
          { builtGraphMerges = reverse reversedMergePairs,
            builtGraphValue = graphValue
          }
    )
    (foldM insertAuthoredTermPair (emptyEGraph analysisSpec, []) termPairs)

insertAuthoredTermPair ::
  (ArithGraph, [AuthoredMerge]) ->
  AuthoredTermPair ->
  Either UnionFindAllocationError (ArithGraph, [AuthoredMerge])
insertAuthoredTermPair (graphValue, authoredMerges) authoredPair = do
  (leftClassId, leftGraph) <- addTerm (authoredLeftTerm authoredPair) graphValue
  (rightClassId, rightGraph) <- addTerm (authoredRightTerm authoredPair) leftGraph
  pure
    ( rightGraph,
      AuthoredMerge
          { authoredMergeContext = authoredTermContext authoredPair,
            authoredMergeLeft = leftClassId,
            authoredMergeRight = rightClassId
          }
        : authoredMerges
      )

commitProductProbe :: ProductArm -> ProductArm
commitProductProbe productArm =
  stageProductMerge (productArmProbeMerge productArm) productArm

runProductRound :: ProductArm -> ProductArm
runProductRound =
  rebuildProductArm . applyProductMerges

applyProductMerges :: ProductArm -> ProductArm
applyProductMerges productArm =
  foldl' (flip stageProductMerge) productArm (productArmMerges productArm)

stageProductMerge :: AuthoredMerge -> ProductArm -> ProductArm
stageProductMerge mergeValue productArm =
  productArm
    { productArmGraphs =
        foldl'
          (\graphs contextValue -> Map.adjust stageGraph contextValue graphs)
          (productArmGraphs productArm)
          (productSupportContexts (productArmContexts productArm) (authoredMergeContext mergeValue))
    }
  where
    stageGraph =
      enqueueEditDelta
        (classUnionsDelta [(authoredMergeLeft mergeValue, authoredMergeRight mergeValue)])

productSupportContexts :: [ContextIndex] -> ContextIndex -> [ContextIndex]
productSupportContexts contexts authoredContext =
  filter
    (\contextValue -> contextIndexValue authoredContext <= contextIndexValue contextValue)
    contexts

rebuildProductArm :: ProductArm -> ProductArm
rebuildProductArm productArm =
  productArm {productArmGraphs = Map.map rebuild (productArmGraphs productArm)}

commitRegionProbe :: RegionArm -> Either BaselineError RegionArm
commitRegionProbe regionArm =
  commitRegionMerge (regionArmProbeMerge regionArm) regionArm

runRegionRound :: RegionArm -> Either BaselineError RegionArm
runRegionRound regionArm = do
  nextArm <- foldM (flip commitRegionMerge) regionArm (regionArmMerges regionArm)
  semanticDigest <- regionSemanticQueryDigest nextArm
  semanticDigest `seq` pure nextArm

commitRegionMerge :: AuthoredMerge -> RegionArm -> Either BaselineError RegionArm
commitRegionMerge mergeValue regionArm =
  fmap
    (\graphValue -> regionArm {regionArmGraph = graphValue})
    ( first
        (RegionCommitFailed (regionArmConfig regionArm))
        ( contextMerge
            (authoredMergeContext mergeValue)
            (authoredMergeLeft mergeValue)
            (authoredMergeRight mergeValue)
            (regionArmGraph regionArm)
        )
    )

timePhase ::
  (a -> Int) ->
  IO (Either BaselineError a) ->
  IO (Either BaselineError (Timed a))
timePhase demand action = do
  start <- Clock.getMonotonicTimeNSec
  phaseResult <- action
  case phaseResult of
    Left failure ->
      pure (Left failure)
    Right value -> do
      forcedDigest <- evaluate (demand value)
      end <- Clock.getMonotonicTimeNSec
      pure
        ( forcedDigest
            `seq` Right
              ( Timed
                { timedValue = value,
                  timedMicros = nanosecondsToMicroseconds (end - start)
                }
              )
        )

sampleLiveBytes :: (a -> Int) -> a -> IO Word64
sampleLiveBytes demand value = do
  beforeDigest <- evaluate (demand value)
  performMajorGC
  stats <- Stats.getRTSStats
  afterDigest <- evaluate (demand value)
  pure
    ( beforeDigest
        `seq` afterDigest
        `seq` fromIntegral (Stats.gcdetails_live_bytes (Stats.gc stats))
    )

nanosecondsToMicroseconds :: Word64 -> Integer
nanosecondsToMicroseconds nanoseconds =
  fromIntegral nanoseconds `div` 1000

productColdDigest :: ProductArm -> Int
productColdDigest productArm =
  productMergeDigest productArm
    + Map.foldl'
      (\digest graphValue -> digest + arithGraphDigest graphValue)
      (length (productArmContexts productArm))
      (productArmGraphs productArm)

productCommitDigest :: ProductArm -> Int
productCommitDigest productArm =
  productColdDigest productArm
    + Map.foldl'
      (\digest graphValue -> digest + length (eGraphPendingClassUnions graphValue))
      0
      (productArmGraphs productArm)

productRoundDigest :: ProductArm -> Int
productRoundDigest =
  productCommitDigest

regionColdDigest :: RegionArm -> Int
regionColdDigest regionArm =
  regionMergeDigest regionArm
    + length (regionArmContexts regionArm)
    + fromIntegral (cegContextRevision (regionArmGraph regionArm))
    + arithGraphDigest (cegBase (regionArmGraph regionArm))

regionCommitDigest :: RegionArm -> Int
regionCommitDigest regionArm =
  regionColdDigest regionArm
    + foldl'
      ( \digest contextValue ->
          digest + length (contextAuthoredUnionPairs contextValue (regionArmGraph regionArm))
      )
      0
      (regionArmContexts regionArm)

regionRoundDigest :: RegionArm -> Int
regionRoundDigest =
  regionCommitDigest

regionSemanticQueryDigest :: RegionArm -> Either BaselineError Int
regionSemanticQueryDigest regionArm =
  fmap sum
    (traverse (contextSemanticDigest regionArm) (regionArmContexts regionArm))

contextSemanticDigest :: RegionArm -> ContextIndex -> Either BaselineError Int
contextSemanticDigest regionArm contextValue = do
  visibleClassKeys <-
    first queryFailure
      (contextVisibleClassKeys contextValue contextGraph)
  fmap sum
    (traverse classDigest (IntSet.toAscList visibleClassKeys))
  where
    contextGraph = regionArmGraph regionArm
    queryFailure =
      RegionSemanticQueryFailed (regionArmConfig regionArm) contextValue
    classDigest classKey = do
      representative <-
        first queryFailure
          (contextRepresentativeAt contextValue (ClassId classKey) contextGraph)
      analysisValue <-
        first queryFailure
          (contextAnalysisValueAt contextValue representative contextGraph)
      pure
        ( classKey
            + classIdKey representative
            + maybe 1 (\(NodeCount nodeCount) -> 2 + nodeCount) analysisValue
        )

productMergeDigest :: ProductArm -> Int
productMergeDigest =
  mergeDigest . productArmMerges

regionMergeDigest :: RegionArm -> Int
regionMergeDigest =
  mergeDigest . regionArmMerges

mergeDigest :: [AuthoredMerge] -> Int
mergeDigest =
  foldl'
    ( \digest mergeValue ->
        digest
          + contextIndexValue (authoredMergeContext mergeValue)
          + classIdKey (authoredMergeLeft mergeValue)
          + classIdKey (authoredMergeRight mergeValue)
    )
    0

arithGraphDigest :: ArithGraph -> Int
arithGraphDigest graphValue =
  eGraphNodeCount graphValue
    + eGraphClassCount graphValue
    + IntMap.foldl'
      (\digest (NodeCount nodeCount) -> digest + nodeCount)
      0
      (eGraphAnalysis graphValue)

renderCsv :: [BaselineRow] -> String
renderCsv rows =
  unlines (csvHeader : fmap renderBaselineRow rows)

csvHeader :: String
csvHeader =
  "arm,K,N,mem_bytes,commit_us,round_us,coldstart_us"

renderBaselineRow :: BaselineRow -> String
renderBaselineRow row =
  intercalate
    ","
    [ renderArmName (baselineRowArm row),
      show (gridK (baselineRowConfig row)),
      show (gridN (baselineRowConfig row)),
      show (baselineRowMemBytes row),
      show (baselineRowCommitUs row),
      show (baselineRowRoundUs row),
      show (baselineRowColdstartUs row)
    ]

renderArmName :: ArmName -> String
renderArmName armName =
  case armName of
    ProductArmName ->
      "product"
    RegionArmName ->
      "region"

renderBaselineError :: BaselineError -> String
renderBaselineError failure =
  case failure of
    InvalidGrid config ->
      "invalid product-baseline grid: " <> show config
    ContextLatticeFailed compileFailure ->
      "context lattice construction failed: " <> show compileFailure
    EmptyProductArm config ->
      "product arm has no authored merges: " <> show config
    EmptyRegionArm config ->
      "region arm has no authored merges: " <> show config
    RegionCommitFailed config commitFailure ->
      "region commit failed for " <> show config <> ": " <> show commitFailure
    RegionSemanticQueryFailed config contextValue queryFailure ->
      "region semantic query failed for "
        <> show config
        <> " at "
        <> show contextValue
        <> ": "
        <> show queryFailure
    GraphClassIdAllocationFailed config allocationError ->
      "graph class-id allocation failed for " <> show config <> ": " <> show allocationError
    RtsStatsUnavailable ->
      "RTS statistics are unavailable; run with -T or use the cabal target with its baked RTS opts"
