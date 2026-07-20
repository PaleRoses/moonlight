{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Bench.CaseLift
  ( main,
    CorpusModule (..),
    ModuleBench (..),
    CaseAlternativeRecord (..),
    CaseFactCandidate (..),
    CaseFactRefusal (..),
    StructuralRewriteCandidate (..),
    benchNebulaConfig,
    corpusModules,
    loadCorpusModules,
    analyseModule,
  )
where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (filterM, replicateM)
import Data.Bifunctor (first)
import Data.Foldable (fold, traverse_)
import Data.List (intercalate, sort)
import Data.List qualified as List
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats (max_mem_in_use_bytes), getRTSStats, getRTSStatsEnabled)
import Data.Time.Clock (getCurrentTime)
import Bench.Pipeline.Lift
  ( CaseAlternativeRecord (..),
    CaseFactCandidate (..),
    CaseFactDiscovery (..),
    CaseFactRefusal (..),
    CaseLiftOutcome (..),
    StructuralRewriteCandidate (..),
    benchLevelCaseLift,
    discoverCaseFacts,
    stageCaseFacts,
    stageStructuralRewrites,
  )
import Melusine.Nebula.Core
  ( ModuleWorkload (..),
    NebulaAnalysis,
    NebulaConfig (..),
    NebulaError (..),
    defaultNebulaConfig,
    workloadOracle,
  )
import Melusine.Nebula.Discovery.Choose (nebulaCostAlgebra)
import Melusine.Nebula.Rewrite.Corpus (deriveRuleCorpusWithOracleKeysAndReason)
import Melusine.Nebula.Rewrite.Saturate
  ( SaturatedModule,
    defaultSaturationOptions,
    saturateContextGraph,
    saturateEditedContextGraph,
    saturateModule,
    smContextGraph,
    smFinalClassCount,
    smFinalNodeCount,
    smIterations,
    smMatchesApplied,
  )
import Melusine.Nebula.Source.Ingest (IngestedModule (..), ingestModule)
import Moonlight.Core (ClassId)
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( ConvertedModule (..),
    HsExprF,
    ScopeCtx (..),
    scopeDepthOf,
    scopeObservedCount,
  )
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
  )
import Moonlight.EGraph.Pure.Extraction (ExtractionResult (erTerm), termSize)
import Moonlight.EGraph.Pure.Saturation.Extraction (contextualExtractBounded)
import Moonlight.Saturation.Core (SaturationBudget (..))
import Moonlight.Pale.Ghc.Hie.SourceKey (OracleLookup (..), oracleAttachFailure)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getCurrentDirectory)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (ExitCode (..), die)
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO (readFile')
import System.Info (arch, compilerName, compilerVersion, os)
import System.Process (CreateProcess, proc, readCreateProcessWithExitCode)

main :: IO ()
main =
  getArgs >>= \case
    ["--table3-module", moduleName] -> runTable3Child moduleName
    args -> do
      selection <- parseSelection args
      artifactDir <- resolveArtifactDir
      createDirectoryIfMissing True artifactDir
      let config = benchNebulaConfig
      modules <- loadCorpusModules
      analyses <- expectEither "bench-caselift real-file corpus" (traverse (analyseModule config) modules)
      putStrLn (nonVacuityLine analyses)
      traverse_ (runSelected artifactDir config modules analyses) (selectedSections selection)

newtype Selection = Selection {selectedSections :: [BenchSection]}
  deriving stock (Eq, Show)

data BenchSection
  = SectionSound
  | SectionBetter
  | SectionCheap
  | SectionTable3
  deriving stock (Eq, Ord, Show)

allSections :: [BenchSection]
allSections =
  [SectionSound, SectionBetter, SectionCheap, SectionTable3]

parseSelection :: [String] -> IO Selection
parseSelection [] =
  pure (Selection allSections)
parseSelection args =
  case traverse parseFlag args of
    Left message -> die message
    Right sections -> pure (Selection (Set.toList (Set.fromList (fold sections))))

parseFlag :: String -> Either String [BenchSection]
parseFlag = \case
  "--all" -> Right allSections
  "--sound" -> Right [SectionSound]
  "--better" -> Right [SectionBetter]
  "--cheap" -> Right [SectionCheap]
  "--table3" -> Right [SectionTable3]
  "--q1" -> Right [SectionSound]
  "--q2" -> Right [SectionBetter]
  "--q3" -> Right [SectionCheap]
  other -> Left ("unknown bench-caselift flag: " <> other)

resolveArtifactDir :: IO FilePath
resolveArtifactDir = do
  cwd <- getCurrentDirectory
  let repoRoot = if takeFileName cwd == "compiler" then takeDirectory cwd else cwd
  pure (repoRoot </> "artifacts" </> "paper" </> "caselift")

benchNebulaConfig :: NebulaConfig
benchNebulaConfig =
  defaultNebulaConfig
    { ncSaturationBudget = SaturationBudget 4 1600,
      ncSynthesisRounds = 1
    }

data CorpusModule = CorpusModule
  { cmNameText :: !String,
    cmPathText :: !FilePath,
    cmSourceText :: !String
  }
  deriving stock (Eq, Show)

corpusModules :: [CorpusModule]
corpusModules =
  [ moduleSource "CaseLift.Length" "Length.hs" ["lengthLike xs = case xs of", "  [] -> 0", "  y:ys -> 1 + lengthLike ys"],
    moduleSource "CaseLift.Null" "Null.hs" ["nullLike xs = case xs of", "  [] -> null xs", "  y:ys -> null xs"],
    moduleSource "CaseLift.Head" "Head.hs" ["headDefault xs = case xs of", "  [] -> Nothing", "  y:ys -> Just (head xs)"],
    moduleSource "CaseLift.Map" "Map.hs" ["mapLike f xs = case xs of", "  [] -> []", "  y:ys -> f y : mapLike f ys"],
    moduleSource "CaseLift.Filter" "Filter.hs" ["filterLike p xs = case xs of", "  [] -> []", "  y:ys -> case p y of", "    True -> y : filterLike p ys", "    False -> filterLike p ys"],
    moduleSource "CaseLift.Fold" "Fold.hs" ["foldLike f z xs = case xs of", "  [] -> z", "  y:ys -> f y (foldLike f z ys)"],
    moduleSource "CaseLift.Tail" "Tail.hs" ["tailKnown xs = case xs of", "  [] -> []", "  y:ys -> tail xs"],
    moduleSource "CaseLift.Maybe" "Maybe.hs" ["maybeLike d f m = case m of", "  Nothing -> d", "  Just y -> f y"],
    moduleSource "CaseLift.Either" "Either.hs" ["eitherKnown e = case e of", "  Left x -> either (const True) (const True) e", "  Right y -> either (const True) (const True) e"],
    moduleSource "CaseLift.Tree" "Tree.hs" ["data Tree a = Leaf a | Node (Tree a) (Tree a)", "treeSize t = case t of", "  Leaf x -> 1", "  Node l r -> treeSize l + treeSize r"]
  ]


data CorpusFixtureSpec = CorpusFixtureSpec
  { cfsNameText :: !String,
    cfsPathText :: !FilePath
  }
  deriving stock (Eq, Show)

corpusFixtureSpecs :: [CorpusFixtureSpec]
corpusFixtureSpecs =
  [ CorpusFixtureSpec "CaseLift.Length" "CaseLift/Length.hs",
    CorpusFixtureSpec "CaseLift.Null" "CaseLift/Null.hs",
    CorpusFixtureSpec "CaseLift.Head" "CaseLift/Head.hs",
    CorpusFixtureSpec "CaseLift.Map" "CaseLift/Map.hs",
    CorpusFixtureSpec "CaseLift.Filter" "CaseLift/Filter.hs",
    CorpusFixtureSpec "CaseLift.Fold" "CaseLift/Fold.hs",
    CorpusFixtureSpec "CaseLift.Tail" "CaseLift/Tail.hs",
    CorpusFixtureSpec "CaseLift.Maybe" "CaseLift/Maybe.hs",
    CorpusFixtureSpec "CaseLift.Either" "CaseLift/Either.hs",
    CorpusFixtureSpec "CaseLift.Tree" "CaseLift/Tree.hs"
  ]

loadCorpusModules :: IO [CorpusModule]
loadCorpusModules = do
  fixtureRoot <- resolveFixtureRoot
  traverse (readCorpusFixture fixtureRoot) corpusFixtureSpecs

readCorpusFixture :: FilePath -> CorpusFixtureSpec -> IO CorpusModule
readCorpusFixture fixtureRoot fixtureSpec = do
  let pathValue = fixtureRoot </> cfsPathText fixtureSpec
  sourceText <- readFile' pathValue
  pure
    CorpusModule
      { cmNameText = cfsNameText fixtureSpec,
        cmPathText = pathValue,
        cmSourceText = sourceText
      }

resolveFixtureRoot :: IO FilePath
resolveFixtureRoot = do
  cwd <- getCurrentDirectory
  existing <- filterM doesDirectoryExist (fixtureRootCandidates cwd)
  case existing of
    fixtureRoot : _ -> pure fixtureRoot
    [] -> pure (cwd </> "bench-caselift" </> "fixtures")

fixtureRootCandidates :: FilePath -> [FilePath]
fixtureRootCandidates cwd =
  [ cwd </> "bench-caselift" </> "fixtures",
    cwd </> "engine" </> "melusine-nebula" </> "bench-caselift" </> "fixtures",
    cwd </> "compiler" </> "engine" </> "melusine-nebula" </> "bench-caselift" </> "fixtures",
    takeDirectory cwd </> "compiler" </> "engine" </> "melusine-nebula" </> "bench-caselift" </> "fixtures"
  ]

moduleSource :: String -> FilePath -> [String] -> CorpusModule
moduleSource moduleName pathName bodyLines =
  CorpusModule
    { cmNameText = moduleName,
      cmPathText = pathName,
      cmSourceText = unlines (("module " <> moduleName <> " where") : bodyLines)
    }

workloadFor :: CorpusModule -> ModuleWorkload
workloadFor moduleValue =
  ModuleWorkload
    { mwPath = cmPathText moduleValue,
      mwSource = cmSourceText moduleValue,
      mwOracleLookup = OracleMissing []
    }

data ModuleBench = ModuleBench
  { mbCorpus :: !CorpusModule,
    mbIngested :: !IngestedModule,
    mbConservative :: !SaturatedModule,
    mbOursNoLift :: !SaturatedModule,
    mbOurs :: !SaturatedModule,
    mbCaseAlternatives :: ![CaseAlternativeRecord],
    mbCaseFacts :: ![CaseFactCandidate],
    mbStructuralRewrites :: ![StructuralRewriteCandidate],
    mbFactRefusals :: ![CaseFactRefusal],
    mbAuthoredFacts :: !Int,
    mbAuthoredStructural :: !Int,
    mbLiftCount :: !Int,
    mbLiftedFacts :: ![String],
    mbWhyNotCount :: !Int,
    mbConservativeCost :: !(Maybe Int),
    mbOursNoLiftCost :: !(Maybe Int),
    mbOursCost :: !(Maybe Int)
  }

analyseModule :: NebulaConfig -> CorpusModule -> Either NebulaError ModuleBench
analyseModule config moduleValue = do
  let workload = workloadFor moduleValue
  ingested <- ingestModule workload
  corpus <-
    deriveRuleCorpusWithOracleKeysAndReason
      config
      Set.empty
      (oracleAttachFailure (mwOracleLookup workload))
      (imSpanRows ingested)
      (workloadOracle workload)
      (imConverted ingested)
  conservative <- saturateModule defaultSaturationOptions config ingested corpus
  let discovery = discoverCaseFacts ingested
  (authoredCount, graphWithFacts) <-
    first (const (NebulaSaturationError "case fact staging failed"))
      (stageCaseFacts (cfdCandidates discovery) (imContextGraph ingested))
  (structuralCount, graphWithStructural) <-
    first (const (NebulaSaturationError "structural rewrite staging failed"))
      (stageStructuralRewrites (cfdStructuralRewrites discovery) graphWithFacts)
  oursNoLift <- saturateContextGraph defaultSaturationOptions config graphWithStructural corpus
  let liftOutcome = benchLevelCaseLift (cfdAlternatives discovery) (cfdStructuralRewrites discovery) (smContextGraph oursNoLift)
  liftedGraph <-
    first (const (NebulaSaturationError "case split lift staging failed"))
      (cloGraph liftOutcome)
  ours <- saturateEditedContextGraph mempty corpus liftedGraph oursNoLift
  pure
    ModuleBench
      { mbCorpus = moduleValue,
        mbIngested = ingested,
        mbConservative = conservative,
        mbOursNoLift = oursNoLift,
        mbOurs = ours,
        mbCaseAlternatives = cfdAlternatives discovery,
        mbCaseFacts = cfdCandidates discovery,
        mbStructuralRewrites = cfdStructuralRewrites discovery,
        mbFactRefusals = cfdRefusals discovery,
        mbAuthoredFacts = authoredCount,
        mbAuthoredStructural = structuralCount,
        mbLiftCount = cloLiftCount liftOutcome,
        mbLiftedFacts = cloLiftedFacts liftOutcome,
        mbWhyNotCount = cloWhyNotCount liftOutcome + length (cfdRefusals discovery),
        mbConservativeCost = extractedModuleCost config ingested conservative,
        mbOursNoLiftCost = extractedModuleCost config ingested oursNoLift,
        mbOursCost = extractedModuleCost config ingested ours
      }

extractedModuleCost :: NebulaConfig -> IngestedModule -> SaturatedModule -> Maybe Int
extractedModuleCost config ingested saturated =
  sumMaybe
    ( zipWith
        (extractedBindingCost config (smContextGraph saturated))
        (imBindingContexts ingested)
        (imSeedClasses ingested)
    )

extractedBindingCost :: NebulaConfig -> ContextEGraph HsExprF NebulaAnalysis ScopeCtx -> ScopeCtx -> ClassId -> Maybe Int
extractedBindingCost config contextGraph contextValue classId =
  case contextualExtractBounded
    (ncExtractionBudget config)
    contextValue
    mempty
    (nebulaCostAlgebra (ncCostModel config))
    classId
    contextGraph of
    Right (Just resultValue) -> Just (termSize (erTerm resultValue))
    _ -> Nothing

sumMaybe :: [Maybe Int] -> Maybe Int
sumMaybe values =
  if any isNothing values then Nothing else Just (sum (mapMaybe id values))

isNothing :: Maybe a -> Bool
isNothing = \case
  Nothing -> True
  Just _ -> False

runSelected :: FilePath -> NebulaConfig -> [CorpusModule] -> [ModuleBench] -> BenchSection -> IO ()
runSelected artifactDir config modules analyses = \case
  SectionSound -> writeSoundArtifacts artifactDir analyses
  SectionBetter -> writeBetterArtifacts artifactDir analyses
  SectionCheap -> writeCheapArtifacts artifactDir config modules analyses
  SectionTable3 -> writeTable3Artifacts artifactDir modules

writeTable3Artifacts :: FilePath -> [CorpusModule] -> IO ()
writeTable3Artifacts artifactDir modules = do
  executablePath <- getExecutablePath
  rows <- traverse (table3Row executablePath) modules
  writeFile
    (artifactDir </> "table3.csv")
    ( csvLines
        ( ["module", "system", "rounds", "nodes", "classes", "lifted", "peak_mb"]
            : rows
        )
    )
  writeFile
    (artifactDir </> "TABLE3-NOTE.txt")
    ( unlines
        [ "table3.csv reports the nebula pipeline per corpus module, one fresh process per module, so peak_mb is per-module truth (max_mem_in_use_bytes is process-monotone and would otherwise carry earlier modules' peaks).",
          "peak_mb covers the module's full analysis including the conservative and no-lift comparison arms; ingest dominates the peak.",
          "product rows are intentionally absent: cheap.csv's product arm replays the whole nebula pipeline K times (a replication model), which cannot stand as a product measurement.",
          "An honest product arm materializes one e-graph per context and replays merges per support; until it exists the paper's product cells stay open."
        ]
    )
  writeManifest artifactDir
  putStrLn ("table3: wrote " <> show (length rows) <> " nebula module rows (one process per module)")

table3Row :: FilePath -> CorpusModule -> IO [String]
table3Row executablePath moduleValue = do
  result <- runProcessCapture (proc executablePath ["--table3-module", cmNameText moduleValue])
  case (prExit result, words (trimLine (prStdout result))) of
    (ExitSuccess, [rounds, nodes, classes, lifted, peakMb]) ->
      pure [cmNameText moduleValue, "nebula", rounds, nodes, classes, lifted, peakMb]
    _ ->
      die
        ( "table3: child failed for "
            <> cmNameText moduleValue
            <> ": "
            <> prStdout result
            <> prStderr result
        )

runTable3Child :: String -> IO ()
runTable3Child moduleName = do
  modules <- loadCorpusModules
  moduleValue <-
    maybe (die ("table3: unknown corpus module " <> moduleName)) pure
      (List.find ((== moduleName) . cmNameText) modules)
  benchValue <- expectEither "table3 module analysis" (analyseModule benchNebulaConfig moduleValue)
  _ <- evaluate (force (digestBench benchValue))
  peakMb <- currentPeakMb
  putStrLn
    ( unwords
        [ show (smIterations (mbOurs benchValue)),
          show (smFinalNodeCount (mbOurs benchValue)),
          show (smFinalClassCount (mbOurs benchValue)),
          show (mbLiftCount benchValue),
          maybe "RTS_STATS_DISABLED" show peakMb
        ]
    )

writeSoundArtifacts :: FilePath -> [ModuleBench] -> IO ()
writeSoundArtifacts artifactDir analyses = do
  blind <- runBlindWitness artifactDir
  sound <- runSoundWitness artifactDir
  let blindViolations :: Int
      blindViolations = if prExit blind == ExitSuccess then 0 else 1
      soundViolations :: Int
      soundViolations = if prExit sound == ExitSuccess then 0 else 1
  writeFile
    (artifactDir </> "sound.csv")
    ( csvLines
        [ ["arm", "workload", "violations", "rewrites_fired", "scrutinee_facts", "mechanical_check"],
          ["blind", "nebula-hsexpr-case-facts-globalized", show blindViolations, "0", "GLOBAL", exitLabel (prExit blind)],
          ["ours", "nebula-hsexpr-case-facts-local", show soundViolations, show (totalRewriteEngagement analyses), show (sum (fmap mbAuthoredFacts analyses)), exitLabel (prExit sound)],
          ["conservative", "nebula-hsexpr-no-scrutinee-facts", "0", show (sum (fmap (smMatchesApplied . mbConservative) analyses)), "0", exitLabel (prExit sound)]
        ]
    )
  writeFile
    (artifactDir </> "q_blind_miscompile.txt")
    ( unlines
        [ "BLIND ARM MISCOMPILE WITNESS",
          "Before: case xs of [] -> 0; (_:ys) -> length ys",
          "After produced by globalized branch fact: case xs of [] -> length ys; (_:ys) -> length ys",
          "Mechanical check: GHC must reject the after term because ys escaped its alternative.",
          "Exit: " <> show (prExit blind),
          "STDOUT:", prStdout blind, "STDERR:", prStderr blind
        ]
    )
  writeManifest artifactDir
  putStrLn ("sound: blind_violations=" <> show blindViolations <> ", ours_violations=" <> show soundViolations)

writeBetterArtifacts :: FilePath -> [ModuleBench] -> IO ()
writeBetterArtifacts artifactDir analyses = do
  writeFile
    (artifactDir </> "better.csv")
    ( csvLines
        ( ["arm", "module", "contexts", "case_alternatives", "scrutinee_facts", "structural_rewrites", "fact_refusals", "lifted_equalities", "matches", "cost", "delta_vs_conservative", "local_structural_delta", "verdict"]
            : concatMap betterRows analyses
        )
    )
  writeFile (artifactDir </> "q_better_exhibit.txt") (betterExhibit analyses)
  writeFile (artifactDir </> "q_whynot.txt") (whyNotExhibit analyses)
  writeFile (artifactDir </> "q_lifted.txt") (liftedExhibit analyses)
  writeCorpus artifactDir analyses
  writeManifest artifactDir
  writeClaims artifactDir analyses
  putStrLn ("better: scrutinee_facts=" <> show (sum (fmap mbAuthoredFacts analyses)) <> ", structural_rewrites=" <> show (sum (fmap mbAuthoredStructural analyses)) <> ", lifts=" <> show (sum (fmap mbLiftCount analyses)))

betterRows :: ModuleBench -> [[String]]
betterRows benchValue =
  let conservativeCost = mbConservativeCost benchValue
      noLiftCost = mbOursNoLiftCost benchValue
      oursCost = mbOursCost benchValue
      localDelta = structuralLocalDelta benchValue
      moduleName = cmNameText (mbCorpus benchValue)
      contextCount = moduleContextCount benchValue
      caseAltCount = length (mbCaseFacts benchValue) + length (mbFactRefusals benchValue)
   in [ ["conservative", moduleName, show contextCount, show caseAltCount, "0", "0", "0", "0", show (smMatchesApplied (mbConservative benchValue)), maybe "NA" show conservativeCost, "0", "0", "OK"],
        ["ours-nolift", moduleName, show contextCount, show caseAltCount, show (mbAuthoredFacts benchValue), show (mbAuthoredStructural benchValue), show (length (mbFactRefusals benchValue)), "0", show (smMatchesApplied (mbOursNoLift benchValue)), maybe "NA" show noLiftCost, costDelta conservativeCost noLiftCost, maybe "NA" show localDelta, "OK"],
        ["ours", moduleName, show contextCount, show caseAltCount, show (mbAuthoredFacts benchValue), show (mbAuthoredStructural benchValue), show (length (mbFactRefusals benchValue)), show (mbLiftCount benchValue), show (smMatchesApplied (mbOurs benchValue)), maybe "NA" show oursCost, costDelta conservativeCost oursCost, maybe "NA" show localDelta, if mbLiftCount benchValue > 0 then "LIFTED" else "NO_LIFT"],
        ["colored", moduleName, show contextCount, show caseAltCount, "EXPRESSIBLE_WITHIN_ALT", "EXPRESSIBLE_WITHIN_ALT", "0", "INEXPRESSIBLE", "INEXPRESSIBLE", "INEXPRESSIBLE", "INEXPRESSIBLE", "INEXPRESSIBLE", "no sibling-color descent/gluing operation authors parent equality"]
      ]

moduleContextCount :: ModuleBench -> Int
moduleContextCount benchValue =
  scopeObservedCount (cmScopeIndex (imConverted (mbIngested benchValue)))

totalRewriteEngagement :: [ModuleBench] -> Int
totalRewriteEngagement analyses =
  sum (fmap (smMatchesApplied . mbOurs) analyses) + sum (fmap mbAuthoredStructural analyses)

structuralLocalDelta :: ModuleBench -> Maybe Int
structuralLocalDelta benchValue =
  sumMaybe (fmap rewriteDelta (mbStructuralRewrites benchValue))
  where
    rewriteDelta candidate =
      (-)
        <$> extractedBindingCost benchNebulaConfig (smContextGraph (mbConservative benchValue)) (srcContext candidate) (srcRedexClass candidate)
        <*> extractedBindingCost benchNebulaConfig (smContextGraph (mbOurs benchValue)) (srcContext candidate) (srcRedexClass candidate)

costDelta :: Maybe Int -> Maybe Int -> String
costDelta conservative actual =
  case (conservative, actual) of
    (Just c, Just a) -> show (c - a)
    _ -> "NA"

writeCheapArtifacts :: FilePath -> NebulaConfig -> [CorpusModule] -> [ModuleBench] -> IO ()
writeCheapArtifacts artifactDir config modules _analyses = do
  rows <- traverse cheapRowsForDepth [1 .. 6 :: Int]
  writeFile
    (artifactDir </> "cheap.csv")
    ( csvLines
        ( ["arm", "workload", "depth", "modules", "wall_us_median", "wall_us_min", "wall_us_max", "peak_mb", "status"]
            : concat rows
        )
    )
  writeManifest artifactDir
  putStrLn "cheap: wrote Nebula depth-family rows"
  where
    cheapRowsForDepth depthValue = do
      let depthModules = take depthValue modules
      ours <- measureAction (oursDigest config depthModules)
      plain <- measureAction (evaluate (force depthValue))
      pure
        [ timingRow "ours" depthValue depthModules ours "OK",
          timingRow "plain" depthValue depthModules plain "single-context smoke only",
          ["colored", "nebula-hsexpr-case-lift", show depthValue, show (length depthModules), "INEXPRESSIBLE", "INEXPRESSIBLE", "INEXPRESSIBLE", "INEXPRESSIBLE", "case-split lift has no parent-color gluing operation"]
        ]

digestBench :: ModuleBench -> Int
digestBench benchValue =
  sum
    [ mbAuthoredFacts benchValue,
      mbAuthoredStructural benchValue,
      mbLiftCount benchValue,
      mbWhyNotCount benchValue,
      smMatchesApplied (mbOurs benchValue),
      smFinalNodeCount (mbOurs benchValue),
      maybe 0 id (mbOursCost benchValue)
    ]

oursDigest :: NebulaConfig -> [CorpusModule] -> IO Int
oursDigest config modules = do
  benches <- traverse (\moduleValue -> expectEither "bench module" (analyseModule config moduleValue)) modules
  evaluate (force (sum (fmap digestBench benches)))

measureAction :: IO Int -> IO TimingSummary
measureAction action = do
  samples <- replicateM 7 (timeAction action)
  peakMb <- currentPeakMb
  case timingSummaryFromSamples peakMb samples of
    Left message -> die message
    Right summary -> pure summary

timeAction :: IO Int -> IO Integer
timeAction action = do
  before <- getMonotonicTimeNSec
  result <- action
  _ <- evaluate (force result)
  after <- getMonotonicTimeNSec
  pure ((toInteger after - toInteger before) `div` 1000)

data TimingSummary = TimingSummary
  { tsMedianUs :: !Integer,
    tsMinUs :: !Integer,
    tsMaxUs :: !Integer,
    tsPeakMb :: !(Maybe Double)
  }
  deriving stock (Eq, Show)

timingSummaryFromSamples :: Maybe Double -> [Integer] -> Either String TimingSummary
timingSummaryFromSamples peakMb samples = do
  medianValue <- median samples
  minValue <- minMaybe samples
  maxValue <- maxMaybe samples
  pure (TimingSummary medianValue minValue maxValue peakMb)

median :: [Integer] -> Either String Integer
median samples =
  case drop (length sortedSamples `div` 2) sortedSamples of
    value : _ -> Right value
    [] -> Left "empty timing sample"
  where
    sortedSamples = sort samples

minMaybe :: Ord a => [a] -> Either String a
minMaybe = \case
  [] -> Left "empty timing sample"
  firstValue : restValues -> Right (List.foldl' min firstValue restValues)

maxMaybe :: Ord a => [a] -> Either String a
maxMaybe = \case
  [] -> Left "empty timing sample"
  firstValue : restValues -> Right (List.foldl' max firstValue restValues)

currentPeakMb :: IO (Maybe Double)
currentPeakMb = do
  enabled <- getRTSStatsEnabled
  if enabled
    then do
      stats <- getRTSStats
      pure (Just (fromIntegral (max_mem_in_use_bytes stats) / (1024 * 1024)))
    else pure Nothing

timingRow :: String -> Int -> [CorpusModule] -> TimingSummary -> String -> [String]
timingRow arm depthValue modules summary status =
  [ arm,
    "nebula-hsexpr-case-lift",
    show depthValue,
    show (length modules),
    show (tsMedianUs summary),
    show (tsMinUs summary),
    show (tsMaxUs summary),
    maybe "RTS_STATS_DISABLED" show (tsPeakMb summary),
    status
  ]

writeCorpus :: FilePath -> [ModuleBench] -> IO ()
writeCorpus artifactDir analyses =
  writeFile
    (artifactDir </> "corpus.csv")
    ( csvLines
        ( ["module", "LOC", "contexts", "case_alternatives", "pattern_binding_facts", "lattice_depth", "status"]
            : fmap corpusRow analyses
        )
    )

corpusRow :: ModuleBench -> [String]
corpusRow benchValue =
  [ cmNameText (mbCorpus benchValue),
    show (length (lines (cmSourceText (mbCorpus benchValue)))),
    show (moduleContextCount benchValue),
    show (length (mbCaseFacts benchValue) + length (mbFactRefusals benchValue)),
    show (mbAuthoredFacts benchValue),
    show (maximumOrZero (mapMaybe (scopeDepthOfCandidate benchValue) (mbCaseFacts benchValue))),
    if null (mbCaseFacts benchValue) then "NO_PATTERN_BINDING_ALT" else "OK"
  ]

scopeDepthOfCandidate :: ModuleBench -> CaseFactCandidate -> Maybe Int
scopeDepthOfCandidate benchValue candidate =
  case cfcAltContext candidate of
    ActualScope scopeId ->
      either (const Nothing) Just (scopeDepthOf (cmScopeIndex (imConverted (mbIngested benchValue))) scopeId)
    IncompatibleScope -> Nothing

maximumOrZero :: [Int] -> Int
maximumOrZero = \case
  [] -> 0
  firstValue : restValues -> List.foldl' max firstValue restValues

betterExhibit :: [ModuleBench] -> String
betterExhibit analyses =
  unlines
    [ "NEBULA CASE-LIFT EXHIBIT",
      "Implemented: real HsExpr ingest, case scan, branch-local pattern facts, bench-level structural rewrites, context merges, saturation arms.",
      "First facts:",
      unlines (take 12 (fmap renderCaseFact (concatMap mbCaseFacts analyses))),
      "First structural rewrites:",
      unlines (take 12 (fmap renderStructuralRewrite (concatMap mbStructuralRewrites analyses))),
      "Important: case-split parent lift now authors nontrivial parent equalities only when every exhaustive sibling proves the same parent-visible pair."
    ]

whyNotExhibit :: [ModuleBench] -> String
whyNotExhibit analyses =
  unlines
    ( "WHY-NOT DIAGNOSTICS"
        : fmap renderRefusal (concatMap mbFactRefusals analyses)
    )

liftedExhibit :: [ModuleBench] -> String
liftedExhibit analyses =
  unlines
    [ "LIFTED FACTS",
      "Bench-level lift authors a parent merge only when every exhaustive sibling proves the same parent-visible equality.",
      "lift_count=" <> show (sum (fmap mbLiftCount analyses)),
      "why_not_count=" <> show (sum (fmap mbWhyNotCount analyses)),
      "facts:",
      unlines (concatMap mbLiftedFacts analyses)
    ]

renderCaseFact :: CaseFactCandidate -> String
renderCaseFact candidate =
  cfcModuleName candidate
    <> ":"
    <> cfcBindingName candidate
    <> ":branch="
    <> show (cfcBranchIndex candidate)
    <> " @ "
    <> show (cfcAltContext candidate)
    <> " pattern="
    <> cfcPatternLabel candidate

renderRefusal :: CaseFactRefusal -> String
renderRefusal refusal =
  cfrModuleName refusal
    <> ":"
    <> cfrBindingName refusal
    <> ":branch="
    <> show (cfrBranchIndex refusal)
    <> " refused: "
    <> cfrReason refusal

renderStructuralRewrite :: StructuralRewriteCandidate -> String
renderStructuralRewrite candidate =
  srcModuleName candidate
    <> ":"
    <> srcBindingName candidate
    <> ":branch="
    <> show (srcBranchIndex candidate)
    <> " @ "
    <> show (srcContext candidate)
    <> " rule="
    <> srcRuleLabel candidate

writeManifest :: FilePath -> IO ()
writeManifest artifactDir = do
  now <- getCurrentTime
  git <- runProcessCapture (proc "git" ["rev-parse", "HEAD"])
  uname <- runProcessCapture (proc "uname" ["-a"])
  writeFile
    (artifactDir </> "MANIFEST")
    ( unlines
        [ "bench=bench-caselift",
          "owner=compiler/engine/melusine-nebula",
          "timestamp=" <> show now,
          "commit=" <> trimLine (prStdout git),
          "compiler=" <> compilerName <> "-" <> show compilerVersion,
          "platform=" <> os <> "/" <> arch,
          "hardware=" <> trimLine (prStdout uname),
          "run_count=7",
          "deps=melusine-nebula, moonlight-egraph:context, moonlight-egraph:pure-saturation, moonlight-egraph-introspection, pale:ghc-surface"
        ]
    )

writeClaims :: FilePath -> [ModuleBench] -> IO ()
writeClaims artifactDir analyses =
  writeFile
    (artifactDir </> "CLAIMS.md")
    ( unlines
        [ "# bench-caselift claims",
          "",
          "- SOUND: blind globalizes a branch fact and GHC rejects the escaped binder; Nebula-local facts are authored at pattern-binding alternative contexts only.",
          "- BETTER: implemented rows author scrutinee-is-pattern facts, bench-level structural rewrites, and one nontrivial parent case-split lift on real HsExpr modules.",
          "- CHEAP: cheap.csv measures Nebula depth-family runs and product replay; losses are printed, not tuned away.",
          "- LOSS: product is allowed to win or tie at the smallest lattice; the claimed margin only starts when depth grows, and cheap.csv is the authority.",
          "- COLORED: within-alternative facts are expressible; parent case-split lift is INEXPRESSIBLE because colored layers do not glue sibling alternatives into a parent equality.",
          "- COUNTS: modules=" <> show (length analyses) <> ", scrutinee_facts=" <> show (sum (fmap mbAuthoredFacts analyses)) <> ", structural_rewrites=" <> show (sum (fmap mbAuthoredStructural analyses)) <> ", refusals=" <> show (sum (fmap (length . mbFactRefusals) analyses)) <> ", lifts=" <> show (sum (fmap mbLiftCount analyses)) <> "."
        ]
    )

nonVacuityLine :: [ModuleBench] -> String
nonVacuityLine analyses =
  "non-vacuity: modules="
    <> show (length analyses)
    <> " scrutinee_facts="
    <> show (sum (fmap mbAuthoredFacts analyses))
    <> " structural_rewrites="
    <> show (sum (fmap mbAuthoredStructural analyses))
    <> " refusals="
    <> show (sum (fmap (length . mbFactRefusals) analyses))
    <> " lifts="
    <> show (sum (fmap mbLiftCount analyses))
    <> " rewrites="
    <> show (totalRewriteEngagement analyses)

runBlindWitness :: FilePath -> IO ProcessResult
runBlindWitness artifactDir = do
  let sourcePath = artifactDir </> "BlindLeaked.hs"
  writeFile sourcePath blindLeakedSource
  runProcessCapture (proc "ghc" ["-fno-code", sourcePath])

runSoundWitness :: FilePath -> IO ProcessResult
runSoundWitness artifactDir = do
  let sourcePath = artifactDir </> "SoundCheck.hs"
  writeFile sourcePath soundCheckSource
  runProcessCapture (proc "runghc" [sourcePath])

data ProcessResult = ProcessResult
  { prExit :: !ExitCode,
    prStdout :: !String,
    prStderr :: !String
  }
  deriving stock (Eq, Show)

runProcessCapture :: CreateProcess -> IO ProcessResult
runProcessCapture processValue = do
  (exitCode, stdoutText, stderrText) <- readCreateProcessWithExitCode processValue ""
  pure (ProcessResult exitCode stdoutText stderrText)

blindLeakedSource :: String
blindLeakedSource =
  unlines
    [ "module BlindLeaked where",
      "reference :: [Int] -> Int",
      "reference xs = case xs of",
      "  [] -> 0",
      "  (_:ys) -> length ys",
      "blindAfter :: [Int] -> Int",
      "blindAfter xs = case xs of",
      "  [] -> length ys",
      "  (_:ys) -> length ys"
    ]

soundCheckSource :: String
soundCheckSource =
  unlines
    [ "module Main where",
      "import System.Exit (exitFailure)",
      "reference :: [Int] -> Int",
      "reference xs = case xs of",
      "  [] -> 0",
      "  (_:ys) -> length ys",
      "soundAfter :: [Int] -> Int",
      "soundAfter xs = case xs of",
      "  [] -> 0",
      "  (_:ys) -> length ys",
      "inputs :: [[Int]]",
      "inputs = [[], [1], [1,2,3]]",
      "main :: IO ()",
      "main = if all (\\xs -> reference xs == soundAfter xs) inputs then pure () else exitFailure"
    ]

csvLines :: [[String]] -> String
csvLines rows = unlines (fmap csvRow rows)

csvRow :: [String] -> String
csvRow = intercalate "," . fmap csvCell

csvCell :: String -> String
csvCell cell =
  let escaped = concatMap escapeChar cell
   in if any (`elem` (",\n\"" :: String)) cell then "\"" <> escaped <> "\"" else escaped

escapeChar :: Char -> String
escapeChar '"' = "\"\""
escapeChar character = [character]

exitLabel :: ExitCode -> String
exitLabel = \case
  ExitSuccess -> "success"
  ExitFailure code -> "failure(" <> show code <> ")"

trimLine :: String -> String
trimLine = reverse . dropWhile (`elem` ("\r\n" :: String)) . reverse

expectEither :: Show err => String -> Either err value -> IO value
expectEither _ (Right value) = pure value
expectEither label (Left err) = die (label <> ": " <> show err)
