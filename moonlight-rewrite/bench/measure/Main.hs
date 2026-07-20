{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Fixture
  ( BenchSig,
    benchProgram,
    benchTerms,
  )
import Control.DeepSeq (force)
import Control.Exception (IOException, evaluate, try)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode)
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (ord)
import Data.Fix (Fix (..), foldFix)
import Data.Foldable (traverse_)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Proxy (Proxy)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.IO qualified as TextIO
import Data.Word (Word64)
import Data.Version (showVersion)
import GHC.Generics (Generic)
import Moonlight.Core
  ( ClassId (..),
    HasConstructorTag (..),
    Pattern (..),
    PatternVar,
    RewriteRuleId (..),
    ZipMatch (..),
    classIdKey,
    emptySubstitution,
    mkPatternVar,
    patternVarKey,
    rewriteRuleIdKey,
    zipSameNodeShape,
  )
import Moonlight.Pale.Bench.Measure
  ( FreshMeasurement (..),
    FreshMeasurementFailure,
    FreshRtsDelta (..),
    measureFreshSample,
  )
import Moonlight.Rewrite.Algebra
  ( PatternQuery (..),
    foldPatternRewriteInterface,
    guardedPatternQuery,
    patternQueryConditions,
    singlePatternQuery,
  )
import Moonlight.Rewrite.ProofContext
  ( ProofCompressionSummary (..),
    ProofQueryError,
    ProofRegistry,
    ProofRetention (..),
    ProofStep (..),
    defaultProofStepInput,
    emptyProofRegistry,
    proofClassesReachableFrom,
    proofReachability,
    proofRegistryRecordedStepCount,
    proofRegistryDroppedStepCount,
    proofRegistryRetainedStepCount,
    recordProofStepWith,
    serializeProofLog,
    summarizeProofLog,
  )
import Moonlight.Rewrite
  ( Engine,
    HostBuildError,
    NoGuardAtom,
    RelationalProgramError,
    RewriteTarget (..),
    SaturationConfig (..),
    SaturationRound (..),
    SaturationResult (..),
    compile,
    defaultSaturationConfig,
    engineHost,
    hostClassCount,
    hostFromTerms,
    prepare,
    saturate,
  )
import Moonlight.Core.Pattern.AntiUnify
  ( NaryLGGResult (..),
    antiUnifyAllTerms,
  )
import Moonlight.Core.Pattern.Automata
  ( compilePatternAutomaton,
    matchPatternAutomaton,
  )
import Moonlight.Rewrite.System
  ( CheckedRewrite,
    CheckedSystem,
    CheckedSystemError,
    RewriteError,
    addComposedPathNamed,
    checkRuleSet,
    checkedRewriteApplicationCondition,
    checkedRewriteCondition,
    checkedRewriteAlgebra,
    checkedRewriteId,
    checkedRewriteLhs,
    checkedRewriteName,
    checkedRewriteOrigin,
    checkedRewritePostSubst,
    checkedRewriteRhs,
    checkedRewrites,
    checkedSystemFromRewrites,
    ruleSet,
    ruleWithId,
    CompiledGuard,
    GuardTerm,
    RewriteCondition (..),
    compileGuard,
    compiledGuardCanonicalNodeWordsWith,
    guardChildIndex,
    guardHasFactTerms,
    guardProjectTerm,
    guardRefTerm,
    data GuardRoot,
    FactId (..),
    RuleNameError,
    mkRuleName,
    ruleNameString,
  )
import System.Environment (getArgs, getExecutablePath, lookupEnv)
import System.Exit (ExitCode (..))
import System.Info (arch, compilerName, compilerVersion, os)
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

data MeasurementMode
  = MeasurementDriver
  | MeasurementWorker !RewriteMeasurementCase !MeasurementSample
  | MeasurementCompare !ComparisonPolicy !FilePath !FilePath
  | MeasurementValidateComparator

data ComparisonPolicy
  = ReleaseComparison
  | ExploratoryComparison

data RewriteMeasurementCase
  = ProofReachabilityContiguous4096
  | ProofReachabilitySparse1000000
  | ProofRetentionNoneHost256
  | ProofRetentionSummaryHost256
  | ProofRetentionRecent64Host256
  | ProofRetentionFullHost256
  | CheckedSystemDerivedInsertion1024
  | CheckedSystemOrderedProjection1024
  | NaryAntiUnificationArity512Terms16
  | GuardEncodingDepth512
  | QueryConditionCollectionDepth4096
  | NonlinearPatternBindingSubterm4096
  deriving stock (Bounded, Enum, Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data MeasurementSpec where
  MeasurementSpec ::
    (input -> Int) ->
    Either MeasurementCaseFailure input ->
    (input -> IO (Either MeasurementCaseFailure output)) ->
    (output -> Int) ->
    MeasurementSpec

newtype MeasurementSample = MeasurementSample
  { measurementSampleValue :: Int
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

data MeasurementModeError
  = MeasurementArgumentsInvalid ![String]
  | MeasurementCaseUnknown !String
  | MeasurementSampleInvalid !String
  deriving stock (Show)

data MeasurementCaseFailure
  = MeasurementProofQueryFailed !ProofQueryError
  | MeasurementSaturationHostBuildFailed !HostBuildError
  | MeasurementSaturationProgramFailed !(RelationalProgramError BenchSig)
  | MeasurementRuleNameFailed !RuleNameError
  | MeasurementRewriteCheckFailed !(RewriteError () MeasurementNode)
  | MeasurementCheckedRewriteMissing
  | MeasurementCheckedSystemFailed !CheckedSystemError
  | MeasurementGuardCompileFailed ![PatternVar]
  | MeasurementNonlinearPatternDidNotMatch
  deriving stock (Show)

data ReceiptSide
  = BaselineReceipt
  | CandidateReceipt
  deriving stock (Show)

data MeasurementRow = MeasurementRow
  { measurementRowCase :: !RewriteMeasurementCase,
    measurementRowSample :: !MeasurementSample,
    measurementRowCosts :: !(MeasurementCosts Word64),
    measurementRowSemanticDigest :: !Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data MeasurementMetric
  = ElapsedNanoseconds
  | AllocatedBytes
  | PeakLiveBytes
  deriving stock (Bounded, Enum, Show)

data MeasurementCosts value = MeasurementCosts !value !value !value
  deriving stock (Foldable, Functor, Generic, Show, Traversable)
  deriving anyclass (FromJSON, ToJSON)

data ReceiptObstruction
  = ReceiptDecodeFailed !String
  | ReceiptVersionInvalid !Int
  | ReceiptCaseMissing !RewriteMeasurementCase
  | ReceiptCaseSamplesInvalid !RewriteMeasurementCase ![MeasurementSample]
  | ReceiptCaseDigestVaried !RewriteMeasurementCase ![Int]
  deriving stock (Show)

data MeasurementRegression
  = BaselineReceiptInvalid !ReceiptObstruction
  | CandidateReceiptInvalid !ReceiptObstruction
  | MeasurementEnvironmentChanged !ReceiptEnvironment !ReceiptEnvironment
  | MeasurementSemanticDigestChanged !RewriteMeasurementCase !Int !Int
  | MeasurementCostIncreased !MeasurementMetric !RewriteMeasurementCase !Word64 !Word64 !Word64
  deriving stock (Show)

data MeasurementFailure
  = MeasurementModeRejected !MeasurementModeError
  | MeasurementPreparationRejected !RewriteMeasurementCase !MeasurementCaseFailure
  | MeasurementSampleRejected
      !RewriteMeasurementCase
      !MeasurementSample
      !(FreshMeasurementFailure MeasurementCaseFailure)
  | MeasurementWorkerLaunchFailed
      !RewriteMeasurementCase
      !MeasurementSample
      !IOException
  | MeasurementWorkerExited
      !RewriteMeasurementCase
      !MeasurementSample
      !ExitCode
      !String
  | MeasurementWorkerOutputInvalid
      !RewriteMeasurementCase
      !MeasurementSample
      !ReceiptObstruction
  | MeasurementWorkerOutputMismatch
      !RewriteMeasurementCase
      !MeasurementSample
      !MeasurementRow
  | MeasurementReceiptReadFailed !ReceiptSide !FilePath !IOException
  | MeasurementEnvironmentCommandLaunchFailed !String !IOException
  | MeasurementEnvironmentCommandFailed !String !ExitCode !String
  | MeasurementComparatorValidationFailed !String
  | MeasurementComparisonRejected !MeasurementRegression
  deriving stock (Show)

data CaseSummary = CaseSummary
  { caseSummaryCase :: !RewriteMeasurementCase,
    caseSummarySemanticDigest :: !Int,
    caseSummaryMedianCosts :: !(MeasurementCosts Word64)
  }

data ReceiptEnvironment = ReceiptEnvironment
  { receiptCompilerIdentity :: !Text,
    receiptBuildOptions :: !Text,
    receiptRtsFlags :: !Text,
    receiptArchitecture :: !Text,
    receiptOperatingSystem :: !Text,
    receiptMachineIdentity :: !Text,
    receiptNoisePolicy :: !Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data ReceiptArtifact = ReceiptArtifact
  { receiptExecutableIdentity :: !Text,
    receiptSourceIdentity :: !Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data ReceiptMetadata = ReceiptMetadata
  { receiptEnvironment :: !ReceiptEnvironment,
    receiptArtifact :: !ReceiptArtifact
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data MeasurementReceipt = MeasurementReceipt
  { measurementReceiptFormatVersion :: !Int,
    measurementReceiptMetadata :: !ReceiptMetadata,
    measurementReceiptRows :: ![MeasurementRow]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data MeasurementNode child
  = MeasurementLeaf !Int
  | MeasurementBranch ![child]
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data MeasurementConstructorTag
  = MeasurementLeafTag !Int
  | MeasurementBranchTag !Int
  deriving stock (Eq, Ord)

instance ZipMatch MeasurementNode where
  zipMatch =
    zipSameNodeShape

instance HasConstructorTag MeasurementNode where
  type ConstructorTag MeasurementNode = MeasurementConstructorTag
  constructorTag = \case
    MeasurementLeaf value ->
      MeasurementLeafTag value
    MeasurementBranch children ->
      MeasurementBranchTag (length children)

main :: IO ()
main =
  getArgs
    >>= expectMeasurement . first MeasurementModeRejected . parseMeasurementMode
    >>= runMeasurementMode

runMeasurementMode :: MeasurementMode -> IO ()
runMeasurementMode = \case
  MeasurementDriver ->
    runMeasurementDriver >>= expectMeasurement >>= TextIO.putStr
  MeasurementWorker measurementCase sample ->
    runMeasurementWorker measurementCase sample
      >>= expectMeasurement
      >>= TextIO.putStrLn . renderJson
  MeasurementCompare comparisonPolicy baselinePath candidatePath ->
    runMeasurementComparison comparisonPolicy baselinePath candidatePath >>= expectMeasurement
  MeasurementValidateComparator ->
    expectMeasurement validateMeasurementComparator

expectMeasurement :: Show obstruction => Either obstruction value -> IO value
expectMeasurement =
  either (fail . show) pure

parseMeasurementMode :: [String] -> Either MeasurementModeError MeasurementMode
parseMeasurementMode = \case
  [] ->
    Right MeasurementDriver
  ["worker", caseToken, sampleToken] ->
    MeasurementWorker
      <$> parseMeasurementCase caseToken
      <*> parseMeasurementSample sampleToken
  ["compare", baselinePath, candidatePath] ->
    Right (MeasurementCompare ReleaseComparison baselinePath candidatePath)
  ["compare-exploratory", baselinePath, candidatePath] ->
    Right (MeasurementCompare ExploratoryComparison baselinePath candidatePath)
  ["validate-comparator"] ->
    Right MeasurementValidateComparator
  arguments ->
    Left (MeasurementArgumentsInvalid arguments)

allMeasurementCases :: [RewriteMeasurementCase]
allMeasurementCases =
  [minBound .. maxBound]

allMeasurementSamples :: [MeasurementSample]
allMeasurementSamples =
  fmap MeasurementSample [1 .. 5]

measurementCaseToken :: RewriteMeasurementCase -> String
measurementCaseToken =
  show

parseMeasurementCase :: String -> Either MeasurementModeError RewriteMeasurementCase
parseMeasurementCase rawCase =
  maybe
    (Left (MeasurementCaseUnknown rawCase))
    Right
    (lookup rawCase (fmap (\measurementCase -> (measurementCaseToken measurementCase, measurementCase)) allMeasurementCases))

parseMeasurementSample :: String -> Either MeasurementModeError MeasurementSample
parseMeasurementSample rawSample =
  case readMaybe rawSample of
    Just sampleValue
      | sampleValue >= 1,
        sampleValue <= 5 ->
          Right (MeasurementSample sampleValue)
    _ ->
      Left (MeasurementSampleInvalid rawSample)

runMeasurementDriver :: IO (Either MeasurementFailure Text)
runMeasurementDriver = do
  executablePath <- getExecutablePath
  receiptMetadata <- captureReceiptMetadata executablePath
  workerRows <- traverse (uncurry (runFreshWorker executablePath)) workerSpecifications
  pure
    ( renderMeasurementReceipt
        <$> receiptMetadata
        <*> sequence workerRows
    )
  where
    workerSpecifications =
      (,) <$> allMeasurementCases <*> allMeasurementSamples

captureReceiptMetadata :: FilePath -> IO (Either MeasurementFailure ReceiptMetadata)
captureReceiptMetadata executablePath = do
  ghcRtsFlags <- lookupEnv "GHCRTS"
  machineIdentity <- captureIdentityCommand "machine identity" "hostname" []
  executableIdentity <- captureIdentityCommand "executable identity" "shasum" ["-a", "256", executablePath]
  sourceRevision <- captureIdentityCommand "source revision" "git" ["rev-parse", "HEAD"]
  sourceStatus <-
    captureIdentityCommand
      "source status"
      "git"
      ["status", "--porcelain=v1", "--untracked-files=all"]
  pure $ do
    machine <- machineIdentity
    executable <- executableIdentity
    revision <- sourceRevision
    status <- sourceStatus
    Right
      ReceiptMetadata
        { receiptEnvironment =
            ReceiptEnvironment
              { receiptCompilerIdentity =
                  Text.pack (compilerName <> "-" <> showVersion compilerVersion),
                receiptBuildOptions = "GHC2024;-O2;-rtsopts",
                receiptRtsFlags =
                  Text.pack (maybe "-T" (<> ";-T") ghcRtsFlags),
                receiptArchitecture = Text.pack arch,
                receiptOperatingSystem = Text.pack os,
                receiptMachineIdentity = machine,
                receiptNoisePolicy = measurementNoisePolicy
              },
          receiptArtifact =
            ReceiptArtifact
              { receiptExecutableIdentity = executable,
                receiptSourceIdentity =
                  revision
                    <> ";status-digest="
                    <> Text.pack (show (textSemanticDigest status))
              }
        }

captureIdentityCommand :: String -> FilePath -> [String] -> IO (Either MeasurementFailure Text)
captureIdentityCommand identityName =
  captureTextProcess
    (MeasurementEnvironmentCommandLaunchFailed identityName)
    (MeasurementEnvironmentCommandFailed identityName)

captureTextProcess ::
  (IOException -> failure) ->
  (ExitCode -> String -> failure) ->
  FilePath ->
  [String] ->
  IO (Either failure Text)
captureTextProcess launchFailure exitFailure command arguments = do
  processResult <- try (readProcessWithExitCode command arguments "")
  pure $ case processResult of
    Left obstruction ->
      Left (launchFailure obstruction)
    Right (ExitSuccess, stdoutText, _stderrText) ->
      Right (Text.strip (Text.pack stdoutText))
    Right (exitCode@(ExitFailure _), _stdoutText, stderrText) ->
      Left (exitFailure exitCode stderrText)

textSemanticDigest :: Text -> Int
textSemanticDigest =
  Text.foldl' (\digest character -> mixSemanticDigest digest (ord character)) 71

-- Median-of-five fresh processes, each prepared and major-collected; no hidden warmup.
-- A noisy failure permits one complete retry, which must itself pass.
measurementNoisePolicy :: Text
measurementNoisePolicy =
  "median-of-five;fresh-process-per-sample;pre-action-major-gc;no-discarded-warmup;retry-once-complete-receipt;max-10-percent-or-100000-ns"

runFreshWorker ::
  FilePath ->
  RewriteMeasurementCase ->
  MeasurementSample ->
  IO (Either MeasurementFailure MeasurementRow)
runFreshWorker executablePath measurementCase sample =
  fmap (>>= decodeWorkerOutput)
    ( captureTextProcess
        (MeasurementWorkerLaunchFailed measurementCase sample)
        (MeasurementWorkerExited measurementCase sample)
        executablePath
        [ "worker",
          measurementCaseToken measurementCase,
          show (measurementSampleValue sample),
          "+RTS",
          "-T",
          "-RTS"
        ]
    )
  where
    decodeWorkerOutput output =
      first (MeasurementWorkerOutputInvalid measurementCase sample) (parseJson output)
        >>= requireWorkerIdentity measurementCase sample

requireWorkerIdentity ::
  RewriteMeasurementCase ->
  MeasurementSample ->
  MeasurementRow ->
  Either MeasurementFailure MeasurementRow
requireWorkerIdentity expectedCase expectedSample row
  | measurementRowCase row == expectedCase,
    measurementRowSample row == expectedSample =
      Right row
  | otherwise =
      Left (MeasurementWorkerOutputMismatch expectedCase expectedSample row)

runMeasurementWorker ::
  RewriteMeasurementCase ->
  MeasurementSample ->
  IO (Either MeasurementFailure MeasurementRow)
runMeasurementWorker measurementCase sample =
  case measurementSpec measurementCase of
    MeasurementSpec inputDigest preparedInput runSample semanticDigest ->
      prepareMeasurementInput inputDigest preparedInput
        >>= either
          (pure . Left . MeasurementPreparationRejected measurementCase)
          ( \input ->
              measurePreparedCase
                measurementCase
                sample
                input
                runSample
                semanticDigest
          )

measurementSpec :: RewriteMeasurementCase -> MeasurementSpec
measurementSpec = \case
  ProofReachabilityContiguous4096 ->
    MeasurementSpec
      proofRegistryInputDigest
      (Right contiguousProofRegistry)
      (proofReachabilityAction (ClassId 0))
      intSetSemanticDigest
  ProofReachabilitySparse1000000 ->
    MeasurementSpec
      proofRegistryInputDigest
      (Right sparseProofRegistry)
      (proofReachabilityAction (ClassId 1_000_000))
      intSetSemanticDigest
  ProofRetentionNoneHost256 ->
    proofRetentionMeasurementSpec KeepNoProof
  ProofRetentionSummaryHost256 ->
    proofRetentionMeasurementSpec KeepProofSummary
  ProofRetentionRecent64Host256 ->
    proofRetentionMeasurementSpec (KeepRecentProofSteps 64)
  ProofRetentionFullHost256 ->
    proofRetentionMeasurementSpec KeepFullProof
  CheckedSystemDerivedInsertion1024 ->
    MeasurementSpec
      checkedSystemInsertionFixtureDigest
      checkedSystemInsertionFixture
      checkedSystemInsertionAction
      checkedSystemSemanticDigest
  CheckedSystemOrderedProjection1024 ->
    pureMeasurementSpec
      checkedSystemInputDigest
      checkedSystemProjectionFixture
      checkedRewrites
      checkedRewriteListSemanticDigest
  NaryAntiUnificationArity512Terms16 ->
    pureMeasurementSpec
      measurementTermsDigest
      (Right antiUnificationTerms)
      antiUnifyAllTerms
      naryAntiUnificationSemanticDigest
  GuardEncodingDepth512 ->
    pureMeasurementSpec
      compiledGuardInputDigest
      guardEncodingFixture
      (compiledGuardCanonicalNodeWordsWith (const 0) (const 0))
      wordListSemanticDigest
  QueryConditionCollectionDepth4096 ->
    pureMeasurementSpec
      queryStructureDigest
      (Right queryConditionFixtureAtDepth4096)
      patternQueryConditions
      intListSemanticDigest
  NonlinearPatternBindingSubterm4096 ->
    MeasurementSpec
      nonlinearMatcherInputDigest
      (Right nonlinearMatcherFixture)
      nonlinearMatcherAction
      id
{-# INLINE measurementSpec #-}

pureMeasurementSpec ::
  (input -> Int) ->
  Either MeasurementCaseFailure input ->
  (input -> output) ->
  (output -> Int) ->
  MeasurementSpec
pureMeasurementSpec inputDigest preparedInput action =
  MeasurementSpec inputDigest preparedInput (pure . Right . action)
{-# INLINE pureMeasurementSpec #-}

proofRetentionMeasurementSpec :: ProofRetention -> MeasurementSpec
proofRetentionMeasurementSpec retention =
  MeasurementSpec
    proofRetentionFixtureDigest
    (proofRetentionFixture retention)
    runProofRetentionFixture
    proofRetentionSemanticDigest
{-# INLINE proofRetentionMeasurementSpec #-}

prepareMeasurementInput ::
  (input -> Int) ->
  Either MeasurementCaseFailure input ->
  IO (Either MeasurementCaseFailure input)
prepareMeasurementInput inputDigest =
  traverse
    ( \input ->
        input <$ evaluate (force (inputDigest input))
    )

measurePreparedCase ::
  RewriteMeasurementCase ->
  MeasurementSample ->
  input ->
  (input -> IO (Either MeasurementCaseFailure value)) ->
  (value -> Int) ->
  IO (Either MeasurementFailure MeasurementRow)
measurePreparedCase measurementCase sample input runSample semanticDigest =
  fmap
    ( first (MeasurementSampleRejected measurementCase sample)
        . fmap (measurementRowFromFreshMeasurement measurementCase sample)
    )
    ( measureFreshSample
        (measurementSampleValue sample)
        input
        runSample
        (\value -> semanticDigest value `seq` ())
        semanticDigest
    )

measurementRowFromFreshMeasurement ::
  RewriteMeasurementCase ->
  MeasurementSample ->
  FreshMeasurement value ->
  MeasurementRow
measurementRowFromFreshMeasurement measurementCase sample measurement =
  MeasurementRow
    { measurementRowCase = measurementCase,
      measurementRowSample = sample,
      measurementRowCosts =
        MeasurementCosts
          (freshMeasurementElapsedNanoseconds measurement)
          (freshRtsDeltaAllocatedBytes (freshMeasurementRtsDelta measurement))
          (freshMeasurementPeakLiveBytesThroughAction measurement),
      measurementRowSemanticDigest = freshMeasurementDigest measurement
    }

type MeasurementProofRegistry = ProofRegistry Proxy () Int

contiguousProofRegistry :: MeasurementProofRegistry
contiguousProofRegistry =
  foldl'
    (flip recordMeasurementProofStep)
    emptyProofRegistry
    (fmap (\key -> (key, key + 1)) [0 .. 4094])

sparseProofRegistry :: MeasurementProofRegistry
sparseProofRegistry =
  recordMeasurementProofStep (1_000_000, 1_000_000) emptyProofRegistry

recordMeasurementProofStep :: (Int, Int) -> MeasurementProofRegistry -> MeasurementProofRegistry
recordMeasurementProofStep (leftKey, rightKey) =
  recordProofStepWith
    ( defaultProofStepInput
        (RewriteRuleId 0)
        (ClassId leftKey)
        (ClassId rightKey)
        emptySubstitution
        0
    )

proofRegistryInputDigest :: MeasurementProofRegistry -> Int
proofRegistryInputDigest registry =
  foldl'
    ( \digest proofStep ->
        mixSemanticDigest
          (mixSemanticDigest digest (classIdKey (psLhsClass proofStep)))
          (classIdKey (psRhsClass proofStep))
    )
    (proofRegistryRecordedStepCount registry)
    (serializeProofLog registry)

proofReachabilityAction ::
  ClassId ->
  MeasurementProofRegistry ->
  IO (Either MeasurementCaseFailure IntSet)
proofReachabilityAction sourceClass registry =
  pure
    ( first MeasurementProofQueryFailed (proofReachability registry)
        >>= Right . proofClassesReachableFrom sourceClass
    )

intSetSemanticDigest :: IntSet -> Int
intSetSemanticDigest =
  intListSemanticDigest . IntSet.toAscList

data ProofRetentionFixture = ProofRetentionFixture
  { retentionFixtureEngine :: !(Engine BenchSig NoGuardAtom),
    retentionFixturePolicy :: !ProofRetention
  }

data ProofRetentionMeasurement = ProofRetentionMeasurement
  { retentionMeasurementEngine :: !(Engine BenchSig NoGuardAtom),
    retentionMeasurementInitialResult :: !(SaturationResult BenchSig),
    retentionMeasurementReuseResult :: !(SaturationResult BenchSig)
  }

proofRetentionFixture ::
  ProofRetention ->
  Either MeasurementCaseFailure ProofRetentionFixture
proofRetentionFixture retention = do
  (host, _roots) <-
    first MeasurementSaturationHostBuildFailed
      (hostFromTerms (benchTerms 256))
  rulesValue <-
    first MeasurementSaturationProgramFailed (compile benchProgram)
  Right
    ProofRetentionFixture
      { retentionFixtureEngine = prepare rulesValue host,
        retentionFixturePolicy = retention
      }

proofRetentionFixtureDigest :: ProofRetentionFixture -> Int
proofRetentionFixtureDigest fixture =
  mixSemanticDigest
    (hostClassCount (engineHost (retentionFixtureEngine fixture)))
    (proofRetentionDigest (retentionFixturePolicy fixture))

proofRetentionDigest :: ProofRetention -> Int
proofRetentionDigest = \case
  KeepNoProof -> 0
  KeepProofSummary -> 1
  KeepRecentProofSteps retained -> mixSemanticDigest 2 (fromIntegral retained)
  KeepFullProof -> 3

runProofRetentionFixture ::
  ProofRetentionFixture ->
  IO (Either MeasurementCaseFailure ProofRetentionMeasurement)
runProofRetentionFixture fixture =
  pure
    ( do
        (initialEngine, initialResult) <-
          first MeasurementSaturationProgramFailed
            (saturate RewriteBase saturationConfig (retentionFixtureEngine fixture))
        (reuseEngine, reuseResult) <-
          first MeasurementSaturationProgramFailed
            (saturate RewriteBase saturationConfig initialEngine)
        Right
          ProofRetentionMeasurement
            { retentionMeasurementEngine = reuseEngine,
              retentionMeasurementInitialResult = initialResult,
              retentionMeasurementReuseResult = reuseResult
            }
    )
  where
    saturationConfig =
      defaultSaturationConfig
        { scProofRetention = retentionFixturePolicy fixture
        }

proofRetentionSemanticDigest :: ProofRetentionMeasurement -> Int
proofRetentionSemanticDigest measurement =
  mixSemanticDigest
    (hostClassCount (engineHost (retentionMeasurementEngine measurement)))
    ( mixSemanticDigest
        (saturationResultSemanticDigest (retentionMeasurementInitialResult measurement))
        (saturationResultSemanticDigest (retentionMeasurementReuseResult measurement))
    )

saturationResultSemanticDigest :: SaturationResult BenchSig -> Int
saturationResultSemanticDigest result =
  proofRegistryForceDigest (saturationProofs result)
    `seq` mixSemanticDigest
      (hostClassCount (saturationHost result))
      (sum (fmap (length . saturationRoundExecuted) (saturationRounds result)))

proofRegistryForceDigest :: (Functor f, Foldable f) => ProofRegistry f c p -> Int
proofRegistryForceDigest registry =
  proofRegistryRecordedStepCount registry
    + proofRegistryRetainedStepCount registry
    + proofRegistryDroppedStepCount registry
    + proofCompressionSummaryWeight (summarizeProofLog registry)
    + sum (fmap proofStepWitnessWeight (serializeProofLog registry))

proofStepWitnessWeight :: (Functor f, Foldable f) => ProofStep f c p -> Int
proofStepWitnessWeight proofStep =
  maybe 0 witnessWeight (psLhsWitness proofStep)
    + maybe 0 witnessWeight (psRhsWitness proofStep)
  where
    witnessWeight =
      foldFix (\childWeights -> 1 + sum childWeights)

proofCompressionSummaryWeight :: ProofCompressionSummary -> Int
proofCompressionSummaryWeight summary =
  pcsTotalSteps summary
    + pcsUniqueClassPairs summary
    + pcsUniqueRewriteRules summary
    + pcsCompressionSavings summary
    + pcsWitnessedSteps summary
    + pcsGuardedSteps summary
    + pcsContextualSteps summary
    + pcsSupportAwareSteps summary
    + pcsUniqueSupports summary
    + pcsFactfulSteps summary

newtype CheckedSystemInsertionFixture = CheckedSystemInsertionFixture
  { insertionCheckedSystem :: CheckedSystem () MeasurementNode
  }

checkedSystemInsertionFixture ::
  Either MeasurementCaseFailure CheckedSystemInsertionFixture
checkedSystemInsertionFixture = do
  checkedSystem <-
    checkedSystemProjectionFixture
  Right (CheckedSystemInsertionFixture checkedSystem)

checkedSystemProjectionFixture :: Either MeasurementCaseFailure (CheckedSystem () MeasurementNode)
checkedSystemProjectionFixture = do
  rewrites <- traverse measurementCheckedRewrite [0 .. 1023]
  first MeasurementCheckedSystemFailed (checkedSystemFromRewrites rewrites)

measurementCheckedRewrite :: Int -> Either MeasurementCaseFailure (CheckedRewrite () MeasurementNode)
measurementCheckedRewrite key = do
  name <-
    first MeasurementRuleNameFailed
      (mkRuleName ("measurement.rule" <> show key))
  let rewriteRuleId = RewriteRuleId key
  checkedSystem <-
    first MeasurementRewriteCheckFailed
      (checkRuleSet (ruleSet [ruleWithId rewriteRuleId name measurementIdentityPattern measurementIdentityPattern]))
  case checkedRewrites checkedSystem of
    [rewriteValue] ->
      Right rewriteValue
    _ ->
      Left MeasurementCheckedRewriteMissing

measurementIdentityPattern :: Pattern MeasurementNode
measurementIdentityPattern =
  PatternVar (mkPatternVar 0)

checkedSystemInsertionFixtureDigest :: CheckedSystemInsertionFixture -> Int
checkedSystemInsertionFixtureDigest insertionFixture =
  checkedSystemInputDigest (insertionCheckedSystem insertionFixture)

checkedSystemInsertionAction ::
  CheckedSystemInsertionFixture ->
  IO (Either MeasurementCaseFailure (CheckedSystem () MeasurementNode))
checkedSystemInsertionAction insertionFixture =
  pure $ do
    firstInput <-
      first MeasurementRuleNameFailed (mkRuleName "measurement.rule0")
    secondInput <-
      first MeasurementRuleNameFailed (mkRuleName "measurement.rule1")
    derivedName <-
      first MeasurementRuleNameFailed (mkRuleName "measurement.derived1024")
    first MeasurementRewriteCheckFailed
      ( addComposedPathNamed
          derivedName
          (firstInput :| [secondInput])
          (insertionCheckedSystem insertionFixture)
      )

checkedSystemSemanticDigest :: CheckedSystem () MeasurementNode -> Int
checkedSystemSemanticDigest =
  checkedRewriteListSemanticDigest . checkedRewrites

checkedSystemInputDigest :: CheckedSystem () MeasurementNode -> Int
checkedSystemInputDigest =
  foldSemanticDigest 17 checkedRewriteInputDigest . checkedRewrites

checkedRewriteListSemanticDigest :: [CheckedRewrite () MeasurementNode] -> Int
checkedRewriteListSemanticDigest =
  foldSemanticDigest 17 checkedRewriteSemanticDigest

checkedRewriteSemanticDigest :: CheckedRewrite () MeasurementNode -> Int
checkedRewriteSemanticDigest rewriteValue =
  mixSemanticDigest
    (rewriteRuleIdKey (checkedRewriteId rewriteValue))
    (foldSemanticDigest 23 ord (ruleNameString (checkedRewriteName rewriteValue)))

checkedRewriteInputDigest :: CheckedRewrite () MeasurementNode -> Int
checkedRewriteInputDigest rewriteValue =
  foldl'
    mixSemanticDigest
    ( mixSemanticDigest
        (rewriteRuleIdKey (checkedRewriteId rewriteValue))
        (foldSemanticDigest 23 ord (ruleNameString (checkedRewriteName rewriteValue)))
    )
    [ measurementPatternDigest (checkedRewriteLhs rewriteValue),
      measurementPatternDigest (checkedRewriteRhs rewriteValue),
      foldPatternRewriteInterface
        (\digest patternVariable -> mixSemanticDigest digest (patternVarKey patternVariable))
        29
        (checkedRewriteAlgebra rewriteValue),
      length (show (checkedRewriteOrigin rewriteValue)),
      length (show (checkedRewriteCondition rewriteValue)),
      length (show (checkedRewriteApplicationCondition rewriteValue)),
      length (show (checkedRewritePostSubst rewriteValue))
    ]

antiUnificationTerms :: NonEmpty (Fix MeasurementNode)
antiUnificationTerms =
  measurementWideTerm 0 :| fmap measurementWideTerm [1 .. 15]

measurementWideTerm :: Int -> Fix MeasurementNode
measurementWideTerm termOffset =
  Fix
    ( MeasurementBranch
        [ Fix (MeasurementLeaf (termOffset + childIndex))
          | childIndex <- [0 .. 511]
        ]
    )

measurementTermsDigest :: NonEmpty (Fix MeasurementNode) -> Int
measurementTermsDigest =
  foldSemanticDigest 29 measurementTermDigest . NonEmpty.toList

measurementTermDigest :: Fix MeasurementNode -> Int
measurementTermDigest =
  foldFix measurementNodeDigest

measurementNodeDigest :: MeasurementNode Int -> Int
measurementNodeDigest = \case
  MeasurementLeaf key ->
    mixSemanticDigest 31 key
  MeasurementBranch childDigests ->
    foldSemanticDigest 37 id childDigests

naryAntiUnificationSemanticDigest :: NaryLGGResult MeasurementNode (Fix MeasurementNode) -> Int
naryAntiUnificationSemanticDigest result =
  foldSemanticDigest
    (mixSemanticDigest (naryLggSharedStructure result) (measurementPatternDigest (naryLggPattern result)))
    bindingRowDigest
    (NonEmpty.toList (naryLggBindings result))

measurementPatternDigest :: Pattern MeasurementNode -> Int
measurementPatternDigest = \case
  PatternVar patternVariable ->
    mixSemanticDigest 41 (patternVarKey patternVariable)
  PatternNode node ->
    measurementNodeDigest (fmap measurementPatternDigest node)

bindingRowDigest :: IntMap.IntMap (Fix MeasurementNode) -> Int
bindingRowDigest =
  IntMap.foldlWithKey'
    (\digest key term -> mixSemanticDigest (mixSemanticDigest digest key) (measurementTermDigest term))
    43

type NonlinearMatcherFixture = (Pattern MeasurementNode, Fix MeasurementNode)

nonlinearMatcherFixture :: NonlinearMatcherFixture
nonlinearMatcherFixture =
  ( PatternNode
      ( MeasurementBranch
          [ PatternVar repeatedPatternVariable,
            PatternVar repeatedPatternVariable
          ]
      ),
    Fix
      ( MeasurementBranch
          [ measurementLinearSubterm,
            measurementLinearSubterm
          ]
      )
  )
  where
    repeatedPatternVariable = mkPatternVar 0

measurementLinearSubterm :: Fix MeasurementNode
measurementLinearSubterm =
  -- One seed leaf plus 2,047 branch/leaf pairs is exactly 4,096 nodes.
  foldl'
    (\child nodeKey -> Fix (MeasurementBranch [Fix (MeasurementLeaf nodeKey), child]))
    (Fix (MeasurementLeaf 0))
    [1 .. 2047]

nonlinearMatcherInputDigest :: NonlinearMatcherFixture -> Int
nonlinearMatcherInputDigest (patternValue, termValue) =
  mixSemanticDigest
    (measurementPatternDigest patternValue)
    (measurementTermDigest termValue)

nonlinearMatcherAction ::
  NonlinearMatcherFixture ->
  IO (Either MeasurementCaseFailure Int)
nonlinearMatcherAction (patternValue, termValue) =
  pure
    ( maybe
        (Left MeasurementNonlinearPatternDidNotMatch)
        (Right . bindingRowDigest)
        (matchPatternAutomaton (compilePatternAutomaton patternValue) termValue IntMap.empty)
    )

guardEncodingFixture :: Either MeasurementCaseFailure (CompiledGuard () [])
guardEncodingFixture =
  first MeasurementGuardCompileFailed
    ( compileGuard
        Set.empty
        (RewriteCondition (guardHasFactTerms (FactId 7) [guardTermAtDepth512]))
    )

compiledGuardInputDigest :: CompiledGuard () [] -> Int
compiledGuardInputDigest =
  length . show

wordListSemanticDigest :: [Word64] -> Int
wordListSemanticDigest =
  foldSemanticDigest 47 fromIntegral

guardTermAtDepth512 :: GuardTerm []
guardTermAtDepth512 =
  foldl'
    guardProjectTerm
    (guardRefTerm GuardRoot)
    (replicate 512 (guardChildIndex 0))

queryConditionFixtureAtDepth4096 :: PatternQuery Int []
queryConditionFixtureAtDepth4096 =
  foldl'
    guardedPatternQuery
    (singlePatternQuery (PatternVar (mkPatternVar 0)))
    [1 .. 4096]

queryStructureDigest :: PatternQuery Int [] -> Int
queryStructureDigest = \case
  SinglePatternQuery patternValue ->
    mixSemanticDigest 53 (simplePatternDigest patternValue)
  ConjunctivePatternQuery childQueries ->
    foldSemanticDigest 59 queryStructureDigest (NonEmpty.toList childQueries)
  GuardedPatternQuery nestedQuery guardValue ->
    mixSemanticDigest (queryStructureDigest nestedQuery) guardValue

simplePatternDigest :: Pattern [] -> Int
simplePatternDigest = \case
  PatternVar patternVariable ->
    patternVarKey patternVariable
  PatternNode childPatterns ->
    foldSemanticDigest 61 simplePatternDigest childPatterns

intListSemanticDigest :: [Int] -> Int
intListSemanticDigest =
  foldSemanticDigest 67 id

foldSemanticDigest :: Foldable collection => Int -> (value -> Int) -> collection value -> Int
foldSemanticDigest seed digestValue =
  foldl' (\digest value -> mixSemanticDigest digest (digestValue value)) seed
{-# INLINE foldSemanticDigest #-}

mixSemanticDigest :: Int -> Int -> Int
mixSemanticDigest accumulator value =
  accumulator * 16777619 + value
{-# INLINE mixSemanticDigest #-}

measurementReceiptVersion :: Int
measurementReceiptVersion =
  3

renderMeasurementReceipt :: ReceiptMetadata -> [MeasurementRow] -> Text
renderMeasurementReceipt metadata rows =
  renderJson
    MeasurementReceipt
      { measurementReceiptFormatVersion = measurementReceiptVersion,
        measurementReceiptMetadata = metadata,
        measurementReceiptRows = rows
      }

parseMeasurementReceipt :: Text -> Either ReceiptObstruction MeasurementReceipt
parseMeasurementReceipt receiptText = do
  receipt <- parseJson receiptText
  if measurementReceiptFormatVersion receipt == measurementReceiptVersion
    then Right receipt
    else Left (ReceiptVersionInvalid (measurementReceiptFormatVersion receipt))

renderJson :: ToJSON value => value -> Text
renderJson =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . encode

parseJson :: FromJSON value => Text -> Either ReceiptObstruction value
parseJson =
  first ReceiptDecodeFailed . eitherDecodeStrict' . TextEncoding.encodeUtf8

runMeasurementComparison :: ComparisonPolicy -> FilePath -> FilePath -> IO (Either MeasurementFailure ())
runMeasurementComparison comparisonPolicy baselinePath candidatePath = do
  baselineReceipt <- readMeasurementReceipt BaselineReceipt baselinePath
  candidateReceipt <- readMeasurementReceipt CandidateReceipt candidatePath
  pure $ do
    baselineText <- baselineReceipt
    candidateText <- candidateReceipt
    first MeasurementComparisonRejected
      (compareMeasurementReceipts comparisonPolicy baselineText candidateText)

readMeasurementReceipt :: ReceiptSide -> FilePath -> IO (Either MeasurementFailure Text)
readMeasurementReceipt receiptSide receiptPath =
  fmap
    (first (MeasurementReceiptReadFailed receiptSide receiptPath))
    (try (TextIO.readFile receiptPath))

compareMeasurementReceipts :: ComparisonPolicy -> Text -> Text -> Either MeasurementRegression ()
compareMeasurementReceipts comparisonPolicy baselineText candidateText = do
  baselineReceipt <-
    first BaselineReceiptInvalid (parseMeasurementReceipt baselineText)
  candidateReceipt <-
    first CandidateReceiptInvalid (parseMeasurementReceipt candidateText)
  compareReceiptEnvironment
    comparisonPolicy
    (receiptEnvironment (measurementReceiptMetadata baselineReceipt))
    (receiptEnvironment (measurementReceiptMetadata candidateReceipt))
  baselineSummaries <-
    first BaselineReceiptInvalid (summarizeMeasurementReceipt (measurementReceiptRows baselineReceipt))
  candidateSummaries <-
    first CandidateReceiptInvalid (summarizeMeasurementReceipt (measurementReceiptRows candidateReceipt))
  traverse_
    (uncurry compareCaseSummary)
    (zip baselineSummaries candidateSummaries)

compareReceiptEnvironment ::
  ComparisonPolicy ->
  ReceiptEnvironment ->
  ReceiptEnvironment ->
  Either MeasurementRegression ()
compareReceiptEnvironment comparisonPolicy baselineEnvironment candidateEnvironment =
  case comparisonPolicy of
    ExploratoryComparison ->
      Right ()
    ReleaseComparison
      | baselineEnvironment == candidateEnvironment ->
          Right ()
      | otherwise ->
          Left (MeasurementEnvironmentChanged baselineEnvironment candidateEnvironment)

summarizeMeasurementReceipt :: [MeasurementRow] -> Either ReceiptObstruction [CaseSummary]
summarizeMeasurementReceipt rows =
  traverse summarizeCase allMeasurementCases
  where
    summarizeCase measurementCase =
      summarizeCaseRows
        measurementCase
        (filter ((== measurementCase) . measurementRowCase) rows)

summarizeCaseRows :: RewriteMeasurementCase -> [MeasurementRow] -> Either ReceiptObstruction CaseSummary
summarizeCaseRows measurementCase = \case
  [] ->
    Left (ReceiptCaseMissing measurementCase)
  rows -> do
    let observedSamples = sort (fmap measurementRowSample rows)
        observedDigests = Set.toAscList (Set.fromList (fmap measurementRowSemanticDigest rows))
    if observedSamples == allMeasurementSamples
      then pure ()
      else Left (ReceiptCaseSamplesInvalid measurementCase observedSamples)
    semanticDigest <-
      case observedDigests of
        [singleDigest] -> Right singleDigest
        variedDigests -> Left (ReceiptCaseDigestVaried measurementCase variedDigests)
    medianCosts <-
      traverse
        (medianOfFive measurementCase)
        ( MeasurementCosts
            (fmap (measurementCost ElapsedNanoseconds . measurementRowCosts) rows)
            (fmap (measurementCost AllocatedBytes . measurementRowCosts) rows)
            (fmap (measurementCost PeakLiveBytes . measurementRowCosts) rows)
        )
    Right
      CaseSummary
        { caseSummaryCase = measurementCase,
          caseSummarySemanticDigest = semanticDigest,
          caseSummaryMedianCosts = medianCosts
        }

medianOfFive :: RewriteMeasurementCase -> [Word64] -> Either ReceiptObstruction Word64
medianOfFive measurementCase values =
  case sort values of
    [_first, _second, medianValue, _fourth, _fifth] ->
      Right medianValue
    _ ->
      Left (ReceiptCaseSamplesInvalid measurementCase [])

compareCaseSummary :: CaseSummary -> CaseSummary -> Either MeasurementRegression ()
compareCaseSummary baseline candidate
  | caseSummaryCase baseline /= caseSummaryCase candidate =
      Left (CandidateReceiptInvalid (ReceiptCaseMissing (caseSummaryCase baseline)))
  | caseSummarySemanticDigest candidate /= caseSummarySemanticDigest baseline =
      Left
        ( MeasurementSemanticDigestChanged
            measurementCase
            (caseSummarySemanticDigest baseline)
            (caseSummarySemanticDigest candidate)
        )
  | otherwise =
      traverse_
        (compareMeasurementCost measurementCase (caseSummaryMedianCosts baseline) (caseSummaryMedianCosts candidate))
        [minBound .. maxBound]
  where
    measurementCase =
      caseSummaryCase baseline

measurementCost :: MeasurementMetric -> MeasurementCosts value -> value
measurementCost metric (MeasurementCosts elapsed allocated peakLive) =
  case metric of
    ElapsedNanoseconds -> elapsed
    AllocatedBytes -> allocated
    PeakLiveBytes -> peakLive

compareMeasurementCost ::
  RewriteMeasurementCase ->
  MeasurementCosts Word64 ->
  MeasurementCosts Word64 ->
  MeasurementMetric ->
  Either MeasurementRegression ()
compareMeasurementCost measurementCase baselineCosts candidateCosts metric =
  if toInteger candidateCost <= toInteger baselineCost + toInteger tolerance
    then Right ()
    else
      Left
        ( MeasurementCostIncreased
            metric
            measurementCase
            baselineCost
            candidateCost
            tolerance
        )
  where
    baselineCost =
      measurementCost metric baselineCosts
    candidateCost =
      measurementCost metric candidateCosts
    tolerance =
      measurementMetricTolerance metric baselineCost

measurementMetricTolerance :: MeasurementMetric -> Word64 -> Word64
measurementMetricTolerance metric =
  case metric of
    ElapsedNanoseconds -> elapsedTimeTolerance
    AllocatedBytes -> const 0
    PeakLiveBytes -> const 0

elapsedTimeTolerance :: Word64 -> Word64
elapsedTimeTolerance baselineNanoseconds =
  fromInteger
    ( min
        (toInteger (maxBound :: Word64))
        ( max
            100000
            ((toInteger baselineNanoseconds * 10 + 99) `div` 100)
        )
    )

validateMeasurementComparator :: Either MeasurementFailure ()
validateMeasurementComparator = do
  first MeasurementComparisonRejected
    (compareMeasurementReceipts ReleaseComparison baselineReceipt baselineReceipt)
  requireExpectedRegression
    "material elapsed regression"
    ( \case
        MeasurementCostIncreased ElapsedNanoseconds _ _ _ _ -> True
        _ -> False
    )
    (compareMeasurementReceipts ReleaseComparison baselineReceipt slowerReceipt)
  requireExpectedRegression
    "environment mismatch"
    ( \case
        MeasurementEnvironmentChanged {} -> True
        _ -> False
    )
    (compareMeasurementReceipts ReleaseComparison baselineReceipt mismatchedEnvironmentReceipt)
  requireExpectedRegression
    "missing environment metadata"
    ( \case
        CandidateReceiptInvalid (ReceiptDecodeFailed _) -> True
        _ -> False
    )
    (compareMeasurementReceipts ReleaseComparison baselineReceipt missingMetadataReceipt)
  where
    baselineReceipt =
      renderMeasurementReceipt syntheticReceiptMetadata (syntheticMeasurementRows 1000000)
    slowerReceipt =
      renderMeasurementReceipt syntheticReceiptMetadata (syntheticMeasurementRows 1500000)
    mismatchedEnvironmentReceipt =
      renderMeasurementReceipt
        syntheticReceiptMetadata
          { receiptEnvironment =
              (receiptEnvironment syntheticReceiptMetadata)
                { receiptMachineIdentity = "another-runner"
                }
          }
        (syntheticMeasurementRows 1000000)
    missingMetadataReceipt =
      "{}"

requireExpectedRegression ::
  String ->
  (MeasurementRegression -> Bool) ->
  Either MeasurementRegression () ->
  Either MeasurementFailure ()
requireExpectedRegression validationName acceptsRegression comparisonResult =
  case comparisonResult of
    Left regression
      | acceptsRegression regression ->
          Right ()
      | otherwise ->
          Left
            ( MeasurementComparatorValidationFailed
                (validationName <> " produced " <> show regression)
            )
    Right () ->
      Left
        ( MeasurementComparatorValidationFailed
            (validationName <> " unexpectedly passed")
        )

syntheticReceiptMetadata :: ReceiptMetadata
syntheticReceiptMetadata =
  ReceiptMetadata
    { receiptEnvironment =
        ReceiptEnvironment
          { receiptCompilerIdentity = "synthetic-ghc",
            receiptBuildOptions = "synthetic-options",
            receiptRtsFlags = "synthetic-rts",
            receiptArchitecture = "synthetic-architecture",
            receiptOperatingSystem = "synthetic-os",
            receiptMachineIdentity = "synthetic-runner",
            receiptNoisePolicy = measurementNoisePolicy
          },
      receiptArtifact =
        ReceiptArtifact
          { receiptExecutableIdentity = "synthetic-executable",
            receiptSourceIdentity = "synthetic-source"
          }
    }

syntheticMeasurementRows :: Word64 -> [MeasurementRow]
syntheticMeasurementRows elapsedNanoseconds =
  [ MeasurementRow
      { measurementRowCase = measurementCase,
        measurementRowSample = sample,
        measurementRowCosts = MeasurementCosts elapsedNanoseconds 1024 2048,
        measurementRowSemanticDigest = semanticDigest
      }
    | (measurementCase, semanticDigest) <- zip allMeasurementCases [1 ..],
      sample <- allMeasurementSamples
  ]
