{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.Nebula.CLI
  ( Command (..),
    CommandName (..),
    CorpusOptions (..),
    DiagnoseOptions (..),
    FlagWithArgument (..),
    OutputFormat (..),
    ReportOptions (..),
    UsageError (..),
    WorkspaceInputs (..),
    WriteOptions (..),
    WriteSelection (..),
    main,
    parseCommand,
  )
where

import Control.Exception (IOException, displayException, evaluate, try)
import Data.Either (isLeft)
import Data.Kind (Type)
import Melusine.Nebula
  ( DiagnoseRegion,
    ModuleImprovement (..),
    ModulePatch (..),
    ModuleWorkload (..),
    SealOutcome (..),
    WorkspaceReport (..),
    defaultNebulaConfig,
    diagnoseEnvelopeJson,
    diagnoseModule,
    enumerateModuleWorkloads,
    improveWorkspace,
    modulePatchHasContent,
    patchedModuleSource,
    renderDiagnoseRegions,
    renderModuleCandidateDiff,
    renderModuleDiff,
    renderWorkspaceReport,
    sealedSourceText,
  )
import Melusine.Nebula.Core
  ( NebulaError,
    nebulaErrorKey,
    nebulaErrorMessage,
  )
import Melusine.Nebula.Report.Json
  ( BaselineFailure (..),
    DetailTier (..),
    J,
    corpusEnvelopeJson,
    parseBaseline,
    renderJson,
    workspaceEnvelopeJson,
  )
import Melusine.Nebula.Write.Back
  ( WriteOutcome (..),
    WriteStatus (..),
  )
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStr, stderr)

type CommandName :: Type
data CommandName
  = ReportName
  | DiagnoseName
  | CorpusName
  | WriteName
  deriving stock (Eq, Show)

type OutputFormat :: Type
data OutputFormat
  = TextOutput
  | JsonOutput
  deriving stock (Eq, Show)

type WriteSelection :: Type
data WriteSelection
  = SealedWrite
  | CandidateWrite
  deriving stock (Eq, Show)

type WorkspaceInputs :: Type
data WorkspaceInputs = WorkspaceInputs
  { inputHieRoots :: ![FilePath],
    inputRoots :: ![FilePath]
  }
  deriving stock (Eq, Show)

type ReportOptions :: Type
data ReportOptions = ReportOptions
  { reportOutputFormat :: !OutputFormat,
    reportDetailTier :: !DetailTier,
    reportWorkspaceInputs :: !WorkspaceInputs
  }

type DiagnoseOptions :: Type
data DiagnoseOptions = DiagnoseOptions
  { diagnoseOutputFormat :: !OutputFormat,
    diagnoseWorkspaceInputs :: !WorkspaceInputs
  }
  deriving stock (Eq, Show)

type CorpusOptions :: Type
data CorpusOptions = CorpusOptions
  { corpusBaselinePath :: !(Maybe FilePath),
    corpusWorkspaceInputs :: !WorkspaceInputs
  }
  deriving stock (Eq, Show)

type WriteOptions :: Type
data WriteOptions = WriteOptions
  { writeSelection :: !WriteSelection,
    writeOutputFormat :: !OutputFormat,
    writeDetailTier :: !DetailTier,
    writeWorkspaceInputs :: !WorkspaceInputs
  }

type Command :: Type
data Command
  = ReportCommand !ReportOptions
  | DiagnoseCommand !DiagnoseOptions
  | CorpusCommand !CorpusOptions
  | WriteCommand !WriteOptions

type FlagWithArgument :: Type
data FlagWithArgument
  = BaselineFlag
  | HieRootFlag
  deriving stock (Eq, Show)

type UsageError :: Type
data UsageError
  = MissingSubcommand
  | UnknownSubcommand !String
  | UnknownFlag !CommandName !String
  | MissingFlagArgument !CommandName !FlagWithArgument
  | MissingRoots !CommandName
  | ConflictingDetailTiers !CommandName
  deriving stock (Eq, Show)

type ExecutionStatus :: Type
data ExecutionStatus
  = ExecutionClean
  | ExecutionDegraded

main :: IO ()
main =
  getArgs >>= either reportUsageError runCommand . parseCommand

parseCommand :: [String] -> Either UsageError Command
parseCommand = \case
  "report" : arguments ->
    ReportCommand <$> parseReportOptions initialReportOptions arguments
  "diagnose" : arguments ->
    DiagnoseCommand <$> parseDiagnoseOptions initialDiagnoseOptions arguments
  "corpus" : arguments ->
    CorpusCommand <$> parseCorpusOptions initialCorpusOptions arguments
  "write" : arguments ->
    WriteCommand <$> parseWriteOptions initialWriteOptions arguments
  [] ->
    Left MissingSubcommand
  subcommand : _ ->
    Left (UnknownSubcommand subcommand)

initialWorkspaceInputs :: WorkspaceInputs
initialWorkspaceInputs = WorkspaceInputs [] []

initialReportOptions :: ReportOptions
initialReportOptions =
  ReportOptions
    { reportOutputFormat = TextOutput,
      reportDetailTier = TierStandard,
      reportWorkspaceInputs = initialWorkspaceInputs
    }

initialDiagnoseOptions :: DiagnoseOptions
initialDiagnoseOptions =
  DiagnoseOptions
    { diagnoseOutputFormat = TextOutput,
      diagnoseWorkspaceInputs = initialWorkspaceInputs
    }

initialCorpusOptions :: CorpusOptions
initialCorpusOptions =
  CorpusOptions
    { corpusBaselinePath = Nothing,
      corpusWorkspaceInputs = initialWorkspaceInputs
    }

initialWriteOptions :: WriteOptions
initialWriteOptions =
  WriteOptions
    { writeSelection = SealedWrite,
      writeOutputFormat = TextOutput,
      writeDetailTier = TierStandard,
      writeWorkspaceInputs = initialWorkspaceInputs
    }

parseReportOptions :: ReportOptions -> [String] -> Either UsageError ReportOptions
parseReportOptions options = \case
  [] ->
    finalizeWorkspaceOptions ReportName reportWorkspaceInputs setReportWorkspaceInputs options
  "--json" : remaining ->
    parseReportOptions (options {reportOutputFormat = JsonOutput}) remaining
  "--summary" : remaining ->
    selectReportDetailTier TierSummary options >>= (`parseReportOptions` remaining)
  "--full" : remaining ->
    selectReportDetailTier TierFull options >>= (`parseReportOptions` remaining)
  "--hie-root" : hieRoot : remaining
    | not (isFlag hieRoot) ->
        parseReportOptions
          (options {reportWorkspaceInputs = prependHieRoot hieRoot (reportWorkspaceInputs options)})
          remaining
  "--hie-root" : _ ->
    Left (MissingFlagArgument ReportName HieRootFlag)
  argument : remaining
    | isFlag argument ->
        Left (UnknownFlag ReportName argument)
    | otherwise ->
        parseReportOptions
          (options {reportWorkspaceInputs = prependRoot argument (reportWorkspaceInputs options)})
          remaining

parseDiagnoseOptions :: DiagnoseOptions -> [String] -> Either UsageError DiagnoseOptions
parseDiagnoseOptions options = \case
  [] ->
    finalizeWorkspaceOptions DiagnoseName diagnoseWorkspaceInputs setDiagnoseWorkspaceInputs options
  "--json" : remaining ->
    parseDiagnoseOptions (options {diagnoseOutputFormat = JsonOutput}) remaining
  "--hie-root" : hieRoot : remaining
    | not (isFlag hieRoot) ->
        parseDiagnoseOptions
          (options {diagnoseWorkspaceInputs = prependHieRoot hieRoot (diagnoseWorkspaceInputs options)})
          remaining
  "--hie-root" : _ ->
    Left (MissingFlagArgument DiagnoseName HieRootFlag)
  argument : remaining
    | isFlag argument ->
        Left (UnknownFlag DiagnoseName argument)
    | otherwise ->
        parseDiagnoseOptions
          (options {diagnoseWorkspaceInputs = prependRoot argument (diagnoseWorkspaceInputs options)})
          remaining

parseCorpusOptions :: CorpusOptions -> [String] -> Either UsageError CorpusOptions
parseCorpusOptions options = \case
  [] ->
    finalizeWorkspaceOptions CorpusName corpusWorkspaceInputs setCorpusWorkspaceInputs options
  "--json" : remaining ->
    parseCorpusOptions options remaining
  "--baseline" : baselinePath : remaining
    | not (isFlag baselinePath) ->
        parseCorpusOptions (options {corpusBaselinePath = Just baselinePath}) remaining
  "--baseline" : _ ->
    Left (MissingFlagArgument CorpusName BaselineFlag)
  "--hie-root" : hieRoot : remaining
    | not (isFlag hieRoot) ->
        parseCorpusOptions
          (options {corpusWorkspaceInputs = prependHieRoot hieRoot (corpusWorkspaceInputs options)})
          remaining
  "--hie-root" : _ ->
    Left (MissingFlagArgument CorpusName HieRootFlag)
  argument : remaining
    | isFlag argument ->
        Left (UnknownFlag CorpusName argument)
    | otherwise ->
        parseCorpusOptions
          (options {corpusWorkspaceInputs = prependRoot argument (corpusWorkspaceInputs options)})
          remaining

parseWriteOptions :: WriteOptions -> [String] -> Either UsageError WriteOptions
parseWriteOptions options = \case
  [] ->
    finalizeWorkspaceOptions WriteName writeWorkspaceInputs setWriteWorkspaceInputs options
  "--candidates" : remaining ->
    parseWriteOptions (options {writeSelection = CandidateWrite}) remaining
  "--json" : remaining ->
    parseWriteOptions (options {writeOutputFormat = JsonOutput}) remaining
  "--summary" : remaining ->
    selectWriteDetailTier TierSummary options >>= (`parseWriteOptions` remaining)
  "--full" : remaining ->
    selectWriteDetailTier TierFull options >>= (`parseWriteOptions` remaining)
  "--hie-root" : hieRoot : remaining
    | not (isFlag hieRoot) ->
        parseWriteOptions
          (options {writeWorkspaceInputs = prependHieRoot hieRoot (writeWorkspaceInputs options)})
          remaining
  "--hie-root" : _ ->
    Left (MissingFlagArgument WriteName HieRootFlag)
  argument : remaining
    | isFlag argument ->
        Left (UnknownFlag WriteName argument)
    | otherwise ->
        parseWriteOptions
          (options {writeWorkspaceInputs = prependRoot argument (writeWorkspaceInputs options)})
          remaining

selectReportDetailTier :: DetailTier -> ReportOptions -> Either UsageError ReportOptions
selectReportDetailTier requestedTier options =
  (\detailTier -> options {reportDetailTier = detailTier})
    <$> selectDetailTier ReportName requestedTier (reportDetailTier options)

selectWriteDetailTier :: DetailTier -> WriteOptions -> Either UsageError WriteOptions
selectWriteDetailTier requestedTier options =
  (\detailTier -> options {writeDetailTier = detailTier})
    <$> selectDetailTier WriteName requestedTier (writeDetailTier options)

selectDetailTier :: CommandName -> DetailTier -> DetailTier -> Either UsageError DetailTier
selectDetailTier commandName requestedTier currentTier =
  case (currentTier, requestedTier) of
    (TierStandard, tier) -> Right tier
    (TierSummary, TierSummary) -> Right TierSummary
    (TierFull, TierFull) -> Right TierFull
    _ -> Left (ConflictingDetailTiers commandName)

finalizeWorkspaceOptions :: CommandName -> (options -> WorkspaceInputs) -> (WorkspaceInputs -> options -> options) -> options -> Either UsageError options
finalizeWorkspaceOptions commandName getInputs setInputs options =
  setInputs <$> finalizeWorkspaceInputs commandName (getInputs options) <*> pure options

finalizeWorkspaceInputs :: CommandName -> WorkspaceInputs -> Either UsageError WorkspaceInputs
finalizeWorkspaceInputs commandName inputs
  | null (inputRoots inputs) =
      Left (MissingRoots commandName)
  | otherwise =
      Right
        inputs
          { inputHieRoots = reverse (inputHieRoots inputs),
            inputRoots = reverse (inputRoots inputs)
          }

setReportWorkspaceInputs :: WorkspaceInputs -> ReportOptions -> ReportOptions
setReportWorkspaceInputs inputs options = options {reportWorkspaceInputs = inputs}

setDiagnoseWorkspaceInputs :: WorkspaceInputs -> DiagnoseOptions -> DiagnoseOptions
setDiagnoseWorkspaceInputs inputs options = options {diagnoseWorkspaceInputs = inputs}

setCorpusWorkspaceInputs :: WorkspaceInputs -> CorpusOptions -> CorpusOptions
setCorpusWorkspaceInputs inputs options = options {corpusWorkspaceInputs = inputs}

setWriteWorkspaceInputs :: WorkspaceInputs -> WriteOptions -> WriteOptions
setWriteWorkspaceInputs inputs options = options {writeWorkspaceInputs = inputs}

prependHieRoot :: FilePath -> WorkspaceInputs -> WorkspaceInputs
prependHieRoot hieRoot inputs = inputs {inputHieRoots = hieRoot : inputHieRoots inputs}

prependRoot :: FilePath -> WorkspaceInputs -> WorkspaceInputs
prependRoot root inputs = inputs {inputRoots = root : inputRoots inputs}

isFlag :: String -> Bool
isFlag = \case
  '-' : '-' : _ -> True
  _ -> False

runCommand :: Command -> IO ()
runCommand = \case
  ReportCommand options -> runReport options
  DiagnoseCommand options -> runDiagnose options
  CorpusCommand options -> runCorpus options
  WriteCommand options -> runWrite options

runReport :: ReportOptions -> IO ()
runReport options = do
  (improvements, report) <- improveWorkspaceFor (reportWorkspaceInputs options)
  putStr (renderReportOutput options report improvements)
  finishExecution (workspaceExecutionStatus report)

runDiagnose :: DiagnoseOptions -> IO ()
runDiagnose options = do
  (workspaceErrors, workloads) <- enumerateWorkspaceFor (diagnoseWorkspaceInputs options)
  let diagnoses = fmap diagnoseWorkload workloads
  putStr (renderDiagnoseOutput options workspaceErrors diagnoses)
  finishExecution (diagnoseExecutionStatus workspaceErrors diagnoses)

runCorpus :: CorpusOptions -> IO ()
runCorpus options = do
  (_improvements, report) <- improveWorkspaceFor (corpusWorkspaceInputs options)
  baseline <- traverse readBaseline (corpusBaselinePath options)
  putStr (renderJson (corpusEnvelopeJson report baseline) <> "\n")
  finishExecution (corpusExecutionStatus report baseline)

runWrite :: WriteOptions -> IO ()
runWrite options = do
  (improvements, report) <- improveWorkspaceFor (writeWorkspaceInputs options)
  outcomes <- traverse (writeImprovement (writeSelection options)) improvements
  putStr (renderWriteOutput options report improvements outcomes)
  finishExecution (writeExecutionStatus report outcomes)

improveWorkspaceFor :: WorkspaceInputs -> IO ([(ModuleWorkload, ModuleImprovement)], WorkspaceReport)
improveWorkspaceFor inputs =
  improveWorkspace defaultNebulaConfig (inputRoots inputs) (inputHieRoots inputs)

enumerateWorkspaceFor :: WorkspaceInputs -> IO ([NebulaError], [ModuleWorkload])
enumerateWorkspaceFor inputs =
  enumerateModuleWorkloads (inputRoots inputs) (inputHieRoots inputs)

renderReportOutput :: ReportOptions -> WorkspaceReport -> [(ModuleWorkload, ModuleImprovement)] -> String
renderReportOutput options report improvements =
  case reportOutputFormat options of
    JsonOutput ->
      renderJson (workspaceEnvelopeJson (reportDetailTier options) report Nothing) <> "\n"
    TextOutput ->
      unlines (renderWorkspaceReport report <> foldMap renderReportImprovement improvements)

renderReportImprovement :: (ModuleWorkload, ModuleImprovement) -> [String]
renderReportImprovement (workload, improvement) =
  renderModuleDiff
    (mpPath modulePatch)
    (mwSource workload)
    modulePatch
    (miSeal improvement)
  where
    modulePatch = miPatch improvement

diagnoseWorkload :: ModuleWorkload -> (FilePath, Either NebulaError [DiagnoseRegion])
diagnoseWorkload workload =
  ( mwPath workload,
    either (Left . snd) Right (diagnoseModule defaultNebulaConfig workload)
  )

renderDiagnoseOutput :: DiagnoseOptions -> [NebulaError] -> [(FilePath, Either NebulaError [DiagnoseRegion])] -> String
renderDiagnoseOutput options workspaceErrors diagnoses =
  case diagnoseOutputFormat options of
    JsonOutput ->
      renderJson (diagnoseEnvelopeJson workspaceErrors diagnoses) <> "\n"
    TextOutput ->
      unlines
        ( fmap renderDiagnoseWorkspaceError workspaceErrors
            <> foldMap renderModuleDiagnosis diagnoses
        )

renderDiagnoseWorkspaceError :: NebulaError -> String
renderDiagnoseWorkspaceError nebulaError =
  "diagnose enumeration failure: " <> renderNebulaError nebulaError

renderModuleDiagnosis :: (FilePath, Either NebulaError [DiagnoseRegion]) -> [String]
renderModuleDiagnosis (path, result) =
  case result of
    Left nebulaError ->
      ["diagnose failure " <> path <> ": " <> renderNebulaError nebulaError]
    Right regions ->
      renderDiagnoseRegions path regions

renderNebulaError :: NebulaError -> String
renderNebulaError nebulaError =
  nebulaErrorKey nebulaError <> maybe "" (": " <>) (nebulaErrorMessage nebulaError)

readBaseline :: FilePath -> IO (Either BaselineFailure J)
readBaseline baselinePath =
  fmap
    (either (Left . BaselineUnreadable . displayException) parseBaseline)
    (readFileStrictly baselinePath)

readFileStrictly :: FilePath -> IO (Either IOException String)
readFileStrictly path =
  try (readFile path >>= forceString)

forceString :: String -> IO String
forceString contents =
  evaluate (length contents) *> pure contents

writeImprovement :: WriteSelection -> (ModuleWorkload, ModuleImprovement) -> IO WriteOutcome
writeImprovement selection (workload, improvement) =
  case (selection, miSeal improvement) of
    (_, Sealed sealedSource) ->
      writeSource
        (mpPath modulePatch)
        WriteWritten
        Nothing
        (sealedSourceText sealedSource)
    (SealedWrite, SealRefused _ refusal) ->
      pure (refusedWriteOutcome (mpPath modulePatch) refusal Nothing)
    (CandidateWrite, SealRefused _ refusal)
      | modulePatchHasContent modulePatch ->
          case patchedModuleSource modulePatch (mwSource workload) of
            Left _ ->
              pure
                ( refusedWriteOutcome
                    (mpPath modulePatch)
                    refusal
                    (Just "candidate source rendering failed after seal refusal")
                )
            Right candidateSource ->
              writeSource
                (mpPath modulePatch)
                WriteCandidateWritten
                (Just (nebulaErrorKey refusal))
                candidateSource
    (CandidateWrite, SealRefused _ refusal) ->
      pure (refusedWriteOutcome (mpPath modulePatch) refusal Nothing)
    (_, SealEmpty) ->
      pure
        WriteOutcome
          { woPath = mpPath modulePatch,
            woStatus = WriteSkipped,
            woReasonKey = Nothing,
            woMessage = Nothing
          }
  where
    modulePatch = miPatch improvement

refusedWriteOutcome :: FilePath -> NebulaError -> Maybe String -> WriteOutcome
refusedWriteOutcome path sealFailure message =
  WriteOutcome
    { woPath = path,
      woStatus = WriteRefused,
      woReasonKey = Just (nebulaErrorKey sealFailure),
      woMessage = maybe (nebulaErrorMessage sealFailure) Just message
    }

writeSource :: FilePath -> WriteStatus -> Maybe String -> String -> IO WriteOutcome
writeSource path successStatus reasonKey contents =
  either (ioErrorWriteOutcome path) (const successfulOutcome)
    <$> tryWriteFile path contents
  where
    successfulOutcome =
      WriteOutcome
        { woPath = path,
          woStatus = successStatus,
          woReasonKey = reasonKey,
          woMessage = Nothing
        }

tryWriteFile :: FilePath -> String -> IO (Either IOException ())
tryWriteFile path contents =
  try (writeFile path contents)

ioErrorWriteOutcome :: FilePath -> IOException -> WriteOutcome
ioErrorWriteOutcome path ioException =
  WriteOutcome
    { woPath = path,
      woStatus = WriteIoError,
      woReasonKey = Nothing,
      woMessage = Just (displayException ioException)
    }

renderWriteOutput :: WriteOptions -> WorkspaceReport -> [(ModuleWorkload, ModuleImprovement)] -> [WriteOutcome] -> String
renderWriteOutput options report improvements outcomes =
  case writeOutputFormat options of
    JsonOutput ->
      renderJson (workspaceEnvelopeJson (writeDetailTier options) report (Just outcomes)) <> "\n"
    TextOutput ->
      unlines
        ( renderWorkspaceReport report
            <> concat (zipWith (renderWrittenImprovement (writeSelection options)) improvements outcomes)
        )

renderWrittenImprovement :: WriteSelection -> (ModuleWorkload, ModuleImprovement) -> WriteOutcome -> [String]
renderWrittenImprovement selection (workload, improvement) outcome =
  renderedDiffLines selection workload modulePatch (miSeal improvement)
    <> renderWriteOutcome selection outcome
  where
    modulePatch = miPatch improvement

renderedDiffLines :: WriteSelection -> ModuleWorkload -> ModulePatch -> SealOutcome -> [String]
renderedDiffLines selection workload modulePatch sealOutcome =
  case selection of
    CandidateWrite ->
      renderModuleCandidateDiff (mpPath modulePatch) (mwSource workload) modulePatch
    SealedWrite ->
      renderModuleDiff (mpPath modulePatch) (mwSource workload) modulePatch sealOutcome

renderWriteOutcome :: WriteSelection -> WriteOutcome -> [String]
renderWriteOutcome selection outcome =
  case woStatus outcome of
    WriteWritten ->
      ["wrote " <> woPath outcome]
    WriteCandidateWritten ->
      ["wrote candidate " <> woPath outcome <> " after seal refusal" <> renderOutcomeDiagnostic outcome]
    WriteRefused ->
      [writeRefusalPrefix selection <> woPath outcome <> renderOutcomeDiagnostic outcome]
    WriteSkipped ->
      []
    WriteIoError ->
      ["write failed for " <> woPath outcome <> renderOutcomeDiagnostic outcome]

writeRefusalPrefix :: WriteSelection -> String
writeRefusalPrefix = \case
  SealedWrite -> "write refused for "
  CandidateWrite -> "candidate write refused for "

renderOutcomeDiagnostic :: WriteOutcome -> String
renderOutcomeDiagnostic outcome =
  case (woReasonKey outcome, woMessage outcome) of
    (Nothing, Nothing) -> ""
    (Just reasonKey, Nothing) -> ": " <> reasonKey
    (Nothing, Just message) -> ": " <> message
    (Just reasonKey, Just message) -> ": " <> reasonKey <> "; " <> message

workspaceExecutionStatus :: WorkspaceReport -> ExecutionStatus
workspaceExecutionStatus report
  | null (wrModules report) = ExecutionDegraded
  | not (null (wrModuleFailures report)) = ExecutionDegraded
  | not (null (wrWorkspaceErrors report)) = ExecutionDegraded
  | otherwise = ExecutionClean

diagnoseExecutionStatus :: [NebulaError] -> [(FilePath, Either NebulaError [DiagnoseRegion])] -> ExecutionStatus
diagnoseExecutionStatus workspaceErrors diagnoses
  | null diagnoses = ExecutionDegraded
  | not (null workspaceErrors) = ExecutionDegraded
  | any (isLeft . snd) diagnoses = ExecutionDegraded
  | otherwise = ExecutionClean

corpusExecutionStatus :: WorkspaceReport -> Maybe (Either BaselineFailure J) -> ExecutionStatus
corpusExecutionStatus report baseline
  | isExecutionDegraded (workspaceExecutionStatus report) = ExecutionDegraded
  | maybe False isLeft baseline = ExecutionDegraded
  | otherwise = ExecutionClean

writeExecutionStatus :: WorkspaceReport -> [WriteOutcome] -> ExecutionStatus
writeExecutionStatus report outcomes
  | isExecutionDegraded (workspaceExecutionStatus report) = ExecutionDegraded
  | any isWriteIoError outcomes = ExecutionDegraded
  | otherwise = ExecutionClean

isWriteIoError :: WriteOutcome -> Bool
isWriteIoError outcome =
  case woStatus outcome of
    WriteIoError -> True
    _ -> False

isExecutionDegraded :: ExecutionStatus -> Bool
isExecutionDegraded = \case
  ExecutionClean -> False
  ExecutionDegraded -> True

finishExecution :: ExecutionStatus -> IO ()
finishExecution = \case
  ExecutionClean -> pure ()
  ExecutionDegraded -> exitWith (ExitFailure 1)

reportUsageError :: UsageError -> IO ()
reportUsageError usageError =
  hPutStr stderr (renderUsageError usageError <> "\n" <> usageText)
    *> exitWith (ExitFailure 2)

renderUsageError :: UsageError -> String
renderUsageError = \case
  MissingSubcommand ->
    "missing subcommand"
  UnknownSubcommand subcommand ->
    "unknown subcommand: " <> subcommand
  UnknownFlag commandName flag ->
    "unknown flag for " <> commandNameKey commandName <> ": " <> flag
  MissingFlagArgument commandName flag ->
    flagWithArgumentKey flag <> " requires an argument for " <> commandNameKey commandName
  MissingRoots commandName ->
    commandNameKey commandName <> " requires at least one root"
  ConflictingDetailTiers commandName ->
    "--summary and --full are mutually exclusive for " <> commandNameKey commandName

commandNameKey :: CommandName -> String
commandNameKey = \case
  ReportName -> "report"
  DiagnoseName -> "diagnose"
  CorpusName -> "corpus"
  WriteName -> "write"

flagWithArgumentKey :: FlagWithArgument -> String
flagWithArgumentKey = \case
  BaselineFlag -> "--baseline"
  HieRootFlag -> "--hie-root"

usageText :: String
usageText =
  unlines
    [ "usage:",
      "  melusine-nebula report [--json] [--summary | --full] [--hie-root DIR]... ROOT...",
      "  melusine-nebula diagnose [--json] [--hie-root DIR]... ROOT...",
      "  melusine-nebula corpus [--json] [--baseline FILE] [--hie-root DIR]... ROOT...",
      "  melusine-nebula write [--candidates] [--json] [--summary | --full] [--hie-root DIR]... ROOT..."
    ]
