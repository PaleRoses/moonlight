module Main
  ( main
  ) where

import Control.Exception (bracket, evaluate)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, find, intercalate, isInfixOf, isPrefixOf)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Fixture
  ( BenchmarkCaseClass (..)
  , BenchmarkFixture
  , CaseId (..)
  , ProbeBudget (..)
  , ProbeBudgetClass (..)
  , ProbeCase (..)
  , ProbeRunResult (..)
  , loadBenchmarkFixtures
  , resolveProbeBudget
  )
import Functor qualified as FunctorBench
import Gluing qualified as GluingBench
import Matrix qualified as MatrixBench
import Morse qualified as MorseBench
import Presentation qualified as PresentationBench
import Pruning qualified as PruningBench
import Site qualified as SiteBench
import Structural qualified as StructuralBench
import Triangulated qualified as TriangulatedBench
import System.Directory (doesFileExist, findExecutable, getTemporaryDirectory, removeFile)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (ExitCode (..), die, exitSuccess, exitWith)
import System.IO (BufferMode (LineBuffering), Handle, hClose, hPutStr, hSetBuffering, openTempFile, stderr, stdout)
import System.Process
  ( CreateProcess (std_err, std_out)
  , StdStream (UseHandle)
  , createProcess
  , proc
  , readProcessWithExitCode
  , terminateProcess
  , waitForProcess
  )
import System.Timeout (timeout)
import Text.Read (readMaybe)

data ProbeOptions = ProbeOptions
  { poIncludeLarge :: !Bool
  , poListOnly :: !Bool
  , poAllHostile :: !Bool
  , poCsvFile :: !(Maybe FilePath)
  , poWorkerCase :: !(Maybe CaseId)
  , poSelectedCase :: !(Maybe CaseId)
  , poTimeoutOverride :: !(Maybe Int)
  , poHeapOverride :: !(Maybe Int)
  , poProtocolSelfTest :: !Bool
  }

data ProbeSupervisorResult = ProbeSupervisorResult
  { psrCaseId :: !CaseId
  , psrStatus :: !ProbeSupervisorStatus
  , psrElapsedMilliseconds :: !Word64
  , psrTimeoutSeconds :: !Int
  , psrHeapMegabytes :: !Int
  , psrExitCode :: !(Maybe Int)
  , psrChecksum :: !(Maybe Int)
  , psrMessage :: !String
  }

data ProbeSupervisorStatus
  = ProbeSucceeded
  | ProbeDomainRejected
  | ProbeTimedOut
  | ProbeHeapCapped
  | ProbeFailed
  deriving stock (Eq, Ord, Show)

data ProbeWorkerStatus
  = WorkerSucceeded
  | WorkerDomainRejected
  deriving stock (Eq, Ord, Read, Show)

data ProbeWorkerRecord = ProbeWorkerRecord
  { pwrCaseId :: !CaseId
  , pwrStatus :: !ProbeWorkerStatus
  , pwrChecksum :: !(Maybe Int)
  , pwrElapsedMilliseconds :: !Word64
  , pwrDetail :: !String
  }
  deriving stock (Eq, Ord, Read, Show)

data ProtocolWorkerMode
  = ProtocolLargeStderr
  | ProtocolLargeMalformedStdout
  | ProtocolMalformedRecord
  | ProtocolMismatchedRecord
  deriving stock (Eq, Ord, Show)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  options <- getArgs >>= either die pure . parseArgs
  if poProtocolSelfTest options
    then runProtocolSelfTest options
    else do
      fixtures <- loadBenchmarkFixtures (poIncludeLarge options)
      let probeRegistry = allProbeCases fixtures
      runSelectedProbeMode options probeRegistry

runSelectedProbeMode :: ProbeOptions -> [ProbeCase] -> IO ()
runSelectedProbeMode options probeRegistry =
  if poListOnly options
    then do
      _ <- traverse (putStrLn . renderProbeListing) probeRegistry
      exitSuccess
    else case poWorkerCase options of
      Just caseIdValue ->
        runWorker probeRegistry caseIdValue
      Nothing ->
        if poAllHostile options
          then runAllHostile probeRegistry options
          else case poSelectedCase options of
            Just caseIdValue ->
              runSupervisor probeRegistry options caseIdValue
                >>= \probeResult -> writeCsvIfRequested options [probeResult] *> exitOnProbeFailures [probeResult]
            Nothing ->
              die "moonlight-derived-bench-probe: expected a case id, --all-hostile, or --list"

allProbeCases :: [BenchmarkFixture] -> [ProbeCase]
allProbeCases fixtures =
  SiteBench.probeCases fixtures
    <> MatrixBench.probeCases fixtures
    <> StructuralBench.probeCases fixtures
    <> GluingBench.probeCases fixtures
    <> FunctorBench.probeCases fixtures
    <> MorseBench.probeCases fixtures
    <> PruningBench.probeCases fixtures
    <> TriangulatedBench.probeCases fixtures
    <> PresentationBench.probeCases fixtures

renderProbeListing :: ProbeCase -> String
renderProbeListing probeCase =
  renderCaseId (pcId probeCase)
    <> " ["
    <> show (pcBudgetClass probeCase)
    <> "]"

renderCaseId :: CaseId -> String
renderCaseId (CaseId caseIdValue) =
  caseIdValue

runWorker :: [ProbeCase] -> CaseId -> IO ()
runWorker probeRegistry caseIdValue =
  case protocolWorkerMode caseIdValue of
    Just protocolMode ->
      runProtocolWorker protocolMode caseIdValue
    Nothing -> case find ((== caseIdValue) . pcId) probeRegistry of
      Nothing ->
        die ("moonlight-derived-bench-probe: unknown worker case " <> show (renderCaseId caseIdValue))
      Just probeCase -> do
        startNanoseconds <- getMonotonicTimeNSec
        probeRunResult <- pcRun probeCase >>= evaluate
        endNanoseconds <- getMonotonicTimeNSec
        print (workerRecordFromResult probeCase startNanoseconds endNanoseconds probeRunResult)

workerRecordFromResult :: ProbeCase -> Word64 -> Word64 -> ProbeRunResult -> ProbeWorkerRecord
workerRecordFromResult probeCase startNanoseconds endNanoseconds probeRunResult =
  case probeRunResult of
    ProbeRunRejected failureMessage ->
      ProbeWorkerRecord
        { pwrCaseId = pcId probeCase
        , pwrStatus = WorkerDomainRejected
        , pwrChecksum = Nothing
        , pwrElapsedMilliseconds = (endNanoseconds - startNanoseconds) `div` 1000000
        , pwrDetail = failureMessage
        }
    ProbeRunSucceeded checksumValue ->
      ProbeWorkerRecord
        { pwrCaseId = pcId probeCase
        , pwrStatus = WorkerSucceeded
        , pwrChecksum = Just checksumValue
        , pwrElapsedMilliseconds = (endNanoseconds - startNanoseconds) `div` 1000000
        , pwrDetail = ""
        }

runProtocolWorker :: ProtocolWorkerMode -> CaseId -> IO ()
runProtocolWorker protocolMode caseIdValue =
  case protocolMode of
    ProtocolLargeStderr ->
      hPutStr stderr (replicate protocolPayloadSize 'e')
        *> print
          ProbeWorkerRecord
            { pwrCaseId = caseIdValue
            , pwrStatus = WorkerSucceeded
            , pwrChecksum = Just 1
            , pwrElapsedMilliseconds = 0
            , pwrDetail = ""
            }
    ProtocolLargeMalformedStdout ->
      putStr (replicate protocolPayloadSize 'x')
    ProtocolMalformedRecord ->
      putStrLn "malformed-worker-record"
    ProtocolMismatchedRecord ->
      print
        ProbeWorkerRecord
          { pwrCaseId = CaseId "__protocol/wrong-case"
          , pwrStatus = WorkerSucceeded
          , pwrChecksum = Just 1
          , pwrElapsedMilliseconds = 0
          , pwrDetail = ""
          }

protocolPayloadSize :: Int
protocolPayloadSize =
  131072

protocolWorkerMode :: CaseId -> Maybe ProtocolWorkerMode
protocolWorkerMode caseIdValue =
  case renderCaseId caseIdValue of
    "__protocol/large-stderr" -> Just ProtocolLargeStderr
    "__protocol/large-malformed-stdout" -> Just ProtocolLargeMalformedStdout
    "__protocol/malformed-record" -> Just ProtocolMalformedRecord
    "__protocol/mismatched-record" -> Just ProtocolMismatchedRecord
    _ -> Nothing

protocolProbeCases :: [ProbeCase]
protocolProbeCases =
  fmap
    ( \caseIdValue ->
        ProbeCase
          { pcId = caseIdValue
          , pcLabel = renderCaseId caseIdValue
          , pcClass = HostileProbe
          , pcBudgetClass = ProbeBudgetModerate
          , pcRun = pure (ProbeRunSucceeded 1)
          }
    )
    ( fmap
        CaseId
        [ "__protocol/large-stderr"
        , "__protocol/large-malformed-stdout"
        , "__protocol/malformed-record"
        , "__protocol/mismatched-record"
        ]
    )

runProtocolSelfTest :: ProbeOptions -> IO ()
runProtocolSelfTest options = do
  probeResults <- traverse (runSupervisor protocolProbeCases options . pcId) protocolProbeCases
  let observedStatuses = fmap psrStatus probeResults
      expectedStatuses = [ProbeSucceeded, ProbeFailed, ProbeFailed, ProbeFailed]
      csvRoundTripFixture = "comma,\"quote\"\r\nline"
      expectedCsvField = "\"comma,\"\"quote\"\"\r\nline\""
  if observedStatuses == expectedStatuses && renderCsvField csvRoundTripFixture == expectedCsvField
    then putStrLn "hostile probe protocol self-test: success"
    else
      die
        ( "hostile probe protocol self-test failed: statuses="
            <> show observedStatuses
            <> ", csv="
            <> show (renderCsvField csvRoundTripFixture)
        )

runAllHostile :: [ProbeCase] -> ProbeOptions -> IO ()
runAllHostile probeRegistry options = do
  probeResults <- traverse (runSupervisor probeRegistry options . pcId) probeRegistry
  writeCsvIfRequested options probeResults
  exitOnProbeFailures probeResults

runSupervisor :: [ProbeCase] -> ProbeOptions -> CaseId -> IO ProbeSupervisorResult
runSupervisor probeRegistry options caseIdValue =
  case find ((== caseIdValue) . pcId) probeRegistry of
    Nothing ->
      die ("moonlight-derived-bench-probe: unknown case " <> show (renderCaseId caseIdValue))
    Just probeCase -> do
      executablePath <- resolveWorkerExecutable
      let budgetValue = effectiveBudget options probeCase
          workerArgs =
            [ "--worker"
            , renderCaseId (pcId probeCase)
            ]
              <> ["--large" | poIncludeLarge options]
              <> [ "+RTS"
                 , "-N1"
                 , "-M" <> show (pbHeapMegabytes budgetValue) <> "m"
                 , "-RTS"
                 ]
      putStrLn
        ( "starting probe "
            <> renderCaseId (pcId probeCase)
            <> " timeout_s="
            <> show (pbTimeoutSeconds budgetValue)
            <> " heap_mb="
            <> show (pbHeapMegabytes budgetValue)
        )
      supervisorStartNanoseconds <- getMonotonicTimeNSec
      withCaptureFile "moonlight-derived-probe-stdout"
        ( \stdoutPath stdoutHandle ->
            withCaptureFile "moonlight-derived-probe-stderr"
              ( \stderrPath stderrHandle -> do
                  (_, _, _, processHandle) <-
                    createProcess
                      (proc executablePath workerArgs)
                        { std_out = UseHandle stdoutHandle
                        , std_err = UseHandle stderrHandle
                        }
                  maybeExitCode <-
                    timeout
                      (pbTimeoutSeconds budgetValue * 1000000)
                      (waitForProcess processHandle)
                  case maybeExitCode of
                    Nothing ->
                      terminateProcess processHandle
                        *> waitForProcess processHandle
                        *> finishCaptured
                          probeCase
                          budgetValue
                          supervisorStartNanoseconds
                          stdoutPath
                          stderrPath
                          ProbeTimedOut
                          (Just 124)
                          Nothing
                          "timeout"
                    Just ExitSuccess ->
                      readCapturedWorker stdoutPath
                        >>= finishWorkerRecord
                          probeCase
                          budgetValue
                          supervisorStartNanoseconds
                          stdoutPath
                          stderrPath
                    Just (ExitFailure exitCodeValue) ->
                      finishCaptured
                        probeCase
                        budgetValue
                        supervisorStartNanoseconds
                        stdoutPath
                        stderrPath
                        (if exitCodeValue == 251 then ProbeHeapCapped else ProbeFailed)
                        (Just exitCodeValue)
                        Nothing
                        "worker-failure"
              )
        )

withCaptureFile :: String -> (FilePath -> Handle -> IO value) -> IO value
withCaptureFile template useCapture = do
  temporaryDirectory <- getTemporaryDirectory
  bracket
    (openTempFile temporaryDirectory template)
    (\(capturePath, captureHandle) -> hClose captureHandle *> removeFile capturePath)
    (uncurry useCapture)

readCapturedWorker :: FilePath -> IO (Either String ProbeWorkerRecord)
readCapturedWorker stdoutPath =
  maybe
    (Left "missing or malformed worker record")
    Right
    . readMaybe
    . trimWhitespace
    <$> readFile stdoutPath

finishWorkerRecord :: ProbeCase -> ProbeBudget -> Word64 -> FilePath -> FilePath -> Either String ProbeWorkerRecord -> IO ProbeSupervisorResult
finishWorkerRecord probeCase budgetValue startedAt stdoutPath stderrPath parsedRecord =
  case parsedRecord of
    Left parseFailure ->
      finishCaptured probeCase budgetValue startedAt stdoutPath stderrPath ProbeFailed Nothing Nothing parseFailure
    Right workerRecord
      | pwrCaseId workerRecord /= pcId probeCase ->
          finishCaptured probeCase budgetValue startedAt stdoutPath stderrPath ProbeFailed Nothing Nothing "worker case mismatch"
      | otherwise ->
          case pwrStatus workerRecord of
            WorkerSucceeded ->
              case pwrChecksum workerRecord of
                Nothing ->
                  finishCaptured probeCase budgetValue startedAt stdoutPath stderrPath ProbeFailed Nothing Nothing "successful worker omitted checksum"
                Just checksumValue ->
                  finishCaptured probeCase budgetValue startedAt stdoutPath stderrPath ProbeSucceeded Nothing (Just checksumValue) (pwrDetail workerRecord)
            WorkerDomainRejected ->
              finishCaptured probeCase budgetValue startedAt stdoutPath stderrPath ProbeDomainRejected Nothing Nothing (pwrDetail workerRecord)

finishCaptured :: ProbeCase -> ProbeBudget -> Word64 -> FilePath -> FilePath -> ProbeSupervisorStatus -> Maybe Int -> Maybe Int -> String -> IO ProbeSupervisorResult
finishCaptured probeCase budgetValue startedAt _stdoutPath stderrPath statusValue exitCodeValue checksumValue messageValue = do
  stderrValue <- readFile stderrPath >>= forceString
  emitStderr stderrValue
  endedAt <- getMonotonicTimeNSec
  pure
    ProbeSupervisorResult
      { psrCaseId = pcId probeCase
      , psrStatus = statusValue
      , psrElapsedMilliseconds = (endedAt - startedAt) `div` 1000000
      , psrTimeoutSeconds = pbTimeoutSeconds budgetValue
      , psrHeapMegabytes = pbHeapMegabytes budgetValue
      , psrExitCode = exitCodeValue
      , psrChecksum = checksumValue
      , psrMessage = messageValue
      }

emitStderr :: String -> IO ()
emitStderr stderrValue =
  if null stderrValue
    then pure ()
    else hPutStr stderr stderrValue

exitOnProbeFailures :: [ProbeSupervisorResult] -> IO ()
exitOnProbeFailures probeResults =
  if all probeSucceeded probeResults
    then exitSuccess
    else exitWith (ExitFailure 1)

probeSucceeded :: ProbeSupervisorResult -> Bool
probeSucceeded probeResult =
  psrStatus probeResult == ProbeSucceeded

writeCsvIfRequested :: ProbeOptions -> [ProbeSupervisorResult] -> IO ()
writeCsvIfRequested options probeResults =
  case poCsvFile options of
    Nothing ->
      pure ()
    Just csvPath ->
      writeFile csvPath (renderProbeCsv probeResults)

renderProbeCsv :: [ProbeSupervisorResult] -> String
renderProbeCsv probeResults =
  unlines
    ( "case_id,status,elapsed_ms,timeout_s,heap_mb,exit_code,checksum,message"
        : fmap renderProbeCsvRow probeResults
    )

renderProbeCsvRow :: ProbeSupervisorResult -> String
renderProbeCsvRow probeResult =
  intercalate ","
    ( fmap
        renderCsvField
        [ renderCaseId (psrCaseId probeResult)
        , renderProbeSupervisorStatus (psrStatus probeResult)
        , show (psrElapsedMilliseconds probeResult)
        , show (psrTimeoutSeconds probeResult)
        , show (psrHeapMegabytes probeResult)
        , maybe "" show (psrExitCode probeResult)
        , maybe "" show (psrChecksum probeResult)
        , psrMessage probeResult
        ]
    )

renderCsvField :: String -> String
renderCsvField fieldValue =
  if any (`elem` [',', '"', '\r', '\n']) fieldValue
    then '"' : foldr escapeCharacter "\"" fieldValue
    else fieldValue
  where
    escapeCharacter '"' escapedValue = '"' : '"' : escapedValue
    escapeCharacter characterValue escapedValue = characterValue : escapedValue

renderProbeSupervisorStatus :: ProbeSupervisorStatus -> String
renderProbeSupervisorStatus probeSupervisorStatus =
  case probeSupervisorStatus of
    ProbeSucceeded ->
      "success"
    ProbeDomainRejected ->
      "domain-failure"
    ProbeTimedOut ->
      "timeout"
    ProbeHeapCapped ->
      "heap-cap"
    ProbeFailed ->
      "failure"

effectiveBudget :: ProbeOptions -> ProbeCase -> ProbeBudget
effectiveBudget options probeCase =
  let ProbeBudget{pbTimeoutSeconds = defaultTimeoutSeconds, pbHeapMegabytes = defaultHeapMegabytes} =
        resolveProbeBudget (pcBudgetClass probeCase)
   in ProbeBudget
        { pbTimeoutSeconds = maybe defaultTimeoutSeconds id (poTimeoutOverride options)
        , pbHeapMegabytes = maybe defaultHeapMegabytes id (poHeapOverride options)
        }

forceString :: String -> IO String
forceString stringValue =
  evaluate (length stringValue) >> pure stringValue

resolveWorkerExecutable :: IO FilePath
resolveWorkerExecutable = do
  currentExecutable <- getExecutablePath
  if looksLikeBuiltBinary currentExecutable
    then pure currentExecutable
    else do
      maybeListedExecutable <- resolveListedExecutable
      pure (maybe currentExecutable id maybeListedExecutable)

looksLikeBuiltBinary :: FilePath -> Bool
looksLikeBuiltBinary executablePath =
  "/dist-newstyle/build/" `isInfixOf` executablePath

resolveListedExecutable :: IO (Maybe FilePath)
resolveListedExecutable = do
  maybeCabalExecutable <- findExecutable "cabal"
  case maybeCabalExecutable of
    Nothing ->
      pure Nothing
    Just cabalExecutable -> do
      (exitCodeValue, stdoutValue, _) <-
        readProcessWithExitCode
          cabalExecutable
          ["list-bin", "exe:moonlight-derived-bench-probe"]
          ""
      let candidatePath = trimWhitespace stdoutValue
      case exitCodeValue of
        ExitSuccess ->
          do
            candidateExists <- doesFileExist candidatePath
            pure
              ( if null candidatePath || not candidateExists
                  then Nothing
                  else Just candidatePath
              )
        ExitFailure _ ->
          pure Nothing

trimWhitespace :: String -> String
trimWhitespace =
  dropWhileEnd isSpace . dropWhile isSpace

parseArgs :: [String] -> Either String ProbeOptions
parseArgs =
  go
    ProbeOptions
      { poIncludeLarge = False
      , poListOnly = False
      , poAllHostile = False
      , poCsvFile = Nothing
      , poWorkerCase = Nothing
      , poSelectedCase = Nothing
      , poTimeoutOverride = Nothing
      , poHeapOverride = Nothing
      , poProtocolSelfTest = False
      }
  where
    go options [] =
      Right options
    go options ("--list" : restArgs) =
      go options{poListOnly = True} restArgs
    go options ("--large" : restArgs) =
      go options{poIncludeLarge = True} restArgs
    go options ("--all-hostile" : restArgs) =
      go options{poAllHostile = True} restArgs
    go options ("--protocol-self-test" : restArgs) =
      go options{poProtocolSelfTest = True} restArgs
    go options ("--csv" : csvPath : restArgs) =
      go options{poCsvFile = Just csvPath} restArgs
    go options ("--worker" : caseIdValue : restArgs) =
      go options{poWorkerCase = Just (CaseId caseIdValue)} restArgs
    go options ("--timeout-seconds" : timeoutValue : restArgs) =
      maybe
        (Left ("moonlight-derived-bench-probe: invalid timeout " <> show timeoutValue))
        (\parsedValue -> go options{poTimeoutOverride = Just parsedValue} restArgs)
        (readMaybe timeoutValue)
    go options ("--heap-megabytes" : heapValue : restArgs) =
      maybe
        (Left ("moonlight-derived-bench-probe: invalid heap size " <> show heapValue))
        (\parsedValue -> go options{poHeapOverride = Just parsedValue} restArgs)
        (readMaybe heapValue)
    go options (argumentValue : restArgs)
      | isPrefixOf "--" argumentValue =
          Left ("moonlight-derived-bench-probe: unknown option " <> show argumentValue)
      | otherwise =
          case poSelectedCase options of
            Nothing ->
              go options{poSelectedCase = Just (CaseId argumentValue)} restArgs
            Just _ ->
              Left "moonlight-derived-bench-probe: expected exactly one case id"
