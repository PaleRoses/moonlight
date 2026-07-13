{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main
  ( main,
  )
where

import Control.DeepSeq (force)
import Control.Exception (IOException, evaluate, try)
import Data.Bifunctor (first)
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
import Data.Text.IO qualified as TextIO
import Data.Word (Word64)
import Moonlight.Core
  ( ClassId (..),
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
    measureFreshSample,
  )
import Moonlight.Rewrite.Algebra
  ( PatternQuery (..),
    guardedPatternQuery,
    patternQueryConditions,
    singlePatternQuery,
  )
import Moonlight.Rewrite.Algebra
  ( PatternRewriteError,
    RewriteOrigin (..),
    mkPatternRewrite,
  )
import Moonlight.Rewrite.ProofContext
  ( ProofQueryError,
    ProofRegistry,
    ProofStep (..),
    defaultProofStepInput,
    emptyProofRegistry,
    proofClassesReachableFrom,
    proofReachability,
    proofRegistryRecordedStepCount,
    recordProofStepWith,
    serializeProofLog,
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
  ( CheckedRewrite (..),
    CheckedSystem,
    CheckedSystemError,
    appendCheckedRewrite,
    checkedRewrites,
    checkedSystemFromRewrites,
  )
import Moonlight.Rewrite.System
  ( LogicalDecoration,
    logicalDecoration,
  )
import Moonlight.Rewrite.System
  ( CompiledGuard,
    GuardTerm,
    RewriteCondition (..),
    compileGuard,
    compiledGuardCanonicalNodeWordsWith,
    guardChildIndex,
    guardHasFactTerms,
    guardProjectTerm,
    guardRefTerm,
    data GuardRoot,
  )
import Moonlight.Rewrite.System (FactId (..))
import Moonlight.Rewrite.System (RuleOrigin (..))
import Moonlight.Rewrite.System
  ( RuleNameError,
    mkRuleName,
    ruleNameString,
  )
import System.Environment (getArgs, getExecutablePath)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

data MeasurementMode
  = MeasurementDriver
  | MeasurementWorker !RewriteMeasurementCase !MeasurementSample
  | MeasurementCompare !FilePath !FilePath

data RewriteMeasurementCase
  = ProofReachabilityContiguous4096
  | ProofReachabilitySparse1000000
  | CheckedSystemAppend1024
  | CheckedSystemOrderedProjection1024
  | NaryAntiUnificationArity512Terms16
  | GuardEncodingDepth512
  | QueryConditionCollectionDepth4096
  | NonlinearPatternBindingSubterm4096
  deriving stock (Eq, Show)

newtype MeasurementSample = MeasurementSample
  { measurementSampleValue :: Int
  }
  deriving stock (Eq, Ord, Show)

data MeasurementModeError
  = MeasurementArgumentsInvalid ![String]
  | MeasurementCaseUnknown !String
  | MeasurementSampleInvalid !String
  deriving stock (Show)

data MeasurementCaseFailure
  = MeasurementProofQueryFailed !ProofQueryError
  | MeasurementRuleNameFailed !RuleNameError
  | MeasurementPatternRewriteFailed !(PatternRewriteError (LogicalDecoration ()) [])
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
    measurementRowElapsedNanoseconds :: !Word64,
    measurementRowAllocatedBytes :: !Word64,
    measurementRowPeakLiveBytes :: !Word64,
    measurementRowSemanticDigest :: !Int
  }
  deriving stock (Show)

data ReceiptObstruction
  = ReceiptHeaderInvalid !Text
  | ReceiptRowColumnCountInvalid !Int ![Text]
  | ReceiptCaseInvalid !Int !Text
  | ReceiptSampleInvalid !Int !Text
  | ReceiptElapsedInvalid !Int !Text
  | ReceiptAllocatedBytesInvalid !Int !Text
  | ReceiptPeakLiveBytesInvalid !Int !Text
  | ReceiptSemanticDigestInvalid !Int !Text
  | ReceiptCaseMissing !RewriteMeasurementCase
  | ReceiptCaseSamplesInvalid !RewriteMeasurementCase ![MeasurementSample]
  | ReceiptCaseDigestVaried !RewriteMeasurementCase ![Int]
  deriving stock (Show)

data MeasurementRegression
  = BaselineReceiptInvalid !ReceiptObstruction
  | CandidateReceiptInvalid !ReceiptObstruction
  | MeasurementSemanticDigestChanged !RewriteMeasurementCase !Int !Int
  | MeasurementAllocatedBytesIncreased !RewriteMeasurementCase !Word64 !Word64
  | MeasurementPeakLiveBytesIncreased !RewriteMeasurementCase !Word64 !Word64
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
  | MeasurementComparisonRejected !MeasurementRegression
  deriving stock (Show)

data CaseSummary = CaseSummary
  { caseSummaryCase :: !RewriteMeasurementCase,
    caseSummarySemanticDigest :: !Int,
    caseSummaryMedianAllocatedBytes :: !Word64,
    caseSummaryMedianPeakLiveBytes :: !Word64
  }

data MeasurementNode child
  = MeasurementLeaf !Int
  | MeasurementBranch ![child]
  deriving stock (Eq, Ord, Functor, Foldable, Traversable)

instance ZipMatch MeasurementNode where
  zipMatch =
    zipSameNodeShape

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
      >>= TextIO.putStrLn . renderMeasurementRow
  MeasurementCompare baselinePath candidatePath ->
    runMeasurementComparison baselinePath candidatePath >>= expectMeasurement

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
    Right (MeasurementCompare baselinePath candidatePath)
  arguments ->
    Left (MeasurementArgumentsInvalid arguments)

allMeasurementCases :: [RewriteMeasurementCase]
allMeasurementCases =
  [ ProofReachabilityContiguous4096,
    ProofReachabilitySparse1000000,
    CheckedSystemAppend1024,
    CheckedSystemOrderedProjection1024,
    NaryAntiUnificationArity512Terms16,
    GuardEncodingDepth512,
    QueryConditionCollectionDepth4096,
    NonlinearPatternBindingSubterm4096
  ]

allMeasurementSamples :: [MeasurementSample]
allMeasurementSamples =
  fmap MeasurementSample [1 .. 5]

measurementCaseToken :: RewriteMeasurementCase -> String
measurementCaseToken = \case
  ProofReachabilityContiguous4096 -> "proof-reachability-contiguous-4096"
  ProofReachabilitySparse1000000 -> "proof-reachability-sparse-1000000"
  CheckedSystemAppend1024 -> "checked-system-append-1024"
  CheckedSystemOrderedProjection1024 -> "checked-system-ordered-projection-1024"
  NaryAntiUnificationArity512Terms16 -> "nary-anti-unification-arity-512-terms-16"
  GuardEncodingDepth512 -> "guard-encoding-depth-512"
  QueryConditionCollectionDepth4096 -> "query-condition-collection-depth-4096"
  NonlinearPatternBindingSubterm4096 -> "nonlinear-pattern-binding-subterm-4096"

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
  workerRows <- traverse (uncurry (runFreshWorker executablePath)) workerSpecifications
  pure (renderMeasurementReceipt <$> sequence workerRows)
  where
    workerSpecifications =
      (,) <$> allMeasurementCases <*> allMeasurementSamples

runFreshWorker ::
  FilePath ->
  RewriteMeasurementCase ->
  MeasurementSample ->
  IO (Either MeasurementFailure MeasurementRow)
runFreshWorker executablePath measurementCase sample = do
  processResult <-
    try
      ( readProcessWithExitCode
          executablePath
          [ "worker",
            measurementCaseToken measurementCase,
            show (measurementSampleValue sample),
            "+RTS",
            "-T",
            "-RTS"
          ]
          ""
      )
  pure $ case processResult of
    Left launchFailure ->
      Left (MeasurementWorkerLaunchFailed measurementCase sample launchFailure)
    Right (ExitFailure exitCode, _stdoutText, stderrText) ->
      Left (MeasurementWorkerExited measurementCase sample (ExitFailure exitCode) stderrText)
    Right (ExitSuccess, stdoutText, _stderrText) ->
      first (MeasurementWorkerOutputInvalid measurementCase sample)
        (parseMeasurementRow 1 (Text.strip (Text.pack stdoutText)))
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
  case measurementCase of
    ProofReachabilityContiguous4096 ->
      fmap
        (first (MeasurementPreparationRejected measurementCase))
        (prepareMeasurementInput proofRegistryInputDigest (Right contiguousProofRegistry))
        >>= traverse
          ( \registry ->
              measurePreparedCase
                measurementCase
                sample
                registry
                (proofReachabilityAction (ClassId 0))
                intSetSemanticDigest
          )
        >>= pure . joinEither
    ProofReachabilitySparse1000000 ->
      fmap
        (first (MeasurementPreparationRejected measurementCase))
        (prepareMeasurementInput proofRegistryInputDigest (Right sparseProofRegistry))
        >>= traverse
          ( \registry ->
              measurePreparedCase
                measurementCase
                sample
                registry
                (proofReachabilityAction (ClassId 1_000_000))
                intSetSemanticDigest
          )
        >>= pure . joinEither
    CheckedSystemAppend1024 ->
      fmap
        (first (MeasurementPreparationRejected measurementCase))
        (prepareMeasurementInput checkedSystemAppendFixtureDigest checkedSystemAppendFixture)
        >>= traverse
          ( \(checkedSystem, appendedRewrite) ->
              measurePreparedCase
                measurementCase
                sample
                (checkedSystem, appendedRewrite)
                checkedSystemAppendAction
                checkedSystemSemanticDigest
          )
        >>= pure . joinEither
    CheckedSystemOrderedProjection1024 ->
      fmap
        (first (MeasurementPreparationRejected measurementCase))
        (prepareMeasurementInput checkedSystemSemanticDigest checkedSystemProjectionFixture)
        >>= traverse
          ( \checkedSystem ->
              measurePreparedCase
                measurementCase
                sample
                checkedSystem
                (pure . Right . checkedRewrites)
                checkedRewriteListSemanticDigest
          )
        >>= pure . joinEither
    NaryAntiUnificationArity512Terms16 ->
      fmap
        (first (MeasurementPreparationRejected measurementCase))
        (prepareMeasurementInput measurementTermsDigest (Right antiUnificationTerms))
        >>= traverse
          ( \terms ->
              measurePreparedCase
                measurementCase
                sample
                terms
                (pure . Right . antiUnifyAllTerms)
                naryAntiUnificationSemanticDigest
          )
        >>= pure . joinEither
    GuardEncodingDepth512 ->
      fmap
        (first (MeasurementPreparationRejected measurementCase))
        (prepareMeasurementInput compiledGuardInputDigest guardEncodingFixture)
        >>= traverse
          ( \compiledGuard ->
              measurePreparedCase
                measurementCase
                sample
                compiledGuard
                (pure . Right . compiledGuardCanonicalNodeWordsWith (const 0) (const 0))
                wordListSemanticDigest
          )
        >>= pure . joinEither
    QueryConditionCollectionDepth4096 ->
      fmap
        (first (MeasurementPreparationRejected measurementCase))
        (prepareMeasurementInput queryStructureDigest (Right queryConditionFixture))
        >>= traverse
          ( \query ->
              measurePreparedCase
                measurementCase
                sample
                query
                (pure . Right . patternQueryConditions)
                intListSemanticDigest
          )
        >>= pure . joinEither
    NonlinearPatternBindingSubterm4096 ->
      fmap
        (first (MeasurementPreparationRejected measurementCase))
        (prepareMeasurementInput nonlinearMatcherInputDigest (Right nonlinearMatcherFixture))
        >>= traverse
          ( \fixture ->
              measurePreparedCase
                measurementCase
                sample
                fixture
                nonlinearMatcherAction
                id
          )
        >>= pure . joinEither

joinEither :: Either obstruction (Either obstruction value) -> Either obstruction value
joinEither =
  (>>= id)

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
    (measureFreshSample (measurementSampleValue sample) input runSample semanticDigest)

measurementRowFromFreshMeasurement ::
  RewriteMeasurementCase ->
  MeasurementSample ->
  FreshMeasurement value ->
  MeasurementRow
measurementRowFromFreshMeasurement measurementCase sample measurement =
  MeasurementRow
    { measurementRowCase = measurementCase,
      measurementRowSample = sample,
      measurementRowElapsedNanoseconds = freshMeasurementElapsedNanoseconds measurement,
      measurementRowAllocatedBytes = freshMeasurementAllocatedBytes measurement,
      measurementRowPeakLiveBytes = freshMeasurementPeakLiveBytes measurement,
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
        mixInt
          (mixInt digest (classIdKey (psLhsClass proofStep)))
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

checkedSystemAppendFixture ::
  Either MeasurementCaseFailure (CheckedSystem () [], CheckedRewrite () [])
checkedSystemAppendFixture =
  (,) <$> checkedSystemProjectionFixture <*> measurementCheckedRewrite 1024

checkedSystemProjectionFixture :: Either MeasurementCaseFailure (CheckedSystem () [])
checkedSystemProjectionFixture = do
  rewrites <- traverse measurementCheckedRewrite [0 .. 1023]
  first MeasurementCheckedSystemFailed (checkedSystemFromRewrites 1024 rewrites)

measurementCheckedRewrite :: Int -> Either MeasurementCaseFailure (CheckedRewrite () [])
measurementCheckedRewrite key = do
  name <-
    first MeasurementRuleNameFailed
      (mkRuleName ("measurement.rule" <> show key))
  let rewriteRuleId = RewriteRuleId key
      patternVariable = mkPatternVar 0
      identityPattern :: Pattern []
      identityPattern = PatternVar patternVariable
      origin =
        RuleOrigin
          { roRuleId = rewriteRuleId,
            roRuleName = name
          }
  algebraicRewrite <-
    first MeasurementPatternRewriteFailed
      ( mkPatternRewrite
          (RewriteAtomic origin)
          identityPattern
          (Set.singleton patternVariable)
          identityPattern
          (logicalDecoration Nothing Nothing)
      )
  Right
    CheckedRewrite
      { checkedRewriteId = rewriteRuleId,
        checkedRewriteName = name,
        checkedRewriteAlgebra = algebraicRewrite
      }

checkedSystemAppendFixtureDigest :: (CheckedSystem () [], CheckedRewrite () []) -> Int
checkedSystemAppendFixtureDigest (checkedSystem, appendedRewrite) =
  mixInt
    (checkedSystemSemanticDigest checkedSystem)
    (checkedRewriteSemanticDigest appendedRewrite)

checkedSystemAppendAction ::
  (CheckedSystem () [], CheckedRewrite () []) ->
  IO (Either MeasurementCaseFailure (CheckedSystem () []))
checkedSystemAppendAction (checkedSystem, appendedRewrite) =
  pure
    ( first MeasurementCheckedSystemFailed
        (appendCheckedRewrite 1025 appendedRewrite checkedSystem)
    )

checkedSystemSemanticDigest :: CheckedSystem () [] -> Int
checkedSystemSemanticDigest =
  checkedRewriteListSemanticDigest . checkedRewrites

checkedRewriteListSemanticDigest :: [CheckedRewrite () []] -> Int
checkedRewriteListSemanticDigest =
  foldl' mixInt 17 . fmap checkedRewriteSemanticDigest

checkedRewriteSemanticDigest :: CheckedRewrite () [] -> Int
checkedRewriteSemanticDigest rewriteValue =
  mixInt
    (rewriteRuleIdKey (checkedRewriteId rewriteValue))
    (foldl' mixInt 23 (fmap ord (ruleNameString (checkedRewriteName rewriteValue))))

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
  foldl' mixInt 29 . fmap measurementTermDigest . NonEmpty.toList

measurementTermDigest :: Fix MeasurementNode -> Int
measurementTermDigest =
  foldFix measurementNodeDigest

measurementNodeDigest :: MeasurementNode Int -> Int
measurementNodeDigest = \case
  MeasurementLeaf key ->
    mixInt 31 key
  MeasurementBranch childDigests ->
    foldl' mixInt 37 childDigests

naryAntiUnificationSemanticDigest :: NaryLGGResult MeasurementNode (Fix MeasurementNode) -> Int
naryAntiUnificationSemanticDigest result =
  foldl'
    mixInt
    (mixInt (naryLggSharedStructure result) (measurementPatternDigest (naryLggPattern result)))
    (fmap bindingRowDigest (NonEmpty.toList (naryLggBindings result)))

measurementPatternDigest :: Pattern MeasurementNode -> Int
measurementPatternDigest = \case
  PatternVar patternVariable ->
    mixInt 41 (patternVarKey patternVariable)
  PatternNode node ->
    measurementNodeDigest (fmap measurementPatternDigest node)

bindingRowDigest :: IntMap.IntMap (Fix MeasurementNode) -> Int
bindingRowDigest =
  IntMap.foldlWithKey'
    (\digest key term -> mixInt (mixInt digest key) (measurementTermDigest term))
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
  mixInt
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
        (RewriteCondition (guardHasFactTerms (FactId 7) [guardEncodingTerm]))
    )

guardEncodingTerm :: GuardTerm []
guardEncodingTerm =
  foldl'
    guardProjectTerm
    (guardRefTerm GuardRoot)
    (replicate 512 (guardChildIndex 0))

compiledGuardInputDigest :: CompiledGuard () [] -> Int
compiledGuardInputDigest =
  length . show

wordListSemanticDigest :: [Word64] -> Int
wordListSemanticDigest =
  foldl' (\digest word -> mixInt digest (fromIntegral word)) 47

queryConditionFixture :: PatternQuery Int []
queryConditionFixture =
  foldl'
    guardedPatternQuery
    (singlePatternQuery (PatternVar (mkPatternVar 0)))
    [1 .. 4096]

queryStructureDigest :: PatternQuery Int [] -> Int
queryStructureDigest = \case
  SinglePatternQuery patternValue ->
    mixInt 53 (simplePatternDigest patternValue)
  ConjunctivePatternQuery childQueries ->
    foldl' mixInt 59 (fmap queryStructureDigest (NonEmpty.toList childQueries))
  GuardedPatternQuery nestedQuery guardValue ->
    mixInt (queryStructureDigest nestedQuery) guardValue

simplePatternDigest :: Pattern [] -> Int
simplePatternDigest = \case
  PatternVar patternVariable ->
    patternVarKey patternVariable
  PatternNode childPatterns ->
    foldl' mixInt 61 (fmap simplePatternDigest childPatterns)

intListSemanticDigest :: [Int] -> Int
intListSemanticDigest =
  foldl' mixInt 67

mixInt :: Int -> Int -> Int
mixInt accumulator value =
  accumulator * 16777619 + value

measurementReceiptHeader :: Text
measurementReceiptHeader =
  "case,sample,elapsed_ns,allocated_bytes,peak_live_bytes,semantic_digest"

renderMeasurementReceipt :: [MeasurementRow] -> Text
renderMeasurementReceipt rows =
  Text.unlines (measurementReceiptHeader : fmap renderMeasurementRow rows)

renderMeasurementRow :: MeasurementRow -> Text
renderMeasurementRow row =
  Text.intercalate
    ","
    [ Text.pack (measurementCaseToken (measurementRowCase row)),
      Text.pack (show (measurementSampleValue (measurementRowSample row))),
      Text.pack (show (measurementRowElapsedNanoseconds row)),
      Text.pack (show (measurementRowAllocatedBytes row)),
      Text.pack (show (measurementRowPeakLiveBytes row)),
      Text.pack (show (measurementRowSemanticDigest row))
    ]

parseMeasurementReceipt :: Text -> Either ReceiptObstruction [MeasurementRow]
parseMeasurementReceipt receiptText =
  case Text.lines receiptText of
    [] ->
      Left (ReceiptHeaderInvalid Text.empty)
    header : rowTexts
      | header == measurementReceiptHeader ->
          traverse (uncurry parseMeasurementRow) (zip [2 ..] rowTexts)
      | otherwise ->
          Left (ReceiptHeaderInvalid header)

parseMeasurementRow :: Int -> Text -> Either ReceiptObstruction MeasurementRow
parseMeasurementRow rowNumber rowText =
  case Text.splitOn "," rowText of
    [caseText, sampleText, elapsedText, allocatedText, peakLiveText, digestText] ->
      MeasurementRow
        <$> first (const (ReceiptCaseInvalid rowNumber caseText)) (parseMeasurementCase (Text.unpack caseText))
        <*> parseReceiptSample rowNumber sampleText
        <*> parseReceiptNumber ReceiptElapsedInvalid rowNumber elapsedText
        <*> parseReceiptNumber ReceiptAllocatedBytesInvalid rowNumber allocatedText
        <*> parseReceiptNumber ReceiptPeakLiveBytesInvalid rowNumber peakLiveText
        <*> parseReceiptNumber ReceiptSemanticDigestInvalid rowNumber digestText
    columns ->
      Left (ReceiptRowColumnCountInvalid rowNumber columns)

parseReceiptSample :: Int -> Text -> Either ReceiptObstruction MeasurementSample
parseReceiptSample rowNumber sampleText =
  first
    (const (ReceiptSampleInvalid rowNumber sampleText))
    (parseMeasurementSample (Text.unpack sampleText))

parseReceiptNumber ::
  Read number =>
  (Int -> Text -> ReceiptObstruction) ->
  Int ->
  Text ->
  Either ReceiptObstruction number
parseReceiptNumber obstruction rowNumber rawValue =
  maybe
    (Left (obstruction rowNumber rawValue))
    Right
    (readMaybe (Text.unpack rawValue))

runMeasurementComparison :: FilePath -> FilePath -> IO (Either MeasurementFailure ())
runMeasurementComparison baselinePath candidatePath = do
  baselineReceipt <- readMeasurementReceipt BaselineReceipt baselinePath
  candidateReceipt <- readMeasurementReceipt CandidateReceipt candidatePath
  pure $ do
    baselineText <- baselineReceipt
    candidateText <- candidateReceipt
    first MeasurementComparisonRejected
      (compareMeasurementReceipts baselineText candidateText)

readMeasurementReceipt :: ReceiptSide -> FilePath -> IO (Either MeasurementFailure Text)
readMeasurementReceipt receiptSide receiptPath =
  fmap
    (first (MeasurementReceiptReadFailed receiptSide receiptPath))
    (try (TextIO.readFile receiptPath))

compareMeasurementReceipts :: Text -> Text -> Either MeasurementRegression ()
compareMeasurementReceipts baselineText candidateText = do
  baselineRows <-
    first BaselineReceiptInvalid (parseMeasurementReceipt baselineText)
  candidateRows <-
    first CandidateReceiptInvalid (parseMeasurementReceipt candidateText)
  baselineSummaries <-
    first BaselineReceiptInvalid (summarizeMeasurementReceipt baselineRows)
  candidateSummaries <-
    first CandidateReceiptInvalid (summarizeMeasurementReceipt candidateRows)
  traverse_
    (uncurry compareCaseSummary)
    (zip baselineSummaries candidateSummaries)

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
    medianAllocatedBytes <-
      medianOfFive measurementCase (fmap measurementRowAllocatedBytes rows)
    medianPeakLiveBytes <-
      medianOfFive measurementCase (fmap measurementRowPeakLiveBytes rows)
    Right
      CaseSummary
        { caseSummaryCase = measurementCase,
          caseSummarySemanticDigest = semanticDigest,
          caseSummaryMedianAllocatedBytes = medianAllocatedBytes,
          caseSummaryMedianPeakLiveBytes = medianPeakLiveBytes
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
  | caseSummaryMedianAllocatedBytes candidate > caseSummaryMedianAllocatedBytes baseline =
      Left
        ( MeasurementAllocatedBytesIncreased
            measurementCase
            (caseSummaryMedianAllocatedBytes baseline)
            (caseSummaryMedianAllocatedBytes candidate)
        )
  | caseSummaryMedianPeakLiveBytes candidate > caseSummaryMedianPeakLiveBytes baseline =
      Left
        ( MeasurementPeakLiveBytesIncreased
            measurementCase
            (caseSummaryMedianPeakLiveBytes baseline)
            (caseSummaryMedianPeakLiveBytes candidate)
        )
  | otherwise =
      Right ()
  where
    measurementCase =
      caseSummaryCase baseline
