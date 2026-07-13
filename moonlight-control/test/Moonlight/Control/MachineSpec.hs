module Moonlight.Control.MachineSpec
  ( tests,
  )
where

import Data.Functor.Identity (Identity (..), runIdentity)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty (..))

import Moonlight.Control.Class
  ( Control (..),
    choices,
    sequenceAll,
  )
import Moonlight.Control.Machine
  ( Disposition (..),
    Execution (..),
    Progress (..),
    Verdict (..),
    continueVerdict,
    executionContinues,
    executionProgressed,
    executionTerminal,
    interpret,
    stopVerdict,
    terminalVerdict,
  )
import Moonlight.Control.Program
  ( normalize,
  )
import Moonlight.Control.Program.Internal
  ( Program (..),
  )
import Moonlight.Control.Trace
  ( ChoiceBranchIndex (..),
    Trace (..),
    TryOutcome (..),
    phaseSummaries,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "Machine execution semantics"
    [ testCase "attempt rolls back stop NoProgress state" testAttemptRollsBackStopNoProgress,
      testCase "attempt rolls back continue NoProgress state" testAttemptRollsBackContinueNoProgress,
      testCase "attempt commits progressed state" testAttemptCommitsProgressed,
      testCase "attempt commits terminal state" testAttemptCommitsTerminal,
      testCase "attempt does not catch typed failure" testAttemptDoesNotCatchTypedFailure,
      testCase "TryTrace TryApplied on commit" testTryTraceApplied,
      testCase "TryTrace TrySkipped on rollback" testTryTraceSkipped,
      testCase "choice: rejected branch trace and chosen index" testChoiceRejectedTrace,
      testCase "choice: skip branch appears as rejected trace" testChoiceSkipBranchRejectedTrace,
      testCase "choice: first branch progresses is chosen at index 0" testChoiceFirstBranchChosenAtIndex0,
      testCase "choice: second branch chosen when first rejected" testChoiceSecondBranchChosenAtIndex1,
      testCase "upTo n exhausted downgrades Continue to Stop" testRepeatContinueDowngradedToStop,
      testCase "upTo budget exhaustion short-circuits remaining iterations" testRepeatExhaustsCount,
      testCase "sequence Terminal short-circuits remaining segments" testSequenceTerminalShortCircuits,
      testCase "repeat Terminal short-circuits remaining iterations" testRepeatTerminalShortCircuits,
      testCase "phaseSummaries collects rejected choice branch summaries" testPhaseSummariesCollectsRejected,
      testCase "sequence of two phases produces SequenceTrace" testSequenceTraceShape,
      testCase "normalize invariant: returned program is in normal form" testNormalizeInvariant,
      testCase "returned program reflects phase rewrites" testSelfRewritingRoundTrip,
      testCase "scoped outer<>inner context composition reaches phase runner" testScopedContextComposition,
      testCase "nested scoped contexts compose left-to-right" testNestedScopedContextsCompose,
      testCase "terminal verdict commits state and skips remainder in sequence" testTerminalCommitsStateThroughSequence,
      testCase "deep attempt nesting uses explicit frames (stack safety)" testDeepAttemptFrames,
      QC.testProperty "try rolls back Stop-NoProgress state" prop_tryRollsBackStopNoProgress,
      QC.testProperty "try rolls back Continue-NoProgress state" prop_tryRollsBackContinueNoProgress,
      QC.testProperty "try preserves progressed state" prop_tryPreservesProgressed,
      QC.testProperty "try preserves terminal state" prop_tryPreservesTerminal
    ]

runSinglePhase ::
  Verdict ->
  () ->
  () ->
  Integer ->
  Identity (Either String ((), Execution Integer String ()))
runSinglePhase verdict () () state =
  pure
    ( Right
        ( (),
          Execution
            { seState = state + 1,
              seLatestReport = Nothing,
              seTrace = PhaseTrace (),
              seVerdict = verdict
            }
        )
    )

runFailingPhase ::
  String ->
  () ->
  () ->
  Integer ->
  Identity (Either String ((), Execution Integer String ()))
runFailingPhase err () () _state =
  pure (Left err)

interpretPure ::
  (() -> () -> Integer -> Identity (Either String ((), Execution Integer String ()))) ->
  Program () () ->
  Integer ->
  Either String (Program () (), Execution Integer String ())
interpretPure runner prog st =
  runIdentity (interpret runner prog st)

testAttemptRollsBackStopNoProgress :: Assertion
testAttemptRollsBackStopNoProgress = do
  let prog :: Program () ()
      prog = attempt (phase ())
  case interpretPure (runSinglePhase (stopVerdict NoProgress)) prog 0 of
    Left err -> assertFailure err
    Right (_p, ex) -> do
      seState ex @?= 0
      seLatestReport ex @?= Nothing
      executionProgressed ex @?= False
      executionContinues ex @?= False
      case seTrace ex of
        TryTrace TrySkipped _ -> pure ()
        other -> assertFailure ("expected TryTrace TrySkipped, got: " <> show other)

testAttemptRollsBackContinueNoProgress :: Assertion
testAttemptRollsBackContinueNoProgress = do
  let prog :: Program () ()
      prog = attempt (phase ())
  case interpretPure (runSinglePhase (continueVerdict NoProgress)) prog 0 of
    Left err -> assertFailure err
    Right (_p, ex) -> do
      seState ex @?= 0
      seLatestReport ex @?= Nothing
      executionProgressed ex @?= False
      case seTrace ex of
        TryTrace TrySkipped _ -> pure ()
        other -> assertFailure ("expected TryTrace TrySkipped, got: " <> show other)

testAttemptCommitsProgressed :: Assertion
testAttemptCommitsProgressed = do
  let prog :: Program () ()
      prog = attempt (phase ())
  case interpretPure (runSinglePhase (stopVerdict Progressed)) prog 10 of
    Left err -> assertFailure err
    Right (_p, ex) -> do
      seState ex @?= 11
      executionProgressed ex @?= True
      case seTrace ex of
        TryTrace TryApplied _ -> pure ()
        other -> assertFailure ("expected TryTrace TryApplied, got: " <> show other)

testAttemptCommitsTerminal :: Assertion
testAttemptCommitsTerminal = do
  let prog :: Program () ()
      prog = attempt (phase ())
  case interpretPure (runSinglePhase (terminalVerdict NoProgress)) prog 5 of
    Left err -> assertFailure err
    Right (_p, ex) -> do
      seState ex @?= 6
      executionTerminal ex @?= True
      case seTrace ex of
        TryTrace TryApplied _ -> pure ()
        other -> assertFailure ("expected TryTrace TryApplied, got: " <> show other)

testAttemptDoesNotCatchTypedFailure :: Assertion
testAttemptDoesNotCatchTypedFailure = do
  let prog :: Program () ()
      prog = attempt (phase ())
  case interpretPure (runFailingPhase "typed-error") prog 0 of
    Left "typed-error" -> pure ()
    Left other -> assertFailure ("unexpected error: " <> other)
    Right _ -> assertFailure "expected typed failure to propagate through attempt"

testTryTraceApplied :: Assertion
testTryTraceApplied = do
  let prog :: Program () ()
      prog = attempt (phase ())
  case interpretPure (runSinglePhase (continueVerdict Progressed)) prog 0 of
    Left err -> assertFailure err
    Right (_p, ex) ->
      case seTrace ex of
        TryTrace TryApplied (PhaseTrace ()) -> pure ()
        other -> assertFailure ("expected TryTrace TryApplied (PhaseTrace ()), got: " <> show other)

testTryTraceSkipped :: Assertion
testTryTraceSkipped = do
  let prog :: Program () ()
      prog = attempt (phase ())
  case interpretPure (runSinglePhase (stopVerdict NoProgress)) prog 0 of
    Left err -> assertFailure err
    Right (_p, ex) ->
      case seTrace ex of
        TryTrace TrySkipped (PhaseTrace ()) -> pure ()
        other -> assertFailure ("expected TryTrace TrySkipped (PhaseTrace ()), got: " <> show other)

testChoiceRejectedTrace :: Assertion
testChoiceRejectedTrace = do
  let prog :: Program () ()
      prog = choices (phase () :| [phase ()])
      runner () () state =
        pure
          ( Right
              ( (),
                Execution
                  { seState = state,
                    seLatestReport = Nothing,
                    seTrace = PhaseTrace (),
                    seVerdict = stopVerdict NoProgress
                  }
              )
          )
  case runIdentity (interpret runner prog (0 :: Integer)) of
    Left err -> assertFailure err
    Right (_p, ex) ->
      case seTrace ex of
        ChoiceTrace
          { ctBranchIndex = ChoiceBranchIndex 1,
            ctRejected = [PhaseTrace ()],
            ctChosen = PhaseTrace ()
          } ->
            pure ()
        other ->
          assertFailure ("unexpected trace shape: " <> show other)

testChoiceSkipBranchRejectedTrace :: Assertion
testChoiceSkipBranchRejectedTrace = do
  let prog :: Program () ()
      prog = orElse skip (phase ())
      runner () () state =
        pure
          ( Right
              ( (),
                Execution
                  { seState = state + 1,
                    seLatestReport = Nothing,
                    seTrace = PhaseTrace (),
                    seVerdict = stopVerdict Progressed
                  }
              )
          )
  case runIdentity (interpret runner prog (0 :: Integer)) of
    Left err -> assertFailure err
    Right (_p, ex) -> do
      executionProgressed ex @?= True
      case seTrace ex of
        ChoiceTrace
          { ctBranchIndex = ChoiceBranchIndex 1,
            ctRejected = [SkipTrace],
            ctChosen = PhaseTrace ()
          } ->
            pure ()
        other ->
          assertFailure ("unexpected trace: " <> show other)

testChoiceFirstBranchChosenAtIndex0 :: Assertion
testChoiceFirstBranchChosenAtIndex0 = do
  let prog :: Program () ()
      prog = orElse (phase ()) (phase ())
      runner () () state =
        pure
          ( Right
              ( (),
                Execution
                  { seState = state + 1,
                    seLatestReport = Nothing,
                    seTrace = PhaseTrace (),
                    seVerdict = stopVerdict Progressed
                  }
              )
          )
  case runIdentity (interpret runner prog (0 :: Integer)) of
    Left err -> assertFailure err
    Right (_p, ex) ->
      case seTrace ex of
        ChoiceTrace {ctBranchIndex = ChoiceBranchIndex 0, ctRejected = []} ->
          pure ()
        other ->
          assertFailure ("expected index 0 with no rejected traces, got: " <> show other)

testChoiceSecondBranchChosenAtIndex1 :: Assertion
testChoiceSecondBranchChosenAtIndex1 = do
  let prog :: Program () ()
      prog = orElse (phase ()) (phase ())
      runner () () state =
        let v =
              if state == 0
                then stopVerdict NoProgress
                else stopVerdict Progressed
         in pure
              ( Right
                  ( (),
                    Execution
                      { seState = state + 1,
                        seLatestReport = Nothing,
                        seTrace = PhaseTrace (),
                        seVerdict = v
                      }
                  )
              )
  case runIdentity (interpret runner prog (0 :: Integer)) of
    Left err -> assertFailure err
    Right (_p, ex) ->
      case seTrace ex of
        ChoiceTrace {ctBranchIndex = ChoiceBranchIndex 1, ctRejected = [PhaseTrace ()]} ->
          pure ()
        other ->
          assertFailure ("expected index 1 with one rejected trace, got: " <> show other)

testRepeatContinueDowngradedToStop :: Assertion
testRepeatContinueDowngradedToStop = do
  let prog :: Program () ()
      prog = upTo 2 (phase ())
      runner () () state =
        pure
          ( Right
              ( (),
                Execution
                  { seState = state + 1,
                    seLatestReport = Nothing,
                    seTrace = PhaseTrace (),
                    seVerdict = continueVerdict Progressed
                  }
              )
          )
  case runIdentity (interpret runner prog (0 :: Integer)) of
    Left err -> assertFailure err
    Right (_p, ex) -> do
      seState ex @?= 2
      verdictDisposition (seVerdict ex) @?= Stop

testRepeatExhaustsCount :: Assertion
testRepeatExhaustsCount = do
  let prog :: Program () ()
      prog = upTo 3 (phase ())
  callsRef <- newIORef (0 :: Int)
  result <-
    interpret
      ( \() () state -> do
          modifyIORef' callsRef (+ 1)
          pure
            ( Right
                ( (),
                  Execution
                    { seState = state + 1,
                      seLatestReport = Nothing,
                      seTrace = PhaseTrace (),
                      seVerdict = continueVerdict Progressed
                    }
                )
            )
      )
      prog
      (0 :: Integer)
  calls <- readIORef callsRef
  calls @?= 3
  case result of
    Left err -> assertFailure err
    Right (_p, ex) -> seState ex @?= 3

testSequenceTerminalShortCircuits :: Assertion
testSequenceTerminalShortCircuits = do
  let prog :: Program () String
      prog = sequenceAll [phase "a", phase "b", phase "c"]
      runner () p state =
        pure
          ( Right
              ( p,
                Execution
                  { seState = state <> [p],
                    seLatestReport = Just p,
                    seTrace = PhaseTrace (),
                    seVerdict =
                      if p == "a"
                        then terminalVerdict Progressed
                        else stopVerdict Progressed
                  }
              )
          )
  case runIdentity (interpret runner prog ([] :: [String])) of
    Left err -> assertFailure err
    Right (_p, ex) -> do
      seState ex @?= ["a"]
      seLatestReport ex @?= Just "a"
      executionTerminal ex @?= True

testRepeatTerminalShortCircuits :: Assertion
testRepeatTerminalShortCircuits = do
  let prog :: Program () ()
      prog = upTo 5 (phase ())
      runner () () state =
        pure
          ( Right
              ( (),
                Execution
                  { seState = state + 1,
                    seLatestReport = Nothing,
                    seTrace = PhaseTrace (),
                    seVerdict =
                      if state == 1
                        then terminalVerdict Progressed
                        else continueVerdict Progressed
                  }
              )
          )
  case runIdentity (interpret runner prog (0 :: Integer)) of
    Left err -> assertFailure err
    Right (_p, ex) -> do
      seState ex @?= 2
      executionTerminal ex @?= True

testPhaseSummariesCollectsRejected :: Assertion
testPhaseSummariesCollectsRejected =
  phaseSummaries
    ChoiceTrace
      { ctBranchIndex = ChoiceBranchIndex 1,
        ctRejected = [PhaseTrace "left"],
        ctChosen = PhaseTrace "right"
      }
    @?= ["left", "right"]

testSequenceTraceShape :: Assertion
testSequenceTraceShape = do
  let prog :: Program () ()
      prog = andThen (phase ()) (phase ())
      runner () () state =
        pure
          ( Right
              ( (),
                Execution
                  { seState = state + 1,
                    seLatestReport = Nothing,
                    seTrace = PhaseTrace (),
                    seVerdict = stopVerdict Progressed
                  }
              )
          )
  case runIdentity (interpret runner prog (0 :: Integer)) of
    Left err -> assertFailure err
    Right (_p, ex) ->
      case seTrace ex of
        SequenceTrace _ -> pure ()
        other -> assertFailure ("expected SequenceTrace, got: " <> show other)

testNormalizeInvariant :: Assertion
testNormalizeInvariant = do
  let prog :: Program () ()
      prog = sequenceAll [skip, phase (), skip, phase (), skip]
      runner () () state =
        pure
          ( Right
              ( (),
                Execution
                  { seState = state + 1,
                    seLatestReport = Nothing,
                    seTrace = PhaseTrace (),
                    seVerdict = stopVerdict Progressed
                  }
              )
          )
  case runIdentity (interpret runner prog (0 :: Integer)) of
    Left err -> assertFailure err
    Right (updatedProg, _ex) ->
      normalize updatedProg @?= updatedProg

testSelfRewritingRoundTrip :: Assertion
testSelfRewritingRoundTrip = do
  let prog :: Program () String
      prog = sequenceAll [phase "a", phase "b"]
      runner () p state =
        let newPhase = p <> "'"
         in pure
              ( Right
                  ( newPhase,
                    Execution
                      { seState = state <> [p],
                        seLatestReport = Just p,
                        seTrace = PhaseTrace (),
                        seVerdict = stopVerdict Progressed
                      }
                  )
              )
  case runIdentity (interpret runner prog ([] :: [String])) of
    Left err -> assertFailure err
    Right (updatedProg, _ex) ->
      case updatedProg of
        Seq (Phase "a'") (Phase "b'") -> pure ()
        other -> assertFailure ("expected rewritten phases in Seq, got: " <> show other)

testScopedContextComposition :: Assertion
testScopedContextComposition = do
  let prog :: Program String String
      prog = scoped "outer" (scoped "inner" (phase "p"))
  observedContexts <- newIORef ([] :: [String])
  result <-
    interpret
      ( \ctx p state -> do
          modifyIORef' observedContexts (ctx :)
          pure
            ( Right
                ( p,
                  Execution
                    { seState = state,
                      seLatestReport = Nothing,
                      seTrace = PhaseTrace (),
                      seVerdict = stopVerdict Progressed
                    }
                )
            )
      )
      prog
      (0 :: Integer)
  case result of
    Left err -> assertFailure err
    Right _ -> do
      ctxs <- readIORef observedContexts
      ctxs @?= ["outerinner"]

testNestedScopedContextsCompose :: Assertion
testNestedScopedContextsCompose = do
  let prog :: Program String String
      prog = scoped "a" (andThen (scoped "b" (phase "p1")) (scoped "c" (phase "p2")))
  observedContexts <- newIORef ([] :: [String])
  result <-
    interpret
      ( \ctx p state -> do
          modifyIORef' observedContexts (<> [ctx])
          pure
            ( Right
                ( p,
                  Execution
                    { seState = state,
                      seLatestReport = Nothing,
                      seTrace = PhaseTrace (),
                      seVerdict = stopVerdict Progressed
                    }
                )
            )
      )
      prog
      (0 :: Integer)
  case result of
    Left err -> assertFailure err
    Right _ -> do
      ctxs <- readIORef observedContexts
      ctxs @?= ["ab", "ac"]

testTerminalCommitsStateThroughSequence :: Assertion
testTerminalCommitsStateThroughSequence = do
  let prog :: Program () String
      prog = sequenceAll [phase "stop", phase "never"]
      runner () p state =
        pure
          ( Right
              ( p,
                Execution
                  { seState = state <> [p],
                    seLatestReport = Just p,
                    seTrace = PhaseTrace (),
                    seVerdict = terminalVerdict NoProgress
                  }
              )
          )
  case runIdentity (interpret runner prog ([] :: [String])) of
    Left err -> assertFailure err
    Right (_p, ex) -> do
      seState ex @?= ["stop"]
      executionTerminal ex @?= True

testDeepAttemptFrames :: Assertion
testDeepAttemptFrames = do
  let prog :: Program () ()
      prog = foldr (const attempt) (phase ()) [1 :: Int .. 20000]
      runner () () state =
        pure
          ( Right
              ( (),
                Execution
                  { seState = state + 1,
                    seLatestReport = Nothing,
                    seTrace = PhaseTrace (),
                    seVerdict = stopVerdict Progressed
                  }
              )
          )
  case runIdentity (interpret runner prog (0 :: Integer)) of
    Left err -> assertFailure err
    Right (_p, ex) -> do
      seState ex @?= 1
      executionProgressed ex @?= True

prop_tryRollsBackStopNoProgress :: Integer -> QC.Property
prop_tryRollsBackStopNoProgress initialState =
  case runIdentity
    ( interpret
        (runSinglePhase (stopVerdict NoProgress))
        (attempt (phase ()))
        initialState
    ) of
    Left err -> QC.counterexample err False
    Right (_p, ex) ->
      QC.conjoin
        [ seState ex QC.=== initialState,
          seLatestReport ex QC.=== Nothing,
          executionProgressed ex QC.=== False,
          executionContinues ex QC.=== False,
          executionTerminal ex QC.=== False,
          case seTrace ex of
            TryTrace TrySkipped _ -> QC.property True
            other -> QC.counterexample ("unexpected trace: " <> show other) False
        ]

prop_tryRollsBackContinueNoProgress :: Integer -> QC.Property
prop_tryRollsBackContinueNoProgress initialState =
  case runIdentity
    ( interpret
        (runSinglePhase (continueVerdict NoProgress))
        (attempt (phase ()))
        initialState
    ) of
    Left err -> QC.counterexample err False
    Right (_p, ex) ->
      QC.conjoin
        [ seState ex QC.=== initialState,
          seLatestReport ex QC.=== Nothing,
          executionProgressed ex QC.=== False,
          case seTrace ex of
            TryTrace TrySkipped _ -> QC.property True
            other -> QC.counterexample ("unexpected trace: " <> show other) False
        ]

prop_tryPreservesProgressed :: Integer -> QC.Property
prop_tryPreservesProgressed initialState =
  case runIdentity
    ( interpret
        (runSinglePhase (stopVerdict Progressed))
        (attempt (phase ()))
        initialState
    ) of
    Left err -> QC.counterexample err False
    Right (_p, ex) ->
      QC.conjoin
        [ seState ex QC.=== initialState + 1,
          executionProgressed ex QC.=== True,
          case seTrace ex of
            TryTrace TryApplied _ -> QC.property True
            other -> QC.counterexample ("unexpected trace: " <> show other) False
        ]

prop_tryPreservesTerminal :: Integer -> QC.Property
prop_tryPreservesTerminal initialState =
  case runIdentity
    ( interpret
        (runSinglePhase (terminalVerdict Progressed))
        (attempt (phase ()))
        initialState
    ) of
    Left err -> QC.counterexample err False
    Right (_p, ex) ->
      QC.conjoin
        [ seState ex QC.=== initialState + 1,
          executionTerminal ex QC.=== True,
          case seTrace ex of
            TryTrace TryApplied _ -> QC.property True
            other -> QC.counterexample ("unexpected trace: " <> show other) False
        ]
