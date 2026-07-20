module Moonlight.EGraph.Saturation.Bench.Egglog
  ( EgglogBinary (..),
    EgglogEngineBenchBinary (..),
    EgglogPrepared (..),
    discoverEgglogBinary,
    discoverEgglogEngineBenchBinary,
    prepareEgglogProgram,
    renderEgglogProgram,
    runPreparedEgglog,
    runEgglogEngineBench,
    cleanupEgglogProgram,
  )
where

import System.Directory (findExecutable, getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitSuccess, ExitFailure))
import System.Process (readProcessWithExitCode)

newtype EgglogBinary = EgglogBinary
  { unEgglogBinary :: FilePath
  }
  deriving stock (Eq, Show)

newtype EgglogEngineBenchBinary = EgglogEngineBenchBinary
  { unEgglogEngineBenchBinary :: FilePath
  }
  deriving stock (Eq, Show)

data EgglogPrepared = EgglogPrepared
  { epBinary :: EgglogBinary,
    epFilePath :: FilePath
  }
  deriving stock (Eq, Show)

discoverEgglogBinary :: IO (Maybe EgglogBinary)
discoverEgglogBinary = do
  configured <- lookupEnv "EGGLOG_BIN"
  case configured of
    Just path -> pure (Just (EgglogBinary path))
    Nothing -> fmap EgglogBinary <$> findExecutable "egglog"

discoverEgglogEngineBenchBinary :: IO (Maybe EgglogEngineBenchBinary)
discoverEgglogEngineBenchBinary =
  fmap EgglogEngineBenchBinary <$> lookupEnv "EGGLOG_ENGINE_BENCH_BIN"

prepareEgglogProgram :: EgglogBinary -> Int -> IO EgglogPrepared
prepareEgglogProgram egglogBinary termCount = do
  tempDir <- getTemporaryDirectory
  let tempFile = tempDir <> "/moonlight-bench-egglog-" <> show termCount <> ".egg"
  writeFile tempFile (renderEgglogProgram termCount)
  pure (EgglogPrepared egglogBinary tempFile)

cleanupEgglogProgram :: EgglogPrepared -> IO ()
cleanupEgglogProgram prepared =
  removeFile (epFilePath prepared)

renderEgglogProgram :: Int -> String
renderEgglogProgram termCount =
  unlines
    [ "(datatype Arith",
      "  (Num i64)",
      "  (Var i64)",
      "  (Add Arith Arith)",
      "  (Mul Arith Arith)",
      "  (Neg Arith))",
      "",
      egglogRules,
      "",
      egglogTerms termCount,
      "",
      "(run 100)"
    ]

egglogRules :: String
egglogRules =
  unlines
    [ "(rewrite (Add ?x (Num 0)) ?x)",
      "(rewrite (Add (Num 0) ?x) ?x)",
      "(rewrite (Mul ?x (Num 0)) (Num 0))",
      "(rewrite (Mul (Num 0) ?x) (Num 0))",
      "(rewrite (Mul ?x (Num 1)) ?x)",
      "(rewrite (Mul (Num 1) ?x) ?x)",
      "(rewrite (Neg (Neg ?x)) ?x)",
      "(rewrite (Neg (Num 0)) (Num 0))",
      "(rewrite (Add ?x (Neg ?x)) (Num 0))",
      "(rewrite (Add ?x ?x) (Mul (Num 2) ?x))"
    ]

egglogTerms :: Int -> String
egglogTerms termCount =
  unlines
    (fmap
      (\termIndex ->
        let i = show termIndex
         in unlines
              [ "(let $t_az_" <> i <> " (Add (Num " <> i <> ") (Num 0)))",
                "(let $t_mo_" <> i <> " (Mul (Num " <> i <> ") (Num 1)))",
                "(let $t_mz_" <> i <> " (Mul (Num " <> i <> ") (Num 0)))",
                "(let $t_nn_" <> i <> " (Neg (Neg (Num " <> i <> "))))",
                "(let $t_an_" <> i <> " (Add (Num " <> i <> ") (Neg (Num " <> i <> "))))"
              ]
      )
      [0 .. termCount - 1]
    )

runPreparedEgglog :: EgglogPrepared -> IO (Either String ())
runPreparedEgglog prepared = do
  (exitCode, _stdoutText, stderrText) <-
    readProcessWithExitCode
      (unEgglogBinary (epBinary prepared))
      [epFilePath prepared]
      ""
  pure $ case exitCode of
    ExitSuccess -> Right ()
    ExitFailure status ->
      Left ("egglog failure: exit " <> show status <> ", stderr: " <> stderrText)

runEgglogEngineBench :: EgglogEngineBenchBinary -> Int -> IO (Either String ())
runEgglogEngineBench benchBinary termCount = do
  (exitCode, _stdoutText, stderrText) <-
    readProcessWithExitCode
      (unEgglogEngineBenchBinary benchBinary)
      [show termCount]
      ""
  pure $ case exitCode of
    ExitSuccess -> Right ()
    ExitFailure status ->
      Left ("egglog engine bench failure: exit " <> show status <> ", stderr: " <> stderrText)
