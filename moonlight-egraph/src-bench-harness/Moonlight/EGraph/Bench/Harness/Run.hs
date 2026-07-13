-- | Benchmark execution, artifact emission, and typed failure handling.
module Moonlight.EGraph.Bench.Harness.Run
  ( BenchFailure,
    ScaleBench (..),
    runScaleBench,
    paperArtifactDir,
    abortBench,
    requireRight,
    requireMaybe,
  ) where

import Data.Foldable (traverse_)
import Data.List qualified as List
import Moonlight.EGraph.Bench.Harness.Report
  ( Card,
    Table,
    renderCard,
    renderCsv,
    renderCsvRow,
  )
import System.Directory qualified as Directory
import System.Exit (exitFailure)
import System.FilePath ((</>), isDrive, takeDirectory, takeFileName)
import System.IO (BufferMode (LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)

type BenchFailure = String

data ScaleBench point row summary = ScaleBench
  { benchName :: !String,
    benchReproCommand :: !String,
    benchPoints :: ![point],
    benchAnnounce :: point -> String,
    benchRunPoint :: point -> IO (Either BenchFailure [row]),
    benchCsv :: !(Table row),
    benchCard :: !(Card row summary)
  }

runScaleBench :: ScaleBench point row summary -> IO ()
runScaleBench bench = do
  hSetBuffering stdout LineBuffering
  artifactDir <- paperArtifactDir (benchName bench)
  Directory.createDirectoryIfMissing True artifactDir
  rows <- fmap concat (traverse runPoint (benchPoints bench))
  writeFile
    (artifactDir </> (benchName bench <> ".csv"))
    (renderCsv (benchCsv bench) rows)
  writeFile
    (artifactDir </> "CARD.md")
    (renderCard (benchReproCommand bench) (benchCard bench) rows)
  putStrLn "DONE"
  where
    runPoint point = do
      putStrLn (benchAnnounce bench point)
      benchRunPoint bench point >>= \case
        Left failure ->
          abortBench (benchName bench <> ": " <> failure)
        Right rows ->
          rows <$ traverse_ (putStrLn . renderCsvRow (benchCsv bench)) rows

paperArtifactDir :: String -> IO FilePath
paperArtifactDir name =
  fmap
    (\cwd -> maybe cwd takeDirectory (compilerAncestor cwd) </> "artifacts" </> "paper" </> name)
    Directory.getCurrentDirectory

compilerAncestor :: FilePath -> Maybe FilePath
compilerAncestor =
  List.find ((== "compiler") . takeFileName)
    . takeWhile (not . isDrive)
    . iterate takeDirectory

abortBench :: BenchFailure -> IO a
abortBench failure = do
  hPutStrLn stderr failure
  exitFailure

requireRight :: Show errorValue => String -> Either errorValue value -> IO value
requireRight label =
  either
    (\errorValue -> abortBench (label <> " failed: " <> show errorValue))
    pure

requireMaybe :: String -> Maybe value -> IO value
requireMaybe label =
  maybe (abortBench (label <> " missing")) pure
