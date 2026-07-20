module Melusine.Nebula.Write.Diff
  ( renderModuleDiff,
    renderModuleCandidateDiff,
  )
where

import Data.List (sortOn)
import Melusine.Nebula.Write.Patch (SourceSplice (..))
import Melusine.Nebula.Write.Seal (SealOutcome (..))
import Melusine.Nebula.Write.Back (AppendedDefinition (..), ModulePatch (..))
import Moonlight.Pale.Ghc.Expr (SourceRegion (..))

renderModuleDiff :: FilePath -> String -> ModulePatch -> SealOutcome -> [String]
renderModuleDiff path source modulePatch = \case
  Sealed _ ->
    renderModuleCandidateDiff path source modulePatch
  SealRefused _ sealFailure ->
    ["diff path=" <> path <> " seal=refused error=" <> show sealFailure]
  SealEmpty ->
    []

renderModuleCandidateDiff :: FilePath -> String -> ModulePatch -> [String]
renderModuleCandidateDiff path source modulePatch =
  concatMap (spliceHunk path sourceLines) sortedSplices <> appendHunk path (mpAppendedDefinitions modulePatch)
  where
    sourceLines = lines source
    sortedSplices = sortOn ((\region -> (srStartLine region, srStartCol region)) . ssRegion) (mpSplices modulePatch)

spliceHunk :: FilePath -> [String] -> SourceSplice -> [String]
spliceHunk path sourceLines splice =
  header : removedLines <> addedLines
  where
    region = ssRegion splice
    lastLine = displayEndLine region
    header = "@@ " <> path <> ":" <> show (srStartLine region) <> "-" <> show lastLine
    removedLines =
      fmap ("-" <>) (take (lastLine - srStartLine region + 1) (drop (srStartLine region - 1) sourceLines))
    addedLines = fmap ("+" <>) (lines (ssReplacement splice))

displayEndLine :: SourceRegion -> Int
displayEndLine region
  | srEndCol region == 1 && srEndLine region > srStartLine region = srEndLine region - 1
  | otherwise = srEndLine region

appendHunk :: FilePath -> [AppendedDefinition] -> [String]
appendHunk path = \case
  [] ->
    []
  definitions ->
    ("@@ " <> path <> ":append")
      : concatMap (fmap ("+" <>) . lines . adSource) definitions
