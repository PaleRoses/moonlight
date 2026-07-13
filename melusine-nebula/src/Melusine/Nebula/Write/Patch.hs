{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.Nebula.Write.Patch
  ( SourceSplice (..),
    applySplices,
  )
where

import Data.Kind (Type)
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Melusine.Nebula.Core (NebulaError (..))
import Moonlight.Pale.Ghc.Expr (SourceRegion (..))

type SourceSplice :: Type
data SourceSplice = SourceSplice
  { ssRegion :: !SourceRegion,
    ssReplacement :: !String
  }
  deriving stock (Eq, Show)

type ResolvedSplice :: Type
data ResolvedSplice = ResolvedSplice
  { rsStart :: !Int,
    rsEnd :: !Int,
    rsReplacement :: !String
  }

applySplices :: [SourceSplice] -> String -> Either NebulaError String
applySplices splices source = do
  resolved <- sortOn rsStart <$> traverse (resolveSplice source) splices
  ensureDisjoint resolved
  pure (spliceFrom source 0 resolved)

resolveSplice :: String -> SourceSplice -> Either NebulaError ResolvedSplice
resolveSplice source splice = do
  startOffset <- regionOffset source (srStartLine region) (srStartCol region)
  endOffset <- regionOffset source (srEndLine region) (srEndCol region)
  if startOffset <= endOffset
    then Right (ResolvedSplice startOffset endOffset (ssReplacement splice))
    else Left (NebulaSpliceError ("splice region ends before it starts: " <> show region))
  where
    region = ssRegion splice

regionOffset :: String -> Int -> Int -> Either NebulaError Int
regionOffset source lineNumber columnNumber =
  maybe
    (Left (NebulaSpliceError ("splice position outside the source: line " <> show lineNumber <> ", column " <> show columnNumber)))
    Right
    boundedOffset
  where
    lineStarts = scanl (\startOffset lineText -> startOffset + length lineText + 1) 0 (lines source)
    boundedOffset = do
      lineStart <- listToMaybe (drop (lineNumber - 1) lineStarts)
      let offset = lineStart + columnNumber - 1
      if lineNumber >= 1 && columnNumber >= 1 && offset <= length source
        then Just offset
        else Nothing

ensureDisjoint :: [ResolvedSplice] -> Either NebulaError ()
ensureDisjoint resolved =
  maybe (Right ()) (Left . NebulaSpliceError) firstOverlap
  where
    firstOverlap =
      listToMaybe
        [ "overlapping splice regions at offsets " <> show (rsStart earlier) <> ".." <> show (rsEnd earlier) <> " and " <> show (rsStart later) <> ".." <> show (rsEnd later)
          | (earlier, later) <- zip resolved (drop 1 resolved),
            rsEnd earlier > rsStart later
        ]

spliceFrom :: String -> Int -> [ResolvedSplice] -> String
spliceFrom source cursor = \case
  [] ->
    drop cursor source
  splice : remaining ->
    take (rsStart splice - cursor) (drop cursor source)
      <> rsReplacement splice
      <> spliceFrom source (rsEnd splice) remaining
