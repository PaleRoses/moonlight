-- | Structure rendering of control programs, as a fold of the canonical
-- 'Program'.
module Moonlight.Control.Interpret.Pretty
  ( renderProgram,
    renderAlgebra,
  )
where

import Moonlight.Control.Program
  ( Program,
    ProgramAlgebra (..),
    foldProgram,
  )

-- | Render a program as an indented constructor tree, one node per line. O(n).
renderProgram :: (p -> String) -> (ctx -> String) -> Program ctx p -> [String]
renderProgram renderPhase renderContext =
  foldProgram (renderAlgebra renderPhase renderContext)

-- | The fold behind 'renderProgram'.
renderAlgebra :: (p -> String) -> (ctx -> String) -> ProgramAlgebra ctx p [String]
renderAlgebra renderPhase renderContext =
  ProgramAlgebra
    { paSkip = ["skip"],
      paPhase = \phaseValue -> ["phase " <> renderPhase phaseValue],
      paSeq = node "andThen",
      paOr = node "orElse",
      paUpTo = \repeatCount body -> ("upTo " <> show repeatCount) : indent body,
      paAttempt = \body -> "attempt" : indent body,
      paScoped = \context body -> ("scoped " <> renderContext context) : indent body
    }
  where
    node label left right =
      label : indent left <> indent right
    indent =
      fmap ("  " <>)
