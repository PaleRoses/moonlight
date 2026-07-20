{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE TypeApplications #-}

module Moonlight.Rewrite.Relational.Front.Internal.GuardDigest
  ( patternVar,
    patternNode,
    compiledGuardCanonicalWords,
  )
where

import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import Moonlight.Core
  ( Pattern (..),
    PatternVar,
  )
import Moonlight.Rewrite.DSL
  ( Node (..),
    NodeTag,
    RewriteSignature (..),
    nodeChildren,
  )
import Moonlight.Rewrite.System
  ( CompiledGuard,
    compiledGuardCanonicalWordsWith,
  )

patternVar ::
  (PatternVar -> var) ->
  Pattern f ->
  Maybe var
patternVar projectPatternVar patternValue =
  case patternValue of
    PatternVar patternVariable ->
      Just (projectPatternVar patternVariable)

    PatternNode _ ->
      Nothing

patternNode ::
  RewriteSignature sig =>
  Pattern (Node sig) ->
  Maybe (NodeTag sig, [Pattern (Node sig)])
patternNode patternValue =
  case patternValue of
    PatternVar _ ->
      Nothing

    PatternNode (Node sigNode) ->
      Just (nodeTag sigNode, nodeChildren sigNode)

compiledGuardCanonicalWords ::
  forall sig capability.
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  (capability -> Word64) ->
  CompiledGuard capability (Node sig) ->
  [Word64]
compiledGuardCanonicalWords capabilityDigest =
  compiledGuardCanonicalWordsWith capabilityDigest (nodeTagDigest (Proxy @sig))
