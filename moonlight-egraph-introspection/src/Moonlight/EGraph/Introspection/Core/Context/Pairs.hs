{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Introspection.Core.Context.Pairs
  ( rewriteContextPairStrategy,
  )
where

import Data.Set qualified as Set
import Moonlight.Sheaf.Site.Context.GeneratorCover
  ( ContextGeneratorCover (..),
    contextClosure,
  )
import Moonlight.Sheaf.Site
  ( ContextPairStrategy (..),
  )
import Moonlight.Sheaf.Site (SystemCtx)

rewriteContextPairStrategy ::
  ContextGeneratorCover system =>
  system ->
  [SystemCtx system] ->
  ContextPairStrategy (SystemCtx system)
rewriteContextPairStrategy rewriteSystem contextValues =
  let generatorContexts = contextGenerators rewriteSystem
   in if Set.fromList contextValues == Set.fromList (contextClosure rewriteSystem)
        then GeneratorSeededPairs generatorContexts
        else ExhaustivePairs
