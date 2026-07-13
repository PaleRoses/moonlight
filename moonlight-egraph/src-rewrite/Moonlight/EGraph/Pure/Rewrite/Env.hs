module Moonlight.EGraph.Pure.Rewrite.Env
  ( EGraphRewriteEnv (..),
    emptyEGraphRewriteEnv,
    rewriteRuntimeGuardCapabilityResolver,
  )
where

import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Moonlight.Rewrite.Runtime
  ( RewriteRuntimeCapabilities,
    emptyRewriteRuntimeCapabilities,
    runtimeGuardCapabilityResolver,
  )
import Moonlight.Rewrite.System
  ( GuardCapabilityResolver,
    emptyGuardCapabilityResolver,
  )
import Moonlight.Rewrite.System (FactStore, emptyFactStore)

type EGraphRewriteEnv :: Type -> (Type -> Type) -> Type
data EGraphRewriteEnv capability f = EGraphRewriteEnv
  { ereFactStore :: !FactStore,
    ereRuntimeCapabilities :: !(RewriteRuntimeCapabilities (GuardCapabilityResolver capability) f)
  }

emptyEGraphRewriteEnv :: EGraphRewriteEnv capability f
emptyEGraphRewriteEnv =
  EGraphRewriteEnv
    { ereFactStore = emptyFactStore,
      ereRuntimeCapabilities = emptyRewriteRuntimeCapabilities
    }

rewriteRuntimeGuardCapabilityResolver ::
  RewriteRuntimeCapabilities (GuardCapabilityResolver capability) f ->
  GuardCapabilityResolver capability
rewriteRuntimeGuardCapabilityResolver =
  fromMaybe emptyGuardCapabilityResolver . runtimeGuardCapabilityResolver
{-# INLINE rewriteRuntimeGuardCapabilityResolver #-}
