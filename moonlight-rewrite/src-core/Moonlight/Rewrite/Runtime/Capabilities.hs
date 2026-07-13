-- | Runtime capability record for optional interpretation algebras.
-- It owns only the presence boundary for guard capability resolution and binder
-- substitution; consumers decide when an absent capability is fatal.
module Moonlight.Rewrite.Runtime.Capabilities
  ( RewriteRuntimeCapabilities (..),
    emptyRewriteRuntimeCapabilities,
    withRuntimeGuardCapabilityResolver,
    withRuntimeBinderSubstAlgebra,
    runtimeGuardCapabilityResolver,
    runtimeBinderSubstAlgebra,
  )
where

import Data.Kind (Type)
import Moonlight.Rewrite.Runtime.PostMatch (BinderSubstAlgebra)

type RewriteRuntimeCapabilities :: Type -> (Type -> Type) -> Type
data RewriteRuntimeCapabilities guardCapability f = RewriteRuntimeCapabilities
  { rrcGuardCapabilityResolver :: !(Maybe guardCapability),
    rrcBinderSubstAlgebra :: !(Maybe (BinderSubstAlgebra f))
  }

emptyRewriteRuntimeCapabilities :: RewriteRuntimeCapabilities guardCapability f
emptyRewriteRuntimeCapabilities =
  RewriteRuntimeCapabilities
    { rrcGuardCapabilityResolver = Nothing,
      rrcBinderSubstAlgebra = Nothing
    }

withRuntimeGuardCapabilityResolver ::
  guardCapability ->
  RewriteRuntimeCapabilities guardCapability f ->
  RewriteRuntimeCapabilities guardCapability f
withRuntimeGuardCapabilityResolver guardCapability runtimeCapabilities =
  runtimeCapabilities
    { rrcGuardCapabilityResolver = Just guardCapability
    }

withRuntimeBinderSubstAlgebra ::
  BinderSubstAlgebra f ->
  RewriteRuntimeCapabilities guardCapability f ->
  RewriteRuntimeCapabilities guardCapability f
withRuntimeBinderSubstAlgebra binderScopeAlgebra runtimeCapabilities =
  runtimeCapabilities
    { rrcBinderSubstAlgebra = Just binderScopeAlgebra
    }

runtimeGuardCapabilityResolver ::
  RewriteRuntimeCapabilities guardCapability f ->
  Maybe guardCapability
runtimeGuardCapabilityResolver =
  rrcGuardCapabilityResolver

runtimeBinderSubstAlgebra ::
  RewriteRuntimeCapabilities guardCapability f ->
  Maybe (BinderSubstAlgebra f)
runtimeBinderSubstAlgebra =
  rrcBinderSubstAlgebra
