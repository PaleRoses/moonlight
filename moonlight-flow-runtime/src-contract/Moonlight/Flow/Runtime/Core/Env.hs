{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Runtime.Core.Env
  ( RuntimeEnvelope (..),
  )
where

import Data.Kind
  ( Type,
  )

type RuntimeEnvelope :: Type -> Type -> Type
data RuntimeEnvelope state env = RelDiffRuntime
  { rdrState :: !state,
    rdrEnv :: !env
  }
