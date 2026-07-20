{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Saturation.Core.Run
  ( SaturationRun (..),
  )
where

import Data.Kind (Type)
import Moonlight.Saturation.Core.Termination
  ( SaturationTermination,
  )

type SaturationRun :: Type -> Type
data SaturationRun state = SaturationRun
  { srTermination :: !SaturationTermination,
    srFinalState :: !state
  }
  deriving stock (Eq, Ord, Show, Read)
