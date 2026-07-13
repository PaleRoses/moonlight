{-# LANGUAGE DerivingStrategies #-}

-- | The connected-component count and Betti vector of a diagnostic site's nerve.
module Moonlight.Pale.Diagnostic.Site.Homotopy
  ( NerveHomotopyProfile (..),
  )
where

import Data.Kind (Type)
import Prelude (Eq, Int, Read, Show)

type NerveHomotopyProfile :: Type
data NerveHomotopyProfile = NerveHomotopyProfile
  { nhpConnectedComponents :: Int,
    nhpBettiVector :: [Int]
  }
  deriving stock (Eq, Show, Read)
