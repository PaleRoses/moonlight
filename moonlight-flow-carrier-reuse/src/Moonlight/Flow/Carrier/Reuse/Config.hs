{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Carrier.Reuse.Config
  ( ReuseConfig (..),
    ReuseMode (..),
    defaultReuseConfig,
  )
where

data ReuseMode
  = ExactOnly
  | ExactOrCover
  | ExactOrContainment
  | ContainmentOnly
  deriving stock (Eq, Ord, Show, Read)

data ReuseConfig = ReuseConfig
  { rcMode :: !ReuseMode,
    rcMaxContainmentCandidates :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord, Show, Read)

defaultReuseConfig :: ReuseConfig
defaultReuseConfig =
  ReuseConfig
    { rcMode = ExactOrContainment,
      rcMaxContainmentCandidates = 64
    }
{-# INLINE defaultReuseConfig #-}
