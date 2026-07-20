{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Runtime.Execution.Shard
  ( Shard (..),
  )
where

newtype Shard = Shard
  { shardKey :: Int
  }
  deriving stock (Eq, Ord, Show, Read)
