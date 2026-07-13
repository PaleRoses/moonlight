{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128 (..),
    stableDigest128,
    stableDigestWords,
  )
where

import Data.Kind
  ( Type,
  )
import Data.Word
  ( Word64,
  )
import Moonlight.Flow.Internal.Digest
  ( digestWords128,
  )

type StableDigest128 :: Type
data StableDigest128 = StableDigest128
  { sdHigh :: {-# UNPACK #-} !Word64,
    sdLow :: {-# UNPACK #-} !Word64
  }
  deriving stock (Eq, Ord, Show, Read)

stableDigestWords :: StableDigest128 -> [Word64]
stableDigestWords digestValue =
  [sdHigh digestValue, sdLow digestValue]
{-# INLINE stableDigestWords #-}

stableDigest128 :: [Word64] -> StableDigest128
stableDigest128 wordsValue =
  let (high, low) =
        digestWords128 wordsValue
   in StableDigest128 high low
{-# INLINE stableDigest128 #-}
