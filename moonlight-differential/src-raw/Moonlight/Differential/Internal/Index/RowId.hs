{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Trusted row identifiers.  Public construction validates non-negativity;
-- internal canonical representations may recover the witness without paying
-- the same check for every element.
module Moonlight.Differential.Internal.Index.RowId
  ( RowId (..),
    RowIdCursor (..),
  )
where

import Data.Kind
  ( Type,
  )

type RowId :: Type
newtype RowId = RowId Int
  deriving stock (Eq, Ord, Show)

type RowIdCursor :: Type
data RowIdCursor
  = RowIdAvailable !RowId
  | RowIdsExhausted
  deriving stock (Eq, Ord, Show)
