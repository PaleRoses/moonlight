{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Model.Event
  ( SupportDelta (..),
    LocalRelationalSlice (..),
    LocalRelationalTime (..),
    LocalRelationalAddr (..),
    LocalRelationalOrigin (..),
    LocalRelationalEvent (..),
  )
where

import Data.IntSet (IntSet)
import Data.Kind (Type)
import Moonlight.Core
  ( AtomId,
    QueryId,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta,
  )
import Moonlight.Flow.Model.Id
import Moonlight.Flow.Model.Phase
  ( RelationalPhase,
  )
import Moonlight.Flow.Model.Scope
  ( RelationalScope,
  )

type SupportDelta :: Type
newtype SupportDelta = SupportDelta
  { unSupportDelta :: IntSet
  }
  deriving stock (Eq, Ord, Show)

type LocalRelationalSlice :: Type
data LocalRelationalSlice
  = LocalRootSlice
  | LocalAtomSlice !AtomId
  | LocalBagSlice !BagId
  | LocalSeparatorSlice !BagId !BagId
  deriving stock (Eq, Ord, Show)

type LocalRelationalTime :: Type
newtype LocalRelationalTime = LocalRelationalTime
  { lrtPhase :: RelationalPhase
  }
  deriving stock (Eq, Ord, Show)

type LocalRelationalAddr :: Type
data LocalRelationalAddr = LocalRelationalAddr
  { lraQueryId :: !QueryId,
    lraSlice :: !LocalRelationalSlice
  }
  deriving stock (Eq, Ord, Show)

type LocalRelationalOrigin :: Type
data LocalRelationalOrigin
  = LocalOriginJoinRepair
  | LocalOriginBatchOracle
  deriving stock (Eq, Ord, Show)

type LocalRelationalEvent :: Type
data LocalRelationalEvent = LocalRelationalEvent
  { lreAddr :: !LocalRelationalAddr,
    lreTime :: !LocalRelationalTime,
    lreSupport :: !SupportDelta,
    lreOrigin :: !LocalRelationalOrigin,
    lreScope :: !RelationalScope,
    lreRows :: !RowDelta
  }
  deriving stock (Eq, Show)
