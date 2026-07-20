module Moonlight.Optics.Pure.Delta
  ( DeltaIR (..),
    singletonDelta,
    DeltaOptic (..),
    mkDeltaOptic,
    deltaEvent,
    emitDelta,
    emitDeltaWith,
  )
where

import Data.Kind (Type)
import qualified Data.Sequence as Seq
import Moonlight.Optics.Pure.Write (WriteOptic, planWrite, writeDelta)

type DeltaIR :: Type -> Type
newtype DeltaIR d = DeltaIR {unDeltaIR :: Seq.Seq d}
  deriving stock (Eq, Show)

instance Semigroup (DeltaIR d) where
  DeltaIR left <> DeltaIR right = DeltaIR (left <> right)

instance Monoid (DeltaIR d) where
  mempty = DeltaIR Seq.empty

singletonDelta :: d -> DeltaIR d
singletonDelta deltaValue = DeltaIR (Seq.singleton deltaValue)

type DeltaOptic :: Type -> Type -> Type -> Type -> Type -> Type
data DeltaOptic d s t a b = DeltaOptic
  { deltaPath :: WriteOptic s t a b,
    encodeDelta :: s -> t -> d
  }

mkDeltaOptic :: (s -> t -> d) -> WriteOptic s t a b -> DeltaOptic d s t a b
mkDeltaOptic encoder path =
  DeltaOptic
    { deltaPath = path,
      encodeDelta = encoder
    }

deltaEvent :: (s -> t -> d) -> WriteOptic s t a b -> (a -> b) -> s -> d
deltaEvent encoder optic update source =
  writeDelta encoder (planWrite optic update source)

emitDelta :: DeltaOptic d s t a b -> (a -> b) -> s -> DeltaIR d
emitDelta deltaOptic update source =
  singletonDelta (deltaEvent (encodeDelta deltaOptic) (deltaPath deltaOptic) update source)

emitDeltaWith :: (s -> t -> d) -> WriteOptic s t a b -> (a -> b) -> s -> DeltaIR d
emitDeltaWith encoder optic update source =
  singletonDelta (deltaEvent encoder optic update source)
