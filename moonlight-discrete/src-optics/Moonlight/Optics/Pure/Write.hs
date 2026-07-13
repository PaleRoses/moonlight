module Moonlight.Optics.Pure.Write
  ( WriteOptic,
    writeOptic,
    WriteResult,
    planWrite,
    writeDelta,
    writeEffect,
  )
where

import Data.Kind (Type)
import Moonlight.Optics.Pure.Multiplicity
  ( WriteOptic,
    overWriteOptic,
    writeOptic,
  )

type WriteResult :: Type -> Type -> Type
data WriteResult s t = WriteResult s t
  deriving stock (Eq, Show)

planWrite :: WriteOptic s t a b -> (a -> b) -> s -> WriteResult s t
planWrite optic update source =
  WriteResult source (overWriteOptic optic update source)

writeDelta :: (s -> t -> d) -> WriteResult s t -> d
writeDelta encoder (WriteResult source target) =
  encoder source target

writeEffect :: (s -> t -> f output) -> WriteResult s t -> f output
writeEffect effectBuilder (WriteResult source target) =
  effectBuilder source target
