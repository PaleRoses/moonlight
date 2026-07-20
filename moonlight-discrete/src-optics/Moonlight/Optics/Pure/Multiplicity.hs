module Moonlight.Optics.Pure.Multiplicity
  ( ReadOptic,
    WriteOptic,
    readOptic,
    writeOptic,
    viewRead,
    previewRead,
    toListOfRead,
    overWriteOptic,
    setWriteOptic,
  )
where

import Data.Kind (Type)
import Optics.Core

type ReadOptic :: Type -> Type -> Type
newtype ReadOptic s a = ReadOptic (Getter s a)

type WriteOptic :: Type -> Type -> Type -> Type -> Type
newtype WriteOptic s t a b = WriteOptic (Setter s t a b)

readOptic :: Is opticKind A_Getter => Optic' opticKind NoIx source focus -> ReadOptic source focus
readOptic optic = ReadOptic (castOptic optic)

writeOptic :: Is opticKind A_Setter => Optic opticKind NoIx source target focus updated -> WriteOptic source target focus updated
writeOptic optic = WriteOptic (castOptic optic)

viewRead :: ReadOptic s a -> s -> a
viewRead (ReadOptic optic) = view optic

previewRead :: ReadOptic s a -> s -> Maybe a
previewRead (ReadOptic optic) = preview optic

toListOfRead :: ReadOptic s a -> s -> [a]
toListOfRead (ReadOptic optic) = toListOf optic

overWriteOptic :: WriteOptic s t a b -> (a -> b) -> s -> t
overWriteOptic (WriteOptic optic) = over optic

setWriteOptic :: WriteOptic s t a b -> b -> s -> t
setWriteOptic (WriteOptic optic) = set optic
