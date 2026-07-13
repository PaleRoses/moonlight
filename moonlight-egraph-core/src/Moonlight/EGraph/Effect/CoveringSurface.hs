module Moonlight.EGraph.Effect.CoveringSurface
  ( SurfaceKind (..),
    surfaceKindDigest,
  )
where

import Data.Kind (Type)
import Data.Word (Word64)

type SurfaceKind :: Type
data SurfaceKind
  = Matching
  | Extraction
  | Provenance
  | Context
  | Analysis
  deriving stock (Eq, Ord, Show)

surfaceKindDigest :: SurfaceKind -> Word64
surfaceKindDigest surfaceKind =
  case surfaceKind of
    Matching -> 0x01
    Extraction -> 0x02
    Provenance -> 0x03
    Context -> 0x04
    Analysis -> 0x05
