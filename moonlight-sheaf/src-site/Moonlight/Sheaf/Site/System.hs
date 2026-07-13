{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Site.System
  ( AnalyzableSystem (..),
    LatticeAnalyzableSystem,
    ContextOrdinalSystem (..),
  )
where

import Data.Kind (Constraint, Type)
import Data.Set qualified as Set
import Moonlight.Algebra (Lattice)
import Moonlight.Sheaf.Site.Interface.Types (MorphismInterface)

type AnalyzableSystem :: Type -> Constraint
class
  ( Ord (SystemOb system),
    Ord (SystemMor system),
    Ord (SystemCtx system)
  ) =>
  AnalyzableSystem system
  where
  type SystemTag system :: Type
  type SystemOb system :: Type
  type SystemMor system :: Type
  type SystemCtx system :: Type
  type SystemMismatch system :: Type

  allContexts :: system -> [SystemCtx system]
  contextLeq :: system -> SystemCtx system -> SystemCtx system -> Bool
  systemObjectsInContext :: system -> SystemCtx system -> [SystemOb system]
  systemMorphismsInContext :: system -> SystemCtx system -> [SystemMor system]
  restrictObject :: system -> SystemCtx system -> SystemCtx system -> SystemOb system -> Maybe (SystemOb system)
  restrictMorphism :: system -> SystemCtx system -> SystemCtx system -> SystemMor system -> Maybe (SystemMor system)
  identityMorphism :: system -> SystemCtx system -> SystemOb system -> SystemMor system
  morphismSource :: system -> SystemMor system -> SystemOb system
  morphismTarget :: system -> SystemMor system -> SystemOb system
  composeMorphisms :: system -> SystemCtx system -> SystemMor system -> SystemMor system -> Either (SystemMismatch system) (SystemMor system)
  morphismInterface :: system -> SystemMor system -> MorphismInterface (SystemTag system)
  normalizeMorphism :: system -> SystemCtx system -> SystemMor system -> SystemMor system

  systemObjects :: system -> [SystemOb system]
  systemObjects systemValue =
    Set.toList . Set.fromList $
      allContexts systemValue >>= systemObjectsInContext systemValue

  systemMorphisms :: system -> [SystemMor system]
  systemMorphisms systemValue =
    Set.toList . Set.fromList $
      allContexts systemValue >>= systemMorphismsInContext systemValue

type LatticeAnalyzableSystem :: Type -> Constraint
type LatticeAnalyzableSystem system =
  ( AnalyzableSystem system,
    Lattice (SystemCtx system)
  )

type ContextOrdinalSystem :: Type -> Constraint
class AnalyzableSystem system => ContextOrdinalSystem system where
  contextOrdinal :: system -> SystemCtx system -> Int
