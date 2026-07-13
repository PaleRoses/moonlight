{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Pure.Query.RootFilter
  ( RootClassFilter (..),
    canonicalRootKeys,
    rootClassAllowed,
  )
where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Moonlight.EGraph.Pure.Rebuild.Index (canonicalizeClassKeys)
import Moonlight.EGraph.Pure.Types
  ( ClassId,
    EGraph,
    canonicalizeClassId,
    classIdKey,
  )

type RootClassFilter :: Type
data RootClassFilter
  = AllRootClasses
  | RestrictedRootClasses !IntSet

canonicalRootKeys :: EGraph f a -> IntSet -> IntSet
canonicalRootKeys graph =
  canonicalizeClassKeys (canonicalizeClassId graph)
{-# INLINE canonicalRootKeys #-}

rootClassAllowed :: RootClassFilter -> EGraph f a -> ClassId -> Bool
rootClassAllowed rootClassFilter graph rootClass =
  case rootClassFilter of
    AllRootClasses ->
      True
    RestrictedRootClasses rootKeys ->
      IntSet.member (classIdKey rootClass) (canonicalRootKeys graph rootKeys)
{-# INLINE rootClassAllowed #-}
