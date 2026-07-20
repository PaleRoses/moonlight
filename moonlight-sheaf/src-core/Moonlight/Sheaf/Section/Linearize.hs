module Moonlight.Sheaf.Section.Linearize
  ( StalkLinearization (..),
    constantRestrictionLinearization,
    identityBoundaryIncidence,
  )
where

import Data.Kind (Type)
import Moonlight.Homology
  ( BoundaryIncidence,
    emptyBoundaryIncidence,
    identityBoundaryIncidenceOf,
  )

type StalkLinearization :: Type -> Type -> Type
data StalkLinearization stalk r = StalkLinearization
  { slStalkDimension :: stalk -> Int,
    slRestrictionIncidence :: stalk -> stalk -> BoundaryIncidence r
  }

constantRestrictionLinearization :: Num r => Int -> StalkLinearization stalk r
constantRestrictionLinearization dimensionValue =
  let normalizedDimension = max 0 dimensionValue
   in StalkLinearization
        { slStalkDimension = const normalizedDimension,
          slRestrictionIncidence = \_ _ -> identityBoundaryIncidence normalizedDimension
        }

identityBoundaryIncidence :: Num r => Int -> BoundaryIncidence r
identityBoundaryIncidence dimensionValue =
  if dimensionValue <= 0
    then emptyBoundaryIncidence
    else identityBoundaryIncidenceOf (fromIntegral dimensionValue)
