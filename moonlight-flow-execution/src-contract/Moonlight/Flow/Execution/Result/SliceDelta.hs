module Moonlight.Flow.Execution.Result.SliceDelta
  ( ResultSliceDelta (..),
    resultSliceDeltaFromSlices,
    lookupResultSlice,
  )
where

import Data.Kind (Type)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)

type ResultSliceDelta :: Type -> Type
data ResultSliceDelta tuple = ResultSliceDelta
  { rsdDirtyResults :: !IntSet,
    rsdSlices :: !(IntMap [tuple])
  }

resultSliceDeltaFromSlices :: IntMap [tuple] -> IntSet -> ResultSliceDelta tuple
resultSliceDeltaFromSlices slices dirtyResults =
  ResultSliceDelta
    { rsdDirtyResults = dirtyResults,
      rsdSlices =
        IntMap.restrictKeys
          slices
          dirtyResults
    }

lookupResultSlice :: ResultSliceDelta tuple -> Int -> [tuple]
lookupResultSlice delta resultKey =
  IntMap.findWithDefault [] resultKey (rsdSlices delta)
