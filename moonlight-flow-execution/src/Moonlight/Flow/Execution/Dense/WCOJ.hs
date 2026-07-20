{-# LANGUAGE RankNTypes #-}

module Moonlight.Flow.Execution.Dense.WCOJ
  ( DenseLeafWitness (..),
    denseJoinRows,
    denseJoinDeltaRows,
    denseJoinSupportIds,
    foldProjectDenseWCOJ,
    foldProjectDenseWCOJKeys,
    foldProjectDenseWCOJWitnessesWithTelemetry,
    foldProjectDenseWCOJDeltaWitnessesWithTelemetry,
    foldProjectDenseWCOJWitnesses,
    foldProjectDenseWCOJSelectedWitnesses,
    joinProjectDenseWCOJ,
  )
where

import Moonlight.Flow.Execution.Dense.WCOJ.Project
  ( denseJoinDeltaRows,
    denseJoinRows,
    denseJoinSupportIds,
    foldProjectDenseWCOJ,
    foldProjectDenseWCOJDeltaWitnessesWithTelemetry,
    foldProjectDenseWCOJKeys,
    foldProjectDenseWCOJSelectedWitnesses,
    foldProjectDenseWCOJWitnesses,
    foldProjectDenseWCOJWitnessesWithTelemetry,
    joinProjectDenseWCOJ,
  )
import Moonlight.Flow.Execution.Dense.WCOJ.Witness
  ( DenseLeafWitness (..),
  )
