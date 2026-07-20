{-# LANGUAGE DerivingStrategies #-}

-- | Coboundary construction errors and nilpotence evidence over the diagnostic site.
module Moonlight.Pale.Diagnostic.Site.Cohomology
  ( CoboundaryConstructionError (..),
    CoboundaryNilpotenceEvidence (..),
    evidenceNilpotent,
  )
where

import Data.Kind (Type)
import Moonlight.Pale.Diagnostic.Site.Boundary (BoundaryIncidenceShapeError)
import Prelude (Bool (False, True), Eq, Int, Read, Show, String)

type CoboundaryConstructionError :: Type
data CoboundaryConstructionError
  = CoboundaryBoundaryShapeError BoundaryIncidenceShapeError
  | CoboundaryMiddleBasisCardinalityMismatch Int Int
  | CoboundaryMiddleBasisCellMismatch Int
  | CoboundaryOperatorBuildError String
  deriving stock (Eq, Show, Read)

type CoboundaryNilpotenceEvidence :: Type
data CoboundaryNilpotenceEvidence
  = SingleContextNilpotent
  | SingleContextNonNilpotent
  | MultiContextNilpotent
  | MultiContextNonNilpotent
  | CoboundaryConstructionFailed CoboundaryConstructionError
  deriving stock (Eq, Show, Read)

evidenceNilpotent :: CoboundaryNilpotenceEvidence -> Bool
evidenceNilpotent evidenceValue =
  case evidenceValue of
    SingleContextNilpotent -> True
    SingleContextNonNilpotent -> False
    MultiContextNilpotent -> True
    MultiContextNonNilpotent -> False
    CoboundaryConstructionFailed _ -> False
