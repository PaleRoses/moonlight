module Moonlight.Derived.Pruning
  ( localSheafLaplacian
  , pruningGapOfSymmetricDenseMat
  , pruningGapAt
  , laplacianGate
  , SpectralPruningOracle
  , SpectralPruningFailure (..)
  , mkSpectralPruningOracle
  , spectralPruningGate
  , iterativeSpectralPrune
  , PreparedVerdierPruning
  , preparedVerdierPrimal
  , preparedVerdierDual
  , VerdierPreparation (..)
  , prepareVerdierPruning
  , verdierGate
  , verdierLocalClosedGate
  ) where

import Moonlight.Derived.Pure.Pruning.LaplacianGate
import Moonlight.Derived.Pure.Pruning.SpectralGate
import Moonlight.Derived.Pure.Pruning.VerdierGate
