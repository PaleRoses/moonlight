{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Moonlight.Sheaf.TestFixture.Mini
  ( MiniCell (..),
    MiniStalk (..),
    bumpMiniStalk,
    miniBasis,
    miniCell0Basis,
    miniCell1Basis,
    miniGhostBasis,
    withMiniModelAndSection,
    MiniRestriction (..),
    miniStalkAlgebra,
    miniRestrictionIndex,
    withMiniSheafModel,
    miniCoboundaryCache,
  )
where

import Data.Kind (Type)
import Moonlight.Sheaf.Operator.GradedComplex
import Moonlight.Sheaf.Kernel.Basis
  ( SheafBasis,
    mkSheafBasis,
  )
import Moonlight.Sheaf.Section.Model
  ( SheafModel,
    withEmptySheafModel,
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( SheafModelVersion (..),
    mkObjectIndex,
  )
import Moonlight.Sheaf.Section.Store.State
import Moonlight.Sheaf.Section.Store.Types
import Moonlight.Sheaf.Section.Restriction
  ( RestrictionIndex,
    emptyRestrictionIndex,
  )
import Moonlight.Sheaf.Section.Stalk (StalkAlgebra (..), StalkRestrictionKernel (..))

type MiniCell :: Type
data MiniCell
  = Cell0
  | Cell1
  | Ghost
  deriving stock (Eq, Ord, Show)

type MiniStalk :: Type
newtype MiniStalk = MiniStalk Double
  deriving stock (Show)
  deriving newtype (Eq)

newtype MiniRestriction = MiniRestriction
  { runMiniRestriction :: MiniStalk -> MiniStalk
  }

miniStalkAlgebra :: StalkAlgebra MiniRestriction MiniStalk () ()
miniStalkAlgebra =
  StalkAlgebra
    { saRestrictionKernel = \(MiniRestriction restriction) -> StalkRestrictionMap restriction,
      saMismatches =
        \left right ->
          ([() | not (miniStalkApproxEq left right)]),
      saMerge =
        \(MiniStalk left) (MiniStalk right) ->
          Right (MiniStalk (left + right)),
      saRepair = const (Left ()),
      saNormalize = id
    }

miniStalkApproxEq :: MiniStalk -> MiniStalk -> Bool
miniStalkApproxEq (MiniStalk left) (MiniStalk right) =
  abs (left - right) < 1.0e-10

bumpMiniStalk :: MiniStalk -> MiniStalk
bumpMiniStalk (MiniStalk value) =
  MiniStalk (value + 1.0)

miniBasis :: SheafBasis MiniCell
miniBasis =
  mkSheafBasis [Cell0, Cell1]

miniCell0Basis :: SheafBasis MiniCell
miniCell0Basis =
  mkSheafBasis [Cell0]

miniCell1Basis :: SheafBasis MiniCell
miniCell1Basis =
  mkSheafBasis [Cell1]

miniGhostBasis :: SheafBasis MiniCell
miniGhostBasis =
  mkSheafBasis [Ghost, Cell0, Cell1]

withMiniModelAndSection ::
  ( forall owner.
    SheafModel owner MiniCell MiniRestriction ->
    TotalSectionStore owner MiniCell MiniStalk ->
    result
  ) ->
  Either String result
withMiniModelAndSection useFixture =
  withMiniSheafModel $ \model ->
    useFixture model (emptyTotalSectionStoreWith model (const (MiniStalk 0.0)))

miniRestrictionIndex :: RestrictionIndex MiniCell MiniRestriction
miniRestrictionIndex =
  emptyRestrictionIndex

withMiniSheafModel ::
  (forall owner. SheafModel owner MiniCell MiniRestriction -> result) ->
  Either String result
withMiniSheafModel useModel =
  Right (withEmptySheafModel (SheafModelVersion 0) (mkObjectIndex [Cell0, Cell1]) useModel)

miniCoboundaryCache :: GradedComplex MiniCell Int
miniCoboundaryCache =
  emptyGradedComplex DegreeIncreasing
