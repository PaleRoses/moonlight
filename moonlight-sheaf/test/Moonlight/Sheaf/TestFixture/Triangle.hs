{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.TestFixture.Triangle
  ( TriangleCell (..),
    triangleVertexBasis,
    triangleEdgeBasis,
    triangleTopBasis,
    triangleBasis,
    triangleEdgeFaces,
    triangleFaces,
    triangleFaceIncidences,
    triangleCoboundarySpec0,
    triangleCoboundarySpec1,
    triangleRestrictionIndex,
    triangleUnitStalkAlgebra,
    triangleUnitStalkDimension,
    triangleUnitCoboundaryBlock,
  )
where

import Data.Kind (Type)
import Moonlight.Homology (BoundaryIncidence)
import Moonlight.Sheaf.Cochain.Coboundary (CoboundarySpec (..))
import Moonlight.Sheaf.Kernel.Basis (basisCells)
import Moonlight.Sheaf.Section.ObjectIndex (mkObjectIndex)
import Moonlight.Sheaf.Kernel.Basis
  ( SheafBasis,
    mkSheafBasis,
  )
import Moonlight.Homology
  ( HomologicalDegree (..),
  )
import Moonlight.Sheaf.Section.Linearize (identityBoundaryIncidence)
import Moonlight.Sheaf.Section.Morphism (RestrictionKind, RestrictionParts (..), mkIncidenceRestriction)
import Moonlight.Sheaf.Section.Restriction
  ( RestrictionIndex,
    RestrictionIndexError (..),
    buildRestrictionIndex,
  )
import Moonlight.Sheaf.Section.Stalk
  ( StalkAlgebra (..),
    StalkRestrictionKernel (..),
  )

type TriangleCell :: Type
data TriangleCell
  = V0
  | V1
  | V2
  | E01
  | E02
  | E12
  | T012
  deriving stock (Eq, Ord, Show)

triangleVertexBasis :: SheafBasis TriangleCell
triangleVertexBasis =
  mkSheafBasis [V0, V1, V2]

triangleEdgeBasis :: SheafBasis TriangleCell
triangleEdgeBasis =
  mkSheafBasis [E01, E02, E12]

triangleTopBasis :: SheafBasis TriangleCell
triangleTopBasis =
  mkSheafBasis [T012]

triangleBasis :: SheafBasis TriangleCell
triangleBasis =
  mkSheafBasis [V0, V1, V2, E01, E02, E12, T012]

triangleEdgeFaces :: [(TriangleCell, TriangleCell, Int)]
triangleEdgeFaces =
  [ (E01, V1, 1),
    (E01, V0, -1),
    (E02, V2, 1),
    (E02, V0, -1),
    (E12, V2, 1),
    (E12, V1, -1)
  ]

triangleFaces :: [(TriangleCell, TriangleCell, Int)]
triangleFaces =
  [ (T012, E12, 1),
    (T012, E02, -1),
    (T012, E01, 1)
  ]

triangleFaceIncidences :: [(TriangleCell, TriangleCell, Int)]
triangleFaceIncidences =
  triangleEdgeFaces <> triangleFaces

triangleCoboundarySpec0 :: CoboundarySpec TriangleCell
triangleCoboundarySpec0 =
  CoboundarySpec
    { csDimension = (HomologicalDegree 0),
      csSourceBasis = triangleVertexBasis,
      csTargetBasis = triangleEdgeBasis
    }

triangleCoboundarySpec1 :: CoboundarySpec TriangleCell
triangleCoboundarySpec1 =
  CoboundarySpec
    { csDimension = (HomologicalDegree 1),
      csSourceBasis = triangleEdgeBasis,
      csTargetBasis = triangleTopBasis
    }

triangleRestrictionIndex ::
  Either
    (RestrictionIndexError TriangleCell)
    (RestrictionIndex TriangleCell (TriangleCell, TriangleCell, Int))
triangleRestrictionIndex = do
  preparedIncidences <- traverse prepareIncidence triangleFaceIncidences
  buildRestrictionIndex
    (mkObjectIndex (basisCells triangleBasis))
    ( \(restrictionKind, incidenceWitnessValue) ->
        RestrictionParts
          { partKind = restrictionKind,
            partSource = incidenceSource incidenceWitnessValue,
            partTarget = incidenceTarget incidenceWitnessValue,
            partWitness = incidenceWitnessValue
          }
    )
    preparedIncidences
  where
    prepareIncidence ::
      (TriangleCell, TriangleCell, Int) ->
      Either
        (RestrictionIndexError TriangleCell)
        (RestrictionKind, (TriangleCell, TriangleCell, Int))
    prepareIncidence incidence =
      case mkIncidenceRestriction (incidenceOrientation incidence) of
        Just restrictionKind ->
          Right (restrictionKind, incidence)
        Nothing ->
          Left (RestrictionZeroIncidenceCoefficient (incidenceSource incidence) (incidenceTarget incidence))

    incidenceSource :: (TriangleCell, TriangleCell, Int) -> TriangleCell
    incidenceSource (source, _, _) =
      source

    incidenceTarget :: (TriangleCell, TriangleCell, Int) -> TriangleCell
    incidenceTarget (_, target, _) =
      target

    incidenceOrientation :: (TriangleCell, TriangleCell, Int) -> Int
    incidenceOrientation (_, _, orientation) =
      orientation

triangleUnitStalkAlgebra :: StalkAlgebra (TriangleCell, TriangleCell, Int) stalk () ()
triangleUnitStalkAlgebra =
  StalkAlgebra
    { saRestrictionKernel = const StalkRestrictionIdentity,
      saMismatches = \_ _ -> [],
      saMerge = \left _ -> Right left,
      saRepair = const (Left ()),
      saNormalize = id
    }

triangleUnitStalkDimension :: stalk -> Int
triangleUnitStalkDimension =
  const 1

triangleUnitCoboundaryBlock :: stalk -> stalk -> BoundaryIncidence Int
triangleUnitCoboundaryBlock _ _ =
  identityBoundaryIncidence 1
