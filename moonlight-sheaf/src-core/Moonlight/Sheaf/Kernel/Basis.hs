{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Kernel.Basis
  ( SheafBasis,
    mkSheafBasis,
    mkSheafBasisByDenseCell,
    basisCells,
    basisCardinality,
    basisIndexedCells,
    basisCellIndex,
    basisCellIndexByDenseKey,
  )
where

import Data.Kind (Type)
import Moonlight.Core (DenseKey)
import Moonlight.Sheaf.Index.Dense
  ( DenseIndex,
    denseIndexCount,
    denseIndexIndexedValues,
    denseIndexKeyOf,
    denseIndexKeyOfDenseValue,
    denseIndexValues,
    mkDenseIndex,
    mkDenseIndexByDenseValue,
  )

type SheafBasis :: Type -> Type
newtype SheafBasis cell = SheafBasis
  { sheafBasisIndex :: DenseIndex Int cell
  }
  deriving stock (Eq, Show)

mkSheafBasis :: Ord cell => [cell] -> SheafBasis cell
mkSheafBasis =
  SheafBasis . mkDenseIndex

mkSheafBasisByDenseCell :: DenseKey cell => [cell] -> SheafBasis cell
mkSheafBasisByDenseCell =
  SheafBasis . mkDenseIndexByDenseValue

basisCells :: SheafBasis cell -> [cell]
basisCells =
  denseIndexValues . sheafBasisIndex

basisCardinality :: SheafBasis cell -> Int
basisCardinality =
  denseIndexCount . sheafBasisIndex

basisIndexedCells :: SheafBasis cell -> [(Int, cell)]
basisIndexedCells =
  denseIndexIndexedValues . sheafBasisIndex

basisCellIndex :: Ord cell => cell -> SheafBasis cell -> Maybe Int
basisCellIndex cell =
  denseIndexKeyOf cell . sheafBasisIndex

basisCellIndexByDenseKey :: DenseKey cell => cell -> SheafBasis cell -> Maybe Int
basisCellIndexByDenseKey cell =
  denseIndexKeyOfDenseValue cell . sheafBasisIndex
