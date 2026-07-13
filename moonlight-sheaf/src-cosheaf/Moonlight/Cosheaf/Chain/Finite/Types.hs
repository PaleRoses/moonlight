{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Cosheaf.Chain.Finite.Types
  ( CosheafNerveChainKey (..),
    CosheafChainBasisKey (..),
    CosheafNerveChain (..),
    CosheafChainCell (..),
    CosheafChainBasisTable (..),
    PreparedFiniteCosheafChain (..),
    CosheafChainFailure (..),
    cosheafChainBasisTableSize,
    cosheafBoundaryIncidenceAtMap,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Moonlight.Cosheaf.Cosection
  ( CosectionRepresentative,
  )
import Moonlight.Cosheaf.Finite
  ( CostalkKey,
    FiniteCosheaf,
  )
import Moonlight.Cosheaf.SiteIndex
  ( CosheafMorphismKey,
  )
import Moonlight.Homology
  ( BoundaryIncidence,
    BoundaryIncidenceShapeError,
    FiniteChainComplex,
    HomologicalDegree (..),
    HomologyFailure,
    emptyBoundaryIncidenceOf,
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectKey,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism,
    Site (..),
  )
import Numeric.Natural
  ( Natural,
  )

-- | Internal dense identity for a normalized nerve chain.
type CosheafNerveChainKey :: Type
data CosheafNerveChainKey = CosheafNerveChainKey
  { cnckSourceObjectKey :: !ObjectKey,
    cnckMorphismKeys :: ![CosheafMorphismKey]
  }
  deriving stock (Eq, Ord, Show)

-- | Internal dense identity for a cosheaf chain basis vector.
type CosheafChainBasisKey :: Type
data CosheafChainBasisKey = CosheafChainBasisKey
  { ccbkNerveChainKey :: !CosheafNerveChainKey,
    ccbkSourceCostalkKey :: !CostalkKey
  }
  deriving stock (Eq, Ord, Show)

-- | Public projection of a normalized nerve chain.
type CosheafNerveChain :: Type -> Type -> Type
data CosheafNerveChain obj mor = CosheafNerveChain
  { cosheafNerveChainSourceObject :: !obj,
    cosheafNerveChainMorphisms :: ![CheckedMorphism obj mor]
  }
  deriving stock (Eq, Ord, Show)

-- | Public projection of a cosheaf chain basis vector.
type CosheafChainCell :: Type -> Type -> Type -> Type
data CosheafChainCell obj mor value = CosheafChainCell
  { cosheafChainCellKey :: !CosheafChainBasisKey,
    cosheafChainCellNerveChain :: !(CosheafNerveChain obj mor),
    cosheafChainCellRepresentative :: !(CosectionRepresentative obj value),
    cosheafChainCellCostalkKey :: !CostalkKey
  }
  deriving stock (Eq, Ord, Show)

-- | Dense basis table for one cosheaf chain degree. The finite chain complex
-- only sees integer coordinates; every cosheaf-facing witness must lift those
-- coordinates through this table.
type CosheafChainBasisTable :: Type -> Type -> Type
data CosheafChainBasisTable site value = CosheafChainBasisTable
  { ccbtDegree :: !HomologicalDegree,
    ccbtCells :: !(Vector (CosheafChainCell (SiteObject site) (SiteMorphism site) value)),
    ccbtIndexByKey :: !(Map CosheafChainBasisKey Int),
    ccbtKeyByIndex :: !(IntMap CosheafChainBasisKey)
  }

-- | Prepared bounded cosheaf chain complex. The basis tables are the authority;
-- homology is a derived view over the finite chain complex assembled from them.
type PreparedFiniteCosheafChain :: Type -> Type -> Type
data PreparedFiniteCosheafChain site value = PreparedFiniteCosheafChain
  { pfccCosheaf :: !(FiniteCosheaf site value),
    pfccMaxDegree :: !HomologicalDegree,
    pfccBasisByDegree :: !(IntMap (CosheafChainBasisTable site value)),
    pfccBoundariesByDegree :: !(IntMap (BoundaryIncidence Int)),
    pfccChainComplex :: !(FiniteChainComplex Int)
  }

type CosheafChainFailure :: Type -> Type -> Type -> Type
data CosheafChainFailure obj mor value
  = CosheafChainDegreeTooLarge !Natural
  | CosheafChainObjectKeyMissing !obj
  | CosheafChainCostalkMissing !ObjectKey
  | CosheafChainCostalkValueMissing !ObjectKey !CostalkKey
  | CosheafChainCorestrictionMalformed !(CheckedMorphism obj mor) !CostalkKey
  | CosheafChainCompositeUndefined !(CheckedMorphism obj mor) !(CheckedMorphism obj mor)
  | CosheafChainCompositeCorestrictionMissing !(CheckedMorphism obj mor)
  | CosheafChainCompositeLookupMissing !(CheckedMorphism obj mor) !(CheckedMorphism obj mor)
  | CosheafChainBoundaryTargetMissing !HomologicalDegree !CosheafChainBasisKey
  | CosheafChainBoundaryShapeFailed !BoundaryIncidenceShapeError
  | CosheafChainComplexFailed !HomologyFailure
  | CosheafChainBoundaryNilpotenceFailed !HomologicalDegree !(BoundaryIncidence Int)
  | CosheafChainDuplicateBasisKey !HomologicalDegree !CosheafChainBasisKey
  | CosheafChainBasisTableMissing !HomologicalDegree
  | CosheafChainBasisIndexMissing !HomologicalDegree !Int
  | CosheafChainBasisKeyMissing !HomologicalDegree !CosheafChainBasisKey
  deriving stock (Eq, Show)

cosheafChainBasisTableSize :: CosheafChainBasisTable site value -> Int
cosheafChainBasisTableSize =
  Vector.length . ccbtCells
{-# INLINE cosheafChainBasisTableSize #-}

cosheafBoundaryIncidenceAtMap ::
  HomologicalDegree ->
  IntMap (BoundaryIncidence Int) ->
  BoundaryIncidence Int
cosheafBoundaryIncidenceAtMap (HomologicalDegree degreeValue) =
  IntMap.findWithDefault (emptyBoundaryIncidenceOf 0 0) degreeValue
{-# INLINE cosheafBoundaryIncidenceAtMap #-}
