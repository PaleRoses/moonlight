module Moonlight.Cosheaf.Chain
  ( module Moonlight.Cosheaf.Chain.Coefficient,
    module Moonlight.Cosheaf.Chain.Provenance,
    module Moonlight.Cosheaf.Chain.Prepared,
    CosheafNerveChainKey,
    CosheafChainBasisKey,
    CosheafNerveChain (..),
    CosheafChainCell (..),
    CosheafChainBasisTable,
    ccbtDegree,
    ccbtCells,
    ccbtIndexByKey,
    ccbtKeyByIndex,
    PreparedFiniteCosheafChain,
    pfccCosheaf,
    pfccMaxDegree,
    pfccBasisByDegree,
    pfccBoundariesByDegree,
    pfccChainComplex,
    CosheafChainFailure (..),
    prepareFiniteCosheafChainFromPreparedSupport,
    prepareFiniteCosheafChainFromSupportPlan,
    cosheafChainBasisAtDegree,
    cosheafChainCellsAtDegree,
    cosheafChainCellByBasisIndex,
    cosheafChainBasisIndexOf,
    cosheafChainBasisKeyAt,
    cosheafBoundaryIncidenceAt,
    verifyCosheafBoundaryNilpotence,
  )
where

import Data.Foldable (traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Moonlight.Cosheaf.Chain.Internal.Dense
  ( buildPreparedFiniteCosheafChainDenseFromSupportPlan,
    buildPreparedFiniteCosheafChainDenseFromPreparedSupport,
  )
import Moonlight.Cosheaf.Chain.Coefficient
import Moonlight.Cosheaf.Chain.Finite.Types
  ( CosheafChainBasisKey,
    CosheafChainBasisTable,
    CosheafChainCell (..),
    CosheafChainFailure (..),
    CosheafNerveChain (..),
    CosheafNerveChainKey,
    PreparedFiniteCosheafChain,
    cosheafChainBasisCellsInternal,
    cosheafChainBasisDegreeInternal,
    cosheafChainBasisIndexByKeyInternal,
    cosheafChainBasisKeyByIndexInternal,
    cosheafBoundaryIncidenceAtMap,
    cosheafChainBasisTableSize,
    preparedFiniteCosheafBasisByDegreeInternal,
    preparedFiniteCosheafBoundariesByDegreeInternal,
    preparedFiniteCosheafChainComplexInternal,
    preparedFiniteCosheafInternal,
    preparedFiniteCosheafMaxDegreeInternal,
  )
import Moonlight.Cosheaf.Chain.Prepared
import Moonlight.Cosheaf.Chain.Provenance
import Moonlight.Cosheaf.Finite
  ( FiniteCosheaf,
  )
import Moonlight.Cosheaf.Support
  ( CosheafSupportFailure,
    CosheafSupportPlan,
    PreparedCosheafSupport,
  )
import Moonlight.Homology
  ( BoundaryIncidence,
    BoundaryIncidenceShapeError (..),
    HomologicalDegree (..),
    FiniteChainComplex,
    boundaryEntries,
    composeBoundaryIncidence,
    sourceCardinality,
    targetCardinality,
  )
import Moonlight.Sheaf.Site.Class
  ( Site (..),
  )

ccbtDegree :: CosheafChainBasisTable site value -> HomologicalDegree
ccbtDegree =
  cosheafChainBasisDegreeInternal

ccbtCells ::
  CosheafChainBasisTable site value ->
  Vector (CosheafChainCell (SiteObject site) (SiteMorphism site) value)
ccbtCells =
  cosheafChainBasisCellsInternal

ccbtIndexByKey :: CosheafChainBasisTable site value -> Map CosheafChainBasisKey Int
ccbtIndexByKey =
  cosheafChainBasisIndexByKeyInternal

ccbtKeyByIndex :: CosheafChainBasisTable site value -> IntMap CosheafChainBasisKey
ccbtKeyByIndex =
  cosheafChainBasisKeyByIndexInternal

pfccCosheaf :: PreparedFiniteCosheafChain site value -> FiniteCosheaf site value
pfccCosheaf =
  preparedFiniteCosheafInternal

pfccMaxDegree :: PreparedFiniteCosheafChain site value -> HomologicalDegree
pfccMaxDegree =
  preparedFiniteCosheafMaxDegreeInternal

pfccBasisByDegree ::
  PreparedFiniteCosheafChain site value ->
  IntMap (CosheafChainBasisTable site value)
pfccBasisByDegree =
  preparedFiniteCosheafBasisByDegreeInternal

pfccBoundariesByDegree ::
  PreparedFiniteCosheafChain site value ->
  IntMap (BoundaryIncidence Int)
pfccBoundariesByDegree =
  preparedFiniteCosheafBoundariesByDegreeInternal

pfccChainComplex :: PreparedFiniteCosheafChain site value -> FiniteChainComplex Int
pfccChainComplex =
  preparedFiniteCosheafChainComplexInternal

prepareFiniteCosheafChainFromSupportPlan ::
  (Site site, Ord (SiteMorphism site)) =>
  CosheafSupportPlan ->
  FiniteCosheaf site value ->
  Either
    (CosheafSupportFailure (SiteObject site) (SiteMorphism site) value)
    (PreparedFiniteCosheafChain site value)
prepareFiniteCosheafChainFromSupportPlan =
  buildPreparedFiniteCosheafChainDenseFromSupportPlan
{-# INLINEABLE prepareFiniteCosheafChainFromSupportPlan #-}

prepareFiniteCosheafChainFromPreparedSupport ::
  (Site site, Ord (SiteMorphism site)) =>
  PreparedCosheafSupport site value ->
  Either
    (CosheafSupportFailure (SiteObject site) (SiteMorphism site) value)
    (PreparedFiniteCosheafChain site value)
prepareFiniteCosheafChainFromPreparedSupport =
  buildPreparedFiniteCosheafChainDenseFromPreparedSupport
{-# INLINEABLE prepareFiniteCosheafChainFromPreparedSupport #-}

cosheafChainBasisAtDegree ::
  HomologicalDegree ->
  PreparedFiniteCosheafChain site value ->
  Maybe (CosheafChainBasisTable site value)
cosheafChainBasisAtDegree (HomologicalDegree degreeValue) =
  IntMap.lookup degreeValue . pfccBasisByDegree
{-# INLINE cosheafChainBasisAtDegree #-}

cosheafChainCellsAtDegree ::
  HomologicalDegree ->
  PreparedFiniteCosheafChain site value ->
  [CosheafChainCell (SiteObject site) (SiteMorphism site) value]
cosheafChainCellsAtDegree degreeValue plan =
  maybe [] (Vector.toList . ccbtCells) (cosheafChainBasisAtDegree degreeValue plan)
{-# INLINE cosheafChainCellsAtDegree #-}

cosheafChainCellByBasisIndex ::
  HomologicalDegree ->
  Int ->
  PreparedFiniteCosheafChain site value ->
  Either
    (CosheafChainFailure (SiteObject site) (SiteMorphism site) value)
    (CosheafChainCell (SiteObject site) (SiteMorphism site) value)
cosheafChainCellByBasisIndex degreeValue basisIndexValue plan = do
  table <-
    maybe
      (Left (CosheafChainBasisTableMissing degreeValue))
      Right
      (cosheafChainBasisAtDegree degreeValue plan)
  maybe
    (Left (CosheafChainBasisIndexMissing degreeValue basisIndexValue))
    Right
    (ccbtCells table Vector.!? basisIndexValue)
{-# INLINE cosheafChainCellByBasisIndex #-}

cosheafChainBasisIndexOf ::
  HomologicalDegree ->
  CosheafChainBasisKey ->
  PreparedFiniteCosheafChain site value ->
  Maybe Int
cosheafChainBasisIndexOf degreeValue key plan = do
  table <- cosheafChainBasisAtDegree degreeValue plan
  Map.lookup key (ccbtIndexByKey table)
{-# INLINE cosheafChainBasisIndexOf #-}

cosheafChainBasisKeyAt ::
  HomologicalDegree ->
  Int ->
  PreparedFiniteCosheafChain site value ->
  Maybe CosheafChainBasisKey
cosheafChainBasisKeyAt degreeValue basisIndexValue plan = do
  table <- cosheafChainBasisAtDegree degreeValue plan
  IntMap.lookup basisIndexValue (ccbtKeyByIndex table)
{-# INLINE cosheafChainBasisKeyAt #-}

cosheafBoundaryIncidenceAt ::
  HomologicalDegree ->
  PreparedFiniteCosheafChain site value ->
  BoundaryIncidence Int
cosheafBoundaryIncidenceAt degreeValue =
  cosheafBoundaryIncidenceAtMap degreeValue . pfccBoundariesByDegree
{-# INLINE cosheafBoundaryIncidenceAt #-}

verifyCosheafBoundaryNilpotence ::
  PreparedFiniteCosheafChain site value ->
  Either (CosheafChainFailure (SiteObject site) (SiteMorphism site) value) ()
verifyCosheafBoundaryNilpotence plan =
  traverse_ (validateBoundaryShapeAt plan) degrees
    *> traverse_ (validateBoundaryCompositeAt plan) nilpotenceDegrees
  where
    HomologicalDegree maxDegreeInt =
      pfccMaxDegree plan

    degrees =
      fmap HomologicalDegree [0 .. maxDegreeInt]

    nilpotenceDegrees =
      fmap HomologicalDegree [1 .. maxDegreeInt]

validateBoundaryShapeAt ::
  PreparedFiniteCosheafChain site value ->
  HomologicalDegree ->
  Either
    (CosheafChainFailure (SiteObject site) (SiteMorphism site) value)
    ()
validateBoundaryShapeAt plan degreeValue =
  let incidence =
        cosheafBoundaryIncidenceAt degreeValue plan
      expectedSource =
        cosheafChainDegreeCardinality degreeValue plan
      expectedTarget =
        cosheafChainDegreeCardinality (previousDegree degreeValue) plan
      actualSource =
        sourceCardinality incidence
      actualTarget =
        targetCardinality incidence
   in if expectedSource == actualSource && expectedTarget == actualTarget
        then Right ()
        else
          Left $
            CosheafChainBoundaryShapeFailed $
              BoundaryIncidenceBlockShapeMismatch
                expectedSource
                expectedTarget
                actualSource
                actualTarget

validateBoundaryCompositeAt ::
  PreparedFiniteCosheafChain site value ->
  HomologicalDegree ->
  Either
    (CosheafChainFailure (SiteObject site) (SiteMorphism site) value)
    ()
validateBoundaryCompositeAt plan degreeValue = do
  composite <-
    case cosheafBoundaryCompositeAt degreeValue plan of
      Left shapeError ->
        Left (CosheafChainBoundaryShapeFailed shapeError)
      Right compositeValue ->
        Right compositeValue
  if null (boundaryEntries composite)
    then Right ()
    else Left (CosheafChainBoundaryNilpotenceFailed degreeValue composite)

cosheafChainDegreeCardinality ::
  HomologicalDegree ->
  PreparedFiniteCosheafChain site value ->
  Int
cosheafChainDegreeCardinality degreeValue =
  maybe 0 cosheafChainBasisTableSize . cosheafChainBasisAtDegree degreeValue

previousDegree :: HomologicalDegree -> HomologicalDegree
previousDegree (HomologicalDegree degreeInt) =
  HomologicalDegree (degreeInt - 1)

cosheafBoundaryCompositeAt ::
  HomologicalDegree ->
  PreparedFiniteCosheafChain site value ->
  Either BoundaryIncidenceShapeError (BoundaryIncidence Int)
cosheafBoundaryCompositeAt (HomologicalDegree degreeInt) plan =
  composeBoundaryIncidence
    (cosheafBoundaryIncidenceAt (HomologicalDegree (degreeInt - 1)) plan)
    (cosheafBoundaryIncidenceAt (HomologicalDegree degreeInt) plan)
