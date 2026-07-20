{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Cosheaf.Chain.Internal.Dense
  ( buildPreparedFiniteCosheafChainDenseFromSupportPlan,
    buildPreparedFiniteCosheafChainDenseFromPreparedSupport,
  )
where

import Data.Bifunctor (first)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Moonlight.Core
  ( encodeDenseKey,
  )
import Moonlight.Core
  ( duplicatesOrd,
  )
import Moonlight.Cosheaf.Chain.Finite.Types
  ( CosheafChainBasisKey (..),
    CosheafChainBasisTable (..),
    CosheafChainCell (..),
    CosheafChainFailure (..),
    CosheafNerveChain (..),
    CosheafNerveChainKey (..),
    PreparedFiniteCosheafChain (..),
    cosheafBoundaryIncidenceAtMap,
    cosheafChainBasisTableSize,
  )
import Moonlight.Cosheaf.Cosection
  ( CosectionRepresentative (..),
  )
import Moonlight.Cosheaf.Finite
  ( CompiledCorestriction,
    CostalkKey (..),
    FiniteCostalk,
    FiniteCosheaf,
    ccMorphism,
    ccMorphismKey,
    ccSourceObjectKey,
    ccSourceToTarget,
    ccTargetObjectKey,
    fcCorestrictions,
    fcSite,
    fcSiteIndex,
    fcostalkObject,
    finiteCostalkAtObjectKey,
    finiteCostalkValueAt,
  )
import Moonlight.Cosheaf.Support
  ( CosheafSupportFailure (..),
    CosheafSupportPlan,
    PreparedCosheafSupport,
    cspChainCells,
    cspMaxDegree,
    cspNerveRows,
    cspObjects,
    pcsCorestrictions,
    pcsCosheaf,
    pcsCostalkKeysByObject,
    pcsPlan,
    prepareCosheafSupport,
    scContains,
    supportCarrierItems,
  )
import Moonlight.Cosheaf.SiteIndex
  ( CosheafMorphismKey,
    cosheafMorphismKeyOf,
    cosheafSiteObjectIndex,
  )
import Moonlight.Homology
  ( BoundaryEntry,
    BoundaryIncidence,
    HomologicalDegree (..),
    emptyBoundaryIncidenceOf,
    mkBoundaryEntry,
    mkBoundaryIncidenceFromOrderedEntries,
    mkFiniteChainComplexChecked,
  )
import Moonlight.Sheaf.Index.Dense (denseIndexValueAt)
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectKey (..),
  )
import Moonlight.Sheaf.Site.Class
  ( Site (..),
    isIdentityMorphism,
  )

type DenseCorestrictionFrontier :: Type -> Type -> Type
type DenseCorestrictionFrontier obj mor =
  IntMap (Vector (CompiledCorestriction obj mor))

type DenseNerveRow :: Type -> Type -> Type
data DenseNerveRow obj mor = DenseNerveRow
  { dnrKey :: !CosheafNerveChainKey,
    dnrSourceObject :: !obj,
    dnrEndObjectKey :: !ObjectKey,
    dnrCorestrictions :: !(Vector (CompiledCorestriction obj mor))
  }

type DenseChainCell :: Type -> Type -> Type
data DenseChainCell obj mor = DenseChainCell
  { dccKey :: !CosheafChainBasisKey,
    dccRow :: !(DenseNerveRow obj mor),
    dccSourceCostalkKey :: !CostalkKey
  }

type BoundaryTerm :: Type
data BoundaryTerm = BoundaryTerm
  { btCoefficient :: !Int,
    btTargetKey :: !CosheafChainBasisKey
  }

type CorestrictionComposite :: Type -> Type -> Type
data CorestrictionComposite obj mor
  = CorestrictionCompositeIdentity
  | CorestrictionCompositeCorestriction !(CompiledCorestriction obj mor)

type CorestrictionCompositionLookup :: Type -> Type -> Type
type CorestrictionCompositionLookup obj mor =
  Map
    (CosheafMorphismKey, CosheafMorphismKey)
    (CorestrictionComposite obj mor)

type InnerFaceContext :: Type -> Type -> Type
data InnerFaceContext obj mor = InnerFaceContext
  { ifcFaceIndex :: !Int,
    ifcBeforeCorestrictions :: ![CompiledCorestriction obj mor],
    ifcInnerCorestriction :: !(CompiledCorestriction obj mor),
    ifcOuterCorestriction :: !(CompiledCorestriction obj mor),
    ifcAfterCorestrictions :: ![CompiledCorestriction obj mor]
  }

buildPreparedFiniteCosheafChainDenseFromSupportPlan ::
  (Site site, Ord (SiteMorphism site)) =>
  CosheafSupportPlan ->
  FiniteCosheaf site value ->
  Either
    (CosheafSupportFailure (SiteObject site) (SiteMorphism site) value)
    (PreparedFiniteCosheafChain site value)
buildPreparedFiniteCosheafChainDenseFromSupportPlan supportPlan cosheaf = do
  preparedSupport <- prepareCosheafSupport cosheaf supportPlan
  buildPreparedFiniteCosheafChainDenseFromPreparedSupport preparedSupport

buildPreparedFiniteCosheafChainDenseFromPreparedSupport ::
  (Site site, Ord (SiteMorphism site)) =>
  PreparedCosheafSupport site value ->
  Either
    (CosheafSupportFailure (SiteObject site) (SiteMorphism site) value)
    (PreparedFiniteCosheafChain site value)
buildPreparedFiniteCosheafChainDenseFromPreparedSupport preparedSupport = do
  let retainedCorestrictionsBySource =
        nonIdentityCorestrictionsBySourceVectorFromPreparedSupport preparedSupport
  rowsByDegree <-
    first CosheafSupportChainFailed $
      denseRowsByDegreeFromSupportPlan supportPlan cosheaf retainedCorestrictionsBySource maxDegreeInt
  cellsByDegree <-
    first CosheafSupportChainFailed $
      IntMap.traverseWithKey
        (\_degreeValue -> denseCellsAtDegreeFromPreparedSupport preparedSupport)
        rowsByDegree
  basisByDegree <-
    first CosheafSupportChainFailed $
      IntMap.traverseWithKey
        (\degreeValue -> mkDenseCosheafChainBasisTable cosheaf (HomologicalDegree degreeValue))
        cellsByDegree
  compositionLookup <-
    first CosheafSupportChainFailed $
      compiledCorestrictionCompositionLookupForRows cosheaf rowsByDegree
  boundariesByDegree <-
    first CosheafSupportChainFailed $
      IntMap.fromAscList
        <$> traverse
          (denseBoundaryAtDegree compositionLookup cellsByDegree basisByDegree)
          [0 .. maxDegreeInt]
  chainComplexValue <-
    first (CosheafSupportChainFailed . CosheafChainComplexFailed) $
      mkFiniteChainComplexChecked
        (HomologicalDegree maxDegreeInt)
        (`cosheafBoundaryIncidenceAtMap` boundariesByDegree)
  pure
    PreparedFiniteCosheafChain
      { preparedFiniteCosheafInternal = cosheaf,
        preparedFiniteCosheafMaxDegreeInternal = HomologicalDegree maxDegreeInt,
        preparedFiniteCosheafBasisByDegreeInternal = basisByDegree,
        preparedFiniteCosheafBoundariesByDegreeInternal = boundariesByDegree,
        preparedFiniteCosheafChainComplexInternal = chainComplexValue
      }
  where
    supportPlan =
      pcsPlan preparedSupport

    cosheaf =
      pcsCosheaf preparedSupport

    HomologicalDegree maxDegreeInt =
      cspMaxDegree supportPlan

denseRowsByDegreeFromSupportPlan ::
  CosheafSupportPlan ->
  FiniteCosheaf site value ->
  DenseCorestrictionFrontier (SiteObject site) (SiteMorphism site) ->
  Int ->
  Either
    (CosheafChainFailure (SiteObject site) (SiteMorphism site) value)
    (IntMap (Vector (DenseNerveRow (SiteObject site) (SiteMorphism site))))
denseRowsByDegreeFromSupportPlan supportPlan cosheaf retainedCorestrictionsBySource maxDegreeInt = do
  zeroRows <- zeroDenseRowsFromSupportPlan supportPlan cosheaf
  pure
    ( IntMap.fromAscList
        (zip [0 .. maxDegreeInt] (denseRowFrontiers supportPlan retainedCorestrictionsBySource maxDegreeInt zeroRows))
    )

denseRowFrontiers ::
  CosheafSupportPlan ->
  DenseCorestrictionFrontier obj mor ->
  Int ->
  Vector (DenseNerveRow obj mor) ->
  [Vector (DenseNerveRow obj mor)]
denseRowFrontiers supportPlan retainedCorestrictionsBySource maxDegreeInt zeroRows =
  take
    (maxDegreeInt + 1)
    ( scanl
        (\rowsValue _degreeValue -> extendDenseRowsFromSupportPlan supportPlan retainedCorestrictionsBySource rowsValue)
        zeroRows
        [1 .. maxDegreeInt]
    )

zeroDenseRowsFromSupportPlan ::
  CosheafSupportPlan ->
  FiniteCosheaf site value ->
  Either
    (CosheafChainFailure (SiteObject site) (SiteMorphism site) value)
    (Vector (DenseNerveRow (SiteObject site) (SiteMorphism site)))
zeroDenseRowsFromSupportPlan supportPlan cosheaf =
  pure
    ( sortDenseRows . Vector.fromList . filter (rowRetained supportPlan) $
        mapMaybe zeroRowForObject retainedObjects
    )
  where
    retainedObjects =
      supportCarrierItems (cspObjects supportPlan)

    zeroRowForObject objectKey = do
      objectValue <- denseIndexValueAt objectKey (cosheafSiteObjectIndex (fcSiteIndex cosheaf))
      Just
        DenseNerveRow
          { dnrKey =
              CosheafNerveChainKey
                { cnckSourceObjectKey = objectKey,
                  cnckMorphismKeys = []
                },
            dnrSourceObject = objectValue,
            dnrEndObjectKey = objectKey,
            dnrCorestrictions = Vector.empty
          }

extendDenseRowsFromSupportPlan ::
  CosheafSupportPlan ->
  DenseCorestrictionFrontier obj mor ->
  Vector (DenseNerveRow obj mor) ->
  Vector (DenseNerveRow obj mor)
extendDenseRowsFromSupportPlan supportPlan retainedCorestrictionsBySource =
  Vector.concatMap extendOne
  where
    extendOne row =
      Vector.mapMaybe
        (appendDenseCorestrictionIfRetained supportPlan row)
        (IntMap.findWithDefault Vector.empty (unObjectKey (dnrEndObjectKey row)) retainedCorestrictionsBySource)

appendDenseCorestrictionIfRetained ::
  CosheafSupportPlan ->
  DenseNerveRow obj mor ->
  CompiledCorestriction obj mor ->
  Maybe (DenseNerveRow obj mor)
appendDenseCorestrictionIfRetained supportPlan row corestrictionValue =
  if nerveKeyRetained supportPlan rowKey
    then
      Just
        DenseNerveRow
          { dnrKey = rowKey,
            dnrSourceObject = dnrSourceObject row,
            dnrEndObjectKey = ccTargetObjectKey corestrictionValue,
            dnrCorestrictions = Vector.snoc (dnrCorestrictions row) corestrictionValue
          }
    else Nothing
  where
    rowKey =
      CosheafNerveChainKey
        { cnckSourceObjectKey = cnckSourceObjectKey (dnrKey row),
          cnckMorphismKeys =
            cnckMorphismKeys (dnrKey row)
              <> [ccMorphismKey corestrictionValue]
        }

rowRetained :: CosheafSupportPlan -> DenseNerveRow obj mor -> Bool
rowRetained supportPlan row =
  nerveKeyRetained supportPlan (dnrKey row)
{-# INLINE rowRetained #-}

nerveKeyRetained :: CosheafSupportPlan -> CosheafNerveChainKey -> Bool
nerveKeyRetained supportPlan rowKey =
  maybe
    True
    (\rowCarrier -> scContains rowCarrier rowKey)
    (cspNerveRows supportPlan)
{-# INLINE nerveKeyRetained #-}

chainCellRetained :: CosheafSupportPlan -> DenseChainCell obj mor -> Bool
chainCellRetained supportPlan cell =
  maybe
    True
    (\cellCarrier -> scContains cellCarrier (dccKey cell))
    (cspChainCells supportPlan)
{-# INLINE chainCellRetained #-}

sortDenseRows ::
  Vector (DenseNerveRow obj mor) ->
  Vector (DenseNerveRow obj mor)
sortDenseRows =
  Vector.fromList
    . List.sortOn dnrKey
    . Vector.toList

nonIdentityCorestrictionsBySourceVectorFromPreparedSupport ::
  (Site site, Eq (SiteMorphism site)) =>
  PreparedCosheafSupport site value ->
  IntMap (Vector (CompiledCorestriction (SiteObject site) (SiteMorphism site)))
nonIdentityCorestrictionsBySourceVectorFromPreparedSupport preparedSupport =
  fmap
    (Vector.fromList . List.sortOn ccMorphismKey)
    ( IntMap.fromListWith
        (<>)
        (fmap sourceCorestrictionPair retainedNonIdentityCorestrictions)
    )
  where
    retainedNonIdentityCorestrictions =
      filter
        (not . isIdentityCorestriction (fcSite cosheaf))
        (pcsCorestrictions preparedSupport)

    cosheaf =
      pcsCosheaf preparedSupport

    sourceCorestrictionPair ::
      CompiledCorestriction obj mor ->
      (Int, [CompiledCorestriction obj mor])
    sourceCorestrictionPair corestrictionValue =
      ( unObjectKey (ccSourceObjectKey corestrictionValue),
        [corestrictionValue]
      )

isIdentityCorestriction ::
  (Site site, Eq (SiteMorphism site)) =>
  site ->
  CompiledCorestriction (SiteObject site) (SiteMorphism site) ->
  Bool
isIdentityCorestriction site corestrictionValue =
  isIdentityMorphism site (ccMorphism corestrictionValue)

denseCellsAtDegreeFromPreparedSupport ::
  PreparedCosheafSupport site value ->
  Vector (DenseNerveRow (SiteObject site) mor) ->
  Either
    (CosheafChainFailure (SiteObject site) mor value)
    (Vector (DenseChainCell (SiteObject site) mor))
denseCellsAtDegreeFromPreparedSupport preparedSupport rows =
  Vector.concat . Vector.toList
    <$> traverse
      (denseCellsForRowFromPreparedSupport preparedSupport)
      rows

denseCellsForRowFromPreparedSupport ::
  PreparedCosheafSupport site value ->
  DenseNerveRow (SiteObject site) mor ->
  Either
    (CosheafChainFailure (SiteObject site) mor value)
    (Vector (DenseChainCell (SiteObject site) mor))
denseCellsForRowFromPreparedSupport preparedSupport row = do
  pure
    ( Vector.fromList
        ( filter
            (chainCellRetained supportPlan)
            ( fmap
                (denseChainCellForCostalk row)
                (retainedCostalkKeysForRow preparedSupport row)
            )
        )
    )
  where
    supportPlan =
      pcsPlan preparedSupport

    denseChainCellForCostalk :: DenseNerveRow obj mor -> CostalkKey -> DenseChainCell obj mor
    denseChainCellForCostalk rowValue costalkKey =
      DenseChainCell
        { dccKey =
            CosheafChainBasisKey
              { ccbkNerveChainKey = dnrKey rowValue,
                ccbkSourceCostalkKey = costalkKey
              },
          dccRow = rowValue,
          dccSourceCostalkKey = costalkKey
        }

retainedCostalkKeysForRow :: PreparedCosheafSupport site value -> DenseNerveRow obj mor -> [CostalkKey]
retainedCostalkKeysForRow preparedSupport row =
  maybe
    []
    (fmap CostalkKey . IntSet.toAscList)
    (IntMap.lookup (unObjectKey (cnckSourceObjectKey (dnrKey row))) (pcsCostalkKeysByObject preparedSupport))

publicDenseChainCell ::
  FiniteCosheaf site value ->
  DenseChainCell (SiteObject site) (SiteMorphism site) ->
  Either
    (CosheafChainFailure (SiteObject site) (SiteMorphism site) value)
    (CosheafChainCell (SiteObject site) (SiteMorphism site) value)
publicDenseChainCell cosheaf cell = do
  sourceCostalk <- costalkAtObjectKey cosheaf sourceObjectKey
  sourceValue <- costalkValueAt sourceObjectKey (dccSourceCostalkKey cell) cosheaf
  pure
    CosheafChainCell
      { cosheafChainCellKey = dccKey cell,
        cosheafChainCellNerveChain =
          CosheafNerveChain
            { cosheafNerveChainSourceObject = dnrSourceObject (dccRow cell),
              cosheafNerveChainMorphisms =
                fmap ccMorphism
                  (Vector.toList (dnrCorestrictions (dccRow cell)))
            },
        cosheafChainCellRepresentative =
          CosectionRepresentative
            { cosectionRepObject = fcostalkObject sourceCostalk,
              cosectionRepValue = sourceValue
            },
        cosheafChainCellCostalkKey = dccSourceCostalkKey cell
      }
  where
    sourceObjectKey =
      cnckSourceObjectKey (dnrKey (dccRow cell))

costalkAtObjectKey ::
  FiniteCosheaf site value ->
  ObjectKey ->
  Either (CosheafChainFailure obj mor value) (FiniteCostalk (SiteObject site) value)
costalkAtObjectKey cosheaf objectKey =
  maybe
    (Left (CosheafChainCostalkMissing objectKey))
    Right
    (finiteCostalkAtObjectKey objectKey cosheaf)

costalkValueAt ::
  ObjectKey ->
  CostalkKey ->
  FiniteCosheaf site value ->
  Either (CosheafChainFailure obj mor value) value
costalkValueAt objectKey costalkKey cosheaf = do
  costalk <- costalkAtObjectKey cosheaf objectKey
  maybe
    (Left (CosheafChainCostalkValueMissing objectKey costalkKey))
    Right
    (finiteCostalkValueAt costalkKey costalk)

mkDenseCosheafChainBasisTable ::
  FiniteCosheaf site value ->
  HomologicalDegree ->
  Vector (DenseChainCell (SiteObject site) (SiteMorphism site)) ->
  Either
    (CosheafChainFailure (SiteObject site) (SiteMorphism site) value)
    (CosheafChainBasisTable site value)
mkDenseCosheafChainBasisTable cosheaf degreeValue denseCells = do
  publicCells <- traverse (publicDenseChainCell cosheaf) denseCells
  let keys =
        fmap cosheafChainCellKey publicCells
  case duplicatesOrd (Vector.toList keys) of
    duplicateKey : _ ->
      Left (CosheafChainDuplicateBasisKey degreeValue duplicateKey)
    [] ->
      pure
        CosheafChainBasisTable
          { cosheafChainBasisDegreeInternal = degreeValue,
            cosheafChainBasisCellsInternal = publicCells,
            cosheafChainBasisIndexByKeyInternal =
              Map.fromList
                (zip (Vector.toList keys) [0 :: Int ..]),
            cosheafChainBasisKeyByIndexInternal =
              IntMap.fromAscList
                (zip [0 :: Int ..] (Vector.toList keys))
          }

compiledCorestrictionCompositionLookupForRows ::
  (Site site, Ord (SiteMorphism site)) =>
  FiniteCosheaf site value ->
  IntMap (Vector (DenseNerveRow (SiteObject site) (SiteMorphism site))) ->
  Either
    (CosheafChainFailure (SiteObject site) (SiteMorphism site) value)
    (CorestrictionCompositionLookup (SiteObject site) (SiteMorphism site))
compiledCorestrictionCompositionLookupForRows cosheaf rowsByDegree =
  Map.fromList
    <$> traverse compositeEntry retainedAdjacentPairs
  where
    retainedAdjacentPairs =
      Map.elems . Map.fromList . foldMap rowAdjacentPairs . foldMap Vector.toList $
        IntMap.elems rowsByDegree

    rowAdjacentPairs ::
      DenseNerveRow obj mor ->
      [
        ( (CosheafMorphismKey, CosheafMorphismKey),
          ( CompiledCorestriction obj mor,
            CompiledCorestriction obj mor
          )
        )
      ]
    rowAdjacentPairs row =
      fmap keyedAdjacentPair (adjacentCorestrictionPairs row)

    keyedAdjacentPair ::
      ( CompiledCorestriction obj mor,
        CompiledCorestriction obj mor
      ) ->
      ( (CosheafMorphismKey, CosheafMorphismKey),
        ( CompiledCorestriction obj mor,
          CompiledCorestriction obj mor
        )
      )
    keyedAdjacentPair pair@(outerCorestriction, innerCorestriction) =
      ((ccMorphismKey outerCorestriction, ccMorphismKey innerCorestriction), pair)

    adjacentCorestrictionPairs ::
      DenseNerveRow obj mor ->
      [
        ( CompiledCorestriction obj mor,
          CompiledCorestriction obj mor
        )
      ]
    adjacentCorestrictionPairs row =
      fmap
        (\(innerCorestriction, outerCorestriction) -> (outerCorestriction, innerCorestriction))
        (zip corestrictions (drop 1 corestrictions))
      where
        corestrictions =
          Vector.toList (dnrCorestrictions row)

    compositeEntry (outerCorestriction, innerCorestriction) = do
      compositeMorphism <-
        maybe
          ( Left
              ( CosheafChainCompositeUndefined
                  outerMorphism
                  innerMorphism
              )
          )
          Right
          ( composeChecked
              (fcSite cosheaf)
              outerMorphism
              innerMorphism
          )
      compositeValue <-
        if isIdentityMorphism (fcSite cosheaf) compositeMorphism
          then Right CorestrictionCompositeIdentity
          else
            maybe
              (Left (CosheafChainCompositeCorestrictionMissing compositeMorphism))
              (Right . CorestrictionCompositeCorestriction)
              (cosheafMorphismKeyOf compositeMorphism (fcSiteIndex cosheaf) >>= corestrictionAtKey)
      pure
        ( (ccMorphismKey outerCorestriction, ccMorphismKey innerCorestriction),
          compositeValue
        )
      where
        outerMorphism =
          ccMorphism outerCorestriction

        innerMorphism =
          ccMorphism innerCorestriction

    corestrictionAtKey morphismKey =
      IntMap.lookup (encodeDenseKey morphismKey) (fcCorestrictions cosheaf)

denseBoundaryAtDegree ::
  CorestrictionCompositionLookup (SiteObject site) (SiteMorphism site) ->
  IntMap (Vector (DenseChainCell (SiteObject site) (SiteMorphism site))) ->
  IntMap (CosheafChainBasisTable site value) ->
  Int ->
  Either
    (CosheafChainFailure (SiteObject site) (SiteMorphism site) value)
    (Int, BoundaryIncidence Int)
denseBoundaryAtDegree compositeLookup cellsByDegree basisByDegree degreeValue = do
  let sourceCells =
        IntMap.findWithDefault Vector.empty degreeValue cellsByDegree
  incidenceValue <-
    if degreeValue <= 0
      then
        Right
          ( emptyBoundaryIncidenceOf
              (fromIntegral (Vector.length sourceCells))
              0
          )
      else do
        targetBasis <-
          maybe
            (Left (CosheafChainBasisTableMissing (HomologicalDegree (degreeValue - 1))))
            Right
            (IntMap.lookup (degreeValue - 1) basisByDegree)
        entries <-
          concat
            <$> traverse
              ( boundaryEntriesForDenseSource
                  compositeLookup
                  degreeValue
                  (cosheafChainBasisIndexByKeyInternal targetBasis)
              )
              (Vector.toList (Vector.indexed sourceCells))
        first CosheafChainBoundaryShapeFailed $
          mkBoundaryIncidenceFromOrderedEntries
            (fromIntegral (Vector.length sourceCells))
            (fromIntegral (cosheafChainBasisTableSize targetBasis))
            entries
  pure (degreeValue, incidenceValue)

boundaryEntriesForDenseSource ::
  CorestrictionCompositionLookup obj mor ->
  Int ->
  Map CosheafChainBasisKey Int ->
  (Int, DenseChainCell obj mor) ->
  Either
    (CosheafChainFailure obj mor value)
    [BoundaryEntry Int]
boundaryEntriesForDenseSource compositeLookup degreeValue targetBasisByKey (sourceIndexValue, sourceCell) = do
  terms <- boundaryTermsForDenseCell compositeLookup sourceCell
  traverse termEntry terms
  where
    termEntry term =
      maybe
        (Left (CosheafChainBoundaryTargetMissing (HomologicalDegree degreeValue) (btTargetKey term)))
        ( \targetIndexValue ->
            Right
              ( mkBoundaryEntry
                  (fromIntegral sourceIndexValue)
                  (fromIntegral targetIndexValue)
                  (btCoefficient term)
              )
        )
        (Map.lookup (btTargetKey term) targetBasisByKey)

boundaryTermsForDenseCell ::
  CorestrictionCompositionLookup obj mor ->
  DenseChainCell obj mor ->
  Either
    (CosheafChainFailure obj mor value)
    [BoundaryTerm]
boundaryTermsForDenseCell compositeLookup cell =
  case Vector.toList (dnrCorestrictions (dccRow cell)) of
    [] ->
      Right []
    firstCorestriction : remainingCorestrictions -> do
      firstTerm <-
        firstFaceTerm firstCorestriction remainingCorestrictions cell
      innerTerms <-
        innerFaceTerms compositeLookup cell
      let lastTerm =
            lastFaceTerm
              (Vector.length (dnrCorestrictions (dccRow cell)))
              cell
      pure (firstTerm : innerTerms <> [lastTerm])

firstFaceTerm ::
  CompiledCorestriction obj mor ->
  [CompiledCorestriction obj mor] ->
  DenseChainCell obj mor ->
  Either (CosheafChainFailure obj mor value) BoundaryTerm
firstFaceTerm firstCorestriction remainingCorestrictions cell = do
  targetCostalkKey <-
    corestrictSourceKey firstCorestriction (dccSourceCostalkKey cell)
  pure
    BoundaryTerm
      { btCoefficient = 1,
        btTargetKey =
          CosheafChainBasisKey
            { ccbkNerveChainKey =
                CosheafNerveChainKey
                  { cnckSourceObjectKey = ccTargetObjectKey firstCorestriction,
                    cnckMorphismKeys = fmap ccMorphismKey remainingCorestrictions
                  },
              ccbkSourceCostalkKey = targetCostalkKey
            }
      }

innerFaceTerms ::
  CorestrictionCompositionLookup obj mor ->
  DenseChainCell obj mor ->
  Either
    (CosheafChainFailure obj mor value)
    [BoundaryTerm]
innerFaceTerms compositeLookup cell =
  fmap (mapMaybe id) $
    traverse
      innerFaceTerm
      (innerFaceContexts (Vector.toList (dnrCorestrictions (dccRow cell))))
  where
    innerFaceTerm
      InnerFaceContext
        { ifcFaceIndex = faceIndexValue,
          ifcBeforeCorestrictions = beforeCorestrictions,
          ifcInnerCorestriction = innerCorestriction,
          ifcOuterCorestriction = outerCorestriction,
          ifcAfterCorestrictions = afterCorestrictions
        } = do
        compositeValue <-
          maybe
            ( Left
                ( CosheafChainCompositeLookupMissing
                    (ccMorphism outerCorestriction)
                    (ccMorphism innerCorestriction)
                )
            )
            Right
            ( Map.lookup
                (ccMorphismKey outerCorestriction, ccMorphismKey innerCorestriction)
                compositeLookup
            )
        case compositeValue of
          CorestrictionCompositeIdentity ->
            Right Nothing
          CorestrictionCompositeCorestriction compositeCorestriction ->
            Right
              ( Just
                  BoundaryTerm
                    { btCoefficient = alternatingFaceCoefficient faceIndexValue,
                      btTargetKey =
                        CosheafChainBasisKey
                          { ccbkNerveChainKey =
                              CosheafNerveChainKey
                                { cnckSourceObjectKey =
                                    cnckSourceObjectKey (dnrKey (dccRow cell)),
                                  cnckMorphismKeys =
                                    fmap ccMorphismKey beforeCorestrictions
                                      <> [ccMorphismKey compositeCorestriction]
                                      <> fmap ccMorphismKey afterCorestrictions
                                },
                            ccbkSourceCostalkKey = dccSourceCostalkKey cell
                          }
                    }
              )

innerFaceContexts ::
  [CompiledCorestriction obj mor] ->
  [InnerFaceContext obj mor]
innerFaceContexts corestrictions =
  fmap
    indexedContext
    ( zip
        [1 ..]
        ( mapMaybe
            contextAt
            ( zip3
                (List.inits corestrictions)
                corestrictions
                (drop 1 (List.tails corestrictions))
            )
        )
    )
  where
    indexedContext ::
      ( Int,
        ( [CompiledCorestriction obj mor],
          CompiledCorestriction obj mor,
          CompiledCorestriction obj mor,
          [CompiledCorestriction obj mor]
        )
      ) ->
      InnerFaceContext obj mor
    indexedContext
      (faceIndexValue, (beforeCorestrictions, innerCorestriction, outerCorestriction, afterCorestrictions)) =
        InnerFaceContext
          { ifcFaceIndex = faceIndexValue,
            ifcBeforeCorestrictions = beforeCorestrictions,
            ifcInnerCorestriction = innerCorestriction,
            ifcOuterCorestriction = outerCorestriction,
            ifcAfterCorestrictions = afterCorestrictions
          }

    contextAt ::
      ( [CompiledCorestriction obj mor],
        CompiledCorestriction obj mor,
        [CompiledCorestriction obj mor]
      ) ->
      Maybe
        ( [CompiledCorestriction obj mor],
          CompiledCorestriction obj mor,
          CompiledCorestriction obj mor,
          [CompiledCorestriction obj mor]
        )
    contextAt
      (beforeCorestrictions, innerCorestriction, outerCorestriction : afterCorestrictions) =
        Just
          ( beforeCorestrictions,
            innerCorestriction,
            outerCorestriction,
            afterCorestrictions
          )
    contextAt (_, _, []) =
      Nothing

lastFaceTerm ::
  Int ->
  DenseChainCell obj mor ->
  BoundaryTerm
lastFaceTerm dimensionValue cell =
  BoundaryTerm
    { btCoefficient = alternatingFaceCoefficient dimensionValue,
      btTargetKey =
        CosheafChainBasisKey
          { ccbkNerveChainKey =
              CosheafNerveChainKey
                { cnckSourceObjectKey = cnckSourceObjectKey (dnrKey (dccRow cell)),
                  cnckMorphismKeys =
                    fmap ccMorphismKey
                      (Vector.toList (dropLastVector (dnrCorestrictions (dccRow cell))))
                },
            ccbkSourceCostalkKey = dccSourceCostalkKey cell
          }
    }

corestrictSourceKey ::
  CompiledCorestriction obj mor ->
  CostalkKey ->
  Either (CosheafChainFailure obj mor value) CostalkKey
corestrictSourceKey corestrictionValue sourceCostalkKey =
  maybe
    (Left (CosheafChainCorestrictionMalformed (ccMorphism corestrictionValue) sourceCostalkKey))
    Right
    (IntMap.lookup (unCostalkKey sourceCostalkKey) (ccSourceToTarget corestrictionValue))

alternatingFaceCoefficient :: Int -> Int
alternatingFaceCoefficient faceIndexValue =
  if even faceIndexValue
    then 1
    else -1

dropLastVector :: Vector value -> Vector value
dropLastVector values
  | Vector.null values =
      Vector.empty
  | otherwise =
      Vector.take (Vector.length values - 1) values
