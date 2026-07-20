{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Cosheaf.Chain.Cover
  ( CoverIntersectionCell (..),
    CoverFace (..),
    CoverNervePlan (..),
    CoverChainSpec (..),
    CoverBoundaryProvenance,
    CoverChainFailure (..),
    coverNervePlanFromEffectiveCoverPlan,
    prepareCoverCosheafChain,
  )
where

import Data.Bifunctor (first)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Algebra
  ( Semiring,
  )
import Moonlight.Cosheaf.Chain.Linear
  ( CosheafBoundaryProvenance,
    LinearCosheafChainFailure (..),
    LinearCosheafChainSpec (..),
    prepareLinearCosheafChainFromSupportPlan,
  )
import Moonlight.Cosheaf.Chain.Coefficient
  ( CoefficientOps,
  )
import Moonlight.Cosheaf.Chain.Prepared
  ( PreparedCosheafChain,
  )
import Moonlight.Cosheaf.Support.Linear
  ( fullLinearCosheafSupportPlan,
  )
import Moonlight.Homology
  ( BoundaryIncidence,
    HomologicalDegree (..),
  )
import Moonlight.Sheaf.Site.Analysis.Scaffold
  ( SiteBoundaryAlgebra (..),
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    PullbackSquare (..),
  )
import Moonlight.Sheaf.Site.Plan
  ( CoverSlot,
    CoverSlotKey,
    EffectiveCoverPlan,
    OverlapPlan,
    coverSlotArrow,
    coverSlotKey,
    effectiveCoverOverlapPlans,
    effectiveCoverSlots,
    opLeftSlot,
    opPullbackSquare,
    opRightSlot,
  )
import Numeric.Natural (Natural)

-- | A q-simplex of the cover nerve together with the concrete local object that
-- realizes that ordered intersection. Degree is length(slotKeys)-1; it is kept
-- explicitly so downstream basis construction never guesses.
type CoverIntersectionCell :: Type -> Type -> Type
data CoverIntersectionCell obj mor = CoverIntersectionCell
  { cicDegree :: !HomologicalDegree,
    cicSlotKeys :: !(NonEmpty CoverSlotKey),
    cicApex :: !obj,
    cicPullbackWitness :: !(Maybe (PullbackSquare obj mor))
  }
  deriving stock (Eq, Ord, Show)

-- | One oriented nerve face. The optional projection is available for the
-- degree-one cover faces produced from 'EffectiveCoverPlan'; higher explicit
-- plans may carry their own typed face morphism or leave coefficient transport
-- to the supplied block algebra.
type CoverFace :: Type -> Type -> Type
data CoverFace obj mor = CoverFace
  { coverFaceSource :: !(CoverIntersectionCell obj mor),
    coverFaceTarget :: !(CoverIntersectionCell obj mor),
    coverFaceDroppedSlot :: !CoverSlotKey,
    coverFaceDroppedOffset :: !Int,
    coverFaceProjection :: !(Maybe (CheckedMorphism obj mor))
  }
  deriving stock (Eq, Ord, Show)

-- | Local cover intersections, already glued into an ordered cellular nerve.
-- This is the authority consumed by chain construction; H0/coequalizer checks
-- are derived views over it, not a second cover registry.
type CoverNervePlan :: Type -> Type -> Type
data CoverNervePlan obj mor = CoverNervePlan
  { cnpEffectiveCoverPlan :: !(EffectiveCoverPlan obj mor),
    cnpMaxDegree :: !HomologicalDegree,
    cnpCellsByDegree :: !(Map HomologicalDegree [CoverIntersectionCell obj mor]),
    cnpFaces :: ![CoverFace obj mor]
  }
  deriving stock (Eq, Show)

-- | Coefficient-cosheaf presentation over the cover nerve.
type CoverChainSpec :: Type -> Type -> Type -> Type -> Type -> Type
data CoverChainSpec obj mor coefficient provenance coreFailure = CoverChainSpec
  { ccsNervePlan :: !(CoverNervePlan obj mor),
    ccsCostalkDimension :: CoverIntersectionCell obj mor -> Int,
    ccsCorestrictionBlock :: CoverFace obj mor -> Either coreFailure (BoundaryIncidence coefficient),
    ccsEntryProvenance :: CoverFace obj mor -> Int -> Int -> coefficient -> provenance
  }

type CoverBoundaryProvenance obj mor coefficient provenance =
  CosheafBoundaryProvenance
    (CoverIntersectionCell obj mor)
    (CoverFace obj mor)
    coefficient
    provenance

type CoverChainFailure :: Type -> Type -> Type -> Type -> Type
data CoverChainFailure obj mor coefficient coreFailure
  = CoverChainNegativeMaxDegree !Int
  | CoverChainHigherIntersectionsRequireExplicitFaceProjections !Int
  | CoverChainSlotMissing !CoverSlotKey
  | CoverChainLinearFailed
      !( LinearCosheafChainFailure
           (CoverIntersectionCell obj mor)
           (CoverFace obj mor)
           coefficient
           coreFailure
       )
  deriving stock (Eq, Show)

coverBoundaryAlgebra :: SiteBoundaryAlgebra (CoverNervePlan obj mor) (CoverIntersectionCell obj mor) (CoverFace obj mor)
coverBoundaryAlgebra =
  SiteBoundaryAlgebra
    { sbaDepth = \plan -> naturalDegree (cnpMaxDegree plan),
      sbaCellsAtDimension = \plan degreeValue ->
        Map.findWithDefault [] (HomologicalDegree degreeValue) (cnpCellsByDegree plan),
      sbaFaceMorphisms = cnpFaces,
      sbaFaceSource = coverFaceSource,
      sbaFaceTarget = coverFaceTarget,
      sbaFaceOrientation = coverFaceOrientation,
      sbaCellDimension = \cell ->
        case cicDegree cell of
          HomologicalDegree degreeValue -> degreeValue
    }

coverNervePlanFromEffectiveCoverPlan ::
  Int ->
  EffectiveCoverPlan obj mor ->
  Either (CoverChainFailure obj mor coefficient coreFailure) (CoverNervePlan obj mor)
coverNervePlanFromEffectiveCoverPlan maxDegreeInt effectivePlan
  | maxDegreeInt < 0 = Left (CoverChainNegativeMaxDegree maxDegreeInt)
  | maxDegreeInt > 1 = Left (CoverChainHigherIntersectionsRequireExplicitFaceProjections maxDegreeInt)
  | otherwise = do
      singletonCells <- traverse singletonCellForSlot (IntMap.elems (effectiveCoverSlots effectivePlan))
      let singletonCellsByKey = Map.fromList (fmap (\cell -> (cellSlotKey cell, cell)) singletonCells)
      pairCellsAndFaces <- traverse (pairCellAndFaces singletonCellsByKey) (effectiveCoverOverlapPlans effectivePlan)
      let pairCells = fmap fst pairCellsAndFaces
          pairFaces = foldMap snd pairCellsAndFaces
          cellsByDegree =
            Map.fromList
              ( [ (HomologicalDegree 0, singletonCells) ]
                  <> if maxDegreeInt >= 1 then [(HomologicalDegree 1, pairCells)] else []
              )
      Right
        CoverNervePlan
          { cnpEffectiveCoverPlan = effectivePlan,
            cnpMaxDegree = HomologicalDegree maxDegreeInt,
            cnpCellsByDegree = cellsByDegree,
            cnpFaces = if maxDegreeInt >= 1 then pairFaces else []
          }
  where
    singletonCellForSlot :: CoverSlot obj mor -> Either (CoverChainFailure obj mor coefficient coreFailure) (CoverIntersectionCell obj mor)
    singletonCellForSlot slot =
      Right
        CoverIntersectionCell
          { cicDegree = HomologicalDegree 0,
            cicSlotKeys = coverSlotKey slot :| [],
            cicApex = cmSource (coverSlotArrow slot),
            cicPullbackWitness = Nothing
          }

    pairCellAndFaces ::
      Map CoverSlotKey (CoverIntersectionCell obj mor) ->
      OverlapPlan obj mor ->
      Either (CoverChainFailure obj mor coefficient coreFailure) (CoverIntersectionCell obj mor, [CoverFace obj mor])
    pairCellAndFaces singletonCellsByKey overlapPlan = do
      leftCell <- lookupSlot singletonCellsByKey (opLeftSlot overlapPlan)
      rightCell <- lookupSlot singletonCellsByKey (opRightSlot overlapPlan)
      let square = opPullbackSquare overlapPlan
          sourceCell =
            CoverIntersectionCell
              { cicDegree = HomologicalDegree 1,
                cicSlotKeys = opLeftSlot overlapPlan :| [opRightSlot overlapPlan],
                cicApex = psApex square,
                cicPullbackWitness = Just square
              }
          dropLeftFace =
            CoverFace
              { coverFaceSource = sourceCell,
                coverFaceTarget = rightCell,
                coverFaceDroppedSlot = opLeftSlot overlapPlan,
                coverFaceDroppedOffset = 0,
                coverFaceProjection = Just (psToRight square)
              }
          dropRightFace =
            CoverFace
              { coverFaceSource = sourceCell,
                coverFaceTarget = leftCell,
                coverFaceDroppedSlot = opRightSlot overlapPlan,
                coverFaceDroppedOffset = 1,
                coverFaceProjection = Just (psToLeft square)
              }
      Right (sourceCell, [dropLeftFace, dropRightFace])

prepareCoverCosheafChain ::
  (Ord obj, Ord mor, Eq coefficient, Num coefficient, Semiring coefficient) =>
  CoefficientOps coefficient ->
  CoverChainSpec obj mor coefficient provenance coreFailure ->
  Either
    (CoverChainFailure obj mor coefficient coreFailure)
    (PreparedCosheafChain (CoverNervePlan obj mor) (CoverIntersectionCell obj mor) coefficient (CoverBoundaryProvenance obj mor coefficient provenance))
prepareCoverCosheafChain coefficientOps spec = do
  supportPlan <-
    first (CoverChainLinearFailed . LinearCosheafChainSupportFailed) $
      fullLinearCosheafSupportPlan nervePlan coverBoundaryAlgebra (ccsCostalkDimension spec)
  first CoverChainLinearFailed $
    prepareLinearCosheafChainFromSupportPlan
      coefficientOps
      supportPlan
      (coverLinearCosheafChainSpec spec)
  where
    nervePlan =
      ccsNervePlan spec

coverLinearCosheafChainSpec ::
  CoverChainSpec obj mor coefficient provenance coreFailure ->
  LinearCosheafChainSpec
    (CoverNervePlan obj mor)
    (CoverIntersectionCell obj mor)
    (CoverFace obj mor)
    coefficient
    provenance
    coreFailure
coverLinearCosheafChainSpec spec =
  LinearCosheafChainSpec
    { lccsSite = ccsNervePlan spec,
      lccsBoundaryAlgebra = coverBoundaryAlgebra,
      lccsCostalkDimension = ccsCostalkDimension spec,
      lccsCorestrictionBlock = ccsCorestrictionBlock spec,
      lccsEntryProvenance = ccsEntryProvenance spec
    }

lookupSlot ::
  Map CoverSlotKey (CoverIntersectionCell obj mor) ->
  CoverSlotKey ->
  Either (CoverChainFailure obj mor coefficient coreFailure) (CoverIntersectionCell obj mor)
lookupSlot singletonCellsByKey slotKey =
  maybe (Left (CoverChainSlotMissing slotKey)) Right (Map.lookup slotKey singletonCellsByKey)

cellSlotKey :: CoverIntersectionCell obj mor -> CoverSlotKey
cellSlotKey cell =
  case cicSlotKeys cell of
    slotKey :| _ -> slotKey
{-# INLINE cellSlotKey #-}

coverFaceOrientation :: CoverFace obj mor -> Int
coverFaceOrientation face =
  if even (coverFaceDroppedOffset face) then 1 else (-1)
{-# INLINE coverFaceOrientation #-}

naturalDegree :: HomologicalDegree -> Natural
naturalDegree (HomologicalDegree degreeValue)
  | degreeValue <= 0 = 0
  | otherwise = fromIntegral degreeValue
{-# INLINE naturalDegree #-}
