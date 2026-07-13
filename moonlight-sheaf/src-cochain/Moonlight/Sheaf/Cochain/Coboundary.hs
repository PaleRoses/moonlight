module Moonlight.Sheaf.Cochain.Coboundary
  ( CoboundaryContribution (..),
    CoboundaryReducer (..),
    CoboundarySpec (..),
    CoboundaryEntry,
    ceRestriction,
    ceSourceCell,
    ceTargetCell,
    ceOrientation,
    ceWitness,
    mkCoboundaryEntry,
    CoboundaryMatrix (..),
    CoboundaryAssemblyPlan,
    CoboundaryIncidencePlan,
    CoboundaryBlockKernel (..),
    RankOneCoboundaryPlan,
    buildCoboundary,
    buildCoboundaryComplex,
    prepareRankOneCoboundaryPlan,
    materializeRankOneCoboundaryIncidence,
    materializeRankOneCoboundaryDifferential,
    applyRankOneCoboundaryPlan,
    applyRankOneCoboundaryPlanDense,
    buildRankOneCoboundaryComplex,
    prepareCoboundaryAssemblyPlan,
    prepareCoboundaryIncidencePlan,
    prepareCoboundaryIncidencePlanWithKernel,
    applyCoboundaryAssemblyPlanWithKernel,
    applyCoboundaryIncidencePlan,
    materializeCoboundaryAssemblyPlan,
    materializeCoboundaryIncidencePlan,
    materializeCoboundaryDifferential,
    materializeCoboundaryIncidence,
    materializeCoboundaryIncidenceWithKernel,
    coboundaryIncidenceToDouble,
    applyCoboundary,
    collapseCoboundary,
    checkCoboundaryNilpotence,
  )
where

import Data.Function ((&))
import Control.Monad (foldM, guard)
import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Vector.Unboxed qualified as Unboxed
import Moonlight.Homology
  ( BoundaryEntry,
    BoundaryIncidence,
    BoundaryIncidenceShapeError (BoundaryIncidenceBlockShapeMismatch),
    boundaryCoefficient,
    boundaryEntries,
    boundaryIncidenceApply,
    composeBoundaryIncidence,
    mapBoundaryCoefficients,
    mkBoundaryEntryFromInts,
    mkBoundaryIncidenceFromOrderedEntries,
    scaleBoundaryIncidence,
    sourceIndex,
    targetIndex,
  )
import Moonlight.LinAlg.Sparse (PackedSparseOperator)
import Moonlight.Sheaf.Operator.BuildError
  ( OperatorBasisRole (..),
    SheafOperatorBuildError (..),
  )
import Moonlight.Sheaf.Operator.GradedComplex
import Moonlight.Homology
  ( HomologicalDegree (..),
    incrementDegree,
  )
import Moonlight.Sheaf.Operator.LinearBasis
  ( LinearBasis,
    linearBasisCardinality,
    linearBasisCells,
    linearBasisCellSlotOrError,
    linearBasisSlotAtIndex,
    mkLinearBasis,
  )
import Moonlight.Sheaf.Operator.Sparse
  ( liftBoundaryShape,
    applyPackedSparseOperatorDenseAsSheafOperator,
    packedSparseOperatorFromBoundary,
    validateBoundaryBlockShape,
  )
import Moonlight.Sheaf.Kernel.Basis
  ( SheafBasis,
    basisCells,
    basisCardinality,
    basisCellIndex,
  )
import Moonlight.Sheaf.Section.Model
  ( SheafModel,
  )
import Moonlight.Sheaf.Section.Store.State
  ( totalStalkAt,
  )
import Moonlight.Sheaf.Section.Store.Types
  ( TotalSectionStore,
  )
import Moonlight.Sheaf.Section.Morphism
  ( Restriction,
    RestrictionKind (..),
    rKind,
    rSource,
    rTarget,
    rWitness,
    restrictionKindCoefficient,
  )
import Moonlight.Sheaf.Section.Restriction
  ( RestrictionIndex,
    incidenceRestrictions,
  )
import Moonlight.Sheaf.Section.Stalk
  ( StalkAlgebra,
    restrictStalk,
  )

data CoboundaryContribution stalk = CoboundaryContribution
  { contributionOrientation :: Int,
    contributionValue :: stalk
  }
  deriving stock (Eq, Show)

data CoboundaryReducer stalk = CoboundaryReducer
  { runCoboundaryReducer :: [CoboundaryContribution stalk] -> stalk
  }

data CoboundarySpec cell = CoboundarySpec
  { csDimension :: HomologicalDegree,
    csSourceBasis :: SheafBasis cell,
    csTargetBasis :: SheafBasis cell
  }

data CoboundaryEntry cell witness = CoboundaryEntry
  { ceRestriction :: Restriction cell witness,
    ceSourceBasisSlot :: !(Maybe Int),
    ceTargetBasisSlot :: !(Maybe Int)
  }
  deriving stock (Eq, Show)

ceSourceCell :: CoboundaryEntry cell witness -> cell
ceSourceCell =
  rTarget . ceRestriction

ceTargetCell :: CoboundaryEntry cell witness -> cell
ceTargetCell =
  rSource . ceRestriction

ceOrientation :: CoboundaryEntry cell witness -> Int
ceOrientation =
  fromMaybe 0 . restrictionKindCoefficient . rKind . ceRestriction

ceWitness :: CoboundaryEntry cell witness -> witness
ceWitness =
  rWitness . ceRestriction

data CoboundaryMatrix cell witness = CoboundaryMatrix
  { cmDimension :: HomologicalDegree,
    cmEntries :: [CoboundaryEntry cell witness],
    cmSourceBasis :: SheafBasis cell,
    cmTargetBasis :: SheafBasis cell
  }
  deriving stock (Eq, Show)

data CoboundaryAssemblyPlan cell stalk = CoboundaryAssemblyPlan
  { capDimension :: HomologicalDegree,
    capSourceLinearBasis :: LinearBasis cell,
    capTargetLinearBasis :: LinearBasis cell,
    capEntries :: [CoboundaryAssemblyEntry stalk]
  }

data CoboundaryAssemblyEntry stalk = CoboundaryAssemblyEntry
  { caeSourceOffset :: Int,
    caeSourceDimension :: Int,
    caeTargetOffset :: Int,
    caeTargetDimension :: Int,
    caeOrientation :: Int,
    caeSourceStalk :: stalk,
    caeTargetStalk :: stalk
  }

data CoboundaryIncidencePlan cell = CoboundaryIncidencePlan
  { cipDimension :: HomologicalDegree,
    cipSourceLinearBasis :: LinearBasis cell,
    cipTargetLinearBasis :: LinearBasis cell,
    cipIncidence :: BoundaryIncidence Int
  }

data CoboundaryBlockKernel stalk
  = GeneralCoboundaryBlock (stalk -> stalk -> BoundaryIncidence Int)
  | DimensionCoboundaryBlock (Int -> Int -> BoundaryIncidence Int)
  | UnitCoboundaryBlock
  | ScalarCoboundaryBlock (stalk -> stalk -> Int)

data RankOneCoboundaryPlan cell = RankOneCoboundaryPlan
  { rocpDimension :: !HomologicalDegree,
    rocpSourceBasis :: !(SheafBasis cell),
    rocpTargetBasis :: !(SheafBasis cell),
    rocpIncidence :: !(BoundaryIncidence Int),
    rocpPackedIncidence :: !(PackedSparseOperator Int)
  }
  deriving stock (Eq, Show)

data RankOneCoboundaryEntry = RankOneCoboundaryEntry
  { rankOneCoboundaryEntrySourceOffset :: !Int,
    rankOneCoboundaryEntryTargetOffset :: !Int,
    rankOneCoboundaryEntryCoefficient :: !Int
  }
  deriving stock (Eq, Show)

data DimensionBlockCacheKey = DimensionBlockCacheKey
  { dbckSourceDimension :: !Int,
    dbckTargetDimension :: !Int,
    dbckOrientation :: !Int
  }
  deriving stock (Eq, Ord, Show)

buildCoboundary ::
  Ord cell =>
  CoboundarySpec cell ->
  RestrictionIndex cell witness ->
  Either (SheafOperatorBuildError cell) (CoboundaryMatrix cell witness)
buildCoboundary spec restrictions =
  buildCoboundaryFromIncidenceRestrictions spec (incidenceRestrictions restrictions)

buildCoboundaryFromIncidenceRestrictions ::
  Ord cell =>
  CoboundarySpec cell ->
  [Restriction cell witness] ->
  Either (SheafOperatorBuildError cell) (CoboundaryMatrix cell witness)
buildCoboundaryFromIncidenceRestrictions spec incidenceRestrictionValues = do
  entries <-
    traverse
      ( \(restriction, sourceSlot, targetSlot) ->
          mkCoboundaryEntryAtSlots restriction sourceSlot targetSlot
      )
      (mapMaybe (restrictionSlotsInSpec spec) incidenceRestrictionValues)
  pure
    CoboundaryMatrix
      { cmDimension = csDimension spec,
        cmEntries = entries,
        cmSourceBasis = csSourceBasis spec,
        cmTargetBasis = csTargetBasis spec
      }

restrictionOffsetsInSpec ::
  Ord cell =>
  CoboundarySpec cell ->
  Restriction cell witness ->
  Maybe (Int, Int)
restrictionOffsetsInSpec spec restriction =
  (,)
    <$> basisCellIndex (rTarget restriction) (csSourceBasis spec)
    <*> basisCellIndex (rSource restriction) (csTargetBasis spec)

restrictionSlotsInSpec ::
  Ord cell =>
  CoboundarySpec cell ->
  Restriction cell witness ->
  Maybe (Restriction cell witness, Int, Int)
restrictionSlotsInSpec spec restriction =
  fmap
    ( \(sourceSlot, targetSlot) ->
        (restriction, sourceSlot, targetSlot)
    )
    (restrictionOffsetsInSpec spec restriction)

buildCoboundaryComplex ::
  Ord cell =>
  (cell -> stalk) ->
  (stalk -> Int) ->
  (stalk -> stalk -> BoundaryIncidence Int) ->
  CoboundarySpec cell ->
  CoboundarySpec cell ->
  RestrictionIndex cell witness ->
  Either (SheafOperatorBuildError cell) (GradedComplex cell Int)
buildCoboundaryComplex lookupCellStalk stalkDimension coboundaryBlock =
  buildCoboundaryComplexWithKernel
    lookupCellStalk
    stalkDimension
    (GeneralCoboundaryBlock coboundaryBlock)

buildCoboundaryComplexWithKernel ::
  Ord cell =>
  (cell -> stalk) ->
  (stalk -> Int) ->
  CoboundaryBlockKernel stalk ->
  CoboundarySpec cell ->
  CoboundarySpec cell ->
  RestrictionIndex cell witness ->
  Either (SheafOperatorBuildError cell) (GradedComplex cell Int)
buildCoboundaryComplexWithKernel lookupCellStalk stalkDimension blockKernel spec0 spec1 restrictions = do
  let incidenceRestrictionValues =
        incidenceRestrictions restrictions
  matrix0 <- buildCoboundaryFromIncidenceRestrictions spec0 incidenceRestrictionValues
  matrix1 <- buildCoboundaryFromIncidenceRestrictions spec1 incidenceRestrictionValues
  buildTwoDifferentialComplex
    cmSourceBasis
    cmTargetBasis
    (linearBasisForSheafBasis lookupCellStalk stalkDimension . cmSourceBasis)
    (linearBasisForSheafBasis lookupCellStalk stalkDimension . cmTargetBasis)
    (materializeCoboundaryMatrixWithBases lookupCellStalk blockKernel)
    matrix0
    matrix1

prepareRankOneCoboundaryPlan ::
  Ord cell =>
  (cell -> stalk) ->
  (Restriction cell witness -> stalk -> stalk -> Int) ->
  CoboundarySpec cell ->
  RestrictionIndex cell witness ->
  Either (SheafOperatorBuildError cell) (RankOneCoboundaryPlan cell)
prepareRankOneCoboundaryPlan lookupCellStalk scalarCoefficient spec restrictions = do
  prepareRankOneCoboundaryPlanFromIncidenceRestrictions
    lookupCellStalk
    scalarCoefficient
    spec
    (incidenceRestrictions restrictions)

prepareRankOneCoboundaryPlanFromIncidenceRestrictions ::
  Ord cell =>
  (cell -> stalk) ->
  (Restriction cell witness -> stalk -> stalk -> Int) ->
  CoboundarySpec cell ->
  [Restriction cell witness] ->
  Either (SheafOperatorBuildError cell) (RankOneCoboundaryPlan cell)
prepareRankOneCoboundaryPlanFromIncidenceRestrictions lookupCellStalk scalarCoefficient spec incidenceRestrictionValues = do
  incidence <-
    rankOneIncidenceFromEntries
      (basisCardinality (csSourceBasis spec))
      (basisCardinality (csTargetBasis spec))
      (rankOneEntriesForSpecFromIncidenceRestrictions lookupCellStalk scalarCoefficient spec incidenceRestrictionValues)
  packedIncidence <- packedSparseOperatorFromBoundary incidence
  pure
    RankOneCoboundaryPlan
      { rocpDimension = csDimension spec,
        rocpSourceBasis = csSourceBasis spec,
        rocpTargetBasis = csTargetBasis spec,
        rocpIncidence = incidence,
        rocpPackedIncidence = packedIncidence
      }

materializeRankOneCoboundaryIncidence ::
  RankOneCoboundaryPlan cell ->
  BoundaryIncidence Int
materializeRankOneCoboundaryIncidence =
  rocpIncidence

rankOneIncidenceFromEntries ::
  Int ->
  Int ->
  [RankOneCoboundaryEntry] ->
  Either (SheafOperatorBuildError cell) (BoundaryIncidence Int)
rankOneIncidenceFromEntries sourceCardinalityValue targetCardinalityValue entries =
  liftBoundaryShape
    ( mkBoundaryIncidenceFromOrderedEntries
        (fromIntegral sourceCardinalityValue)
        (fromIntegral targetCardinalityValue)
        (fmap rankOneEntryToBoundaryEntry entries)
    )

materializeRankOneCoboundaryDifferential ::
  Ord cell =>
  RankOneCoboundaryPlan cell ->
  Either (SheafOperatorBuildError cell) (GradedOperator cell Int)
materializeRankOneCoboundaryDifferential plan = do
  sourceLinearBasis <- rankOneSourceLinearBasis plan
  targetLinearBasis <- rankOneTargetLinearBasis plan
  materializeRankOneCoboundaryDifferentialWithBases sourceLinearBasis targetLinearBasis plan

applyRankOneCoboundaryPlan ::
  RankOneCoboundaryPlan cell ->
  Map Int Int ->
  Map Int Int
applyRankOneCoboundaryPlan plan =
  boundaryIncidenceApply (rocpIncidence plan)

applyRankOneCoboundaryPlanDense ::
  RankOneCoboundaryPlan cell ->
  Unboxed.Vector Int ->
  Either (SheafOperatorBuildError cell) (Unboxed.Vector Int)
applyRankOneCoboundaryPlanDense plan =
  applyPackedSparseOperatorDenseAsSheafOperator OperatorSourceBasis (rocpPackedIncidence plan)

buildRankOneCoboundaryComplex ::
  Ord cell =>
  (cell -> stalk) ->
  (Restriction cell witness -> stalk -> stalk -> Int) ->
  CoboundarySpec cell ->
  CoboundarySpec cell ->
  RestrictionIndex cell witness ->
  Either (SheafOperatorBuildError cell) (GradedComplex cell Int)
buildRankOneCoboundaryComplex lookupCellStalk scalarCoefficient spec0 spec1 restrictions = do
  let incidenceRestrictionValues =
        incidenceRestrictions restrictions
  plan0 <-
    prepareRankOneCoboundaryPlanFromIncidenceRestrictions
      lookupCellStalk
      scalarCoefficient
      spec0
      incidenceRestrictionValues
  plan1 <-
    prepareRankOneCoboundaryPlanFromIncidenceRestrictions
      lookupCellStalk
      scalarCoefficient
      spec1
      incidenceRestrictionValues
  buildRankOneCoboundaryComplexFromPlans plan0 plan1

buildRankOneCoboundaryComplexFromPlans ::
  Ord cell =>
  RankOneCoboundaryPlan cell ->
  RankOneCoboundaryPlan cell ->
  Either (SheafOperatorBuildError cell) (GradedComplex cell Int)
buildRankOneCoboundaryComplexFromPlans =
  buildTwoDifferentialComplex
    rocpSourceBasis
    rocpTargetBasis
    rankOneSourceLinearBasis
    rankOneTargetLinearBasis
    materializeRankOneCoboundaryDifferentialWithBases

buildTwoDifferentialComplex ::
  (Eq basis, Eq cell) =>
  (plan -> basis) ->
  (plan -> basis) ->
  (plan -> Either (SheafOperatorBuildError cell) (LinearBasis cell)) ->
  (plan -> Either (SheafOperatorBuildError cell) (LinearBasis cell)) ->
  (LinearBasis cell -> LinearBasis cell -> plan -> Either (SheafOperatorBuildError cell) (GradedOperator cell Int)) ->
  plan ->
  plan ->
  Either (SheafOperatorBuildError cell) (GradedComplex cell Int)
buildTwoDifferentialComplex sourceBasisOf targetBasisOf sourceLinearBasisOf targetLinearBasisOf materializeDifferential plan0 plan1 = do
  sourceBasis0 <- sourceLinearBasisOf plan0
  middleTargetBasis <- targetLinearBasisOf plan0
  middleSourceBasis <-
    if targetBasisOf plan0 == sourceBasisOf plan1
      then Right middleTargetBasis
      else sourceLinearBasisOf plan1
  targetBasis1 <- targetLinearBasisOf plan1
  differential0 <- materializeDifferential sourceBasis0 middleTargetBasis plan0
  differential1 <- materializeDifferential middleSourceBasis targetBasis1 plan1
  mkGradedComplexFromList DegreeIncreasing [differential0, differential1]

materializeCoboundaryMatrixWithBases ::
  Ord cell =>
  (cell -> stalk) ->
  CoboundaryBlockKernel stalk ->
  LinearBasis cell ->
  LinearBasis cell ->
  CoboundaryMatrix cell witness ->
  Either (SheafOperatorBuildError cell) (GradedOperator cell Int)
materializeCoboundaryMatrixWithBases lookupCellStalk blockKernel sourceLinearBasis targetLinearBasis matrix =
  prepareCoboundaryAssemblyPlanWithBases
    lookupCellStalk
    sourceLinearBasis
    targetLinearBasis
    matrix
    >>= materializeCoboundaryAssemblyPlanWithKernel blockKernel

materializeCoboundaryIncidence ::
  Ord cell =>
  (cell -> stalk) ->
  (stalk -> Int) ->
  (stalk -> stalk -> BoundaryIncidence Int) ->
  CoboundaryMatrix cell witness ->
  Either (SheafOperatorBuildError cell) (BoundaryIncidence Int)
materializeCoboundaryIncidence lookupCellStalk stalkDimension coboundaryBlock =
  materializeCoboundaryIncidenceWithKernel
    lookupCellStalk
    stalkDimension
    (GeneralCoboundaryBlock coboundaryBlock)

materializeCoboundaryIncidenceWithKernel ::
  Ord cell =>
  (cell -> stalk) ->
  (stalk -> Int) ->
  CoboundaryBlockKernel stalk ->
  CoboundaryMatrix cell witness ->
  Either (SheafOperatorBuildError cell) (BoundaryIncidence Int)
materializeCoboundaryIncidenceWithKernel lookupCellStalk stalkDimension blockKernel matrix =
  prepareCoboundaryAssemblyPlan
      lookupCellStalk
      stalkDimension
      matrix
    >>= assembleCoboundaryIncidenceWithKernel blockKernel

prepareCoboundaryAssemblyPlan ::
  Ord cell =>
  (cell -> stalk) ->
  (stalk -> Int) ->
  CoboundaryMatrix cell witness ->
  Either (SheafOperatorBuildError cell) (CoboundaryAssemblyPlan cell stalk)
prepareCoboundaryAssemblyPlan lookupCellStalk stalkDimension matrix = do
  sourceLinearBasis <-
    linearBasisForSheafBasis
      lookupCellStalk
      stalkDimension
      (cmSourceBasis matrix)
  targetLinearBasis <-
    linearBasisForSheafBasis
      lookupCellStalk
      stalkDimension
      (cmTargetBasis matrix)
  prepareCoboundaryAssemblyPlanWithBases
    lookupCellStalk
    sourceLinearBasis
    targetLinearBasis
    matrix

materializeCoboundaryAssemblyPlan ::
  (stalk -> stalk -> BoundaryIncidence Int) ->
  CoboundaryAssemblyPlan cell stalk ->
  Either (SheafOperatorBuildError cell) (GradedOperator cell Int)
materializeCoboundaryAssemblyPlan coboundaryBlock =
  materializeCoboundaryAssemblyPlanWithKernel
    (GeneralCoboundaryBlock coboundaryBlock)

materializeCoboundaryAssemblyPlanWithKernel ::
  CoboundaryBlockKernel stalk ->
  CoboundaryAssemblyPlan cell stalk ->
  Either (SheafOperatorBuildError cell) (GradedOperator cell Int)
materializeCoboundaryAssemblyPlanWithKernel blockKernel assemblyPlan =
  prepareCoboundaryIncidencePlanWithKernel blockKernel assemblyPlan
    >>= materializeCoboundaryIncidencePlan

prepareCoboundaryIncidencePlan ::
  (stalk -> stalk -> BoundaryIncidence Int) ->
  CoboundaryAssemblyPlan cell stalk ->
  Either (SheafOperatorBuildError cell) (CoboundaryIncidencePlan cell)
prepareCoboundaryIncidencePlan coboundaryBlock =
  prepareCoboundaryIncidencePlanWithKernel (GeneralCoboundaryBlock coboundaryBlock)

prepareCoboundaryIncidencePlanWithKernel ::
  CoboundaryBlockKernel stalk ->
  CoboundaryAssemblyPlan cell stalk ->
  Either (SheafOperatorBuildError cell) (CoboundaryIncidencePlan cell)
prepareCoboundaryIncidencePlanWithKernel blockKernel assemblyPlan = do
  incidence <- assembleCoboundaryIncidenceWithKernel blockKernel assemblyPlan
  pure
    CoboundaryIncidencePlan
      { cipDimension = capDimension assemblyPlan,
        cipSourceLinearBasis = capSourceLinearBasis assemblyPlan,
        cipTargetLinearBasis = capTargetLinearBasis assemblyPlan,
        cipIncidence = incidence
      }

applyCoboundaryAssemblyPlanWithKernel ::
  CoboundaryBlockKernel stalk ->
  CoboundaryAssemblyPlan cell stalk ->
  Map Int Int ->
  Either (SheafOperatorBuildError cell) (Map Int Int)
applyCoboundaryAssemblyPlanWithKernel blockKernel assemblyPlan vectorValue =
  Map.filter (/= 0) . Map.fromListWith (+)
    <$> assemblyApplyTermsWithKernel blockKernel vectorValue (capEntries assemblyPlan)

applyCoboundaryIncidencePlan ::
  CoboundaryIncidencePlan cell ->
  Map Int Int ->
  Map Int Int
applyCoboundaryIncidencePlan incidencePlan =
  boundaryIncidenceApply (cipIncidence incidencePlan)

materializeCoboundaryIncidencePlan ::
  CoboundaryIncidencePlan cell ->
  Either (SheafOperatorBuildError cell) (GradedOperator cell Int)
materializeCoboundaryIncidencePlan incidencePlan =
  mkGradedOperator
    (cipDimension incidencePlan)
    (cipSourceLinearBasis incidencePlan)
    (cipTargetLinearBasis incidencePlan)
    (cipIncidence incidencePlan)

assembleCoboundaryIncidenceWithKernel ::
  CoboundaryBlockKernel stalk ->
  CoboundaryAssemblyPlan cell stalk ->
  Either (SheafOperatorBuildError cell) (BoundaryIncidence Int)
assembleCoboundaryIncidenceWithKernel blockKernel assemblyPlan = do
  flattenedEntries <-
    assemblyEntriesWithKernel blockKernel (capEntries assemblyPlan)
  let sourceLinearBasis =
        capSourceLinearBasis assemblyPlan
      targetLinearBasis =
        capTargetLinearBasis assemblyPlan
  liftBoundaryShape
    ( mkBoundaryIncidenceFromOrderedEntries
        (fromIntegral (linearBasisCardinality sourceLinearBasis))
        (fromIntegral (linearBasisCardinality targetLinearBasis))
        flattenedEntries
    )

materializeCoboundaryDifferential ::
  Ord cell =>
  (cell -> stalk) ->
  (stalk -> Int) ->
  (stalk -> stalk -> BoundaryIncidence Int) ->
  CoboundaryMatrix cell witness ->
  Either (SheafOperatorBuildError cell) (GradedOperator cell Int)
materializeCoboundaryDifferential lookupCellStalk stalkDimension coboundaryBlock =
  materializeCoboundaryDifferentialWithKernel
    lookupCellStalk
    stalkDimension
    (GeneralCoboundaryBlock coboundaryBlock)

materializeCoboundaryDifferentialWithKernel ::
  Ord cell =>
  (cell -> stalk) ->
  (stalk -> Int) ->
  CoboundaryBlockKernel stalk ->
  CoboundaryMatrix cell witness ->
  Either (SheafOperatorBuildError cell) (GradedOperator cell Int)
materializeCoboundaryDifferentialWithKernel lookupCellStalk stalkDimension blockKernel matrix = do
  sourceLinearBasis <-
    linearBasisForSheafBasis
      lookupCellStalk
      stalkDimension
      (cmSourceBasis matrix)
  targetLinearBasis <-
    linearBasisForSheafBasis
      lookupCellStalk
      stalkDimension
      (cmTargetBasis matrix)
  assemblyPlan <-
    prepareCoboundaryAssemblyPlanWithBases
      lookupCellStalk
      sourceLinearBasis
      targetLinearBasis
      matrix
  materializeCoboundaryAssemblyPlanWithKernel
    blockKernel
    assemblyPlan

linearBasisForSheafBasis ::
  Ord cell =>
  (cell -> stalk) ->
  (stalk -> Int) ->
  SheafBasis cell ->
  Either (SheafOperatorBuildError cell) (LinearBasis cell)
linearBasisForSheafBasis lookupCellStalk stalkDimension =
  mkLinearBasis (stalkDimension . lookupCellStalk)

prepareCoboundaryAssemblyPlanWithBases ::
  Ord cell =>
  (cell -> stalk) ->
  LinearBasis cell ->
  LinearBasis cell ->
  CoboundaryMatrix cell witness ->
  Either (SheafOperatorBuildError cell) (CoboundaryAssemblyPlan cell stalk)
prepareCoboundaryAssemblyPlanWithBases lookupCellStalk sourceLinearBasis targetLinearBasis matrix = do
  let sourceSlotResolver =
        linearBasisSlotResolver
          OperatorSourceBasis
          sourceLinearBasis
          (cmSourceBasis matrix)
          ceSourceCell
          ceSourceBasisSlot
      targetSlotResolver =
        linearBasisSlotResolver
          OperatorTargetBasis
          targetLinearBasis
          (cmTargetBasis matrix)
          ceTargetCell
          ceTargetBasisSlot
  preparedEntries <-
    traverse
      (entryToAssemblyEntry lookupCellStalk sourceSlotResolver targetSlotResolver)
      (cmEntries matrix)
  pure
    CoboundaryAssemblyPlan
      { capDimension = cmDimension matrix,
        capSourceLinearBasis = sourceLinearBasis,
        capTargetLinearBasis = targetLinearBasis,
        capEntries = preparedEntries
      }

rankOneEntriesForSpec ::
  Ord cell =>
  (cell -> stalk) ->
  (Restriction cell witness -> stalk -> stalk -> Int) ->
  CoboundarySpec cell ->
  RestrictionIndex cell witness ->
  [RankOneCoboundaryEntry]
rankOneEntriesForSpec lookupCellStalk scalarCoefficient spec restrictions =
  rankOneEntriesForSpecFromIncidenceRestrictions
    lookupCellStalk
    scalarCoefficient
    spec
    (incidenceRestrictions restrictions)

rankOneEntriesForSpecFromIncidenceRestrictions ::
  Ord cell =>
  (cell -> stalk) ->
  (Restriction cell witness -> stalk -> stalk -> Int) ->
  CoboundarySpec cell ->
  [Restriction cell witness] ->
  [RankOneCoboundaryEntry]
rankOneEntriesForSpecFromIncidenceRestrictions lookupCellStalk scalarCoefficient spec incidenceRestrictionValues =
  if basisCardinality (csSourceBasis spec) == 0 || basisCardinality (csTargetBasis spec) == 0
    then []
    else
      mapMaybe
        ( \(restriction, sourceOffsetValue, targetOffsetValue) ->
            rankOneEntryFromRestrictionAtOffsets
              lookupCellStalk
              scalarCoefficient
              (sourceOffsetValue, targetOffsetValue)
              restriction
        )
        (mapMaybe (restrictionSlotsInSpec spec) incidenceRestrictionValues)

rankOneEntryFromRestriction ::
  Ord cell =>
  (cell -> stalk) ->
  (Restriction cell witness -> stalk -> stalk -> Int) ->
  CoboundarySpec cell ->
  Restriction cell witness ->
  Maybe RankOneCoboundaryEntry
rankOneEntryFromRestriction lookupCellStalk scalarCoefficient spec restriction = do
  offsets <-
    restrictionOffsetsInSpec spec restriction
  rankOneEntryFromRestrictionAtOffsets
    lookupCellStalk
    scalarCoefficient
    offsets
    restriction

rankOneEntryFromRestrictionAtOffsets ::
  (cell -> stalk) ->
  (Restriction cell witness -> stalk -> stalk -> Int) ->
  (Int, Int) ->
  Restriction cell witness ->
  Maybe RankOneCoboundaryEntry
rankOneEntryFromRestrictionAtOffsets lookupCellStalk scalarCoefficient (sourceOffsetValue, targetOffsetValue) restriction = do
  let sourceStalk = lookupCellStalk (rTarget restriction)
      targetStalk = lookupCellStalk (rSource restriction)
      coefficientValue =
        fromMaybe 0 (restrictionKindCoefficient (rKind restriction))
          * scalarCoefficient restriction sourceStalk targetStalk
  guard (coefficientValue /= 0)
  pure
    RankOneCoboundaryEntry
      { rankOneCoboundaryEntrySourceOffset = sourceOffsetValue,
        rankOneCoboundaryEntryTargetOffset = targetOffsetValue,
        rankOneCoboundaryEntryCoefficient = coefficientValue
      }

rankOneEntryToBoundaryEntry :: RankOneCoboundaryEntry -> BoundaryEntry Int
rankOneEntryToBoundaryEntry entry =
  mkBoundaryEntryFromInts
    (rankOneCoboundaryEntrySourceOffset entry)
    (rankOneCoboundaryEntryTargetOffset entry)
    (rankOneCoboundaryEntryCoefficient entry)

rankOneSourceLinearBasis ::
  Ord cell =>
  RankOneCoboundaryPlan cell ->
  Either (SheafOperatorBuildError cell) (LinearBasis cell)
rankOneSourceLinearBasis =
  rankOneLinearBasis . rocpSourceBasis

rankOneTargetLinearBasis ::
  Ord cell =>
  RankOneCoboundaryPlan cell ->
  Either (SheafOperatorBuildError cell) (LinearBasis cell)
rankOneTargetLinearBasis =
  rankOneLinearBasis . rocpTargetBasis

rankOneLinearBasis ::
  Ord cell =>
  SheafBasis cell ->
  Either (SheafOperatorBuildError cell) (LinearBasis cell)
rankOneLinearBasis =
  mkLinearBasis (const 1)

materializeRankOneCoboundaryDifferentialWithBases ::
  LinearBasis cell ->
  LinearBasis cell ->
  RankOneCoboundaryPlan cell ->
  Either (SheafOperatorBuildError cell) (GradedOperator cell Int)
materializeRankOneCoboundaryDifferentialWithBases sourceLinearBasis targetLinearBasis plan = do
  mkGradedOperator
    (rocpDimension plan)
    sourceLinearBasis
    targetLinearBasis
    (materializeRankOneCoboundaryIncidence plan)

coboundaryIncidenceToDouble :: BoundaryIncidence Int -> BoundaryIncidence Double
coboundaryIncidenceToDouble =
  mapBoundaryCoefficients fromIntegral

applyCoboundary ::
  Ord cell =>
  StalkAlgebra witness stalk mismatch repairObstruction ->
  CoboundaryMatrix cell witness ->
  SheafModel cell modelWitness ->
  TotalSectionStore cell stalk ->
  Either (SheafOperatorBuildError cell) (Map cell [CoboundaryContribution stalk])
applyCoboundary stalkAlgebra matrix model section =
  Map.fromListWith (<>)
    <$> traverse contributionForEntry (cmEntries matrix)
  where
    contributionForEntry entry = do
      sourceStalk <-
        first
          (OperatorSectionLookupFailure (ceSourceCell entry))
          (totalStalkAt model (ceSourceCell entry) section)
      pure
        ( ceTargetCell entry,
          [ CoboundaryContribution
              { contributionOrientation = ceOrientation entry,
                contributionValue = restrictStalk stalkAlgebra (ceWitness entry) sourceStalk
              }
          ]
        )

collapseCoboundary :: CoboundaryReducer stalk -> Map cell [CoboundaryContribution stalk] -> Map cell stalk
collapseCoboundary reducer =
  Map.map (runCoboundaryReducer reducer)

checkCoboundaryNilpotence :: GradedComplex cell Int -> Bool
checkCoboundaryNilpotence complex =
  all differentialComposesToZero (Map.toList differentials)
  where
    differentials =
      gradedOperatorsByDegree complex

    differentialComposesToZero :: (HomologicalDegree, GradedOperator cell Int) -> Bool
    differentialComposesToZero (rightDegree, rightDifferential) =
      maybe
        True
        (compositionIsZero rightDifferential)
        (Map.lookup (incrementDegree rightDegree) differentials)

    compositionIsZero :: GradedOperator rightCell Int -> GradedOperator leftCell Int -> Bool
    compositionIsZero rightDifferential leftDifferential =
      either
        (const False)
        (all ((== 0) . boundaryCoefficient) . boundaryEntries)
        ( composeBoundaryIncidence
            (gradedOperatorIncidence leftDifferential)
            (gradedOperatorIncidence rightDifferential)
        )

mkCoboundaryEntry ::
  Restriction cell witness ->
  Either (SheafOperatorBuildError cell) (CoboundaryEntry cell witness)
mkCoboundaryEntry restriction =
  mkCoboundaryEntryAtSlotsValue restriction Nothing Nothing

mkCoboundaryEntryAtSlots ::
  Restriction cell witness ->
  Int ->
  Int ->
  Either (SheafOperatorBuildError cell) (CoboundaryEntry cell witness)
mkCoboundaryEntryAtSlots restriction sourceSlot targetSlot =
  mkCoboundaryEntryAtSlotsValue restriction (Just sourceSlot) (Just targetSlot)

mkCoboundaryEntryAtSlotsValue ::
  Restriction cell witness ->
  Maybe Int ->
  Maybe Int ->
  Either (SheafOperatorBuildError cell) (CoboundaryEntry cell witness)
mkCoboundaryEntryAtSlotsValue restriction sourceSlot targetSlot =
  case rKind restriction of
    IncidenceRestriction _coefficient ->
      Right
        CoboundaryEntry
          { ceRestriction = restriction,
            ceSourceBasisSlot = sourceSlot,
            ceTargetBasisSlot = targetSlot
          }
    PortalRestriction ->
      Left (OperatorExpectedIncidenceRestriction (rSource restriction) (rTarget restriction))

entryToAssemblyEntry ::
  (cell -> stalk) ->
  (CoboundaryEntry cell witness -> Either (SheafOperatorBuildError cell) (Int, Int)) ->
  (CoboundaryEntry cell witness -> Either (SheafOperatorBuildError cell) (Int, Int)) ->
  CoboundaryEntry cell witness ->
  Either (SheafOperatorBuildError cell) (CoboundaryAssemblyEntry stalk)
entryToAssemblyEntry lookupCellStalk sourceSlotResolver targetSlotResolver entry = do
  (sourceOffsetValue, sourceDimensionValue) <-
    sourceSlotResolver entry
  (targetOffsetValue, targetDimensionValue) <-
    targetSlotResolver entry
  pure
    CoboundaryAssemblyEntry
      { caeSourceOffset = sourceOffsetValue,
        caeSourceDimension = sourceDimensionValue,
        caeTargetOffset = targetOffsetValue,
        caeTargetDimension = targetDimensionValue,
        caeOrientation = ceOrientation entry,
        caeSourceStalk = lookupCellStalk (ceSourceCell entry),
        caeTargetStalk = lookupCellStalk (ceTargetCell entry)
      }

linearBasisSlotResolver ::
  Ord cell =>
  OperatorBasisRole ->
  LinearBasis cell ->
  SheafBasis cell ->
  (CoboundaryEntry cell witness -> cell) ->
  (CoboundaryEntry cell witness -> Maybe Int) ->
  CoboundaryEntry cell witness ->
  Either (SheafOperatorBuildError cell) (Int, Int)
linearBasisSlotResolver role linearBasis sheafBasis cellOf slotOf =
  if linearBasisCells linearBasis == basisCells sheafBasis
    then slotResolvedBasisCellSlot
    else lookupBasisCellSlot
  where
    lookupBasisCellSlot entry =
      linearBasisCellSlotOrError role linearBasis (cellOf entry)

    slotResolvedBasisCellSlot entry =
      maybe
        (lookupBasisCellSlot entry)
        ( \slotIndex ->
            maybe
              (lookupBasisCellSlot entry)
              Right
              (linearBasisSlotAtIndex slotIndex linearBasis)
        )
        (slotOf entry)

assemblyEntryToKernelEntries ::
  CoboundaryBlockKernel stalk ->
  CoboundaryAssemblyEntry stalk ->
  Either (SheafOperatorBuildError cell) [BoundaryEntry Int]
assemblyEntryToKernelEntries =
  eliminateCoboundaryBlockKernel
    assemblyEntryToBlockEntries
    assemblyEntryToDimensionBlockEntries
    assemblyEntryToScalarUnitEntry

assemblyEntriesWithKernel ::
  CoboundaryBlockKernel stalk ->
  [CoboundaryAssemblyEntry stalk] ->
  Either (SheafOperatorBuildError cell) [BoundaryEntry Int]
assemblyEntriesWithKernel blockKernel assemblyEntries =
  case blockKernel of
    DimensionCoboundaryBlock blockForDimensions ->
      cachedDimensionBlockValues
        globalBoundaryEntriesFromLocalBlock
        blockForDimensions
        assemblyEntries
    _ ->
      concat <$> traverse (assemblyEntryToKernelEntries blockKernel) assemblyEntries

assemblyEntryToScalarUnitEntry ::
  Int ->
  CoboundaryAssemblyEntry stalk ->
  Either (SheafOperatorBuildError cell) [BoundaryEntry Int]
assemblyEntryToScalarUnitEntry scalar assemblyEntry = do
  validateScalarUnitBlockShape assemblyEntry
  let coefficientValue =
        caeOrientation assemblyEntry * scalar
  pure
    [ mkBoundaryEntryFromInts
        (caeSourceOffset assemblyEntry)
        (caeTargetOffset assemblyEntry)
        coefficientValue
    | coefficientValue /= 0
    ]

validateScalarUnitBlockShape ::
  CoboundaryAssemblyEntry stalk ->
  Either (SheafOperatorBuildError cell) ()
validateScalarUnitBlockShape assemblyEntry =
  if caeSourceDimension assemblyEntry == 1
    && caeTargetDimension assemblyEntry == 1
    then Right ()
    else
      Left
        ( OperatorBoundaryShapeError
            ( BoundaryIncidenceBlockShapeMismatch
                (caeSourceDimension assemblyEntry)
                (caeTargetDimension assemblyEntry)
                1
                1
            )
        )

assemblyEntryToKernelApplyTerms ::
  CoboundaryBlockKernel stalk ->
  Map Int Int ->
  CoboundaryAssemblyEntry stalk ->
  Either (SheafOperatorBuildError cell) [(Int, Int)]
assemblyEntryToKernelApplyTerms blockKernel vectorValue =
  eliminateCoboundaryBlockKernel
    (`assemblyEntryToGeneralApplyTerms` vectorValue)
    (`assemblyEntryToDimensionApplyTerms` vectorValue)
    (`assemblyEntryToScalarUnitApplyTerms` vectorValue)
    blockKernel

assemblyApplyTermsWithKernel ::
  CoboundaryBlockKernel stalk ->
  Map Int Int ->
  [CoboundaryAssemblyEntry stalk] ->
  Either (SheafOperatorBuildError cell) [(Int, Int)]
assemblyApplyTermsWithKernel blockKernel vectorValue assemblyEntries =
  case blockKernel of
    DimensionCoboundaryBlock blockForDimensions ->
      cachedDimensionBlockValues
        (applyTermsFromLocalBlock vectorValue)
        blockForDimensions
        assemblyEntries
    _ ->
      concat <$> traverse (assemblyEntryToKernelApplyTerms blockKernel vectorValue) assemblyEntries

eliminateCoboundaryBlockKernel ::
  ((stalk -> stalk -> BoundaryIncidence Int) -> CoboundaryAssemblyEntry stalk -> result) ->
  ((Int -> Int -> BoundaryIncidence Int) -> CoboundaryAssemblyEntry stalk -> result) ->
  (Int -> CoboundaryAssemblyEntry stalk -> result) ->
  CoboundaryBlockKernel stalk ->
  CoboundaryAssemblyEntry stalk ->
  result
eliminateCoboundaryBlockKernel general dimension scalarUnit blockKernel assemblyEntry =
  case blockKernel of
    GeneralCoboundaryBlock coboundaryBlock ->
      general coboundaryBlock assemblyEntry
    DimensionCoboundaryBlock blockForDimensions ->
      dimension blockForDimensions assemblyEntry
    UnitCoboundaryBlock ->
      scalarUnit 1 assemblyEntry
    ScalarCoboundaryBlock scalarOf ->
      scalarUnit
        (scalarOf (caeSourceStalk assemblyEntry) (caeTargetStalk assemblyEntry))
        assemblyEntry

assemblyEntryToGeneralApplyTerms ::
  (stalk -> stalk -> BoundaryIncidence Int) ->
  Map Int Int ->
  CoboundaryAssemblyEntry stalk ->
  Either (SheafOperatorBuildError cell) [(Int, Int)]
assemblyEntryToGeneralApplyTerms coboundaryBlock vectorValue assemblyEntry = do
  localBlock <- assemblyEntryToValidatedBlock coboundaryBlock assemblyEntry
  pure (applyTermsFromLocalBlock vectorValue assemblyEntry localBlock)

assemblyEntryToDimensionApplyTerms ::
  (Int -> Int -> BoundaryIncidence Int) ->
  Map Int Int ->
  CoboundaryAssemblyEntry stalk ->
  Either (SheafOperatorBuildError cell) [(Int, Int)]
assemblyEntryToDimensionApplyTerms blockForDimensions vectorValue assemblyEntry = do
  localBlock <- dimensionBoundaryBlock blockForDimensions assemblyEntry
  pure (applyTermsFromLocalBlock vectorValue assemblyEntry localBlock)

assemblyEntryToScalarUnitApplyTerms ::
  Int ->
  Map Int Int ->
  CoboundaryAssemblyEntry stalk ->
  Either (SheafOperatorBuildError cell) [(Int, Int)]
assemblyEntryToScalarUnitApplyTerms scalar vectorValue assemblyEntry = do
  validateScalarUnitBlockShape assemblyEntry
  let coefficientValue =
        caeOrientation assemblyEntry * scalar
      sourceValue =
        Map.findWithDefault 0 (caeSourceOffset assemblyEntry) vectorValue
      targetValue =
        coefficientValue * sourceValue
  pure
    [ (caeTargetOffset assemblyEntry, targetValue)
    | targetValue /= 0
    ]

assemblyEntryToBlockEntries ::
  (stalk -> stalk -> BoundaryIncidence Int) ->
  CoboundaryAssemblyEntry stalk ->
  Either (SheafOperatorBuildError cell) [BoundaryEntry Int]
assemblyEntryToBlockEntries coboundaryBlock assemblyEntry = do
  localBlock <- assemblyEntryToValidatedBlock coboundaryBlock assemblyEntry
  pure (globalBoundaryEntriesFromLocalBlock assemblyEntry localBlock)

assemblyEntryToDimensionBlockEntries ::
  (Int -> Int -> BoundaryIncidence Int) ->
  CoboundaryAssemblyEntry stalk ->
  Either (SheafOperatorBuildError cell) [BoundaryEntry Int]
assemblyEntryToDimensionBlockEntries blockForDimensions assemblyEntry = do
  localBlock <- dimensionBoundaryBlock blockForDimensions assemblyEntry
  pure (globalBoundaryEntriesFromLocalBlock assemblyEntry localBlock)

assemblyEntryToValidatedBlock ::
  (stalk -> stalk -> BoundaryIncidence Int) ->
  CoboundaryAssemblyEntry stalk ->
  Either (SheafOperatorBuildError cell) (BoundaryIncidence Int)
assemblyEntryToValidatedBlock coboundaryBlock assemblyEntry = do
  let localBlock =
        orientBoundaryBlock
          (caeOrientation assemblyEntry)
          (coboundaryBlock (caeSourceStalk assemblyEntry) (caeTargetStalk assemblyEntry))
  validateBoundaryBlockShape
    (caeSourceDimension assemblyEntry)
    (caeTargetDimension assemblyEntry)
    localBlock
  pure localBlock

dimensionBoundaryBlock ::
  (Int -> Int -> BoundaryIncidence Int) ->
  CoboundaryAssemblyEntry stalk ->
  Either (SheafOperatorBuildError cell) (BoundaryIncidence Int)
dimensionBoundaryBlock blockForDimensions assemblyEntry =
  validatedDimensionBoundaryBlock
    blockForDimensions
    (caeSourceDimension assemblyEntry)
    (caeTargetDimension assemblyEntry)
    (caeOrientation assemblyEntry)

validatedDimensionBoundaryBlock ::
  (Int -> Int -> BoundaryIncidence Int) ->
  Int ->
  Int ->
  Int ->
  Either (SheafOperatorBuildError cell) (BoundaryIncidence Int)
validatedDimensionBoundaryBlock blockForDimensions sourceDimensionValue targetDimensionValue orientationValue = do
  let localBlock =
        orientBoundaryBlock
          orientationValue
          (blockForDimensions sourceDimensionValue targetDimensionValue)
  validateBoundaryBlockShape
    sourceDimensionValue
    targetDimensionValue
    localBlock
  pure localBlock

cachedDimensionBlockValues ::
  (CoboundaryAssemblyEntry stalk -> BoundaryIncidence Int -> [value]) ->
  (Int -> Int -> BoundaryIncidence Int) ->
  [CoboundaryAssemblyEntry stalk] ->
  Either (SheafOperatorBuildError cell) [value]
cachedDimensionBlockValues valuesFromLocalBlock blockForDimensions assemblyEntries =
  concat . reverse . snd
    <$> foldM
      appendDimensionBlockValues
      (Map.empty, [])
      assemblyEntries
  where
    appendDimensionBlockValues (cache, reversedValueChunks) assemblyEntry = do
      (nextCache, localBlock) <-
        cachedDimensionBoundaryBlock blockForDimensions cache assemblyEntry
      pure
        ( nextCache,
          valuesFromLocalBlock assemblyEntry localBlock : reversedValueChunks
        )

cachedDimensionBoundaryBlock ::
  (Int -> Int -> BoundaryIncidence Int) ->
  Map DimensionBlockCacheKey (BoundaryIncidence Int) ->
  CoboundaryAssemblyEntry stalk ->
  Either
    (SheafOperatorBuildError cell)
    (Map DimensionBlockCacheKey (BoundaryIncidence Int), BoundaryIncidence Int)
cachedDimensionBoundaryBlock blockForDimensions cache assemblyEntry =
  maybe
    (insertDimensionBoundaryBlock blockForDimensions cache assemblyEntry key)
    (\localBlock -> Right (cache, localBlock))
    (Map.lookup key cache)
  where
    key =
      DimensionBlockCacheKey
        { dbckSourceDimension = caeSourceDimension assemblyEntry,
          dbckTargetDimension = caeTargetDimension assemblyEntry,
          dbckOrientation = caeOrientation assemblyEntry
        }

insertDimensionBoundaryBlock ::
  (Int -> Int -> BoundaryIncidence Int) ->
  Map DimensionBlockCacheKey (BoundaryIncidence Int) ->
  CoboundaryAssemblyEntry stalk ->
  DimensionBlockCacheKey ->
  Either
    (SheafOperatorBuildError cell)
    (Map DimensionBlockCacheKey (BoundaryIncidence Int), BoundaryIncidence Int)
insertDimensionBoundaryBlock blockForDimensions cache assemblyEntry key = do
  localBlock <- dimensionBoundaryBlock blockForDimensions assemblyEntry
  pure (Map.insert key localBlock cache, localBlock)

globalBoundaryEntriesFromLocalBlock ::
  CoboundaryAssemblyEntry stalk ->
  BoundaryIncidence Int ->
  [BoundaryEntry Int]
globalBoundaryEntriesFromLocalBlock assemblyEntry localBlock =
  boundaryEntries localBlock
    & fmap
      ( \localEntry ->
          mkBoundaryEntryFromInts
            (caeSourceOffset assemblyEntry + sourceIndex localEntry)
            (caeTargetOffset assemblyEntry + targetIndex localEntry)
            (boundaryCoefficient localEntry)
      )

applyTermsFromLocalBlock ::
  Map Int Int ->
  CoboundaryAssemblyEntry stalk ->
  BoundaryIncidence Int ->
  [(Int, Int)]
applyTermsFromLocalBlock vectorValue assemblyEntry localBlock =
  [ (targetOffsetValue, targetValue)
  | localEntry <- boundaryEntries localBlock,
    let sourceOffsetValue = caeSourceOffset assemblyEntry + sourceIndex localEntry
        targetOffsetValue = caeTargetOffset assemblyEntry + targetIndex localEntry
        sourceValue = Map.findWithDefault 0 sourceOffsetValue vectorValue
        targetValue = boundaryCoefficient localEntry * sourceValue,
    targetValue /= 0
  ]

orientBoundaryBlock :: Int -> BoundaryIncidence Int -> BoundaryIncidence Int
orientBoundaryBlock scalar incidence
  | scalar == 1 = incidence
  | otherwise = scaleBoundaryIncidence scalar incidence
