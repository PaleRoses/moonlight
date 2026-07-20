{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Solve
  ( BiomechanicalElasticSolve (..),
    solveBiomechanicalElasticSystemDetailed,
    solveBiomechanicalElasticSystem,
  )
where

import Data.Kind (Type)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Vector.Unboxed qualified as U
import Moonlight.Core (averageOf, maximumOf, minimumOf, pairwise)
import Moonlight.Core qualified as Aggregate
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Core
  ( BiomechanicalBlueprint (..),
    BiomechanicalBoneConstraint (..),
    BiomechanicalElasticSpectralSignature (..),
    BiomechanicalEvidence (..),
    BiomechanicalStructuralKind (..),
    BiomechanicalSpectralSignature (..),
    BiomechanicalSite,
    BiomechanicalSolveFailure (..)
  )
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Policy
  ( BiomechanicalAnchorFidelityEnergy (..),
    BiomechanicalElasticStrainEnergy (..),
    BiomechanicalRoundLimit (..),
    BiomechanicalStructuralCoherenceEnergy (..),
    BiomechanicalSolvePolicy (..),
    BiomechanicalSpectralPolicy (..),
    BiomechanicalTolerance (..),
    BiomechanicalVolumetricPreservationEnergy (..)
  )
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Operator
  ( SparseNormalEquation (..),
    SparseNormalSystem (..),
    assembleSparseNormalSystem,
    solveSparseNormalSystemPCGWithFamily,
  )
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Operator qualified as Operator
import Moonlight.LinAlg.Krylov
  ( PositiveCount,
    SpectrumEnd (..),
    defaultLanczosConfig,
    mkPositiveCount,
  )
import Moonlight.LinAlg.Dense.Decomposition
  ( symmetricEigenPairs,
  )
import Moonlight.LinAlg.Operator
  ( LinearOperator,
    OperatorSymmetry (..),
    selfAdjointCSRLinearOperator,
  )
import Moonlight.LinAlg.Spectral
  ( Eigenpairs,
    EigenRequest (..),
    EigenSolveConfig,
    defaultEigenSolveConfig,
    eigenpairValues,
    eigenpairVectorAt,
    solveEigenRequest,
    withEigenFallbackInitialVector,
    withEigenFallbackLanczosConfig,
  )
import Moonlight.LinAlg.Sparse
  ( SparseCSR,
    csrCols,
    csrColumnIndicesVector,
    csrRows,
    csrRowOffsetsVector,
    csrValuesVector,
  )
import Moonlight.LinAlg.Geometry
  ( Vec3 (..),
    normalizeVec3Safe,
    scaleVec3,
    subVec3,
    vec3FromList,
    vec3Zero,
  )

type BiomechanicalElasticSolve :: Type
data BiomechanicalElasticSolve = BiomechanicalElasticSolve
  { besSolvedPositions :: Map BiomechanicalSite Vec3,
    besAnchorFidelityEnergy :: Double,
    besElasticStrainEnergy :: Double,
    besStructuralCoherenceEnergy :: Double,
    besVolumetricPreservationEnergy :: Double,
    besSpectralSignature :: BiomechanicalSpectralSignature,
    besObjective :: Double
  }

solveBiomechanicalElasticSystem ::
  BiomechanicalBlueprint ->
  BiomechanicalEvidence ->
  Map BiomechanicalSite Vec3 ->
  Maybe BiomechanicalElasticSolve
solveBiomechanicalElasticSystem blueprint evidence anchoredJointPositions =
  either (const Nothing) Just (solveBiomechanicalElasticSystemDetailed blueprint evidence anchoredJointPositions)

solveBiomechanicalElasticSystemDetailed ::
  BiomechanicalBlueprint ->
  BiomechanicalEvidence ->
  Map BiomechanicalSite Vec3 ->
  Either [BiomechanicalSolveFailure] BiomechanicalElasticSolve
solveBiomechanicalElasticSystemDetailed blueprint evidence anchoredJointPositions =
  let solvedSites = bmeOrderedJointSites evidence <> bmeOrderedStructuralSites evidence
      siteIndexBySite = Map.fromList (zip solvedSites [0 :: Int ..])
      anchoredSitePositions = Map.union anchoredJointPositions (bmeStructuralAnchorBySite evidence)
      supportWeights = jointSupportWeights evidence
      solvePolicy = bmbSolvePolicy blueprint
      equations =
        anchorEquations solvePolicy siteIndexBySite supportWeights anchoredJointPositions
          <> targetEquations solvePolicy siteIndexBySite supportWeights (bmeEffectorSite evidence) (bmbTarget blueprint)
          <> boneEquations solvePolicy siteIndexBySite anchoredJointPositions evidence
          <> structuralAnchorEquations solvePolicy siteIndexBySite evidence
          <> structuralEquations solvePolicy siteIndexBySite anchoredJointPositions evidence
          <> volumetricEquations solvePolicy siteIndexBySite anchoredJointPositions evidence
      initialGuess = sitePositionVector solvedSites anchoredSitePositions
      toleranceValue = unBiomechanicalTolerance (bmbTolerance blueprint)
      iterationLimit = max 1 (unBiomechanicalRoundLimit (bmbRoundLimit blueprint))
   in do
        normalSystem <- assembleNormalSystem solvePolicy (length solvedSites) equations
        spectralSignature <- spectralSignatureOfNormalSystem (bmbSpectralPolicy blueprint) evidence normalSystem
        if elasticSpectralAdmissible (bmbSpectralPolicy blueprint) spectralSignature
          then
            case solveSparseNormalSystemPCGWithFamily (bslpPreconditionerFamily solvePolicy) iterationLimit toleranceValue normalSystem initialGuess of
              Just solutionVector -> do
                solvedPositions <- decodeSitePositions solvedSites solutionVector
                let objectiveBreakdown = elasticObjectiveBreakdownOf equations solutionVector
                Right
                  BiomechanicalElasticSolve
                    { besSolvedPositions = solvedPositions,
                      besAnchorFidelityEnergy = eobAnchorFidelity objectiveBreakdown,
                      besElasticStrainEnergy = eobElasticStrain objectiveBreakdown,
                      besStructuralCoherenceEnergy = eobStructuralCoherence objectiveBreakdown,
                      besVolumetricPreservationEnergy = eobVolumetricPreservation objectiveBreakdown,
                      besSpectralSignature = spectralSignature,
                      besObjective = eobTotal objectiveBreakdown
                    }
              Nothing ->
                Left [BiomechanicalElasticSystemDidNotConverge]
          else Left [BiomechanicalElasticSpectralPolicyViolation spectralSignature]

type ElasticEquation :: Type
type ElasticEquation = SparseNormalEquation ElasticEnergyComponent

type ElasticEnergyComponent :: Type
data ElasticEnergyComponent
  = AnchorFidelityElasticEnergy
  | ElasticStrainElasticEnergy
  | StructuralCoherenceElasticEnergy
  | VolumetricPreservationElasticEnergy
  deriving stock (Eq, Ord, Show)

type ElasticObjectiveBreakdown :: Type
data ElasticObjectiveBreakdown = ElasticObjectiveBreakdown
  { eobAnchorFidelity :: Double,
    eobElasticStrain :: Double,
    eobStructuralCoherence :: Double,
    eobVolumetricPreservation :: Double,
    eobTotal :: Double
  }

type ElasticNormalSystem :: Type
type ElasticNormalSystem = SparseNormalSystem

anchorEquations :: BiomechanicalSolvePolicy -> Map BiomechanicalSite Int -> Map BiomechanicalSite Double -> Map BiomechanicalSite Vec3 -> [ElasticEquation]
anchorEquations solvePolicy siteIndexBySite supportWeights anchoredPositions =
  Map.toList anchoredPositions >>= \(site, anchorPosition) ->
    anchorFidelityEquations
      siteIndexBySite
      (bafeJointAnchorWeight (bslpAnchorFidelity solvePolicy) * (1.0 + Map.findWithDefault 0.0 site supportWeights))
      site
      anchorPosition

targetEquations :: BiomechanicalSolvePolicy -> Map BiomechanicalSite Int -> Map BiomechanicalSite Double -> BiomechanicalSite -> Vec3 -> [ElasticEquation]
targetEquations solvePolicy siteIndexBySite supportWeights effectorSite targetPosition =
  anchorFidelityEquations
    siteIndexBySite
    (bafeEffectorTargetWeight (bslpAnchorFidelity solvePolicy) * (1.0 + Map.findWithDefault 0.0 effectorSite supportWeights))
    effectorSite
    targetPosition

anchorFidelityEquations :: Map BiomechanicalSite Int -> Double -> BiomechanicalSite -> Vec3 -> [ElasticEquation]
anchorFidelityEquations siteIndexBySite weight site targetPosition =
  case Map.lookup site siteIndexBySite of
    Just siteIndex ->
      axisEquations AnchorFidelityElasticEnergy weight (siteRows (Map.size siteIndexBySite) siteIndex) targetPosition
    Nothing ->
      []

boneEquations :: BiomechanicalSolvePolicy -> Map BiomechanicalSite Int -> Map BiomechanicalSite Vec3 -> BiomechanicalEvidence -> [ElasticEquation]
boneEquations solvePolicy siteIndexBySite anchoredJointPositions evidence =
  Map.toList (bmeBoneEndpointsBySite evidence)
    >>= \(boneSite, (sourceSite, targetSite)) ->
      case
        ( Map.lookup boneSite (bmeBoneConstraintBySite evidence),
          Map.lookup sourceSite siteIndexBySite,
          Map.lookup targetSite siteIndexBySite,
          Map.lookup sourceSite anchoredJointPositions,
          Map.lookup targetSite anchoredJointPositions
        ) of
        (Just constraintValue, Just sourceIndex, Just targetIndex, Just sourceAnchor, Just targetAnchor) ->
          elasticStrainEquations
            (beseBoneWeight (bslpElasticStrain solvePolicy) * biomechanicalBoneStiffness constraintValue)
            (differenceRows (Map.size siteIndexBySite) sourceIndex targetIndex)
            (anchoredRestVector constraintValue sourceAnchor targetAnchor)
        _ ->
          []

anchoredRestVector :: BiomechanicalBoneConstraint -> Vec3 -> Vec3 -> Vec3
anchoredRestVector constraintValue sourceAnchor targetAnchor =
  scaleVec3
    (biomechanicalBoneRestLength constraintValue)
    (normalizeVec3Safe (subVec3 targetAnchor sourceAnchor))

jointSupportWeights :: BiomechanicalEvidence -> Map BiomechanicalSite Double
jointSupportWeights evidence =
  foldr
    (\(boneSite, (sourceSite, targetSite)) supportWeights ->
        case Map.lookup boneSite (bmeBoneConstraintBySite evidence) of
          Just constraintValue ->
            Map.insertWith (+) sourceSite (biomechanicalBoneStiffness constraintValue)
              (Map.insertWith (+) targetSite (biomechanicalBoneStiffness constraintValue) supportWeights)
          Nothing ->
            supportWeights
    )
    (Map.fromList (fmap (, 0.0) (bmeOrderedJointSites evidence)))
    (Map.toList (bmeBoneEndpointsBySite evidence))

structuralAnchorEquations :: BiomechanicalSolvePolicy -> Map BiomechanicalSite Int -> BiomechanicalEvidence -> [ElasticEquation]
structuralAnchorEquations solvePolicy siteIndexBySite evidence =
  Map.toList (bmeStructuralAnchorBySite evidence)
    >>= \(structuralSite, structuralAnchor) ->
      case (Map.lookup structuralSite siteIndexBySite, Map.lookup structuralSite (bmeStructuralKindBySite evidence)) of
        (Just structuralIndex, Just structuralKind) ->
          axisEquations
            (structuralComponent structuralKind)
            (structuralWeight solvePolicy structuralKind)
            (siteRows (Map.size siteIndexBySite) structuralIndex)
            structuralAnchor
        _ ->
          []

structuralEquations :: BiomechanicalSolvePolicy -> Map BiomechanicalSite Int -> Map BiomechanicalSite Vec3 -> BiomechanicalEvidence -> [ElasticEquation]
structuralEquations solvePolicy siteIndexBySite anchoredJointPositions evidence =
  Map.toList (bmeStructuralMembersBySite evidence)
    >>= \(structuralSite, memberSites) ->
      case (Map.lookup structuralSite siteIndexBySite, Map.lookup structuralSite (bmeStructuralAnchorBySite evidence), Map.lookup structuralSite (bmeStructuralKindBySite evidence)) of
        (Just structuralIndex, Just structuralAnchor, Just structuralKind) ->
          memberSites
            >>= \memberSite ->
              case (Map.lookup memberSite siteIndexBySite, Map.lookup memberSite anchoredJointPositions) of
                (Just memberIndex, Just memberAnchor) ->
                  axisEquations
                    (structuralComponent structuralKind)
                    (structuralWeight solvePolicy structuralKind)
                    (differenceRows (Map.size siteIndexBySite) structuralIndex memberIndex)
                    (subVec3 memberAnchor structuralAnchor)
                _ ->
                  []
        _ ->
          []

volumetricEquations :: BiomechanicalSolvePolicy -> Map BiomechanicalSite Int -> Map BiomechanicalSite Vec3 -> BiomechanicalEvidence -> [ElasticEquation]
volumetricEquations solvePolicy siteIndexBySite anchoredJointPositions evidence =
  Map.toList (bmeStructuralMembersBySite evidence)
    >>= \(structuralSite, memberSites) ->
      case Map.lookup structuralSite (bmeStructuralKindBySite evidence) of
        Just VolumetricBiomechanicalSiteKind ->
          pairwiseMemberSites memberSites
            >>= \(leftSite, rightSite) ->
              case
                ( Map.lookup leftSite siteIndexBySite,
                  Map.lookup rightSite siteIndexBySite,
                  Map.lookup leftSite anchoredJointPositions,
                  Map.lookup rightSite anchoredJointPositions
                ) of
                (Just leftIndex, Just rightIndex, Just leftAnchor, Just rightAnchor) ->
                  axisEquations
                    VolumetricPreservationElasticEnergy
                    (bvpeVolumetricWeight (bslpVolumetricPreservation solvePolicy))
                    (differenceRows (Map.size siteIndexBySite) leftIndex rightIndex)
                    (subVec3 rightAnchor leftAnchor)
                _ ->
                  []
        _ ->
          []

structuralWeight :: BiomechanicalSolvePolicy -> BiomechanicalStructuralKind -> Double
structuralWeight solvePolicy structuralKind =
  case structuralKind of
    StructuralBiomechanicalSiteKind ->
      bsceStructuralWeight (bslpStructuralCoherence solvePolicy)
    VolumetricBiomechanicalSiteKind ->
      bvpeVolumetricWeight (bslpVolumetricPreservation solvePolicy)

structuralComponent :: BiomechanicalStructuralKind -> ElasticEnergyComponent
structuralComponent structuralKind =
  case structuralKind of
    StructuralBiomechanicalSiteKind ->
      StructuralCoherenceElasticEnergy
    VolumetricBiomechanicalSiteKind ->
      VolumetricPreservationElasticEnergy

pairwiseMemberSites :: [BiomechanicalSite] -> [(BiomechanicalSite, BiomechanicalSite)]
pairwiseMemberSites =
  pairwise

elasticStrainEquations :: Double -> [[(Int, Double)]] -> Vec3 -> [ElasticEquation]
elasticStrainEquations =
  axisEquations ElasticStrainElasticEnergy

axisEquations :: ElasticEnergyComponent -> Double -> [[(Int, Double)]] -> Vec3 -> [ElasticEquation]
axisEquations component weight termsByAxis targetVector =
  zipWith
    (\axisTerms rhsValue -> SparseNormalEquation weight component axisTerms rhsValue)
    termsByAxis
    (vecToList targetVector)

siteRows :: Int -> Int -> [[(Int, Double)]]
siteRows _ siteIndex =
  fmap
    (\componentIndex -> [(componentOffset siteIndex componentIndex, 1.0)])
    [0 :: Int .. 2]

differenceRows :: Int -> Int -> Int -> [[(Int, Double)]]
differenceRows _ sourceIndex targetIndex =
  fmap
    (\componentIndex ->
        [ (componentOffset sourceIndex componentIndex, -1.0),
          (componentOffset targetIndex componentIndex, 1.0)
        ]
    )
    [0 :: Int .. 2]

componentOffset :: Int -> Int -> Int
componentOffset siteIndex componentIndex =
  3 * siteIndex + componentIndex

assembleNormalSystem :: BiomechanicalSolvePolicy -> Int -> [ElasticEquation] -> Either [BiomechanicalSolveFailure] ElasticNormalSystem
assembleNormalSystem solvePolicy siteCount equations =
  case assembleSparseNormalSystem (bslpRegularizationWeight solvePolicy) (3 * siteCount) equations of
    Left assemblyError ->
      Left [BiomechanicalElasticSystemAssemblyFailure assemblyError]
    Right normalSystem ->
      Right normalSystem

elasticObjectiveBreakdownOf :: [ElasticEquation] -> [Double] -> ElasticObjectiveBreakdown
elasticObjectiveBreakdownOf equations solutionVector =
  let objectiveByComponent = Operator.objectiveBreakdownOf equations solutionVector
      componentEnergy componentValue = Map.findWithDefault 0.0 componentValue objectiveByComponent
      anchorEnergy = componentEnergy AnchorFidelityElasticEnergy
      strainEnergy = componentEnergy ElasticStrainElasticEnergy
      structuralEnergy = componentEnergy StructuralCoherenceElasticEnergy
      volumetricEnergy = componentEnergy VolumetricPreservationElasticEnergy
   in ElasticObjectiveBreakdown
        { eobAnchorFidelity = anchorEnergy,
          eobElasticStrain = strainEnergy,
          eobStructuralCoherence = structuralEnergy,
          eobVolumetricPreservation = volumetricEnergy,
          eobTotal = anchorEnergy + strainEnergy + structuralEnergy + volumetricEnergy
        }

sitePositionVector :: [BiomechanicalSite] -> Map BiomechanicalSite Vec3 -> [Double]
sitePositionVector solvedSites anchoredPositions =
  solvedSites >>= \site -> vecToList (Map.findWithDefault vec3Zero site anchoredPositions)

decodeSitePositions :: [BiomechanicalSite] -> [Double] -> Either [BiomechanicalSolveFailure] (Map BiomechanicalSite Vec3)
decodeSitePositions solvedSites solutionVector =
  Map.fromList <$> traverse decodeSitePosition (zip [0 :: Int ..] solvedSites)
  where
    decodeSitePosition (siteIndex, site) =
      case vec3FromList (take 3 (drop (3 * siteIndex) solutionVector)) of
        Left decodeError ->
          Left [BiomechanicalSolutionDecodeFailure site decodeError]
        Right position ->
          Right (site, position)

vecToList :: Vec3 -> [Double]
vecToList (Vec3 x y z) = [x, y, z]

spectralSignatureOfNormalSystem ::
  BiomechanicalSpectralPolicy ->
  BiomechanicalEvidence ->
  ElasticNormalSystem ->
  Either [BiomechanicalSolveFailure] BiomechanicalSpectralSignature
spectralSignatureOfNormalSystem spectralPolicy evidence SparseNormalSystem {snsMatrix = normalMatrix} =
  BiomechanicalSpectralSignature
    <$> pure (bmeGraphSignature evidence)
    <*> (Just <$> elasticSpectralSignatureOfMatrix spectralPolicy evidence normalMatrix)

elasticSpectralSignatureOfMatrix ::
  BiomechanicalSpectralPolicy ->
  BiomechanicalEvidence ->
  SparseCSR Double ->
  Either [BiomechanicalSolveFailure] BiomechanicalElasticSpectralSignature
elasticSpectralSignatureOfMatrix spectralPolicy evidence sparseMatrix =
  let matrixSize = csrRows sparseMatrix
      requestedCount = max 1 (min matrixSize (bspModeCount spectralPolicy))
      seedVector = spectralSeedVector matrixSize
      solveConfig =
        withEigenFallbackInitialVector seedVector
          (withEigenFallbackLanczosConfig defaultLanczosConfig defaultEigenSolveConfig)
   in do
        countValue <- liftSpectralEither (mkPositiveCount requestedCount)
        largestCount <- liftSpectralEither (mkPositiveCount 1)
        operatorValue <- liftSpectralEither (selfAdjointCSRLinearOperator sparseMatrix)
        (smallestColumns, largestEigenvalue) <-
          elasticEigenSample solveConfig operatorValue requestedCount countValue largestCount sparseMatrix
        let orderedPairs = sortOnEigenvalue smallestColumns
            orderedEigenvalues = fmap fst orderedPairs
            requestedEigenvalues = take requestedCount orderedEigenvalues
            smallestEigenvalue = minimumOrZero orderedEigenvalues
            modeVectors = fmap snd orderedPairs
            modeLocalizations = fmap elasticModeLocalization modeVectors
            structuralModePenaltyValue = meanOrZero (fmap (fst . structuralVolumetricModePenalties evidence) modeVectors)
            volumetricModePenaltyValue = meanOrZero (fmap (snd . structuralVolumetricModePenalties evidence) modeVectors)
            spectralGap =
              fromMaybe 0.0 (Aggregate.spectralGap requestedEigenvalues)
            nearZeroModeCount =
              length
                ( filter
                    (\eigenvalue -> abs eigenvalue <= bspElasticNearZeroTolerance spectralPolicy)
                    requestedEigenvalues
                )
            conditionEstimate =
              if abs smallestEigenvalue <= max 1.0e-12 (bspElasticNearZeroTolerance spectralPolicy)
                then 1.0 / max 1.0e-12 (bspElasticNearZeroTolerance spectralPolicy)
                else largestEigenvalue / smallestEigenvalue
        Right
          BiomechanicalElasticSpectralSignature
            { bessEigenvalues = requestedEigenvalues,
              bessSmallestEigenvalue = smallestEigenvalue,
              bessLargestEigenvalue = largestEigenvalue,
              bessSpectralGap = spectralGap,
              bessConditionEstimate = conditionEstimate,
              bessNearZeroModeCount = nearZeroModeCount,
              bessModeLocalizations = modeLocalizations,
              bessMeanLocalization = meanOrZero modeLocalizations,
              bessStructuralModePenalty = structuralModePenaltyValue,
              bessVolumetricModePenalty = volumetricModePenaltyValue
            }

liftSpectralEither :: Show err => Either err value -> Either [BiomechanicalSolveFailure] value
liftSpectralEither =
  either
    (\err -> Left [BiomechanicalElasticSpectralDecompositionFailure (show err)])
    Right

elasticEigenSample ::
  EigenSolveConfig ->
  LinearOperator 'SelfAdjointOperator ->
  Int ->
  PositiveCount ->
  PositiveCount ->
  SparseCSR Double ->
  Either [BiomechanicalSolveFailure] ([(Double, [Double])], Double)
elasticEigenSample solveConfig operatorValue requestedCount countValue largestCount sparseMatrix =
  case solveEigenRequest solveConfig operatorValue (EigenpairsRequest SmallestEigenvalues countValue) of
    Right smallestPairs -> do
      smallestColumns <- eigenpairColumns smallestPairs
      largestEigenvalues <-
        liftSpectralEither
          (solveEigenRequest solveConfig operatorValue (EigenvaluesRequest LargestEigenvalues largestCount))
      Right
        ( smallestColumns,
          case U.toList largestEigenvalues of
            largestValue : _ -> largestValue
            [] -> maximumOrZero (fmap fst smallestColumns)
        )
    Left spectralError ->
      case denseElasticEigenSample requestedCount sparseMatrix of
        Right denseSample ->
          Right denseSample
        Left _ ->
          liftSpectralEither (Left spectralError)

denseElasticEigenSample ::
  Int ->
  SparseCSR Double ->
  Either [BiomechanicalSolveFailure] ([(Double, [Double])], Double)
denseElasticEigenSample requestedCount sparseMatrix =
  if csrRows sparseMatrix == csrCols sparseMatrix
    then do
      eigenPairs <- liftSpectralEither (symmetricEigenPairs (csrRows sparseMatrix) (csrDenseRows sparseMatrix))
      let orderedPairs = sortOnEigenvalue eigenPairs
      Right (take requestedCount orderedPairs, maximumOrZero (fmap fst orderedPairs))
    else
      Left [BiomechanicalElasticSpectralDecompositionFailure "elastic normal matrix must be square"]

csrDenseRows :: SparseCSR Double -> [[Double]]
csrDenseRows sparseMatrix =
  fmap (csrDenseRow sparseMatrix) [0 :: Int .. csrRows sparseMatrix - 1]

csrDenseRow :: SparseCSR Double -> Int -> [Double]
csrDenseRow sparseMatrix rowIndex =
  let offsets = csrRowOffsetsVector sparseMatrix
      columnIndices = csrColumnIndicesVector sparseMatrix
      values = csrValuesVector sparseMatrix
      entryStart = fromMaybe 0 (offsets U.!? rowIndex)
      entryStop = fromMaybe entryStart (offsets U.!? (rowIndex + 1))
      rowEntries =
        fmap
          ( \entryIndex ->
              ( fromMaybe 0 (columnIndices U.!? entryIndex),
                fromMaybe 0.0 (values U.!? entryIndex)
              )
          )
          [entryStart .. entryStop - 1]
      rowEntryByColumn = Map.fromList rowEntries
   in fmap (\columnIndex -> Map.findWithDefault 0.0 columnIndex rowEntryByColumn) [0 :: Int .. csrCols sparseMatrix - 1]

eigenpairColumns :: Eigenpairs -> Either [BiomechanicalSolveFailure] [(Double, [Double])]
eigenpairColumns pairs =
  traverse eigenpairColumn (U.toList (U.indexed (eigenpairValues pairs)))
  where
    eigenpairColumn (columnIndex, eigenvalue) =
      fmap
        (\eigenvector -> (eigenvalue, U.toList eigenvector))
        (liftSpectralEither (eigenpairVectorAt columnIndex pairs))

elasticSpectralAdmissible :: BiomechanicalSpectralPolicy -> BiomechanicalSpectralSignature -> Bool
elasticSpectralAdmissible spectralPolicy spectralSignature =
  case bssElasticSignature spectralSignature of
    Nothing ->
      True
    Just elasticSignature ->
      bessSmallestEigenvalue elasticSignature >= bspMinElasticEigenvalue spectralPolicy
        && bessConditionEstimate elasticSignature <= bspMaxElasticConditionEstimate spectralPolicy
        && bessNearZeroModeCount elasticSignature <= bspMaxElasticNearZeroModes spectralPolicy
        && bessMeanLocalization elasticSignature <= bspMaxElasticLocalization spectralPolicy

minimumOrZero :: [Double] -> Double
minimumOrZero =
  fromMaybe 0.0 . minimumOf

maximumOrZero :: [Double] -> Double
maximumOrZero =
  fromMaybe 0.0 . maximumOf

spectralSeedVector :: Int -> U.Vector Double
spectralSeedVector dimension =
  if dimension <= 0
    then U.empty
    else U.replicate dimension 1.0

sortOnEigenvalue :: [(Double, [Double])] -> [(Double, [Double])]
sortOnEigenvalue =
  sortBy (\leftPair rightPair -> compare (fst leftPair) (fst rightPair))

elasticModeLocalization :: [Double] -> Double
elasticModeLocalization vectorValues =
  let squaredNormValue = sum (fmap (\value -> value * value) vectorValues)
   in if squaredNormValue <= 1.0e-12
        then 0.0
        else sum (fmap (\value -> value * value * value * value) vectorValues) / (squaredNormValue * squaredNormValue)

structuralVolumetricModePenalties :: BiomechanicalEvidence -> [Double] -> (Double, Double)
structuralVolumetricModePenalties evidence vectorValues =
  let structuralDofs = dofIndicesForStructuralKind StructuralBiomechanicalSiteKind evidence
      volumetricDofs = dofIndicesForStructuralKind VolumetricBiomechanicalSiteKind evidence
      structuralEnergy = squaredMassOnDofs structuralDofs vectorValues
      volumetricEnergy = squaredMassOnDofs volumetricDofs vectorValues
      expectedShares = expectedStructuralVolumetricShares evidence
   in case expectedShares of
        Nothing ->
          (0.0, 0.0)
        Just (expectedStructuralShare, expectedVolumetricShare) ->
          let familyEnergy = structuralEnergy + volumetricEnergy
              observedStructuralShare =
                if familyEnergy <= 1.0e-12
                  then expectedStructuralShare
                  else structuralEnergy / familyEnergy
              observedVolumetricShare =
                if familyEnergy <= 1.0e-12
                  then expectedVolumetricShare
                  else volumetricEnergy / familyEnergy
           in
            ( abs (observedStructuralShare - expectedStructuralShare),
              abs (observedVolumetricShare - expectedVolumetricShare)
            )

dofIndicesForStructuralKind :: BiomechanicalStructuralKind -> BiomechanicalEvidence -> [Int]
dofIndicesForStructuralKind structuralKind evidence =
  concatMap siteIndexToDofs matchingSiteIndices
  where
    structuralOffset = length (bmeOrderedJointSites evidence)
    matchingSiteIndices =
      fmap
        (\(siteIndex, _) -> structuralOffset + siteIndex)
        ( filter
            (\(_, structuralSite) -> Map.lookup structuralSite (bmeStructuralKindBySite evidence) == Just structuralKind)
            (zip [0 :: Int ..] (bmeOrderedStructuralSites evidence))
        )

siteIndexToDofs :: Int -> [Int]
siteIndexToDofs siteIndex =
  let baseIndex = 3 * siteIndex
   in [baseIndex, baseIndex + 1, baseIndex + 2]

squaredMassOnDofs :: [Int] -> [Double] -> Double
squaredMassOnDofs dofIndices vectorValues =
  sum (fmap (\dofIndex -> square (valueAtIndex dofIndex vectorValues)) dofIndices)

expectedStructuralVolumetricShares :: BiomechanicalEvidence -> Maybe (Double, Double)
expectedStructuralVolumetricShares evidence =
  let structuralCount =
        length
          ( filter
              (\site -> Map.lookup site (bmeStructuralKindBySite evidence) == Just StructuralBiomechanicalSiteKind)
              (bmeOrderedStructuralSites evidence)
          )
      volumetricCount =
        length
          ( filter
              (\site -> Map.lookup site (bmeStructuralKindBySite evidence) == Just VolumetricBiomechanicalSiteKind)
              (bmeOrderedStructuralSites evidence)
          )
      totalCount = structuralCount + volumetricCount
   in if totalCount <= 0
        then Nothing
        else
          Just
            ( fromIntegral structuralCount / fromIntegral totalCount,
              fromIntegral volumetricCount / fromIntegral totalCount
            )

valueAtIndex :: Int -> [Double] -> Double
valueAtIndex indexValue vectorValues =
  case drop indexValue vectorValues of
    value : _ ->
      value
    [] ->
      0.0

square :: Double -> Double
square value =
  value * value

meanOrZero :: [Double] -> Double
meanOrZero =
  fromMaybe 0.0 . averageOf
