{-# LANGUAGE RecordWildCards #-}

module Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Score
  ( rankBiomechanicalScore,
    compareBiomechanicalRanks,
    interpretBiomechanicalScore,
    stalkScalar,
    boneElasticEnergy,
    normalizedStrain,
    elasticEnergy,
  )
where

import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Core
  ( BiomechanicalRank (..),
    BiomechanicalScore (..)
  )
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Policy
  ( BiomechanicalAnchorFidelityScoreComponent (..),
    BiomechanicalElasticStrainScoreComponent (..),
    BiomechanicalLexicographicRankOrder (..),
    BiomechanicalRankDimension (..),
    BiomechanicalRankPolicy (..),
    BiomechanicalResidualScoreComponent (..),
    BiomechanicalScorePolicy (..),
    BiomechanicalSpectralDriftScoreComponent (..),
    BiomechanicalStructuralCoherenceScoreComponent (..),
    BiomechanicalVolumetricPreservationScoreComponent (..)
  )
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.SheafStalk
  ( BiomechanicalStalk (..) )
import Moonlight.EGraph.Fuzzy.Rank
  ( RankMode (..),
    compareRankBy,
    totalOf,
    weightedComponent,
  )
import Moonlight.LinAlg.Geometry (distanceVec3)

rankBiomechanicalScore :: BiomechanicalScorePolicy -> BiomechanicalScore -> BiomechanicalRank
rankBiomechanicalScore BiomechanicalScorePolicy {..} BiomechanicalScore {..} =
  let residualValue = residualContribution bscpResidualComponent bmsEndEffectorResidual
      anchorFidelityValue = anchorFidelityContribution bscpAnchorFidelityComponent bmsAnchorFidelityEnergy
      elasticStrainValue = elasticStrainContribution bscpElasticStrainComponent bmsStrainEnergy
      structuralCoherenceValue = structuralCoherenceContribution bscpStructuralCoherenceComponent bmsStructuralCoherenceEnergy
      volumetricPreservationValue = volumetricPreservationContribution bscpVolumetricPreservationComponent bmsVolumetricPreservationEnergy
      spectralDriftValue = spectralDriftContribution bscpSpectralDriftComponent bmsSpectralDrift
      componentValues =
        [ residualValue,
          anchorFidelityValue,
          elasticStrainValue,
          structuralCoherenceValue,
          volumetricPreservationValue,
          spectralDriftValue
        ]
   in BiomechanicalRank
        { bmrResidualComponent = residualValue,
          bmrAnchorFidelityComponent = anchorFidelityValue,
          bmrElasticStrainComponent = elasticStrainValue,
          bmrStructuralCoherenceComponent = structuralCoherenceValue,
          bmrVolumetricPreservationComponent = volumetricPreservationValue,
          bmrSpectralDriftComponent = spectralDriftValue,
          bmrTotal = totalOf componentValues
        }

interpretBiomechanicalScore :: BiomechanicalScorePolicy -> BiomechanicalScore -> Double
interpretBiomechanicalScore scorePolicy =
  bmrTotal . rankBiomechanicalScore scorePolicy

compareBiomechanicalRanks :: BiomechanicalRankPolicy -> BiomechanicalRank -> BiomechanicalRank -> Ordering
compareBiomechanicalRanks rankPolicy =
  compareRankBy rankMode [minBound .. maxBound] rankComponent bmrTotal
  where
    rankMode =
      case rankPolicy of
        TotalBiomechanicalRankPolicy ->
          CompareByTotal
        LexicographicBiomechanicalRankPolicy (BiomechanicalLexicographicRankOrder rankDimensions) ->
          CompareLexicographic rankDimensions
        ParetoBiomechanicalRankPolicy ->
          ComparePareto

residualContribution :: BiomechanicalResidualScoreComponent -> Double -> Double
residualContribution (BiomechanicalResidualScoreComponent weight) =
  weightedComponent weight

anchorFidelityContribution :: BiomechanicalAnchorFidelityScoreComponent -> Double -> Double
anchorFidelityContribution (BiomechanicalAnchorFidelityScoreComponent weight) =
  weightedComponent weight

elasticStrainContribution :: BiomechanicalElasticStrainScoreComponent -> Double -> Double
elasticStrainContribution (BiomechanicalElasticStrainScoreComponent weight) =
  weightedComponent weight

structuralCoherenceContribution :: BiomechanicalStructuralCoherenceScoreComponent -> Double -> Double
structuralCoherenceContribution (BiomechanicalStructuralCoherenceScoreComponent weight) =
  weightedComponent weight

volumetricPreservationContribution :: BiomechanicalVolumetricPreservationScoreComponent -> Double -> Double
volumetricPreservationContribution (BiomechanicalVolumetricPreservationScoreComponent weight) =
  weightedComponent weight

spectralDriftContribution :: BiomechanicalSpectralDriftScoreComponent -> Double -> Double
spectralDriftContribution (BiomechanicalSpectralDriftScoreComponent weight) =
  weightedComponent weight

rankComponent :: BiomechanicalRankDimension -> BiomechanicalRank -> Double
rankComponent rankDimension biomechanicalRank =
  case rankDimension of
    ResidualRankDimension ->
      bmrResidualComponent biomechanicalRank
    AnchorFidelityRankDimension ->
      bmrAnchorFidelityComponent biomechanicalRank
    ElasticStrainRankDimension ->
      bmrElasticStrainComponent biomechanicalRank
    StructuralCoherenceRankDimension ->
      bmrStructuralCoherenceComponent biomechanicalRank
    VolumetricPreservationRankDimension ->
      bmrVolumetricPreservationComponent biomechanicalRank
    SpectralDriftRankDimension ->
      bmrSpectralDriftComponent biomechanicalRank

stalkScalar :: BiomechanicalStalk -> Double
stalkScalar stalk =
  case stalk of
    JointBiomechanicalStalk anchorPosition solvedPosition ->
      distanceVec3 anchorPosition solvedPosition
    BoneEndpointBiomechanicalStalk _ anchorPosition solvedPosition ->
      distanceVec3 anchorPosition solvedPosition
    BoneBiomechanicalStalk _ _ _ _ _ _ _ _ elasticEnergyValue ->
      elasticEnergyValue

boneElasticEnergy :: BiomechanicalStalk -> Double
boneElasticEnergy stalk =
  case stalk of
    BoneBiomechanicalStalk _ _ _ _ _ _ _ _ elasticEnergyValue ->
      elasticEnergyValue
    JointBiomechanicalStalk _ _ ->
      0.0
    BoneEndpointBiomechanicalStalk _ _ _ ->
      0.0

normalizedStrain :: Double -> Double -> Double
normalizedStrain currentLength restLength =
  if restLength <= 1.0e-12
    then currentLength - restLength
    else (currentLength - restLength) / restLength

elasticEnergy :: Double -> Double -> Double -> Double
elasticEnergy stiffness currentLength restLength =
  let extension = currentLength - restLength
   in 0.5 * stiffness * extension * extension
