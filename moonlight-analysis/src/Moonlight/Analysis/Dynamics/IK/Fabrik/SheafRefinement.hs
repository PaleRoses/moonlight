{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Analysis.Dynamics.IK.Fabrik.SheafRefinement
  ( MinimumJointCount,
    mkMinimumJointCount,
    FabrikRoundLimit,
    mkFabrikRoundLimit,
    FabrikTolerance,
    mkFabrikTolerance,
    IKJointSite (..),
    IKChainBlueprint (..),
    IKChainEvidence (..),
    IKChainRefinementDetail (..),
    IKChainRefinementModel,
    mkIKChainRefinementModel,
    ikChainRefiner,
  )
where

import Data.Kind (Type)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Moonlight.Analysis.Dynamics.IK.Fabrik
  ( IKChain,
    endEffector,
    ikJoints,
    mkIKChain,
    solveFabrik,
  )
import Moonlight.Analysis.SheafRefinement
  ( SheafRefinementModel (..),
    SheafEnergy (..),
    SheafSolve (..),
    SheafRefiner (..),
  )
import Moonlight.EGraph.Fuzzy.Core
  ( FuzzyRank (..),
    RefinementCandidate (..),
  )
import Moonlight.Core (Substitution (..))
import Moonlight.EGraph.Pure.Types (ClassId)
import Moonlight.LinAlg.Geometry (Vec3, distanceVec3)

type MinimumJointCount :: Type
newtype MinimumJointCount = MinimumJointCount
  { unMinimumJointCount :: Int
  }
  deriving stock (Eq, Show)

type FabrikRoundLimit :: Type
newtype FabrikRoundLimit = FabrikRoundLimit
  { unFabrikRoundLimit :: Int
  }
  deriving stock (Eq, Show)

type FabrikTolerance :: Type
newtype FabrikTolerance = FabrikTolerance
  { unFabrikTolerance :: Double
  }
  deriving stock (Eq, Show)

type IKJointSite :: Type
data IKJointSite = IKJointSite
  { ikJointPatternKey :: Int,
    ikJointClassId :: ClassId
  }
  deriving stock (Eq, Ord, Show)

type IKChainBlueprint :: Type
data IKChainBlueprint = IKChainBlueprint
  { ikbMinimumJointCount :: MinimumJointCount,
    ikbTarget :: Vec3
  }
  deriving stock (Eq, Show)

type IKChainEvidence :: Type
data IKChainEvidence = IKChainEvidence
  { ikeOrderedSites :: [IKJointSite],
    ikeTarget :: Vec3
  }
  deriving stock (Eq, Show)

type IKChainRefinementDetail :: Type
data IKChainRefinementDetail = IKChainRefinementDetail
  { ikdJointCount :: Int,
    ikdEndEffector :: Vec3
  }
  deriving stock (Eq, Show)

type IKChainRefinementModel :: Type
data IKChainRefinementModel = IKChainRefinementModel
  { ikmMinimumJointCount :: MinimumJointCount,
    ikmRoundLimit :: FabrikRoundLimit,
    ikmTolerance :: FabrikTolerance,
    ikmTarget :: Vec3,
    ikmLookupJointPosition :: ClassId -> Maybe Vec3
  }

mkMinimumJointCount :: Int -> Maybe MinimumJointCount
mkMinimumJointCount jointCount
  | jointCount >= 0 =
      Just (MinimumJointCount jointCount)
  | otherwise =
      Nothing

mkFabrikRoundLimit :: Int -> Maybe FabrikRoundLimit
mkFabrikRoundLimit roundLimit
  | roundLimit >= 0 =
      Just (FabrikRoundLimit roundLimit)
  | otherwise =
      Nothing

mkFabrikTolerance :: Double -> Maybe FabrikTolerance
mkFabrikTolerance tolerance
  | tolerance >= 0.0 =
      Just (FabrikTolerance tolerance)
  | otherwise =
      Nothing

mkIKChainRefinementModel ::
  MinimumJointCount ->
  FabrikRoundLimit ->
  FabrikTolerance ->
  Vec3 ->
  (ClassId -> Maybe Vec3) ->
  IKChainRefinementModel
mkIKChainRefinementModel minimumJointCount roundLimit tolerance target lookupJointPosition =
  IKChainRefinementModel
    { ikmMinimumJointCount = minimumJointCount,
      ikmRoundLimit = roundLimit,
      ikmTolerance = tolerance,
      ikmTarget = target,
      ikmLookupJointPosition = lookupJointPosition
    }

ikChainRefiner ::
  MinimumJointCount ->
  FabrikRoundLimit ->
  FabrikTolerance ->
  Vec3 ->
  (ClassId -> Maybe Vec3) ->
  SheafRefiner IKChainRefinementModel
ikChainRefiner minimumJointCount roundLimit tolerance target lookupJointPosition =
  SheafRefiner
    (mkIKChainRefinementModel minimumJointCount roundLimit tolerance target lookupJointPosition)

instance SheafRefinementModel IKChainRefinementModel where
  type SheafSite IKChainRefinementModel = IKJointSite
  type SheafAnchor IKChainRefinementModel = Vec3
  type SheafEvidence IKChainRefinementModel = IKChainEvidence
  type SheafValue IKChainRefinementModel = Vec3
  type SheafDetail IKChainRefinementModel = IKChainRefinementDetail
  type SheafBlueprint IKChainRefinementModel = IKChainBlueprint
  type SheafScore IKChainRefinementModel = Double
  type SheafRank IKChainRefinementModel = Double
  type SheafSeed IKChainRefinementModel = (ClassId, Substitution)

  compileSheafBlueprint model =
    IKChainBlueprint
      { ikbMinimumJointCount = ikmMinimumJointCount model,
        ikbTarget = ikmTarget model
      }

  enumerateSheafCandidates model _ =
    fmap (seedToCandidate model)

  acceptSheafCandidate _ blueprint candidate =
    length (ikeOrderedSites (rcEvidence candidate)) >= unMinimumJointCount (ikbMinimumJointCount blueprint)
      && case chainFromAnchors candidate of
        Just _ -> True
        Nothing -> False

  solveSheafCandidate model _ candidate =
    fmap
      ( \initialChain ->
          let solvedChain =
                solveFabrik
                  (unFabrikRoundLimit (ikmRoundLimit model))
                  (unFabrikTolerance (ikmTolerance model))
                  (ikeTarget (rcEvidence candidate))
                  initialChain
              solvedJoints = NonEmpty.toList (ikJoints solvedChain)
              residualValue = distanceVec3 (endEffector solvedChain) (ikeTarget (rcEvidence candidate))
           in SheafSolve
                { ssValueBySite =
                    Map.fromList
                      (zip (ikeOrderedSites (rcEvidence candidate)) solvedJoints),
                  ssResidual = residualValue,
                  ssDetail =
                    IKChainRefinementDetail
                      { ikdJointCount = length solvedJoints,
                        ikdEndEffector = endEffector solvedChain
                      }
                }
      )
      (chainFromAnchors candidate)

  interpretSheafSolve _ _ candidate sheafSolve =
    chainSolveEnergy candidate sheafSolve
    where
      chainSolveEnergy ::
        RefinementCandidate IKJointSite Vec3 IKChainEvidence ->
        SheafSolve IKJointSite Vec3 IKChainRefinementDetail ->
        SheafEnergy Double
      chainSolveEnergy candidateValue solveValue =
        let orderedSites = ikeOrderedSites (rcEvidence candidateValue)
         in case
              ( traverse (`Map.lookup` rcAnchors candidateValue) orderedSites,
                traverse (`Map.lookup` ssValueBySite solveValue) orderedSites
              )
              of
              (Just initialJoints, Just solvedJoints) ->
                SheafEnergy (sum (zipWith distanceVec3 initialJoints solvedJoints))
              _ ->
                SheafEnergy 0.0

  rankSheafEnergy _ (SheafEnergy energyValue) = FuzzyRank energyValue

  compareSheafRanks _ (FuzzyRank leftRank) (FuzzyRank rightRank) =
    compare leftRank rightRank

seedToCandidate ::
  IKChainRefinementModel ->
  (ClassId, Substitution) ->
  RefinementCandidate IKJointSite Vec3 IKChainEvidence
seedToCandidate model (rootClassId, substitution@(Substitution bindings)) =
  let orderedVarSites =
        IntMap.mapWithKey
          (\patternKey classId -> IKJointSite patternKey classId)
          bindings
      orderedSites = fmap snd (IntMap.toAscList orderedVarSites)
      anchoredPositions = Map.fromList (mapJointPosition model orderedSites)
   in RefinementCandidate
        { rcRootClass = rootClassId,
          rcDiscreteSubstitution = substitution,
          rcVarSites = orderedVarSites,
          rcSites = orderedSites,
          rcAnchors = anchoredPositions,
          rcEvidence =
            IKChainEvidence
              { ikeOrderedSites = orderedSites,
                ikeTarget = ikmTarget model
              }
        }

mapJointPosition :: IKChainRefinementModel -> [IKJointSite] -> [(IKJointSite, Vec3)]
mapJointPosition IKChainRefinementModel {..} =
  mapMaybe (\site -> fmap ((,) site) (lookupSitePosition site))
  where
    lookupSitePosition (IKJointSite _ classId) = ikmLookupJointPosition classId

chainFromAnchors ::
  RefinementCandidate IKJointSite Vec3 IKChainEvidence ->
  Maybe IKChain
chainFromAnchors candidate =
  anchoredJointPositions
    (ikeOrderedSites (rcEvidence candidate))
    (rcAnchors candidate)
    >>= (Just . mkIKChain)

anchoredJointPositions ::
  [IKJointSite] ->
  Map IKJointSite Vec3 ->
  Maybe (NonEmpty.NonEmpty Vec3)
anchoredJointPositions orderedSites anchoredPositions =
  NonEmpty.nonEmpty =<< traverse (`Map.lookup` anchoredPositions) orderedSites
