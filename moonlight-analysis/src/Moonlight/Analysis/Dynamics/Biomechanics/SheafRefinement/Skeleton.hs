module Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Skeleton
  ( Graph1Skeleton (..),
    compileBiomechanicalAnatomicalBlueprint,
    uniquePatternVars,
    skeletonFromAnatomicalBlueprint,
    skeletonFromEdgeSupports,
    canonicalVertexEdge,
    graphSpectralSignature,
    graphSpectralDistance,
    graphSpectralCompatible,
    graphSpectrum,
    maxAbsDiff,
    adjacencyFromEdges,
  )
where

import Data.Kind (Type)
import Data.Foldable (toList)
import Data.List (sort)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Moonlight.Core qualified as Aggregate
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Core
  ( BiomechanicalAnatomicalBlueprint (..),
    BiomechanicalBoneBlueprint (..),
    BiomechanicalBoneName (..),
    BiomechanicalGraphSpectralSignature (..),
    BiomechanicalJointBlueprint (..),
    BiomechanicalJointName (..),
    BiomechanicalStructuralBlueprint (..),
    BiomechanicalStructuralKind (..)
  )
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Policy
  ( BiomechanicalSpectralPolicy (..) )
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Validate
  ( BiomechanicalAnatomicalBlueprintProgram (..) )
import Moonlight.Core (Pattern (..), patternVarKey)
import Moonlight.Homology
  ( Graph1Skeleton (..),
    GraphSpectralMode (..),
    graphFromEdgeSupports,
    graphSpectralModes,
  )

compileBiomechanicalAnatomicalBlueprint :: Traversable f => BiomechanicalAnatomicalBlueprintProgram -> Pattern f -> BiomechanicalAnatomicalBlueprint
compileBiomechanicalAnatomicalBlueprint blueprintProgram patternValue =
  let fragment = anatomicalFragment blueprintProgram [] patternValue
   in BiomechanicalAnatomicalBlueprint
        { babJointBlueprints = afJointBlueprints fragment,
          babBoneBlueprints = afBoneBlueprints fragment,
          babStructuralBlueprints = afStructuralBlueprints fragment,
          babEffectorJoint = babpEffectorJointForNames blueprintProgram (afOrderedJointNames fragment)
        }

type AnatomicalFragment :: Type
data AnatomicalFragment = AnatomicalFragment
  { afJointBlueprints :: [BiomechanicalJointBlueprint],
    afBoneBlueprints :: [BiomechanicalBoneBlueprint],
    afStructuralBlueprints :: [BiomechanicalStructuralBlueprint],
    afOrderedJointNames :: [BiomechanicalJointName],
    afRepresentativeJoint :: Maybe BiomechanicalJointName
  }

anatomicalFragment :: Traversable f => BiomechanicalAnatomicalBlueprintProgram -> [Int] -> Pattern f -> AnatomicalFragment
anatomicalFragment blueprintProgram path patternValue =
  case patternValue of
    PatternVar patternVar ->
      let jointName = babpJointNameForVar blueprintProgram patternVar
       in AnatomicalFragment
            { afJointBlueprints = [BiomechanicalJointBlueprint jointName patternVar],
              afBoneBlueprints = [],
              afStructuralBlueprints = [],
              afOrderedJointNames = [jointName],
              afRepresentativeJoint = Just jointName
            }
    PatternNode patternNode ->
      let childFragments = fmap (uncurry (anatomicalFragment blueprintProgram)) (indexedChildPatterns path (toList patternNode))
          orderedJointNames = uniqueJointNames (foldMap afOrderedJointNames childFragments)
          branchBones = uniqueBoneBlueprints (branchBoneBlueprints blueprintProgram path childFragments)
          structuralBlueprints = foldMap afStructuralBlueprints childFragments <> maybeStructuralBlueprint blueprintProgram path orderedJointNames
       in AnatomicalFragment
            { afJointBlueprints = uniqueJointBlueprints (foldMap afJointBlueprints childFragments),
              afBoneBlueprints = uniqueBoneBlueprints (foldMap afBoneBlueprints childFragments <> branchBones),
              afStructuralBlueprints = uniqueStructuralBlueprints structuralBlueprints,
              afOrderedJointNames = orderedJointNames,
              afRepresentativeJoint = listToMaybe orderedJointNames
            }

indexedChildPatterns :: [Int] -> [Pattern f] -> [([Int], Pattern f)]
indexedChildPatterns path childPatterns =
  fmap
    (\(childIndex, childPattern) -> (path <> [childIndex], childPattern))
    (zip [0 :: Int ..] childPatterns)

branchBoneBlueprints :: BiomechanicalAnatomicalBlueprintProgram -> [Int] -> [AnatomicalFragment] -> [BiomechanicalBoneBlueprint]
branchBoneBlueprints blueprintProgram path childFragments =
  case mapMaybe afRepresentativeJoint childFragments of
    representativeJoint : remainingRepresentativeJoints ->
      fmap (mkBoneBlueprint blueprintProgram path representativeJoint) remainingRepresentativeJoints
    [] ->
      []

mkBoneBlueprint :: BiomechanicalAnatomicalBlueprintProgram -> [Int] -> BiomechanicalJointName -> BiomechanicalJointName -> BiomechanicalBoneBlueprint
mkBoneBlueprint blueprintProgram path leftJoint rightJoint =
  let canonicalBoneName = canonicalBiomechanicalBoneName (babpBoneNameForJoints blueprintProgram) path leftJoint rightJoint
   in BiomechanicalBoneBlueprint
        { bbbName = canonicalBoneName,
          bbbSourceJoint = bbnSourceJoint canonicalBoneName,
          bbbTargetJoint = bbnTargetJoint canonicalBoneName
        }

canonicalBiomechanicalBoneName ::
  ([Int] -> BiomechanicalJointName -> BiomechanicalJointName -> BiomechanicalBoneName) ->
  [Int] ->
  BiomechanicalJointName ->
  BiomechanicalJointName ->
  BiomechanicalBoneName
canonicalBiomechanicalBoneName mkBoneName path leftJoint rightJoint =
  let leftPatternVar = biomechanicalJointPatternVar leftJoint
      rightPatternVar = biomechanicalJointPatternVar rightJoint
      canonicalJointPair =
        if patternVarKey leftPatternVar <= patternVarKey rightPatternVar
          then (leftJoint, rightJoint)
          else (rightJoint, leftJoint)
   in uncurry (mkBoneName path) canonicalJointPair

maybeStructuralBlueprint :: BiomechanicalAnatomicalBlueprintProgram -> [Int] -> [BiomechanicalJointName] -> [BiomechanicalStructuralBlueprint]
maybeStructuralBlueprint blueprintProgram path incidentJoints =
  if length incidentJoints >= 2
    then
      [ BiomechanicalStructuralBlueprint
          { bsbName = babpStructuralNameForPath blueprintProgram path incidentJoints,
            bsbKind = structuralKind (length incidentJoints),
            bsbIncidentJoints = incidentJoints
          }
      ]
    else
      []

structuralKind :: Int -> BiomechanicalStructuralKind
structuralKind jointCount =
  if jointCount >= 3
    then VolumetricBiomechanicalSiteKind
    else StructuralBiomechanicalSiteKind

uniquePatternVars :: Ord a => [a] -> [a]
uniquePatternVars values =
  snd
    ( foldr
        uniqueStep
        (Set.empty, [])
        values
    )

uniqueStep :: Ord a => a -> (Set.Set a, [a]) -> (Set.Set a, [a])
uniqueStep value (seenValues, uniqueValues) =
  if Set.member value seenValues
    then (seenValues, uniqueValues)
    else (Set.insert value seenValues, value : uniqueValues)

uniqueJointNames :: [BiomechanicalJointName] -> [BiomechanicalJointName]
uniqueJointNames = uniquePatternVars

uniqueJointBlueprints :: [BiomechanicalJointBlueprint] -> [BiomechanicalJointBlueprint]
uniqueJointBlueprints blueprints =
  snd
    ( foldr
        (\blueprintValue (seenNames, uniqueBlueprints) ->
            if Set.member (bjbName blueprintValue) seenNames
              then (seenNames, uniqueBlueprints)
              else (Set.insert (bjbName blueprintValue) seenNames, blueprintValue : uniqueBlueprints)
        )
        (Set.empty, [])
        blueprints
    )

uniqueBoneBlueprints :: [BiomechanicalBoneBlueprint] -> [BiomechanicalBoneBlueprint]
uniqueBoneBlueprints blueprints =
  snd
    ( foldr
        (\blueprintValue (seenNames, uniqueBlueprints) ->
            if Set.member (bbbName blueprintValue) seenNames
              then (seenNames, uniqueBlueprints)
              else (Set.insert (bbbName blueprintValue) seenNames, blueprintValue : uniqueBlueprints)
        )
        (Set.empty, [])
        blueprints
    )

uniqueStructuralBlueprints :: [BiomechanicalStructuralBlueprint] -> [BiomechanicalStructuralBlueprint]
uniqueStructuralBlueprints blueprints =
  snd
    ( foldr
        (\blueprintValue (seenNames, uniqueBlueprints) ->
            if Set.member (bsbName blueprintValue) seenNames
              then (seenNames, uniqueBlueprints)
              else (Set.insert (bsbName blueprintValue) seenNames, blueprintValue : uniqueBlueprints)
        )
        (Set.empty, [])
        blueprints
    )

skeletonFromAnatomicalBlueprint :: BiomechanicalAnatomicalBlueprint -> Graph1Skeleton
skeletonFromAnatomicalBlueprint anatomicalBlueprint =
  let orderedJointNames = fmap bjbName (babJointBlueprints anatomicalBlueprint)
      jointIndexByName = Map.fromList (zip orderedJointNames [0 :: Int ..])
      edgeSupports =
        mapMaybe
          (\boneBlueprint ->
              canonicalVertexEdge
                <$> Map.lookup (bbbSourceJoint boneBlueprint) jointIndexByName
                <*> Map.lookup (bbbTargetJoint boneBlueprint) jointIndexByName
          )
          (babBoneBlueprints anatomicalBlueprint)
   in skeletonFromEdgeSupports (length orderedJointNames) edgeSupports

canonicalVertexEdge :: Int -> Int -> (Int, Int)
canonicalVertexEdge leftVertex rightVertex =
  if leftVertex <= rightVertex
    then (leftVertex, rightVertex)
    else (rightVertex, leftVertex)

skeletonFromEdgeSupports :: Int -> [(Int, Int)] -> Graph1Skeleton
skeletonFromEdgeSupports vertexCount edgeSupports =
  graphFromEdgeSupports vertexCount edgeSupports

graphSpectrum :: Int -> Graph1Skeleton -> [Double]
graphSpectrum requestedModes skeleton =
  bgssEigenvalues (graphSpectralSignature requestedModes skeleton)

graphSpectralSignature :: Int -> Graph1Skeleton -> BiomechanicalGraphSpectralSignature
graphSpectralSignature requestedModes skeleton =
  let modes =
        case graphSpectralModes requestedModes skeleton of
          Left _ ->
            []
          Right resolvedModes ->
            resolvedModes
      eigenvalues = fmap spectralEigenvalue modes
   in BiomechanicalGraphSpectralSignature
        { bgssEigenvalues = eigenvalues,
          bgssSpectralGap = spectralGap eigenvalues,
          bgssPositiveSupportSizes = fmap (length . spectralPositiveSupport) modes,
          bgssNegativeSupportSizes = fmap (length . spectralNegativeSupport) modes,
          bgssSupportCriticalities = fmap spectralSupportCriticality modes
        }

graphSpectralDistance ::
  BiomechanicalGraphSpectralSignature ->
  BiomechanicalGraphSpectralSignature ->
  Double
graphSpectralDistance expectedSignature observedSignature =
  let expectedCriticalities = bgssSupportCriticalities expectedSignature
      observedCriticalities = bgssSupportCriticalities observedSignature
      averageCriticalities = zipPadWith 1.0 (\leftCrit rightCrit -> (leftCrit + rightCrit) / 2.0) expectedCriticalities observedCriticalities
      positiveWeightedDiff =
        weightedIntDiff
          averageCriticalities
          (bgssPositiveSupportSizes expectedSignature)
          (bgssPositiveSupportSizes observedSignature)
      negativeWeightedDiff =
        weightedIntDiff
          averageCriticalities
          (bgssNegativeSupportSizes expectedSignature)
          (bgssNegativeSupportSizes observedSignature)
   in maxAbsDiff (bgssEigenvalues expectedSignature) (bgssEigenvalues observedSignature)
        + abs (bgssSpectralGap expectedSignature - bgssSpectralGap observedSignature)
        + positiveWeightedDiff
        + negativeWeightedDiff

graphSpectralCompatible ::
  BiomechanicalSpectralPolicy ->
  BiomechanicalGraphSpectralSignature ->
  BiomechanicalGraphSpectralSignature ->
  Bool
graphSpectralCompatible spectralPolicy expectedSignature observedSignature =
  maxAbsDiff (bgssEigenvalues expectedSignature) (bgssEigenvalues observedSignature) <= bspMaxEigenvalueDrift spectralPolicy
    && abs (bgssSpectralGap expectedSignature - bgssSpectralGap observedSignature) <= bspMaxGraphGapDrift spectralPolicy
    && maxIntDiff (bgssPositiveSupportSizes expectedSignature) (bgssPositiveSupportSizes observedSignature) <= bspMaxGraphSupportDrift spectralPolicy
    && maxIntDiff (bgssNegativeSupportSizes expectedSignature) (bgssNegativeSupportSizes observedSignature) <= bspMaxGraphSupportDrift spectralPolicy

maxAbsDiff :: [Double] -> [Double] -> Double
maxAbsDiff leftValues rightValues =
  maximum
    ( 0.0
        : zipWith
          (\leftValue rightValue -> abs (leftValue - rightValue))
          (padTo sharedLength leftValues)
          (padTo sharedLength rightValues)
    )
  where
    sharedLength = max (length leftValues) (length rightValues)

padTo :: Int -> [Double] -> [Double]
padTo targetLength values =
  values <> replicate (max 0 (targetLength - length values)) 0.0

maxIntDiff :: [Int] -> [Int] -> Int
maxIntDiff leftValues rightValues =
  maximum
    ( 0
        : zipWith
          (\leftValue rightValue -> abs (leftValue - rightValue))
          (padIntTo sharedLength leftValues)
          (padIntTo sharedLength rightValues)
    )
  where
    sharedLength = max (length leftValues) (length rightValues)

padIntTo :: Int -> [Int] -> [Int]
padIntTo targetLength values =
  values <> replicate (max 0 (targetLength - length values)) 0

zipPadWith :: a -> (a -> a -> b) -> [a] -> [a] -> [b]
zipPadWith defaultValue combine leftValues rightValues =
  let sharedLength = max (length leftValues) (length rightValues)
      paddedLeft = leftValues <> replicate (max 0 (sharedLength - length leftValues)) defaultValue
      paddedRight = rightValues <> replicate (max 0 (sharedLength - length rightValues)) defaultValue
   in zipWith combine paddedLeft paddedRight

weightedIntDiff :: [Double] -> [Int] -> [Int] -> Double
weightedIntDiff weights leftValues rightValues =
  let sharedLength = max (length leftValues) (length rightValues)
      paddedWeights = weights <> replicate (max 0 (sharedLength - length weights)) 1.0
      paddedLeft = padIntTo sharedLength leftValues
      paddedRight = padIntTo sharedLength rightValues
      weightedDiffs =
        zipWith3
          (\weight leftValue rightValue -> weight * fromIntegral (abs (leftValue - rightValue)))
          paddedWeights
          paddedLeft
          paddedRight
   in maximum (0.0 : weightedDiffs)

spectralGap :: [Double] -> Double
spectralGap =
  fromMaybe 0.0 . Aggregate.spectralGap . sort

adjacencyFromEdges :: Int -> [(Int, Int)] -> Map.Map Int (Set.Set Int)
adjacencyFromEdges vertexCount edgeSupports =
  foldr
    addEdgeAdjacency
    (Map.fromList (fmap (\vertexIndex -> (vertexIndex, Set.empty)) [0 :: Int .. max 0 (vertexCount - 1)]))
    edgeSupports

addEdgeAdjacency :: (Int, Int) -> Map.Map Int (Set.Set Int) -> Map.Map Int (Set.Set Int)
addEdgeAdjacency (sourceVertex, targetVertex) adjacencyMap =
  Map.insertWith Set.union targetVertex (Set.singleton sourceVertex)
    (Map.insertWith Set.union sourceVertex (Set.singleton targetVertex) adjacencyMap)
