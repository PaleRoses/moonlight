module Moonlight.Cosheaf.Tropical.Cost
  ( module Moonlight.Cosheaf.Tropical.Cost.MinPlus,
    TropicalTransition (..),
    TropicalWeightedTransition (..),
    TropicalCostModel (..),
    TropicalCostTable,
    tctColimit,
    tctRepresentativeCosts,
    tctTransitions,
    TropicalClassChoice (..),
    TropicalCosectionPlan,
    tcpCostTable,
    tcpClassChoices,
    TropicalCosectionFailure (..),
    compileTropicalCostTableFromSupportPlan,
    planTropicalCosections,
  )
where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (find)
import Moonlight.Cosheaf.Colimit
  ( CosheafColimit,
    CosheafColimitFailure (..),
    ccCosheaf,
    ccEquivalence,
    ccRepresentativeIndex,
    cosectionRepresentativeAt,
    cosectionRepresentativeKeyOf,
    cosheafColimitClassKeys,
  )
import Moonlight.Cosheaf.Cosection
  ( CosectionClassKey,
    CosectionRepKey (..),
    CosectionRepresentative (..),
    cosectionClassOfRepresentativeKey,
    cosectionClassKeyInt,
    cosectionRepKeyInt,
  )
import Moonlight.Cosheaf.Finite
  ( CostalkKey (..),
    ccMorphism,
    ccSourceObjectKey,
    ccSourceToTarget,
    ccTargetObjectKey,
    finiteCostalkAtObjectKey,
    finiteCostalkValueAt,
  )
import Moonlight.Cosheaf.Support
  ( CosheafSupportPlan,
    PreparedCosheafSupport,
    pcsCorestrictions,
    pcsCostalkKeysByObject,
    prepareCosheafSupport,
  )
import Moonlight.Cosheaf.Tropical.Cost.MinPlus
import Moonlight.Cosheaf.Tropical.Cost.Types
import Moonlight.Sheaf.Index.Dense
  ( denseIndexIndexedValues,
  )
import Moonlight.Sheaf.Section.Congruence.Equivalence.Relation
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectKey (..),
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    Site (..),
  )

tctColimit :: TropicalCostTable site value -> CosheafColimit site value
tctColimit =
  tropicalCostTableColimitInternal

tctRepresentativeCosts :: TropicalCostTable site value -> IntMap MinPlusWeight
tctRepresentativeCosts =
  tropicalCostTableRepresentativeCostsInternal

tctTransitions ::
  TropicalCostTable site value ->
  [TropicalWeightedTransition (SiteObject site) (SiteMorphism site) value]
tctTransitions =
  tropicalCostTableTransitionsInternal

tcpCostTable :: TropicalCosectionPlan site value -> TropicalCostTable site value
tcpCostTable =
  tropicalCosectionPlanCostTableInternal

tcpClassChoices ::
  TropicalCosectionPlan site value ->
  IntMap (TropicalClassChoice (SiteObject site) value)
tcpClassChoices =
  tropicalCosectionPlanClassChoicesInternal

compileTropicalCostTableFromSupportPlan ::
  (Site site, Ord value) =>
  CosheafSupportPlan ->
  CosheafColimit site value ->
  TropicalCostModel site value ->
  Either
    (TropicalCosectionFailure (SiteObject site) (SiteMorphism site) value)
    (TropicalCostTable site value)
compileTropicalCostTableFromSupportPlan supportPlan colimit costModel = do
  preparedSupport <-
    first TropicalSupportInvalid $
      prepareCosheafSupport (ccCosheaf colimit) supportPlan
  representativeCosts <-
    IntMap.fromList
      <$> traverse compileRepresentativeCost (representativesWithKeys colimit)
  transitions <-
    cosheafColimitTransitionsFromPreparedSupport preparedSupport colimit
  weightedTransitions <-
    traverse compileTransitionCost transitions
  pure
    TropicalCostTable
      { tropicalCostTableColimitInternal = colimit,
        tropicalCostTableRepresentativeCostsInternal = representativeCosts,
        tropicalCostTableTransitionsInternal = weightedTransitions
      }
  where
    compileRepresentativeCost (repKey, representativeValue) = do
      costValue <-
        tcmRepresentativeCost costModel representativeValue
      pure (cosectionRepKeyInt repKey, costValue)

    compileTransitionCost (sourceKey, targetKey, transitionValue) = do
      costValue <-
        tcmTransitionCost costModel transitionValue
      pure
        TropicalWeightedTransition
          { twtTransition = transitionValue,
            twtSourceKey = sourceKey,
            twtTargetKey = targetKey,
            twtWeight = costValue
          }

planTropicalCosections ::
  TropicalCostTable site value ->
  Either
    (TropicalCosectionFailure (SiteObject site) (SiteMorphism site) value)
    (TropicalCosectionPlan site value)
planTropicalCosections costTable = do
  let distances =
        relaxDistances costTable
  case find (canRelax distances) (tctTransitions costTable) of
    Just transitionValue ->
      Left (TropicalUnboundedCost (twtTargetKey transitionValue))
    Nothing -> do
      choices <-
        foldM insertChoice IntMap.empty (IntMap.toAscList distances)
      traverse_ (requireClass choices) (cosheafColimitClassKeys (tctColimit costTable))
      pure
        TropicalCosectionPlan
          { tropicalCosectionPlanCostTableInternal = costTable,
            tropicalCosectionPlanClassChoicesInternal = choices
          }
  where
    insertChoice choices (repKeyInt, costValue) = do
      let repKey =
            CosectionRepKey repKeyInt
      representativeValue <-
        maybe
          (Left (TropicalRepresentativeMissing repKey))
          Right
          (cosectionRepresentativeAt repKey (tctColimit costTable))
      classRep <-
        maybe
          (Left (TropicalRepresentativeMissing repKey))
          Right
          (equivalenceRepresentative (ccEquivalence (tctColimit costTable)) repKey)
      let classKey =
            cosectionClassOfRepresentativeKey classRep
          choice =
            TropicalClassChoice
              { tccClassKey = classKey,
                tccRepresentativeKey = repKey,
                tccRepresentative = representativeValue,
                tccCost = costValue
              }
      pure (IntMap.insertWith chooseLowerCost (cosectionClassKeyInt classKey) choice choices)

    chooseLowerCost :: TropicalClassChoice obj value -> TropicalClassChoice obj value -> TropicalClassChoice obj value
    chooseLowerCost newChoice oldChoice =
      if tccCost newChoice < tccCost oldChoice
        then newChoice
        else oldChoice

    requireClass :: IntMap choice -> CosectionClassKey -> Either (TropicalCosectionFailure obj mor value) ()
    requireClass choices classKey =
      if IntMap.member (cosectionClassKeyInt classKey) choices
        then Right ()
        else Left (TropicalEmptyColimitClass classKey)

relaxDistances :: TropicalCostTable site value -> IntMap MinPlusWeight
relaxDistances costTable =
  relaxToFixpoint passBound initialFrontier initialDistances
  where
    initialDistances =
      tctRepresentativeCosts costTable

    passBound =
      max 0 (IntMap.size initialDistances - 1)

    initialFrontier =
      IntMap.keysSet (IntMap.filter (/= minPlusZero) initialDistances)

    transitionsBySource =
      IntMap.fromListWith
        (<>)
        [ (cosectionRepKeyInt (twtSourceKey transitionValue), [transitionValue])
          | transitionValue <- tctTransitions costTable
        ]

    relaxToFixpoint remainingPasses frontier distances
      | remainingPasses <= 0 || IntSet.null frontier =
          distances
      | otherwise =
          let (relaxedDistances, changedTargets) =
                IntSet.foldl' relaxFromSource (distances, IntSet.empty) frontier
           in relaxToFixpoint (remainingPasses - 1) changedTargets relaxedDistances

    relaxFromSource accumulated sourceKeyInt =
      foldl'
        relaxTransition
        accumulated
        (IntMap.findWithDefault [] sourceKeyInt transitionsBySource)

    relaxTransition ::
      (IntMap MinPlusWeight, IntSet.IntSet) ->
      TropicalWeightedTransition obj mor value ->
      (IntMap MinPlusWeight, IntSet.IntSet)
    relaxTransition accumulated@(distances, changedTargets) transitionValue =
      case IntMap.lookup (cosectionRepKeyInt (twtSourceKey transitionValue)) distances of
        Nothing ->
          accumulated
        Just sourceCost ->
          let candidateCost =
                minPlusMul sourceCost (twtWeight transitionValue)
              targetKeyInt =
                cosectionRepKeyInt (twtTargetKey transitionValue)
              improvesTarget =
                case IntMap.lookup targetKeyInt distances of
                  Nothing -> True
                  Just targetCost -> candidateCost < targetCost
           in if improvesTarget
                then
                  ( IntMap.insert targetKeyInt candidateCost distances,
                    IntSet.insert targetKeyInt changedTargets
                  )
                else accumulated

canRelax ::
  IntMap MinPlusWeight ->
  TropicalWeightedTransition obj mor value ->
  Bool
canRelax distances transitionValue =
  case IntMap.lookup (cosectionRepKeyInt (twtSourceKey transitionValue)) distances of
    Nothing ->
      False
    Just sourceCost ->
      let candidateCost =
            minPlusMul sourceCost (twtWeight transitionValue)
       in case IntMap.lookup (cosectionRepKeyInt (twtTargetKey transitionValue)) distances of
            Nothing -> True
            Just targetCost -> candidateCost < targetCost

representativesWithKeys ::
  CosheafColimit site value ->
  [(CosectionRepKey, CosectionRepresentative (SiteObject site) value)]
representativesWithKeys =
  denseIndexIndexedValues . ccRepresentativeIndex

cosheafColimitTransitionsFromPreparedSupport ::
  (Site site, Ord value) =>
  PreparedCosheafSupport site value ->
  CosheafColimit site value ->
  Either
    (TropicalCosectionFailure (SiteObject site) (SiteMorphism site) value)
    [(CosectionRepKey, CosectionRepKey, TropicalTransition (SiteObject site) (SiteMorphism site) value)]
cosheafColimitTransitionsFromPreparedSupport preparedSupport colimit =
  fmap concat $
    traverse transitionPairs (pcsCorestrictions preparedSupport)
  where
    transitionPairs corestrictionValue =
      traverse (transitionFor corestrictionValue) (retainedSourcePairs corestrictionValue)

    retainedSourcePairs corestrictionValue =
      IntSet.toAscList
        ( IntMap.findWithDefault
            IntSet.empty
            (unObjectKey (ccSourceObjectKey corestrictionValue))
            (pcsCostalkKeysByObject preparedSupport)
        )

    transitionFor corestrictionValue sourceKeyInt = do
      targetKey <-
        maybe
          (Left (TropicalColimitMalformed (CosheafColimitCorestrictionMalformed (ccMorphism corestrictionValue) (CostalkKey sourceKeyInt))))
          Right
          (IntMap.lookup sourceKeyInt (ccSourceToTarget corestrictionValue))
      sourceValue <-
        colimitCostalkValue (ccSourceObjectKey corestrictionValue) (CostalkKey sourceKeyInt)
      targetValue <-
        colimitCostalkValue (ccTargetObjectKey corestrictionValue) targetKey
      let sourceRepresentative =
            CosectionRepresentative
              { cosectionRepObject = cmSource (ccMorphism corestrictionValue),
                cosectionRepValue = sourceValue
              }
          targetRepresentative =
            CosectionRepresentative
              { cosectionRepObject = cmTarget (ccMorphism corestrictionValue),
                cosectionRepValue = targetValue
              }
      sourceRepKey <-
        first TropicalColimitMalformed $
          cosectionRepresentativeKeyOf sourceRepresentative colimit
      targetRepKey <-
        first TropicalColimitMalformed $
          cosectionRepresentativeKeyOf targetRepresentative colimit
      pure
        ( sourceRepKey,
          targetRepKey,
          TropicalTransition
            { tropicalTransitionMorphism = ccMorphism corestrictionValue,
              tropicalTransitionSource = sourceRepresentative,
              tropicalTransitionTarget = targetRepresentative
            }
        )

    colimitCostalkValue objectKey costalkKey = do
      costalkValue <-
        maybe
          (Left (TropicalColimitMalformed (CosheafColimitCostalkMissing objectKey)))
          Right
          (finiteCostalkAtObjectKey objectKey (ccCosheaf colimit))
      maybe
        (Left (TropicalColimitMalformed (CosheafColimitCostalkValueMissing objectKey costalkKey)))
        Right
        (finiteCostalkValueAt costalkKey costalkValue)
