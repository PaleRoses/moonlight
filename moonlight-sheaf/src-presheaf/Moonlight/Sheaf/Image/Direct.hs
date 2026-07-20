{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | Direct image along a site map: pushforward presheaves and their cones.
module Moonlight.Sheaf.Image.Direct
  ( DirectImageIndexObject (..),
    DirectImageCone,
    DirectImageBuildFailure (..),
    DirectImageRestrictionFailure (..),
    DirectImageMismatch (..),
    DirectImageEnumerationCost (..),
    mkDirectImageCone,
    directImageConeTarget,
    directImageConeAssignments,
    directImageConeValueAt,
    pushforwardFinitePresheaf,
  )
where

import Data.Bifunctor (first)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Numeric.Natural (Natural)
import Moonlight.Core (note)
import Moonlight.Sheaf.Index.Dense (mkDenseIndex)
import Moonlight.Sheaf.Presheaf.Enumeration
  ( FiniteEnumerationBudget,
    assignmentUpperBound,
    guardEnumerationBudget,
  )
import Moonlight.Sheaf.Presheaf.Finite
  ( FiniteFiber (..),
    FinitePresheaf (..),
    finiteFiberAt,
    finiteFiberValues,
  )
import Moonlight.Sheaf.Section.ObjectIndex (mkObjectIndex)
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    Site (..),
    siteMorphismUniverse,
  )
import Moonlight.Sheaf.Site.Construction.Map
  ( ContinuousSiteMap,
    continuousFiniteSiteMap,
    finiteSiteMapSource,
    finiteSiteMapTarget,
    siteMapMorphismImage,
    siteMapObjectImage,
  )

data DirectImageIndexObject sourceObj targetObj targetMor = DirectImageIndexObject
  { directImageIndexSourceObject :: !sourceObj,
    directImageIndexTargetMorphism :: !(CheckedMorphism targetObj targetMor)
  }
  deriving stock (Eq, Ord, Show)

data DirectImageCone sourceObj targetObj targetMor value = DirectImageCone
  { directImageConeTargetInternal :: !targetObj,
    directImageConeAssignmentsInternal :: !(Map (DirectImageIndexObject sourceObj targetObj targetMor) value)
  }
  deriving stock (Eq, Ord, Show)

data DirectImageEnumerationCost = DirectImageEnumerationCost
  { diecIndexObjectCount :: !Natural,
    diecAssignmentUpperBound :: !Natural
  }
  deriving stock (Eq, Show)

data DirectImageRestrictionFailure sourceObj targetObj targetMor
  = DirectImageRestrictionConeTargetMismatch !(CheckedMorphism targetObj targetMor) !targetObj
  | DirectImageRestrictionProjectionMissing !(CheckedMorphism targetObj targetMor)
  | DirectImageRestrictionTargetCompositionMissing
      !(CheckedMorphism targetObj targetMor)
      !(CheckedMorphism targetObj targetMor)
  | DirectImageRestrictionAssignmentMissing
      !(CheckedMorphism targetObj targetMor)
      !(DirectImageIndexObject sourceObj targetObj targetMor)
      !(DirectImageIndexObject sourceObj targetObj targetMor)
  deriving stock (Eq, Show)

data DirectImageMismatch sourceObj targetObj targetMor value mismatch
  = DirectImageConeTargetMismatch !targetObj !targetObj
  | DirectImageConeDomainMismatch
      !targetObj
      ![DirectImageIndexObject sourceObj targetObj targetMor]
      ![DirectImageIndexObject sourceObj targetObj targetMor]
  | DirectImageAssignmentMismatch
      !(DirectImageIndexObject sourceObj targetObj targetMor)
      !value
      !value
      ![mismatch]
  deriving stock (Eq, Show)

data DirectImageBuildFailure sourceObj sourceMor targetObj targetMor value mismatch restrictionFailure
  = DirectImageSourceSiteObjectMismatch ![sourceObj] ![sourceObj]
  | DirectImageSourceSiteMorphismMismatch
      ![CheckedMorphism sourceObj sourceMor]
      ![CheckedMorphism sourceObj sourceMor]
  | DirectImageObjectImageMissing !sourceObj
  | DirectImageTargetCompositionMissing
      !(CheckedMorphism targetObj targetMor)
      !(CheckedMorphism targetObj targetMor)
  | DirectImageProjectionIndexMissing
      !(CheckedMorphism targetObj targetMor)
      !(DirectImageIndexObject sourceObj targetObj targetMor)
      !(CheckedMorphism targetObj targetMor)
  | DirectImageSourceFiberMissing !sourceObj
  | DirectImageCandidateAssignmentMissing !(DirectImageIndexObject sourceObj targetObj targetMor)
  | DirectImageCandidateRestrictionFailed
      !(CheckedMorphism sourceObj sourceMor)
      !value
      !restrictionFailure
  | DirectImageEnumerationBudgetExceeded !targetObj !DirectImageEnumerationCost
  deriving stock (Eq, Show)

data DirectImageIndexMorphism sourceObj sourceMor targetObj targetMor = DirectImageIndexMorphism
  { diimSourceIndex :: !(DirectImageIndexObject sourceObj targetObj targetMor),
    diimTargetIndex :: !(DirectImageIndexObject sourceObj targetObj targetMor),
    diimSourceMorphism :: !(CheckedMorphism sourceObj sourceMor)
  }
  deriving stock (Eq, Ord, Show)

data DirectImageTerminalAssignment sourceObj sourceMor targetObj targetMor = DirectImageTerminalAssignment
  { ditaIndexObject :: !(DirectImageIndexObject sourceObj targetObj targetMor),
    ditaSourceMorphism :: !(CheckedMorphism sourceObj sourceMor)
  }

data DirectImageTerminalFiberPlan sourceObj sourceMor targetObj targetMor = DirectImageTerminalFiberPlan
  { ditfpSourceObject :: !sourceObj,
    ditfpAssignments :: ![DirectImageTerminalAssignment sourceObj sourceMor targetObj targetMor]
  }

data DirectImageTerminalPlan sourceObj sourceMor targetObj targetMor = DirectImageTerminalPlan
  { ditpFiberEntries :: ![(targetObj, DirectImageTerminalFiberPlan sourceObj sourceMor targetObj targetMor)],
    ditpFiberByTarget :: !(Map targetObj (DirectImageTerminalFiberPlan sourceObj sourceMor targetObj targetMor))
  }

data DirectAssignmentDomain key value = DirectAssignmentDomain
  { dadIndexObject :: !key,
    dadValues :: ![(Int, value)]
  }

data PartialDirectAssignment key value = PartialDirectAssignment
  { pdaAssignments :: !(Map key value),
    pdaPositions :: !(Map key Int)
  }

data DirectImagePlan sourceObj sourceMor targetObj targetMor = DirectImagePlan
  { dipIndexByTarget :: !(Map targetObj [DirectImageIndexObject sourceObj targetObj targetMor]),
    dipArrowsByTarget :: !(Map targetObj [DirectImageIndexMorphism sourceObj sourceMor targetObj targetMor]),
    dipProjectionByMorphism ::
      !( Map
           (CheckedMorphism targetObj targetMor)
           [(DirectImageIndexObject sourceObj targetObj targetMor, DirectImageIndexObject sourceObj targetObj targetMor)]
       )
  }

data DirectImageExecution sourceObj sourceMor targetObj targetMor
  = DirectImageTerminalExecution !(DirectImageTerminalPlan sourceObj sourceMor targetObj targetMor)
  | DirectImageIndexedExecution !(DirectImagePlan sourceObj sourceMor targetObj targetMor)

mkDirectImageTerminalPlan ::
  Ord targetObj =>
  [(targetObj, DirectImageTerminalFiberPlan sourceObj sourceMor targetObj targetMor)] ->
  DirectImageTerminalPlan sourceObj sourceMor targetObj targetMor
mkDirectImageTerminalPlan entries =
  DirectImageTerminalPlan
    { ditpFiberEntries = entries,
      ditpFiberByTarget = Map.fromList entries
    }
{-# INLINE mkDirectImageTerminalPlan #-}

mkDirectImageCone ::
  targetObj ->
  Map (DirectImageIndexObject sourceObj targetObj targetMor) value ->
  DirectImageCone sourceObj targetObj targetMor value
mkDirectImageCone =
  DirectImageCone
{-# INLINE mkDirectImageCone #-}

directImageConeTarget :: DirectImageCone sourceObj targetObj targetMor value -> targetObj
directImageConeTarget =
  directImageConeTargetInternal
{-# INLINE directImageConeTarget #-}

directImageConeAssignments ::
  DirectImageCone sourceObj targetObj targetMor value ->
  Map (DirectImageIndexObject sourceObj targetObj targetMor) value
directImageConeAssignments =
  directImageConeAssignmentsInternal
{-# INLINE directImageConeAssignments #-}

directImageConeValueAt ::
  (Ord sourceObj, Ord targetObj, Ord targetMor) =>
  DirectImageIndexObject sourceObj targetObj targetMor ->
  DirectImageCone sourceObj targetObj targetMor value ->
  Maybe value
directImageConeValueAt indexObject =
  Map.lookup indexObject . directImageConeAssignmentsInternal
{-# INLINE directImageConeValueAt #-}

pushforwardFinitePresheaf ::
  forall source target value mismatch restrictionFailure.
  ( Site source,
    Site target,
    Ord (SiteMorphism source),
    Ord (SiteMorphism target),
    Ord value
  ) =>
  FiniteEnumerationBudget ->
  ContinuousSiteMap source target ->
  FinitePresheaf source value mismatch restrictionFailure ->
  Either
    ( DirectImageBuildFailure
        (SiteObject source)
        (SiteMorphism source)
        (SiteObject target)
        (SiteMorphism target)
        value
        mismatch
        restrictionFailure
    )
    ( FinitePresheaf
        target
        (DirectImageCone (SiteObject source) (SiteObject target) (SiteMorphism target) value)
        (DirectImageMismatch (SiteObject source) (SiteObject target) (SiteMorphism target) value mismatch)
        (DirectImageRestrictionFailure (SiteObject source) (SiteObject target) (SiteMorphism target))
    )
pushforwardFinitePresheaf budget continuousMap sourcePresheaf = do
  same DirectImageSourceSiteObjectMismatch (siteObjects sourceSite) (siteObjects (fpSite sourcePresheaf))
  same DirectImageSourceSiteMorphismMismatch (siteMorphismUniverse sourceSite) (siteMorphismUniverse (fpSite sourcePresheaf))
  directImageExecution <- prepareExecution
  let restrictCone targetMorphism coneValue =
        case directImageExecution of
          DirectImageTerminalExecution terminalPlan ->
            restrictConeForTerminal terminalPlan targetMorphism coneValue
          DirectImageIndexedExecution indexedPlan ->
            restrictConeForIndexed indexedPlan targetMorphism coneValue
      restrictConeForIndexed ::
        DirectImagePlan
          (SiteObject source)
          (SiteMorphism source)
          (SiteObject target)
          (SiteMorphism target) ->
        CheckedMorphism (SiteObject target) (SiteMorphism target) ->
        DirectImageCone (SiteObject source) (SiteObject target) (SiteMorphism target) value ->
        Either
          (DirectImageRestrictionFailure (SiteObject source) (SiteObject target) (SiteMorphism target))
          (DirectImageCone (SiteObject source) (SiteObject target) (SiteMorphism target) value)
      restrictConeForIndexed directImagePlan targetMorphism coneValue
        | directImageConeTarget coneValue /= cmTarget targetMorphism =
            Left (DirectImageRestrictionConeTargetMismatch targetMorphism (directImageConeTarget coneValue))
        | otherwise = do
            projectionEntries <-
              note
                (DirectImageRestrictionProjectionMissing targetMorphism)
                (Map.lookup targetMorphism (dipProjectionByMorphism directImagePlan))
            DirectImageCone (cmSource targetMorphism) . Map.fromList
              <$> traverse
                (\(outputIndex, inputIndex) ->
                  (outputIndex,)
                    <$> note
                      (DirectImageRestrictionAssignmentMissing targetMorphism outputIndex inputIndex)
                      (directImageConeValueAt inputIndex coneValue)
                )
                projectionEntries
      restrictConeForTerminal terminalPlan targetMorphism coneValue
        | directImageConeTarget coneValue /= cmTarget targetMorphism =
            Left (DirectImageRestrictionConeTargetMismatch targetMorphism (directImageConeTarget coneValue))
        | targetMorphism `notElem` targetMorphisms =
            Left (DirectImageRestrictionProjectionMissing targetMorphism)
        | otherwise = do
            targetFiberPlan <-
              note
                (DirectImageRestrictionProjectionMissing targetMorphism)
                (Map.lookup (cmSource targetMorphism) (ditpFiberByTarget terminalPlan))
            DirectImageCone (cmSource targetMorphism) . Map.fromList
              <$> traverse
                (projectTerminalAssignment targetMorphism coneValue)
                (ditfpAssignments targetFiberPlan)
      projectTerminalAssignment targetMorphism coneValue terminalAssignment = do
        let outputIndex =
              ditaIndexObject terminalAssignment
        composedMorphism <-
          note
            (DirectImageRestrictionTargetCompositionMissing targetMorphism (directImageIndexTargetMorphism outputIndex))
            (composeChecked targetSite targetMorphism (directImageIndexTargetMorphism outputIndex))
        let inputIndex =
              DirectImageIndexObject (directImageIndexSourceObject outputIndex) composedMorphism
        (outputIndex,)
          <$> note
            (DirectImageRestrictionAssignmentMissing targetMorphism outputIndex inputIndex)
            (directImageConeValueAt inputIndex coneValue)
  rawFibers <-
    case directImageExecution of
      DirectImageTerminalExecution terminalPlan ->
        Map.fromList <$> traverse (uncurry directTerminalFiber) (ditpFiberEntries terminalPlan)
      DirectImageIndexedExecution indexedPlan ->
        Map.fromList <$> traverse (directIndexedFiber indexedPlan) (siteObjects targetSite)
  pure
    FinitePresheaf
      { fpSite = targetSite,
        fpObjectIndex = mkObjectIndex (siteObjects targetSite),
        fpFibers = Map.mapWithKey directImageFiniteFiber rawFibers,
        fpRestrict = restrictCone,
        fpMismatches = coneMismatches,
        fpNormalize = normalizeCone
      }
  where
    siteMapValue =
      continuousFiniteSiteMap continuousMap

    sourceSite =
      finiteSiteMapSource siteMapValue

    targetSite =
      finiteSiteMapTarget siteMapValue

    sourceMorphisms =
      siteMorphismUniverse sourceSite

    targetMorphisms =
      siteMorphismUniverse targetSite

    prepareExecution = do
      sourceObjectImages <- traverse sourceObjectImageEntry (siteObjects sourceSite)
      let indexByTarget =
            Map.fromList (indexEntryForTarget sourceObjectImages <$> siteObjects targetSite)
      case prepareTerminalPlan sourceObjectImages indexByTarget of
        Just terminalPlan ->
          Right (DirectImageTerminalExecution terminalPlan)
        Nothing ->
          DirectImageIndexedExecution <$> prepareIndexedPlan indexByTarget

    sourceObjectImageEntry sourceObject =
      (sourceObject,)
        <$> note
          (DirectImageObjectImageMissing sourceObject)
          (siteMapObjectImage sourceObject siteMapValue)

    prepareIndexedPlan indexByTarget = do
      let indexSetByTarget =
            Set.fromList <$> indexByTarget
      DirectImagePlan indexByTarget
        <$> (Map.fromList <$> traverse (arrowEntryForTarget indexByTarget) (siteObjects targetSite))
        <*> (Map.fromList <$> traverse (projectionEntryForMorphism indexByTarget indexSetByTarget) targetMorphisms)

    indexEntryForTarget sourceObjectImages targetObject =
      (targetObject, concatMap (indexObjectsForSource targetObject) sourceObjectImages)

    indexObjectsForSource targetObject (sourceObject, targetImage) =
      [ DirectImageIndexObject sourceObject targetMorphism
      | targetMorphism <- targetMorphisms,
        cmSource targetMorphism == targetImage,
        cmTarget targetMorphism == targetObject
      ]

    prepareTerminalPlan sourceObjectImages indexByTarget =
      mkDirectImageTerminalPlan
        <$> traverse terminalFiberEntry (siteObjects targetSite)
      where
        sourceMorphismsByImage =
          Map.fromListWith
            (<>)
            [ ((cmSource sourceMorphism, targetMorphism), [sourceMorphism])
            | sourceMorphism <- sourceMorphisms,
              Just targetMorphism <- [siteMapMorphismImage sourceMorphism siteMapValue]
            ]

        terminalFiberEntry targetObject = do
          terminalSourceObject <-
            single
              [ sourceObject
              | (sourceObject, targetImage) <- sourceObjectImages,
                targetImage == targetObject
              ]
          let terminalIndex =
                DirectImageIndexObject terminalSourceObject (identityAt targetSite targetObject)
              indexObjects =
                Map.findWithDefault [] targetObject indexByTarget
          if terminalIndex `elem` indexObjects
            then do
              assignments <- traverse (terminalAssignmentFor terminalSourceObject) indexObjects
              pure
                ( targetObject,
                  DirectImageTerminalFiberPlan
                    { ditfpSourceObject = terminalSourceObject,
                      ditfpAssignments = assignments
                    }
                )
            else Nothing

        terminalAssignmentFor terminalSourceObject indexObject =
          DirectImageTerminalAssignment indexObject
            <$> single
              [ sourceMorphism
              | sourceMorphism <-
                  Map.findWithDefault
                    []
                    (directImageIndexSourceObject indexObject, directImageIndexTargetMorphism indexObject)
                    sourceMorphismsByImage,
                cmTarget sourceMorphism == terminalSourceObject
              ]

    arrowEntryForTarget indexByTarget targetObject =
      pure (targetObject, indexArrows (Map.findWithDefault [] targetObject indexByTarget))

    indexArrows indexObjects =
      [ DirectImageIndexMorphism sourceIndex targetIndex sourceMorphism
      | sourceMorphism <- sourceMorphisms,
        sourceIndex <- indexObjects,
        targetIndex <- indexObjects,
        directImageIndexSourceObject sourceIndex == cmSource sourceMorphism,
        directImageIndexSourceObject targetIndex == cmTarget sourceMorphism,
        triangleCommutes sourceMorphism sourceIndex targetIndex
      ]

    triangleCommutes sourceMorphism sourceIndex targetIndex =
      maybe False commutes (siteMapMorphismImage sourceMorphism siteMapValue)
      where
        commutes targetMorphism =
          composeChecked targetSite (directImageIndexTargetMorphism targetIndex) targetMorphism
            == Just (directImageIndexTargetMorphism sourceIndex)

    projectionEntryForMorphism
      (indexByTarget :: Map (SiteObject target) [DirectImageIndexObject (SiteObject source) (SiteObject target) (SiteMorphism target)])
      (indexSetByTarget :: Map (SiteObject target) (Set (DirectImageIndexObject (SiteObject source) (SiteObject target) (SiteMorphism target))))
      (targetMorphism :: CheckedMorphism (SiteObject target) (SiteMorphism target)) =
        (targetMorphism,) <$> traverse project (Map.findWithDefault [] (cmSource targetMorphism) indexByTarget)
      where
        inputIndexes =
          Map.findWithDefault Set.empty (cmTarget targetMorphism) indexSetByTarget

        project outputIndex = do
          composedMorphism <-
            note
              (DirectImageTargetCompositionMissing targetMorphism (directImageIndexTargetMorphism outputIndex))
              (composeChecked targetSite targetMorphism (directImageIndexTargetMorphism outputIndex))
          let inputIndex = DirectImageIndexObject (directImageIndexSourceObject outputIndex) composedMorphism
          if Set.member inputIndex inputIndexes
            then Right (outputIndex, inputIndex)
            else Left (DirectImageProjectionIndexMissing targetMorphism outputIndex composedMorphism)

    directIndexedFiber (directImagePlan :: DirectImagePlan (SiteObject source) (SiteMorphism source) (SiteObject target) (SiteMorphism target)) targetObject = do
      let indexObjects = Map.findWithDefault [] targetObject (dipIndexByTarget directImagePlan)
      domains <- traverse assignmentDomain indexObjects
      let cost =
            DirectImageEnumerationCost
              (fromIntegral (length indexObjects))
              (assignmentUpperBound (directAssignmentDomain <$> domains))
      guardEnumerationBudget budget (diecAssignmentUpperBound cost) (DirectImageEnumerationBudgetExceeded targetObject cost)
      cones <-
        fmap (DirectImageCone targetObject)
          <$> compatibleAssignments targetObject domains
      pure (targetObject, cones)
      where
        directAssignmentDomain ::
          DirectAssignmentDomain
            (DirectImageIndexObject (SiteObject source) (SiteObject target) (SiteMorphism target))
            value ->
          ( DirectImageIndexObject (SiteObject source) (SiteObject target) (SiteMorphism target)
          , [value]
          )
        directAssignmentDomain domainValue =
          (dadIndexObject domainValue, snd <$> dadValues domainValue)

        compatibleAssignments objectValue domains =
          fmap
            (fmap pdaAssignments . List.sortOn (assignmentOriginalOrder domains))
            ( List.foldl'
                extendAssignments
                ( Right
                    [ PartialDirectAssignment
                        { pdaAssignments = Map.empty,
                          pdaPositions = Map.empty
                        }
                    ]
                )
                (orderedDirectDomains constraintsByIndex domains)
            )
          where
            constraintsByIndex =
              directConstraintsByIndex objectValue

            extendAssignments partialAssignments domainValue =
              partialAssignments >>= fmap concat . traverse (extendAssignment domainValue)

            extendAssignment domainValue partialAssignment =
              fmap concat $
                traverse
                  (extendAssignmentValue domainValue partialAssignment)
                  (dadValues domainValue)

            extendAssignmentValue domainValue partialAssignment (valueIndex, value) = do
              let indexObject = dadIndexObject domainValue
                  nextAssignment =
                    PartialDirectAssignment
                      { pdaAssignments =
                          Map.insert
                            indexObject
                            value
                            (pdaAssignments partialAssignment),
                        pdaPositions =
                          Map.insert
                            indexObject
                            valueIndex
                            (pdaPositions partialAssignment)
                      }
              compatible <-
                partialAssignmentSatisfiesIndex
                  nextAssignment
                  (Map.findWithDefault [] indexObject constraintsByIndex)
              pure [nextAssignment | compatible]

        orderedDirectDomains ::
          Map
            (DirectImageIndexObject (SiteObject source) (SiteObject target) (SiteMorphism target))
            [ DirectImageIndexMorphism
                (SiteObject source)
                (SiteMorphism source)
                (SiteObject target)
                (SiteMorphism target)
            ] ->
          [ DirectAssignmentDomain
              (DirectImageIndexObject (SiteObject source) (SiteObject target) (SiteMorphism target))
              value
          ] ->
          [ DirectAssignmentDomain
              (DirectImageIndexObject (SiteObject source) (SiteObject target) (SiteMorphism target))
              value
          ]
        orderedDirectDomains constraintsByIndex =
          List.sortOn
            ( \domainValue ->
                ( negate
                    (length (Map.findWithDefault [] (dadIndexObject domainValue) constraintsByIndex)),
                  dadIndexObject domainValue
                )
            )

        directConstraintsByIndex objectValue =
          Map.fromListWith
            (flip (<>))
            [ (indexObject, [arrowValue])
            | arrowValue <- Map.findWithDefault [] objectValue (dipArrowsByTarget directImagePlan),
              indexObject <- [diimSourceIndex arrowValue, diimTargetIndex arrowValue]
            ]

        partialAssignmentSatisfiesIndex partialAssignment =
          fmap and . traverse (partialAssignmentSatisfiesArrow partialAssignment)

        partialAssignmentSatisfiesArrow partialAssignment arrowValue =
          case
            ( Map.lookup (diimSourceIndex arrowValue) (pdaAssignments partialAssignment),
              Map.lookup (diimTargetIndex arrowValue) (pdaAssignments partialAssignment)
            )
          of
            (Just _sourceValue, Just _targetValue) ->
              compatibleArrow (pdaAssignments partialAssignment) arrowValue
            _ ->
              Right True

        assignmentOriginalOrder ::
          [ DirectAssignmentDomain
              (DirectImageIndexObject (SiteObject source) (SiteObject target) (SiteMorphism target))
              value
          ] ->
          PartialDirectAssignment
            (DirectImageIndexObject (SiteObject source) (SiteObject target) (SiteMorphism target))
            value ->
          [Int]
        assignmentOriginalOrder domains partialAssignment =
          [ valueIndex
          | domainValue <- domains,
            Just valueIndex <- [Map.lookup (dadIndexObject domainValue) (pdaPositions partialAssignment)]
          ]

    directImageFiniteFiber ::
      SiteObject target ->
      [DirectImageCone (SiteObject source) (SiteObject target) (SiteMorphism target) value] ->
      FiniteFiber
        (SiteObject target)
        (DirectImageCone (SiteObject source) (SiteObject target) (SiteMorphism target) value)
    directImageFiniteFiber objectValue coneValues =
      FiniteFiber
        { ffObject = objectValue,
          ffValues = mkDenseIndex coneValues
        }

    assignmentDomain indexObject =
      DirectAssignmentDomain indexObject . zip [0 :: Int ..] . finiteFiberValues
        <$> note
          (DirectImageSourceFiberMissing (directImageIndexSourceObject indexObject))
          (finiteFiberAt (directImageIndexSourceObject indexObject) sourcePresheaf)

    compatibleArrow assignments arrowValue = do
      sourceValue <-
        note
          (DirectImageCandidateAssignmentMissing (diimSourceIndex arrowValue))
          (Map.lookup (diimSourceIndex arrowValue) assignments)
      targetValue <-
        note
          (DirectImageCandidateAssignmentMissing (diimTargetIndex arrowValue))
          (Map.lookup (diimTargetIndex arrowValue) assignments)
      restrictedValue <-
        first
          (DirectImageCandidateRestrictionFailed (diimSourceMorphism arrowValue) targetValue)
          (fpRestrict sourcePresheaf (diimSourceMorphism arrowValue) targetValue)
      pure . null $
        fpMismatches
          sourcePresheaf
          (cmSource (diimSourceMorphism arrowValue))
          sourceValue
          restrictedValue

    directTerminalFiber ::
      SiteObject target ->
      DirectImageTerminalFiberPlan (SiteObject source) (SiteMorphism source) (SiteObject target) (SiteMorphism target) ->
      Either
        ( DirectImageBuildFailure
            (SiteObject source)
            (SiteMorphism source)
            (SiteObject target)
            (SiteMorphism target)
            value
            mismatch
            restrictionFailure
        )
        (SiteObject target, [DirectImageCone (SiteObject source) (SiteObject target) (SiteMorphism target) value])
    directTerminalFiber targetObject terminalFiberPlan = do
      terminalFiber <-
        note
          (DirectImageSourceFiberMissing (ditfpSourceObject terminalFiberPlan))
          (finiteFiberAt (ditfpSourceObject terminalFiberPlan) sourcePresheaf)
      let terminalValues =
            finiteFiberValues terminalFiber
          cost =
            DirectImageEnumerationCost
              (fromIntegral (length (ditfpAssignments terminalFiberPlan)))
              (fromIntegral (length terminalValues))
      guardEnumerationBudget budget (diecAssignmentUpperBound cost) (DirectImageEnumerationBudgetExceeded targetObject cost)
      (targetObject,) <$> traverse (terminalCone targetObject terminalFiberPlan) terminalValues

    terminalCone targetObject terminalFiberPlan terminalValue =
      DirectImageCone targetObject . Map.fromList
        <$> traverse (terminalConeAssignment (ditfpSourceObject terminalFiberPlan) terminalValue) (ditfpAssignments terminalFiberPlan)

    terminalConeAssignment terminalSourceObject terminalValue terminalAssignment
      | ditaSourceMorphism terminalAssignment == identityAt sourceSite terminalSourceObject =
          Right (ditaIndexObject terminalAssignment, terminalValue)
      | otherwise =
          (ditaIndexObject terminalAssignment,)
            <$> first
              (DirectImageCandidateRestrictionFailed (ditaSourceMorphism terminalAssignment) terminalValue)
              (fpRestrict sourcePresheaf (ditaSourceMorphism terminalAssignment) terminalValue)

    coneMismatches objectValue leftCone rightCone =
      targetFailures <> domainFailures <> Map.foldMapWithKey assignmentFailures leftAssignments
      where
        leftAssignments = directImageConeAssignments leftCone
        rightAssignments = directImageConeAssignments rightCone
        leftKeys = Map.keys leftAssignments
        rightKeys = Map.keys rightAssignments

        targetFailures =
          [ DirectImageConeTargetMismatch (directImageConeTarget leftCone) (directImageConeTarget rightCone)
          | directImageConeTarget leftCone /= directImageConeTarget rightCone
          ]

        domainFailures =
          [DirectImageConeDomainMismatch objectValue leftKeys rightKeys | leftKeys /= rightKeys]

        assignmentFailures indexObject leftValue =
          case Map.lookup indexObject rightAssignments of
            Nothing -> []
            Just rightValue ->
              let mismatches = fpMismatches sourcePresheaf (directImageIndexSourceObject indexObject) leftValue rightValue
               in [DirectImageAssignmentMismatch indexObject leftValue rightValue mismatches | not (null mismatches)]

    normalizeCone objectValue =
      DirectImageCone objectValue
        . Map.mapWithKey
          (\indexObject -> fpNormalize sourcePresheaf (directImageIndexSourceObject indexObject))
        . directImageConeAssignments

same :: Eq value => (value -> value -> failure) -> value -> value -> Either failure ()
same failure expected actual =
  if expected == actual then Right () else Left (failure expected actual)
{-# INLINE same #-}

single :: [value] -> Maybe value
single [value] =
  Just value
single _ =
  Nothing
{-# INLINE single #-}
