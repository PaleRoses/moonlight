-- | Compile a restriction index into a prepared section descent.
module Moonlight.Sheaf.Section.Store.Descent.Prepare
  ( prepareSectionDescent,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Vector qualified as Vector
import Data.Vector.Unboxed qualified as UVector
import Moonlight.Sheaf.Section.Model
  ( SheafModel,
    sheafModelObjects,
    sheafModelRestrictions,
  )
import Moonlight.Sheaf.Index.Dense
  ( denseIndexCount
  )
import Moonlight.Sheaf.Section.Morphism
  ( Restriction (..),
    RestrictionId (..),
    rId,
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectKey,
    unObjectKey,
  )
import Moonlight.Sheaf.Section.Restriction
  ( RestrictionIndex,
    restrictionEndpointKeyMap,
    restrictionCount,
    restrictionEntries,
    restrictionIncomingByObject,
    restrictionOutgoingByObject,
  )
import Moonlight.Sheaf.Section.Store.Descent.FastPath.Internal
  ( fastEditProgramsByObject,
  )
import Moonlight.Sheaf.Section.Store.Internal
  ( PreparedSectionDescent (PreparedSectionDescentInternal),
    PreparedSectionDescentViews (PreparedSectionDescentViewsInternal),
    SectionDescentRestrictionRow (SectionDescentRestrictionRowInternal),
    preparedSectionDescentFastEditProgramsInternal,
    preparedSectionDescentFrontierClosureBudgetInternal,
    preparedSectionDescentIncidentRestrictionIdsInternal,
    preparedSectionDescentIncomingRestrictionIdsInternal,
    preparedSectionDescentObjectCountInternal,
    preparedSectionDescentOutgoingRestrictionIdsInternal,
    preparedSectionDescentRowsByRestrictionIdInternal,
    preparedSectionDescentViewsInternal,
    preparedSectionDescentAllRestrictionIdsInternal,
    sectionDescentRowRestrictionInternal,
    sectionDescentRowRestrictionKeyInternal,
    sectionDescentRowSourceKeyInternal,
    sectionDescentRowSourceOrdinalInternal,
    sectionDescentRowTargetKeyInternal,
    sectionDescentRowTargetOrdinalInternal,
  )
import Moonlight.Sheaf.Section.Store.Types

prepareSectionDescent ::
  SheafModel owner cell witness ->
  Either SectionDescentPreparationError (PreparedSectionDescent owner cell witness)
prepareSectionDescent model = do
  let restrictions = sheafModelRestrictions model
      objectCountValue = denseIndexCount (sheafModelObjects model)
      restrictionCountValue = restrictionCount restrictions
  rowsByRestrictionId <- restrictionRowsById restrictions
  frontierBudget <- frontierClosureTerminationBudget objectCountValue restrictionCountValue
  let descentViews =
        preparedSectionDescentViews
          objectCountValue
          restrictionCountValue
          restrictions
          (Vector.replicate objectCountValue Nothing)
      preparedDescent =
        PreparedSectionDescentInternal
          { preparedSectionDescentObjectCountInternal = objectCountValue,
            preparedSectionDescentFrontierClosureBudgetInternal = frontierBudget,
            preparedSectionDescentRowsByRestrictionIdInternal = rowsByRestrictionId,
            preparedSectionDescentViewsInternal = descentViews
          }
  pure
    preparedDescent
      { preparedSectionDescentViewsInternal =
          descentViews
            { preparedSectionDescentFastEditProgramsInternal =
                fastEditProgramsByObject preparedDescent
            }
      }

frontierClosureTerminationBudget :: Int -> Int -> Either SectionDescentPreparationError FrontierClosureBudget
frontierClosureTerminationBudget objectCountValue restrictionCountValue =
  if budget > toInteger (maxBound :: Int)
    then Left (SectionDescentPreparationBudgetOverflow budget)
    else Right (FrontierClosureBudget (fromInteger (max 1 budget)))
  where
    budget =
      toInteger objectCountValue * (toInteger restrictionCountValue + 1)

preparedSectionDescentViews ::
  Int ->
  Int ->
  RestrictionIndex cell witness ->
  Vector.Vector (Maybe (SectionFastEditProgram owner cell witness)) ->
  PreparedSectionDescentViews owner cell witness
preparedSectionDescentViews objectCountValue restrictionCountValue restrictions fastEditPrograms =
  PreparedSectionDescentViewsInternal
    { preparedSectionDescentIncidentRestrictionIdsInternal =
        objectOrdinalVectorsFromIntSetMap objectCountValue (restrictionIncidentByObject restrictions),
      preparedSectionDescentIncomingRestrictionIdsInternal =
        objectOrdinalVectorsFromIntSetMap objectCountValue (restrictionIncomingByObject restrictions),
      preparedSectionDescentOutgoingRestrictionIdsInternal =
        objectOrdinalVectorsFromIntSetMap objectCountValue (restrictionOutgoingByObject restrictions),
      preparedSectionDescentFastEditProgramsInternal = fastEditPrograms,
      preparedSectionDescentAllRestrictionIdsInternal = UVector.generate restrictionCountValue id
    }

objectVectorFromMap :: Int -> value -> IntMap value -> Vector.Vector value
objectVectorFromMap objectCountValue fallbackValue entries =
  Vector.generate objectCountValue (\objectOrdinal -> IntMap.findWithDefault fallbackValue objectOrdinal entries)

objectOrdinalVectorsFromIntSetMap :: Int -> IntMap IntSet -> Vector.Vector (UVector.Vector Int)
objectOrdinalVectorsFromIntSetMap objectCountValue =
  objectVectorFromMap objectCountValue UVector.empty . IntMap.map (UVector.fromList . IntSet.toAscList)

restrictionIncidentByObject :: RestrictionIndex cell witness -> IntMap IntSet
restrictionIncidentByObject restrictions =
  IntMap.unionWith IntSet.union (restrictionOutgoingByObject restrictions) (restrictionIncomingByObject restrictions)

restrictionRowsById ::
  RestrictionIndex cell witness ->
  Either SectionDescentPreparationError (Vector.Vector (SectionDescentRestrictionRow cell witness))
restrictionRowsById restrictions =
  Vector.fromList <$> traverse (restrictionRowFor (restrictionEndpointKeyMap restrictions)) (restrictionEntries restrictions)

restrictionRowFor ::
  IntMap (ObjectKey, ObjectKey) ->
  Restriction cell witness ->
  Either SectionDescentPreparationError (SectionDescentRestrictionRow cell witness)
restrictionRowFor endpointKeys restriction =
  case IntMap.lookup restrictionKey endpointKeys of
    Just (sourceKey, targetKey) ->
      Right
        SectionDescentRestrictionRowInternal
          { sectionDescentRowRestrictionKeyInternal = restrictionKey,
            sectionDescentRowRestrictionInternal = restriction,
            sectionDescentRowSourceKeyInternal = sourceKey,
            sectionDescentRowTargetKeyInternal = targetKey,
            sectionDescentRowSourceOrdinalInternal = unObjectKey sourceKey,
            sectionDescentRowTargetOrdinalInternal = unObjectKey targetKey
          }
    Nothing ->
      Left (SectionDescentPreparationRestrictionMissing restrictionId)
  where
    restrictionId =
      rId restriction
    restrictionKey =
      unRestrictionId restrictionId
