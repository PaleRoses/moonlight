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
    sheafModelFingerprint,
    sheafModelObjects,
    sheafModelRestrictions,
    sheafModelVersion,
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
import Moonlight.Sheaf.Section.Store.Descent.FastPath
  ( fastEditProgramsByObject,
  )
import Moonlight.Sheaf.Section.Store.Types

prepareSectionDescent ::
  SheafModel cell witness ->
  Either SectionDescentPreparationError (PreparedSectionDescent cell witness)
prepareSectionDescent model = do
  let restrictions = sheafModelRestrictions model
      objectCountValue = denseIndexCount (sheafModelObjects model)
      restrictionCountValue = restrictionCount restrictions
  rowsByRestrictionId <- restrictionRowsById restrictions
  let descentViews =
        preparedSectionDescentViews
          objectCountValue
          restrictionCountValue
          restrictions
          (Vector.replicate objectCountValue Nothing)
      preparedDescent =
        PreparedSectionDescent
          { psdModelFingerprint = sheafModelFingerprint model,
            psdModelVersion = sheafModelVersion model,
            psdObjectCount = objectCountValue,
            psdFrontierClosureBudget = frontierClosureTerminationBudget objectCountValue restrictionCountValue,
            psdRowsByRestrictionId = rowsByRestrictionId,
            psdViews = descentViews
          }
  pure
    preparedDescent
      { psdViews =
          descentViews
            { psdvFastEditProgramsByObject =
                fastEditProgramsByObject preparedDescent
            }
      }

frontierClosureTerminationBudget :: Int -> Int -> FrontierClosureBudget
frontierClosureTerminationBudget objectCountValue restrictionCountValue =
  FrontierClosureBudget (max 1 (objectCountValue * (restrictionCountValue + 1)))

preparedSectionDescentViews ::
  Int ->
  Int ->
  RestrictionIndex cell witness ->
  Vector.Vector (Maybe (SectionFastEditProgram cell witness)) ->
  PreparedSectionDescentViews cell witness
preparedSectionDescentViews objectCountValue restrictionCountValue restrictions fastEditPrograms =
  PreparedSectionDescentViews
    { psdvIncidentRestrictionIdsByObject =
        objectOrdinalVectorsFromIntSetMap objectCountValue (restrictionIncidentByObject restrictions),
      psdvIncomingRestrictionIdsByObject =
        objectOrdinalVectorsFromIntSetMap objectCountValue (restrictionIncomingByObject restrictions),
      psdvOutgoingRestrictionIdsByObject =
        objectOrdinalVectorsFromIntSetMap objectCountValue (restrictionOutgoingByObject restrictions),
      psdvFastEditProgramsByObject = fastEditPrograms,
      psdvAllRestrictionIds = UVector.generate restrictionCountValue id
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
        SectionDescentRestrictionRow
          { sdrRestrictionKey = restrictionKey,
            sdrRestriction = restriction,
            sdrSourceKey = sourceKey,
            sdrTargetKey = targetKey,
            sdrSourceOrdinal = unObjectKey sourceKey,
            sdrTargetOrdinal = unObjectKey targetKey
          }
    Nothing ->
      Left (SectionDescentPreparationRestrictionMissing restrictionId)
  where
    restrictionId =
      rId restriction
    restrictionKey =
      unRestrictionId restrictionId
