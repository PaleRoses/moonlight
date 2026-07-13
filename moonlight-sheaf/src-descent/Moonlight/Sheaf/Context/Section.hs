{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Moonlight.Sheaf.Context.Section
  ( ContextClassSection (..),
    contextSectionsApproxEq,
    contextSectionMismatches,
    mergeContextSections,
    mergeByCanonical,
    restrictClassIdWith,
    restrictSectionToTarget,
    analysisSectionMismatches,
    combineMismatches,
  )
where

import Data.Kind (Type)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Moonlight.Core (DenseKey (..))
import Moonlight.Sheaf.Context.Core
  ( AnalysisRestrictionMismatch (..),
    ContextRestrictionMismatch (..),
    SectionMismatch (..),
    mismatchAtKey,
  )

type ContextClassSection :: Type -> Type
newtype ContextClassSection classId = ContextClassSection
  { ccsEntries :: IntMap classId
  }
  deriving stock (Eq, Show)

contextSectionsApproxEq :: Eq classId => ContextClassSection classId -> ContextClassSection classId -> Bool
contextSectionsApproxEq leftSection rightSection =
  ccsEntries leftSection == ccsEntries rightSection

contextSectionMismatches :: Eq classId => ContextClassSection classId -> ContextClassSection classId -> [ContextRestrictionMismatch classId]
contextSectionMismatches leftSection rightSection =
  let leftEntries = ccsEntries leftSection
      rightEntries = ccsEntries rightSection
      allKeys = IntSet.union (IntMap.keysSet leftEntries) (IntMap.keysSet rightEntries)
   in concatMap (mismatchAtKey leftEntries rightEntries) (IntSet.toAscList allKeys)

mergeContextSections :: DenseKey classId => ContextClassSection classId -> ContextClassSection classId -> ContextClassSection classId
mergeContextSections leftSection rightSection =
  ContextClassSection (mergeByCanonical (ccsEntries leftSection) (ccsEntries rightSection))

mergeByCanonical :: DenseKey classId => IntMap classId -> IntMap classId -> IntMap classId
mergeByCanonical = IntMap.unionWith chooseCanonical
  where
    chooseCanonical :: DenseKey classId => classId -> classId -> classId
    chooseCanonical leftRepresentative rightRepresentative
      | encodeDenseKey leftRepresentative <= encodeDenseKey rightRepresentative = leftRepresentative
      | otherwise = rightRepresentative

restrictClassIdWith :: DenseKey classId => IntMap classId -> classId -> classId
restrictClassIdWith targetClasses classId =
  IntMap.findWithDefault classId (encodeDenseKey classId) targetClasses

restrictSectionToTarget :: DenseKey classId => IntMap classId -> ctx -> ctx -> ContextClassSection classId -> ContextClassSection classId
restrictSectionToTarget targetClasses _sourceContext _targetContext sourceSection =
  ContextClassSection
    ( IntMap.map
        (restrictClassIdWith targetClasses)
        (ccsEntries sourceSection)
    )

analysisSectionMismatches :: Eq a => IntMap a -> IntMap a -> [AnalysisRestrictionMismatch a]
analysisSectionMismatches leftSection rightSection =
  let allKeys = IntSet.union (IntMap.keysSet leftSection) (IntMap.keysSet rightSection)
   in concatMap (analysisMismatchAtKey leftSection rightSection) (IntSet.toAscList allKeys)

analysisMismatchAtKey :: Eq a => IntMap a -> IntMap a -> Int -> [AnalysisRestrictionMismatch a]
analysisMismatchAtKey leftEntries rightEntries key =
  let expected = IntMap.lookup key leftEntries
      actual = IntMap.lookup key rightEntries
   in ([AnalysisRestrictionMismatch key expected actual | expected /= actual])

combineMismatches :: [ContextRestrictionMismatch classId] -> [AnalysisRestrictionMismatch a] -> [SectionMismatch classId a]
combineMismatches classMismatches analysisMismatches =
  let classMap = IntMap.fromList [(crmClassKey mismatch, mismatch) | mismatch <- classMismatches]
      analysisMap = IntMap.fromList [(armClassKey mismatch, mismatch) | mismatch <- analysisMismatches]
      allKeys = IntSet.union (IntMap.keysSet classMap) (IntMap.keysSet analysisMap)
   in IntSet.foldl'
        ( \acc key ->
            case (IntMap.lookup key classMap, IntMap.lookup key analysisMap) of
              (Just classMismatch, Just analysisMismatch) -> BothMismatch classMismatch analysisMismatch : acc
              (Just classMismatch, Nothing) -> OnlyClass classMismatch : acc
              (Nothing, Just analysisMismatch) -> OnlyAnalysis analysisMismatch : acc
              (Nothing, Nothing) -> acc
        )
        []
        allKeys
