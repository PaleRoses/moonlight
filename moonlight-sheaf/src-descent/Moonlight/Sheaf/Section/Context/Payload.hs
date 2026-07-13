{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Section.Context.Payload
  ( ContextClassPayload (..),
    PayloadMapInvariantViolation (..),
    payloadMapFromSections,
    payloadMapToRepresentativeMap,
    payloadMapToAnalysisMap,
    canonicalizePayloadMapWith,
    validateCanonicalPayloadMap,
    coalescePayloadMapByRepresentative,
    payloadMapMismatches,
    payloadRestrictionMismatchesToTarget,
    payloadRestrictionMismatchKeys,
    restrictPayloadMapToTarget,
    contextPayloadMapStalkAlgebra,
  )
where

import Control.Applicative ((<|>))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Monoid (Endo (..), appEndo)
import Moonlight.Algebra (JoinSemilattice (join))
import Moonlight.Core (DenseKey (..))
import Moonlight.Sheaf.Context.Algebra
  ( restrictAnalysisToTarget,
  )
import Moonlight.Sheaf.Context.Core
  ( AnalysisRestrictionMismatch (..),
    ContextRestrictionMismatch (..),
    SectionMismatch (..),
  )
import Moonlight.Sheaf.Context.Section
  ( ContextClassSection (..),
    analysisSectionMismatches,
    combineMismatches,
    contextSectionMismatches,
    restrictSectionToTarget,
  )
import Moonlight.Sheaf.Section.Stalk
  ( StalkAlgebra (..),
    StalkRestrictionKernel (..),
  )

type ContextClassPayload :: Type -> Type -> Type
data ContextClassPayload classId a = ContextClassPayload
  { ccpRepresentative :: !classId,
    ccpAnalysis :: !a
  }
  deriving stock (Eq, Show)

type PayloadMapInvariantViolation :: Type -> Type
data PayloadMapInvariantViolation classId
  = PayloadKeyRepresentativeMismatch !Int !classId
  | PayloadDuplicateRepresentative !classId
  deriving stock (Eq, Show)

payloadMapFromSections :: DenseKey classId => IntMap classId -> IntMap a -> IntMap (ContextClassPayload classId a)
payloadMapFromSections representatives analysis =
  IntSet.foldl'
    ( \acc key ->
        let representative = IntMap.findWithDefault (decodeDenseKey key) key representatives
            representativeKey = encodeDenseKey representative
         in case IntMap.lookup (encodeDenseKey representative) analysis <|> IntMap.lookup key analysis of
              Nothing -> acc
              Just analysisValue ->
                IntMap.insertWith
                  (\_ existingPayload -> existingPayload)
                  representativeKey
                  ContextClassPayload
                    { ccpRepresentative = representative,
                      ccpAnalysis = analysisValue
                    }
                  acc
    )
    IntMap.empty
    (IntSet.union (IntMap.keysSet representatives) (IntMap.keysSet analysis))

payloadMapToRepresentativeMap :: IntMap (ContextClassPayload classId a) -> IntMap classId
payloadMapToRepresentativeMap =
  fmap ccpRepresentative

payloadMapToAnalysisMap :: DenseKey classId => IntMap (ContextClassPayload classId a) -> IntMap a
payloadMapToAnalysisMap =
  IntMap.foldl'
    ( \acc payload ->
        IntMap.insertWith
          (\_ existing -> existing)
          (encodeDenseKey (ccpRepresentative payload))
          (ccpAnalysis payload)
          acc
    )
    IntMap.empty

canonicalizePayloadMapWith ::
  (DenseKey classId, JoinSemilattice a) =>
  (classId -> classId) ->
  IntMap (ContextClassPayload classId a) ->
  IntMap (ContextClassPayload classId a)
canonicalizePayloadMapWith canonicalizeRepresentative =
  IntMap.foldl'
    insertCanonicalPayload
    IntMap.empty
  where
    insertCanonicalPayload acc payload =
      let representative = canonicalizeRepresentative (ccpRepresentative payload)
          representativeKey = encodeDenseKey representative
       in IntMap.insertWith
            mergeCanonicalPayload
            representativeKey
            ContextClassPayload
              { ccpRepresentative = representative,
                ccpAnalysis = ccpAnalysis payload
              }
            acc

    mergeCanonicalPayload ::
      JoinSemilattice a =>
      ContextClassPayload classId a ->
      ContextClassPayload classId a ->
      ContextClassPayload classId a
    mergeCanonicalPayload leftPayload rightPayload =
      ContextClassPayload
        { ccpRepresentative = ccpRepresentative rightPayload,
          ccpAnalysis = join (ccpAnalysis leftPayload) (ccpAnalysis rightPayload)
        }

coalescePayloadMapByRepresentative ::
  (DenseKey classId, JoinSemilattice a) =>
  IntMap (ContextClassPayload classId a) ->
  IntMap (ContextClassPayload classId a)
coalescePayloadMapByRepresentative =
  canonicalizePayloadMapWith id

validateCanonicalPayloadMap ::
  forall classId a.
  DenseKey classId =>
  IntMap (ContextClassPayload classId a) ->
  Either (NonEmpty (PayloadMapInvariantViolation classId)) (IntMap (ContextClassPayload classId a))
validateCanonicalPayloadMap payloads =
  case nonEmpty (appEndo violations []) of
    Nothing -> Right payloads
    Just violationList -> Left violationList
  where
    (_, violations) =
      IntMap.foldlWithKey'
        collectViolation
        (IntSet.empty, mempty)
        payloads

    collectViolation ::
      (IntSet.IntSet, Endo [PayloadMapInvariantViolation classId]) ->
      Int ->
      ContextClassPayload classId a ->
      (IntSet.IntSet, Endo [PayloadMapInvariantViolation classId])
    collectViolation (seenKeys, accumulatedViolations) key payload =
      let representative = ccpRepresentative payload
          representativeKey = encodeDenseKey representative
          keyViolation =
            if key == representativeKey
              then mempty
              else Endo (PayloadKeyRepresentativeMismatch key representative :)
          duplicateViolation =
            if IntSet.member representativeKey seenKeys
              then Endo (PayloadDuplicateRepresentative representative :)
              else mempty
       in ( IntSet.insert representativeKey seenKeys,
            accumulatedViolations <> keyViolation <> duplicateViolation
          )

restrictPayloadMapToTarget ::
  (DenseKey classId, JoinSemilattice a) =>
  IntMap classId ->
  ctx ->
  ctx ->
  IntMap (ContextClassPayload classId a) ->
  IntMap (ContextClassPayload classId a)
restrictPayloadMapToTarget targetClasses sourceContext targetContext payloads =
  let restrictedClasses =
        restrictSectionToTarget
          targetClasses
          sourceContext
          targetContext
          (ContextClassSection (payloadMapToRepresentativeMap payloads))
      restrictedAnalysis =
        restrictAnalysisToTarget join targetClasses (payloadMapToAnalysisMap payloads)
   in payloadMapFromSections (ccsEntries restrictedClasses) restrictedAnalysis

payloadMapMismatches ::
  (DenseKey classId, Eq a) =>
  IntMap (ContextClassPayload classId a) ->
  IntMap (ContextClassPayload classId a) ->
  [SectionMismatch classId a]
payloadMapMismatches left right =
  combineMismatches
    ( contextSectionMismatches
        (ContextClassSection (payloadMapToRepresentativeMap left))
        (ContextClassSection (payloadMapToRepresentativeMap right))
    )
    (analysisSectionMismatches (payloadMapToAnalysisMap left) (payloadMapToAnalysisMap right))

payloadRestrictionMismatchesToTarget ::
  (DenseKey classId, Eq a) =>
  (a -> a -> a) ->
  IntMap classId ->
  ctx ->
  ctx ->
  IntMap (ContextClassPayload classId a) ->
  IntMap (ContextClassPayload classId a) ->
  [SectionMismatch classId a]
payloadRestrictionMismatchesToTarget joinFn targetClasses sourceContext targetContext sourcePayloads targetPayloads =
  let sourceClasses =
        payloadMapToRepresentativeMap sourcePayloads
      targetAnalysis =
        payloadMapToAnalysisMap targetPayloads
      restrictedClasses =
        restrictSectionToTarget
          targetClasses
          sourceContext
          targetContext
          (ContextClassSection sourceClasses)
      restrictedAnalysis =
        restrictAnalysisToTarget joinFn targetClasses (payloadMapToAnalysisMap sourcePayloads)
   in combineMismatches
        (contextSectionMismatches restrictedClasses (ContextClassSection targetClasses))
        (analysisSectionMismatches targetAnalysis restrictedAnalysis)

payloadRestrictionMismatchKeys ::
  (DenseKey classId, Eq a) =>
  (a -> a -> a) ->
  IntMap classId ->
  ctx ->
  ctx ->
  IntMap (ContextClassPayload classId a) ->
  IntMap (ContextClassPayload classId a) ->
  IntSet
payloadRestrictionMismatchKeys joinFn targetClasses sourceContext targetContext sourcePayloads targetPayloads =
  IntSet.fromList
    ( fmap
        sectionMismatchClassKey
        (payloadRestrictionMismatchesToTarget joinFn targetClasses sourceContext targetContext sourcePayloads targetPayloads)
    )

sectionMismatchClassKey :: SectionMismatch classId a -> Int
sectionMismatchClassKey mismatch =
  case mismatch of
    OnlyClass classMismatch ->
      crmClassKey classMismatch
    OnlyAnalysis analysisMismatch ->
      armClassKey analysisMismatch
    BothMismatch classMismatch _analysisMismatch ->
      crmClassKey classMismatch

contextPayloadMapStalkAlgebra ::
  (DenseKey classId, Eq a, JoinSemilattice a) =>
  StalkAlgebra witness (IntMap (ContextClassPayload classId a)) (SectionMismatch classId a) ()
contextPayloadMapStalkAlgebra =
  StalkAlgebra
    { saRestrictionKernel = const StalkRestrictionIdentity,
      saMismatches = payloadMapMismatches,
      saMerge =
        \left right ->
          Right
            ( coalescePayloadMapByRepresentative
                (IntMap.unionWith mergePayload left right)
            ),
      saRepair = const (Left ()),
      saNormalize = coalescePayloadMapByRepresentative
    }

mergePayload ::
  JoinSemilattice a =>
  ContextClassPayload classId a ->
  ContextClassPayload classId a ->
  ContextClassPayload classId a
mergePayload leftPayload rightPayload =
  ContextClassPayload
    { ccpRepresentative = ccpRepresentative leftPayload,
      ccpAnalysis = join (ccpAnalysis leftPayload) (ccpAnalysis rightPayload)
    }
