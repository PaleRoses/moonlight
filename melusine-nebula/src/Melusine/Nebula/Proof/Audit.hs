{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.Nebula.Proof.Audit
  ( TypeEvidenceCensus (..),
    typeEvidenceCensus,
    StepTypeConflict (..),
    TypeVerdict (..),
    typeVerdictKey,
    ReplayStep (..),
    classEvidenceByKey,
    replayStepVerdicts,
    verdictWord,
    verdictConflictWords,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Foldable (fold)
import Data.List (mapAccumL)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Melusine.Nebula.Core
  ( NebulaAnalysis (..),
    TypeEvidence,
    typeEvidenceObservations,
  )
import Moonlight.Core (RewriteRuleId (..))
import Moonlight.EGraph.Introspection.Core.HsExpr (HsExprF, ScopeCtx)
import Moonlight.EGraph.Pure.Context (ContextEGraph)
import Moonlight.EGraph.Pure.Context.Core
  ( ContextFiber (..),
    cegBase,
    cegContextFibers,
    contextAnalysisValueAt,
    contextRepresentativeAt,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    EClass (..),
    EGraph,
    classIdKey,
    eGraphClasses,
  )
import Moonlight.Flow.Model.Schema.Digest (StableDigest128)
import Moonlight.Sheaf.Section.Congruence.Equivalence.Relation (equivalencePairs)
import Moonlight.Sheaf.Context.Site (PreparedContextSupportError)
import Data.Fix (Fix)

type TypeEvidenceCensus :: Type
data TypeEvidenceCensus = TypeEvidenceCensus
  { tecObservedClassCount :: !Int,
    tecPolymorphicClassCount :: !Int,
    tecUnobservedClassCount :: !Int
  }
  deriving stock (Eq, Ord, Show)

typeEvidenceCensus :: ContextEGraph HsExprF NebulaAnalysis ScopeCtx -> Either (PreparedContextSupportError ScopeCtx) TypeEvidenceCensus
typeEvidenceCensus contextGraph =
  fmap
    (baseTypeEvidenceCensus (cegBase contextGraph) <>)
    (localMergeTypeEvidenceCensus contextGraph)

baseTypeEvidenceCensus :: EGraph HsExprF NebulaAnalysis -> TypeEvidenceCensus
baseTypeEvidenceCensus graph =
  foldMap
    (typeEvidenceCensusRow . naType . eClassData . snd)
    (IntMap.toAscList (eGraphClasses graph))

localMergeTypeEvidenceCensus :: ContextEGraph HsExprF NebulaAnalysis ScopeCtx -> Either (PreparedContextSupportError ScopeCtx) TypeEvidenceCensus
localMergeTypeEvidenceCensus contextGraph =
  fmap fold $
    traverse
      (localContextMergeCensus contextGraph)
      (fmap (fmap (equivalencePairs . cfRelation)) (Map.toAscList (cegContextFibers contextGraph)))

localContextMergeCensus ::
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  (ScopeCtx, [(ClassId, ClassId)]) ->
  Either (PreparedContextSupportError ScopeCtx) TypeEvidenceCensus
localContextMergeCensus contextGraph (contextValue, localUnions) = do
  localRepresentatives <-
    fmap Set.fromList $
      traverse
        (\classIdValue -> contextRepresentativeAt contextValue classIdValue contextGraph)
        [ classIdValue
          | (leftClass, rightClass) <- localUnions,
            classIdValue <- [leftClass, rightClass]
        ]
  localAnalyses <-
    traverse
      (\classIdValue -> contextAnalysisValueAt contextValue classIdValue contextGraph)
      (Set.toAscList localRepresentatives)
  pure (foldMap (foldMap (typeEvidenceCensusRow . naType)) localAnalyses)

typeEvidenceCensusRow :: TypeEvidence -> TypeEvidenceCensus
typeEvidenceCensusRow evidence =
  let observations = typeEvidenceObservations evidence
   in if Set.null observations
        then mempty {tecUnobservedClassCount = 1}
        else
          mempty
            { tecObservedClassCount = 1,
              tecPolymorphicClassCount = if Set.size observations > 1 then 1 else 0
            }

instance Semigroup TypeEvidenceCensus where
  leftCensus <> rightCensus =
    TypeEvidenceCensus
      { tecObservedClassCount = tecObservedClassCount leftCensus + tecObservedClassCount rightCensus,
        tecPolymorphicClassCount = tecPolymorphicClassCount leftCensus + tecPolymorphicClassCount rightCensus,
        tecUnobservedClassCount = tecUnobservedClassCount leftCensus + tecUnobservedClassCount rightCensus
      }

instance Monoid TypeEvidenceCensus where
  mempty =
    TypeEvidenceCensus
      { tecObservedClassCount = 0,
        tecPolymorphicClassCount = 0,
        tecUnobservedClassCount = 0
      }

type StepTypeConflict :: Type
data StepTypeConflict = StepTypeConflict
  { stcRule :: !RewriteRuleId,
    stcLhsClass :: !ClassId,
    stcRhsClass :: !ClassId
  }
  deriving stock (Eq, Ord, Show)

type TypeVerdict :: Type
data TypeVerdict
  = TypeCompatible
  | TypePolymorphic
  | TypeUnknown
  | TypeIncompatible ![StepTypeConflict]
  deriving stock (Eq, Ord, Show)

typeVerdictKey :: TypeVerdict -> String
typeVerdictKey = \case
  TypeCompatible ->
    "compatible"
  TypePolymorphic ->
    "polymorphic"
  TypeUnknown ->
    "unknown"
  TypeIncompatible {} ->
    "incompatible"

type ReplayStep :: Type
data ReplayStep = ReplayStep
  { repRule :: !RewriteRuleId,
    repLhs :: !ClassId,
    repRhs :: !ClassId,
    repLhsWitness :: !(Maybe (Fix HsExprF)),
    repRhsWitness :: !(Maybe (Fix HsExprF))
  }

classEvidenceByKey ::
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  IntMap (Set StableDigest128)
classEvidenceByKey contextGraph =
  IntMap.fromList
    [ (classKey, observations)
    | (classKey, classValue) <- IntMap.toAscList (eGraphClasses (cegBase contextGraph)),
      let observations = typeEvidenceObservations (naType (eClassData classValue)),
      not (Set.null observations)
    ]

replayStepVerdicts ::
  EGraph HsExprF NebulaAnalysis ->
  IntMap (Set StableDigest128) ->
  [ReplayStep] ->
  [TypeVerdict]
replayStepVerdicts anchorGraph initialEvidence =
  snd
    . mapAccumL
      replayStep
      ReplayState
        { rsAnchorGraph = anchorGraph,
          rsNextSynthetic = -1,
          rsParent = IntMap.empty,
          rsEvidence = initialEvidence
        }

type ReplayState :: Type
data ReplayState = ReplayState
  { rsAnchorGraph :: !(EGraph HsExprF NebulaAnalysis),
    rsNextSynthetic :: !Int,
    rsParent :: !(IntMap Int),
    rsEvidence :: !(IntMap (Set StableDigest128))
  }

replayStep :: ReplayState -> ReplayStep -> (ReplayState, TypeVerdict)
replayStep state step =
  let (anchoredLhs, lhsState) = anchorKey (repLhsWitness step) state
      (anchoredRhs, anchoredState) = anchorKey (repRhsWitness step) lhsState
      lhsRep = representative (rsParent anchoredState) anchoredLhs
      rhsRep = representative (rsParent anchoredState) anchoredRhs
      evidenceAt key = IntMap.findWithDefault Set.empty key (rsEvidence anchoredState)
   in if lhsRep == rhsRep
        then (anchoredState, settledVerdict (evidenceAt lhsRep))
        else
          let lhsObservations = evidenceAt lhsRep
              rhsObservations = evidenceAt rhsRep
              mergedState =
                anchoredState
                  { rsParent = IntMap.insert rhsRep lhsRep (rsParent anchoredState),
                    rsEvidence =
                      IntMap.insert
                        lhsRep
                        (Set.union lhsObservations rhsObservations)
                        (IntMap.delete rhsRep (rsEvidence anchoredState))
                  }
           in (mergedState, mergeVerdict step lhsObservations rhsObservations)

anchorKey :: Maybe (Fix HsExprF) -> ReplayState -> (Int, ReplayState)
anchorKey maybeWitness state =
  case maybeWitness of
    Just witness ->
      let (anchorClass, anchoredGraph) = addTerm witness (rsAnchorGraph state)
       in (classIdKey anchorClass, state {rsAnchorGraph = anchoredGraph})
    Nothing ->
      ( rsNextSynthetic state,
        state {rsNextSynthetic = rsNextSynthetic state - 1}
      )

representative :: IntMap Int -> Int -> Int
representative parents key =
  maybe key (representative parents) (IntMap.lookup key parents)

settledVerdict :: Set StableDigest128 -> TypeVerdict
settledVerdict observations
  | Set.null observations = TypeUnknown
  | Set.size observations == 1 = TypeCompatible
  | otherwise = TypePolymorphic

mergeVerdict :: ReplayStep -> Set StableDigest128 -> Set StableDigest128 -> TypeVerdict
mergeVerdict step lhsObservations rhsObservations
  | Set.null lhsObservations || Set.null rhsObservations = TypeUnknown
  | Set.null (Set.intersection lhsObservations rhsObservations) =
      TypeIncompatible
        [ StepTypeConflict
            { stcRule = repRule step,
              stcLhsClass = repLhs step,
              stcRhsClass = repRhs step
            }
        ]
  | Set.size lhsObservations == 1 && Set.size rhsObservations == 1 = TypeCompatible
  | otherwise = TypePolymorphic

instance Semigroup TypeVerdict where
  TypeIncompatible leftConflicts <> TypeIncompatible rightConflicts =
    TypeIncompatible (leftConflicts <> rightConflicts)
  TypeIncompatible conflicts <> _ =
    TypeIncompatible conflicts
  _ <> TypeIncompatible conflicts =
    TypeIncompatible conflicts
  TypeUnknown <> _ =
    TypeUnknown
  _ <> TypeUnknown =
    TypeUnknown
  TypePolymorphic <> _ =
    TypePolymorphic
  _ <> TypePolymorphic =
    TypePolymorphic
  TypeCompatible <> TypeCompatible =
    TypeCompatible

instance Monoid TypeVerdict where
  mempty =
    TypeCompatible

verdictWord :: TypeVerdict -> Word64
verdictWord = \case
  TypeCompatible ->
    1
  TypePolymorphic ->
    2
  TypeUnknown ->
    3
  TypeIncompatible {} ->
    4

verdictConflictWords :: TypeVerdict -> [Word64]
verdictConflictWords = \case
  TypeIncompatible conflicts ->
    fromIntegral (length conflicts) : foldMap conflictWords conflicts
  _ ->
    [0]

conflictWords :: StepTypeConflict -> [Word64]
conflictWords conflict =
  [ rewriteRuleWord (stcRule conflict),
    fromIntegral (classIdKey (stcLhsClass conflict)),
    fromIntegral (classIdKey (stcRhsClass conflict))
  ]

rewriteRuleWord :: RewriteRuleId -> Word64
rewriteRuleWord (RewriteRuleId ruleKey) =
  fromIntegral ruleKey
