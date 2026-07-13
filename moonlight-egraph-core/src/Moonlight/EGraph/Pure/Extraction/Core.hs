module Moonlight.EGraph.Pure.Extraction.Core
  ( CostAlgebra (..),
    AnalysisCostAlgebra (..),
    costOnly,
    depthCost,
    liftCostAlgebra,
    ExtractionClass,
    extractionClass,
    extractionClassAnalysis,
    extractionClassNodes,
    ExtractionTable,
    extractionTable,
    uncheckedExtractionTable,
    extractionClasses,
    extractionCanonicalClass,
    lookupExtractionClass,
    StableExtractionSnapshot,
    stableExtractionSnapshotFromEGraph,
    stableExtractionSnapshotTable,
    ExtractionFixpointBudget (..),
    ExtractionConvergenceReport (..),
    ExtractionResult (..),
    BestChoice (..),
    termSize,
    termCost,
    minimumMaybe,
  )
where

import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Set qualified as Set
import Moonlight.Core (Language)
import Moonlight.EGraph.Pure.Change
  ( GraphPhase (..),
    eGraphPhase,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    EClass (..),
    ENode (..),
    EGraph,
    canonicalizeClassId,
    classIdKey,
    eGraphClasses,
  )
import Data.Fix (Fix (..))
import Moonlight.Core (OrderedFix (..))
import Data.Functor.Foldable (cata)
import Numeric.Natural (Natural)

type CostAlgebra :: (Type -> Type) -> Type -> Type
newtype CostAlgebra f cost = CostAlgebra
  { costAlgebra :: f cost -> cost
  }

type AnalysisCostAlgebra :: (Type -> Type) -> Type -> Type -> Type
newtype AnalysisCostAlgebra f a cost = AnalysisCostAlgebra
  { analysisCostAlgebra :: a -> f (a, cost) -> cost
  }

costOnly :: Functor f => (f cost -> cost) -> AnalysisCostAlgebra f a cost
costOnly computeCost =
  AnalysisCostAlgebra (\_ childPairs -> computeCost (fmap snd childPairs))

-- | Tree-depth cost: a leaf costs @1@ and an internal node costs one more than
-- its deepest child.  This is deliberately distinct from 'termSize'.
depthCost :: Foldable f => CostAlgebra f Int
depthCost =
  CostAlgebra ((+ 1) . foldr max 0)

liftCostAlgebra :: Functor f => CostAlgebra f cost -> AnalysisCostAlgebra f a cost
liftCostAlgebra (CostAlgebra computeCost) =
  costOnly computeCost

type ExtractionClass :: (Type -> Type) -> Type -> Type
data ExtractionClass f a = ExtractionClass
  { extractionClassAnalysis :: a,
    extractionClassNodes :: [ENode f]
  }

extractionClass :: a -> [ENode f] -> ExtractionClass f a
extractionClass analysis nodes =
  ExtractionClass
    { extractionClassAnalysis = analysis,
      extractionClassNodes = nodes
    }

type ExtractionTable :: (Type -> Type) -> Type -> Type
data ExtractionTable f a = ExtractionTable
  { extractionClasses :: IntMap (ExtractionClass f a),
    extractionCanonicalClass :: ClassId -> Maybe ClassId
  }

extractionTable ::
  Foldable f =>
  IntMap (ExtractionClass f a) ->
  (ClassId -> Maybe ClassId) ->
  Maybe (ExtractionTable f a)
extractionTable classes canonicalClass =
  if canonicalKeysAreStable && childKeysAreAdmitted
    then
      Just
        ExtractionTable
          { extractionClasses = classes,
            extractionCanonicalClass = canonicalClass
          }
    else Nothing
  where
    canonicalKeysAreStable =
      all
        (\classKey -> canonicalClass (ClassId classKey) == Just (ClassId classKey))
        (IntMap.keys classes)

    childKeysAreAdmitted =
      all
        ( maybe
            False
            (\canonicalChild -> IntMap.member (classIdKey canonicalChild) classes)
            . canonicalClass
        )
        [ childClassId
        | eClassValue <- IntMap.elems classes,
          ENode nodeValue <- extractionClassNodes eClassValue,
          childClassId <- toList nodeValue
        ]

-- | Unchecked table assembly.  The caller carries the proof obligations that
-- 'extractionTable' would otherwise verify: every key of the class map is its
-- own canonical under the supplied canonicalizer, and every child class of
-- every node canonicalizes to an admitted key.  Reserved for table extension
-- over freshly inserted classes, where both obligations hold by construction.
uncheckedExtractionTable ::
  IntMap (ExtractionClass f a) ->
  (ClassId -> Maybe ClassId) ->
  ExtractionTable f a
uncheckedExtractionTable classes canonicalClass =
  ExtractionTable
    { extractionClasses = classes,
      extractionCanonicalClass = canonicalClass
    }

-- | Internal unchecked table materialization for an already-stable graph.
--
-- Public callers must pass through 'stableExtractionSnapshotFromEGraph' so a
-- dirty graph cannot masquerade as an extraction source.
uncheckedExtractionTableFromEGraph :: Language f => EGraph f a -> ExtractionTable f a
uncheckedExtractionTableFromEGraph graph =
  ExtractionTable
    { extractionClasses =
        fmap
          ( \eClassValue ->
              ExtractionClass
                { extractionClassAnalysis = eClassData eClassValue,
                  extractionClassNodes = Set.toAscList (eClassNodes eClassValue)
                }
          )
          classMap,
      extractionCanonicalClass =
        \classId ->
          let canonicalClass = canonicalizeClassId graph classId
           in if IntMap.member (classIdKey canonicalClass) classMap
                then Just canonicalClass
                else Nothing
    }
  where
    classMap =
      eGraphClasses graph

lookupExtractionClass :: ExtractionTable f a -> ClassId -> Maybe (ExtractionClass f a)
lookupExtractionClass table classId = do
  canonicalClassId <- extractionCanonicalClass table classId
  IntMap.lookup (classIdKey canonicalClassId) (extractionClasses table)

type StableExtractionSnapshot :: (Type -> Type) -> Type -> Type
newtype StableExtractionSnapshot f a = StableExtractionSnapshot
  { stableExtractionSnapshotTable :: ExtractionTable f a
  }

stableExtractionSnapshotFromEGraph :: Language f => EGraph f a -> Maybe (StableExtractionSnapshot f a)
stableExtractionSnapshotFromEGraph graph =
  if eGraphPhase graph == Stable
    then Just (StableExtractionSnapshot (uncheckedExtractionTableFromEGraph graph))
    else Nothing
{-# INLINE stableExtractionSnapshotFromEGraph #-}

type ExtractionFixpointBudget :: Type
newtype ExtractionFixpointBudget = ExtractionFixpointBudget
  { extractionFixpointBudgetRounds :: Natural
  }
  deriving stock (Eq, Ord, Show)

type ExtractionConvergenceReport :: Type
data ExtractionConvergenceReport = ExtractionConvergenceReport
  { ecrBudget :: !ExtractionFixpointBudget,
    ecrTotalClassCount :: !Int,
    ecrResolvedClassCount :: !Int,
    ecrUnresolvedClassCount :: !Int
  }
  deriving stock (Eq, Ord, Show)

type ExtractionResult :: (Type -> Type) -> Type -> Type
data ExtractionResult f cost = ExtractionResult
  { erTerm :: Fix f,
    erCost :: cost,
    erClass :: ClassId
  }

instance (Language f, Eq cost) => Eq (ExtractionResult f cost) where
  leftResult == rightResult =
    erCost leftResult == erCost rightResult
      && erClass leftResult == erClass rightResult
      && OrderedFix (erTerm leftResult) == OrderedFix (erTerm rightResult)

instance (Language f, Ord cost) => Ord (ExtractionResult f cost) where
  compare leftResult rightResult =
    compare
      ( erCost leftResult,
        erClass leftResult,
        OrderedFix (erTerm leftResult)
      )
      ( erCost rightResult,
        erClass rightResult,
        OrderedFix (erTerm rightResult)
      )

type BestChoice :: (Type -> Type) -> Type -> Type
data BestChoice f cost = BestChoice
  { bcCost :: cost,
    bcSize :: Int,
    bcNode :: ENode f
  }

instance (Language f, Eq cost) => Eq (BestChoice f cost) where
  leftChoice == rightChoice =
    bcCost leftChoice == bcCost rightChoice
      && bcSize leftChoice == bcSize rightChoice
      && bcNode leftChoice == bcNode rightChoice

termSize :: (Functor f, Foldable f) => Fix f -> Int
termSize = cata ((+ 1) . sum)

termCost :: Functor f => CostAlgebra f cost -> Fix f -> cost
termCost = cata . costAlgebra

minimumMaybe :: Ord key => [(key, value)] -> Maybe value
minimumMaybe =
  fmap snd
    . foldr
      (\candidate maybeBest ->
         Just
           ( maybe
               candidate
               (\bestCandidate -> if fst candidate < fst bestCandidate then candidate else bestCandidate)
               maybeBest
           )
      )
      Nothing
