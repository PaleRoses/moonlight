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
    ExtractionDependencyCoverProbe (..),
    probeExtractionDependencyCover,
    completeExtractionDependencyCover,
    lookupExtractionClass,
    StableExtractionSnapshot,
    stableExtractionSnapshotFromEGraph,
    stableExtractionSnapshotTable,
    ExtractionWorkBudget (..),
    ExtractionBudgetExhaustion (..),
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
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Maybe (maybeToList)
import Moonlight.Core (Language, reachabilityFromInt)
import Moonlight.EGraph.Pure.Change
  ( GraphPhase (..),
    eGraphPhase,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    ENode (..),
    EGraph,
    canonicalizeClassId,
    classIdKey,
    eGraphAnalysis,
    eGraphStore,
  )
import Moonlight.EGraph.Pure.Structural.Store
  ( structuralTuplesForResultKey,
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
uncheckedExtractionTableFromEGraph :: EGraph f a -> ExtractionTable f a
uncheckedExtractionTableFromEGraph graph =
  ExtractionTable
    { extractionClasses =
        IntMap.mapWithKey
          ( \classKey analysisValue ->
              ExtractionClass
                { extractionClassAnalysis = analysisValue,
                  extractionClassNodes = structuralTuplesForResultKey classKey (eGraphStore graph)
                }
          )
          analysisMap,
      extractionCanonicalClass =
        \classId ->
          let canonicalClass = canonicalizeClassId graph classId
           in if IntMap.member (classIdKey canonicalClass) analysisMap
                then Just canonicalClass
                else Nothing
    }
  where
    analysisMap =
      eGraphAnalysis graph

type ExtractionDependencyCoverProbe :: (Type -> Type) -> Type -> Type
data ExtractionDependencyCoverProbe f a
  = ClosedExtractionDependencyCover !(ExtractionTable f a)
  | DeferredExtractionDependencyCover !(ExtractionTable f a) !IntSet.IntSet !IntSet.IntSet

-- | Close a cheap dependency cover, or defer descent before discovery costs
-- more than the point extraction it was meant to save.
probeExtractionDependencyCover :: Foldable f => ClassId -> ExtractionTable f a -> Maybe (ExtractionDependencyCoverProbe f a)
probeExtractionDependencyCover requestedClass table = do
  canonicalRequestedClass <- extractionCanonicalClass table requestedClass
  let seed =
        IntSet.singleton (classIdKey canonicalRequestedClass)
  pure
    ( case dependencyKeysWithin pointDependencyCoverLimit (extractionChildKeys table) seed of
        Left (visited, frontier) ->
          DeferredExtractionDependencyCover table visited frontier
        Right dependencyKeys ->
          ClosedExtractionDependencyCover (restrictExtractionTable table dependencyKeys)
    )

completeExtractionDependencyCover :: Foldable f => ExtractionDependencyCoverProbe f a -> ExtractionTable f a
completeExtractionDependencyCover coverProbe =
  case coverProbe of
    ClosedExtractionDependencyCover dependencyTable ->
      dependencyTable
    DeferredExtractionDependencyCover table visited frontier ->
      restrictExtractionTable
        table
        ( visited
            <> reachabilityFromInt
              (\classKey -> IntSet.difference (extractionChildKeys table classKey) visited)
              frontier
        )

restrictExtractionTable :: ExtractionTable f a -> IntSet.IntSet -> ExtractionTable f a
restrictExtractionTable table dependencyKeys =
  uncheckedExtractionTable
    (IntMap.restrictKeys (extractionClasses table) dependencyKeys)
    ( \classId -> do
        canonicalClass <- extractionCanonicalClass table classId
        if IntSet.member (classIdKey canonicalClass) dependencyKeys
          then Just canonicalClass
          else Nothing
    )

extractionChildKeys :: Foldable f => ExtractionTable f a -> Int -> IntSet.IntSet
extractionChildKeys table classKey =
  IntSet.fromList
    [ classIdKey canonicalChild
      | extractionClassValue <- maybeToList (IntMap.lookup classKey (extractionClasses table)),
        ENode childClassIds <- extractionClassNodes extractionClassValue,
        childClassId <- toList childClassIds,
        canonicalChild <- maybeToList (extractionCanonicalClass table childClassId)
    ]

dependencyKeysWithin :: Natural -> (Int -> IntSet.IntSet) -> IntSet.IntSet -> Either (IntSet.IntSet, IntSet.IntSet) IntSet.IntSet
dependencyKeysWithin limit expand =
  descend limit IntSet.empty
  where
    descend remaining visited frontier =
      case IntSet.minView frontier of
        Nothing ->
          Right visited
        Just (classKey, rest)
          | remaining == 0 ->
              Left (visited, frontier)
          | otherwise ->
              let nextVisited =
                    IntSet.insert classKey visited
               in descend
                    (remaining - 1)
                    nextVisited
                    (IntSet.union rest (IntSet.difference (expand classKey) nextVisited))

pointDependencyCoverLimit :: Natural
pointDependencyCoverLimit = 64

lookupExtractionClass :: ExtractionTable f a -> ClassId -> Maybe (ExtractionClass f a)
lookupExtractionClass table classId = do
  canonicalClassId <- extractionCanonicalClass table classId
  IntMap.lookup (classIdKey canonicalClassId) (extractionClasses table)

type StableExtractionSnapshot :: (Type -> Type) -> Type -> Type
newtype StableExtractionSnapshot f a = StableExtractionSnapshot
  { stableExtractionSnapshotTable :: ExtractionTable f a
  }

stableExtractionSnapshotFromEGraph :: EGraph f a -> Maybe (StableExtractionSnapshot f a)
stableExtractionSnapshotFromEGraph graph =
  if eGraphPhase graph == Stable
    then Just (StableExtractionSnapshot (uncheckedExtractionTableFromEGraph graph))
    else Nothing
{-# INLINE stableExtractionSnapshotFromEGraph #-}

-- | Maximum extraction work. One step is either one Knuth class
-- finalization or, after a cost algebra violates strict superiority, one
-- whole-table improvement pass. The two interpreters consume the same finite
-- credit supply; switching interpreters does not reset the budget.
type ExtractionWorkBudget :: Type
newtype ExtractionWorkBudget = ExtractionWorkBudget
  { extractionWorkBudgetSteps :: Natural
  }
  deriving stock (Eq, Ord, Show)

type ExtractionBudgetExhaustion :: Type
data ExtractionBudgetExhaustion = ExtractionBudgetExhaustion
  { ebeBudget :: !ExtractionWorkBudget,
    ebeConsumedWorkSteps :: !Natural,
    ebeTotalClassCount :: !Int,
    ebeResolvedClassCount :: !Int,
    ebeUnresolvedClassCount :: !Int
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
