module Moonlight.EGraph.Effect.Harness
  ( findIdempotent,
    mergeCommutative,
    hashConsIdempotent,
    rebuildIdempotent,
    saturationBounded,
    extractInClass,
    extractOptimal,
    extractDeterministic,
    analysisJoinCommutative,
    analysisJoinAssociative,
    contextGlobalSection,
    contextGlobalSectionInvariantLaw,
    contextRestrictionIdentityLaw,
    contextRestrictionComposition,
    contextMorphismLeftIdentityLaw,
    contextMorphismRightIdentityLaw,
    contextMorphismAssociativeLaw,
    contextRestrictionFunctorialActionLaw,
    contextMergeMonotone,
    proofSoundness,
    proofContextConsistency,
    antiUnifyGeneralizes,
    antiUnifyLeast,
    obstructionComplete,
  )
where

import Moonlight.Core (ZipMatch)
import Moonlight.Core (UnionFindAllocationError)
import Data.IntSet qualified as IntSet
import Moonlight.EGraph.Effect.CoveringSurface
  ( SurfaceKind,
  )
import Moonlight.Algebra
  ( JoinSemilattice
  )
import Moonlight.FiniteLattice
  ( contextLatticeElements,
    leqContext
  )
import Data.Fix (Fix (..))
import Data.IntMap.Strict qualified as IntMap
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec (..))
import Moonlight.EGraph.Pure.AntiUnify (BinaryLGGResult (..), antiUnify)
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    contextMerge,
  )
import Moonlight.EGraph.Pure.Context.Core
  ( cegBase,
    cegLattice,
  )
import Moonlight.EGraph.Pure.Extraction
  ( CostAlgebra,
    ExtractionResult (..),
    depthCost,
    extract,
    extractAll,
    stableExtractionSnapshotFromEGraph,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.Core (ConstructorTag, HasConstructorTag, Language)
import Moonlight.EGraph.Pure.Query.RootFilter (RootClassFilter (..))
import Moonlight.EGraph.Pure.Relational (wcojMatchCompiledWithRootFilter)
import Moonlight.Sheaf.Context.Core (contextRefinesTo)
import Moonlight.Sheaf.Context.Algebra
  ( contextEquivalentAt,
    restrictionMap,
  )
import Moonlight.Sheaf.Context.Witness
  ( contextGlobalSectionInvariant,
    contextRestrictionFunctorialAction,
    contextRestrictionIdentity,
    mkContextMorphism,
  )
import Moonlight.Sheaf.Section.Restriction.Witness
  ( contextMorphismAssociative,
    contextMorphismLeftIdentity,
    contextMorphismRightIdentity,
  )
import Moonlight.Sheaf.Obstruction (obstructionReport)
import Moonlight.Core
  ( Pattern
  )
import Moonlight.Core (Substitution (..))
import Moonlight.Rewrite.System
  ( RawRewriteRule
  )
import Moonlight.Rewrite.System (CompiledGuard, combineCompiledGuards, compileGuard, RewriteCondition)
import Moonlight.Rewrite.Algebra (CompiledPatternQuery, compilePatternQuery, singlePatternQuery)
import Moonlight.EGraph.Pure.Context.Proof (ProofGraph (pgGraph, pgProofRegistry), serializeProofLog)
import Moonlight.Rewrite.ProofContext qualified as RewriteProof
import Moonlight.Rewrite.ProofContext (ProofStep (..))
import Moonlight.EGraph.Pure.Rebuild (merge, rebuild)
import Moonlight.EGraph.Test.Saturation
  ( SaturationBudget (..),
    srIterations,
    saturate,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    EGraph,
    canonicalizeClassId,
    classIdKey,
    eGraphClassCount,
    eGraphClasses,
    eGraphHashCons,
    eGraphNodeCount,
    eGraphPendingClassUnions,
    eGraphUnionFind,
  )
import Moonlight.Core qualified as UnionFind

findIdempotent :: ClassId -> EGraph f a -> Bool
findIdempotent classId graph =
  let (firstRoot, unionFindAfterFirst) = UnionFind.find classId (eGraphUnionFind graph)
      (secondRoot, _) = UnionFind.find firstRoot unionFindAfterFirst
   in firstRoot == secondRoot

mergeCommutative :: (Language f, Eq a) => ClassId -> ClassId -> EGraph f a -> Bool
mergeCommutative leftClassId rightClassId graph =
  sameGraphState
    (rebuild (merge leftClassId rightClassId graph))
    (rebuild (merge rightClassId leftClassId graph))

hashConsIdempotent :: Language f => Fix f -> EGraph f a -> Either UnionFindAllocationError Bool
hashConsIdempotent term graph = do
  (firstClassId, firstGraph) <- addTerm term graph
  (secondClassId, secondGraph) <- addTerm term firstGraph
  pure (firstClassId == secondClassId && eGraphNodeCount firstGraph == eGraphNodeCount secondGraph)

rebuildIdempotent :: (Language f, Eq a) => EGraph f a -> Bool
rebuildIdempotent graph =
  let rebuiltGraph = rebuild graph
   in sameGraphState rebuiltGraph (rebuild rebuiltGraph)

relationalEmatchInClass :: (HasConstructorTag f, Show (ConstructorTag f), Show (f ())) => Pattern f -> ClassId -> EGraph f a -> [Substitution]
relationalEmatchInClass patternValue classId =
  fmap snd
    . relationalEmatchWithRootFilter
      (RestrictedRootClasses (IntSet.singleton (classIdKey classId)))
      patternValue

relationalEmatchWithRootFilter ::
  forall f a.
  (HasConstructorTag f, Show (ConstructorTag f), Show (f ())) =>
  RootClassFilter ->
  Pattern f ->
  EGraph f a ->
  [(ClassId, Substitution)]
relationalEmatchWithRootFilter rootFilter patternValue graph =
  case compilePatternQuery combineCompiledGuards compileGuard (singlePatternQuery patternValue) of
    Left _unboundVariables ->
      []
    Right compiledQuery ->
      either
        (const [])
        id
        (wcojMatchCompiledWithRootFilter rootFilter (compiledQuery :: CompiledPatternQuery (CompiledGuard SurfaceKind f) f) graph)

saturationBounded :: (HasConstructorTag f, Show (ConstructorTag f), Show (f ()), Ord a, JoinSemilattice a) => SaturationBudget -> [RawRewriteRule (RewriteCondition SurfaceKind f) f] -> EGraph f a -> Bool
saturationBounded budget rewriteRules graph =
  case saturate budget rewriteRules graph of
    Left _ -> False
    Right saturationReport -> srIterations saturationReport <= sbMaxIterations budget

extractInClass :: (Language f, Ord cost) => CostAlgebra f cost -> ClassId -> EGraph f a -> Bool
extractInClass costAlgebraValue classId graph =
  maybe
    True
    (\extractionResult -> UnionFind.equivalent classId (erClass extractionResult) (eGraphUnionFind graph))
    (stableExtractionSnapshotFromEGraph graph >>= extract costAlgebraValue classId)

extractOptimal :: (Language f, Ord cost) => CostAlgebra f cost -> ClassId -> EGraph f a -> Bool
extractOptimal costAlgebraValue classId graph =
  case stableExtractionSnapshotFromEGraph graph of
    Nothing ->
      True
    Just snapshot ->
      let allResults = extractAll costAlgebraValue snapshot
          canonicalClassId = fst (UnionFind.find classId (eGraphUnionFind graph))
       in maybe
            True
            (\extractionResult -> maybe False ((== erCost extractionResult) . erCost) (IntMap.lookup (classIdKey canonicalClassId) allResults))
            (extract costAlgebraValue classId snapshot)

extractDeterministic :: (Language f, Ord cost) => CostAlgebra f cost -> ClassId -> EGraph f a -> Bool
extractDeterministic costAlgebraValue classId graph =
  case stableExtractionSnapshotFromEGraph graph of
    Nothing ->
      True
    Just snapshot ->
      let firstExtraction = extract costAlgebraValue classId snapshot
          secondExtraction = extract costAlgebraValue classId snapshot
       in fmap erCost firstExtraction == fmap erCost secondExtraction
            && fmap erClass firstExtraction == fmap erClass secondExtraction

analysisJoinCommutative :: Eq a => a -> a -> AnalysisSpec f a -> Bool
analysisJoinCommutative leftValue rightValue analysisSpec =
  let joinValue = asJoin analysisSpec
   in joinValue leftValue rightValue == joinValue rightValue leftValue

analysisJoinAssociative :: Eq a => a -> a -> a -> AnalysisSpec f a -> Bool
analysisJoinAssociative firstValue secondValue thirdValue analysisSpec =
  let joinValue = asJoin analysisSpec
   in joinValue firstValue (joinValue secondValue thirdValue)
        == joinValue (joinValue firstValue secondValue) thirdValue

isGlobalEquivalence :: ClassId -> ClassId -> ContextEGraph f a c -> Bool
isGlobalEquivalence leftClassId rightClassId =
  UnionFind.equivalent leftClassId rightClassId . eGraphUnionFind . cegBase

classesEquivalentAt :: (Language f, Ord c) => c -> ClassId -> ClassId -> ContextEGraph f a c -> Bool
classesEquivalentAt contextValue leftClassId rightClassId contextGraph =
  either
    (const False)
    id
    (contextEquivalentAt contextValue leftClassId rightClassId contextGraph)

checkedLaw :: Either error Bool -> Bool
checkedLaw =
  either (const False) id

contextGlobalSection :: (Language f, Ord c) => ClassId -> ClassId -> ContextEGraph f a c -> Bool
contextGlobalSection leftClassId rightClassId contextGraph =
  isGlobalEquivalence leftClassId rightClassId contextGraph
    == all
      (\context -> classesEquivalentAt context leftClassId rightClassId contextGraph)
      (contextLatticeElements (cegLattice contextGraph))

contextRestrictionComposition :: (Language f, Ord c) => c -> c -> c -> ContextEGraph f a c -> Bool
contextRestrictionComposition firstContext secondContext thirdContext contextGraph =
  let latticeValue = cegLattice contextGraph
      thirdBelowSecond =
        either (const False) id (leqContext latticeValue thirdContext secondContext)
      secondBelowFirst =
        either (const False) id (leqContext latticeValue secondContext firstContext)
   in if thirdBelowSecond && secondBelowFirst
        then
          case (restrictionMap firstContext secondContext contextGraph, restrictionMap secondContext thirdContext contextGraph, restrictionMap firstContext thirdContext contextGraph) of
            (Right firstToSecond, Right secondToThird, Right firstToThird) ->
              firstToThird == IntMap.map (\classId -> IntMap.findWithDefault classId (classIdKey classId) secondToThird) firstToSecond
            _ -> False
        else True

contextRestrictionIdentityLaw :: (Language f, Ord c) => c -> ContextEGraph f a c -> Bool
contextRestrictionIdentityLaw contextValue =
  checkedLaw . contextRestrictionIdentity contextValue

contextMorphismLeftIdentityLaw :: Ord c => c -> ContextEGraph f a c -> Bool
contextMorphismLeftIdentityLaw contextValue contextGraph =
  let latticeValue = cegLattice contextGraph
   in checkedLaw $ do
        maybeContextMorphism <- mkContextMorphism latticeValue contextValue contextValue
        maybe
          (pure False)
          (contextMorphismLeftIdentity (contextRefinesTo latticeValue))
          maybeContextMorphism

contextMorphismRightIdentityLaw :: Ord c => c -> ContextEGraph f a c -> Bool
contextMorphismRightIdentityLaw contextValue contextGraph =
  let latticeValue = cegLattice contextGraph
   in checkedLaw $ do
        maybeContextMorphism <- mkContextMorphism latticeValue contextValue contextValue
        maybe
          (pure False)
          (contextMorphismRightIdentity (contextRefinesTo latticeValue))
          maybeContextMorphism

contextMorphismAssociativeLaw :: Ord c => c -> c -> c -> c -> ContextEGraph f a c -> Bool
contextMorphismAssociativeLaw firstContext secondContext thirdContext fourthContext contextGraph =
  let latticeValue = cegLattice contextGraph
   in checkedLaw $ do
        firstWitness <- mkContextMorphism latticeValue firstContext secondContext
        secondWitness <- mkContextMorphism latticeValue secondContext thirdContext
        thirdWitness <- mkContextMorphism latticeValue thirdContext fourthContext
        case (firstWitness, secondWitness, thirdWitness) of
          (Just witnessOne, Just witnessTwo, Just witnessThree) ->
            contextMorphismAssociative (contextRefinesTo latticeValue) witnessOne witnessTwo witnessThree
          _ ->
            pure True

contextRestrictionFunctorialActionLaw :: (Language f, Ord c) => c -> c -> c -> ContextEGraph f a c -> Bool
contextRestrictionFunctorialActionLaw firstContext secondContext thirdContext contextGraph =
  let latticeValue = cegLattice contextGraph
   in case (mkContextMorphism latticeValue firstContext secondContext, mkContextMorphism latticeValue secondContext thirdContext) of
        (Right (Just firstContextMorphism), Right (Just secondContextMorphism)) ->
          checkedLaw (contextRestrictionFunctorialAction firstContextMorphism secondContextMorphism contextGraph)
        (Right _, Right _) ->
          True
        _ ->
          False

contextGlobalSectionInvariantLaw :: (Language f, Ord c) => c -> c -> ContextEGraph f a c -> Bool
contextGlobalSectionInvariantLaw sourceContext targetContext contextGraph =
  let latticeValue = cegLattice contextGraph
   in case mkContextMorphism latticeValue sourceContext targetContext of
        Right (Just contextMorphism) ->
          checkedLaw (contextGlobalSectionInvariant contextMorphism contextGraph)
        Right Nothing ->
          True
        Left _ ->
          False

contextMergeMonotone :: (Language f, JoinSemilattice a, Ord c) => c -> ClassId -> ClassId -> ContextEGraph f a c -> Bool
contextMergeMonotone context leftClassId rightClassId contextGraph =
  case contextMerge context leftClassId rightClassId contextGraph of
    Left _ -> False
    Right mergedContextGraph ->
      classesEquivalentAt context leftClassId rightClassId mergedContextGraph
        && all
          ( \targetContext ->
              if either (const False) id (leqContext (cegLattice mergedContextGraph) context targetContext)
                then classesEquivalentAt targetContext leftClassId rightClassId mergedContextGraph
                else True
          )
          (contextLatticeElements (cegLattice mergedContextGraph))

proofSoundness :: (graph -> EGraph f a) -> ProofGraph graph f c p -> Bool
proofSoundness projectBaseGraph proofEGraph =
  all
    (\proofStepValue ->
       UnionFind.equivalent
         (psLhsClass proofStepValue)
         (psRhsClass proofStepValue)
         (eGraphUnionFind (projectBaseGraph (pgGraph proofEGraph)))
    )
    (serializeProofLog proofEGraph)

proofContextConsistency :: (Language f, Ord c) => (graph -> ContextEGraph f a c) -> ProofGraph graph f c p -> Bool
proofContextConsistency projectContextGraph proofEGraph =
  all
    ( \proofStepValue ->
        all
          ( \context ->
              let contextGraph = projectContextGraph (pgGraph proofEGraph)
                  canonicalize = canonicalizeClassId (cegBase contextGraph)
               in case contextEquivalentAt context (psLhsClass proofStepValue) (psRhsClass proofStepValue) contextGraph of
                    Left _lookupFailure ->
                      False
                    Right False ->
                      True
                    Right True ->
                      either
                        (const False)
                        (const True)
                        ( RewriteProof.proofBetween
                            (canonicalize (psLhsClass proofStepValue))
                            (canonicalize (psRhsClass proofStepValue))
                            (pgProofRegistry proofEGraph)
                        )
          )
          (contextLatticeElements (cegLattice (projectContextGraph (pgGraph proofEGraph))))
    )
    (serializeProofLog proofEGraph)

antiUnifyGeneralizes :: (Language f, HasConstructorTag f, Show (ConstructorTag f), Show (f ()), ZipMatch f) => ClassId -> ClassId -> EGraph f a -> Bool
antiUnifyGeneralizes leftClassId rightClassId graph =
  case antiUnify depthCost leftClassId rightClassId graph of
    Left _ ->
      False
    Right lggResult ->
      Substitution (binaryLggLeftBindings lggResult) `elem` relationalEmatchInClass (binaryLggPattern lggResult) leftClassId graph
        && Substitution (binaryLggRightBindings lggResult) `elem` relationalEmatchInClass (binaryLggPattern lggResult) rightClassId graph

antiUnifyLeast :: (Language f, ZipMatch f) => ClassId -> ClassId -> EGraph f a -> Bool
antiUnifyLeast leftClassId rightClassId graph =
  case antiUnify depthCost leftClassId rightClassId graph of
    Left _ ->
      False
    Right lggResult ->
      binaryLggSharedStructure lggResult >= 0
        && (UnionFind.equivalent leftClassId rightClassId (eGraphUnionFind graph) || binaryLggSharedStructure lggResult >= 0)

obstructionComplete :: (Language f, Ord c, Eq a) => ClassId -> ClassId -> c -> ContextEGraph f a c -> Bool
obstructionComplete leftClassId rightClassId context contextGraph =
  if classesEquivalentAt context leftClassId rightClassId contextGraph
    then null (obstructionReport leftClassId rightClassId context contextGraph)
    else not (null (obstructionReport leftClassId rightClassId context contextGraph))

sameGraphState :: (Language f, Eq a) => EGraph f a -> EGraph f a -> Bool
sameGraphState leftGraph rightGraph =
  UnionFind.canonicalMap (eGraphUnionFind leftGraph) == UnionFind.canonicalMap (eGraphUnionFind rightGraph)
    && eGraphClasses leftGraph == eGraphClasses rightGraph
    && eGraphHashCons leftGraph == eGraphHashCons rightGraph
    && eGraphPendingClassUnions leftGraph == eGraphPendingClassUnions rightGraph
    && eGraphClassCount leftGraph == eGraphClassCount rightGraph
