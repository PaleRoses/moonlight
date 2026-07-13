{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Moonlight.EGraph.Fuzzy.Simplicial.Backend.Internal
  ( SimplicialBackend (..),
    SimplicialBackendValidationError (..),
    mkSimplicialBackend,
    parallelBlocksFor,
    canonicalPatternBlockOrder,
    canonicalClassBlockOrder,
    parallelFaceKey,
    tagFingerprintOf,
    HomotopicSubstitution (..),
    homotopicMatch,
    homotopicMatchWithRoots,
    homotopicMatchCompiledWithRoots,
    homotopicMatchCompiledWithRootFilter,
  )
where

import Data.Function ((&))
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Moonlight.EGraph.Fuzzy.Simplicial.Complex.Internal
  ( ParallelTagFingerprint,
    SimplexId (..),
    adjacentPairs,
    orderedPair,
    safeIndex,
  )
import Moonlight.EGraph.Fuzzy.Simplicial.Graph.Internal
  ( EGraphLiftAlgebra (..),
    LiftedEGraph (..),
    canonicalClassIds,
    liftEGraphSimplicially,
  )
import Moonlight.EGraph.Fuzzy.Simplicial.Pattern.Internal
  ( PatternLiftAlgebra (..),
    ChildFrame (..),
    PatternFrame (..),
    SimplicialPattern (..),
    liftPatternSimplicially,
    patternFrameVertex,
    patternTriangleWellFormed,
  )
import Moonlight.EGraph.Fuzzy.Simplicial.Shape
  ( ParallelEvidence (..),
    ParallelFaceKey (..),
    ParallelRequirement (..),
  )
import Moonlight.EGraph.Fuzzy.Simplicial.Shape qualified as Shape
import Moonlight.Core
  ( ClassId,
    ConstructorTag,
    HasConstructorTag (..),
    Pattern,
    PatternVar,
    classIdKey,
    patternVarKey,
    sameNodeShape,
  )
import Moonlight.EGraph.Pure.Query.RootFilter (RootClassFilter (..))
import Moonlight.Rewrite.System (CompiledGuard)
import Moonlight.Rewrite.Algebra
  ( CompiledPatternQuery (..),
    patternQueryPatterns,
  )
import Moonlight.Core
  ( Substitution (..),
    emptySubstitution,
    intersectRootedMatches,
    lookupSubst
  )
import Moonlight.EGraph.Pure.Types
  ( EClass (..),
    EGraph,
    ENode (..),
    canonicalizeClassId,
    lookupEClass,
  )
import Numeric.Natural (Natural)
import Prelude

type SimplicialBackendValidationError :: Type
data SimplicialBackendValidationError
  = SimplicialUpperBoundTooSmall !Natural
  deriving stock (Eq, Ord, Show)

type SimplicialBackend :: (Type -> Type) -> Type
data SimplicialBackend f = SimplicialBackend
  { sbUpperBound :: !Natural,
    sbParallelShape :: !(Shape.ParallelShapeAlgebra f),
    sbDeferredFaces :: !Bool
  }

mkSimplicialBackend ::
  Natural ->
  Shape.ParallelShapeAlgebra f ->
  Bool ->
  Either SimplicialBackendValidationError (SimplicialBackend f)
mkSimplicialBackend upperBound parallelShape deferredFaces =
  if upperBound < 2
    then Left (SimplicialUpperBoundTooSmall upperBound)
    else
      Right
        SimplicialBackend
          { sbUpperBound = upperBound,
            sbParallelShape = parallelShape,
            sbDeferredFaces = deferredFaces
          }

parallelBlocksFor :: HasConstructorTag f => SimplicialBackend f -> ConstructorTag f -> Int -> [IntSet]
parallelBlocksFor backend =
  Shape.parallelBlocksFor (sbParallelShape backend)

canonicalPatternBlockOrder :: HasConstructorTag f => SimplicialBackend f -> f (Pattern f) -> [Int] -> [Int]
canonicalPatternBlockOrder backend =
  Shape.canonicalPatternBlockOrder (sbParallelShape backend)

canonicalClassBlockOrder :: HasConstructorTag f => SimplicialBackend f -> f ClassId -> [Int] -> [Int]
canonicalClassBlockOrder backend =
  Shape.canonicalClassBlockOrder (sbParallelShape backend)

tagFingerprintOf :: SimplicialBackend f -> ConstructorTag f -> ParallelTagFingerprint
tagFingerprintOf backend =
  Shape.tagFingerprintOf (sbParallelShape backend)

parallelFaceKey ::
  SimplicialBackend f ->
  ClassId ->
  ConstructorTag f ->
  Int ->
  Int ->
  ClassId ->
  ClassId ->
  ParallelFaceKey
parallelFaceKey backend rootClass tag leftSlot rightSlot leftChild rightChild =
  Shape.parallelFaceKey
    (tagFingerprintOf backend tag)
    rootClass
    leftSlot
    rightSlot
    leftChild
    rightChild

patternLiftAlgebra :: HasConstructorTag f => SimplicialBackend f -> PatternLiftAlgebra f
patternLiftAlgebra backend =
  PatternLiftAlgebra
    { plaUpperBound = sbUpperBound backend,
      plaParallelBlocksFor = parallelBlocksFor backend,
      plaCanonicalPatternBlockOrder = canonicalPatternBlockOrder backend,
      plaTagFingerprint = tagFingerprintOf backend
    }

eGraphLiftAlgebra :: HasConstructorTag f => SimplicialBackend f -> EGraphLiftAlgebra f
eGraphLiftAlgebra backend =
  EGraphLiftAlgebra
    { glaUpperBound = sbUpperBound backend,
      glaDeferredFaces = sbDeferredFaces backend,
      glaParallelBlocksFor = parallelBlocksFor backend,
      glaCanonicalClassBlockOrder = canonicalClassBlockOrder backend,
      glaParallelFaceKey = parallelFaceKey backend,
      glaTagFingerprint = tagFingerprintOf backend
    }

type HomotopicSubstitution :: Type
data HomotopicSubstitution = HomotopicSubstitution
  { hsRootClass :: !ClassId,
    hsSubstitution :: !Substitution,
    hsVertexAssignments :: !(IntMap ClassId),
    hsParallelEvidence :: ![ParallelEvidence]
  }
  deriving stock (Eq, Ord, Show)

type MatchState :: Type
data MatchState = MatchState
  { msSubstitution :: !Substitution,
    msVertexAssignments :: !(IntMap ClassId),
    msParallelEvidence :: ![ParallelEvidence]
  }


type ParallelFaceWitness :: Type
data ParallelFaceWitness
  = EagerParallelFace !ParallelFaceKey !SimplexId
  | DeferredParallelFace !ParallelFaceKey

homotopicMatch ::
  forall f a.
  HasConstructorTag f =>
  SimplicialBackend f ->
  Pattern f ->
  EGraph f a ->
  [(ClassId, HomotopicSubstitution)]
homotopicMatch backend patternValue graph =
  let simplicialPattern = liftPatternSimplicially (patternLiftAlgebra backend) patternValue
      liftedGraph = liftEGraphSimplicially (eGraphLiftAlgebra backend) graph
      initialState =
        MatchState
          { msSubstitution = emptySubstitution,
            msVertexAssignments = IntMap.empty,
            msParallelEvidence = []
          }
   in dedupeMatches
        ( candidateRootClasses simplicialPattern liftedGraph
            >>= \rootClassId ->
              fmap
                ( \finalState ->
                    ( rootClassId,
                      HomotopicSubstitution
                        { hsRootClass = rootClassId,
                          hsSubstitution = msSubstitution finalState,
                          hsVertexAssignments = msVertexAssignments finalState,
                          hsParallelEvidence = reverse (msParallelEvidence finalState)
                        }
                    )
                )
                (matchFrame backend simplicialPattern liftedGraph (spRootFrame simplicialPattern) rootClassId initialState)
        )

homotopicMatchWithRoots ::
  HasConstructorTag f =>
  SimplicialBackend f ->
  Pattern f ->
  EGraph f a ->
  [(ClassId, Substitution)]
homotopicMatchWithRoots backend patternValue graph =
  fmap
    (\(rootClassId, homotopy) -> (rootClassId, hsSubstitution homotopy))
    (homotopicMatch backend patternValue graph)

homotopicMatchCompiledWithRoots ::
  HasConstructorTag f =>
  SimplicialBackend f ->
  CompiledPatternQuery (CompiledGuard capability f) f ->
  EGraph f a ->
  [(ClassId, Substitution)]
homotopicMatchCompiledWithRoots backend compiledQuery graph =
  intersectRootedMatches
    (fmap (\patternValue -> homotopicMatchWithRoots backend patternValue graph) (patternQueryPatterns (cpqQuery compiledQuery)))

homotopicMatchCompiledWithRootFilter ::
  HasConstructorTag f =>
  SimplicialBackend f ->
  RootClassFilter ->
  CompiledPatternQuery (CompiledGuard capability f) f ->
  EGraph f a ->
  [(ClassId, Substitution)]
homotopicMatchCompiledWithRootFilter backend rootClassFilter compiledQuery graph =
  case rootClassFilter of
    RestrictedRootClasses classKeys
      | IntSet.null classKeys ->
          []
    _ ->
      filterRootedMatches rootClassFilter (homotopicMatchCompiledWithRoots backend compiledQuery graph)

filterRootedMatches :: RootClassFilter -> [(ClassId, Substitution)] -> [(ClassId, Substitution)]
filterRootedMatches rootClassFilter =
  filter
    ( \(rootClassId, _) ->
        case rootClassFilter of
          AllRootClasses -> True
          RestrictedRootClasses allowedRootClasses ->
            IntSet.member (classIdKey rootClassId) allowedRootClasses
    )

matchFrame ::
  forall f a.
  HasConstructorTag f =>
  SimplicialBackend f ->
  SimplicialPattern f ->
  LiftedEGraph f a ->
  PatternFrame f ->
  ClassId ->
  MatchState ->
  [MatchState]
matchFrame backend simplicialPattern liftedGraph frame classId state =
  let graph = leBaseGraph liftedGraph
      canonicalRoot = canonicalizeClassId graph classId
   in case bindVertex graph (patternFrameVertex frame) canonicalRoot state of
        Nothing -> []
        Just stateWithVertex ->
          case frame of
            PatternVarFrame _ patternVar ->
              bindVar graph patternVar canonicalRoot stateWithVertex
            PatternNodeFrame _ expectedTag expectedNode childFrames parallelReqs ->
              maybe
                []
                ( \eClass ->
                    Set.toAscList (eClassNodes eClass)
                      >>= \case
                        ENode candidateNode
                          | constructorTag candidateNode == expectedTag && sameNodeShape expectedNode candidateNode ->
                              let candidateChildren = fmap (canonicalizeClassId graph) (toList candidateNode)
                               in matchChildren backend simplicialPattern liftedGraph canonicalRoot candidateNode childFrames parallelReqs candidateChildren stateWithVertex
                          | otherwise ->
                              []
                )
                (lookupEClass graph canonicalRoot)

matchChildren ::
  forall f a.
  HasConstructorTag f =>
  SimplicialBackend f ->
  SimplicialPattern f ->
  LiftedEGraph f a ->
  ClassId ->
  f ClassId ->
  IntMap (ChildFrame f) ->
  [ParallelRequirement] ->
  [ClassId] ->
  MatchState ->
  [MatchState]
matchChildren backend simplicialPattern liftedGraph rootClass candidateNode childFrames parallelReqs candidateChildren initialState =
  let coveredSlots = foldMap (IntSet.fromList . prRawSlots) parallelReqs
      plainSlots =
        IntMap.keys childFrames
          & filter (`IntSet.notMember` coveredSlots)
      afterPlain =
        foldl'
          (\states slot -> states >>= matchPlainSlot slot)
          [initialState]
          plainSlots
   in foldl'
        (\states requirement -> states >>= matchParallelRequirement requirement)
        afterPlain
        parallelReqs
  where
    matchPlainSlot slot state =
      case (IntMap.lookup slot childFrames, safeIndex slot candidateChildren) of
        (Just childFrame, Just childClassId) ->
          matchFrame backend simplicialPattern liftedGraph (childFrameSubpattern childFrame) childClassId state
        _ ->
          []

    matchParallelRequirement requirement state
      | not (all (patternTriangleWellFormed simplicialPattern) (prTriangles requirement)) =
          []
      | otherwise =
          let patternOrder = prCanonicalPatternSlots requirement
              candidateOrder = canonicalClassBlockOrder backend candidateNode (prRawSlots requirement)
           in if length patternOrder /= length candidateOrder
                then []
                else
                  let afterBlockChildren =
                        foldl'
                          ( \states (patternSlot, candidateSlot) ->
                              states
                                >>= \currentState ->
                                  case (IntMap.lookup patternSlot childFrames, safeIndex candidateSlot candidateChildren) of
                                    (Just childFrame, Just childClassId) ->
                                      matchFrame backend simplicialPattern liftedGraph (childFrameSubpattern childFrame) childClassId currentState
                                    _ ->
                                      []
                          )
                          [state]
                          (zip patternOrder candidateOrder)
                      adjacentCandidatePairs = adjacentPairs candidateOrder
                   in afterBlockChildren
                        >>= \currentState ->
                          maybe
                            []
                            ( \parallelEvidence ->
                                [ currentState
                                    { msParallelEvidence = reverse parallelEvidence <> msParallelEvidence currentState
                                    }
                                ]
                            )
                            (fmap catMaybes (traverse (parallelEvidenceForCandidatePair currentState) adjacentCandidatePairs))

    parallelEvidenceForCandidatePair _ (leftSlot, rightSlot) =
      case (safeIndex leftSlot candidateChildren, safeIndex rightSlot candidateChildren) of
        (Just leftChild, Just rightChild)
          | leftChild == rightChild ->
              Just Nothing
          | otherwise ->
              fmap
                Just
                ( mkParallelEvidence
                    liftedGraph
                    backend
                    rootClass
                    (constructorTag candidateNode)
                    leftSlot
                    rightSlot
                    leftChild
                    rightChild
                )
        _ ->
          Nothing

mkParallelEvidence ::
  forall f a.
  LiftedEGraph f a ->
  SimplicialBackend f ->
  ClassId ->
  ConstructorTag f ->
  Int ->
  Int ->
  ClassId ->
  ClassId ->
  Maybe ParallelEvidence
mkParallelEvidence liftedGraph backend rootClass tag leftSlot rightSlot leftChild rightChild =
  let (normalizedLeftSlot, normalizedRightSlot) = orderedPair leftSlot rightSlot
      (normalizedLeftChild, normalizedRightChild) = orderedPair leftChild rightChild
   in fmap
        ( \witness ->
            ParallelEvidence
              { peFaceKey = parallelFaceKey backend rootClass tag leftSlot rightSlot leftChild rightChild,
                peFaceSimplex = witnessSimplex witness,
                peLeftSlot = normalizedLeftSlot,
                peRightSlot = normalizedRightSlot,
                peLeftChild = normalizedLeftChild,
                peRightChild = normalizedRightChild
              }
        )
    (resolveParallelFaceWitness liftedGraph backend rootClass tag leftSlot rightSlot leftChild rightChild)

resolveParallelFaceWitness ::
  forall f a.
  LiftedEGraph f a ->
  SimplicialBackend f ->
  ClassId ->
  ConstructorTag f ->
  Int ->
  Int ->
  ClassId ->
  ClassId ->
  Maybe ParallelFaceWitness
resolveParallelFaceWitness liftedGraph backend rootClass tag leftSlot rightSlot leftChild rightChild =
  let faceKey = parallelFaceKey backend rootClass tag leftSlot rightSlot leftChild rightChild
   in if leDeferredFaces liftedGraph
        then Just (DeferredParallelFace faceKey)
        else fmap (EagerParallelFace faceKey) (Map.lookup faceKey (leEagerFaces liftedGraph))

witnessSimplex :: ParallelFaceWitness -> Maybe SimplexId
witnessSimplex witness =
  case witness of
    EagerParallelFace _ simplexId -> Just simplexId
    DeferredParallelFace {} -> Nothing

bindVar :: EGraph f a -> PatternVar -> ClassId -> MatchState -> [MatchState]
bindVar graph patternVar classId state =
  let canonicalClassId = canonicalizeClassId graph classId
   in case msSubstitution state of
        Substitution entries ->
          case lookupSubst patternVar (msSubstitution state) of
            Nothing ->
              [ state
                  { msSubstitution =
                      Substitution (IntMap.insert (patternVarKey patternVar) canonicalClassId entries)
                  }
              ]
            Just existingClassId
              | canonicalizeClassId graph existingClassId == canonicalClassId ->
                  [state]
              | otherwise ->
                  []

bindVertex :: EGraph f a -> SimplexId -> ClassId -> MatchState -> Maybe MatchState
bindVertex graph simplexId classId state =
  let canonicalClassId = canonicalizeClassId graph classId
   in case IntMap.lookup (simplexIdKey simplexId) (msVertexAssignments state) of
        Nothing ->
          Just
            state
              { msVertexAssignments =
                  IntMap.insert (simplexIdKey simplexId) canonicalClassId (msVertexAssignments state)
              }
        Just existingClassId
          | canonicalizeClassId graph existingClassId == canonicalClassId ->
              Just state
          | otherwise ->
              Nothing

candidateRootClasses ::
  HasConstructorTag f =>
  SimplicialPattern f ->
  LiftedEGraph f a ->
  [ClassId]
candidateRootClasses simplicialPattern liftedGraph =
  case spRootFrame simplicialPattern of
    PatternVarFrame _ _ ->
      canonicalClassIds (leBaseGraph liftedGraph)
    PatternNodeFrame _ tag _ _ _ ->
      Map.findWithDefault [] tag (leRootsByTag liftedGraph)

dedupeMatches :: Ord homotopic => [(ClassId, homotopic)] -> [(ClassId, homotopic)]
dedupeMatches =
  Set.toAscList . Set.fromList
