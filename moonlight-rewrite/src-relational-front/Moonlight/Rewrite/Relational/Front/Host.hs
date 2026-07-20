{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Canonical e-class host for the relational front.
-- Owns union-find state, nodes by class, class sorts, node owners, backend
-- section projection, and the revision counter.
-- Contracts: host terms are ground and sort-correct; program merges update
-- classes eagerly, while congruence and backend views settle at the barrier.
module Moonlight.Rewrite.Relational.Front.Host
  ( Host,
    hostBackend,
    hostCanonicalClass,
    hostClassCount,
    hostNodeClasses,
    hostClassHasWitness,
    hostClassWitness,
    hostClassWitnessMemoized,
    hostLookupTermClass,
    HostProgramResult (..),
    runHostRewriteProgram,
    HostRebuildResult (..),
    rebuildHostBarrier,
    HostTerm (..),
    emptyHost,
    hostFromTerm,
    hostFromTerms,
    hostFromNodes,
    hostFromNodeClasses,
    hostRevision,
    hostSections,
    hostSectionsFromClasses,
  )
where

import Control.Monad.Trans.Class
  ( lift,
  )
import Control.Monad
  ( foldM,
  )
import Control.Monad.ST
  ( ST,
  )
import Data.Foldable
  ( traverse_,
  )
import Control.Monad.Trans.State.Strict
  ( StateT (..),
    get,
    modify',
    runStateT,
  )
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
  ( isJust,
    listToMaybe,
  )
import Moonlight.Core
  ( ClassId (..),
    classIdKey,
  )
import Moonlight.Core.EGraph.Program
  ( EGraphProgram,
    EGraphProgramOp (..),
    EGraphProgramEffect,
    foldEGraphProgram,
    insertedFreshNodeEffect,
    requiredClassMergeEffect,
  )
import Moonlight.Core (UnionFind)
import Moonlight.Core qualified as UnionFind
import Moonlight.Core
  ( UnionFindEditor,
    UnionOutcome (..),
  )
import Moonlight.Core qualified as UnionFindTransaction
import Moonlight.EGraph.Pure.Delta
  ( EGraphEditDelta,
    classUnionDelta,
    eGraphEditDeltaClassUnions,
    eGraphEditDeltaNull,
  )
import Data.Fix
  ( Fix (..),
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    RepKey (..),
    tupleKeyFromRepKeys,
  )
import Moonlight.Rewrite.Runtime (RewriteApplicationError (..))
import Moonlight.Rewrite.DSL
  ( HTraversable (..),
    K (..),
    Node (..),
    NodeTag,
    RewriteSignature (..),
    SomeSortWitness (..),
    SortWitness (..),
    nodeChildren,
    nodeSort,
    someSortWitnessName,
    someSortWitnessSortName,
    sortWitnessName,
    sortWitnessSortName,
  )
import Moonlight.Rewrite.DSL
  ( Term (..),
    typedVarName,
    typedVarSort,
  )
import Moonlight.Rewrite.Relational
  ( RewriteRelationalHost,
    emptyRewriteRelationalHost,
    replaceRewriteRelationalHost,
    rewriteRelationalHostSections,
  )
import Moonlight.Rewrite.Relational.Front.Error
  ( HostBuildError (..),
  )

-- | Canonical rewrite host state. The relational backend is a projection of
-- this e-class store; it is not a second source of truth.
data Host sig = Host
  { hRevision :: {-# UNPACK #-} !Int,
    hUnionFind :: !UnionFind,
    hNodesByClass :: !(IntMap [Node sig ClassId]),
    hClassSorts :: !(IntMap SomeSortWitness),
    hNodeOwners :: !(Map (Node sig ClassId) ClassId),
    hBackend :: RewriteRelationalHost (NodeTag sig)
  }

hostBackend :: Host sig -> RewriteRelationalHost (NodeTag sig)
hostBackend =
  hBackend

data HostTerm sig where
  HostTerm :: !(Term sig sort) -> HostTerm sig

emptyHost :: Host sig
emptyHost =
  Host
    { hRevision = 0,
      hUnionFind = UnionFind.emptyUnionFind,
      hNodesByClass = IntMap.empty,
      hClassSorts = IntMap.empty,
      hNodeOwners = Map.empty,
      hBackend = emptyRewriteRelationalHost
    }


hostRevision :: Host sig -> Int
hostRevision =
  hRevision


hostClassCount :: Host sig -> Int
hostClassCount =
  IntMap.size . hNodesByClass


hostNodeClasses :: Host sig -> [(ClassId, [Node sig ClassId])]
hostNodeClasses =
  fmap
    (\(classKey, nodes) -> (ClassId classKey, nodes))
    . IntMap.toAscList
    . hNodesByClass

hostCanonicalClass :: Host sig -> ClassId -> Maybe ClassId
hostCanonicalClass host classId =
  UnionFind.canonicalClass classId (hUnionFind host)

hostClassWitness ::
  RewriteSignature sig =>
  Int ->
  ClassId ->
  Host sig ->
  Maybe (Fix (Node sig))
hostClassWitness =
  hostClassWitnessWith Fix

-- | Resolve a witness through an immutable cache, preserving DAG sharing.
hostClassWitnessMemoized ::
  forall sig.
  RewriteSignature sig =>
  Int ->
  ClassId ->
  Host sig ->
  IntMap (Fix (Node sig)) ->
  (IntMap (Fix (Node sig)), Maybe (Fix (Node sig)))
hostClassWitnessMemoized fuel classId host initialCache =
  case runStateT (witnessClass (max 0 fuel) IntSet.empty classId) initialCache of
    Nothing ->
      (initialCache, Nothing)
    Just (witness, cache) ->
      (cache, Just witness)
  where
    witnessClass ::
      Int ->
      IntSet ->
      ClassId ->
      StateT (IntMap (Fix (Node sig))) Maybe (Fix (Node sig))
    witnessClass remaining activeClasses currentClass
      | remaining <= 0 =
          lift Nothing
      | otherwise = do
          rootClass <-
            lift (hostCanonicalClass host currentClass)
          cache <-
            get
          let rootKey =
                classIdKey rootClass
          case IntMap.lookup rootKey cache of
            Just cachedWitness ->
              pure cachedWitness
            Nothing
              | IntSet.member rootKey activeClasses ->
                  lift Nothing
              | otherwise -> do
                  witnessNode <-
                    lift
                      ( listToMaybe
                          (IntMap.findWithDefault [] rootKey (hNodesByClass host))
                      )
                  witnessChildren <-
                    traverse
                      (witnessClass (remaining - 1) (IntSet.insert rootKey activeClasses))
                      witnessNode
                  let witness =
                        Fix witnessChildren
                  modify' (IntMap.insert rootKey witness)
                  pure witness

-- | Test witness availability without allocating its term.
hostClassHasWitness ::
  RewriteSignature sig =>
  Int ->
  ClassId ->
  Host sig ->
  Bool
hostClassHasWitness fuel classId host =
  isJust (hostClassWitnessWith (const ()) fuel classId host)

hostClassWitnessWith ::
  RewriteSignature sig =>
  (Node sig result -> result) ->
  Int ->
  ClassId ->
  Host sig ->
  Maybe result
hostClassWitnessWith buildWitness fuel classId host =
  witnessClass (max 0 fuel) classId
  where
    witnessClass remaining currentClass
      | remaining <= 0 =
          Nothing
      | otherwise = do
          rootClass <-
            hostCanonicalClass host currentClass
          witnessNode <-
            listToMaybe
              (IntMap.findWithDefault [] (classIdKey rootClass) (hNodesByClass host))
          buildWitness <$> traverse (witnessClass (remaining - 1)) witnessNode

hostLookupTermClass ::
  forall sig sort.
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  Term sig sort ->
  Host sig ->
  Either HostBuildError (Maybe ClassId)
hostLookupTermClass termValue host =
  go termValue
  where
    go ::
      forall sort'.
      Term sig sort' ->
      Either HostBuildError (Maybe ClassId)
    go =
      \case
        TVar typedVariable ->
          Left
            ( HostTermContainsVariable
                (typedVarName typedVariable)
                (typedVarSort typedVariable)
            )

        TNode sigNode -> do
          childMatches <-
            htraverse
              (\childTerm -> K <$> go childTerm)
              sigNode

          let maybeKeyedNode =
                htraverse
                  (\(K maybeClass) -> K <$> maybeClass)
                  childMatches

          pure $ do
            keyedNode <- maybeKeyedNode
            owner <- Map.lookup (Node keyedNode) (hNodeOwners host)
            hostCanonicalClass host owner

hostFromTerm ::
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  Term sig sort ->
  Either HostBuildError (Host sig, ClassId)
hostFromTerm termValue =
  case runStateT (internTerm termValue) emptyHostInternState of
    Left hostError ->
      Left hostError

    Right (rootKey, internState) ->
      Right (hostFromInternState internState, rootKey)

hostFromTerms ::
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  [HostTerm sig] ->
  Either HostBuildError (Host sig, [ClassId])
hostFromTerms terms =
  case runStateT (traverse internHostTerm terms) emptyHostInternState of
    Left hostError ->
      Left hostError

    Right (rootKeys, internState) ->
      Right (hostFromInternState internState, rootKeys)

hostFromNodes ::
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  IntMap (Node sig Int) ->
  Either HostBuildError (Host sig)
hostFromNodes nodes =
  hostFromNodeClasses (fmap (: []) nodes)

hostFromNodeClasses ::
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  IntMap [Node sig Int] ->
  Either HostBuildError (Host sig)
hostFromNodeClasses nodeClasses = do
  let knownKeys =
        IntMap.keysSet nodeClasses

  nodeClasses' <-
    IntMap.traverseWithKey
      (validateHostNodeClass knownKeys)
      nodeClasses
  classSorts <-
    IntMap.traverseWithKey
      classSortFromNodes
      nodeClasses'
  traverse_
    (\(classKey, nodes) -> traverse_ (validateHostNodeChildSorts classSorts classKey) nodes)
    (IntMap.toAscList nodeClasses')

  let unionFind =
        UnionFind.fromClassIds (ClassId <$> IntMap.keys nodeClasses')
      host =
        materializeHost
          0
          unionFind
          nodeClasses'
          classSorts
          (nodeOwnersFromClasses nodeClasses')
          (hostSectionsFromClasses nodeClasses')

  case rebuildHostBarrier host of
    Right rebuildResult ->
      Right (hrrHost rebuildResult)

    Left rebuildError ->
      Left (hostBuildErrorFromRebuild classSorts rebuildError)

-- | Interprets a rewrite program against the host. Merges update the
-- union-find eagerly but congruence closure is deferred: the resulting host
-- is sound for canonicalized reads, while quotient-level views
-- ('hostNodeClasses', the relational backend) lag until 'rebuildHostBarrier'.
runHostRewriteProgram ::
  forall sig a.
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  EGraphProgram RewriteApplicationError (Node sig ClassId) a ->
  Host sig ->
  Either RewriteApplicationError (HostProgramResult sig a)
runHostRewriteProgram programValue initialHost = do
  (resultValue, finalState) <-
    runStateT
      (foldEGraphProgram interpretHostProgramOp programValue)
      (emptyHostProgramState initialHost)
  pure
    HostProgramResult
      { hprHost = hpsHost finalState,
        hprValue = resultValue,
        hprEffect = hpsEffect finalState,
        hprDelta = hpsDelta finalState,
        hprDirtyResultKeys = hpsDirtyResultKeys finalState
      }
  where
    interpretHostProgramOp ::
      EGraphProgramOp RewriteApplicationError (Node sig ClassId) next ->
      StateT (HostProgramState sig) (Either RewriteApplicationError) next
    interpretHostProgramOp programOp =
      StateT $ \state ->
        case programOp of
          CanonicalizeClass classId continue -> do
            canonicalClass <-
              requireHostClass (hpsHost state) classId
            pure (continue canonicalClass, state)

          AddNode nodeValue continue -> do
            (host', classId, addChanged) <-
              hostAddNode nodeValue (hpsHost state)
            let effect' =
                  if addChanged
                    then hpsEffect state <> insertedFreshNodeEffect
                    else hpsEffect state
            pure
              ( continue classId,
                state
                  { hpsHost = host',
                    hpsEffect = effect',
                    hpsDirtyResultKeys =
                      if addChanged
                        then IntSet.insert (classIdKey classId) (hpsDirtyResultKeys state)
                        else hpsDirtyResultKeys state
                  }
              )

          MergeClasses leftClass rightClass continue -> do
            leftRoot <-
              requireHostClass (hpsHost state) leftClass
            rightRoot <-
              requireHostClass (hpsHost state) rightClass
            (host', mergedClass, mergeChanged) <-
              hostMergeClasses leftRoot rightRoot (hpsHost state)
            let effect' =
                  if mergeChanged
                    then hpsEffect state <> requiredClassMergeEffect
                    else hpsEffect state
                unionDelta =
                  if mergeChanged
                    then classUnionDelta leftRoot rightRoot
                    else mempty
            pure
              ( continue mergedClass,
                state
                  { hpsHost = host',
                    hpsEffect = effect',
                    hpsDelta = hpsDelta state <> unionDelta,
                    hpsDirtyResultKeys = hpsDirtyResultKeys state <> editDeltaDirtyResultKeys unionDelta
                  }
              )

          AbortProgram applicationError ->
            Left applicationError

data HostProgramResult sig a = HostProgramResult
  { hprHost :: !(Host sig),
    hprValue :: !a,
    hprEffect :: !EGraphProgramEffect,
    hprDelta :: !EGraphEditDelta,
    hprDirtyResultKeys :: !IntSet
  }

data HostProgramState sig = HostProgramState
  { hpsHost :: !(Host sig),
    hpsEffect :: !EGraphProgramEffect,
    hpsDelta :: !EGraphEditDelta,
    hpsDirtyResultKeys :: !IntSet
  }

emptyHostProgramState :: Host sig -> HostProgramState sig
emptyHostProgramState host =
  HostProgramState
    { hpsHost = host,
      hpsEffect = mempty,
      hpsDelta = mempty,
      hpsDirtyResultKeys = IntSet.empty
    }

data HostInternState sig = HostInternState
  { hisNodeKeys :: !(Map (Node sig ClassId) ClassId),
    hisUnionFind :: !UnionFind,
    hisNodesByClass :: !(IntMap [Node sig ClassId]),
    hisClassSorts :: !(IntMap SomeSortWitness)
  }

emptyHostInternState :: HostInternState sig
emptyHostInternState =
  HostInternState
    { hisNodeKeys = Map.empty,
      hisUnionFind = UnionFind.emptyUnionFind,
      hisNodesByClass = IntMap.empty,
      hisClassSorts = IntMap.empty
    }

hostFromInternState ::
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  HostInternState sig ->
  Host sig
hostFromInternState internState =
  materializeHost
    0
    (hisUnionFind internState)
    (hisNodesByClass internState)
    (hisClassSorts internState)
    (hisNodeKeys internState)
    (hostSectionsFromClasses (hisNodesByClass internState))

internHostTerm ::
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  HostTerm sig ->
  StateT (HostInternState sig) (Either HostBuildError) ClassId
internHostTerm (HostTerm termValue) =
  internTerm termValue

internTerm ::
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  Term sig sort ->
  StateT (HostInternState sig) (Either HostBuildError) ClassId
internTerm =
  \case
    TVar typedVariable ->
      lift
        ( Left
            ( HostTermContainsVariable
                (typedVarName typedVariable)
                (typedVarSort typedVariable)
            )
        )

    TNode sigNode -> do
      keyedNode <-
        htraverse
          (\childTerm -> K <$> internTerm childTerm)
          sigNode
      internNode (Node keyedNode)

internNode ::
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  Node sig ClassId ->
  StateT (HostInternState sig) (Either HostBuildError) ClassId
internNode nodeValue = do
  internState <- get
  case Map.lookup nodeValue (hisNodeKeys internState) of
    Just classId ->
      pure classId

    Nothing -> do
      (classId, unionFind') <-
        case UnionFind.makeSet (hisUnionFind internState) of
          Left allocationError ->
            lift (Left (HostClassIdAllocationFailed allocationError))
          Right allocation ->
            pure allocation
      let classKey =
            classIdKey classId
      modify'
        ( \stateValue ->
            stateValue
              { hisNodeKeys = Map.insert nodeValue classId (hisNodeKeys stateValue),
                hisUnionFind = unionFind',
                hisNodesByClass =
                  IntMap.insert classKey [nodeValue] (hisNodesByClass stateValue),
                hisClassSorts =
                  IntMap.insert classKey (nodeSort nodeValue) (hisClassSorts stateValue)
              }
        )
      pure classId

hostAddNode ::
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  Node sig ClassId ->
  Host sig ->
  Either RewriteApplicationError (Host sig, ClassId, Bool)
hostAddNode nodeValue host = do
  canonicalNode <-
    traverse (requireHostClass host) nodeValue
  requireHostNodeChildSorts host canonicalNode

  case Map.lookup canonicalNode (hNodeOwners host) >>= hostCanonicalClass host of
    Just existingClass ->
      Right (host, existingClass, False)

    Nothing -> do
      (classId, unionFind') <-
        case UnionFind.makeSet (hUnionFind host) of
          Left allocationError ->
            Left (RewriteClassIdAllocationFailed allocationError)
          Right allocation ->
            Right allocation
      let classKey =
            classIdKey classId
          nodesByClass' =
            IntMap.insert classKey [canonicalNode] (hNodesByClass host)
          classSorts' =
            IntMap.insert classKey (nodeSort canonicalNode) (hClassSorts host)
          owners' =
            Map.insert canonicalNode classId (hNodeOwners host)

      Right
        ( materializeHost
            (hRevision host + 1)
            unionFind'
            nodesByClass'
            classSorts'
            owners'
            (hostSectionsInsertNode classKey (hostSections host) canonicalNode),
          classId,
          True
        )

hostMergeClasses ::
  ClassId ->
  ClassId ->
  Host sig ->
  Either RewriteApplicationError (Host sig, ClassId, Bool)
hostMergeClasses leftClass rightClass host = do
  leftRoot <-
    requireHostClass host leftClass
  rightRoot <-
    requireHostClass host rightClass

  if leftRoot == rightRoot
    then Right (host, leftRoot, False)
    else do
      requireSameHostClassSort host leftRoot rightRoot
      let unionFind' =
            UnionFind.union leftRoot rightRoot (hUnionFind host)
          mergedRoot =
            fst (UnionFind.find leftRoot unionFind')
          host' =
            materializeHost
              (hRevision host + 1)
              unionFind'
              (hNodesByClass host)
              (hClassSorts host)
              (hNodeOwners host)
              (hostSections host)

      pure (host', mergedRoot, True)

data HostRebuildResult sig = HostRebuildResult
  { hrrHost :: !(Host sig),
    hrrDelta :: !EGraphEditDelta,
    hrrDirtyResultKeys :: !IntSet
  }

-- | Restores congruence closure after a batch of deferred merges. Runs the
-- rebuild fixpoint once for the whole batch; nodes that became congruent
-- mid-batch collapse here.
rebuildHostBarrier ::
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  Host sig ->
  Either RewriteApplicationError (HostRebuildResult sig)
rebuildHostBarrier host = do
  rebuildState <-
    rebuildFixedPoint (hUnionFind host) (hClassSorts host) (hNodesByClass host)

  pure
    HostRebuildResult
      { hrrHost =
          materializeHost
            (hRevision host)
            (hrsUnionFind rebuildState)
            (hrsNodesByClass rebuildState)
            (hrsClassSorts rebuildState)
            (nodeOwnersFromClasses (hrsNodesByClass rebuildState))
            (hostSectionsFromClasses (hrsNodesByClass rebuildState)),
        hrrDelta = hrsDelta rebuildState,
        hrrDirtyResultKeys = editDeltaDirtyResultKeys (hrsDelta rebuildState)
      }

editDeltaDirtyResultKeys :: EGraphEditDelta -> IntSet
editDeltaDirtyResultKeys =
  IntSet.fromList
    . concatMap (\(leftClass, rightClass) -> [classIdKey leftClass, classIdKey rightClass])
    . eGraphEditDeltaClassUnions
{-# INLINE editDeltaDirtyResultKeys #-}

data HostRebuildState sig = HostRebuildState
  { hrsUnionFind :: !UnionFind,
    hrsNodesByClass :: !(IntMap [Node sig ClassId]),
    hrsClassSorts :: !(IntMap SomeSortWitness),
    hrsDelta :: !EGraphEditDelta
  }

rebuildFixedPoint ::
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  UnionFind ->
  IntMap SomeSortWitness ->
  IntMap [Node sig ClassId] ->
  Either RewriteApplicationError (HostRebuildState sig)
rebuildFixedPoint unionFind classSorts nodesByClass = do
  rebuildState <-
    rebuildOnce unionFind classSorts nodesByClass

  if eGraphEditDeltaNull (hrsDelta rebuildState)
    then Right rebuildState
    else do
      fixedState <-
        rebuildFixedPoint
          (hrsUnionFind rebuildState)
          (hrsClassSorts rebuildState)
          (hrsNodesByClass rebuildState)
      Right
        fixedState
          { hrsDelta =
              hrsDelta rebuildState <> hrsDelta fixedState
          }

rebuildOnce ::
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  UnionFind ->
  IntMap SomeSortWitness ->
  IntMap [Node sig ClassId] ->
  Either RewriteApplicationError (HostRebuildState sig)
rebuildOnce unionFind classSorts nodesByClass = do
  normalizedEntries <-
    fmap concat
      ( traverse
          normalizeClassEntry
          (IntMap.toAscList nodesByClass)
      )

  ((ownerAccum, compressedParents), compressedUnionFind) <-
    UnionFindTransaction.runUnionFindTransactionEither unionFind $ \unionFindEditor -> do
      ownerResult <-
        foldM
          (observeNodeOwner unionFindEditor)
          ( Right
              ( NodeOwnerAccum
                  { noaOwners = Map.empty,
                    noaDelta = mempty
                  }
              )
          )
          normalizedEntries
      case ownerResult of
        Left rewriteError ->
          pure (Left rewriteError)
        Right completedOwnerAccum -> do
          compressedParents <- UnionFindTransaction.transactionCanonicalMapAndCompress unionFindEditor
          pure (Right (completedOwnerAccum, compressedParents))
  classSorts' <-
    compressClassSorts compressedParents classSorts
  groupedEntries <-
    traverse
      ( \(nodeValue, ownerClass) -> do
          rootClass <- requireUnionFindClass compressedUnionFind ownerClass
          pure (classIdKey rootClass, [nodeValue])
      )
      (Map.toAscList (noaOwners ownerAccum))

  let groupedNodes =
        IntMap.fromListWith (<>) groupedEntries
      emptyRootClasses =
        IntMap.fromSet
          (const [])
          ( IntSet.fromList
              [ classIdKey rootClass
                | rootClass <- IntMap.elems compressedParents
              ]
          )

  pure
    HostRebuildState
      { hrsUnionFind = compressedUnionFind,
        hrsNodesByClass = IntMap.unionWith (<>) groupedNodes emptyRootClasses,
        hrsClassSorts = classSorts',
        hrsDelta = noaDelta ownerAccum
      }
  where
    normalizeClassEntry (classKey, nodes) = do
      classRoot <-
        requireUnionFindClass unionFind (ClassId classKey)
      traverse
        ( \nodeValue -> do
            canonicalNode <-
              traverse (requireUnionFindClass unionFind) nodeValue
            pure (classRoot, canonicalNode)
        )
        nodes

data NodeOwnerAccum sig = NodeOwnerAccum
  { noaOwners :: !(Map (Node sig ClassId) ClassId),
    noaDelta :: !EGraphEditDelta
  }

observeNodeOwner ::
  Ord (Node sig ClassId) =>
  UnionFindEditor s ->
  Either RewriteApplicationError (NodeOwnerAccum sig) ->
  (ClassId, Node sig ClassId) ->
  ST s (Either RewriteApplicationError (NodeOwnerAccum sig))
observeNodeOwner _ (Left rewriteError) _ =
  pure (Left rewriteError)
observeNodeOwner unionFindEditor (Right ownerAccum) (ownerClass, nodeValue) =
  case Map.lookup nodeValue (noaOwners ownerAccum) of
    Nothing ->
      pure
        ( Right
            ownerAccum
              { noaOwners =
                  Map.insert nodeValue ownerClass (noaOwners ownerAccum)
              }
        )

    Just existingOwner -> do
      existingRootResult <-
        requireUnionFindClassEditor unionFindEditor existingOwner
      ownerRootResult <-
        requireUnionFindClassEditor unionFindEditor ownerClass
      case (existingRootResult, ownerRootResult) of
        (Left rewriteError, _) ->
          pure (Left rewriteError)
        (_, Left rewriteError) ->
          pure (Left rewriteError)
        (Right existingRoot, Right ownerRoot) -> do
          unionOutcome <-
            UnionFindTransaction.transactionUnion unionFindEditor existingRoot ownerRoot
          let unionDelta =
                case unionOutcome of
                  AlreadyEquivalent _ ->
                    mempty
                  MergedClasses _ _ ->
                    classUnionDelta existingRoot ownerRoot
          pure
            ( Right
                ownerAccum
                  { noaDelta = noaDelta ownerAccum <> unionDelta
                  }
            )

requireUnionFindClassEditor ::
  UnionFindEditor s ->
  ClassId ->
  ST s (Either RewriteApplicationError ClassId)
requireUnionFindClassEditor unionFindEditor classId = do
  canonicalClassResult <-
    UnionFindTransaction.transactionCanonicalClass unionFindEditor classId
  pure
    ( case canonicalClassResult of
        Nothing ->
          Left (RewriteMissingEClass classId)
        Just rootClass ->
          Right rootClass
    )

compressClassSorts ::
  IntMap ClassId ->
  IntMap SomeSortWitness ->
  Either RewriteApplicationError (IntMap SomeSortWitness)
compressClassSorts parents =
  IntMap.foldlWithKey' insertCanonicalSort (Right IntMap.empty)
  where
    insertCanonicalSort ::
      Either RewriteApplicationError (IntMap SomeSortWitness) ->
      Int ->
      SomeSortWitness ->
      Either RewriteApplicationError (IntMap SomeSortWitness)
    insertCanonicalSort accumulated classKey classSort =
      accumulated >>= \canonicalSorts -> do
        rootClass <-
          lookupCanonicalParent parents (ClassId classKey)
        insertClassSort rootClass (ClassId classKey) classSort canonicalSorts

insertClassSort ::
  ClassId ->
  ClassId ->
  SomeSortWitness ->
  IntMap SomeSortWitness ->
  Either RewriteApplicationError (IntMap SomeSortWitness)
insertClassSort rootClass sourceClass classSort classSorts =
  case IntMap.lookup (classIdKey rootClass) classSorts of
    Nothing ->
      Right (IntMap.insert (classIdKey rootClass) classSort classSorts)

    Just existingSort
      | existingSort == classSort ->
          Right classSorts

      | otherwise ->
          Left (RewriteClassSortMismatch rootClass sourceClass)

requireSameHostClassSort ::
  Host sig ->
  ClassId ->
  ClassId ->
  Either RewriteApplicationError ()
requireSameHostClassSort host leftClass rightClass = do
  leftSort <-
    requireHostClassSort host leftClass
  rightSort <-
    requireHostClassSort host rightClass
  if leftSort == rightSort
    then Right ()
    else Left (RewriteClassSortMismatch leftClass rightClass)

requireHostNodeChildSorts ::
  RewriteSignature sig =>
  Host sig ->
  Node sig ClassId ->
  Either RewriteApplicationError ()
requireHostNodeChildSorts host =
  validateNodeChildSorts requireChildClassSort
  where
    requireChildClassSort ::
      SortWitness sort ->
      ClassId ->
      Either RewriteApplicationError ()
    requireChildClassSort expectedSort childClass = do
      observedSort <-
        requireHostClassSort host childClass
      if SomeSortWitness expectedSort == observedSort
        then Right ()
        else
          Left
            ( RewriteNodeChildSortMismatch
                childClass
                (sortWitnessName expectedSort)
                (someSortWitnessName observedSort)
            )

requireHostClassSort ::
  Host sig ->
  ClassId ->
  Either RewriteApplicationError SomeSortWitness
requireHostClassSort host classId =
  case hostCanonicalClass host classId of
    Nothing ->
      Left (RewriteMissingEClass classId)

    Just rootClass ->
      maybe
        (Left (RewriteMissingEClass rootClass))
        Right
        (IntMap.lookup (classIdKey rootClass) (hClassSorts host))

requireHostClass ::
  Host sig ->
  ClassId ->
  Either RewriteApplicationError ClassId
requireHostClass host classId =
  maybe
    (Left (RewriteMissingEClass classId))
    Right
    (hostCanonicalClass host classId)

requireUnionFindClass ::
  UnionFind ->
  ClassId ->
  Either RewriteApplicationError ClassId
requireUnionFindClass unionFind classId =
  maybe
    (Left (RewriteMissingEClass classId))
    Right
    (UnionFind.canonicalClass classId unionFind)

lookupCanonicalParent ::
  IntMap ClassId ->
  ClassId ->
  Either RewriteApplicationError ClassId
lookupCanonicalParent parents classId =
  maybe
    (Left (RewriteMissingEClass classId))
    Right
    (IntMap.lookup (classIdKey classId) parents)

materializeHost ::
  Int ->
  UnionFind ->
  IntMap [Node sig ClassId] ->
  IntMap SomeSortWitness ->
  Map (Node sig ClassId) ClassId ->
  Map (NodeTag sig) (IntMap [RowTupleKey]) ->
  Host sig
materializeHost revision unionFind nodesByClass classSorts owners sections =
  Host
    { hRevision = revision,
      hUnionFind = unionFind,
      hNodesByClass = nodesByClass,
      hClassSorts = classSorts,
      hNodeOwners = owners,
      hBackend =
        replaceRewriteRelationalHost
          revision
          sections
    }

-- | The full-fold projection of an e-class store into constructor-tag
-- sections. This is the executable specification: every incremental section
-- update must agree with folding the whole store through it.
hostSectionsFromClasses ::
  forall sig.
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  IntMap [Node sig ClassId] ->
  Map (NodeTag sig) (IntMap [RowTupleKey])
hostSectionsFromClasses =
  IntMap.foldlWithKey' insertClassNodes Map.empty
  where
    insertClassNodes ::
      Map (NodeTag sig) (IntMap [RowTupleKey]) ->
      Int ->
      [Node sig ClassId] ->
      Map (NodeTag sig) (IntMap [RowTupleKey])
    insertClassNodes sections resultKey =
      foldl' (hostSectionsInsertNode resultKey) sections
{-# INLINE hostSectionsFromClasses #-}

hostSectionsInsertNode ::
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  Int ->
  Map (NodeTag sig) (IntMap [RowTupleKey]) ->
  Node sig ClassId ->
  Map (NodeTag sig) (IntMap [RowTupleKey])
hostSectionsInsertNode resultKey sections node@(Node sigNode) =
  Map.alter
    (Just . IntMap.insertWith (<>) resultKey [hostNodeRow resultKey node] . maybe IntMap.empty id)
    (nodeTag sigNode)
    sections
{-# INLINE hostSectionsInsertNode #-}

hostSections :: Host sig -> Map (NodeTag sig) (IntMap [RowTupleKey])
hostSections =
  rewriteRelationalHostSections . hBackend
{-# INLINE hostSections #-}

hostNodeRow :: RewriteSignature sig => Int -> Node sig ClassId -> RowTupleKey
hostNodeRow resultKey (Node sigNode) =
  tupleKeyFromRepKeys
    (RepKey resultKey : fmap (RepKey . classIdKey) (nodeChildren sigNode))
{-# INLINE hostNodeRow #-}

nodeOwnersFromClasses ::
  forall sig.
  Ord (Node sig ClassId) =>
  IntMap [Node sig ClassId] ->
  Map (Node sig ClassId) ClassId
nodeOwnersFromClasses =
  foldl'
    insertClassNodes
    Map.empty
    . IntMap.toAscList
  where
    insertClassNodes ::
      Map (Node sig ClassId) ClassId ->
      (Int, [Node sig ClassId]) ->
      Map (Node sig ClassId) ClassId
    insertClassNodes owners (classKey, nodes) =
      foldl'
        (\currentOwners nodeValue -> Map.insert nodeValue (ClassId classKey) currentOwners)
        owners
        nodes

validateHostNodeClass ::
  RewriteSignature sig =>
  IntSet ->
  Int ->
  [Node sig Int] ->
  Either HostBuildError [Node sig ClassId]
validateHostNodeClass knownKeys nodeKey nodes = do
  validateHostNodeKey nodeKey
  traverse (validateHostNode knownKeys nodeKey) nodes

classSortFromNodes ::
  RewriteSignature sig =>
  Int ->
  [Node sig ClassId] ->
  Either HostBuildError SomeSortWitness
classSortFromNodes classKey nodes =
  case nodes of
    [] ->
      Left (HostEmptyNodeClass classKey)

    firstNode : restNodes ->
      foldM
        (requireSameNodeClassSort classKey)
        (nodeSort firstNode)
        restNodes

requireSameNodeClassSort ::
  RewriteSignature sig =>
  Int ->
  SomeSortWitness ->
  Node sig ClassId ->
  Either HostBuildError SomeSortWitness
requireSameNodeClassSort classKey expectedSort nodeValue =
  let observedSort =
        nodeSort nodeValue
   in if expectedSort == observedSort
        then Right expectedSort
        else
          Left
            ( HostClassSortMismatch
                classKey
                (someSortWitnessSortName expectedSort)
                (someSortWitnessSortName observedSort)
            )

validateHostNodeChildSorts ::
  RewriteSignature sig =>
  IntMap SomeSortWitness ->
  Int ->
  Node sig ClassId ->
  Either HostBuildError ()
validateHostNodeChildSorts classSorts nodeKey =
  validateNodeChildSorts validateChildClassSort
  where
    validateChildClassSort ::
      SortWitness sort ->
      ClassId ->
      Either HostBuildError ()
    validateChildClassSort expectedSort childClass = do
      observedSort <-
        maybe
          (Left (HostUnknownChildKey nodeKey (classIdKey childClass)))
          Right
          (IntMap.lookup (classIdKey childClass) classSorts)
      if SomeSortWitness expectedSort == observedSort
        then Right ()
        else
          Left
            ( HostChildSortMismatch
                nodeKey
                (classIdKey childClass)
                (sortWitnessSortName expectedSort)
                (someSortWitnessSortName observedSort)
            )

validateNodeChildSorts ::
  forall sig error.
  RewriteSignature sig =>
  (forall sort. SortWitness sort -> ClassId -> Either error ()) ->
  Node sig ClassId ->
  Either error ()
validateNodeChildSorts validateChildSort (Node sigNode) =
  () <$ htraverseWithSort validateChild sigNode
  where
    validateChild ::
      SortWitness sort ->
      K ClassId sort ->
      Either error (K ClassId sort)
    validateChild expectedSort (K childClass) =
      K childClass <$ validateChildSort expectedSort childClass

hostClassSortMismatchFromClasses ::
  IntMap SomeSortWitness ->
  ClassId ->
  ClassId ->
  HostBuildError
hostClassSortMismatchFromClasses classSorts leftClass rightClass =
  HostClassSortMismatch
    (classIdKey leftClass)
    (renderClassSort leftClass)
    (renderClassSort rightClass)
  where
    renderClassSort classId =
      maybe
        (sortWitnessSortName (SortWitness @"<unknown>"))
        someSortWitnessSortName
        (IntMap.lookup (classIdKey classId) classSorts)

hostBuildErrorFromRebuild ::
  IntMap SomeSortWitness ->
  RewriteApplicationError ->
  HostBuildError
hostBuildErrorFromRebuild classSorts =
  \case
    RewriteClassIdAllocationFailed allocationError ->
      HostClassIdAllocationFailed allocationError

    RewriteClassSortMismatch leftClass rightClass ->
      hostClassSortMismatchFromClasses classSorts leftClass rightClass

    rebuildError@RewriteMissingEClass {} ->
      HostRebuildApplicationError rebuildError

    rebuildError@RewriteNodeChildSortMismatch {} ->
      HostRebuildApplicationError rebuildError

    rebuildError@RewriteMissingBinding {} ->
      HostRebuildApplicationError rebuildError

    rebuildError@RewriteMissingInstantiatedNode ->
      HostRebuildApplicationError rebuildError

    rebuildError@RewriteDuplicateInstantiationRef {} ->
      HostRebuildApplicationError rebuildError

    rebuildError@RewriteInstantiationInputUnavailable {} ->
      HostRebuildApplicationError rebuildError

    rebuildError@RewriteMissingBinderSubstAlgebra {} ->
      HostRebuildApplicationError rebuildError

    rebuildError@RewriteConditionRejected {} ->
      HostRebuildApplicationError rebuildError

    rebuildError@RewriteUnloweredBinderScope ->
      HostRebuildApplicationError rebuildError

    rebuildError@RewriteProgramReadAfterMerge ->
      HostRebuildApplicationError rebuildError

validateHostNode ::
  RewriteSignature sig =>
  IntSet ->
  Int ->
  Node sig Int ->
  Either HostBuildError (Node sig ClassId)
validateHostNode knownKeys nodeKey nodeValue =
  traverse (validateHostChildKey knownKeys nodeKey) nodeValue

validateHostNodeKey :: Int -> Either HostBuildError ()
validateHostNodeKey nodeKey
  | nodeKey < 0 =
      Left (HostNegativeNodeKey nodeKey)
  | otherwise =
      Right ()

validateHostChildKey :: IntSet -> Int -> Int -> Either HostBuildError ClassId
validateHostChildKey knownKeys nodeKey childKey
  | childKey < 0 =
      Left (HostNegativeChildKey nodeKey childKey)
  | not (IntSet.member childKey knownKeys) =
      Left (HostUnknownChildKey nodeKey childKey)
  | otherwise =
        Right (ClassId childKey)
