{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Site.Context.Pairs
  ( ContextPairStrategy (..),
    contextDepth,
    latticeContextsBelow,
    overlappingContextPairs,
    downwardContextPairsFromGenerators,
    downwardPairsByStrategy,
    reflexiveDownwardPairsByStrategy,
  )
where

import Data.Kind (Type)
import Data.Set qualified as Set
import Moonlight.Algebra (JoinSemilattice (..), MeetSemilattice (..))
import Moonlight.Sheaf.Site.System (AnalyzableSystem (..), LatticeAnalyzableSystem, SystemCtx)

type ContextPairStrategy :: Type -> Type
data ContextPairStrategy context
  = ExhaustivePairs
  | GeneratorSeededPairs [context]
  deriving stock (Eq, Ord, Show)

contextDepth :: AnalyzableSystem system => system -> SystemCtx system -> Int
contextDepth systemValue =
  length . systemObjectsInContext systemValue

latticeContextsBelow :: LatticeAnalyzableSystem system => system -> SystemCtx system -> [SystemCtx system]
latticeContextsBelow systemValue contextValue =
  filter
    (\candidateContext -> contextLeq systemValue candidateContext contextValue)
    (allContexts systemValue)

overlappingContextPairs ::
  LatticeAnalyzableSystem system =>
  system ->
  [SystemCtx system] ->
  [(SystemCtx system, SystemCtx system)]
overlappingContextPairs systemValue contextValues =
  [ (leftContext, rightContext)
  | leftContext <- contextValues,
    rightContext <- contextValues,
    not
      ( null
          ( systemObjectsInContext
              systemValue
              (meet leftContext rightContext)
          )
      )
  ]

downwardPairsByStrategy ::
  LatticeAnalyzableSystem system =>
  ContextPairStrategy (SystemCtx system) ->
  system ->
  [SystemCtx system] ->
  [(SystemCtx system, SystemCtx system)]
downwardPairsByStrategy pairStrategy systemValue contextValues =
  case pairStrategy of
    ExhaustivePairs ->
      downwardContextPairsExhaustive
        (contextLeq systemValue)
        contextValues
    GeneratorSeededPairs generatorContexts ->
      downwardContextPairsFromGenerators
        (contextLeq systemValue)
        generatorContexts
        contextValues

reflexiveDownwardPairsByStrategy ::
  LatticeAnalyzableSystem system =>
  ContextPairStrategy (SystemCtx system) ->
  system ->
  [SystemCtx system] ->
  [(SystemCtx system, SystemCtx system)]
reflexiveDownwardPairsByStrategy pairStrategy systemValue contextValues =
  case pairStrategy of
    ExhaustivePairs ->
      reflexiveDownwardContextPairsExhaustive
        (contextLeq systemValue)
        contextValues
    GeneratorSeededPairs generatorContexts ->
      reflexiveDownwardContextPairsFromGenerators
        (contextLeq systemValue)
        generatorContexts
        contextValues

downwardContextPairsFromGenerators ::
  (Ord context, JoinSemilattice context, MeetSemilattice context) =>
  (context -> context -> Bool) ->
  [context] ->
  [context] ->
  [(context, context)]
downwardContextPairsFromGenerators isSubcontext generatorContexts contextValues =
  generatorSeededPairs isSubcontext admissibleContexts initialFrontier
  where
    admissibleContexts =
      Set.fromList contextValues

    initialFrontier =
      Set.fromList
        [ generatorContext
        | generatorContext <- generatorContexts,
          Set.member generatorContext admissibleContexts
        ]

reflexiveDownwardContextPairsFromGenerators ::
  (Ord context, JoinSemilattice context, MeetSemilattice context) =>
  (context -> context -> Bool) ->
  [context] ->
  [context] ->
  [(context, context)]
reflexiveDownwardContextPairsFromGenerators isSubcontext generatorContexts contextValues =
  fmap (\contextValue -> (contextValue, contextValue)) contextValues
    <> downwardContextPairsFromGenerators isSubcontext generatorContexts contextValues

generatorSeededPairs ::
  (Ord context, JoinSemilattice context, MeetSemilattice context) =>
  (context -> context -> Bool) ->
  Set.Set context ->
  Set.Set context ->
  [(context, context)]
generatorSeededPairs isSubcontext admissibleContexts = go Set.empty
  where
    go discoveredContexts frontierContexts
      | Set.null frontierContexts = []
      | otherwise =
          let frontierValues = Set.toAscList frontierContexts
              discoveredWithFrontier = Set.union discoveredContexts frontierContexts
              discoveredValues = Set.toAscList discoveredWithFrontier
              nextFrontier =
                Set.fromList
                  [ candidateContext
                  | frontierContext <- frontierValues,
                    discoveredContext <- discoveredValues,
                    candidateContext <- [join frontierContext discoveredContext, meet frontierContext discoveredContext],
                    Set.member candidateContext admissibleContexts,
                    Set.notMember candidateContext discoveredWithFrontier
                  ]
           in foldMap pairsBelowContext frontierValues
                <> go discoveredWithFrontier nextFrontier

    pairsBelowContext sourceContext =
      [ (sourceContext, targetContext)
      | targetContext <- Set.toAscList admissibleContexts,
        sourceContext /= targetContext,
        isSubcontext targetContext sourceContext
      ]

downwardContextPairsExhaustive ::
  Eq context =>
  (context -> context -> Bool) ->
  [context] ->
  [(context, context)]
downwardContextPairsExhaustive isSubcontext contextValues =
  [ (sourceContext, targetContext)
  | sourceContext <- contextValues,
    targetContext <- contextValues,
    sourceContext /= targetContext,
    isSubcontext targetContext sourceContext
  ]

reflexiveDownwardContextPairsExhaustive ::
  (context -> context -> Bool) ->
  [context] ->
  [(context, context)]
reflexiveDownwardContextPairsExhaustive isSubcontext contextValues =
  [ (sourceContext, targetContext)
  | sourceContext <- contextValues,
    targetContext <- contextValues,
    isSubcontext targetContext sourceContext
  ]
