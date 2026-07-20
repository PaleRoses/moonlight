{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Moonlight.Flow.Runtime.Spec.Schema
  ( RuntimeAtom (..),
    RuntimeAtomSchema (..),
    RuntimeContextSchema (..),
    RuntimeSchema (..),
    RuntimePlan (..),
    RuntimePlanProjection (..),
    PlanStrategy (..),
    RuntimePlanOptions (..),
    RuntimePlanError (..),
    RuntimeInitialData (..),
    RuntimeSpec (..),
    RuntimeSpecError (..),
    ContextOrderDecl (..),
    ContextLatticeCompileError (..),
    contextOrderDecl,
    runtimeAtom,
    runtimeAtomWithSchema,
    runtimeAtomSchema,
    runtimeAtomSchemaWithTouches,
    runtimeAtomRef,
    runtimeMatch,
    runtimeContextSchema,
    runtimeSchema,
    runtimeSchemaWithContextOrder,
    withContextOrder,
    defaultRuntimePlanOptions,
    runtimePlan,
    runtimePlanWith,
    runtimePlanWithDecomp,
    runtimePlanQuery,
    runtimePlanWithDecompQuery,
    runtimePlanProjectionFromQueryPlan,
    runtimePlanProjectionFromSlots,
    runtimeSpec,
    runtimeInitialData,
    emptyRuntimeInitialData,
    withInitialData,
    runtimePlanQueryId,
    runtimePlanAtomKeys,
    runtimePlanAtomSourcePairs,
    runtimePlanSourceAtomKeys,
    runtimePlanOccurrenceAtomSchemas,
    runtimePlanFactorNodes,
    validateRuntimePlanProgramSpec,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Moonlight.Core
  ( AtomId,
    QueryId,
    SlotId,
  )
import Moonlight.Differential.Proposition
  ( PropositionKey,
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )
import Moonlight.Flow.Query
  ( Query,
    AtomRef,
    Match,
  )
import Moonlight.Flow.Query qualified as Query
import Moonlight.Flow.Plan.Physical.Meta
  ( decompFromJoinForest,
  )
import Moonlight.Flow.Plan.Query.Core
  ( BagId (..),
    DecompPlan,
    FactorNode,
    QueryPlan,
    QueryAtomId,
    SourceAtomId,
    foldJoinShape,
    jmAtomSchemas,
    jmShape,
    mkQueryAtomId,
    mkDecompBag,
    mkDecompPlan,
    qpFullSchema,
    qpJoinMeta,
    qpOutputSlots,
    sourceAtomKey,
  )
import Moonlight.Flow.Runtime.Spec.FactorProgram
  ( FactorProgramSpec,
    FactorProgramError,
    factorProgramSpecAtomKeys,
    factorProgramSpecAtomSchemas,
    factorProgramSpecAtomSourceMap,
    factorProgramSpecFactorNodes,
    factorProgramSpecQueryId,
    compileFactorProgramSpec,
    validateFactorProgramSpec,
  )
import Moonlight.Flow.Runtime.Core.Patch.Internal
  ( Patch,
    emptyPatch,
  )
import Moonlight.FiniteLattice
  ( ContextLatticeCompileError (..),
    ContextOrderDecl (..),
    contextOrderDecl
  )

data RuntimeAtomSchema = RuntimeAtomSchema
  { rasColumns :: ![SlotId],
    rasBoundarySensitiveSlots :: !IntSet,
    rasBoundarySlotKeys :: !(IntMap IntSet),
    rasTouchDeps :: !IntSet,
    rasTouchTopo :: !IntSet
  }
  deriving stock (Eq, Ord, Show, Read)

data RuntimeAtom ctx prop = RuntimeAtom
  { runtimeAtomId :: !AtomId,
    runtimeAtomSchemaDefinition :: !RuntimeAtomSchema
  }
  deriving stock (Eq, Ord, Show, Read)

data RuntimeContextSchema prop = RuntimeContextSchema
  { rcsAtoms :: !(Map AtomId RuntimeAtomSchema),
    rcsPropositions :: !(Set (PropositionKey prop))
  }
  deriving stock (Eq, Ord, Show, Read)

data RuntimeSchema ctx prop = RuntimeSchema
  { rscContexts :: !(Map ctx (RuntimeContextSchema prop)),
    rscContextOrder :: !(Maybe (ContextOrderDecl ctx))
  }
  deriving stock (Eq, Ord, Show, Read)

data RuntimePlan ctx prop = RuntimePlan
  { rpContext :: !ctx,
    rpProp :: !(PropositionKey prop),
    rpProjection :: !RuntimePlanProjection,
    rpProgram :: !FactorProgramSpec
  }

data RuntimePlanProjection = RuntimePlanProjection
  { rppFullSchema :: ![SlotId],
    rppOutputSlots :: ![SlotId]
  }
  deriving stock (Eq, Ord, Show, Read)

data PlanStrategy
  = AutoPlan
  | ExplicitDecompPlan !DecompPlan
  deriving stock (Eq, Ord, Show)

data RuntimePlanOptions = RuntimePlanOptions
  { rpoStrategy :: !PlanStrategy
  }
  deriving stock (Eq, Ord, Show)

data RuntimePlanError
  = RuntimePlanFactorProgramError !FactorProgramError
  deriving stock (Eq, Show)

data RuntimeInitialData ctx prop = RuntimeInitialData
  { unRuntimeInitialData :: !Patch
  }
  deriving stock (Eq, Show)

data RuntimeSpec ctx prop = RuntimeSpec
  { rsSchema :: !(RuntimeSchema ctx prop),
    rsPlans :: ![RuntimePlan ctx prop],
    rsInitialData :: !(RuntimeInitialData ctx prop)
  }

data RuntimeSpecError ctx prop
  = RuntimeSpecEmptyContexts
  | RuntimeSpecEmptyPropositions
  | RuntimeSpecAtomSchemaConflict !AtomId !RuntimeAtomSchema !RuntimeAtomSchema
  | RuntimeSpecPlanContextMissing !ctx
  | RuntimeSpecPlanPropositionMissing !ctx !(PropositionKey prop)
  | RuntimeSpecDuplicateQuery !QueryId
  | RuntimeSpecPlanAtomUndeclared !ctx !QueryId !AtomId
  | RuntimeSpecInitialAtomUndeclared !AtomId
  | RuntimeSpecInitialRowWidthMismatch !AtomId !Int !RowTupleKey
  deriving stock (Eq, Ord, Show)

runtimeAtom :: AtomId -> [SlotId] -> RuntimeAtom ctx prop
runtimeAtom atomId columns =
  RuntimeAtom
    { runtimeAtomId = atomId,
      runtimeAtomSchemaDefinition = runtimeAtomSchema columns
    }
{-# INLINE runtimeAtom #-}

runtimeAtomWithSchema ::
  AtomId ->
  RuntimeAtomSchema ->
  RuntimeAtom ctx prop
runtimeAtomWithSchema atomId schema =
  RuntimeAtom
    { runtimeAtomId = atomId,
      runtimeAtomSchemaDefinition = schema
    }
{-# INLINE runtimeAtomWithSchema #-}

runtimeAtomRef ::
  RuntimeAtom ctx prop ->
  AtomRef
runtimeAtomRef atomValue =
  Query.atomRef
    (runtimeAtomId atomValue)
    (rasColumns (runtimeAtomSchemaDefinition atomValue))
{-# INLINE runtimeAtomRef #-}

runtimeMatch ::
  RuntimeAtom ctx prop ->
  Match
runtimeMatch =
  Query.match . runtimeAtomRef
{-# INLINE runtimeMatch #-}

runtimeAtomSchema :: [SlotId] -> RuntimeAtomSchema
runtimeAtomSchema columns =
  RuntimeAtomSchema
    { rasColumns = columns,
      rasBoundarySensitiveSlots = IntSet.empty,
      rasBoundarySlotKeys = IntMap.empty,
      rasTouchDeps = IntSet.empty,
      rasTouchTopo = IntSet.empty
    }
{-# INLINE runtimeAtomSchema #-}

runtimeAtomSchemaWithTouches ::
  [SlotId] ->
  IntSet ->
  IntSet ->
  RuntimeAtomSchema
runtimeAtomSchemaWithTouches columns depTouches topoTouches =
  (runtimeAtomSchema columns)
    { rasTouchDeps = depTouches,
      rasTouchTopo = topoTouches
    }
{-# INLINE runtimeAtomSchemaWithTouches #-}

runtimeContextSchema ::
  Ord prop =>
  [RuntimeAtom ctx prop] ->
  [PropositionKey prop] ->
  RuntimeContextSchema prop
runtimeContextSchema atoms propositions =
  RuntimeContextSchema
    { rcsAtoms =
        Map.fromList
          [ (runtimeAtomId atom, runtimeAtomSchemaDefinition atom)
          | atom <- atoms
          ],
      rcsPropositions =
        Set.fromList propositions
    }
{-# INLINE runtimeContextSchema #-}

runtimeSchema ::
  Ord ctx =>
  [(ctx, RuntimeContextSchema prop)] ->
  RuntimeSchema ctx prop
runtimeSchema contexts =
  RuntimeSchema
    { rscContexts = Map.fromList contexts,
      rscContextOrder = Nothing
    }
{-# INLINE runtimeSchema #-}

runtimeSchemaWithContextOrder ::
  Ord ctx =>
  ContextOrderDecl ctx ->
  [(ctx, RuntimeContextSchema prop)] ->
  RuntimeSchema ctx prop
runtimeSchemaWithContextOrder decl contexts =
  withContextOrder decl (runtimeSchema contexts)
{-# INLINE runtimeSchemaWithContextOrder #-}

withContextOrder ::
  ContextOrderDecl ctx ->
  RuntimeSchema ctx prop ->
  RuntimeSchema ctx prop
withContextOrder decl schemaValue =
  schemaValue
    { rscContextOrder = Just decl
    }
{-# INLINE withContextOrder #-}

defaultRuntimePlanOptions :: RuntimePlanOptions
defaultRuntimePlanOptions =
  RuntimePlanOptions
    { rpoStrategy = AutoPlan
    }
{-# INLINE defaultRuntimePlanOptions #-}

runtimePlan ::
  ctx ->
  PropositionKey prop ->
  QueryPlan compiled output guard tag tuple key ->
  Either RuntimePlanError (RuntimePlan ctx prop)
runtimePlan =
  runtimePlanWith defaultRuntimePlanOptions
{-# INLINE runtimePlan #-}

runtimePlanQuery ::
  ctx ->
  PropositionKey prop ->
  Query ->
  Either RuntimePlanError (RuntimePlan ctx prop)
runtimePlanQuery contextValue propKey queryValue =
  runtimePlan contextValue propKey (Query.queryPlan queryValue)
{-# INLINE runtimePlanQuery #-}

runtimePlanWith ::
  RuntimePlanOptions ->
  ctx ->
  PropositionKey prop ->
  QueryPlan compiled output guard tag tuple key ->
  Either RuntimePlanError (RuntimePlan ctx prop)
runtimePlanWith options contextValue propKey queryPlan =
  let decompPlan =
        runtimePlanDecomp (rpoStrategy options) queryPlan
   in firstRuntimePlanFactorError
        (runtimePlanWithDecomp contextValue propKey queryPlan decompPlan)
{-# INLINE runtimePlanWith #-}

runtimePlanWithDecomp ::
  ctx ->
  PropositionKey prop ->
  QueryPlan compiled output guard tag tuple key ->
  DecompPlan ->
  Either FactorProgramError (RuntimePlan ctx prop)
runtimePlanWithDecomp contextValue propKey queryPlan decompPlan = do
  program <- compileFactorProgramSpec queryPlan decompPlan
  pure
    RuntimePlan
      { rpContext = contextValue,
        rpProp = propKey,
        rpProjection = runtimePlanProjectionFromQueryPlan queryPlan,
        rpProgram = program
      }
{-# INLINE runtimePlanWithDecomp #-}

runtimePlanWithDecompQuery ::
  ctx ->
  PropositionKey prop ->
  Query ->
  DecompPlan ->
  Either FactorProgramError (RuntimePlan ctx prop)
runtimePlanWithDecompQuery contextValue propKey queryValue =
  runtimePlanWithDecomp contextValue propKey (Query.queryPlan queryValue)
{-# INLINE runtimePlanWithDecompQuery #-}

runtimePlanProjectionFromQueryPlan ::
  QueryPlan compiled output guard tag tuple key ->
  RuntimePlanProjection
runtimePlanProjectionFromQueryPlan queryPlan =
  runtimePlanProjectionFromSlots
    (Vector.toList (qpFullSchema queryPlan))
    (Vector.toList (qpOutputSlots queryPlan))
{-# INLINE runtimePlanProjectionFromQueryPlan #-}

runtimePlanProjectionFromSlots ::
  [SlotId] ->
  [SlotId] ->
  RuntimePlanProjection
runtimePlanProjectionFromSlots fullSchema outputSlots =
  RuntimePlanProjection
    { rppFullSchema = fullSchema,
      rppOutputSlots = outputSlots
    }
{-# INLINE runtimePlanProjectionFromSlots #-}

runtimePlanDecomp ::
  PlanStrategy ->
  QueryPlan compiled output guard tag tuple key ->
  DecompPlan
runtimePlanDecomp strategy queryPlan =
  case strategy of
    AutoPlan ->
      autoRuntimePlanDecomp queryPlan
    ExplicitDecompPlan decomp ->
      decomp
{-# INLINE runtimePlanDecomp #-}

autoRuntimePlanDecomp ::
  QueryPlan compiled output guard tag tuple key ->
  DecompPlan
autoRuntimePlanDecomp queryPlan =
  foldJoinShape
    (denseRuntimePlanDecomp queryPlan)
    (\forest -> decompFromJoinForest forest (runtimePlanAtomColumns queryPlan))
    id
    (jmShape (qpJoinMeta queryPlan))
{-# INLINE autoRuntimePlanDecomp #-}

denseRuntimePlanDecomp ::
  QueryPlan compiled output guard tag tuple key ->
  DecompPlan
denseRuntimePlanDecomp queryPlan =
  mkDecompPlan
    rootBag
    (IntMap.singleton rootBagKey rootDecompBag)
    IntMap.empty
    IntMap.empty
    Map.empty
    (fmap (const rootBag) atomSchemas)
  where
    rootBagKey =
      0

    rootBag =
      BagId rootBagKey

    atomSchemas =
      queryPlanRuntimeAtomSchemas queryPlan

    rootDecompBag =
      mkDecompBag
        rootBag
        (Vector.toList (qpFullSchema queryPlan))
        (IntMap.keysSet atomSchemas)
{-# INLINE denseRuntimePlanDecomp #-}

queryPlanRuntimeAtomSchemas ::
  QueryPlan compiled output guard tag tuple key ->
  IntMap RuntimeAtomSchema
queryPlanRuntimeAtomSchemas queryPlan =
  fmap runtimeAtomSchema (jmAtomSchemas (qpJoinMeta queryPlan))
{-# INLINE queryPlanRuntimeAtomSchemas #-}

runtimePlanAtomColumns ::
  QueryPlan compiled output guard tag tuple key ->
  IntMap [SlotId]
runtimePlanAtomColumns =
  jmAtomSchemas . qpJoinMeta
{-# INLINE runtimePlanAtomColumns #-}

firstRuntimePlanFactorError ::
  Either FactorProgramError value ->
  Either RuntimePlanError value
firstRuntimePlanFactorError =
  either (Left . RuntimePlanFactorProgramError) Right
{-# INLINE firstRuntimePlanFactorError #-}

runtimeSpec ::
  RuntimeSchema ctx prop ->
  [RuntimePlan ctx prop] ->
  RuntimeSpec ctx prop
runtimeSpec schema plans =
  RuntimeSpec
    { rsSchema = schema,
      rsPlans = plans,
      rsInitialData = emptyRuntimeInitialData
    }
{-# INLINE runtimeSpec #-}

runtimeInitialData :: Patch -> RuntimeInitialData ctx prop
runtimeInitialData =
  RuntimeInitialData
{-# INLINE runtimeInitialData #-}

emptyRuntimeInitialData :: RuntimeInitialData ctx prop
emptyRuntimeInitialData =
  RuntimeInitialData emptyPatch
{-# INLINE emptyRuntimeInitialData #-}

withInitialData ::
  RuntimeInitialData ctx prop ->
  RuntimeSpec ctx prop ->
  RuntimeSpec ctx prop
withInitialData initialData spec =
  spec
    { rsInitialData = initialData
    }
{-# INLINE withInitialData #-}

runtimePlanQueryId :: RuntimePlan ctx prop -> QueryId
runtimePlanQueryId =
  factorProgramSpecQueryId . rpProgram
{-# INLINE runtimePlanQueryId #-}

runtimePlanAtomKeys ::
  RuntimePlan ctx prop ->
  IntSet
runtimePlanAtomKeys =
  factorProgramSpecAtomKeys . rpProgram
{-# INLINE runtimePlanAtomKeys #-}

runtimePlanAtomSourcePairs ::
  RuntimePlan ctx prop ->
  [(QueryAtomId, SourceAtomId)]
runtimePlanAtomSourcePairs plan =
  [ (mkQueryAtomId occurrenceKey, sourceAtomIdValue)
  | (occurrenceKey, sourceAtomIdValue) <-
      IntMap.toAscList (factorProgramSpecAtomSourceMap (rpProgram plan))
  ]
{-# INLINE runtimePlanAtomSourcePairs #-}

runtimePlanSourceAtomKeys ::
  RuntimePlan ctx prop ->
  IntSet
runtimePlanSourceAtomKeys =
  IntSet.fromList
    . fmap (sourceAtomKey . snd)
    . runtimePlanAtomSourcePairs
{-# INLINE runtimePlanSourceAtomKeys #-}

runtimePlanOccurrenceAtomSchemas ::
  RuntimePlan ctx prop ->
  IntMap RuntimeAtomSchema
runtimePlanOccurrenceAtomSchemas =
  fmap runtimeAtomSchema . factorProgramSpecAtomSchemas . rpProgram
{-# INLINE runtimePlanOccurrenceAtomSchemas #-}

runtimePlanFactorNodes ::
  RuntimePlan ctx prop ->
  [FactorNode]
runtimePlanFactorNodes =
  factorProgramSpecFactorNodes . rpProgram
{-# INLINE runtimePlanFactorNodes #-}

validateRuntimePlanProgramSpec ::
  RuntimePlan ctx prop ->
  Either FactorProgramError ()
validateRuntimePlanProgramSpec =
  validateFactorProgramSpec . rpProgram
{-# INLINE validateRuntimePlanProgramSpec #-}
