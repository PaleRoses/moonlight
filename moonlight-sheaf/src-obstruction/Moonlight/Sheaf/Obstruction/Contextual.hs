{-# LANGUAGE FunctionalDependencies #-}

module Moonlight.Sheaf.Obstruction.Contextual
  ( StructuralMismatchData (..),
    ConditionFailureData (..),
    ContextBarrierData (..),
    RestrictionBarrierData (..),
    RestrictionLookupFailureData (..),
    PropagationBarrierData (..),
    EquivalenceLookupFailureData (..),
    Obstruction (..),
    ContextualObstructionStore (..),
    obstructionReport,
    whyNotMerged,
    obstructionReportWith,
    whyNotMergedWith,
  )
where

import Data.Kind (Constraint, Type)
import Data.Maybe (maybeToList)

type StructuralMismatchData :: Type -> Type -> Type -> Type
data StructuralMismatchData eq node ctx = StructuralMismatchData
  { smdLeft :: !eq,
    smdRight :: !eq,
    smdContext :: !ctx,
    smdMismatchedNodes :: ![(node, node)]
  }
  deriving stock (Eq, Ord, Show)

type ConditionFailureData :: Type -> Type -> Type -> Type
data ConditionFailureData rule ctx subst = ConditionFailureData
  { cfdRule :: !rule,
    cfdContext :: !ctx,
    cfdSubstitution :: !subst
  }
  deriving stock (Eq, Ord, Show)

type ContextBarrierData :: Type -> Type -> Type
data ContextBarrierData eq ctx = ContextBarrierData
  { cbdLeft :: !eq,
    cbdRight :: !eq,
    cbdValidContexts :: ![ctx],
    cbdInvalidContexts :: ![ctx]
  }
  deriving stock (Eq, Ord, Show)

type RestrictionBarrierData :: Type -> Type -> Type -> Type
data RestrictionBarrierData eq ctx stat = RestrictionBarrierData
  { rbdLeft :: !eq,
    rbdRight :: !eq,
    rbdContext :: !ctx,
    rbdStats :: ![stat]
  }
  deriving stock (Eq, Ord, Show)

type RestrictionLookupFailureData :: Type -> Type -> Type -> Type
data RestrictionLookupFailureData eq ctx failure = RestrictionLookupFailureData
  { rlfdLeft :: !eq,
    rlfdRight :: !eq,
    rlfdContext :: !ctx,
    rlfdFailure :: !failure
  }
  deriving stock (Eq, Ord, Show)

type PropagationBarrierData :: Type -> Type -> Type -> Type
data PropagationBarrierData eq ctx failure = PropagationBarrierData
  { pbdLeft :: !eq,
    pbdRight :: !eq,
    pbdContext :: !ctx,
    pbdFailure :: !failure
  }
  deriving stock (Eq, Ord, Show)

type EquivalenceLookupFailureData :: Type -> Type -> Type -> Type
data EquivalenceLookupFailureData eq ctx failure = EquivalenceLookupFailureData
  { elfdLeft :: !eq,
    elfdRight :: !eq,
    elfdContext :: !ctx,
    elfdFailure :: !failure
  }
  deriving stock (Eq, Ord, Show)

type Obstruction :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data Obstruction eq node rule ctx subst stat failure
  = StructuralMismatch !(StructuralMismatchData eq node ctx)
  | ConditionFailure !(ConditionFailureData rule ctx subst)
  | EquivalenceLookupFailure !(EquivalenceLookupFailureData eq ctx failure)
  | ContextBarrier !(ContextBarrierData eq ctx)
  | RestrictionBarrier !(RestrictionBarrierData eq ctx stat)
  | RestrictionLookupFailure !(RestrictionLookupFailureData eq ctx failure)
  | PropagationBarrier !(PropagationBarrierData eq ctx failure)
  deriving stock (Eq, Ord, Show)

type ContextualObstructionStore :: Type -> Type -> Type -> Type -> Type -> Type -> Constraint
class ContextualObstructionStore store ctx eq node stat failure | store -> ctx eq node stat failure where
  obstructionContexts :: store -> [ctx]
  obstructionEquivalentAt :: ctx -> eq -> eq -> store -> Either failure Bool
  obstructionStructuralPairsAt :: eq -> eq -> ctx -> store -> [(node, node)]
  obstructionRestrictionStatsAt :: ctx -> store -> Either failure [stat]
  obstructionPropagationFailure :: store -> Maybe failure

obstructionReport ::
  ContextualObstructionStore store ctx eq node stat failure =>
  eq ->
  eq ->
  ctx ->
  store ->
  [Obstruction eq node rule ctx subst stat failure]
obstructionReport leftValue rightValue contextValue store =
  obstructionReportWith
    (obstructionContexts store)
    obstructionEquivalentAt
    obstructionStructuralPairsAt
    obstructionRestrictionStatsAt
    obstructionPropagationFailure
    leftValue
    rightValue
    contextValue
    store

whyNotMerged ::
  ContextualObstructionStore store ctx eq node stat failure =>
  eq ->
  eq ->
  store ->
  [Obstruction eq node rule ctx subst stat failure]
whyNotMerged leftValue rightValue store =
  whyNotMergedWith
    (obstructionContexts store)
    obstructionEquivalentAt
    obstructionStructuralPairsAt
    obstructionRestrictionStatsAt
    obstructionPropagationFailure
    leftValue
    rightValue
    store

obstructionReportWith ::
  [ctx] ->
  (ctx -> eq -> eq -> store -> Either failure Bool) ->
  (eq -> eq -> ctx -> store -> [(node, node)]) ->
  (ctx -> store -> Either failure [stat]) ->
  (store -> Maybe failure) ->
  eq ->
  eq ->
  ctx ->
  store ->
  [Obstruction eq node rule ctx subst stat failure]
obstructionReportWith allContexts equivalentAt structuralPairs restrictionStats propagationFailure leftValue rightValue contextValue store =
  case equivalentAt contextValue leftValue rightValue store of
    Left failureValue ->
      [equivalenceLookupFailure contextValue failureValue]
        <> propagationBarrier
    Right True ->
      []
    Right False ->
      structuralBarrier
        <> contextBarrier
        <> equivalenceLookupFailures
        <> restrictionLookupFailure
        <> restrictionBarrier
        <> propagationBarrier
  where
    equivalenceLookupFailure failedContext failureValue =
      EquivalenceLookupFailure
        EquivalenceLookupFailureData
          { elfdLeft = leftValue,
            elfdRight = rightValue,
            elfdContext = failedContext,
            elfdFailure = failureValue
          }

    structuralBarrier =
      [ StructuralMismatch
          StructuralMismatchData
            { smdLeft = leftValue,
              smdRight = rightValue,
              smdContext = contextValue,
              smdMismatchedNodes = structuralPairs leftValue rightValue contextValue store
            }
      ]

    contextEquivalenceResults =
      fmap
        (\candidate -> (candidate, equivalentAt candidate leftValue rightValue store))
        allContexts

    equivalenceLookupFailures =
      [ equivalenceLookupFailure candidate failureValue
      | (candidate, Left failureValue) <- contextEquivalenceResults
      ]

    contextBarrier =
      let validContexts =
            [candidate | (candidate, Right True) <- contextEquivalenceResults]
          invalidContexts =
            [candidate | (candidate, Right False) <- contextEquivalenceResults]
       in [ ContextBarrier
              ContextBarrierData
                { cbdLeft = leftValue,
                  cbdRight = rightValue,
                  cbdValidContexts = validContexts,
                  cbdInvalidContexts = invalidContexts
                }
          | not (null validContexts || null invalidContexts)
          ]

    restrictionLookupFailure =
      case restrictionStatsResult of
        Left failureValue ->
          [ RestrictionLookupFailure
              RestrictionLookupFailureData
                { rlfdLeft = leftValue,
                  rlfdRight = rightValue,
                  rlfdContext = contextValue,
                  rlfdFailure = failureValue
                }
          ]
        Right _stats ->
          []

    restrictionBarrier =
      case restrictionStatsResult of
        Left _failureValue ->
          []
        Right stats ->
          [ RestrictionBarrier
              RestrictionBarrierData
                { rbdLeft = leftValue,
                  rbdRight = rightValue,
                  rbdContext = contextValue,
                  rbdStats = stats
                }
          | not (null stats)
          ]

    restrictionStatsResult =
      restrictionStats contextValue store

    propagationBarrier =
      [ PropagationBarrier
          PropagationBarrierData
            { pbdLeft = leftValue,
              pbdRight = rightValue,
              pbdContext = contextValue,
              pbdFailure = failureValue
            }
      | failureValue <- maybeToList (propagationFailure store)
      ]

whyNotMergedWith ::
  [ctx] ->
  (ctx -> eq -> eq -> store -> Either failure Bool) ->
  (eq -> eq -> ctx -> store -> [(node, node)]) ->
  (ctx -> store -> Either failure [stat]) ->
  (store -> Maybe failure) ->
  eq ->
  eq ->
  store ->
  [Obstruction eq node rule ctx subst stat failure]
whyNotMergedWith allContexts equivalentAt structuralPairs restrictionStats propagationFailure leftValue rightValue store =
  concatMap
    reportForContext
    contextEquivalenceResults
  where
    contextEquivalenceResults =
      fmap
        (\candidate -> (candidate, equivalentAt candidate leftValue rightValue store))
        allContexts

    equivalenceLookupFailure failedContext failureValue =
      EquivalenceLookupFailure
        EquivalenceLookupFailureData
          { elfdLeft = leftValue,
            elfdRight = rightValue,
            elfdContext = failedContext,
            elfdFailure = failureValue
          }

    equivalenceLookupFailures =
      [ equivalenceLookupFailure candidate failureValue
      | (candidate, Left failureValue) <- contextEquivalenceResults
      ]

    validContexts =
      [candidate | (candidate, Right True) <- contextEquivalenceResults]

    invalidContexts =
      [candidate | (candidate, Right False) <- contextEquivalenceResults]

    contextBarrier =
      [ ContextBarrier
          ContextBarrierData
            { cbdLeft = leftValue,
              cbdRight = rightValue,
              cbdValidContexts = validContexts,
              cbdInvalidContexts = invalidContexts
            }
      | not (null validContexts || null invalidContexts)
      ]

    propagationBarrier contextValue =
      [ PropagationBarrier
          PropagationBarrierData
            { pbdLeft = leftValue,
              pbdRight = rightValue,
              pbdContext = contextValue,
              pbdFailure = failureValue
            }
      | failureValue <- maybeToList (propagationFailure store)
      ]

    structuralBarrier contextValue =
      [ StructuralMismatch
          StructuralMismatchData
            { smdLeft = leftValue,
              smdRight = rightValue,
              smdContext = contextValue,
              smdMismatchedNodes = structuralPairs leftValue rightValue contextValue store
            }
      ]

    restrictionLookupFailure contextValue restrictionStatsResult =
      case restrictionStatsResult of
        Left failureValue ->
          [ RestrictionLookupFailure
              RestrictionLookupFailureData
                { rlfdLeft = leftValue,
                  rlfdRight = rightValue,
                  rlfdContext = contextValue,
                  rlfdFailure = failureValue
                }
          ]
        Right _stats ->
          []

    restrictionBarrier contextValue restrictionStatsResult =
      case restrictionStatsResult of
        Left _failureValue ->
          []
        Right stats ->
          [ RestrictionBarrier
              RestrictionBarrierData
                { rbdLeft = leftValue,
                  rbdRight = rightValue,
                  rbdContext = contextValue,
                  rbdStats = stats
                }
          | not (null stats)
          ]

    reportForContext (contextValue, equivalenceResult) =
      case equivalenceResult of
        Left failureValue ->
          [equivalenceLookupFailure contextValue failureValue]
            <> propagationBarrier contextValue
        Right True ->
          []
        Right False ->
          let restrictionStatsResult =
                restrictionStats contextValue store
           in structuralBarrier contextValue
                <> contextBarrier
                <> equivalenceLookupFailures
                <> restrictionLookupFailure contextValue restrictionStatsResult
                <> restrictionBarrier contextValue restrictionStatsResult
                <> propagationBarrier contextValue
