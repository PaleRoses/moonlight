{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Carrier.Morphism.Core.Program
  ( CarrierMorphismProgram (..),
    carrierMorphismProgramSource,
    applyCarrierMorphismProgram,
    CarrierMorphismContext,
    CarrierMorphismRuntime (..),
    emptyCarrierMorphismContext,
    emptyCarrierMorphismRuntime,
    installCarrierMorphismPrograms,
    carrierMorphismContextFromRestrictionPrograms,
    carrierMorphismProgramsFrom,
    lookupCarrierMorphismRestrictionProgram,
    lookupCarrierMorphismCompiledRestriction,
    hasCarrierMorphismRestriction,
    carrierMorphismRestrictionsBetweenFrom,
    mkCarrierMorphismRuntime,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.Kind
  ( Type,
  )
import Data.List qualified as List
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Maybe
  ( isJust,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
    RestrictKey,
    rkSource,
    rkTarget,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
  )
import Moonlight.Flow.Carrier.Morphism.Core.Types
  ( CarrierMorphismState,
    emptyCarrierMorphismState,
  )
import Moonlight.Flow.Carrier.Morphism.Result
  ( CarrierMorphismError (..),
  )
import Moonlight.Flow.Carrier.Morphism.Restriction
  ( CarrierRestrictionDiagnostic (..),
    CompiledCarrierRestriction (..),
    RestrictionDeltaError,
    restrictCarrierDelta,
  )

type CarrierMorphismProgram :: Type -> Type -> Type -> Type -> Type -> Type
data CarrierMorphismProgram ctx carrier prop boundary evidence
  = RestrictionProgram
      !(CompiledCarrierRestriction ctx carrier prop boundary)
  | ReuseProgram
      !(CarrierAddr ctx carrier prop)
      !( RelationalCarrierTime ctx ->
         RelationalCarrierDelta ctx carrier prop boundary evidence ->
         Either
           (CarrierMorphismError ctx carrier prop boundary evidence)
           (RelationalCarrierDelta ctx carrier prop boundary evidence)
       )
  | AmalgamationProgram
      !(CarrierAddr ctx carrier prop)
      !( RelationalCarrierTime ctx ->
         RelationalCarrierDelta ctx carrier prop boundary evidence ->
         Either
           (CarrierMorphismError ctx carrier prop boundary evidence)
           (RelationalCarrierDelta ctx carrier prop boundary evidence)
       )

carrierMorphismProgramSource ::
  CarrierMorphismProgram ctx carrier prop boundary evidence ->
  CarrierAddr ctx carrier prop
carrierMorphismProgramSource program =
  case program of
    RestrictionProgram restriction ->
      rkSource (ccrKey restriction)
    ReuseProgram sourceAddress _apply ->
      sourceAddress
    AmalgamationProgram sourceAddress _apply ->
      sourceAddress
{-# INLINE carrierMorphismProgramSource #-}

applyCarrierMorphismProgram ::
  (Ord ctx, Ord carrier, Ord prop) =>
  RelationalCarrierTime ctx ->
  RelationalCarrierDelta ctx carrier prop boundary evidence ->
  CarrierMorphismProgram ctx carrier prop boundary evidence ->
  Either
    (CarrierMorphismError ctx carrier prop boundary evidence)
    (RelationalCarrierDelta ctx carrier prop boundary evidence)
applyCarrierMorphismProgram eventTime sourceDelta program =
  case program of
    RestrictionProgram restriction ->
      first
        (CarrierMorphismRestrictionError . restrictionDiagnostic restriction)
        ( do
            restrictedDelta <- restrictCarrierDelta restriction sourceDelta
            pure restrictedDelta {deTime = eventTime}
        )
    ReuseProgram _sourceAddress applyReuse ->
      applyReuse eventTime sourceDelta
    AmalgamationProgram _sourceAddress applyAmalgamation ->
      applyAmalgamation eventTime sourceDelta
{-# INLINE applyCarrierMorphismProgram #-}

type CarrierMorphismContext :: Type -> Type -> Type -> Type -> Type -> Type
data CarrierMorphismContext ctx carrier prop boundary evidence = CarrierMorphismContext
  { cmcProgramsBySource ::
      !(Map
          (CarrierAddr ctx carrier prop)
          [CarrierMorphismProgram ctx carrier prop boundary evidence]
       )
  }

instance
  Ord (CarrierAddr ctx carrier prop) =>
  Semigroup (CarrierMorphismContext ctx carrier prop boundary evidence)
  where
  left <> right =
    CarrierMorphismContext
      { cmcProgramsBySource =
          Map.unionWith (<>) (cmcProgramsBySource left) (cmcProgramsBySource right)
      }

instance
  Ord (CarrierAddr ctx carrier prop) =>
  Monoid (CarrierMorphismContext ctx carrier prop boundary evidence)
  where
  mempty =
    emptyCarrierMorphismContext

type CarrierMorphismRuntime :: Type -> Type -> Type -> Type -> Type -> Type
data CarrierMorphismRuntime ctx carrier prop boundary evidence = CarrierMorphismRuntime
  { cmrContext :: !(CarrierMorphismContext ctx carrier prop boundary evidence),
    cmrState :: !CarrierMorphismState
  }

emptyCarrierMorphismContext :: CarrierMorphismContext ctx carrier prop boundary evidence
emptyCarrierMorphismContext =
  CarrierMorphismContext
    { cmcProgramsBySource = Map.empty
    }
{-# INLINE emptyCarrierMorphismContext #-}

emptyCarrierMorphismRuntime :: CarrierMorphismRuntime ctx carrier prop boundary evidence
emptyCarrierMorphismRuntime =
  mkCarrierMorphismRuntime emptyCarrierMorphismContext
{-# INLINE emptyCarrierMorphismRuntime #-}

installCarrierMorphismPrograms ::
  (Ord ctx, Ord carrier, Ord prop) =>
  [CarrierMorphismProgram ctx carrier prop boundary evidence] ->
  CarrierMorphismContext ctx carrier prop boundary evidence
installCarrierMorphismPrograms programs =
  CarrierMorphismContext
    { cmcProgramsBySource = programsBySource programs
    }
{-# INLINE installCarrierMorphismPrograms #-}

carrierMorphismContextFromRestrictionPrograms ::
  (Ord ctx, Ord carrier, Ord prop) =>
  [CompiledCarrierRestriction ctx carrier prop boundary] ->
  CarrierMorphismContext ctx carrier prop boundary evidence
carrierMorphismContextFromRestrictionPrograms =
  installCarrierMorphismPrograms . fmap restrictionProgram
{-# INLINE carrierMorphismContextFromRestrictionPrograms #-}

programsBySource ::
  (Ord ctx, Ord carrier, Ord prop) =>
  [CarrierMorphismProgram ctx carrier prop boundary evidence] ->
  Map
    (CarrierAddr ctx carrier prop)
    [CarrierMorphismProgram ctx carrier prop boundary evidence]
programsBySource =
  List.foldl' insertProgram Map.empty
  where
    insertProgram ::
      (Ord ctx, Ord carrier, Ord prop) =>
      Map
        (CarrierAddr ctx carrier prop)
        [CarrierMorphismProgram ctx carrier prop boundary evidence] ->
      CarrierMorphismProgram ctx carrier prop boundary evidence ->
      Map
        (CarrierAddr ctx carrier prop)
        [CarrierMorphismProgram ctx carrier prop boundary evidence]
    insertProgram programs program =
      Map.insertWith (<>) (carrierMorphismProgramSource program) [program] programs
{-# INLINE programsBySource #-}

carrierMorphismProgramsFrom ::
  Ord (CarrierAddr ctx carrier prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierMorphismContext ctx carrier prop boundary evidence ->
  [CarrierMorphismProgram ctx carrier prop boundary evidence]
carrierMorphismProgramsFrom sourceAddress contextValue =
  Map.findWithDefault [] sourceAddress (cmcProgramsBySource contextValue)
{-# INLINE carrierMorphismProgramsFrom #-}

lookupCarrierMorphismRestrictionProgram ::
  (Ord ctx, Ord carrier, Ord prop) =>
  RestrictKey ctx carrier prop ->
  CarrierMorphismRuntime ctx carrier prop boundary evidence ->
  Maybe (CarrierMorphismProgram ctx carrier prop boundary evidence)
lookupCarrierMorphismRestrictionProgram restrictKey runtime =
  List.find
    (restrictionProgramHasKey restrictKey)
    (carrierMorphismProgramsFrom (rkSource restrictKey) (cmrContext runtime))
{-# INLINE lookupCarrierMorphismRestrictionProgram #-}

lookupCarrierMorphismCompiledRestriction ::
  (Ord ctx, Ord carrier, Ord prop) =>
  RestrictKey ctx carrier prop ->
  CarrierMorphismContext ctx carrier prop boundary evidence ->
  Maybe (CompiledCarrierRestriction ctx carrier prop boundary)
lookupCarrierMorphismCompiledRestriction restrictKey contextValue =
  List.find
    ((== restrictKey) . ccrKey)
    (restrictionProgramsFrom (rkSource restrictKey) contextValue)
{-# INLINE lookupCarrierMorphismCompiledRestriction #-}

hasCarrierMorphismRestriction ::
  (Ord ctx, Ord carrier, Ord prop) =>
  RestrictKey ctx carrier prop ->
  CarrierMorphismRuntime ctx carrier prop boundary evidence ->
  Bool
hasCarrierMorphismRestriction restrictKey =
  isJust . lookupCarrierMorphismRestrictionProgram restrictKey
{-# INLINE hasCarrierMorphismRestriction #-}

carrierMorphismRestrictionsBetweenFrom ::
  (Ord ctx, Ord carrier, Ord prop) =>
  ctx ->
  ctx ->
  CarrierAddr ctx carrier prop ->
  CarrierMorphismContext ctx carrier prop boundary evidence ->
  [CompiledCarrierRestriction ctx carrier prop boundary]
carrierMorphismRestrictionsBetweenFrom sourceContext targetContext sourceAddress contextValue =
  filter
    (restrictionProgramBetween sourceContext targetContext)
    (restrictionProgramsFrom sourceAddress contextValue)
{-# INLINE carrierMorphismRestrictionsBetweenFrom #-}

restrictionProgramsFrom ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierMorphismContext ctx carrier prop boundary evidence ->
  [CompiledCarrierRestriction ctx carrier prop boundary]
restrictionProgramsFrom sourceAddress contextValue =
  Map.findWithDefault [] sourceAddress (cmcProgramsBySource contextValue)
    >>= restrictionProgramMaybe
{-# INLINE restrictionProgramsFrom #-}

restrictionProgramMaybe ::
  CarrierMorphismProgram ctx carrier prop boundary evidence ->
  [CompiledCarrierRestriction ctx carrier prop boundary]
restrictionProgramMaybe program =
  case program of
    RestrictionProgram restriction ->
      [restriction]
    ReuseProgram {} ->
      []
    AmalgamationProgram {} ->
      []
{-# INLINE restrictionProgramMaybe #-}

restrictionProgramHasKey ::
  (Eq ctx, Eq carrier, Eq prop) =>
  RestrictKey ctx carrier prop ->
  CarrierMorphismProgram ctx carrier prop boundary evidence ->
  Bool
restrictionProgramHasKey restrictKey program =
  case program of
    RestrictionProgram restriction ->
      ccrKey restriction == restrictKey
    ReuseProgram {} ->
      False
    AmalgamationProgram {} ->
      False
{-# INLINE restrictionProgramHasKey #-}

restrictionProgramBetween ::
  Eq ctx =>
  ctx ->
  ctx ->
  CompiledCarrierRestriction ctx carrier prop boundary ->
  Bool
restrictionProgramBetween sourceContext targetContext program =
  let key =
        ccrKey program
   in caContext (rkSource key) == sourceContext
        && caContext (rkTarget key) == targetContext
{-# INLINE restrictionProgramBetween #-}

mkCarrierMorphismRuntime ::
  CarrierMorphismContext ctx carrier prop boundary evidence ->
  CarrierMorphismRuntime ctx carrier prop boundary evidence
mkCarrierMorphismRuntime contextValue =
  CarrierMorphismRuntime
    { cmrContext = contextValue,
      cmrState = emptyCarrierMorphismState
    }
{-# INLINE mkCarrierMorphismRuntime #-}

restrictionProgram ::
  CompiledCarrierRestriction ctx carrier prop boundary ->
  CarrierMorphismProgram ctx carrier prop boundary evidence
restrictionProgram =
  RestrictionProgram
{-# INLINE restrictionProgram #-}

restrictionDiagnostic ::
  CompiledCarrierRestriction ctx carrier prop boundary ->
  RestrictionDeltaError ->
  CarrierRestrictionDiagnostic ctx carrier prop
restrictionDiagnostic program restrictionError =
  CarrierRestrictionDiagnostic
    { crdSource = rkSource (ccrKey program),
      crdTarget = rkTarget (ccrKey program),
      crdError = restrictionError
    }
{-# INLINE restrictionDiagnostic #-}
