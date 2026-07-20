{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Carrier.Morphism.Restriction
  ( StrictDescentWitness (..),
    CompiledCarrierRestriction (..),
    CarrierRestrictionDiagnostic (..),
    RestrictionDeltaError (..),
    restrictCarrierDelta,
    CarrierRestrictionCompileError (..),
    CarrierRestrictionInstallError (..),
    CarrierRestrictionEdgeSpec (..),
    ContextRank (..),
    compileCarrierRestriction,
    compileCarrierRestrictionsForEdge,
    classMapToRepKeys,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Kind
  ( Type,
  )
import Moonlight.Core
  ( DenseKey (..),
  )
import Moonlight.Differential.Context.Restriction
  ( ContextRestrictionEdge (..),
  )
import Moonlight.Flow.Carrier.Boundary.Restrict
  ( BoundaryRestrictionError,
    restrictRuntimeBoundary,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
    RestrictKey,
    rkSource,
    rkTarget,
    restrictKey,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Origin
  ( originConsRestriction,
    originHasRestriction,
  )
import Moonlight.Flow.Carrier.Morphism.Core.Types
  ( CarrierMorphism (..),
    CarrierMorphismPlan (..),
  )
import Moonlight.Flow.Carrier.Morphism.Internal.Apply
  ( applyCarrierMorphism,
  )
import Moonlight.Differential.Row.Patch
  ( mapPlainRowPatchRows,
  )
import Moonlight.Differential.Row.Tuple
  ( RepKey (..),
    restrictTupleKey,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.FiniteLattice
  ( ContextLattice,
    ContextLatticeLookupError,
    leqContext
  )

data StrictDescentWitness ctx = StrictDescentWitness
  { sdwSourceContext :: !ctx,
    sdwTargetContext :: !ctx,
    sdwRankBefore :: !Int,
    sdwRankAfter :: !Int
  }
  deriving stock (Eq, Ord, Show)

type CompiledCarrierRestriction :: Type -> Type -> Type -> Type -> Type
data CompiledCarrierRestriction ctx carrier prop boundary = CompiledCarrierRestriction
  { ccrKey :: !(RestrictKey ctx carrier prop),
    ccrTargetClasses :: !(IntMap RepKey),
    ccrBoundaryMap :: boundary -> Either RestrictionDeltaError boundary,
    ccrDescentWitness :: !(StrictDescentWitness ctx)
  }

instance
  (Eq ctx, Eq carrier, Eq prop) =>
  Eq (CompiledCarrierRestriction ctx carrier prop boundary)
  where
  left == right =
    ccrKey left == ccrKey right
      && ccrTargetClasses left == ccrTargetClasses right
      && ccrDescentWitness left == ccrDescentWitness right

instance
  (Ord ctx, Ord carrier, Ord prop) =>
  Ord (CompiledCarrierRestriction ctx carrier prop boundary)
  where
  compare left right =
    compare
      (ccrKey left, ccrTargetClasses left, ccrDescentWitness left)
      (ccrKey right, ccrTargetClasses right, ccrDescentWitness right)

instance
  (Show ctx, Show carrier, Show prop) =>
  Show (CompiledCarrierRestriction ctx carrier prop boundary)
  where
  showsPrec precedence program =
    showParen (precedence > 10) $
      showString "CompiledCarrierRestriction "
        . shows
          ( ccrKey program,
            ccrTargetClasses program,
            ccrDescentWitness program
          )

type CarrierRestrictionDiagnostic :: Type -> Type -> Type -> Type
data CarrierRestrictionDiagnostic ctx carrier prop = CarrierRestrictionDiagnostic
  { crdSource :: !(CarrierAddr ctx carrier prop),
    crdTarget :: !(CarrierAddr ctx carrier prop),
    crdError :: !RestrictionDeltaError
  }
  deriving stock (Eq, Ord, Show)

type RestrictionDeltaError :: Type
data RestrictionDeltaError
  = RestrictionSourceTargetEqual
  | RestrictionSourceAddressMismatch
  | RestrictionLoopDetected
  | RestrictionBoundaryFailed !BoundaryRestrictionError
  | RestrictionBoundaryMapFailed
  | RestrictionDescentInvalid !Int !Int
  deriving stock (Eq, Ord, Show)

restrictCarrierDelta ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CompiledCarrierRestriction ctx carrier prop boundary ->
  RelationalCarrierDelta ctx carrier prop boundary evidence ->
  Either RestrictionDeltaError (RelationalCarrierDelta ctx carrier prop boundary evidence)
restrictCarrierDelta program =
  applyCarrierMorphism (restrictionCarrierMorphism program)
{-# INLINE restrictCarrierDelta #-}

restrictionCarrierMorphism ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CompiledCarrierRestriction ctx carrier prop boundary ->
  CarrierMorphism
    ()
    ctx
    carrier
    prop
    boundary
    evidence
    RestrictionDeltaError
restrictionCarrierMorphism program =
  CarrierMorphism
    { cmPrepare = prepareRestriction,
      cmRows = restrictRows,
      cmTime = id,
      cmSupport = \_profile -> Right,
      cmEvidence = \_profile -> Right,
      cmOrigin = originConsRestriction restrictKey,
      cmScope = id
    }
  where
    restrictKey =
      ccrKey program

    sourceAddress =
      rkSource restrictKey

    targetAddress =
      rkTarget restrictKey

    descent =
      ccrDescentWitness program

    prepareRestriction sourceDelta = do
      if deAddr sourceDelta /= sourceAddress
        then Left RestrictionSourceAddressMismatch
        else Right ()

      if sourceAddress == targetAddress
        then Left RestrictionSourceTargetEqual
        else Right ()

      if sdwRankAfter descent < sdwRankBefore descent
        then Right ()
        else Left (RestrictionDescentInvalid (sdwRankBefore descent) (sdwRankAfter descent))

      if originHasRestriction restrictKey (deOrigin sourceDelta)
        then Left RestrictionLoopDetected
        else Right ()

      targetBoundary <-
        ccrBoundaryMap program (deBoundary sourceDelta)

      pure
        CarrierMorphismPlan
          { cmpTarget = targetAddress,
            cmpBoundary = targetBoundary,
            cmpProfile = ()
          }

    restrictRows _unit =
      Right . mapPlainRowPatchRows (restrictTupleKey (ccrTargetClasses program))
{-# INLINE restrictionCarrierMorphism #-}

type ContextRank :: Type -> Type
newtype ContextRank ctx = ContextRank
  { runContextRank :: ctx -> Int
  }

type CarrierRestrictionEdgeSpec :: Type -> Type -> Type -> Type -> Type
data CarrierRestrictionEdgeSpec ctx carrier prop classId = CarrierRestrictionEdgeSpec
  { cresEdge :: !(ContextRestrictionEdge ctx),
    cresSourceAddresses :: ![CarrierAddr ctx carrier prop],
    cresTargetClasses :: !(IntMap classId)
  }
  deriving stock (Eq, Show)

data CarrierRestrictionCompileError ctx carrier prop classId
  = CarrierRestrictionSourceContextMismatch
      !(CarrierAddr ctx carrier prop)
      !(ContextRestrictionEdge ctx)
  | CarrierRestrictionNotRefinement
      !(ContextRestrictionEdge ctx)
  | CarrierRestrictionNotStrict
      !(ContextRestrictionEdge ctx)
  | CarrierRestrictionInvalidRank
      !(ContextRestrictionEdge ctx)
      !Int
      !Int
  | CarrierRestrictionLatticeLookupFailed
      !(ContextRestrictionEdge ctx)
      !(ContextLatticeLookupError ctx)
  | CarrierRestrictionNegativeClassRepresentative
      !Int
      !classId
  deriving stock (Eq, Show)

data CarrierRestrictionInstallError ctx carrier prop classId
  = CarrierRestrictionCompileFailed
      !(CarrierRestrictionCompileError ctx carrier prop classId)
  | CarrierRestrictionCycleDetected
      ![(ctx, ctx)]
  deriving stock (Eq, Show)

compileCarrierRestriction ::
  (Ord ctx, DenseKey classId) =>
  ContextLattice ctx ->
  ContextRank ctx ->
  ContextRestrictionEdge ctx ->
  CarrierAddr ctx carrier prop ->
  IntMap classId ->
  Either
    (CarrierRestrictionCompileError ctx carrier prop classId)
    (CompiledCarrierRestriction ctx carrier prop RuntimeBoundary)
compileCarrierRestriction latticeValue rankOf edge sourceAddress targetClasses = do
  let sourceContext = creSourceContext edge
      targetContext = creTargetContext edge
      beforeRank = runContextRank rankOf sourceContext
      afterRank = runContextRank rankOf targetContext
  if caContext sourceAddress == sourceContext
    then Right ()
    else Left (CarrierRestrictionSourceContextMismatch sourceAddress edge)
  if sourceContext == targetContext
    then Left (CarrierRestrictionNotStrict edge)
    else Right ()
  case leqContext latticeValue targetContext sourceContext of
    Left lookupError ->
      Left (CarrierRestrictionLatticeLookupFailed edge lookupError)
    Right True ->
      Right ()
    Right False ->
      Left (CarrierRestrictionNotRefinement edge)
  if afterRank < beforeRank
    then Right ()
    else Left (CarrierRestrictionInvalidRank edge beforeRank afterRank)
  repMap <- classMapToRepKeys targetClasses
  let targetAddress =
        sourceAddress {caContext = targetContext}
      restrictionKey =
        restrictKey sourceAddress targetAddress
  pure
    CompiledCarrierRestriction
      { ccrKey = restrictionKey,
        ccrTargetClasses = repMap,
        ccrBoundaryMap =
          \boundary ->
            first RestrictionBoundaryFailed
              (restrictRuntimeBoundary repMap boundary),
        ccrDescentWitness =
          StrictDescentWitness
            { sdwSourceContext = sourceContext,
              sdwTargetContext = targetContext,
              sdwRankBefore = beforeRank,
              sdwRankAfter = afterRank
            }
      }

compileCarrierRestrictionsForEdge ::
  (Ord ctx, DenseKey classId) =>
  ContextLattice ctx ->
  ContextRank ctx ->
  CarrierRestrictionEdgeSpec ctx carrier prop classId ->
  Either
    (CarrierRestrictionCompileError ctx carrier prop classId)
    [CompiledCarrierRestriction ctx carrier prop RuntimeBoundary]
compileCarrierRestrictionsForEdge latticeValue rankOf spec =
  traverse
    ( \sourceAddress ->
        compileCarrierRestriction
          latticeValue
          rankOf
          (cresEdge spec)
          sourceAddress
          (cresTargetClasses spec)
    )
    (cresSourceAddresses spec)

classMapToRepKeys ::
  DenseKey classId =>
  IntMap classId ->
  Either
    (CarrierRestrictionCompileError ctx carrier prop classId)
    (IntMap RepKey)
classMapToRepKeys =
  IntMap.traverseWithKey
    ( \sourceKey classId ->
        let targetKey = encodeDenseKey classId
         in if targetKey < 0
              then Left (CarrierRestrictionNegativeClassRepresentative sourceKey classId)
              else Right (RepKey targetKey)
    )
