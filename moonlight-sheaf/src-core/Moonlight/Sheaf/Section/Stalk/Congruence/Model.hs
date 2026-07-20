module Moonlight.Sheaf.Section.Stalk.Congruence.Model
  ( CongruenceStalk,
    CongruenceRestriction,
    CongruenceRestrictionObstruction (..),
    CongruenceStalkMergeDelta (..),
    PreparedCongruenceRestrictionSpec (..),
    PreparedCongruenceBuildError (..),
    PreparedCongruenceStalk,
    PreparedCongruenceRestriction,
    PreparedCongruenceModel,
    mkDiscreteCongruenceStalk,
    mkCongruenceStalkFromPairs,
    mkCongruenceStalkFromRelation,
    congruenceStalkCarrier,
    congruenceStalkVisible,
    congruenceStalkRelation,
    replaceCongruenceStalkRelation,
    applyCanonicalCongruenceStalkSeeds,
    applyCongruenceStalkMergesCounted,
    congruenceRestrictionCarrier,
    congruenceRestrictionSourceVisible,
    congruenceRestrictionTargetVisible,
    mkCongruenceRestriction,
    restrictCongruenceStalk,
    mergeCongruenceStalks,
    congruenceMismatches,
    normalizeCongruenceStalk,
    prepareCongruenceModelWith,
    preparedCongruenceSheafModel,
    preparedCongruenceVisibleAt,
    preparedCongruenceRestrictionForSpec,
    mkPreparedCongruenceStalkFromRelationAt,
    preparedCongruenceStalkAlgebra,
  )
where

import Control.Monad (join)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core (DenseKey (..))
import Moonlight.Sheaf.Section.Congruence.Equivalence.Relation
  ( DomainEndomap,
    DomainEquivalence,
    EquivalenceDomain,
    EquivalenceEndomap,
    EquivalenceRelation,
    EquivalenceRelationError,
    applyCheckedDomainEndomap,
    applyDomainEndomap,
    applyDomainEquivalenceMergesCounted,
    discreteEquivalence,
    domainEquivalenceRaw,
    equivalenceFromPairs,
    mergeCheckedDomainEquivalence,
    mergeDomainEquivalence,
    mkEquivalenceEndomap,
    mkDomainEndomap,
    mkDomainEquivalence,
    withEquivalenceDomain,
  )
import Moonlight.Sheaf.Section.Model
  ( SheafModel,
    SheafModelBuildError,
    withPreparedSheafModel,
  )
import Moonlight.Sheaf.Section.Morphism
  ( RestrictionKind,
    RestrictionParts (..),
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( initialSheafModelVersion,
    mkObjectIndex,
  )
import Moonlight.Sheaf.Section.Stalk
  ( MergeObstruction (..),
    RepairInput (..),
    StalkAlgebra (..),
    StalkRestrictionKernel (..),
    mismatchObstruction,
  )
import Moonlight.Sheaf.Section.Stalk.Congruence.Carrier
  ( CarrierId (..),
    CongruenceConstructionError (..),
    CongruenceVisibleSide (..),
    GlobalCarrier,
    carrierDomain,
    carrierIndexedValues,
    congruenceEndomapError,
    globalCarrierId,
    sameCarrier,
    visibleKeySet,
  )
import Moonlight.Sheaf.Section.Stalk.Congruence.Mismatch
  ( CongruenceMismatch (..),
    carrierMismatchPair,
    representativeMismatchPair,
    visibleMismatchPair,
  )

type CongruenceStalk :: Type -> Type -> Type
data CongruenceStalk rep atom = CongruenceStalk
  { csCarrier :: !(GlobalCarrier rep atom),
    csVisible :: !IntSet,
    csRelation :: !(RuntimeCongruenceRelation rep)
  }
  deriving stock (Eq, Show)

type CongruenceRestriction :: Type -> Type -> Type
data CongruenceRestriction rep atom = CongruenceRestriction
  { crCarrier :: !(GlobalCarrier rep atom),
    crSourceVisible :: !IntSet,
    crTargetVisible :: !IntSet,
    crEndomap :: !(EquivalenceEndomap rep)
  }
  deriving stock (Eq, Show)

type RuntimeCongruenceRelation :: Type -> Type
data RuntimeCongruenceRelation rep where
  RuntimeCongruenceRelation :: !(DomainEquivalence carrier rep) -> RuntimeCongruenceRelation rep

instance Eq rep => Eq (RuntimeCongruenceRelation rep) where
  leftRelation == rightRelation =
    runtimeRelationRaw leftRelation == runtimeRelationRaw rightRelation

instance Show rep => Show (RuntimeCongruenceRelation rep) where
  show =
    show . runtimeRelationRaw

type PreparedCongruenceStalk :: Type -> Type -> Type -> Type
type role PreparedCongruenceStalk nominal nominal nominal
data PreparedCongruenceStalk carrier rep atom = PreparedCongruenceStalk
  { pcsVisible :: !IntSet,
    pcsRelation :: !(DomainEquivalence carrier rep)
  }
  deriving stock (Eq, Ord, Show)

type PreparedCongruenceRestriction :: Type -> Type -> Type -> Type
type role PreparedCongruenceRestriction nominal nominal nominal
data PreparedCongruenceRestriction carrier rep atom = PreparedCongruenceRestriction
  { pcrTargetVisible :: !IntSet,
    pcrEndomap :: !(DomainEndomap carrier rep)
  }
  deriving stock (Eq, Show)

type PreparedCongruenceModel :: Type -> Type -> Type -> Type -> Type -> Type
type role PreparedCongruenceModel nominal nominal nominal nominal nominal
data PreparedCongruenceModel carrier owner cell rep atom = PreparedCongruenceModel
  { pcmDomain :: !(EquivalenceDomain carrier rep),
    pcmVisibleByCell :: !(Map.Map cell IntSet),
    pcmSheafModel :: !(SheafModel owner cell (PreparedCongruenceRestriction carrier rep atom))
  }
  deriving stock (Eq, Show)

type CongruenceRestrictionObstruction :: Type -> Type -> Type
data CongruenceRestrictionObstruction rep atom
  = CongruenceRestrictionCarrierMismatch
      !CarrierId
      !CarrierId
      ![(rep, atom)]
      ![(rep, atom)]
  | CongruenceRestrictionRelationFailure !EquivalenceRelationError
  deriving stock (Eq, Ord, Show)

type CongruenceStalkMergeDelta :: Type -> Type -> Type
data CongruenceStalkMergeDelta rep atom = CongruenceStalkMergeDelta
  { csmdStalk :: !(CongruenceStalk rep atom),
    csmdChanged :: !IntSet
  }
  deriving stock (Eq, Show)

type PreparedCongruenceRestrictionSpec :: Type -> Type -> Type
data PreparedCongruenceRestrictionSpec cell rep = PreparedCongruenceRestrictionSpec
  { pcrsKind :: !RestrictionKind,
    pcrsSource :: !cell,
    pcrsTarget :: !cell,
    pcrsCarrierMap :: !(IntMap rep)
  }
  deriving stock (Eq, Show)

type PreparedCongruenceBuildError :: Type -> Type -> Type
data PreparedCongruenceBuildError cell atom
  = PreparedCongruenceDomainInvalid !EquivalenceRelationError
  | PreparedCongruenceVisibleSupportMissing !cell
  | PreparedCongruenceVisibleSupportUnknownObject !cell
  | PreparedCongruenceVisibleSupportInvalid !cell !(CongruenceConstructionError atom)
  | PreparedCongruenceRestrictionMissingVisible !cell
  | PreparedCongruenceRestrictionInvalid !cell !cell !(CongruenceConstructionError atom)
  | PreparedCongruenceModelInvalid !(SheafModelBuildError cell)
  | PreparedCongruenceStalkUnknownCell !cell
  | PreparedCongruenceStalkRelationInvalid !cell !EquivalenceRelationError
  deriving stock (Eq, Show)

mkDiscreteCongruenceStalk ::
  DenseKey rep =>
  GlobalCarrier rep atom ->
  [rep] ->
  Either (CongruenceConstructionError atom) (CongruenceStalk rep atom)
mkDiscreteCongruenceStalk carrier visibleKeys = do
  relationValue <-
    first CongruenceRelationFailure $
      discreteEquivalence (carrierDomain carrier)
  mkCongruenceStalkFromRelation carrier visibleKeys relationValue
{-# INLINEABLE mkDiscreteCongruenceStalk #-}

mkCongruenceStalkFromPairs ::
  DenseKey rep =>
  GlobalCarrier rep atom ->
  [rep] ->
  [(rep, rep)] ->
  Either (CongruenceConstructionError atom) (CongruenceStalk rep atom)
mkCongruenceStalkFromPairs carrier visibleKeys pairs = do
  relationValue <-
    first CongruenceRelationFailure $
      equivalenceFromPairs
        (carrierDomain carrier)
        pairs
  mkCongruenceStalkFromRelation carrier visibleKeys relationValue
{-# INLINEABLE mkCongruenceStalkFromPairs #-}

mkCongruenceStalkFromRelation ::
  DenseKey rep =>
  GlobalCarrier rep atom ->
  [rep] ->
  EquivalenceRelation rep ->
  Either (CongruenceConstructionError atom) (CongruenceStalk rep atom)
mkCongruenceStalkFromRelation carrier visibleKeys relationValue = do
  visible <- visibleKeySet carrier CongruenceStalkVisible visibleKeys
  domainRelation <- runtimeDomainEquivalence carrier relationValue
  pure
    CongruenceStalk
      { csCarrier = carrier,
        csVisible = visible,
        csRelation = domainRelation
      }
{-# INLINEABLE mkCongruenceStalkFromRelation #-}

congruenceStalkCarrier :: CongruenceStalk rep atom -> GlobalCarrier rep atom
congruenceStalkCarrier =
  csCarrier
{-# INLINE congruenceStalkCarrier #-}

congruenceStalkVisible :: CongruenceStalk rep atom -> IntSet
congruenceStalkVisible =
  csVisible
{-# INLINE congruenceStalkVisible #-}

congruenceStalkRelation ::
  CongruenceStalk rep atom ->
  EquivalenceRelation rep
congruenceStalkRelation =
  runtimeRelationRaw . csRelation
{-# INLINE congruenceStalkRelation #-}

replaceCongruenceStalkRelation ::
  DenseKey rep =>
  EquivalenceRelation rep ->
  CongruenceStalk rep atom ->
  Either (CongruenceConstructionError atom) (CongruenceStalk rep atom)
replaceCongruenceStalkRelation relationValue stalk = do
  domainRelation <- runtimeDomainEquivalence (csCarrier stalk) relationValue
  pure stalk {csRelation = domainRelation}
{-# INLINEABLE replaceCongruenceStalkRelation #-}

applyCanonicalCongruenceStalkSeeds ::
  DenseKey rep =>
  [(rep, rep)] ->
  CongruenceStalk rep atom ->
  Either (CongruenceConstructionError atom) (CongruenceStalkMergeDelta rep atom)
applyCanonicalCongruenceStalkSeeds canonicalizedUnions stalk = do
  (relationValue, changedKeys, _) <-
    applyRuntimeCongruenceMerges canonicalizedUnions (csRelation stalk)
  pure
    CongruenceStalkMergeDelta
      { csmdStalk = stalk {csRelation = relationValue},
        csmdChanged = changedKeys
      }
{-# INLINEABLE applyCanonicalCongruenceStalkSeeds #-}

applyCongruenceStalkMergesCounted ::
  DenseKey rep =>
  [(rep, rep)] ->
  CongruenceStalk rep atom ->
  Either
    (CongruenceConstructionError atom)
    (CongruenceStalkMergeDelta rep atom, Int)
applyCongruenceStalkMergesCounted repPairs stalk = do
  (relationValue, changedKeys, mergeCount) <-
    applyRuntimeCongruenceMerges repPairs (csRelation stalk)
  pure
    ( CongruenceStalkMergeDelta
        { csmdStalk = stalk {csRelation = relationValue},
          csmdChanged = changedKeys
        },
      mergeCount
    )
{-# INLINEABLE applyCongruenceStalkMergesCounted #-}

congruenceRestrictionCarrier :: CongruenceRestriction rep atom -> GlobalCarrier rep atom
congruenceRestrictionCarrier =
  crCarrier
{-# INLINE congruenceRestrictionCarrier #-}

congruenceRestrictionSourceVisible ::
  CongruenceRestriction rep atom ->
  IntSet
congruenceRestrictionSourceVisible =
  crSourceVisible
{-# INLINE congruenceRestrictionSourceVisible #-}

congruenceRestrictionTargetVisible ::
  CongruenceRestriction rep atom ->
  IntSet
congruenceRestrictionTargetVisible =
  crTargetVisible
{-# INLINE congruenceRestrictionTargetVisible #-}

mkCongruenceRestriction ::
  DenseKey rep =>
  GlobalCarrier rep atom ->
  [rep] ->
  [rep] ->
  IntMap rep ->
  Either (CongruenceConstructionError atom) (CongruenceRestriction rep atom)
mkCongruenceRestriction carrier sourceVisibleKeys targetVisibleKeys carrierMap = do
  sourceVisible <-
    visibleKeySet
      carrier
      CongruenceRestrictionSourceVisible
      sourceVisibleKeys
  targetVisible <-
    visibleKeySet
      carrier
      CongruenceRestrictionTargetVisible
      targetVisibleKeys
  endomap <-
    first congruenceEndomapError $
      mkEquivalenceEndomap (carrierDomain carrier) carrierMap
  validateSourceVisibleImage targetVisible carrierMap sourceVisible
  pure
    CongruenceRestriction
      { crCarrier = carrier,
        crSourceVisible = sourceVisible,
        crTargetVisible = targetVisible,
        crEndomap = endomap
      }
{-# INLINEABLE mkCongruenceRestriction #-}

restrictCongruenceStalk ::
  (DenseKey rep, Eq atom) =>
  CongruenceRestriction rep atom ->
  CongruenceStalk rep atom ->
  Either
    (CongruenceRestrictionObstruction rep atom)
    (CongruenceStalk rep atom)
restrictCongruenceStalk restriction stalk
  | sameCarrier (crCarrier restriction) (csCarrier stalk) =
      case csRelation stalk of
        RuntimeCongruenceRelation relationValue ->
          fmap
            ( \restrictedRelation ->
                stalk
                  { csVisible = crTargetVisible restriction,
                    csRelation = RuntimeCongruenceRelation restrictedRelation
                  }
            )
            ( first CongruenceRestrictionRelationFailure $
                applyCheckedDomainEndomap
                  (crEndomap restriction)
                  relationValue
            )
  | otherwise =
      Left
        ( CongruenceRestrictionCarrierMismatch
            (globalCarrierId (crCarrier restriction))
            (globalCarrierId (csCarrier stalk))
            (carrierIndexedValues (crCarrier restriction))
            (carrierIndexedValues (csCarrier stalk))
        )
{-# INLINEABLE restrictCongruenceStalk #-}

mergeCongruenceStalks ::
  (DenseKey rep, Eq atom) =>
  CongruenceStalk rep atom ->
  CongruenceStalk rep atom ->
  Either
    (MergeObstruction (CongruenceMismatch rep atom))
    (CongruenceStalk rep atom)
mergeCongruenceStalks leftStalk rightStalk =
  case mismatchObstruction (mergeShapeMismatches leftStalk rightStalk) of
    Just obstruction ->
      Left obstruction
    Nothing ->
      case (csRelation leftStalk, csRelation rightStalk) of
        (RuntimeCongruenceRelation leftRelation, RuntimeCongruenceRelation rightRelation) ->
          first (MergeMismatchObstruction . pure . CongruenceRelationMismatch) $
            fmap
              (\relationValue -> leftStalk {csRelation = RuntimeCongruenceRelation relationValue})
              (mergeCheckedDomainEquivalence leftRelation rightRelation)
{-# INLINEABLE mergeCongruenceStalks #-}

congruenceMismatches ::
  (DenseKey rep, Eq atom) =>
  CongruenceStalk rep atom ->
  CongruenceStalk rep atom ->
  [CongruenceMismatch rep atom]
congruenceMismatches leftStalk rightStalk =
  mergeShapeMismatches leftStalk rightStalk
    <> representativeMismatches leftStalk rightStalk
{-# INLINEABLE congruenceMismatches #-}

normalizeCongruenceStalk ::
  CongruenceStalk rep atom ->
  CongruenceStalk rep atom
normalizeCongruenceStalk stalk =
  stalk
    { csRelation =
        normalizeRuntimeRelation (csRelation stalk)
    }
{-# INLINEABLE normalizeCongruenceStalk #-}

prepareCongruenceModelWith ::
  (DenseKey rep, Ord cell) =>
  GlobalCarrier rep atom ->
  [cell] ->
  Map.Map cell [rep] ->
  [PreparedCongruenceRestrictionSpec cell rep] ->
  (forall carrier owner. PreparedCongruenceModel carrier owner cell rep atom -> result) ->
  Either (PreparedCongruenceBuildError cell atom) result
prepareCongruenceModelWith carrier cells visibleSupport restrictions continue =
  case
    withEquivalenceDomain (carrierDomain carrier) $ \domain -> do
      rejectUnknownVisibleSupport cells visibleSupport
      visibleByCell <-
        fmap Map.fromList $
          traverse
            (compileVisibleSupport carrier visibleSupport)
            cells
      compiledRestrictions <-
        traverse
          (compilePreparedRestriction domain visibleByCell)
          restrictions
      first PreparedCongruenceModelInvalid $
        withPreparedSheafModel
          initialSheafModelVersion
          (mkObjectIndex cells)
          ( \(restrictionSpec, compiledRestriction) ->
              RestrictionParts
                { partKind = pcrsKind restrictionSpec,
                  partSource = pcrsSource restrictionSpec,
                  partTarget = pcrsTarget restrictionSpec,
                  partWitness = compiledRestriction
                }
          )
          compiledRestrictions
          ( \sheafModel ->
              continue
                PreparedCongruenceModel
                  { pcmDomain = domain,
                    pcmVisibleByCell = visibleByCell,
                    pcmSheafModel = sheafModel
                  }
          )
    of
    Left domainFailure ->
      Left (PreparedCongruenceDomainInvalid domainFailure)
    Right outcome ->
      outcome
{-# INLINEABLE prepareCongruenceModelWith #-}

preparedCongruenceSheafModel ::
  PreparedCongruenceModel carrier owner cell rep atom ->
  SheafModel owner cell (PreparedCongruenceRestriction carrier rep atom)
preparedCongruenceSheafModel =
  pcmSheafModel
{-# INLINE preparedCongruenceSheafModel #-}

preparedCongruenceVisibleAt ::
  Ord cell =>
  cell ->
  PreparedCongruenceModel carrier owner cell rep atom ->
  Maybe IntSet
preparedCongruenceVisibleAt cell model =
  Map.lookup cell (pcmVisibleByCell model)
{-# INLINEABLE preparedCongruenceVisibleAt #-}

preparedCongruenceRestrictionForSpec ::
  (DenseKey rep, Ord cell) =>
  PreparedCongruenceModel carrier owner cell rep atom ->
  PreparedCongruenceRestrictionSpec cell rep ->
  Either
    (PreparedCongruenceBuildError cell atom)
    (PreparedCongruenceRestriction carrier rep atom)
preparedCongruenceRestrictionForSpec model spec =
  fmap snd $
    compilePreparedRestriction
      (pcmDomain model)
      (pcmVisibleByCell model)
      spec
{-# INLINEABLE preparedCongruenceRestrictionForSpec #-}

mkPreparedCongruenceStalkFromRelationAt ::
  (DenseKey rep, Ord cell) =>
  PreparedCongruenceModel carrier owner cell rep atom ->
  cell ->
  EquivalenceRelation rep ->
  Either
    (PreparedCongruenceBuildError cell atom)
    (PreparedCongruenceStalk carrier rep atom)
mkPreparedCongruenceStalkFromRelationAt model cell relationValue = do
  visible <-
    maybe
      (Left (PreparedCongruenceStalkUnknownCell cell))
      Right
      (preparedCongruenceVisibleAt cell model)
  domainRelation <-
    first (PreparedCongruenceStalkRelationInvalid cell) $
      mkDomainEquivalence (pcmDomain model) relationValue
  pure
    PreparedCongruenceStalk
      { pcsVisible = visible,
        pcsRelation = domainRelation
      }
{-# INLINEABLE mkPreparedCongruenceStalkFromRelationAt #-}

restrictPreparedCongruenceStalk ::
  DenseKey rep =>
  PreparedCongruenceRestriction carrier rep atom ->
  PreparedCongruenceStalk carrier rep atom ->
  PreparedCongruenceStalk carrier rep atom
restrictPreparedCongruenceStalk restriction stalk =
  PreparedCongruenceStalk
    { pcsVisible = pcrTargetVisible restriction,
      pcsRelation =
        applyDomainEndomap
          (pcrEndomap restriction)
          (pcsRelation stalk)
    }
{-# INLINEABLE restrictPreparedCongruenceStalk #-}

mergePreparedCongruenceStalks ::
  DenseKey rep =>
  PreparedCongruenceStalk carrier rep atom ->
  PreparedCongruenceStalk carrier rep atom ->
  Either
    (MergeObstruction (CongruenceMismatch rep atom))
    (PreparedCongruenceStalk carrier rep atom)
mergePreparedCongruenceStalks leftStalk rightStalk =
  maybe
    ( Right
        PreparedCongruenceStalk
          { pcsVisible = pcsVisible leftStalk,
            pcsRelation =
              mergeDomainEquivalence
                (pcsRelation leftStalk)
                (pcsRelation rightStalk)
          }
    )
    Left
    (mismatchObstruction (preparedVisibleMismatches leftStalk rightStalk))
{-# INLINEABLE mergePreparedCongruenceStalks #-}

preparedCongruenceMismatches ::
  DenseKey rep =>
  PreparedCongruenceStalk carrier rep atom ->
  PreparedCongruenceStalk carrier rep atom ->
  [CongruenceMismatch rep atom]
preparedCongruenceMismatches leftStalk rightStalk =
  preparedVisibleMismatches leftStalk rightStalk
    <> representativeMismatchPair
      (pcsVisible leftStalk, domainEquivalenceRaw (pcsRelation leftStalk))
      (pcsVisible rightStalk, domainEquivalenceRaw (pcsRelation rightStalk))
{-# INLINEABLE preparedCongruenceMismatches #-}

normalizePreparedCongruenceStalk ::
  PreparedCongruenceStalk carrier rep atom ->
  PreparedCongruenceStalk carrier rep atom
normalizePreparedCongruenceStalk =
  id
{-# INLINEABLE normalizePreparedCongruenceStalk #-}

preparedCongruenceStalkAlgebra ::
  DenseKey rep =>
  StalkAlgebra
    (PreparedCongruenceRestriction carrier rep atom)
    (PreparedCongruenceStalk carrier rep atom)
    (CongruenceMismatch rep atom)
    (MergeObstruction (CongruenceMismatch rep atom))
preparedCongruenceStalkAlgebra =
  StalkAlgebra
    { saRestrictionKernel = StalkRestrictionMap . restrictPreparedCongruenceStalk,
      saMismatches = preparedCongruenceMismatches,
      saMerge = mergePreparedCongruenceStalks,
      saRepair = repairPreparedCongruenceStalk,
      saNormalize = normalizePreparedCongruenceStalk
    }
{-# INLINEABLE preparedCongruenceStalkAlgebra #-}

rejectUnknownVisibleSupport ::
  Ord cell =>
  [cell] ->
  Map.Map cell [rep] ->
  Either (PreparedCongruenceBuildError cell atom) ()
rejectUnknownVisibleSupport cells visibleSupport =
  case Set.lookupMin unknownCells of
    Nothing ->
      Right ()
    Just unknownCell ->
      Left (PreparedCongruenceVisibleSupportUnknownObject unknownCell)
  where
    unknownCells =
      Set.difference
        (Map.keysSet visibleSupport)
        (Set.fromList cells)
{-# INLINEABLE rejectUnknownVisibleSupport #-}

compileVisibleSupport ::
  (DenseKey rep, Ord cell) =>
  GlobalCarrier rep atom ->
  Map.Map cell [rep] ->
  cell ->
  Either (PreparedCongruenceBuildError cell atom) (cell, IntSet)
compileVisibleSupport carrier visibleSupport cell =
  case Map.lookup cell visibleSupport of
    Nothing ->
      Left (PreparedCongruenceVisibleSupportMissing cell)
    Just visibleKeys -> do
      visible <-
        first (PreparedCongruenceVisibleSupportInvalid cell) $
          visibleKeySet carrier CongruenceStalkVisible visibleKeys
      pure (cell, visible)
{-# INLINEABLE compileVisibleSupport #-}

compilePreparedRestriction ::
  (DenseKey rep, Ord cell) =>
  EquivalenceDomain carrier rep ->
  Map.Map cell IntSet ->
  PreparedCongruenceRestrictionSpec cell rep ->
  Either
    (PreparedCongruenceBuildError cell atom)
    ( PreparedCongruenceRestrictionSpec cell rep,
      PreparedCongruenceRestriction carrier rep atom
    )
compilePreparedRestriction domain visibleByCell spec = do
  sourceVisible <-
    maybe
      (Left (PreparedCongruenceRestrictionMissingVisible (pcrsSource spec)))
      Right
      (Map.lookup (pcrsSource spec) visibleByCell)
  targetVisible <-
    maybe
      (Left (PreparedCongruenceRestrictionMissingVisible (pcrsTarget spec)))
      Right
      (Map.lookup (pcrsTarget spec) visibleByCell)
  domainEndomap <-
    first (PreparedCongruenceRestrictionInvalid (pcrsSource spec) (pcrsTarget spec) . congruenceEndomapError) $
      mkDomainEndomap domain (pcrsCarrierMap spec)
  first (PreparedCongruenceRestrictionInvalid (pcrsSource spec) (pcrsTarget spec)) $
    validateSourceVisibleImage targetVisible (pcrsCarrierMap spec) sourceVisible
  pure
    ( spec,
      PreparedCongruenceRestriction
        { pcrTargetVisible = targetVisible,
          pcrEndomap = domainEndomap
        }
    )
{-# INLINEABLE compilePreparedRestriction #-}

repairPreparedCongruenceStalk ::
  DenseKey rep =>
  RepairInput
    (PreparedCongruenceRestriction carrier rep atom)
    (PreparedCongruenceStalk carrier rep atom)
    (CongruenceMismatch rep atom) ->
  Either
    (MergeObstruction (CongruenceMismatch rep atom))
    (PreparedCongruenceStalk carrier rep atom)
repairPreparedCongruenceStalk input =
  case input of
    RepairRestrictionInput _ restrictedStalk targetStalk _ ->
      repairPreparedCandidate
        targetStalk
        (preparedCongruenceMismatches restrictedStalk targetStalk)
    RepairMergeInput stalks _ ->
      repairPreparedCandidate
        (NonEmpty.head stalks)
        (foldMap (preparedCongruenceMismatches (NonEmpty.head stalks)) (NonEmpty.tail stalks))
{-# INLINEABLE repairPreparedCongruenceStalk #-}

repairPreparedCandidate ::
  PreparedCongruenceStalk carrier rep atom ->
  [CongruenceMismatch rep atom] ->
  Either
    (MergeObstruction (CongruenceMismatch rep atom))
    (PreparedCongruenceStalk carrier rep atom)
repairPreparedCandidate candidate mismatches =
  case mismatchObstruction mismatches of
    Nothing ->
      Right (normalizePreparedCongruenceStalk candidate)
    Just obstruction ->
      Left obstruction
{-# INLINEABLE repairPreparedCandidate #-}

preparedVisibleMismatches ::
  PreparedCongruenceStalk carrier rep atom ->
  PreparedCongruenceStalk carrier rep atom ->
  [CongruenceMismatch rep atom]
preparedVisibleMismatches leftStalk rightStalk =
  visibleMismatchPair (pcsVisible leftStalk) (pcsVisible rightStalk)
{-# INLINE preparedVisibleMismatches #-}

mergeShapeMismatches ::
  (DenseKey rep, Eq atom) =>
  CongruenceStalk rep atom ->
  CongruenceStalk rep atom ->
  [CongruenceMismatch rep atom]
mergeShapeMismatches leftStalk rightStalk =
  carrierMismatchPair (csCarrier leftStalk) (csCarrier rightStalk)
    <> visibleMismatchPair (csVisible leftStalk) (csVisible rightStalk)
{-# INLINEABLE mergeShapeMismatches #-}

representativeMismatches ::
  (DenseKey rep, Eq atom) =>
  CongruenceStalk rep atom ->
  CongruenceStalk rep atom ->
  [CongruenceMismatch rep atom]
representativeMismatches leftStalk rightStalk
  | not (sameCarrier (csCarrier leftStalk) (csCarrier rightStalk)) =
      []
  | otherwise =
      representativeMismatchPair
        (csVisible leftStalk, runtimeRelationRaw (csRelation leftStalk))
        (csVisible rightStalk, runtimeRelationRaw (csRelation rightStalk))
{-# INLINEABLE representativeMismatches #-}

runtimeRelationRaw :: RuntimeCongruenceRelation rep -> EquivalenceRelation rep
runtimeRelationRaw (RuntimeCongruenceRelation relationValue) =
  domainEquivalenceRaw relationValue
{-# INLINE runtimeRelationRaw #-}

runtimeDomainEquivalence ::
  DenseKey rep =>
  GlobalCarrier rep atom ->
  EquivalenceRelation rep ->
  Either (CongruenceConstructionError atom) (RuntimeCongruenceRelation rep)
runtimeDomainEquivalence carrier relationValue =
  first CongruenceRelationFailure $
    join $
      withEquivalenceDomain (carrierDomain carrier) $ \domain ->
        fmap RuntimeCongruenceRelation (mkDomainEquivalence domain relationValue)
{-# INLINEABLE runtimeDomainEquivalence #-}

normalizeRuntimeRelation ::
  RuntimeCongruenceRelation rep ->
  RuntimeCongruenceRelation rep
normalizeRuntimeRelation =
  id
{-# INLINEABLE normalizeRuntimeRelation #-}

applyRuntimeCongruenceMerges ::
  DenseKey rep =>
  [(rep, rep)] ->
  RuntimeCongruenceRelation rep ->
  Either
    (CongruenceConstructionError atom)
    (RuntimeCongruenceRelation rep, IntSet, Int)
applyRuntimeCongruenceMerges repPairs (RuntimeCongruenceRelation relationValue) = do
  (domainRelation, changedKeys, mergeCount) <-
    first CongruenceRelationFailure $
      applyDomainEquivalenceMergesCounted repPairs relationValue
  pure
    ( RuntimeCongruenceRelation domainRelation,
      changedKeys,
      mergeCount
    )
{-# INLINEABLE applyRuntimeCongruenceMerges #-}

validateSourceVisibleImage ::
  DenseKey rep =>
  IntSet ->
  IntMap rep ->
  IntSet ->
  Either (CongruenceConstructionError atom) ()
validateSourceVisibleImage targetVisible carrierMap sourceVisible =
  traverse_ validateSourceKey (IntSet.toAscList sourceVisible)
  where
    validateSourceKey sourceKey =
      case IntMap.lookup sourceKey carrierMap of
        Nothing ->
          Left (CongruenceRestrictionMapMissingCarrierKey sourceKey)
        Just targetKey ->
          let targetKeyId = encodeDenseKey targetKey
           in if IntSet.member targetKeyId targetVisible
                then Right ()
                else Left (CongruenceRestrictionImageOutsideTargetVisible sourceKey targetKeyId)
{-# INLINEABLE validateSourceVisibleImage #-}
