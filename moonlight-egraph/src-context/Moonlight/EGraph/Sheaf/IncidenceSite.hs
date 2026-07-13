{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Sheaf.IncidenceSite
  ( EGraphIncidenceTag,
    EGraphIncidenceCategory,
    EGraphIncidenceMorphism,
    EGraphIncidenceObject (..),
    EGraphIncidenceArrow (..),
    ENodeArrowWitness,
    EGraphIncidenceCategoryError (..),
    defaultEGraphIncidenceNerveDepth,
    egraphIncidenceCategoryFromSnapshot,
    egraphIncidenceNerveSite,
    egraphIncidenceNerveSiteFromSnapshot,
    egraphIncidenceMicrosupportEnrichment,
    incidenceCategoryStructuralMorphisms,
    incidenceClassRepresentative,
    incidenceClassesEquivalent,
    incidenceClassObjects,
    eimSource,
    eimTarget,
    eimWitness,
    witnessSource,
    structuralPathSource,
    structuralPathTarget,
    structuralPathArrow,
    composeStructuralPaths,
  )
where

import Data.Foldable (fold, toList)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Category
  ( Category (..),
    FinObjectId,
    FiniteComposableCategory (..),
    chainMorphisms,
    chainStartObject,
  )
import Moonlight.Core
  ( ClassId (..),
    Language,
    MoonlightError,
    classIdKey,
  )
import Moonlight.Core
  ( UnionFind,
    find,
    samePartition,
  )
import Moonlight.EGraph.Pure.Types
  ( ENode (..),
  )
import Moonlight.Sheaf.Obstruction
  ( MicrosupportEnrichment,
    computeNerveMicrosupportEnrichment,
  )
import Moonlight.Sheaf.Operator.GradedComplex
  ( GradedComplex,
  )
import Moonlight.Sheaf.Site
  ( NerveCell,
    NerveSite,
    NerveSiteAlgebra (..),
    mkNerveSite,
  )
import Moonlight.Category.Simplicial
  ( NerveSimplex,
    nerve,
    nerveSimplexChain,
  )
import Numeric.Natural (Natural)

-- | Tag selecting the e-graph incidence category as a generic nerve site source.
type EGraphIncidenceTag :: (Type -> Type) -> Type
data EGraphIncidenceTag f

-- | Objects of the per-context class/enode incidence category.
type EGraphIncidenceObject :: (Type -> Type) -> Type
data EGraphIncidenceObject f
  = IncidenceClassObject !ClassId
  deriving stock (Eq, Ord, Show)

type ENodeArgumentIndex :: Type
newtype ENodeArgumentIndex = ENodeArgumentIndex Int
  deriving stock (Eq, Ord, Show)

-- | A single argument occurrence witnessing an incidence arrow from the argument
-- class to the e-node's target class.
--
-- The constructor is intentionally private: witnesses are derived from an
-- actual 'ENode' argument list, so an invalid argument position cannot be
-- smuggled into the incidence category and silently repaired later.
type ENodeArrowWitness :: (Type -> Type) -> Type
data ENodeArrowWitness f = ENodeArrowWitness
  { eawTargetClass :: !ClassId,
    eawSourceClass :: !ClassId,
    eawENode :: !(ENode f),
    eawArgumentIndex :: !ENodeArgumentIndex
  }

deriving stock instance Language f => Eq (ENodeArrowWitness f)
deriving stock instance Language f => Ord (ENodeArrowWitness f)
deriving stock instance (forall a. Show a => Show (f a)) => Show (ENodeArrowWitness f)

-- | The local witness payload of an incidence morphism.
type EGraphIncidenceArrow :: (Type -> Type) -> Type
data EGraphIncidenceArrow f
  = IncidenceIdentityArrow
  | IncidenceStructuralPathArrow !(NE.NonEmpty (ENodeArrowWitness f))

deriving stock instance Language f => Eq (EGraphIncidenceArrow f)
deriving stock instance Language f => Ord (EGraphIncidenceArrow f)
deriving stock instance (forall a. Show a => Show (f a)) => Show (EGraphIncidenceArrow f)

-- | A category morphism carries source and target explicitly.  The old arrow
-- witness alone could not be a lawful 'Mor': identity and bottom witnesses do
-- not determine their boundaries.
type EGraphIncidenceMorphism :: (Type -> Type) -> Type
data EGraphIncidenceMorphism f = EGraphIncidenceMorphism
  { eimSource :: !(EGraphIncidenceObject f),
    eimTarget :: !(EGraphIncidenceObject f),
    eimWitness :: !(EGraphIncidenceArrow f)
  }

deriving stock instance Language f => Eq (EGraphIncidenceMorphism f)
deriving stock instance Language f => Ord (EGraphIncidenceMorphism f)
deriving stock instance (forall a. Show a => Show (f a)) => Show (EGraphIncidenceMorphism f)

type EGraphIncidenceTwoMorphism :: (Type -> Type) -> Type
data EGraphIncidenceTwoMorphism f = EGraphIncidenceTrivialTwoMorphism
  deriving stock (Eq, Ord, Show)

type EGraphIncidenceCompositor :: (Type -> Type) -> Type
data EGraphIncidenceCompositor f = EGraphIncidenceCompositor
  deriving stock (Eq, Ord, Show)

-- | Finite per-context incidence category built from a direct e-graph snapshot.
type EGraphIncidenceCategory :: (Type -> Type) -> Type
data EGraphIncidenceCategory f = EGraphIncidenceCategory
  { egraphIncidenceClasses :: !(IntMap ClassId),
    egraphIncidenceENodeMembership :: !(IntMap [ENode f]),
    egraphIncidenceStructuralMorphismsBySource :: !(Map (EGraphIncidenceObject f) [EGraphIncidenceMorphism f]),
    egraphIncidenceUnionFind :: !UnionFind
  }

instance Language f => Eq (EGraphIncidenceCategory f) where
  leftCategory == rightCategory =
    egraphIncidenceClasses leftCategory == egraphIncidenceClasses rightCategory
      && egraphIncidenceENodeMembership leftCategory == egraphIncidenceENodeMembership rightCategory
      && egraphIncidenceStructuralMorphismsBySource leftCategory == egraphIncidenceStructuralMorphismsBySource rightCategory
      && samePartition (egraphIncidenceUnionFind leftCategory) (egraphIncidenceUnionFind rightCategory)
deriving stock instance (forall a. Show a => Show (f a)) => Show (EGraphIncidenceCategory f)

type EGraphIncidenceCategoryError :: (Type -> Type) -> Type
data EGraphIncidenceCategoryError f
  = IncidenceUnknownClass !ClassId
  | IncidenceUnknownObject !(EGraphIncidenceObject f)
  | IncidenceNonComposable !(EGraphIncidenceObject f) !(EGraphIncidenceObject f)
  deriving stock (Eq, Show)

defaultEGraphIncidenceNerveDepth :: Natural
defaultEGraphIncidenceNerveDepth = 2

egraphIncidenceCategoryFromSnapshot ::
  Language f =>
  UnionFind ->
  IntMap [ENode f] ->
  Either (EGraphIncidenceCategoryError f) (EGraphIncidenceCategory f)
egraphIncidenceCategoryFromSnapshot unionFind membership =
  case foldMap (unknownENodeArguments classes) (fold membership) of
    unknownClass : _ ->
      Left (IncidenceUnknownClass unknownClass)
    [] ->
      Right
        EGraphIncidenceCategory
          { egraphIncidenceClasses = classes,
            egraphIncidenceENodeMembership = membership,
            egraphIncidenceStructuralMorphismsBySource = morphismsBySource structuralMorphisms,
            egraphIncidenceUnionFind = unionFind
          }
  where
    classes = IntMap.mapWithKey (\classKey _ -> ClassId classKey) membership
    structuralMorphisms = fmap argumentMorphism (argumentWitnesses membership)

egraphIncidenceNerveSite ::
  forall f.
  Language f =>
  EGraphIncidenceCategory f ->
  Natural ->
  NerveSite (EGraphIncidenceTag f)
egraphIncidenceNerveSite =
  mkNerveSite @(EGraphIncidenceTag f)

egraphIncidenceNerveSiteFromSnapshot ::
  Language f =>
  UnionFind ->
  IntMap [ENode f] ->
  Natural ->
  Either (EGraphIncidenceCategoryError f) (NerveSite (EGraphIncidenceTag f))
egraphIncidenceNerveSiteFromSnapshot unionFind membership depthValue =
  fmap (`egraphIncidenceNerveSite` depthValue) (egraphIncidenceCategoryFromSnapshot unionFind membership)

egraphIncidenceMicrosupportEnrichment ::
  Ord node =>
  (FinObjectId -> Maybe node) ->
  NerveSite (EGraphIncidenceTag f) ->
  GradedComplex (NerveCell (EGraphIncidenceTag f)) Int ->
  Either MoonlightError (MicrosupportEnrichment node)
egraphIncidenceMicrosupportEnrichment =
  computeNerveMicrosupportEnrichment

incidenceCategoryStructuralMorphisms :: EGraphIncidenceCategory f -> [EGraphIncidenceMorphism f]
incidenceCategoryStructuralMorphisms =
  fold . Map.elems . egraphIncidenceStructuralMorphismsBySource

incidenceClassObjects :: EGraphIncidenceCategory f -> [ClassId]
incidenceClassObjects =
  IntMap.elems . egraphIncidenceClasses

incidenceClassRepresentative ::
  ClassId ->
  EGraphIncidenceCategory f ->
  Either (EGraphIncidenceCategoryError f) ClassId
incidenceClassRepresentative classId categoryValue =
  let classKey = classIdKey classId
      representative =
        fst (find classId (egraphIncidenceUnionFind categoryValue))
      representativeKey =
        classIdKey representative
   in if IntMap.member classKey (egraphIncidenceClasses categoryValue)
        || IntMap.member representativeKey (egraphIncidenceClasses categoryValue)
        then Right representative
        else Left (IncidenceUnknownClass classId)

incidenceClassesEquivalent ::
  ClassId ->
  ClassId ->
  EGraphIncidenceCategory f ->
  Either (EGraphIncidenceCategoryError f) Bool
incidenceClassesEquivalent leftClassId rightClassId categoryValue =
  (==)
    <$> incidenceClassRepresentative leftClassId categoryValue
    <*> incidenceClassRepresentative rightClassId categoryValue

instance Category (EGraphIncidenceCategory f) where
  type Ob (EGraphIncidenceCategory f) = EGraphIncidenceObject f
  type Mor (EGraphIncidenceCategory f) = EGraphIncidenceMorphism f
  type TwoMor (EGraphIncidenceCategory f) = EGraphIncidenceTwoMorphism f
  type Compositor (EGraphIncidenceCategory f) = EGraphIncidenceCompositor f
  type CategoryError (EGraphIncidenceCategory f) = EGraphIncidenceCategoryError f

  identity categoryValue objectValue =
    ensureIncidenceObject categoryValue objectValue
      *> Right
        EGraphIncidenceMorphism
          { eimSource = objectValue,
            eimTarget = objectValue,
            eimWitness = IncidenceIdentityArrow
          }

  compose categoryValue outerMorphism innerMorphism =
    ensureIncidenceMorphismEndpoints categoryValue outerMorphism
      *> ensureIncidenceMorphismEndpoints categoryValue innerMorphism
      *> maybe
        (Left (IncidenceNonComposable (eimTarget innerMorphism) (eimSource outerMorphism)))
        (\composite -> Right (composite, EGraphIncidenceCompositor))
        (composeIncidenceMorphisms outerMorphism innerMorphism)

  source categoryValue morphismValue =
    eimSource morphismValue <$ ensureIncidenceMorphismEndpoints categoryValue morphismValue

  target categoryValue morphismValue =
    eimTarget morphismValue <$ ensureIncidenceMorphismEndpoints categoryValue morphismValue

instance Language f => FiniteComposableCategory (EGraphIncidenceCategory f) where
  enumerateObjects =
    fmap IncidenceClassObject . incidenceClassObjects

  enumerateMorphisms categoryValue =
    incidenceCategoryStructuralMorphisms categoryValue
      <> foldMap
        (either (const []) pure . identity @(EGraphIncidenceCategory f) categoryValue)
        (enumerateObjects categoryValue)

  enumerateMorphismsFrom categoryValue sourceObject =
    Map.findWithDefault [] sourceObject (egraphIncidenceStructuralMorphismsBySource categoryValue)
      <> either
        (const [])
        pure
        (identity @(EGraphIncidenceCategory f) categoryValue sourceObject)

instance Language f => NerveSiteAlgebra (EGraphIncidenceTag f) where
  type NerveCategory (EGraphIncidenceTag f) = EGraphIncidenceCategory f
  type NerveSource (EGraphIncidenceTag f) = EGraphIncidenceObject f
  type NerveMorphism (EGraphIncidenceTag f) = EGraphIncidenceMorphism f

  buildSiteNerve = nerve
  simplexSourceValue = incidenceSimplexSource
  simplexMorphismChain = incidenceSimplexMorphisms

incidenceSimplexSource :: NerveSimplex (EGraphIncidenceCategory f) -> EGraphIncidenceObject f
incidenceSimplexSource =
  chainStartObject . nerveSimplexChain

incidenceSimplexMorphisms :: NerveSimplex (EGraphIncidenceCategory f) -> [EGraphIncidenceMorphism f]
incidenceSimplexMorphisms =
  chainMorphisms . nerveSimplexChain

composeIncidenceMorphisms ::
  EGraphIncidenceMorphism f ->
  EGraphIncidenceMorphism f ->
  Maybe (EGraphIncidenceMorphism f)
composeIncidenceMorphisms outerMorphism innerMorphism
  | eimTarget innerMorphism /= eimSource outerMorphism =
      Nothing
  | IncidenceIdentityArrow <- eimWitness outerMorphism =
      Just innerMorphism
  | IncidenceIdentityArrow <- eimWitness innerMorphism =
      Just outerMorphism
  | Just outerPath <- arrowPath (eimWitness outerMorphism),
    Just innerPath <- arrowPath (eimWitness innerMorphism) = do
      compositePath <- composeStructuralPaths outerPath innerPath
      Just
        EGraphIncidenceMorphism
          { eimSource = eimSource innerMorphism,
            eimTarget = eimTarget outerMorphism,
            eimWitness = structuralPathArrow compositePath
          }
  | otherwise =
      Nothing

ensureIncidenceMorphismEndpoints ::
  EGraphIncidenceCategory f ->
  EGraphIncidenceMorphism f ->
  Either (EGraphIncidenceCategoryError f) ()
ensureIncidenceMorphismEndpoints categoryValue morphismValue =
  ensureIncidenceObject categoryValue (eimSource morphismValue)
    *> ensureIncidenceObject categoryValue (eimTarget morphismValue)

ensureIncidenceObject ::
  EGraphIncidenceCategory f ->
  EGraphIncidenceObject f ->
  Either (EGraphIncidenceCategoryError f) ()
ensureIncidenceObject categoryValue objectValue =
  if incidenceObjectDeclared categoryValue objectValue
    then Right ()
    else Left (IncidenceUnknownObject objectValue)

incidenceObjectDeclared ::
  EGraphIncidenceCategory f ->
  EGraphIncidenceObject f ->
  Bool
incidenceObjectDeclared categoryValue (IncidenceClassObject classId) =
  IntMap.member (classIdKey classId) (egraphIncidenceClasses categoryValue)

argumentMorphism :: ENodeArrowWitness f -> EGraphIncidenceMorphism f
argumentMorphism witness =
  EGraphIncidenceMorphism
    { eimSource = witnessSource witness,
      eimTarget = IncidenceClassObject (eawTargetClass witness),
      eimWitness = structuralPathArrow (witness NE.:| [])
    }

arrowPath :: EGraphIncidenceArrow f -> Maybe (NE.NonEmpty (ENodeArrowWitness f))
arrowPath arrowValue =
  case arrowValue of
    IncidenceStructuralPathArrow witnesses ->
      Just witnesses
    IncidenceIdentityArrow ->
      Nothing

structuralPathSource :: NE.NonEmpty (ENodeArrowWitness f) -> EGraphIncidenceObject f
structuralPathSource =
  witnessSource . NE.head

structuralPathTarget :: NE.NonEmpty (ENodeArrowWitness f) -> EGraphIncidenceObject f
structuralPathTarget =
  IncidenceClassObject . eawTargetClass . NE.last

structuralPathArrow :: NE.NonEmpty (ENodeArrowWitness f) -> EGraphIncidenceArrow f
structuralPathArrow =
  IncidenceStructuralPathArrow

composeStructuralPaths ::
  NE.NonEmpty (ENodeArrowWitness f) ->
  NE.NonEmpty (ENodeArrowWitness f) ->
  Maybe (NE.NonEmpty (ENodeArrowWitness f))
composeStructuralPaths outerPath innerPath
  | structuralPathWellFormed outerPath
      && structuralPathWellFormed innerPath
      && structuralPathTarget innerPath == structuralPathSource outerPath =
      Just (innerPath <> outerPath)
  | otherwise =
      Nothing

structuralPathWellFormed ::
  NE.NonEmpty (ENodeArrowWitness f) ->
  Bool
structuralPathWellFormed witnesses =
  and
    ( zipWith
        ( \leftWitness rightWitness ->
            IncidenceClassObject (eawTargetClass leftWitness) == witnessSource rightWitness
        )
        (NE.toList witnesses)
        (NE.tail witnesses)
    )

witnessSource :: ENodeArrowWitness f -> EGraphIncidenceObject f
witnessSource =
  IncidenceClassObject . eawSourceClass

argumentWitnesses :: Language f => IntMap [ENode f] -> [ENodeArrowWitness f]
argumentWitnesses membership =
  IntMap.toList membership >>= uncurry witnessesForMembership

witnessesForMembership :: Language f => Int -> [ENode f] -> [ENodeArrowWitness f]
witnessesForMembership targetKey =
  foldMap (witnessesForENode (ClassId targetKey))

witnessesForENode :: Language f => ClassId -> ENode f -> [ENodeArrowWitness f]
witnessesForENode targetClass enode@(ENode nodeValue) =
  fmap
    ( \(position, sourceClass) ->
        ENodeArrowWitness
          { eawTargetClass = targetClass,
            eawSourceClass = sourceClass,
            eawENode = enode,
            eawArgumentIndex = ENodeArgumentIndex position
          }
    )
    (zip [0 :: Int ..] (toList nodeValue))

unknownENodeArguments :: Language f => IntMap ClassId -> ENode f -> [ClassId]
unknownENodeArguments classes (ENode nodeValue) =
  filter
    (\classId -> IntMap.notMember (classIdKey classId) classes)
    (toList nodeValue)

morphismsBySource :: [EGraphIncidenceMorphism f] -> Map (EGraphIncidenceObject f) [EGraphIncidenceMorphism f]
morphismsBySource =
  Map.fromListWith (<>) . fmap (\morphismValue -> (eimSource morphismValue, [morphismValue]))
