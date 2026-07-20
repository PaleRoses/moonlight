{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Rewrite.Kernel.SpanModel
  ( SpanModel (..),
    SpanOverlap (..),
    ProjectedInterface (..),
    ComposedInterface (..),
    projectedInterface,
    PatternSpanModel,
    PatternInterfaceLeg (..),
    PatternSpanModelError (..),
    patternInterfaceLeg,
  )
where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core
  ( HasConstructorTag,
    Pattern,
    PatternVar,
    ZipMatch,
    patternVariables,
  )
import Moonlight.Rewrite.Kernel.Decoration
  ( PatternProjection (..),
    projectPattern,
    projectVariableSet,
  )
import Moonlight.Rewrite.Kernel.Rewrite.Internal
  ( PatternInterface,
    mkPatternInterface,
    patternInterfaceVariables,
  )
import Moonlight.Rewrite.Kernel.Unify
  ( PatternUnifier (..),
    UnificationError,
    unifyPatternsWithApexFreshFrom,
  )

type ProjectedInterface :: Type -> Type
data ProjectedInterface model = ProjectedInterface
  { piObject :: !(SpanObject model),
    piInterface :: !(SpanInterface model),
    piLeg :: !(SpanLeg model)
  }

deriving stock instance
  ( Eq (SpanObject model),
    Eq (SpanInterface model),
    Eq (SpanLeg model)
  ) =>
  Eq (ProjectedInterface model)

deriving stock instance
  ( Ord (SpanObject model),
    Ord (SpanInterface model),
    Ord (SpanLeg model)
  ) =>
  Ord (ProjectedInterface model)

deriving stock instance
  ( Show (SpanObject model),
    Show (SpanInterface model),
    Show (SpanLeg model)
  ) =>
  Show (ProjectedInterface model)

type ComposedInterface :: Type -> Type
data ComposedInterface model = ComposedInterface
  { ciInterface :: !(SpanInterface model),
    ciLeftLeg :: !(SpanLeg model),
    ciRightLeg :: !(SpanLeg model)
  }

deriving stock instance
  ( Eq (SpanInterface model),
    Eq (SpanLeg model)
  ) =>
  Eq (ComposedInterface model)

deriving stock instance
  ( Ord (SpanInterface model),
    Ord (SpanLeg model)
  ) =>
  Ord (ComposedInterface model)

deriving stock instance
  ( Show (SpanInterface model),
    Show (SpanLeg model)
  ) =>
  Show (ComposedInterface model)

type SpanModel :: Type -> Constraint
class SpanModel model where
  type SpanObject model :: Type
  type SpanInterface model :: Type
  type SpanLeg model :: Type
  type SpanProjection model :: Type
  type SpanRef model :: Type
  type SpanModelError model :: Type

  spanObjectRefs ::
    Proxy model ->
    SpanObject model ->
    Set (SpanRef model)

  spanInterfaceRefs ::
    Proxy model ->
    SpanInterface model ->
    Set (SpanRef model)

  spanIdentityInterface ::
    Proxy model ->
    SpanObject model ->
    SpanInterface model

  spanIdentityLeg ::
    Proxy model ->
    SpanObject model ->
    SpanLeg model

  spanValidateLeg ::
    Proxy model ->
    SpanInterface model ->
    SpanLeg model ->
    SpanObject model ->
    Either (SpanModelError model) ()

  spanProjectObject ::
    Proxy model ->
    SpanProjection model ->
    SpanObject model ->
    SpanObject model

  spanProjectInterface ::
    Proxy model ->
    SpanProjection model ->
    SpanInterface model ->
    SpanInterface model

  spanProjectLeg ::
    Proxy model ->
    SpanProjection model ->
    SpanLeg model ->
    SpanLeg model

type SpanOverlap :: Type -> Constraint
class SpanModel model => SpanOverlap model where
  type SpanOverlapWitness model :: Type
  type SpanOverlapError model :: Type

  spanOverlapFreshFrom ::
    Proxy model ->
    Set (SpanRef model) ->
    SpanObject model ->
    SpanObject model ->
    Either (SpanOverlapError model) (SpanOverlapWitness model)

  spanOverlapLeftProjection ::
    Proxy model ->
    SpanOverlapWitness model ->
    SpanProjection model

  spanOverlapRightProjection ::
    Proxy model ->
    SpanOverlapWitness model ->
    SpanProjection model

  spanOverlapApex ::
    Proxy model ->
    SpanOverlapWitness model ->
    SpanObject model

  spanComposeInterfaces ::
    Proxy model ->
    SpanOverlapWitness model ->
    ProjectedInterface model ->
    ProjectedInterface model ->
    Either (SpanModelError model) (ComposedInterface model)

projectedInterface ::
  SpanModel model =>
  Proxy model ->
  SpanProjection model ->
  SpanObject model ->
  SpanInterface model ->
  SpanLeg model ->
  ProjectedInterface model
projectedInterface model projection object interface leg =
  ProjectedInterface
    { piObject = spanProjectObject model projection object,
      piInterface = spanProjectInterface model projection interface,
      piLeg = spanProjectLeg model projection leg
    }

type PatternSpanModel :: (Type -> Type) -> Type
data PatternSpanModel f

type PatternInterfaceLeg :: Type
data PatternInterfaceLeg = PatternInterfaceLeg
  deriving stock (Eq, Ord, Show, Read)

patternInterfaceLeg :: PatternInterfaceLeg
patternInterfaceLeg =
  PatternInterfaceLeg

type PatternSpanModelError :: Type
data PatternSpanModelError
  = PatternInterfaceLegNotContained ![PatternVar]
  deriving stock (Eq, Ord, Show)

instance
  (Functor f, Foldable f) =>
  SpanModel (PatternSpanModel f)
  where
  type SpanObject (PatternSpanModel f) = Pattern f
  type SpanInterface (PatternSpanModel f) = PatternInterface
  type SpanLeg (PatternSpanModel f) = PatternInterfaceLeg
  type SpanProjection (PatternSpanModel f) = PatternProjection f
  type SpanRef (PatternSpanModel f) = PatternVar
  type SpanModelError (PatternSpanModel f) = PatternSpanModelError

  spanObjectRefs _ =
    patternVariables

  spanInterfaceRefs _ =
    patternInterfaceVariables

  spanIdentityInterface _ =
    mkPatternInterface . patternVariables

  spanIdentityLeg _ _ =
    PatternInterfaceLeg

  spanValidateLeg _ interface PatternInterfaceLeg object =
    let missingRefs =
          Set.toAscList
            (Set.difference (patternInterfaceVariables interface) (patternVariables object))
     in case missingRefs of
          [] ->
            Right ()
          _ : _ ->
            Left (PatternInterfaceLegNotContained missingRefs)

  spanProjectObject _ =
    projectPattern

  spanProjectInterface _ projection =
    mkPatternInterface
      . projectVariableSet projection
      . patternInterfaceVariables

  spanProjectLeg _ _ PatternInterfaceLeg =
    PatternInterfaceLeg

instance
  (HasConstructorTag f, ZipMatch f) =>
  SpanOverlap (PatternSpanModel f)
  where
  type SpanOverlapWitness (PatternSpanModel f) = PatternUnifier f
  type SpanOverlapError (PatternSpanModel f) = UnificationError

  spanOverlapFreshFrom _ =
    unifyPatternsWithApexFreshFrom

  spanOverlapLeftProjection _ =
    PatternProjection . puLeftMap

  spanOverlapRightProjection _ =
    PatternProjection . puRightMap

  spanOverlapApex _ =
    puUnifiedPattern

  spanComposeInterfaces model _ leftProjected rightProjected = do
    let composedInterface =
          mkPatternInterface
            ( Set.intersection
                (patternInterfaceVariables (piInterface leftProjected))
                (patternInterfaceVariables (piInterface rightProjected))
            )

        composedInterfaceLegs =
          ComposedInterface
            { ciInterface = composedInterface,
              ciLeftLeg = PatternInterfaceLeg,
              ciRightLeg = PatternInterfaceLeg
            }

    spanValidateLeg model composedInterface (ciLeftLeg composedInterfaceLegs) (piObject leftProjected)
    spanValidateLeg model composedInterface (ciRightLeg composedInterfaceLegs) (piObject rightProjected)
    Right composedInterfaceLegs
