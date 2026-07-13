{-# LANGUAGE LambdaCase #-}

module Moonlight.Rewrite.Kernel.Rewrite.Internal
  ( RewriteOrigin (..),
    rewriteOriginAtoms,
    rewriteOriginFoldMap,
    PatternInterface,
    patternInterfaceVariables,
    mkPatternInterface,
    PatternRewrite,
    prOrigin,
    prLeft,
    prInterface,
    prRight,
    prDecoration,
    PatternRewriteError (..),
    mkPatternRewrite,
    identityPatternRewrite,
    unitPatternRewriteWithCommonInterface,
    renamePatternRewrite,
    canonicalizePatternRewrite,
    samePatternRewriteShape,
    patternRewriteLeftVars,
    patternRewriteRightVars,
    patternRewriteDeletedVars,
    patternRewriteCreatedVars,
    allPatternRewriteVariables,
    isInvertiblePatternRewrite,
    isLeftLinearPatternRewrite,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core
  ( Pattern (..),
    PatternVar,
    patternVariables,
  )
import Moonlight.Rewrite.Kernel.Decoration
  ( DecorationError,
    DecorationObstruction,
    PatternRenaming,
    RewriteDecoration (..),
    UnitDecoration (..),
    canonicalPatternRenaming,
    renameDecoration,
    renamePattern,
    renamePatternVariableSet,
  )

type RewriteOrigin :: Type -> Type
data RewriteOrigin atom
  = RewriteIdentity
  | RewriteAtomic !atom
  | RewriteComposite !(RewriteOrigin atom) !(RewriteOrigin atom)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

rewriteOriginAtoms :: Ord atom => RewriteOrigin atom -> Set atom
rewriteOriginAtoms =
  \case
    RewriteIdentity ->
      Set.empty
    RewriteAtomic atom ->
      Set.singleton atom
    RewriteComposite leftOrigin rightOrigin ->
      rewriteOriginAtoms leftOrigin <> rewriteOriginAtoms rightOrigin

rewriteOriginFoldMap :: Monoid m => (atom -> m) -> RewriteOrigin atom -> m
rewriteOriginFoldMap project =
  \case
    RewriteIdentity ->
      mempty
    RewriteAtomic atom ->
      project atom
    RewriteComposite leftOrigin rightOrigin ->
      rewriteOriginFoldMap project leftOrigin
        <> rewriteOriginFoldMap project rightOrigin

type PatternInterface :: Type
newtype PatternInterface = PatternInterface
  { patternInterfaceVariables :: Set PatternVar
  }
  deriving stock (Eq, Ord, Show)

mkPatternInterface :: Set PatternVar -> PatternInterface
mkPatternInterface =
  PatternInterface

type PatternRewrite :: Type -> ((Type -> Type) -> Type) -> (Type -> Type) -> Type
data PatternRewrite atom dec f = PatternRewrite
  { prOrigin :: !(RewriteOrigin atom),
    prLeft :: !(Pattern f),
    prInterface :: !PatternInterface,
    prRight :: !(Pattern f),
    prDecoration :: !(dec f)
  }

deriving stock instance
  (Eq atom, Eq (Pattern f), Eq (dec f)) =>
  Eq (PatternRewrite atom dec f)

deriving stock instance
  (Ord atom, Ord (Pattern f), Ord (dec f)) =>
  Ord (PatternRewrite atom dec f)

deriving stock instance
  (Show atom, Show (Pattern f), Show (dec f)) =>
  Show (PatternRewrite atom dec f)

type PatternRewriteError :: ((Type -> Type) -> Type) -> (Type -> Type) -> Type
data PatternRewriteError dec f
  = RewriteInterfaceNotInLeft ![PatternVar]
  | RewriteInterfaceNotInRight ![PatternVar]
  | RewriteInterfaceNotInBoth ![PatternVar] ![PatternVar]
  | RewriteInvalidDecoration !(DecorationError (DecorationObstruction dec f) f)

deriving stock instance
  Eq (DecorationObstruction dec f) =>
  Eq (PatternRewriteError dec f)

deriving stock instance
  Ord (DecorationObstruction dec f) =>
  Ord (PatternRewriteError dec f)

deriving stock instance
  Show (DecorationObstruction dec f) =>
  Show (PatternRewriteError dec f)

mkPatternRewrite ::
  (Foldable f, RewriteDecoration dec, DecorationConstraint dec f) =>
  RewriteOrigin atom ->
  Pattern f ->
  Set PatternVar ->
  Pattern f ->
  dec f ->
  Either (PatternRewriteError dec f) (PatternRewrite atom dec f)
mkPatternRewrite origin leftPattern interfaceVars rightPattern decoration =
  let leftVars = patternVariables leftPattern
      rightVars = patternVariables rightPattern
      missingLeftVars =
        Set.toAscList (Set.difference interfaceVars leftVars)
      missingRightVars =
        Set.toAscList (Set.difference interfaceVars rightVars)
   in case (missingLeftVars, missingRightVars) of
        ([], []) -> do
          first RewriteInvalidDecoration (validateDecoration leftVars decoration)
          Right
            PatternRewrite
              { prOrigin = origin,
                prLeft = leftPattern,
                prInterface = PatternInterface interfaceVars,
                prRight = rightPattern,
                prDecoration = decoration
              }
        (_ : _, _ : _) ->
          Left (RewriteInterfaceNotInBoth missingLeftVars missingRightVars)
        (_ : _, []) ->
          Left (RewriteInterfaceNotInLeft missingLeftVars)
        ([], _ : _) ->
          Left (RewriteInterfaceNotInRight missingRightVars)

identityPatternRewrite ::
  (Foldable f, RewriteDecoration dec) =>
  Pattern f ->
  PatternRewrite atom dec f
identityPatternRewrite patternValue =
  PatternRewrite
    { prOrigin = RewriteIdentity,
      prLeft = patternValue,
      prInterface = PatternInterface (patternVariables patternValue),
      prRight = patternValue,
      prDecoration = emptyDecoration
    }

unitPatternRewriteWithCommonInterface ::
  Foldable f =>
  RewriteOrigin atom ->
  Pattern f ->
  Pattern f ->
  PatternRewrite atom UnitDecoration f
unitPatternRewriteWithCommonInterface origin leftPattern rightPattern =
  PatternRewrite
    { prOrigin = origin,
      prLeft = leftPattern,
      prInterface =
        PatternInterface
          (Set.intersection (patternVariables leftPattern) (patternVariables rightPattern)),
      prRight = rightPattern,
      prDecoration = UnitDecoration
    }

renamePatternRewrite ::
  (Functor f, Foldable f, RewriteDecoration dec, DecorationConstraint dec f) =>
  PatternRenaming ->
  PatternRewrite atom dec f ->
  PatternRewrite atom dec f
renamePatternRewrite renaming rewriteValue =
  if renaming == mempty
    then rewriteValue
    else
      rewriteValue
        { prLeft = renamePattern renaming (prLeft rewriteValue),
          prInterface =
            PatternInterface
              (renamePatternVariableSet renaming (patternInterfaceVariables (prInterface rewriteValue))),
          prRight = renamePattern renaming (prRight rewriteValue),
          prDecoration = renameDecoration renaming (prDecoration rewriteValue)
        }

canonicalizePatternRewrite ::
  (Functor f, Foldable f, RewriteDecoration dec, DecorationConstraint dec f) =>
  PatternRewrite atom dec f ->
  PatternRewrite atom dec f
canonicalizePatternRewrite rewriteValue =
  renamePatternRewrite
    (canonicalPatternRenaming (allPatternRewriteVariables rewriteValue))
    rewriteValue

samePatternRewriteShape ::
  (Eq (Pattern f), Eq (dec f)) =>
  PatternRewrite atom dec f ->
  PatternRewrite atom dec f ->
  Bool
samePatternRewriteShape leftRewrite rightRewrite =
  prLeft leftRewrite == prLeft rightRewrite
    && prInterface leftRewrite == prInterface rightRewrite
    && prRight leftRewrite == prRight rightRewrite
    && prDecoration leftRewrite == prDecoration rightRewrite

patternRewriteLeftVars :: Foldable f => PatternRewrite atom dec f -> Set PatternVar
patternRewriteLeftVars =
  patternVariables . prLeft

patternRewriteRightVars :: Foldable f => PatternRewrite atom dec f -> Set PatternVar
patternRewriteRightVars =
  patternVariables . prRight

patternRewriteDeletedVars :: Foldable f => PatternRewrite atom dec f -> Set PatternVar
patternRewriteDeletedVars rewriteValue =
  Set.difference
    (patternRewriteLeftVars rewriteValue)
    (patternInterfaceVariables (prInterface rewriteValue))

patternRewriteCreatedVars :: Foldable f => PatternRewrite atom dec f -> Set PatternVar
patternRewriteCreatedVars rewriteValue =
  Set.difference
    (patternRewriteRightVars rewriteValue)
    (patternInterfaceVariables (prInterface rewriteValue))

allPatternRewriteVariables ::
  (Foldable f, RewriteDecoration dec, DecorationConstraint dec f) =>
  PatternRewrite atom dec f ->
  Set PatternVar
allPatternRewriteVariables rewriteValue =
  patternRewriteLeftVars rewriteValue
    <> patternRewriteRightVars rewriteValue
    <> patternInterfaceVariables (prInterface rewriteValue)
    <> decorationVariables (prDecoration rewriteValue)

isInvertiblePatternRewrite :: Foldable f => PatternRewrite atom dec f -> Bool
isInvertiblePatternRewrite rewriteValue =
  let leftVars = patternRewriteLeftVars rewriteValue
      rightVars = patternRewriteRightVars rewriteValue
      interfaceVars = patternInterfaceVariables (prInterface rewriteValue)
   in interfaceVars == leftVars && interfaceVars == rightVars

isLeftLinearPatternRewrite :: Foldable f => PatternRewrite atom dec f -> Bool
isLeftLinearPatternRewrite =
  all (== 1) . Map.elems . patternMultiplicity . prLeft

patternMultiplicity :: Foldable f => Pattern f -> Map.Map PatternVar Int
patternMultiplicity =
  \case
    PatternVar patternVar ->
      Map.singleton patternVar 1
    PatternNode patternNode ->
      foldr
        (Map.unionWith (+) . patternMultiplicity)
        Map.empty
        patternNode
