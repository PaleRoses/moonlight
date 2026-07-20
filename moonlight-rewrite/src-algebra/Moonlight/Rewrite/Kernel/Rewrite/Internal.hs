{-# LANGUAGE LambdaCase #-}

module Moonlight.Rewrite.Kernel.Rewrite.Internal
  ( RewriteOrigin (..),
    rewriteOriginAtoms,
    rewriteOriginFoldMap,
    PatternInterface,
    patternInterfaceVariables,
    foldPatternInterface,
    mkPatternInterface,
    PatternRewrite,
    prOrigin,
    prLeft,
    prInterface,
    prRight,
    prDecoration,
    foldPatternRewriteInterface,
    PatternRewriteError (..),
    mkPatternRewrite,
    identityPatternRewrite,
    unitPatternRewriteWithCommonInterface,
    erasePatternRewriteOrigin,
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
  rewriteOriginFoldMap Set.singleton

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
data PatternInterface
  = EmptyPatternInterface
  | SingletonPatternInterface !PatternVar
  | MultiplePatternInterface !(Set PatternVar)

instance Eq PatternInterface where
  leftInterface == rightInterface =
    patternInterfaceVariables leftInterface == patternInterfaceVariables rightInterface

instance Ord PatternInterface where
  compare leftInterface rightInterface =
    compare
      (patternInterfaceVariables leftInterface)
      (patternInterfaceVariables rightInterface)

instance Show PatternInterface where
  showsPrec precedence interface =
    showParen (precedence > 10) $
      showString "PatternInterface {patternInterfaceVariables = "
        . shows (patternInterfaceVariables interface)
        . showString "}"

patternInterfaceVariables :: PatternInterface -> Set PatternVar
patternInterfaceVariables interface =
  case interface of
    EmptyPatternInterface ->
      Set.empty

    SingletonPatternInterface patternVariable ->
      Set.singleton patternVariable

    MultiplePatternInterface patternVariablesValue ->
      patternVariablesValue

foldPatternInterface ::
  (accumulator -> PatternVar -> accumulator) ->
  accumulator ->
  PatternInterface ->
  accumulator
foldPatternInterface combine initialValue interface =
  case interface of
    EmptyPatternInterface ->
      initialValue

    SingletonPatternInterface patternVariable ->
      combine initialValue patternVariable

    MultiplePatternInterface patternVariablesValue ->
      Set.foldl' combine initialValue patternVariablesValue

mkPatternInterface :: Set PatternVar -> PatternInterface
mkPatternInterface patternVariablesValue =
  case Set.lookupMin patternVariablesValue of
    Nothing ->
      EmptyPatternInterface

    Just patternVariable
      | Set.size patternVariablesValue == 1 ->
          SingletonPatternInterface patternVariable

    Just _ ->
      MultiplePatternInterface patternVariablesValue

type PatternRewrite :: Type -> ((Type -> Type) -> Type) -> (Type -> Type) -> Type
data PatternRewrite atom dec f
  = EmptyInterfacePatternRewrite
      !(RewriteOrigin atom)
      !(Pattern f)
      !(Pattern f)
      !(dec f)
  | SingletonInterfacePatternRewrite
      !(RewriteOrigin atom)
      !(Pattern f)
      !PatternVar
      !(Pattern f)
      !(dec f)
  | MultipleInterfacePatternRewrite
      !(RewriteOrigin atom)
      !(Pattern f)
      !(Set PatternVar)
      !(Pattern f)
      !(dec f)

instance
  (Eq atom, Eq (Pattern f), Eq (dec f)) =>
  Eq (PatternRewrite atom dec f)
  where
  leftRewrite == rightRewrite =
    prOrigin leftRewrite == prOrigin rightRewrite
      && prLeft leftRewrite == prLeft rightRewrite
      && prInterface leftRewrite == prInterface rightRewrite
      && prRight leftRewrite == prRight rightRewrite
      && prDecoration leftRewrite == prDecoration rightRewrite

instance
  (Ord atom, Ord (Pattern f), Ord (dec f)) =>
  Ord (PatternRewrite atom dec f)
  where
  compare leftRewrite rightRewrite =
    compare (prOrigin leftRewrite) (prOrigin rightRewrite)
      <> compare (prLeft leftRewrite) (prLeft rightRewrite)
      <> compare (prInterface leftRewrite) (prInterface rightRewrite)
      <> compare (prRight leftRewrite) (prRight rightRewrite)
      <> compare (prDecoration leftRewrite) (prDecoration rightRewrite)

instance
  (Show atom, Show (Pattern f), Show (dec f)) =>
  Show (PatternRewrite atom dec f)
  where
  showsPrec precedence rewriteValue =
    showParen (precedence > 10) $
      showString "PatternRewrite {patternRewriteOriginValue = "
        . shows (prOrigin rewriteValue)
        . showString ", patternRewriteLeftPattern = "
        . shows (prLeft rewriteValue)
        . showString ", patternRewriteInterfaceValue = "
        . shows (prInterface rewriteValue)
        . showString ", patternRewriteRightPattern = "
        . shows (prRight rewriteValue)
        . showString ", patternRewriteDecorationValue = "
        . shows (prDecoration rewriteValue)
        . showString "}"

prOrigin :: PatternRewrite atom dec f -> RewriteOrigin atom
prOrigin rewriteValue =
  case rewriteValue of
    EmptyInterfacePatternRewrite origin _leftPattern _rightPattern _decoration -> origin
    SingletonInterfacePatternRewrite origin _leftPattern _interfaceVariable _rightPattern _decoration -> origin
    MultipleInterfacePatternRewrite origin _leftPattern _interfaceVariables _rightPattern _decoration -> origin

prLeft :: PatternRewrite atom dec f -> Pattern f
prLeft rewriteValue =
  case rewriteValue of
    EmptyInterfacePatternRewrite _origin leftPattern _rightPattern _decoration -> leftPattern
    SingletonInterfacePatternRewrite _origin leftPattern _interfaceVariable _rightPattern _decoration -> leftPattern
    MultipleInterfacePatternRewrite _origin leftPattern _interfaceVariables _rightPattern _decoration -> leftPattern

prInterface :: PatternRewrite atom dec f -> PatternInterface
prInterface rewriteValue =
  case rewriteValue of
    EmptyInterfacePatternRewrite {} -> EmptyPatternInterface
    SingletonInterfacePatternRewrite _origin _leftPattern interfaceVariable _rightPattern _decoration ->
      SingletonPatternInterface interfaceVariable
    MultipleInterfacePatternRewrite _origin _leftPattern interfaceVariables _rightPattern _decoration ->
      MultiplePatternInterface interfaceVariables

prRight :: PatternRewrite atom dec f -> Pattern f
prRight rewriteValue =
  case rewriteValue of
    EmptyInterfacePatternRewrite _origin _leftPattern rightPattern _decoration -> rightPattern
    SingletonInterfacePatternRewrite _origin _leftPattern _interfaceVariable rightPattern _decoration -> rightPattern
    MultipleInterfacePatternRewrite _origin _leftPattern _interfaceVariables rightPattern _decoration -> rightPattern

prDecoration :: PatternRewrite atom dec f -> dec f
prDecoration rewriteValue =
  case rewriteValue of
    EmptyInterfacePatternRewrite _origin _leftPattern _rightPattern decoration -> decoration
    SingletonInterfacePatternRewrite _origin _leftPattern _interfaceVariable _rightPattern decoration -> decoration
    MultipleInterfacePatternRewrite _origin _leftPattern _interfaceVariables _rightPattern decoration -> decoration

foldPatternRewriteInterface ::
  (accumulator -> PatternVar -> accumulator) ->
  accumulator ->
  PatternRewrite atom dec f ->
  accumulator
foldPatternRewriteInterface combine initialValue rewriteValue =
  case rewriteValue of
    EmptyInterfacePatternRewrite {} ->
      initialValue

    SingletonInterfacePatternRewrite _origin _leftPattern interfaceVariable _rightPattern _decoration ->
      combine initialValue interfaceVariable

    MultipleInterfacePatternRewrite _origin _leftPattern interfaceVariables _rightPattern _decoration ->
      Set.foldl' combine initialValue interfaceVariables

patternRewriteFromInterface ::
  RewriteOrigin atom ->
  Pattern f ->
  PatternInterface ->
  Pattern f ->
  dec f ->
  PatternRewrite atom dec f
patternRewriteFromInterface origin leftPattern interface rightPattern decoration =
  case interface of
    EmptyPatternInterface ->
      EmptyInterfacePatternRewrite origin leftPattern rightPattern decoration

    SingletonPatternInterface interfaceVariable ->
      SingletonInterfacePatternRewrite origin leftPattern interfaceVariable rightPattern decoration

    MultiplePatternInterface interfaceVariables ->
      MultipleInterfacePatternRewrite origin leftPattern interfaceVariables rightPattern decoration

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
            ( patternRewriteFromInterface
                origin
                leftPattern
                (mkPatternInterface interfaceVars)
                rightPattern
                decoration
            )
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
  patternRewriteFromInterface
    RewriteIdentity
    patternValue
    (mkPatternInterface (patternVariables patternValue))
    patternValue
    emptyDecoration

unitPatternRewriteWithCommonInterface ::
  Foldable f =>
  RewriteOrigin atom ->
  Pattern f ->
  Pattern f ->
  PatternRewrite atom UnitDecoration f
unitPatternRewriteWithCommonInterface origin leftPattern rightPattern =
  patternRewriteFromInterface
    origin
    leftPattern
    ( mkPatternInterface
        (Set.intersection (patternVariables leftPattern) (patternVariables rightPattern))
    )
    rightPattern
    UnitDecoration

erasePatternRewriteOrigin :: PatternRewrite atom dec f -> PatternRewrite atom dec f
erasePatternRewriteOrigin rewriteValue =
  patternRewriteFromInterface
    RewriteIdentity
    (prLeft rewriteValue)
    (prInterface rewriteValue)
    (prRight rewriteValue)
    (prDecoration rewriteValue)

renamePatternRewrite ::
  (Functor f, Foldable f, RewriteDecoration dec, DecorationConstraint dec f) =>
  PatternRenaming ->
  PatternRewrite atom dec f ->
  PatternRewrite atom dec f
renamePatternRewrite renaming rewriteValue =
  if renaming == mempty
    then rewriteValue
    else
      patternRewriteFromInterface
        (prOrigin rewriteValue)
        (renamePattern renaming (prLeft rewriteValue))
        ( mkPatternInterface
            (renamePatternVariableSet renaming (patternInterfaceVariables (prInterface rewriteValue)))
        )
        (renamePattern renaming (prRight rewriteValue))
        (renameDecoration renaming (prDecoration rewriteValue))

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
