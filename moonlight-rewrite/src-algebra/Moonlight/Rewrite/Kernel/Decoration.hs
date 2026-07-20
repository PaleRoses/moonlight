{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- | Variable-transport algebra for rewrite decorations.
-- It owns pattern renamings and projections plus the 'RewriteDecoration' contract:
-- decoration variables stay bound, while rename, project, and compose failures are typed.
module Moonlight.Rewrite.Kernel.Decoration
  ( PatternRenaming (..),
    emptyPatternRenaming,
    composePatternRenaming,
    applyPatternRenamingVar,
    renamePattern,
    renamePatternVariableSet,
    offsetPatternRenaming,
    canonicalPatternRenaming,
    PatternProjection (..),
    emptyPatternProjection,
    isEmptyPatternProjection,
    projectPattern,
    projectVariableSet,
    DecorationError (..),
    mapDecorationError,
    RewriteDecoration (..),
    UnitDecoration (..),
  )
where

import Data.Kind (Constraint, Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void (Void)
import Moonlight.Core
  ( Pattern (..),
    PatternVar,
    patternVarKey,
  )
import Moonlight.Core qualified as EGraph

type PatternRenaming :: Type
newtype PatternRenaming = PatternRenaming
  { unPatternRenaming :: Map PatternVar PatternVar
  }
  deriving stock (Eq, Ord, Show)

instance Semigroup PatternRenaming where
  leftRenaming <> rightRenaming =
    composePatternRenaming rightRenaming leftRenaming

instance Monoid PatternRenaming where
  mempty =
    emptyPatternRenaming

emptyPatternRenaming :: PatternRenaming
emptyPatternRenaming =
  PatternRenaming Map.empty

composePatternRenaming :: PatternRenaming -> PatternRenaming -> PatternRenaming
composePatternRenaming outerRenaming innerRenaming =
  let innerMap = unPatternRenaming innerRenaming
      outerMap = unPatternRenaming outerRenaming
      keys = Map.keysSet innerMap <> Map.keysSet outerMap
      composedEntries =
        Set.toAscList keys >>= \patternVar ->
          let renamedVar =
                applyPatternRenamingVar outerRenaming
                  (applyPatternRenamingVar innerRenaming patternVar)
           in [(patternVar, renamedVar) | renamedVar /= patternVar]
   in PatternRenaming (Map.fromList composedEntries)

applyPatternRenamingVar :: PatternRenaming -> PatternVar -> PatternVar
applyPatternRenamingVar (PatternRenaming renaming) patternVar =
  Map.findWithDefault patternVar patternVar renaming

renamePattern :: Functor f => PatternRenaming -> Pattern f -> Pattern f
renamePattern renaming =
  \case
    PatternVar patternVar ->
      PatternVar (applyPatternRenamingVar renaming patternVar)
    PatternNode patternNode ->
      PatternNode (fmap (renamePattern renaming) patternNode)

renamePatternVariableSet :: PatternRenaming -> Set PatternVar -> Set PatternVar
renamePatternVariableSet =
  Set.map . applyPatternRenamingVar

offsetPatternRenaming :: Int -> Set PatternVar -> PatternRenaming
offsetPatternRenaming offset patternVars =
  PatternRenaming
    ( Map.fromList
        [ (patternVar, EGraph.mkPatternVar (patternVarKey patternVar + offset))
          | patternVar <- Set.toAscList patternVars
        ]
    )

canonicalPatternRenaming :: Set PatternVar -> PatternRenaming
canonicalPatternRenaming patternVars =
  PatternRenaming
    ( Map.fromList
        (zip (Set.toAscList patternVars) (fmap EGraph.mkPatternVar [0 ..]))
    )

type PatternProjection :: (Type -> Type) -> Type
newtype PatternProjection f = PatternProjection
  { unPatternProjection :: Map PatternVar (Pattern f)
  }

deriving stock instance Eq (Pattern f) => Eq (PatternProjection f)
deriving stock instance Ord (Pattern f) => Ord (PatternProjection f)
deriving stock instance Show (Pattern f) => Show (PatternProjection f)

emptyPatternProjection :: PatternProjection f
emptyPatternProjection =
  PatternProjection Map.empty

isEmptyPatternProjection :: PatternProjection f -> Bool
isEmptyPatternProjection (PatternProjection projection) =
  Map.null projection

projectPattern :: Functor f => PatternProjection f -> Pattern f -> Pattern f
projectPattern (PatternProjection projection) =
  \case
    PatternVar patternVar ->
      Map.findWithDefault (PatternVar patternVar) patternVar projection
    PatternNode patternNode ->
      PatternNode (fmap (projectPattern (PatternProjection projection)) patternNode)

projectVariableSet :: Functor f => PatternProjection f -> Set PatternVar -> Set PatternVar
projectVariableSet projection =
  Set.fromList
    . mapMaybe
      ( \patternVar ->
          case projectPattern projection (PatternVar patternVar) of
            PatternVar projectedVar ->
              Just projectedVar
            PatternNode _ ->
              Nothing
      )
    . Set.toAscList

type DecorationError :: Type -> (Type -> Type) -> Type
data DecorationError obstruction f
  = DecorationUnboundVariables ![PatternVar]
  | DecorationInvalidProjection !obstruction
  deriving stock (Eq, Ord, Show)

mapDecorationError ::
  (leftObstruction -> rightObstruction) ->
  DecorationError leftObstruction f ->
  DecorationError rightObstruction f
mapDecorationError mapObstruction =
  \case
    DecorationUnboundVariables variables ->
      DecorationUnboundVariables variables
    DecorationInvalidProjection obstruction ->
      DecorationInvalidProjection (mapObstruction obstruction)

type RewriteDecoration :: ((Type -> Type) -> Type) -> Constraint
class RewriteDecoration dec where
  type DecorationConstraint dec (f :: Type -> Type) :: Constraint
  type DecorationConstraint dec f = ()
  type DecorationObstruction dec (f :: Type -> Type) :: Type
  type DecorationObstruction dec f = Void

  emptyDecoration :: dec f

  decorationVariables ::
    (Foldable f, DecorationConstraint dec f) =>
    dec f ->
    Set PatternVar

  renameDecoration ::
    (Functor f, Foldable f, DecorationConstraint dec f) =>
    PatternRenaming ->
    dec f ->
    dec f

  projectDecoration ::
    (Functor f, Foldable f, DecorationConstraint dec f) =>
    PatternProjection f ->
    dec f ->
    Either (DecorationError (DecorationObstruction dec f) f) (dec f)

  composeDecoration ::
    DecorationConstraint dec f =>
    dec f ->
    dec f ->
    Either (DecorationError (DecorationObstruction dec f) f) (dec f)

  validateDecoration ::
    (Foldable f, DecorationConstraint dec f) =>
    Set PatternVar ->
    dec f ->
    Either (DecorationError (DecorationObstruction dec f) f) ()
  validateDecoration boundVariables decoration =
    let unboundVariables =
          Set.toAscList
            (Set.difference (decorationVariables decoration) boundVariables)
     in if null unboundVariables
          then Right ()
          else Left (DecorationUnboundVariables unboundVariables)

type UnitDecoration :: (Type -> Type) -> Type
data UnitDecoration f = UnitDecoration
  deriving stock (Eq, Ord, Show)

instance RewriteDecoration UnitDecoration where
  emptyDecoration =
    UnitDecoration

  decorationVariables _ =
    Set.empty

  renameDecoration _ UnitDecoration =
    UnitDecoration

  projectDecoration _ UnitDecoration =
    Right UnitDecoration

  composeDecoration UnitDecoration UnitDecoration =
    Right UnitDecoration
