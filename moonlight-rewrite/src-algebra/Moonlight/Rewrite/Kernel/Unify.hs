{-# LANGUAGE LambdaCase #-}

-- | Boundary-overlap algebra for patterns.
-- It owns first-order unification into a canonical apex, with right-side
-- freshening, occurs-check failure, and identity projection pruning for composition.
module Moonlight.Rewrite.Kernel.Unify
  ( PatternUnifier (..),
    UnifierSide (..),
    UnificationError (..),
    unifyPatterns,
    unifyPatternsWithApexFreshFrom,
    unifyPatternEquations,
    applyLeftUnifier,
    applyRightUnifier,
    applyUnifierSide,
    applyUnifier,
    composeUnifiers,
  )
where

import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core
  ( HasConstructorTag (..),
    Pattern (..),
    PatternVar,
    ZipMatch (..),
    patternVarKey,
    patternVariables,
  )
import Moonlight.Core qualified as EGraph
import Moonlight.Rewrite.Kernel.Subst
  ( TermSubst (..),
    resolveTermSubst,
    resolvedTermSubst,
  )

type PatternUnifier :: (Type -> Type) -> Type
data PatternUnifier f = PatternUnifier
  { puLeftMap :: !(Map PatternVar (Pattern f)),
    puRightMap :: !(Map PatternVar (Pattern f)),
    puUnifiedPattern :: !(Pattern f)
  }

deriving stock instance Eq (Pattern f) => Eq (PatternUnifier f)
deriving stock instance Show (Pattern f) => Show (PatternUnifier f)

type UnifierSide :: Type
data UnifierSide
  = UnifierLeft
  | UnifierRight
  deriving stock (Eq, Ord, Show, Read)

type UnificationError :: Type
data UnificationError
  = ConstructorMismatch
  | ArityMismatch
  | OccursCheck !PatternVar
  deriving stock (Eq, Ord, Show)

unifyPatterns ::
  (HasConstructorTag f, ZipMatch f) =>
  Pattern f ->
  Pattern f ->
  Either UnificationError (PatternUnifier f)
unifyPatterns =
  unifyPatternsWithApexFreshFrom Set.empty

unifyPatternsWithApexFreshFrom ::
  (HasConstructorTag f, ZipMatch f) =>
  Set.Set PatternVar ->
  Pattern f ->
  Pattern f ->
  Either UnificationError (PatternUnifier f)
unifyPatternsWithApexFreshFrom forbiddenApexVars leftPattern rightPattern =
  let leftVars =
        patternVariables leftPattern

      rightVars =
        patternVariables rightPattern

      rightFreshening =
        freshenRightVariables leftVars rightVars

      renamedRightPattern =
        renamePatternBy rightFreshening rightPattern
   in do
        solvedBindings <-
          solvePatternEquations [(leftPattern, renamedRightPattern)] IntMap.empty

        let resolveToRaw =
              resolveTermSubst (TermSubst solvedBindings)

            rawApex =
              resolveToRaw leftPattern

            canonicalRenaming =
              canonicalizeVariablesAvoiding forbiddenApexVars rawApex

            projectVar sourceVar =
              renamePatternBy canonicalRenaming (resolveToRaw (PatternVar sourceVar))

            renamedRightVar sourceVar =
              IntMap.findWithDefault sourceVar (patternVarKey sourceVar) rightFreshening

        pure
          PatternUnifier
            { puLeftMap =
                pruneIdentityBindings
                  (Map.fromAscList
                     [ (leftVar, projectVar leftVar)
                       | leftVar <- Set.toAscList leftVars
                     ]),
              puRightMap =
                pruneIdentityBindings
                  (Map.fromAscList
                     [ (rightVar, projectVar (renamedRightVar rightVar))
                       | rightVar <- Set.toAscList rightVars
                     ]),
              puUnifiedPattern =
                renamePatternBy canonicalRenaming rawApex
            }

applyLeftUnifier :: Functor f => PatternUnifier f -> Pattern f -> Pattern f
applyLeftUnifier =
  applyUnifierSide UnifierLeft

applyRightUnifier :: Functor f => PatternUnifier f -> Pattern f -> Pattern f
applyRightUnifier =
  applyUnifierSide UnifierRight

applyUnifierSide ::
  Functor f =>
  UnifierSide ->
  PatternUnifier f ->
  Pattern f ->
  Pattern f
applyUnifierSide side patternUnifier =
  applyProjection
    ( case side of
        UnifierLeft ->
          puLeftMap patternUnifier
        UnifierRight ->
          puRightMap patternUnifier
    )

applyUnifier :: Functor f => PatternUnifier f -> Pattern f -> Pattern f
applyUnifier =
  applyLeftUnifier

composeUnifiers ::
  Functor f =>
  PatternUnifier f ->
  PatternUnifier f ->
  PatternUnifier f
composeUnifiers outer inner =
  PatternUnifier
    { puLeftMap =
        composeProjectionMaps (puLeftMap outer) (puLeftMap inner),
      puRightMap =
        composeProjectionMaps (puRightMap outer) (puRightMap inner),
      puUnifiedPattern =
        applyLeftUnifier outer (puUnifiedPattern inner)
    }

unifyPatternEquations ::
  (HasConstructorTag f, ZipMatch f) =>
  [(Pattern f, Pattern f)] ->
  Either UnificationError (TermSubst f)
unifyPatternEquations equations =
  fmap
    (resolvedTermSubst . TermSubst)
    (solvePatternEquations equations IntMap.empty)

solvePatternEquations ::
  forall f.
  (HasConstructorTag f, ZipMatch f) =>
  [(Pattern f, Pattern f)] ->
  IntMap (Pattern f) ->
  Either UnificationError (IntMap (Pattern f))
solvePatternEquations equations bindings =
  case equations of
    [] ->
      Right bindings
    (leftSide, rightSide) : remainingEquations ->
      case (walkVariable bindings leftSide, walkVariable bindings rightSide) of
        (PatternVar leftVar, PatternVar rightVar)
          | leftVar == rightVar ->
              solvePatternEquations remainingEquations bindings
        (PatternVar leftVar, rightWalked) ->
          bindPatternVar leftVar rightWalked remainingEquations bindings
        (leftWalked, PatternVar rightVar) ->
          bindPatternVar rightVar leftWalked remainingEquations bindings
        (PatternNode leftNode, PatternNode rightNode) ->
          case zipMatch leftNode rightNode of
            Nothing ->
              Left (nodeMismatchError leftNode rightNode)
            Just zippedNode ->
              solvePatternEquations
                (foldr (:) remainingEquations (toList zippedNode))
                bindings

bindPatternVar ::
  (HasConstructorTag f, ZipMatch f) =>
  PatternVar ->
  Pattern f ->
  [(Pattern f, Pattern f)] ->
  IntMap (Pattern f) ->
  Either UnificationError (IntMap (Pattern f))
bindPatternVar patternVar boundTerm remainingEquations bindings
  | occursUnderBindings bindings patternVar boundTerm =
      Left (OccursCheck patternVar)
  | otherwise =
      solvePatternEquations
        remainingEquations
        (IntMap.insert (patternVarKey patternVar) boundTerm bindings)

walkVariable :: IntMap (Pattern f) -> Pattern f -> Pattern f
walkVariable bindings =
  go
  where
    go patternValue =
      case patternValue of
        PatternVar patternVar ->
          case IntMap.lookup (patternVarKey patternVar) bindings of
            Nothing ->
              patternValue
            Just boundTerm ->
              go boundTerm
        PatternNode _ ->
          patternValue

occursUnderBindings ::
  Foldable f =>
  IntMap (Pattern f) ->
  PatternVar ->
  Pattern f ->
  Bool
occursUnderBindings bindings targetVar =
  go IntSet.empty . pure
  where
    targetKey =
      patternVarKey targetVar

    go _ [] =
      False
    go visited (currentTerm : pendingTerms) =
      case currentTerm of
        PatternVar patternVar
          | patternVarKey patternVar == targetKey ->
              True
          | IntSet.member (patternVarKey patternVar) visited ->
              go visited pendingTerms
          | otherwise ->
              let visitedNext =
                    IntSet.insert (patternVarKey patternVar) visited
               in case IntMap.lookup (patternVarKey patternVar) bindings of
                    Nothing ->
                      go visitedNext pendingTerms
                    Just boundTerm ->
                      go visitedNext (boundTerm : pendingTerms)
        PatternNode patternNode ->
          go visited (foldr (:) pendingTerms patternNode)

nodeMismatchError ::
  HasConstructorTag f =>
  f (Pattern f) ->
  f (Pattern f) ->
  UnificationError
nodeMismatchError leftNode rightNode
  | constructorTag leftNode /= constructorTag rightNode =
      ConstructorMismatch
  | otherwise =
      ArityMismatch

freshenRightVariables ::
  Set.Set PatternVar ->
  Set.Set PatternVar ->
  IntMap PatternVar
freshenRightVariables leftVars rightVars
  | Set.disjoint leftVars rightVars =
      IntMap.empty
  | otherwise =
      let offset =
            max (nextFreshKey leftVars) (nextFreshKey rightVars)
       in IntMap.fromAscList
            [ (patternVarKey rightVar, EGraph.mkPatternVar (patternVarKey rightVar + offset))
              | rightVar <- Set.toAscList rightVars
            ]

nextFreshKey :: Set.Set PatternVar -> Int
nextFreshKey patternVars =
  case Set.lookupMax patternVars of
    Nothing ->
      0
    Just patternVar ->
      patternVarKey patternVar + 1

renamePatternBy ::
  Functor f =>
  IntMap PatternVar ->
  Pattern f ->
  Pattern f
renamePatternBy renaming
  | IntMap.null renaming =
      id
  | otherwise =
      go
  where
    go =
      \case
        PatternVar patternVar ->
          PatternVar (IntMap.findWithDefault patternVar (patternVarKey patternVar) renaming)
        PatternNode patternNode ->
          PatternNode (fmap go patternNode)

canonicalizeVariablesAvoiding ::
  Foldable f =>
  Set.Set PatternVar ->
  Pattern f ->
  IntMap PatternVar
canonicalizeVariablesAvoiding forbiddenVars patternValue =
  IntMap.fromAscList
    (zip
       (fmap patternVarKey (Set.toAscList (patternVariables patternValue)))
       (freshPatternVars forbiddenVars))

freshPatternVars :: Set.Set PatternVar -> [PatternVar]
freshPatternVars forbiddenVars =
  filter
    (`Set.notMember` forbiddenVars)
    (fmap EGraph.mkPatternVar [0 ..])

applyProjection ::
  Functor f =>
  Map PatternVar (Pattern f) ->
  Pattern f ->
  Pattern f
applyProjection substitution =
  go IntSet.empty
  where
    go seenVars =
      \case
        PatternVar patternVar ->
          case Map.lookup patternVar substitution of
            Nothing ->
              PatternVar patternVar
            Just replacement
              | IntSet.member (patternVarKey patternVar) seenVars ->
                  PatternVar patternVar
              | otherwise ->
                  go (IntSet.insert (patternVarKey patternVar) seenVars) replacement
        PatternNode patternNode ->
          PatternNode (fmap (go seenVars) patternNode)

composeProjectionMaps ::
  Functor f =>
  Map PatternVar (Pattern f) ->
  Map PatternVar (Pattern f) ->
  Map PatternVar (Pattern f)
composeProjectionMaps outerMap innerMap =
  pruneIdentityBindings
    (Map.union (fmap (applyProjection outerMap) innerMap) outerMap)

pruneIdentityBindings ::
  Map PatternVar (Pattern f) ->
  Map PatternVar (Pattern f)
pruneIdentityBindings =
  Map.filterWithKey
    ( \patternVar ->
        \case
          PatternVar replacementVar ->
            replacementVar /= patternVar
          PatternNode _ ->
            True
    )
