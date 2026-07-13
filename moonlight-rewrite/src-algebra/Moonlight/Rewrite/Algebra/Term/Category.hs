{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Rewrite.Algebra.Term.Category
  ( TermCat (..),
    TermOb (..),
    TermMor,
    termMorSource,
    termMorTarget,
    termMorSubst,
    TermTwoMor (..),
    TermCompositor (..),
    termMor,
    termIdentity,
    matchPattern,
    relativeAntiUnify,
  )
where

import Control.Monad.State.Strict (State, runState, state)
import Data.Foldable (foldlM, toList)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Category
  ( Category (..),
    HasPullbacks (..),
    HasPushouts (..),
    composeMor,
  )
import Moonlight.Core
  ( HasConstructorTag,
    Language,
    Pattern (..),
    PatternVar,
    ZipMatch,
    patternVarKey,
    patternVariables,
    zipSameNodeShape,
  )
import Moonlight.Core qualified as EGraph
import Moonlight.Rewrite.Kernel.Subst
  ( TermSubst (..),
    applyTermSubst,
    composeTermSubst,
    emptyTermSubst,
    restrictTermSubst,
    termSubstFromList,
  )
import Moonlight.Rewrite.Kernel.Unify
  ( unifyPatternEquations,
  )

type TermCat :: (Type -> Type) -> Type
data TermCat f = TermCat

type TermOb :: (Type -> Type) -> Type
newtype TermOb f = TermOb
  { termObPattern :: Pattern f
  }

deriving stock instance Eq (Pattern f) => Eq (TermOb f)
deriving stock instance Ord (Pattern f) => Ord (TermOb f)
deriving stock instance Show (Pattern f) => Show (TermOb f)

type TermMor :: (Type -> Type) -> Type
data TermMor f = TermMor
  { termMorSource :: !(Pattern f),
    termMorTarget :: !(Pattern f),
    termMorSubst :: !(TermSubst f)
  }

deriving stock instance Eq (Pattern f) => Eq (TermMor f)
deriving stock instance Ord (Pattern f) => Ord (TermMor f)
deriving stock instance Show (Pattern f) => Show (TermMor f)

type TermTwoMor :: (Type -> Type) -> Type
newtype TermTwoMor f = TermTwoMor ()

type TermCompositor :: (Type -> Type) -> Type
newtype TermCompositor f = TermCompositor ()

termMor :: (Functor f, Foldable f) => Pattern f -> TermSubst f -> TermMor f
termMor sourcePattern rawSubst =
  let normalSubst =
        normalizeTermSubst (patternVariableKeys sourcePattern) rawSubst
   in TermMor
        { termMorSource = sourcePattern,
          termMorTarget = applyTermSubst normalSubst sourcePattern,
          termMorSubst = normalSubst
        }

termIdentity :: Pattern f -> TermMor f
termIdentity patternValue =
  TermMor
    { termMorSource = patternValue,
      termMorTarget = patternValue,
      termMorSubst = emptyTermSubst
    }

instance Language f => Category (TermCat f) where
  type Ob (TermCat f) = TermOb f
  type Mor (TermCat f) = TermMor f
  type TwoMor (TermCat f) = TermTwoMor f
  type Compositor (TermCat f) = TermCompositor f

  identity _ =
    Right . termIdentity . termObPattern

  compose _ outer inner
    | termMorTarget inner == termMorSource outer =
        Right
          ( TermMor
              { termMorSource = termMorSource inner,
                termMorTarget = termMorTarget outer,
                termMorSubst =
                  normalizeTermSubst
                    (patternVariableKeys (termMorSource inner))
                    (composeTermSubst (termMorSubst outer) (termMorSubst inner))
              },
            TermCompositor ()
          )
    | otherwise =
        Left ()

  source _ =
    Right . TermOb . termMorSource

  target _ =
    Right . TermOb . termMorTarget

instance Language f => HasPullbacks (TermCat f) where
  pullback _ leftBase rightBase
    | termMorTarget leftBase /= termMorTarget rightBase =
        Nothing
    | otherwise =
        let (apexPattern, leftBindings, rightBindings) =
              relativeAntiUnify (termMorSource leftBase) (termMorSource rightBase)
         in Just
              ( TermOb apexPattern,
                termMor apexPattern (termSubstFromList leftBindings),
                termMor apexPattern (termSubstFromList rightBindings)
              )

  pullbackMediator categoryValue leftBase rightBase coneLeft coneRight = do
    (TermOb apexPattern, projLeft, projRight) <-
      pullback categoryValue leftBase rightBase

    mediatorSubst <-
      matchPattern (termMorSource coneLeft) apexPattern

    let mediator =
          termMor (termMorSource coneLeft) mediatorSubst

    composedLeft <-
      either (const Nothing) Just (composeMor @(TermCat f) categoryValue projLeft mediator)

    composedRight <-
      either (const Nothing) Just (composeMor @(TermCat f) categoryValue projRight mediator)

    if composedLeft == coneLeft && composedRight == coneRight
      then Just mediator
      else Nothing

instance (HasConstructorTag f, ZipMatch f) => HasPushouts (TermCat f) where
  pushout _ leftLeg rightLeg
    | termMorSource leftLeg /= termMorSource rightLeg =
        Nothing
    | otherwise =
        let spanVars =
              Set.toAscList (patternVariables (termMorSource leftLeg))

            leftTarget =
              termMorTarget leftLeg

            rightTarget =
              termMorTarget rightLeg

            rightRenaming =
              disjointRenaming leftTarget rightTarget

            renameRight =
              applyTermSubst rightRenaming

            equations =
              [ ( applyTermSubst (termMorSubst leftLeg) (PatternVar spanVar),
                  renameRight (applyTermSubst (termMorSubst rightLeg) (PatternVar spanVar))
                )
                | spanVar <- spanVars
              ]
         in case unifyPatternEquations equations of
              Left _ ->
                Nothing
              Right unifyingSubst ->
                Just
                  ( TermOb
                      (applyTermSubst unifyingSubst leftTarget),
                    termMor leftTarget unifyingSubst,
                    termMor rightTarget (composeTermSubst unifyingSubst rightRenaming)
                  )

matchPattern ::
  forall f.
  Language f =>
  Pattern f ->
  Pattern f ->
  Maybe (TermSubst f)
matchPattern generalPattern targetPattern =
  TermSubst <$> go generalPattern targetPattern IntMap.empty
  where
    go ::
      Pattern f ->
      Pattern f ->
      IntMap.IntMap (Pattern f) ->
      Maybe (IntMap.IntMap (Pattern f))
    go generalValue targetValue acc =
      case (generalValue, targetValue) of
        (PatternVar generalVar, _) ->
          case IntMap.lookup (patternVarKey generalVar) acc of
            Nothing ->
              Just (IntMap.insert (patternVarKey generalVar) targetValue acc)
            Just existingBinding
              | existingBinding == targetValue ->
                  Just acc
              | otherwise ->
                  Nothing
        (PatternNode _, PatternVar _) ->
          Nothing
        (PatternNode generalNode, PatternNode targetNode) ->
          zipSameNodeShape generalNode targetNode
            >>= \zippedNode ->
              foldlM
                (\currentAcc (generalChild, targetChild) -> go generalChild targetChild currentAcc)
                acc
                (toList zippedNode)

relativeAntiUnify ::
  forall f.
  Language f =>
  Pattern f ->
  Pattern f ->
  (Pattern f, [(PatternVar, Pattern f)], [(PatternVar, Pattern f)])
relativeAntiUnify leftPattern rightPattern =
  let (apexPattern, finalMemo) =
        runState (go leftPattern rightPattern) Map.empty

      orderedEntries =
        [ (apexVar, leftValue, rightValue)
          | ((leftValue, rightValue), apexVar) <- Map.toList finalMemo
        ]
   in ( apexPattern,
        [(apexVar, leftValue) | (apexVar, leftValue, _) <- orderedEntries],
        [(apexVar, rightValue) | (apexVar, _, rightValue) <- orderedEntries]
      )
  where
    go ::
      Pattern f ->
      Pattern f ->
      State (Map (Pattern f, Pattern f) PatternVar) (Pattern f)
    go leftValue rightValue =
      case (leftValue, rightValue) of
        (PatternNode leftNode, PatternNode rightNode) ->
          case zipSameNodeShape leftNode rightNode of
            Nothing ->
              generalizationVar leftValue rightValue
            Just zippedNode ->
              PatternNode
                <$> traverse
                  (uncurry go)
                  zippedNode
        _ ->
          generalizationVar leftValue rightValue

    generalizationVar ::
      Pattern f ->
      Pattern f ->
      State (Map (Pattern f, Pattern f) PatternVar) (Pattern f)
    generalizationVar leftValue rightValue =
      state
        ( \memo ->
            case Map.lookup (leftValue, rightValue) memo of
              Just apexVar ->
                (PatternVar apexVar, memo)
              Nothing ->
                let apexVar =
                      EGraph.mkPatternVar (Map.size memo)
                 in (PatternVar apexVar, Map.insert (leftValue, rightValue) apexVar memo)
        )

normalizeTermSubst :: IntSet -> TermSubst f -> TermSubst f
normalizeTermSubst sourceKeys =
  TermSubst
    . IntMap.filterWithKey
      ( \bindingKey ->
          \case
            PatternVar replacementVar ->
              patternVarKey replacementVar /= bindingKey
            PatternNode _ ->
              True
      )
    . unTermSubst
    . restrictTermSubst sourceKeys

patternVariableKeys :: Foldable f => Pattern f -> IntSet
patternVariableKeys =
  IntSet.fromList
    . fmap patternVarKey
    . Set.toAscList
    . patternVariables

disjointRenaming ::
  Foldable f =>
  Pattern f ->
  Pattern f ->
  TermSubst f
disjointRenaming leftTarget rightTarget =
  let leftVars =
        patternVariables leftTarget

      rightVars =
        patternVariables rightTarget
   in if Set.disjoint leftVars rightVars
        then emptyTermSubst
        else
          let offset =
                max (nextVarKey leftVars) (nextVarKey rightVars)
           in termSubstFromList
                [ (rightVar, PatternVar (EGraph.mkPatternVar (patternVarKey rightVar + offset)))
                  | rightVar <- Set.toAscList rightVars
                ]

nextVarKey :: Set.Set PatternVar -> Int
nextVarKey patternVars =
  case Set.lookupMax patternVars of
    Nothing ->
      0
    Just patternVar ->
      patternVarKey patternVar + 1
