-- | Free-scope e-class analysis: witness-valued scope legality for derived classes.
module Moonlight.EGraph.Introspection.Core.HsExpr.FreeScope
  ( FreeScopeWitness (..),
    HasFreeScopeWitness (..),
    freeScopeWitnessEmpty,
    freeScopeWitnessScopes,
    hsExprFreeScopeWitness,
    hsExprFreeScopeAnalysisSpec,
  )
where

import Data.Foldable (toList)
import Moonlight.Algebra (JoinSemilattice (join))
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec, semilatticeAnalysis)
import Moonlight.Pale.Ghc.Expr
  ( BinderAnn (..),
    GuardedAltF (..),
    HsExprF (..),
    HsGuardStmtF (..),
    HsStmtF (..),
    HsVarRef (..),
    ScopeId,
    ScopeIndex,
    binderIntroScope,
    patBinders,
    scopeDepthOf,
  )

data FreeScopeWitness
  = FreeScopeKnown ![(Int, ScopeId)]
  | FreeScopeUnknown
  deriving stock (Eq, Ord, Show)

instance JoinSemilattice FreeScopeWitness where
  join = min

class HasFreeScopeWitness a where
  freeScopeWitness :: a -> FreeScopeWitness

instance HasFreeScopeWitness FreeScopeWitness where
  freeScopeWitness = id

instance HasFreeScopeWitness () where
  freeScopeWitness _ = FreeScopeUnknown

freeScopeWitnessEmpty :: FreeScopeWitness
freeScopeWitnessEmpty =
  FreeScopeKnown []

freeScopeWitnessScopes :: FreeScopeWitness -> Maybe [ScopeId]
freeScopeWitnessScopes = \case
  FreeScopeKnown chain -> Just (fmap snd chain)
  FreeScopeUnknown -> Nothing

hsExprFreeScopeAnalysisSpec :: ScopeIndex -> AnalysisSpec HsExprF FreeScopeWitness
hsExprFreeScopeAnalysisSpec scopeIndex =
  semilatticeAnalysis (hsExprFreeScopeWitness scopeIndex)

hsExprFreeScopeWitness :: ScopeIndex -> HsExprF FreeScopeWitness -> FreeScopeWitness
hsExprFreeScopeWitness scopeIndex node =
  case node of
    VarF (GlobalName _) ->
      freeScopeWitnessEmpty
    VarF (LocalName binderAnn) ->
      either (const FreeScopeUnknown) id
        ( do introScope <- binderIntroScope scopeIndex (baId binderAnn)
             introDepth <- scopeDepthOf scopeIndex introScope
             pure (FreeScopeKnown [(introDepth, introScope)])
        )
    LamF binderAnn body ->
      bindOver [binderAnn] [body]
    LetF _ bindings body ->
      bindOver (foldMap (patBinders . fst) bindings) (body : fmap snd bindings)
    CaseF scrutinee alternatives ->
      bindOver (foldMap (patBinders . fst) alternatives) (scrutinee : fmap snd alternatives)
    DoF stmts ->
      bindOver (foldMap stmtBinders stmts) (foldMap toList stmts)
    GuardedF alternatives ->
      bindOver (foldMap guardedAltBinders alternatives) (foldMap toList alternatives)
    MultiIfF alternatives ->
      bindOver (foldMap guardedAltBinders alternatives) (foldMap toList alternatives)
    ClausesF clauses ->
      bindOver (foldMap (foldMap patBinders . fst) clauses) (fmap snd clauses)
    OpaqueF _ ->
      FreeScopeUnknown
    other ->
      unionWitnesses (toList other)
  where
    bindOver binderAnns children =
      case traverse (binderIntroScope scopeIndex . baId) binderAnns of
        Left _ ->
          FreeScopeUnknown
        Right boundScopes ->
          case unionWitnesses children of
            FreeScopeKnown chain ->
              FreeScopeKnown (filter (\(_, scopeId) -> scopeId `notElem` boundScopes) chain)
            FreeScopeUnknown ->
              FreeScopeUnknown

    stmtBinders = \case
      BindStmtF pat _ -> patBinders pat
      BodyStmtF _ -> []
      LetStmtF _ bindings -> foldMap (patBinders . fst) bindings

    guardStmtBinders = \case
      GuardBoolF _ -> []
      GuardPatF pat _ -> patBinders pat
      GuardLetF _ bindings -> foldMap (patBinders . fst) bindings

    guardedAltBinders alternative =
      foldMap guardStmtBinders (gaGuards alternative)

unionWitnesses :: [FreeScopeWitness] -> FreeScopeWitness
unionWitnesses =
  foldr
    ( \childWitness unionValue ->
        case (childWitness, unionValue) of
          (FreeScopeKnown left, FreeScopeKnown right) ->
            FreeScopeKnown (unionDescending left right)
          _ ->
            FreeScopeUnknown
    )
    freeScopeWitnessEmpty
  where
    unionDescending left right =
      case (left, right) of
        ([], _) -> right
        (_, []) -> left
        (leftEntry : leftRest, rightEntry : rightRest) ->
          case compare leftEntry rightEntry of
            GT -> leftEntry : unionDescending leftRest right
            LT -> rightEntry : unionDescending left rightRest
            EQ -> leftEntry : unionDescending leftRest rightRest
