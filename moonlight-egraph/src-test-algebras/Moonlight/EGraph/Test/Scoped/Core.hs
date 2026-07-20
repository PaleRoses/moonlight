{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Test.Scoped.Core
  ( ScopedF (..),
    ScopedTag (..),
    scopedAnalysisSpec,
    scopedBinderSubstAlgebra,
    scopedCost,
    scopedBetaRule,
    scopedLocalEtaRule,
    scopedBinderIndependentFactId,
    scopedBinderIndependentFactRule,
    scopedFactGatedEtaRule,
    scopedFree,
    scopedLocal,
    scopedApp,
    scopedLam,
    scopedBetaRedex,
    scopedBetaContractum,
    scopedEtaRedex,
    scopedEtaContractum,
  )
where

import Data.Fix (Fix (..))
import Data.Kind (Type)
import Moonlight.Core
  ( BinderId,
    HasConstructorTag (..),
    Pattern (..),
    PatternVar,
    ZipMatch (..),
    zipSameNodeShape,
  )
import Moonlight.Core qualified as EGraph
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec (..))
import Moonlight.EGraph.Pure.Extraction (CostAlgebra (..))
import Moonlight.EGraph.Pure.Types (RewriteRuleId)
import Moonlight.Rewrite.Runtime
  ( BinderSubstAlgebra (..),
    PostMatchSubst (..),
    PostMatchTerm (..),
  )
import Moonlight.Rewrite.System
  ( RewriteCondition (..),
    data GuardRoot,
    data GuardVar,
    guardHasFact,
  )
import Moonlight.Rewrite.System
  ( FactRule,
    FactRuleId,
    RawFactRule (..),
  )
import Moonlight.Rewrite.System (FactId (..))
import Moonlight.Rewrite.System (RawRewriteRule (..))

type ScopedF :: Type -> Type
data ScopedF a
  = ScopedFree String
  | ScopedLocal BinderId
  | ScopedApp a a
  | ScopedLam BinderId a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type ScopedTag :: Type
data ScopedTag
  = ScopedFreeTag String
  | ScopedLocalTag BinderId
  | ScopedAppTag
  | ScopedLamTag BinderId
  deriving stock (Eq, Ord, Show)

instance HasConstructorTag ScopedF where
  type ConstructorTag ScopedF = ScopedTag
  constructorTag = \case
    ScopedFree freeName -> ScopedFreeTag freeName
    ScopedLocal binderId -> ScopedLocalTag binderId
    ScopedApp {} -> ScopedAppTag
    ScopedLam binderId _ -> ScopedLamTag binderId

instance ZipMatch ScopedF where
  zipMatch = zipSameNodeShape

scopedAnalysisSpec :: AnalysisSpec ScopedF ()
scopedAnalysisSpec =
  AnalysisSpec
    { asMake = const (),
      asJoin = \_ _ -> (),
      asJoinChanged = \_ _ -> ((), False)
    }

scopedBinderSubstAlgebra :: BinderSubstAlgebra ScopedF
scopedBinderSubstAlgebra =
  BinderSubstAlgebra
    { bsaSubstituteBinder = substituteScopedBinder
    }

scopedCost :: CostAlgebra ScopedF Int
scopedCost =
  CostAlgebra $ \case
    ScopedFree _ -> 1
    ScopedLocal _ -> 1
    ScopedApp functionCost argumentCost -> functionCost + argumentCost + 1
    ScopedLam _ bodyCost -> bodyCost + 1

scopedBetaRule :: RewriteRuleId -> BinderId -> RawRewriteRule (RewriteCondition capability ScopedF) ScopedF
scopedBetaRule rewriteRuleId binderId =
  scopedRewriteRule
    rewriteRuleId
    ( PatternNode
        ( ScopedApp
            (PatternNode (ScopedLam binderId (PatternVar scopedBodyPatternVar)))
            (PatternVar scopedArgumentPatternVar)
        )
    )
    (PatternVar scopedBodyPatternVar)
    Nothing
    (Just (SubstBinder binderId (PostMatchVar scopedArgumentPatternVar)))

scopedLocalEtaRule :: RewriteRuleId -> BinderId -> String -> RawRewriteRule (RewriteCondition capability ScopedF) ScopedF
scopedLocalEtaRule rewriteRuleId binderId freeName =
  scopedRewriteRule
    rewriteRuleId
    (scopedEtaPattern binderId (PatternNode (ScopedFree freeName)))
    (PatternNode (ScopedFree freeName))
    Nothing
    Nothing

scopedBinderIndependentFactId :: FactId
scopedBinderIndependentFactId =
  FactId 0

scopedBinderIndependentFactRule :: FactRuleId -> String -> FactRule capability ScopedF
scopedBinderIndependentFactRule factRuleId freeName =
  FactRule
    { frId = factRuleId,
      frName = "derive-scoped-free-binder-independence",
      frPattern = PatternNode (ScopedFree freeName),
      frProjection = [GuardRoot],
      frFactId = scopedBinderIndependentFactId,
      frCondition = Nothing
    }

scopedFactGatedEtaRule :: RewriteRuleId -> BinderId -> RawRewriteRule (RewriteCondition capability ScopedF) ScopedF
scopedFactGatedEtaRule rewriteRuleId binderId =
  scopedRewriteRule
    rewriteRuleId
    (scopedEtaPattern binderId (PatternVar scopedFunctionPatternVar))
    (PatternVar scopedFunctionPatternVar)
    ( Just
        ( RewriteCondition
            (guardHasFact scopedBinderIndependentFactId [GuardVar scopedFunctionPatternVar])
        )
    )
    Nothing

scopedFree :: String -> Fix ScopedF
scopedFree freeName =
  Fix (ScopedFree freeName)

scopedLocal :: BinderId -> Fix ScopedF
scopedLocal binderId =
  Fix (ScopedLocal binderId)

scopedApp :: Fix ScopedF -> Fix ScopedF -> Fix ScopedF
scopedApp functionTerm argumentTerm =
  Fix (ScopedApp functionTerm argumentTerm)

scopedLam :: BinderId -> Fix ScopedF -> Fix ScopedF
scopedLam binderId bodyTerm =
  Fix (ScopedLam binderId bodyTerm)

scopedBetaRedex :: BinderId -> Fix ScopedF
scopedBetaRedex binderId =
  scopedApp
    (scopedLam binderId (scopedApp (scopedFree "f") (scopedLocal binderId)))
    (scopedFree "arg")

scopedBetaContractum :: Fix ScopedF
scopedBetaContractum =
  scopedApp (scopedFree "f") (scopedFree "arg")

scopedEtaRedex :: BinderId -> String -> Fix ScopedF
scopedEtaRedex binderId freeName =
  scopedLam binderId (scopedApp (scopedFree freeName) (scopedLocal binderId))

scopedEtaContractum :: String -> Fix ScopedF
scopedEtaContractum =
  scopedFree

scopedRewriteRule ::
  RewriteRuleId ->
  Pattern ScopedF ->
  Pattern ScopedF ->
  Maybe (RewriteCondition capability ScopedF) ->
  Maybe (PostMatchSubst ScopedF) ->
  RawRewriteRule (RewriteCondition capability ScopedF) ScopedF
scopedRewriteRule rewriteRuleId lhsPattern rhsPattern rewriteCondition postMatchSubstitution =
  RawRewriteRule
    { rrId = rewriteRuleId,
      rrLhs = lhsPattern,
      rrRhs = rhsPattern,
      rrCondition = rewriteCondition,
      rrApplicationCondition = Nothing,
      rrPostSubst = postMatchSubstitution
    }

scopedEtaPattern :: BinderId -> Pattern ScopedF -> Pattern ScopedF
scopedEtaPattern binderId functionPattern =
  PatternNode
    ( ScopedLam
        binderId
        ( PatternNode
            (ScopedApp functionPattern (PatternNode (ScopedLocal binderId)))
        )
    )

scopedBodyPatternVar :: PatternVar
scopedBodyPatternVar =
  EGraph.mkPatternVar 0

scopedArgumentPatternVar :: PatternVar
scopedArgumentPatternVar =
  EGraph.mkPatternVar 1

scopedFunctionPatternVar :: PatternVar
scopedFunctionPatternVar =
  EGraph.mkPatternVar 0

substituteScopedBinder :: BinderId -> Pattern ScopedF -> Pattern ScopedF -> Pattern ScopedF
substituteScopedBinder targetBinderId argumentPattern =
  go
  where
    go patternValue =
      case patternValue of
        PatternVar patternVar ->
          PatternVar patternVar
        PatternNode scopedNode ->
          case scopedNode of
            ScopedFree freeName ->
              PatternNode (ScopedFree freeName)
            ScopedLocal binderId
              | binderId == targetBinderId ->
                  argumentPattern
              | otherwise ->
                  PatternNode (ScopedLocal binderId)
            ScopedApp functionValue argumentValue ->
              PatternNode (ScopedApp (go functionValue) (go argumentValue))
            ScopedLam binderId bodyValue
              | binderId == targetBinderId ->
                  PatternNode (ScopedLam binderId bodyValue)
              | otherwise ->
                  PatternNode (ScopedLam binderId (go bodyValue))
