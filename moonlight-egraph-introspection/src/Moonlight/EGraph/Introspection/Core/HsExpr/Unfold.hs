{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Introspection.Core.HsExpr.Unfold
  ( hsExprSelfUnfoldLawId,
    hsExprSelfUnfoldRuleIdBase,
    SelfLawRefusal (..),
    SelfLawRow (..),
    hsExprSelfUnfoldLawFamily,
  )
where

import Data.Kind (Type)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import Moonlight.Core (Pattern (..), RewriteRuleId (..))
import Moonlight.Core qualified as EGraph
import Moonlight.EGraph.Introspection.Core.HsExpr.BinderAlgebra (substituteBinderHsExpr)
import Moonlight.Rewrite.System
  ( LawBook (..),
    LawId,
    LawSpec (..),
    OracleRequirement (..),
    SemanticFidelity (..),
    TrustTier (..),
    mkLawId,
  )
import Moonlight.Rewrite.System (RewriteCondition)
import Moonlight.Rewrite.System (RawRewriteRule (..))
import Moonlight.Sheaf.Context.Core (ClassSiteSupport)
import Moonlight.Sheaf.Twist.SupportedRuleSpec (SupportedRuleSpec (..))
import Moonlight.Pale.Ghc.Expr (BinderAnn (..), ConvertedModule (..), HsExprF (..), HsVarRef (..), ScopeCtx, TopLevelBinding (..), scopeBottomCtx)
import Moonlight.FiniteLattice
  ( principalSupport
  )

hsExprSelfUnfoldLawId :: LawId
hsExprSelfUnfoldLawId =
  mkLawId 4000000

hsExprSelfUnfoldRuleIdBase :: Int
hsExprSelfUnfoldRuleIdBase =
  400000000

type SelfLawRefusal :: Type
data SelfLawRefusal
  = RefusedNotLambdaSpine
  | RefusedNotSizeDecreasing
  | RefusedSelfRecursive
  | RefusedMultiNameEquation
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type SelfLawRow :: Type
data SelfLawRow = SelfLawRow
  { slrBinding :: !String,
    slrOutcome :: !(Either SelfLawRefusal LawId)
  }
  deriving stock (Eq, Ord, Show)

type HsExprLawRule :: Type
type HsExprLawRule = RawRewriteRule (RewriteCondition ScopeCtx HsExprF) HsExprF

hsExprSelfUnfoldLawFamily :: ConvertedModule -> ([SelfLawRow], LawBook (SupportedRuleSpec ScopeCtx HsExprLawRule))
hsExprSelfUnfoldLawFamily convertedModule =
  (fmap fst rows, LawBook (foldMap rowLaw rows))
  where
    support =
      principalSupport (scopeBottomCtx (cmScopeIndex convertedModule))

    rows =
      [ selfLawRow support bindingIndex binding
      | (bindingIndex, binding) <- zip [0 ..] (cmBindings convertedModule)
      ]

    rowLaw :: (SelfLawRow, Maybe (LawSpec (SupportedRuleSpec ScopeCtx HsExprLawRule))) -> [LawSpec (SupportedRuleSpec ScopeCtx HsExprLawRule)]
    rowLaw (_, Just lawSpec) =
      [lawSpec]
    rowLaw (_, Nothing) =
      []

selfLawRow :: ClassSiteSupport ScopeCtx -> Int -> TopLevelBinding -> (SelfLawRow, Maybe (LawSpec (SupportedRuleSpec ScopeCtx HsExprLawRule)))
selfLawRow support bindingIndex binding =
  case tlbNames binding of
    [bindingName] ->
      case lambdaSpine (tlbTerm binding) of
        Nothing ->
          refused RefusedNotLambdaSpine
        Just (binders, body) ->
          if selfReferenceOccurs bindingName body
            then refused RefusedSelfRecursive
            else
              let lhs =
                    unfoldLhs bindingName binders
                  rhs =
                    substituteBinderVars binders body
               in if patternNodeCount rhs < patternNodeCount lhs
                    then
                      let lawSpec =
                            LawSpec
                              { lawId = hsExprSelfUnfoldLawId,
                                lawTier = ModuleDerived,
                                lawFidelity = Observational,
                                lawOracle = NoOracleRequired,
                                lawRule =
                                  SupportedRuleSpec
                                    { srsSupport = support,
                                      srsRule =
                                        RawRewriteRule
                                          { rrId = RewriteRuleId (hsExprSelfUnfoldRuleIdBase + bindingIndex),
                                            rrLhs = lhs,
                                            rrRhs = rhs,
                                            rrCondition = Nothing,
                                            rrApplicationCondition = Nothing,
                                            rrPostSubst = Nothing
                                          }
                                    }
                              }
                       in (SelfLawRow (bindingDisplayName binding) (Right hsExprSelfUnfoldLawId), Just lawSpec)
                    else refused RefusedNotSizeDecreasing
    _ ->
      refused RefusedMultiNameEquation
  where
    refused reason =
      (SelfLawRow (bindingDisplayName binding) (Left reason), Nothing)

lambdaSpine :: Pattern HsExprF -> Maybe ([BinderAnn], Pattern HsExprF)
lambdaSpine = \case
  PatternNode (LamF binder body) ->
    let (binders, finalBody) = lambdaSpineWith binder body
     in Just (binders, finalBody)
  _ ->
    Nothing

lambdaSpineWith :: BinderAnn -> Pattern HsExprF -> ([BinderAnn], Pattern HsExprF)
lambdaSpineWith binder body =
  case body of
    PatternNode (LamF nextBinder nextBody) ->
      let (binders, finalBody) = lambdaSpineWith nextBinder nextBody
       in (binder : binders, finalBody)
    _ ->
      ([binder], body)

unfoldLhs :: RdrName -> [BinderAnn] -> Pattern HsExprF
unfoldLhs bindingName binders =
  foldl' (\functionTerm argumentTerm -> PatternNode (AppF functionTerm argumentTerm)) (PatternNode (VarF (GlobalName bindingName))) arguments
  where
    arguments =
      fmap (PatternVar . EGraph.mkPatternVar) [0 .. length binders - 1]

substituteBinderVars :: [BinderAnn] -> Pattern HsExprF -> Pattern HsExprF
substituteBinderVars binders body =
  foldl'
    ( \rewrittenBody (patternIndex, binder) ->
        substituteBinderHsExpr (baId binder) (PatternVar (EGraph.mkPatternVar patternIndex)) rewrittenBody
    )
    body
    (zip [0 ..] binders)

selfReferenceOccurs :: RdrName -> Pattern HsExprF -> Bool
selfReferenceOccurs bindingName patternValue =
  case patternValue of
    PatternVar {} ->
      False
    PatternNode node ->
      matchingReference node || any (selfReferenceOccurs bindingName) node
  where
    bindingOcc =
      rdrOccText bindingName

    matchingReference = \case
      VarF (GlobalName referenceName) ->
        rdrOccText referenceName == bindingOcc
      _ ->
        False

patternNodeCount :: Pattern HsExprF -> Int
patternNodeCount = \case
  PatternVar {} ->
    0
  PatternNode node ->
    1 + sum (fmap patternNodeCount node)

bindingDisplayName :: TopLevelBinding -> String
bindingDisplayName binding =
  case tlbNames binding of
    [] ->
      "_unnamed"
    bindingName : _ ->
      rdrOccText bindingName

rdrOccText :: RdrName -> String
rdrOccText =
  occNameString . rdrNameOcc
