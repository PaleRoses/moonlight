{-# LANGUAGE LambdaCase #-}

module Moonlight.EGraph.Introspection.Core.HsExpr.Equation
  ( HsExprLawRule,
    EquationSides,
    equationSides,
    splitEquation,
    equationRule,
    rewriteRule,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName, isQual_maybe, rdrNameOcc)
import Moonlight.Core (BinderId (..), Pattern (..), RewriteRuleId (..))
import Moonlight.EGraph.Introspection.Core.Equation
  ( EquationBinderState (..),
    EquationError,
    EquationFront (..),
    EquationSides,
    equationRuleWith,
    equationSides,
  )
import Moonlight.EGraph.Introspection.Core.Equation qualified as Equation
import Moonlight.EGraph.Introspection.Core.HsExpr.Front
  ( HsExprLawEmitError (..),
    HsExprTerm,
    emitExpr,
  )
import Moonlight.Rewrite.System (LawId, lawIdKey)
import Moonlight.Rewrite.System (RewriteCondition)
import Moonlight.Rewrite.System (RawRewriteRule (..))
import Moonlight.Pale.Ghc.Expr (BinderAnn (..), HsExprF (..), HsVarRef (..), ScopeCtx)
import Moonlight.Pale.Ghc.Expr.Parse (convertHaskellExprSource)

type HsExprLawRule :: Type
type HsExprLawRule = RawRewriteRule (RewriteCondition ScopeCtx HsExprF) HsExprF

splitEquation :: String -> Either HsExprLawEmitError EquationSides
splitEquation =
  first hsExprEquationError . Equation.splitEquation

equationRule ::
  LawId ->
  Int ->
  [String] ->
  [(String, RdrName)] ->
  String ->
  Either HsExprLawEmitError HsExprLawRule
equationRule lawIdValue instantiationIndex names substitutions sourceText =
  first hsExprEquationError (equationRuleWith hsExprEquationFront lawIdValue instantiationIndex names substitutions sourceText)

rewriteRule :: LawId -> Int -> [String] -> HsExprTerm -> HsExprTerm -> Either HsExprLawEmitError HsExprLawRule
rewriteRule lawIdValue instantiationIndex names lhs rhs
  | instantiationIndex < 0 || instantiationIndex >= 100 =
      Left (InvalidRuleInstantiationIndex instantiationIndex)
  | otherwise =
      RawRewriteRule
        <$> pure (RewriteRuleId (lawIdKey lawIdValue * 100 + instantiationIndex))
        <*> emitExpr names lhs
        <*> emitExpr names rhs
        <*> pure Nothing
        <*> pure Nothing
        <*> pure Nothing

hsExprEquationFront :: EquationFront HsExprLawEmitError RdrName BinderAnn HsExprF
hsExprEquationFront =
  EquationFront
    { efParseTerm = parseHsExprEquationTerm,
      efParseTermWithVariables = const parseHsExprEquationTerm,
      efVariableName = hsExprVariableName,
      efSubstitutionPattern = PatternNode . VarF . GlobalName,
      efEnterBinderNode = enterHsExprBinder,
      efRewriteBoundReference = rewriteHsExprBoundReference
    }

parseHsExprEquationTerm :: String -> Either HsExprLawEmitError (Pattern HsExprF)
parseHsExprEquationTerm =
  first EquationConvertFailure . convertHaskellExprSource

hsExprVariableName :: HsExprF (Pattern HsExprF) -> Maybe String
hsExprVariableName =
  \case
    VarF (GlobalName nameValue)
      | Nothing <- isQual_maybe nameValue ->
          Just (occNameString (rdrNameOcc nameValue))
    _ ->
      Nothing

enterHsExprBinder ::
  LawId ->
  Map.Map String RdrName ->
  EquationBinderState BinderAnn ->
  HsExprF (Pattern HsExprF) ->
  Maybe (HsExprF (Pattern HsExprF), EquationBinderState BinderAnn)
enterHsExprBinder lawIdValue substitutions binderState =
  \case
    LamF binder body ->
      let offset = ebsNextBinderOffset binderState
          originalName = occNameString (rdrNameOcc (baName binder))
          rewrittenBinder =
            BinderAnn
              { baId = BinderId (negate (lawIdKey lawIdValue) - offset),
                baName = Map.findWithDefault (baName binder) originalName substitutions
              }
       in Just
            ( LamF rewrittenBinder body,
              binderState
                { ebsNextBinderOffset = offset + 1,
                  ebsBinders = Map.insert binder rewrittenBinder (ebsBinders binderState)
                }
            )
    _ ->
      Nothing

rewriteHsExprBoundReference :: EquationBinderState BinderAnn -> HsExprF (Pattern HsExprF) -> Maybe (HsExprF (Pattern HsExprF))
rewriteHsExprBoundReference binderState =
  \case
    VarF (LocalName binder) ->
      Just (VarF (LocalName (Map.findWithDefault binder binder (ebsBinders binderState))))
    _ ->
      Nothing

hsExprEquationError :: EquationError HsExprLawEmitError -> HsExprLawEmitError
hsExprEquationError =
  \case
    Equation.DuplicateEquationVariable name ->
      DuplicateEmitterVariable name
    Equation.InvalidEquationRuleInstantiationIndex instantiationIndex ->
      InvalidRuleInstantiationIndex instantiationIndex
    Equation.EquationMissingEquals sourceText ->
      EquationMissingEquals sourceText
    Equation.EquationAmbiguousEquals sourceText count ->
      EquationAmbiguousEquals sourceText count
    Equation.EquationParseFailure parseFailure ->
      parseFailure
