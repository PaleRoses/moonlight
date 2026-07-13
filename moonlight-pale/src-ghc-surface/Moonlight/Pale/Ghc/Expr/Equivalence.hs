{-# LANGUAGE LambdaCase #-}

module Moonlight.Pale.Ghc.Expr.Equivalence
  ( renderRoundTripEquivalent,
  )
where

import Data.Function (on)
import Moonlight.Core (Pattern (..))
import Moonlight.Pale.Ghc.Expr.NameRender (renderRdrName)
import Moonlight.Pale.Ghc.Expr.Syntax

renderRoundTripEquivalent :: Pattern HsExprF -> Pattern HsExprF -> Bool
renderRoundTripEquivalent leftValue rightValue =
  case (stripParens leftValue, stripParens rightValue) of
    (PatternVar leftVar, PatternVar rightVar) ->
      leftVar == rightVar
    (PatternNode leftNode, PatternNode rightNode) ->
      equivalentNode leftNode rightNode
    _ ->
      False

stripParens :: Pattern HsExprF -> Pattern HsExprF
stripParens = \case
  PatternNode (ParF innerValue) -> stripParens innerValue
  patternValue -> patternValue

equivalentNode :: HsExprF (Pattern HsExprF) -> HsExprF (Pattern HsExprF) -> Bool
equivalentNode leftNode rightNode =
  case (leftNode, rightNode) of
    (VarF leftRef, VarF rightRef) ->
      equivalentVarRef leftRef rightRef
    (AppF leftFunction leftArgument, AppF rightFunction rightArgument) ->
      renderRoundTripEquivalent leftFunction rightFunction
        && renderRoundTripEquivalent leftArgument rightArgument
    (LamF leftBinder leftBody, LamF rightBinder rightBody) ->
      equivalentBinderAnn leftBinder rightBinder
        && renderRoundTripEquivalent leftBody rightBody
    (LetF leftMode leftBindings leftBody, LetF rightMode rightBindings rightBody) ->
      leftMode == rightMode
        && equivalentListWith equivalentBinding leftBindings rightBindings
        && renderRoundTripEquivalent leftBody rightBody
    (OpAppF leftOperand leftOperator leftArgument, OpAppF rightOperand rightOperator rightArgument) ->
      renderRoundTripEquivalent leftOperand rightOperand
        && renderRoundTripEquivalent leftOperator rightOperator
        && renderRoundTripEquivalent leftArgument rightArgument
    (SectionLF leftExpr leftOperator, SectionLF rightExpr rightOperator) ->
      renderRoundTripEquivalent leftExpr rightExpr
        && renderRoundTripEquivalent leftOperator rightOperator
    (SectionRF leftOperator leftExpr, SectionRF rightOperator rightExpr) ->
      renderRoundTripEquivalent leftOperator rightOperator
        && renderRoundTripEquivalent leftExpr rightExpr
    (LitF leftLiteral, LitF rightLiteral) ->
      equivalentLit leftLiteral rightLiteral
    (OverLitF leftLiteral, OverLitF rightLiteral) ->
      leftLiteral == rightLiteral
    (IfF leftCondition leftThen leftElse, IfF rightCondition rightThen rightElse) ->
      renderRoundTripEquivalent leftCondition rightCondition
        && renderRoundTripEquivalent leftThen rightThen
        && renderRoundTripEquivalent leftElse rightElse
    (CaseF leftScrutinee leftAlternatives, CaseF rightScrutinee rightAlternatives) ->
      renderRoundTripEquivalent leftScrutinee rightScrutinee
        && equivalentListWith equivalentCaseAlternative leftAlternatives rightAlternatives
    (DoF leftStatements, DoF rightStatements) ->
      equivalentListWith equivalentStatement leftStatements rightStatements
    (NegF leftExpr, NegF rightExpr) ->
      renderRoundTripEquivalent leftExpr rightExpr
    (ExplicitListF leftExprs, ExplicitListF rightExprs) ->
      equivalentListWith renderRoundTripEquivalent leftExprs rightExprs
    (ExplicitTupleF leftExprs, ExplicitTupleF rightExprs) ->
      equivalentListWith renderRoundTripEquivalent leftExprs rightExprs
    (RecordConF leftConstructor leftFields, RecordConF rightConstructor rightFields) ->
      renderRoundTripEquivalent leftConstructor rightConstructor
        && equivalentListWith equivalentField leftFields rightFields
    (RecordUpdF leftRecord leftFields, RecordUpdF rightRecord rightFields) ->
      renderRoundTripEquivalent leftRecord rightRecord
        && equivalentListWith equivalentField leftFields rightFields
    (ArithSeqF leftSeq, ArithSeqF rightSeq) ->
      equivalentArithSeq leftSeq rightSeq
    (GuardedF leftAlts, GuardedF rightAlts) ->
      equivalentListWith equivalentGuardedAlt leftAlts rightAlts
    (ClausesF leftClauses, ClausesF rightClauses) ->
      equivalentListWith equivalentClause leftClauses rightClauses
    (MultiIfF leftAlts, MultiIfF rightAlts) ->
      equivalentListWith equivalentGuardedAlt leftAlts rightAlts
    (ExprWithTySigF leftExpr leftType, ExprWithTySigF rightExpr rightType) ->
      renderRoundTripEquivalent leftExpr rightExpr
        && equivalentTypeText leftType rightType
    (AppTypeF leftExpr leftType, AppTypeF rightExpr rightType) ->
      renderRoundTripEquivalent leftExpr rightExpr
        && equivalentTypeText leftType rightType
    (OpaqueF leftTag, OpaqueF rightTag) ->
      leftTag == rightTag
    _ ->
      False

equivalentVarRef :: HsVarRef -> HsVarRef -> Bool
equivalentVarRef leftRef rightRef =
  case (leftRef, rightRef) of
    (GlobalName leftName, GlobalName rightName) ->
      renderRdrName leftName == renderRdrName rightName
    (LocalName leftBinder, LocalName rightBinder) ->
      equivalentBinderAnn leftBinder rightBinder
    _ ->
      False

equivalentBinderAnn :: BinderAnn -> BinderAnn -> Bool
equivalentBinderAnn =
  (==) `on` (renderRdrName . baName)

equivalentTypeText :: NormalizedTypeText -> NormalizedTypeText -> Bool
equivalentTypeText leftType rightType =
  nttText leftType == nttText rightType

equivalentPat :: HsPatF -> HsPatF -> Bool
equivalentPat leftPattern rightPattern =
  case (stripPatParens leftPattern, stripPatParens rightPattern) of
    (PVarP leftAnn, PVarP rightAnn) ->
      equivalentBinderAnn leftAnn rightAnn
    (PWildP, PWildP) ->
      True
    (PConP leftName leftSubs, PConP rightName rightSubs) ->
      renderRdrName leftName == renderRdrName rightName
        && equivalentListWith equivalentPat leftSubs rightSubs
    (PRecP leftName leftFields, PRecP rightName rightFields) ->
      renderRdrName leftName == renderRdrName rightName
        && equivalentListWith equivalentPatternField leftFields rightFields
    (PTupleP leftSubs, PTupleP rightSubs) ->
      equivalentListWith equivalentPat leftSubs rightSubs
    (PListP leftSubs, PListP rightSubs) ->
      equivalentListWith equivalentPat leftSubs rightSubs
    (PLitP leftLit, PLitP rightLit) ->
      equivalentLit leftLit rightLit
    (POverLitP leftLit, POverLitP rightLit) ->
      leftLit == rightLit
    (PAsP leftAnn leftSub, PAsP rightAnn rightSub) ->
      equivalentBinderAnn leftAnn rightAnn && equivalentPat leftSub rightSub
    (PBangP leftSub, PBangP rightSub) ->
      equivalentPat leftSub rightSub
    (PLazyP leftSub, PLazyP rightSub) ->
      equivalentPat leftSub rightSub
    (PLossyP leftTag leftAnns, PLossyP rightTag rightAnns) ->
      leftTag == rightTag
        && equivalentListWith equivalentBinderAnn leftAnns rightAnns
    _ ->
      False

stripPatParens :: HsPatF -> HsPatF
stripPatParens = \case
  PParP innerPattern -> stripPatParens innerPattern
  patternValue -> patternValue

equivalentLit :: NormalizedLit -> NormalizedLit -> Bool
equivalentLit leftLiteral rightLiteral =
  normalizeMultiline leftLiteral == normalizeMultiline rightLiteral
  where
    normalizeMultiline = \case
      NormalizedMultilineString value -> NormalizedString value
      literalValue -> literalValue

equivalentBinding :: (HsPatF, Pattern HsExprF) -> (HsPatF, Pattern HsExprF) -> Bool
equivalentBinding (leftPattern, leftRhs) (rightPattern, rightRhs) =
  equivalentPat leftPattern rightPattern
    && renderRoundTripEquivalent leftRhs rightRhs

equivalentCaseAlternative :: (HsPatF, Pattern HsExprF) -> (HsPatF, Pattern HsExprF) -> Bool
equivalentCaseAlternative (leftPattern, leftBody) (rightPattern, rightBody) =
  equivalentPat leftPattern rightPattern
    && renderRoundTripEquivalent leftBody rightBody

equivalentClause :: ([HsPatF], Pattern HsExprF) -> ([HsPatF], Pattern HsExprF) -> Bool
equivalentClause (leftPatterns, leftBody) (rightPatterns, rightBody) =
  equivalentListWith equivalentPat leftPatterns rightPatterns
    && renderRoundTripEquivalent leftBody rightBody

equivalentStatement :: HsStmtF (Pattern HsExprF) -> HsStmtF (Pattern HsExprF) -> Bool
equivalentStatement leftStatement rightStatement =
  case (leftStatement, rightStatement) of
    (BindStmtF leftPattern leftExpr, BindStmtF rightPattern rightExpr) ->
      equivalentPat leftPattern rightPattern
        && renderRoundTripEquivalent leftExpr rightExpr
    (BodyStmtF leftExpr, BodyStmtF rightExpr) ->
      renderRoundTripEquivalent leftExpr rightExpr
    (LetStmtF leftMode leftBindings, LetStmtF rightMode rightBindings) ->
      leftMode == rightMode
        && equivalentListWith equivalentBinding leftBindings rightBindings
    _ ->
      False

equivalentGuardedAlt :: GuardedAltF (Pattern HsExprF) -> GuardedAltF (Pattern HsExprF) -> Bool
equivalentGuardedAlt leftAlt rightAlt =
  equivalentListWith equivalentGuardStatement (gaGuards leftAlt) (gaGuards rightAlt)
    && renderRoundTripEquivalent (gaBody leftAlt) (gaBody rightAlt)

equivalentGuardStatement :: HsGuardStmtF (Pattern HsExprF) -> HsGuardStmtF (Pattern HsExprF) -> Bool
equivalentGuardStatement leftStatement rightStatement =
  case (leftStatement, rightStatement) of
    (GuardBoolF leftExpr, GuardBoolF rightExpr) ->
      renderRoundTripEquivalent leftExpr rightExpr
    (GuardPatF leftPattern leftExpr, GuardPatF rightPattern rightExpr) ->
      equivalentPat leftPattern rightPattern
        && renderRoundTripEquivalent leftExpr rightExpr
    (GuardLetF leftMode leftBindings, GuardLetF rightMode rightBindings) ->
      leftMode == rightMode
        && equivalentListWith equivalentBinding leftBindings rightBindings
    _ ->
      False

equivalentField :: (NormalizedFieldLabel, Pattern HsExprF) -> (NormalizedFieldLabel, Pattern HsExprF) -> Bool
equivalentField (leftLabel, leftValue) (rightLabel, rightValue) =
  leftLabel == rightLabel
    && renderRoundTripEquivalent leftValue rightValue

equivalentPatternField :: (String, HsPatF) -> (String, HsPatF) -> Bool
equivalentPatternField (leftField, leftPattern) (rightField, rightPattern) =
  leftField == rightField
    && equivalentPat leftPattern rightPattern

equivalentArithSeq :: NormalizedArithSeq (Pattern HsExprF) -> NormalizedArithSeq (Pattern HsExprF) -> Bool
equivalentArithSeq leftSeq rightSeq =
  case (leftSeq, rightSeq) of
    (ArithSeqFrom leftFrom, ArithSeqFrom rightFrom) ->
      renderRoundTripEquivalent leftFrom rightFrom
    (ArithSeqFromThen leftFrom leftThen, ArithSeqFromThen rightFrom rightThen) ->
      renderRoundTripEquivalent leftFrom rightFrom
        && renderRoundTripEquivalent leftThen rightThen
    (ArithSeqFromTo leftFrom leftTo, ArithSeqFromTo rightFrom rightTo) ->
      renderRoundTripEquivalent leftFrom rightFrom
        && renderRoundTripEquivalent leftTo rightTo
    (ArithSeqFromThenTo leftFrom leftThen leftTo, ArithSeqFromThenTo rightFrom rightThen rightTo) ->
      renderRoundTripEquivalent leftFrom rightFrom
        && renderRoundTripEquivalent leftThen rightThen
        && renderRoundTripEquivalent leftTo rightTo
    _ ->
      False

equivalentListWith :: (element -> element -> Bool) -> [element] -> [element] -> Bool
equivalentListWith equivalentElement leftValues rightValues =
  length leftValues == length rightValues
    && and (zipWith equivalentElement leftValues rightValues)
