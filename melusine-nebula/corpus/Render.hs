{-# LANGUAGE LambdaCase #-}

module Moonlight.Pale.Ghc.Expr.Render
  ( RenderRefusal (..),
    renderHsExpr,
    renderTopLevelBinding,
    renderModuleSource,
    renderRoundTripEquivalent,
  )
where

import Data.ByteString qualified as ByteString
import Data.Char (isAlpha)
import Data.Function (on)
import Data.Kind (Type)
import Data.List (intercalate)
import GHC.Types.Name.Occurrence (isSymOcc, occNameString)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import Moonlight.Core (Pattern (..))
import Moonlight.Pale.Ghc.Expr.Syntax

type RenderRefusal :: Type
data RenderRefusal
  = RenderOpaque !HsOpaqueTag
  | RenderPatternVariable
  | RenderNonVarOperator
  | RenderLossyBinderPattern
  | RenderEmptyBindingName
  deriving stock (Eq, Ord, Show)

type Doc :: Type
newtype Doc = Doc {docLines :: [(Int, String)]}

instance Semigroup Doc where
  Doc leftLines <> Doc rightLines =
    Doc (mergeDocLines leftLines rightLines)

instance Monoid Doc where
  mempty = Doc []

mergeDocLines :: [(Int, String)] -> [(Int, String)] -> [(Int, String)]
mergeDocLines leftLines rightLines =
  case leftLines of
    [] -> rightLines
    [lastLine] -> mergeTail lastLine rightLines
    firstLine : restLines -> firstLine : mergeDocLines restLines rightLines
  where
    mergeTail (lastIndent, lastText) = \case
      [] -> [(lastIndent, lastText)]
      (_, firstText) : rightRest -> (lastIndent, lastText <> firstText) : rightRest

text :: String -> Doc
text textValue =
  Doc [(0, textValue)]

nest :: Int -> Doc -> Doc
nest indentDelta (Doc lineValues) =
  Doc (fmap (\(indentValue, textValue) -> (indentValue + indentDelta, textValue)) lineValues)

vcat :: [Doc] -> Doc
vcat docValues =
  Doc (concatMap docLines docValues)

hcat :: [Doc] -> Doc
hcat =
  mconcat

(<+>) :: Doc -> Doc -> Doc
leftDoc <+> rightDoc =
  leftDoc <> text " " <> rightDoc

flattenDoc :: Doc -> String
flattenDoc (Doc lineValues) =
  intercalate "\n" (fmap (\(indentValue, textValue) -> replicate indentValue ' ' <> textValue) lineValues)

renderHsExpr :: Pattern HsExprF -> Either RenderRefusal String
renderHsExpr expressionValue =
  flattenDoc <$> renderExpr 0 expressionValue

renderTopLevelBinding :: String -> Pattern HsExprF -> Either RenderRefusal String
renderTopLevelBinding bindingName bindingTerm =
  flattenDoc <$> renderTopLevelBindingDoc bindingName bindingTerm

renderTopLevelBindingDoc :: String -> Pattern HsExprF -> Either RenderRefusal Doc
renderTopLevelBindingDoc bindingName bindingTerm
  | null bindingName =
      Left RenderEmptyBindingName
  | otherwise = do
      bodyDoc <- renderExpr 0 bindingTerm
      Right (renderDefinitionName bindingName <> text " = " <> bodyDoc)

renderModuleSource :: String -> Maybe String -> [(String, Pattern HsExprF)] -> Either RenderRefusal String
renderModuleSource headerPrefix maybeModuleName renderedBindings = do
  bindingTexts <- traverse (uncurry renderTopLevelBinding) renderedBindings
  let prefixValue =
        if null headerPrefix
          then maybe "" (\moduleNameValue -> "module " <> moduleNameValue <> " where\n\n") maybeModuleName
          else headerPrefix
  Right (prefixValue <> intercalate "\n\n" bindingTexts <> "\n")

renderExpr :: Int -> Pattern HsExprF -> Either RenderRefusal Doc
renderExpr parentPrecedence = \case
  PatternVar _ ->
    Left RenderPatternVariable
  PatternNode expressionValue ->
    case expressionValue of
      VarF variableReference ->
        Right (renderVarRefAtom variableReference)
      AppF functionValue argumentValue -> do
        functionDoc <- renderExpr 10 functionValue
        argumentDoc <- renderExpr 11 argumentValue
        Right (wrapParen (parentPrecedence > 10) (functionDoc <> text " " <> argumentDoc))
      LamF binderAnn bodyValue -> do
        bodyDoc <- renderExpr 0 bodyValue
        Right (wrapParen (parentPrecedence > 0) (text "\\" <> renderBinderAnn binderAnn <> text " -> " <> bodyDoc))
      LetF _ bindingValues bodyValue -> do
        bindingDocs <- traverse renderLetBinding bindingValues
        bodyDoc <- renderExpr 0 bodyValue
        Right (wrapParen (parentPrecedence > 0) (text "let " <> intercalateDoc (text "; ") bindingDocs <> text " in " <> bodyDoc))
      OpAppF leftValue operatorValue rightValue -> do
        operatorDoc <- renderOperator operatorValue
        leftDoc <- renderExpr 7 leftValue
        rightDoc <- renderExpr 7 rightValue
        Right (wrapParen (parentPrecedence > 6) (leftDoc <> text " " <> operatorDoc <> text " " <> rightDoc))
      SectionLF leftValue operatorValue -> do
        leftDoc <- renderExpr 0 leftValue
        operatorDoc <- renderOperator operatorValue
        Right (text "(" <> leftDoc <> text " " <> operatorDoc <> text ")")
      SectionRF operatorValue rightValue -> do
        operatorDoc <- renderOperator operatorValue
        rightDoc <- renderExpr 0 rightValue
        Right (text "(" <> operatorDoc <> text " " <> rightDoc <> text ")")
      ParF innerValue -> do
        innerDoc <- renderExpr 0 innerValue
        Right (text "(" <> innerDoc <> text ")")
      LitF literalValue ->
        Right (renderNormalizedLit literalValue)
      OverLitF literalValue ->
        Right (renderNormalizedOverLit literalValue)
      IfF conditionValue thenValue elseValue -> do
        conditionDoc <- renderExpr 0 conditionValue
        thenDoc <- renderExpr 0 thenValue
        elseDoc <- renderExpr 0 elseValue
        Right (wrapParen (parentPrecedence > 0) (text "if " <> conditionDoc <> text " then " <> thenDoc <> text " else " <> elseDoc))
      CaseF scrutineeValue branchValues -> do
        scrutineeDoc <- renderExpr 0 scrutineeValue
        branchDocs <- traverse renderCaseBranch branchValues
        Right
          ( wrapParen
              (parentPrecedence > 0)
              (text "case " <> scrutineeDoc <> text " of { " <> intercalateDoc (text "; ") branchDocs <> text " }")
          )
      DoF statementValues -> do
        statementDocs <- traverse renderDoStatement statementValues
        Right (wrapParen (parentPrecedence > 0) (text "do { " <> intercalateDoc (text "; ") statementDocs <> text " }"))
      NegF innerValue -> do
        innerDoc <- renderExpr 10 innerValue
        Right (wrapParen (parentPrecedence > 9) (text "-" <> innerDoc))
      ExplicitListF valueList -> do
        elementDocs <- traverse (renderExpr 0) valueList
        Right (text "[" <> intercalateDoc (text ", ") elementDocs <> text "]")
      ExplicitTupleF valueList -> do
        elementDocs <- traverse (renderExpr 0) valueList
        Right (text "(" <> intercalateDoc (text ", ") elementDocs <> text ")")
      RecordConF constructorValue fieldValues -> do
        constructorDoc <- renderExpr 11 constructorValue
        fieldDocs <- traverse renderField fieldValues
        Right (constructorDoc <> text " { " <> intercalateDoc (text ", ") fieldDocs <> text " }")
      RecordUpdF recordValue fieldValues -> do
        recordDoc <- renderExpr 11 recordValue
        fieldDocs <- traverse renderField fieldValues
        Right (recordDoc <> text " { " <> intercalateDoc (text ", ") fieldDocs <> text " }")
      ArithSeqF arithSeqValue ->
        renderArithSeq arithSeqValue
      OpaqueF opaqueTag ->
        Left (RenderOpaque opaqueTag)

renderOperator :: Pattern HsExprF -> Either RenderRefusal Doc
renderOperator = \case
  PatternNode (VarF variableReference) ->
    Right (renderVarRefOperator variableReference)
  PatternVar _ ->
    Left RenderPatternVariable
  _ ->
    Left RenderNonVarOperator

renderCaseBranch :: (BinderSpec, Pattern HsExprF) -> Either RenderRefusal Doc
renderCaseBranch (binderSpecValue, branchValue) = do
  binderDoc <- renderBinderPattern binderSpecValue
  branchDoc <- renderExpr 0 branchValue
  Right (binderDoc <> text " -> " <> branchDoc)

renderDoStatement :: HsStmtF (Pattern HsExprF) -> Either RenderRefusal Doc
renderDoStatement = \case
  BindStmtF binderSpecValue rhsValue -> do
    binderDoc <- renderBinderPattern binderSpecValue
    rhsDoc <- renderExpr 0 rhsValue
    Right (binderDoc <> text " <- " <> rhsDoc)
  BodyStmtF exprValue ->
    renderExpr 0 exprValue
  LetStmtF _ bindingValues -> do
    bindingDocs <- traverse renderLetBinding bindingValues
    Right (text "let { " <> intercalateDoc (text "; ") bindingDocs <> text " }")

renderLetBinding :: (BinderAnn, Pattern HsExprF) -> Either RenderRefusal Doc
renderLetBinding (binderAnn, rhsValue) = do
  rhsDoc <- renderExpr 0 rhsValue
  Right (renderBinderAnn binderAnn <> text " = " <> rhsDoc)

renderBinderPattern :: BinderSpec -> Either RenderRefusal Doc
renderBinderPattern (BinderSpec binderShape binderAnns) =
  let binderNameDocs = fmap renderBinderAnn binderAnns
   in case binderShape of
        BinderOpaque ->
          case binderNameDocs of
            [] -> Right (text "_")
            [binderNameDoc] -> Right binderNameDoc
            _ -> Left RenderLossyBinderPattern
        BinderTuple ->
          Right (text "(" <> intercalateDoc (text ", ") binderNameDocs <> text ")")
        BinderLossy ->
          Left RenderLossyBinderPattern

renderField :: (NormalizedFieldLabel, Pattern HsExprF) -> Either RenderRefusal Doc
renderField (fieldLabelValue, fieldValue) = do
  fieldDoc <- renderExpr 0 fieldValue
  Right (text (nflSelector fieldLabelValue) <> text " = " <> fieldDoc)

renderArithSeq :: NormalizedArithSeq (Pattern HsExprF) -> Either RenderRefusal Doc
renderArithSeq = \case
  ArithSeqFrom fromValue -> do
    fromDoc <- renderExpr 0 fromValue
    Right (text "[" <> fromDoc <> text " ..]")
  ArithSeqFromThen fromValue thenValue -> do
    fromDoc <- renderExpr 0 fromValue
    thenDoc <- renderExpr 0 thenValue
    Right (text "[" <> fromDoc <> text ", " <> thenDoc <> text " ..]")
  ArithSeqFromTo fromValue toValue -> do
    fromDoc <- renderExpr 0 fromValue
    toDoc <- renderExpr 0 toValue
    Right (text "[" <> fromDoc <> text " .. " <> toDoc <> text "]")
  ArithSeqFromThenTo fromValue thenValue toValue -> do
    fromDoc <- renderExpr 0 fromValue
    thenDoc <- renderExpr 0 thenValue
    toDoc <- renderExpr 0 toValue
    Right (text "[" <> fromDoc <> text ", " <> thenDoc <> text " .. " <> toDoc <> text "]")

renderVarRefAtom :: HsVarRef -> Doc
renderVarRefAtom variableReference =
  let nameValue = varRefRdrName variableReference
   in if isSymOcc (rdrNameOcc nameValue)
        then text ("(" <> renderRdrName nameValue <> ")")
        else text (renderRdrName nameValue)

renderVarRefOperator :: HsVarRef -> Doc
renderVarRefOperator variableReference =
  let nameValue = varRefRdrName variableReference
   in if isSymOcc (rdrNameOcc nameValue)
        then text (renderRdrName nameValue)
        else text ("`" <> renderRdrName nameValue <> "`")

renderBinderAnn :: BinderAnn -> Doc
renderBinderAnn binderAnn =
  if isSymOcc (rdrNameOcc (baName binderAnn))
    then text ("(" <> renderRdrName (baName binderAnn) <> ")")
    else text (renderRdrName (baName binderAnn))

renderDefinitionName :: String -> Doc
renderDefinitionName definitionName =
  case definitionName of
    headChar : _
      | isAlpha headChar || headChar == '_' ->
          text definitionName
    _ ->
      text ("(" <> definitionName <> ")")

varRefRdrName :: HsVarRef -> RdrName
varRefRdrName = \case
  GlobalName rdrName -> rdrName
  LocalName binderAnn -> baName binderAnn

renderRdrName :: RdrName -> String
renderRdrName =
  occNameString . rdrNameOcc

wrapParen :: Bool -> Doc -> Doc
wrapParen shouldWrap innerDoc =
  if shouldWrap then text "(" <> innerDoc <> text ")" else innerDoc

intercalateDoc :: Doc -> [Doc] -> Doc
intercalateDoc separatorDoc docValues =
  case docValues of
    [] -> mempty
    firstDoc : restDocs ->
      hcat (firstDoc : concatMap (\docValue -> [separatorDoc, docValue]) restDocs)

renderNormalizedLit :: NormalizedLit -> String
renderNormalizedLit = \case
  NormalizedChar value -> show value
  NormalizedCharPrim value -> show value <> "#"
  NormalizedString value -> show value
  NormalizedMultilineString value -> show value
  NormalizedStringPrim value -> show (ByteString.unpack value) <> "#"
  NormalizedInt value -> show value
  NormalizedIntPrim value -> show value <> "#"
  NormalizedWordPrim value -> show value <> "##"
  NormalizedInt8Prim value -> show value <> "#Int8"
  NormalizedInt16Prim value -> show value <> "#Int16"
  NormalizedInt32Prim value -> show value <> "#Int32"
  NormalizedInt64Prim value -> show value <> "#Int64"
  NormalizedWord8Prim value -> show value <> "#Word8"
  NormalizedWord16Prim value -> show value <> "#Word16"
  NormalizedWord32Prim value -> show value <> "#Word32"
  NormalizedWord64Prim value -> show value <> "#Word64"
  NormalizedFloatPrim value -> show (fromRational value :: Double) <> "#"
  NormalizedDoublePrim value -> show (fromRational value :: Double) <> "##"

renderNormalizedOverLit :: NormalizedOverLit -> String
renderNormalizedOverLit = \case
  NormalizedIntegralOverLit value -> show value
  NormalizedFractionalOverLit value -> show (fromRational value :: Double)
  NormalizedStringOverLit value -> show value

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

equivalentBinderSpec :: BinderSpec -> BinderSpec -> Bool
equivalentBinderSpec leftSpec rightSpec =
  bsShape leftSpec == bsShape rightSpec
    && equivalentListWith equivalentBinderAnn (bsBinders leftSpec) (bsBinders rightSpec)

equivalentLit :: NormalizedLit -> NormalizedLit -> Bool
equivalentLit leftLiteral rightLiteral =
  normalizeMultiline leftLiteral == normalizeMultiline rightLiteral
  where
    normalizeMultiline = \case
      NormalizedMultilineString value -> NormalizedString value
      literalValue -> literalValue

equivalentBinding :: (BinderAnn, Pattern HsExprF) -> (BinderAnn, Pattern HsExprF) -> Bool
equivalentBinding (leftBinder, leftRhs) (rightBinder, rightRhs) =
  equivalentBinderAnn leftBinder rightBinder
    && renderRoundTripEquivalent leftRhs rightRhs

equivalentCaseAlternative :: (BinderSpec, Pattern HsExprF) -> (BinderSpec, Pattern HsExprF) -> Bool
equivalentCaseAlternative (leftSpec, leftBody) (rightSpec, rightBody) =
  equivalentBinderSpec leftSpec rightSpec
    && renderRoundTripEquivalent leftBody rightBody

equivalentStatement :: HsStmtF (Pattern HsExprF) -> HsStmtF (Pattern HsExprF) -> Bool
equivalentStatement leftStatement rightStatement =
  case (leftStatement, rightStatement) of
    (BindStmtF leftSpec leftExpr, BindStmtF rightSpec rightExpr) ->
      equivalentBinderSpec leftSpec rightSpec
        && renderRoundTripEquivalent leftExpr rightExpr
    (BodyStmtF leftExpr, BodyStmtF rightExpr) ->
      renderRoundTripEquivalent leftExpr rightExpr
    (LetStmtF leftMode leftBindings, LetStmtF rightMode rightBindings) ->
      leftMode == rightMode
        && equivalentListWith equivalentBinding leftBindings rightBindings
    _ ->
      False

equivalentField :: (NormalizedFieldLabel, Pattern HsExprF) -> (NormalizedFieldLabel, Pattern HsExprF) -> Bool
equivalentField (leftLabel, leftValue) (rightLabel, rightValue) =
  leftLabel == rightLabel
    && renderRoundTripEquivalent leftValue rightValue

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
