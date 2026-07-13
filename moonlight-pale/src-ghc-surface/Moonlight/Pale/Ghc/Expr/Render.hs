{-# LANGUAGE LambdaCase #-}

module Moonlight.Pale.Ghc.Expr.Render
  ( RenderRefusal (..),
    renderHsExpr,
    renderReadableHsExpr,
    renderTopLevelBinding,
    renderReadableTopLevelBinding,
    renderGeneratedTopLevelBinding,
    renderModuleSource,
    renderRoundTripEquivalent,
  )
where

import Data.ByteString qualified as ByteString
import Data.Char (isAlpha)
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import GHC.Types.Name.Occurrence (isSymOcc)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import Moonlight.Core (Pattern (..))
import Moonlight.Pale.Ghc.Expr.Equivalence (renderRoundTripEquivalent)
import Moonlight.Pale.Ghc.Expr.NameRender (renderRdrName, varRefRdrName)
import Moonlight.Pale.Ghc.Expr.Syntax

type RenderRefusal :: Type
data RenderRefusal
  = RenderOpaque !HsOpaqueTag
  | RenderGuardedExpression
  | RenderWhereExpression
  | RenderPatternVariable
  | RenderNonVarOperator
  | RenderPatOpaque !HsPatOpaqueTag
  | RenderEmptyBindingName
  | RenderClausesShape
  deriving stock (Eq, Ord, Show)

type WhereBindings :: Type
type WhereBindings = [(HsPatF, Pattern HsExprF)]

type BindingCore :: Type
data BindingCore = BindingCore ![BinderAnn] !(Maybe WhereBindings) !(Pattern HsExprF)

type WhereCore :: Type
data WhereCore = WhereCore !(Maybe WhereBindings) !(Pattern HsExprF)

type RenderMode :: Type
data RenderMode
  = CompactRender
  | GeneratedRender

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
    mergeTail :: (Int, String) -> [(Int, String)] -> [(Int, String)]
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
  flattenDoc <$> renderExprWith CompactRender 0 expressionValue

renderReadableHsExpr :: Pattern HsExprF -> Either RenderRefusal String
renderReadableHsExpr expressionValue =
  flattenDoc <$> renderExprWith GeneratedRender 0 expressionValue

renderTopLevelBinding :: String -> Pattern HsExprF -> Either RenderRefusal String
renderTopLevelBinding bindingName bindingTerm =
  flattenDoc <$> renderTopLevelBindingWith CompactRender bindingName bindingTerm

renderReadableTopLevelBinding :: String -> Pattern HsExprF -> Either RenderRefusal String
renderReadableTopLevelBinding bindingName bindingTerm =
  flattenDoc <$> renderTopLevelBindingWith GeneratedRender bindingName bindingTerm

renderGeneratedTopLevelBinding :: String -> Pattern HsExprF -> Either RenderRefusal String
renderGeneratedTopLevelBinding bindingName bindingTerm =
  renderReadableTopLevelBinding bindingName bindingTerm

renderTopLevelBindingWith :: RenderMode -> String -> Pattern HsExprF -> Either RenderRefusal Doc
renderTopLevelBindingWith renderMode bindingName bindingTerm
  | null bindingName =
      Left RenderEmptyBindingName
  | otherwise = do
      case bindingTerm of
        PatternNode (ClausesF clauseValues) ->
          renderTopLevelClauses renderMode bindingName clauseValues
        _ -> do
          let BindingCore binderAnns maybeWhereBindings bodyValue = collectTopLevelBindingCore bindingTerm
              lhsDoc = renderBindingLhs (renderDefinitionName bindingName) binderAnns
          equationDoc <- renderTopLevelEquation renderMode lhsDoc bodyValue
          appendWhereBlock renderMode maybeWhereBindings equationDoc

renderModuleSource :: String -> Maybe String -> [(String, Pattern HsExprF)] -> Either RenderRefusal String
renderModuleSource headerPrefix maybeModuleName renderedBindings = do
  bindingTexts <- traverse (uncurry renderTopLevelBinding) renderedBindings
  let prefixValue =
        if null headerPrefix
          then maybe "" (\moduleNameValue -> "module " <> moduleNameValue <> " where\n\n") maybeModuleName
          else headerPrefix
  Right (prefixValue <> intercalate "\n\n" bindingTexts <> "\n")

renderExprWith :: RenderMode -> Int -> Pattern HsExprF -> Either RenderRefusal Doc
renderExprWith renderMode parentPrecedence = \case
  PatternVar _ ->
    Left RenderPatternVariable
  PatternNode expressionValue ->
    case expressionValue of
      VarF variableReference ->
        Right (renderVarRefAtom variableReference)
      AppF functionValue argumentValue -> do
        functionDoc <- renderExprWith renderMode 10 functionValue
        argumentDoc <- renderExprWith renderMode 11 argumentValue
        Right (wrapParen (parentPrecedence > 10) (functionDoc <> text " " <> argumentDoc))
      LamF binderAnn bodyValue -> do
        bodyDoc <- renderExprWith renderMode 0 bodyValue
        Right (wrapParen (parentPrecedence > 0) (text "\\" <> renderBinderAnn binderAnn <> text " -> " <> bodyDoc))
      LetF letMode bindingValues bodyValue
        | lmProvenance letMode == WhereSyntax ->
            Left RenderWhereExpression
        | otherwise -> do
            renderLetExpression renderMode parentPrecedence bindingValues bodyValue
      OpAppF leftValue operatorValue rightValue ->
        renderOpAppChain renderMode parentPrecedence (PatternNode (OpAppF leftValue operatorValue rightValue))
      SectionLF leftValue operatorValue -> do
        leftDoc <- renderExprWith renderMode 0 leftValue
        operatorDoc <- renderOperator operatorValue
        Right (text "(" <> leftDoc <> text " " <> operatorDoc <> text ")")
      SectionRF operatorValue rightValue -> do
        operatorDoc <- renderOperator operatorValue
        rightDoc <- renderExprWith renderMode 0 rightValue
        Right (text "(" <> operatorDoc <> text " " <> rightDoc <> text ")")
      ParF innerValue -> do
        innerDoc <- renderExprWith renderMode 0 innerValue
        Right (parenthesizeDoc innerDoc)
      LitF literalValue ->
        Right (text (renderNormalizedLit literalValue))
      OverLitF literalValue ->
        Right (text (renderNormalizedOverLit literalValue))
      IfF conditionValue thenValue elseValue -> do
        conditionDoc <- renderExprWith renderMode 0 conditionValue
        thenDoc <- renderExprWith renderMode 0 thenValue
        elseDoc <- renderExprWith renderMode 0 elseValue
        Right (wrapParen (parentPrecedence > 0) (text "if " <> conditionDoc <> text " then " <> thenDoc <> text " else " <> elseDoc))
      CaseF scrutineeValue branchValues ->
        renderCaseExpression renderMode parentPrecedence scrutineeValue branchValues
      DoF statementValues -> do
        renderDoExpression renderMode parentPrecedence statementValues
      NegF innerValue -> do
        innerDoc <- renderExprWith renderMode 10 innerValue
        Right (wrapParen (parentPrecedence > 9) (text "-" <> innerDoc))
      ExplicitListF valueList -> do
        elementDocs <- traverse (renderExprWith renderMode 0) valueList
        Right (text "[" <> intercalateDoc (text ", ") elementDocs <> text "]")
      ExplicitTupleF valueList -> do
        elementDocs <- traverse (renderExprWith renderMode 0) valueList
        Right (text "(" <> intercalateDoc (text ", ") elementDocs <> text ")")
      RecordConF constructorValue fieldValues -> do
        constructorDoc <- renderExprWith renderMode 11 constructorValue
        fieldDocs <- traverse (renderField renderMode) fieldValues
        Right (renderRecordExpression renderMode constructorDoc fieldDocs)
      RecordUpdF recordValue fieldValues -> do
        recordDoc <- renderExprWith renderMode 11 recordValue
        fieldDocs <- traverse (renderField renderMode) fieldValues
        Right (renderRecordExpression renderMode recordDoc fieldDocs)
      ArithSeqF arithSeqValue ->
        renderArithSeq renderMode arithSeqValue
      GuardedF {} ->
        Left RenderGuardedExpression
      ClausesF clauseValues ->
        renderClausesExpression renderMode parentPrecedence clauseValues
      MultiIfF guardedAlts ->
        renderMultiIf renderMode parentPrecedence guardedAlts
      ExprWithTySigF bodyValue typeTextValue -> do
        bodyDoc <- renderExprWith renderMode 0 bodyValue
        Right (wrapParen (parentPrecedence > 0) (bodyDoc <> text " :: " <> renderTypeText typeTextValue))
      AppTypeF functionValue typeTextValue -> do
        functionDoc <- renderExprWith renderMode 10 functionValue
        Right (wrapParen (parentPrecedence > 10) (functionDoc <> text " @" <> renderTypeText typeTextValue))
      OpaqueF opaqueTag ->
        Left (RenderOpaque opaqueTag)

validateClauses :: [([HsPatF], Pattern HsExprF)] -> Either RenderRefusal [([HsPatF], Pattern HsExprF)]
validateClauses clauseValues =
  case clauseValues of
    [] ->
      Left RenderClausesShape
    [(patternValues, _)]
      | null patternValues || all isLambdaBinderPattern patternValues ->
          Left RenderClausesShape
    (firstPatterns, _) : _ ->
      let arityValue = length firstPatterns
       in if arityValue == 0 || any ((/= arityValue) . length . fst) clauseValues
            then Left RenderClausesShape
            else Right clauseValues

isLambdaBinderPattern :: HsPatF -> Bool
isLambdaBinderPattern = \case
  PVarP {} ->
    True
  PParP innerPattern ->
    isLambdaBinderPattern innerPattern
  PBangP innerPattern ->
    isLambdaBinderPattern innerPattern
  PLazyP innerPattern ->
    isLambdaBinderPattern innerPattern
  _ ->
    False

renderTopLevelClauses :: RenderMode -> String -> [([HsPatF], Pattern HsExprF)] -> Either RenderRefusal Doc
renderTopLevelClauses renderMode bindingName clauseValues = do
  validClauses <- validateClauses clauseValues
  vcat <$> traverse (renderTopLevelClause renderMode bindingName) validClauses

renderTopLevelClause :: RenderMode -> String -> ([HsPatF], Pattern HsExprF) -> Either RenderRefusal Doc
renderTopLevelClause renderMode bindingName (patternValues, bodyValue) = do
  lhsDoc <- renderClauseLhs (renderDefinitionName bindingName) patternValues
  renderTopLevelClauseBody renderMode lhsDoc bodyValue

renderTopLevelClauseBody :: RenderMode -> Doc -> Pattern HsExprF -> Either RenderRefusal Doc
renderTopLevelClauseBody renderMode lhsDoc bodyValue = do
  let WhereCore maybeWhereBindings coreBody = collectWhereCore bodyValue
  equationDoc <- renderTopLevelEquation renderMode lhsDoc coreBody
  appendWhereBlock renderMode maybeWhereBindings equationDoc

renderClausesExpression :: RenderMode -> Int -> [([HsPatF], Pattern HsExprF)] -> Either RenderRefusal Doc
renderClausesExpression renderMode parentPrecedence clauseValues = do
  validClauses <- validateClauses clauseValues
  case validClauses of
    [(patternValues, bodyValue)]
      | not (all isLambdaBinderPattern patternValues) && not (isGuardedBody bodyValue) ->
          renderPatternLambda renderMode parentPrecedence patternValues bodyValue
    _
      | all ((== 1) . length . fst) validClauses ->
          renderLambdaCase renderMode parentPrecedence validClauses
    _ ->
      renderLambdaCases renderMode parentPrecedence validClauses

isGuardedBody :: Pattern HsExprF -> Bool
isGuardedBody = \case
  PatternNode (GuardedF {}) ->
    True
  _ ->
    False

renderPatternLambda :: RenderMode -> Int -> [HsPatF] -> Pattern HsExprF -> Either RenderRefusal Doc
renderPatternLambda renderMode parentPrecedence patternValues bodyValue = do
  lhsDoc <- renderClausePatterns patternValues
  bodyDoc <- renderExprWith renderMode 0 bodyValue
  Right (wrapParen (parentPrecedence > 0) (text "\\" <> lhsDoc <> text " -> " <> bodyDoc))

renderLambdaCase :: RenderMode -> Int -> [([HsPatF], Pattern HsExprF)] -> Either RenderRefusal Doc
renderLambdaCase renderMode parentPrecedence clauseValues = do
  altDocs <- traverse (renderLambdaCaseAlt renderMode) clauseValues
  Right
    ( wrapParen (parentPrecedence > 0) $
        case renderMode of
          CompactRender ->
            text "\\case { " <> intercalateDoc (text "; ") altDocs <> text " }"
          GeneratedRender ->
            vcat [text "\\case", nest 2 (vcat altDocs)]
    )

renderLambdaCaseAlt :: RenderMode -> ([HsPatF], Pattern HsExprF) -> Either RenderRefusal Doc
renderLambdaCaseAlt renderMode = \case
  ([patternValue], bodyValue) -> do
    patternDoc <- renderPat False patternValue
    renderClauseArrow renderMode patternDoc bodyValue
  _ ->
    Left RenderClausesShape

renderLambdaCases :: RenderMode -> Int -> [([HsPatF], Pattern HsExprF)] -> Either RenderRefusal Doc
renderLambdaCases renderMode parentPrecedence clauseValues = do
  altDocs <- traverse (renderLambdaCasesAlt renderMode) clauseValues
  Right
    ( wrapParen (parentPrecedence > 0) $
        case renderMode of
          CompactRender ->
            text "\\cases { " <> intercalateDoc (text "; ") altDocs <> text " }"
          GeneratedRender ->
            vcat [text "\\cases", nest 2 (vcat altDocs)]
    )

renderLambdaCasesAlt :: RenderMode -> ([HsPatF], Pattern HsExprF) -> Either RenderRefusal Doc
renderLambdaCasesAlt renderMode (patternValues, bodyValue) = do
  lhsDoc <- renderClausePatterns patternValues
  renderClauseArrow renderMode lhsDoc bodyValue

renderClauseLhs :: Doc -> [HsPatF] -> Either RenderRefusal Doc
renderClauseLhs headDoc patternValues =
  (headDoc <+>) <$> renderClausePatterns patternValues

renderClausePatterns :: [HsPatF] -> Either RenderRefusal Doc
renderClausePatterns patternValues = do
  patternDocs <- traverse (renderPat True) patternValues
  Right (intercalateDoc (text " ") patternDocs)

renderClauseArrow :: RenderMode -> Doc -> Pattern HsExprF -> Either RenderRefusal Doc
renderClauseArrow renderMode lhsDoc bodyValue = do
  let WhereCore maybeWhereBindings coreBody = collectWhereCore bodyValue
  arrowDoc <- renderClauseArrowCore renderMode lhsDoc coreBody
  appendInlineWhereSuffix renderMode maybeWhereBindings arrowDoc

collectTopLevelBindingCore :: Pattern HsExprF -> BindingCore
collectTopLevelBindingCore bindingTerm =
  let (binderAnns, bodyValue) = collectLambdaSpine bindingTerm
      WhereCore maybeWhereBindings coreBody = collectWhereCore bodyValue
   in BindingCore binderAnns maybeWhereBindings coreBody

collectLambdaSpine :: Pattern HsExprF -> ([BinderAnn], Pattern HsExprF)
collectLambdaSpine =
  go []
  where
    go :: [BinderAnn] -> Pattern HsExprF -> ([BinderAnn], Pattern HsExprF)
    go binderAnns = \case
      PatternNode (LamF binderAnn bodyValue) ->
        go (binderAnn : binderAnns) bodyValue
      bodyValue ->
        (reverse binderAnns, bodyValue)

collectWhereCore :: Pattern HsExprF -> WhereCore
collectWhereCore = \case
  PatternNode (LetF letMode bindingValues bodyValue)
    | lmProvenance letMode == WhereSyntax ->
        WhereCore (Just bindingValues) bodyValue
  bodyValue ->
    WhereCore Nothing bodyValue

renderBindingLhs :: Doc -> [BinderAnn] -> Doc
renderBindingLhs headDoc binderAnns =
  case binderAnns of
    [] ->
      headDoc
    _ ->
      headDoc <> text " " <> intercalateDoc (text " ") (fmap renderBinderAnn binderAnns)

renderTopLevelEquation :: RenderMode -> Doc -> Pattern HsExprF -> Either RenderRefusal Doc
renderTopLevelEquation renderMode lhsDoc = \case
  PatternNode (GuardedF guardedAlts) ->
    renderGuardedTopLevelAlts renderMode lhsDoc guardedAlts
  bodyValue -> do
    bodyDoc <- renderExprWith renderMode 0 bodyValue
    Right (renderDelimitedExpression lhsDoc "=" bodyDoc)

renderClauseArrowCore :: RenderMode -> Doc -> Pattern HsExprF -> Either RenderRefusal Doc
renderClauseArrowCore renderMode lhsDoc = \case
  PatternNode (GuardedF guardedAlts) ->
    renderGuardedCaseAlts renderMode lhsDoc guardedAlts
  bodyValue -> do
    bodyDoc <- renderExprWith renderMode 0 bodyValue
    Right (renderDelimitedExpression lhsDoc "->" bodyDoc)

renderDelimitedExpression :: Doc -> String -> Doc -> Doc
renderDelimitedExpression lhsDoc delimiter bodyDoc =
  case docLines bodyDoc of
    _ : _ : _ ->
      vcat [lhsDoc <+> text delimiter, nest 2 bodyDoc]
    _ ->
      lhsDoc <+> text delimiter <+> bodyDoc

appendWhereBlock :: RenderMode -> Maybe WhereBindings -> Doc -> Either RenderRefusal Doc
appendWhereBlock renderMode maybeWhereBindings equationDoc =
  case maybeWhereBindings of
    Nothing ->
      Right equationDoc
    Just bindingValues -> do
      whereDoc <- renderWhereBlock renderMode bindingValues
      Right (vcat [equationDoc, whereDoc])

appendInlineWhereSuffix :: RenderMode -> Maybe WhereBindings -> Doc -> Either RenderRefusal Doc
appendInlineWhereSuffix renderMode maybeWhereBindings equationDoc =
  case maybeWhereBindings of
    Nothing ->
      Right equationDoc
    Just bindingValues -> do
      suffixDoc <- renderInlineWhereSuffix renderMode bindingValues
      Right (equationDoc <> suffixDoc)

renderWhereBlock :: RenderMode -> WhereBindings -> Either RenderRefusal Doc
renderWhereBlock renderMode bindingValues = do
  bindingDocs <- traverse (renderLetBinding renderMode) bindingValues
  Right (vcat [nest 2 (text "where"), nest 4 (vcat bindingDocs)])

renderInlineWhereSuffix :: RenderMode -> WhereBindings -> Either RenderRefusal Doc
renderInlineWhereSuffix renderMode bindingValues = do
  bindingDocs <- traverse (renderLetBinding renderMode) bindingValues
  Right $
    case renderMode of
      CompactRender ->
        text " where { " <> intercalateDoc (text "; ") bindingDocs <> text " }"
      GeneratedRender ->
        vcat [text " where", nest 2 (vcat bindingDocs)]

renderGuardedTopLevelAlts :: RenderMode -> Doc -> [GuardedAltF (Pattern HsExprF)] -> Either RenderRefusal Doc
renderGuardedTopLevelAlts renderMode lhsDoc guardedAlts =
  case guardedAlts of
    [] ->
      Left RenderGuardedExpression
    [GuardedAltF [] bodyValue] -> do
      bodyDoc <- renderExprWith renderMode 0 bodyValue
      Right (lhsDoc <> text " = " <> bodyDoc)
    _ -> do
      altDocs <- traverse (renderGuardedTopLevelAlt renderMode) guardedAlts
      Right (vcat (lhsDoc : fmap (nest 2) altDocs))

renderGuardedTopLevelAlt :: RenderMode -> GuardedAltF (Pattern HsExprF) -> Either RenderRefusal Doc
renderGuardedTopLevelAlt renderMode guardedAlt =
  case gaGuards guardedAlt of
    [] ->
      Left RenderGuardedExpression
    guardStatements -> do
      guardDoc <- renderGuardStatements renderMode guardStatements
      bodyDoc <- renderExprWith renderMode 0 (gaBody guardedAlt)
      Right (text "| " <> guardDoc <> text " = " <> bodyDoc)

renderOperator :: Pattern HsExprF -> Either RenderRefusal Doc
renderOperator = \case
  PatternNode (VarF variableReference) ->
    Right (renderVarRefOperator variableReference)
  PatternVar _ ->
    Left RenderPatternVariable
  _ ->
    Left RenderNonVarOperator

type OpAppChain :: Type
data OpAppChain = OpAppChain ![Pattern HsExprF] ![Pattern HsExprF]

renderOpAppChain :: RenderMode -> Int -> Pattern HsExprF -> Either RenderRefusal Doc
renderOpAppChain renderMode parentPrecedence expressionValue = do
  let OpAppChain operands operators =
        collectOpAppChain expressionValue
      chainPrecedence =
        foldr (min . operatorPrecedence) 9 operators
      operandPrecedence =
        chainPrecedence + 1
  operandDocs <- traverse (renderExprWith renderMode operandPrecedence) operands
  operatorDocs <- traverse renderOperator operators
  chainDoc <- maybe (Left RenderNonVarOperator) Right (intercalateOpAppDocs operandDocs operatorDocs)
  Right (wrapParen (parentPrecedence > chainPrecedence) chainDoc)

collectOpAppChain :: Pattern HsExprF -> OpAppChain
collectOpAppChain = \case
  PatternNode (OpAppF leftValue operatorValue rightValue) ->
    mergeOpAppChains (collectOpAppChain leftValue) operatorValue (collectOpAppChain rightValue)
  expressionValue ->
    OpAppChain [expressionValue] []

mergeOpAppChains :: OpAppChain -> Pattern HsExprF -> OpAppChain -> OpAppChain
mergeOpAppChains (OpAppChain leftOperands leftOperators) operatorValue (OpAppChain rightOperands rightOperators) =
  OpAppChain
    (leftOperands <> rightOperands)
    (leftOperators <> [operatorValue] <> rightOperators)

intercalateOpAppDocs :: [Doc] -> [Doc] -> Maybe Doc
intercalateOpAppDocs operandDocs operatorDocs =
  case operandDocs of
    [] ->
      Nothing
    firstOperand : remainingOperands ->
      appendRemaining firstOperand operatorDocs remainingOperands
  where
    appendRemaining currentDoc [] [] =
      Just currentDoc
    appendRemaining currentDoc (operatorDoc : remainingOperators) (operandDoc : remainingOperands) =
      appendRemaining
        (currentDoc <> text " " <> operatorDoc <> text " " <> operandDoc)
        remainingOperators
        remainingOperands
    appendRemaining _ _ _ =
      Nothing

operatorPrecedence :: Pattern HsExprF -> Int
operatorPrecedence operatorValue =
  fromMaybe 9 (operatorName operatorValue >>= knownOperatorPrecedence)

operatorName :: Pattern HsExprF -> Maybe String
operatorName = \case
  PatternNode (VarF variableReference) ->
    Just (renderRdrName (varRefRdrName variableReference))
  _ ->
    Nothing

knownOperatorPrecedence :: String -> Maybe Int
knownOperatorPrecedence nameValue =
  case lookup nameValue knownOperatorPrecedences of
    Just precedenceValue ->
      Just precedenceValue
    Nothing ->
      lookup (unqualifiedOperatorName nameValue) knownOperatorPrecedences

knownOperatorPrecedences :: [(String, Int)]
knownOperatorPrecedences =
  [ (".", 9),
    ("!!", 9),
    ("^", 8),
    ("^^", 8),
    ("**", 8),
    ("*", 7),
    ("/", 7),
    ("div", 7),
    ("mod", 7),
    ("rem", 7),
    ("quot", 7),
    ("+", 6),
    ("-", 6),
    (":", 5),
    ("++", 5),
    ("==", 4),
    ("/=", 4),
    ("<", 4),
    ("<=", 4),
    (">", 4),
    (">=", 4),
    ("elem", 4),
    ("notElem", 4),
    ("&&", 3),
    ("||", 2),
    (">>", 1),
    (">>=", 1),
    ("=<<", 1),
    ("$", 0),
    ("$!", 0),
    ("seq", 0)
  ]

unqualifiedOperatorName :: String -> String
unqualifiedOperatorName nameValue
  | all (== '.') nameValue =
      nameValue
  | otherwise =
      reverse (takeWhile (/= '.') (reverse nameValue))

renderCaseBranch :: RenderMode -> (HsPatF, Pattern HsExprF) -> Either RenderRefusal Doc
renderCaseBranch renderMode (casePattern, branchValue) = do
  patternDoc <- renderPat False casePattern
  renderClauseArrow renderMode patternDoc branchValue

renderCaseExpression :: RenderMode -> Int -> Pattern HsExprF -> [(HsPatF, Pattern HsExprF)] -> Either RenderRefusal Doc
renderCaseExpression renderMode parentPrecedence scrutineeValue branchValues = do
  scrutineeDoc <- renderExprWith renderMode 0 scrutineeValue
  branchDocs <- traverse (renderCaseBranch renderMode) branchValues
  Right
    ( wrapParen
        (parentPrecedence > 0)
        ( vcat
            [ text "case " <> scrutineeDoc <> text " of",
              nest 2 (vcat branchDocs)
            ]
        )
    )

renderGuardedCaseAlts :: RenderMode -> Doc -> [GuardedAltF (Pattern HsExprF)] -> Either RenderRefusal Doc
renderGuardedCaseAlts renderMode patternDoc guardedAlts =
  case guardedAlts of
    [] ->
      Left RenderGuardedExpression
    [GuardedAltF [] bodyValue] -> do
      bodyDoc <- renderExprWith renderMode 0 bodyValue
      Right (patternDoc <> text " -> " <> bodyDoc)
    _ -> do
      altDocs <- traverse (renderGuardedCaseAlt renderMode) guardedAlts
      Right (patternDoc <> hcat altDocs)

renderGuardedCaseAlt :: RenderMode -> GuardedAltF (Pattern HsExprF) -> Either RenderRefusal Doc
renderGuardedCaseAlt renderMode guardedAlt =
  case gaGuards guardedAlt of
    [] ->
      Left RenderGuardedExpression
    guardStatements -> do
      guardDoc <- renderGuardStatements renderMode guardStatements
      bodyDoc <- renderExprWith renderMode 0 (gaBody guardedAlt)
      Right (text " | " <> guardDoc <> text " -> " <> bodyDoc)

renderMultiIf :: RenderMode -> Int -> [GuardedAltF (Pattern HsExprF)] -> Either RenderRefusal Doc
renderMultiIf renderMode parentPrecedence guardedAlts =
  case guardedAlts of
    [] ->
      Left RenderGuardedExpression
    firstAlt : restAlts -> do
      firstDoc <- renderMultiIfAlt renderMode (text "if ") firstAlt
      restDocs <- traverse (renderMultiIfAlt renderMode (text "   ")) restAlts
      Right (wrapParen (parentPrecedence > 0) (vcat (firstDoc : restDocs)))

renderMultiIfAlt :: RenderMode -> Doc -> GuardedAltF (Pattern HsExprF) -> Either RenderRefusal Doc
renderMultiIfAlt renderMode prefixDoc guardedAlt =
  case gaGuards guardedAlt of
    [] ->
      Left RenderGuardedExpression
    guardStatements -> do
      guardDoc <- renderGuardStatements renderMode guardStatements
      bodyDoc <- renderExprWith renderMode 0 (gaBody guardedAlt)
      Right (prefixDoc <> text "| " <> guardDoc <> text " -> " <> bodyDoc)

renderDoExpression :: RenderMode -> Int -> [HsStmtF (Pattern HsExprF)] -> Either RenderRefusal Doc
renderDoExpression renderMode parentPrecedence statementValues =
  case renderMode of
    CompactRender -> do
      statementDocs <- traverse (renderDoStatement CompactRender) statementValues
      Right (wrapParen (parentPrecedence > 0) (text "do { " <> intercalateDoc (text "; ") statementDocs <> text " }"))
    GeneratedRender -> do
      statementDocs <- traverse (renderDoStatement GeneratedRender) statementValues
      Right (wrapParen (parentPrecedence > 0) (vcat (text "do" : fmap (nest 2) statementDocs)))

renderDoStatement :: RenderMode -> HsStmtF (Pattern HsExprF) -> Either RenderRefusal Doc
renderDoStatement renderMode = \case
  BindStmtF bindPattern rhsValue -> do
    patternDoc <- renderPat False bindPattern
    rhsDoc <- renderExprWith renderMode 0 rhsValue
    Right (patternDoc <> text " <- " <> rhsDoc)
  BodyStmtF exprValue ->
    renderExprWith renderMode 0 exprValue
  LetStmtF _ bindingValues ->
    renderLetStatement renderMode bindingValues

renderLetStatement :: RenderMode -> WhereBindings -> Either RenderRefusal Doc
renderLetStatement renderMode bindingValues = do
  bindingDocs <- traverse (renderLetBinding renderMode) bindingValues
  Right $
    case renderMode of
      CompactRender ->
        text "let { " <> intercalateDoc (text "; ") bindingDocs <> text " }"
      GeneratedRender ->
        vcat [text "let", nest 2 (vcat bindingDocs)]

renderLetExpression :: RenderMode -> Int -> WhereBindings -> Pattern HsExprF -> Either RenderRefusal Doc
renderLetExpression renderMode parentPrecedence bindingValues bodyValue =
  case renderMode of
    CompactRender -> do
      bindingDocs <- traverse (renderLetBinding CompactRender) bindingValues
      bodyDoc <- renderExprWith CompactRender 0 bodyValue
      Right (wrapParen (parentPrecedence > 0) (text "let " <> intercalateDoc (text "; ") bindingDocs <> text " in " <> bodyDoc))
    GeneratedRender -> do
      bindingDocs <- traverse (renderLetBinding GeneratedRender) bindingValues
      bodyDoc <- renderExprWith GeneratedRender 0 bodyValue
      Right
        ( wrapParen
            (parentPrecedence > 0)
            ( vcat
                [ text "let",
                  nest 2 (vcat bindingDocs),
                  text "in " <> bodyDoc
                ]
            )
        )

renderLetBinding :: RenderMode -> (HsPatF, Pattern HsExprF) -> Either RenderRefusal Doc
renderLetBinding renderMode (bindingPattern, rhsValue) = do
  patternDoc <- renderPat False bindingPattern
  case localBindingCore patternDoc bindingPattern rhsValue of
    Just (lhsDoc, maybeWhereBindings, coreBody) -> do
      equationDoc <- renderTopLevelEquation renderMode lhsDoc coreBody
      appendInlineWhereSuffix renderMode maybeWhereBindings equationDoc
    Nothing -> do
      rhsDoc <- renderExprWith renderMode 0 rhsValue
      Right (patternDoc <> text " = " <> rhsDoc)

localBindingCore :: Doc -> HsPatF -> Pattern HsExprF -> Maybe (Doc, Maybe WhereBindings, Pattern HsExprF)
localBindingCore patternDoc bindingPattern rhsValue
  | localFunctionBindingPattern bindingPattern =
      let (binderAnns, lambdaBody) = collectLambdaSpine rhsValue
          WhereCore maybeWhereBindings coreBody = collectWhereCore lambdaBody
       in case (binderAnns, maybeWhereBindings, coreBody) of
            ([], Nothing, PatternNode (GuardedF {})) ->
              Just (patternDoc, Nothing, coreBody)
            ([], Just {}, _) ->
              Just (patternDoc, maybeWhereBindings, coreBody)
            (_ : _, _, _) ->
              Just (renderBindingLhs patternDoc binderAnns, maybeWhereBindings, coreBody)
            _ ->
              Nothing
  | otherwise =
      Nothing

localFunctionBindingPattern :: HsPatF -> Bool
localFunctionBindingPattern = \case
  PVarP {} ->
    True
  PParP innerPattern ->
    localFunctionBindingPattern innerPattern
  _ ->
    False

renderGuardStatements :: RenderMode -> [HsGuardStmtF (Pattern HsExprF)] -> Either RenderRefusal Doc
renderGuardStatements renderMode guardStatements = do
  guardDocs <- traverse (renderGuardStatement renderMode) guardStatements
  Right (intercalateDoc (text ", ") guardDocs)

renderGuardStatement :: RenderMode -> HsGuardStmtF (Pattern HsExprF) -> Either RenderRefusal Doc
renderGuardStatement renderMode = \case
  GuardBoolF exprValue ->
    renderExprWith renderMode 0 exprValue
  GuardPatF patternValue rhsValue -> do
    patternDoc <- renderPat False patternValue
    rhsDoc <- renderExprWith renderMode 0 rhsValue
    Right (patternDoc <+> text "<-" <+> rhsDoc)
  GuardLetF _ bindingValues -> do
    bindingDocs <- traverse (renderLetBinding renderMode) bindingValues
    Right $
      case renderMode of
        CompactRender ->
          text "let { " <> intercalateDoc (text "; ") bindingDocs <> text " }"
        GeneratedRender ->
          vcat [text "let", nest 2 (vcat bindingDocs)]

renderPat :: Bool -> HsPatF -> Either RenderRefusal Doc
renderPat atomicContext = \case
  PVarP binderAnn ->
    Right (renderBinderAnn binderAnn)
  PWildP ->
    Right (text "_")
  PConP conName subPatterns ->
    case subPatterns of
      [] ->
        Right (renderConName conName)
      [leftPattern, rightPattern]
        | isSymOcc (rdrNameOcc conName) -> do
            leftDoc <- renderPat True leftPattern
            rightDoc <- renderPat True rightPattern
            Right (wrapParen atomicContext (leftDoc <> text (" " <> renderRdrName conName <> " ") <> rightDoc))
      _ -> do
        argDocs <- traverse (renderPat True) subPatterns
        Right (wrapParen atomicContext (intercalateDoc (text " ") (renderConName conName : argDocs)))
  PTupleP subPatterns -> do
    componentDocs <- traverse (renderPat False) subPatterns
    Right (text "(" <> intercalateDoc (text ", ") componentDocs <> text ")")
  PListP subPatterns -> do
    componentDocs <- traverse (renderPat False) subPatterns
    Right (text "[" <> intercalateDoc (text ", ") componentDocs <> text "]")
  PLitP literalValue ->
    Right (text (renderNormalizedLit literalValue))
  POverLitP literalValue ->
    Right (text (renderNormalizedOverLit literalValue))
  PAsP binderAnn subPattern -> do
    subDoc <- renderPat True subPattern
    Right (renderBinderAnn binderAnn <> text "@" <> subDoc)
  PBangP subPattern -> do
    subDoc <- renderPat True subPattern
    Right (text "!" <> subDoc)
  PLazyP subPattern -> do
    subDoc <- renderPat True subPattern
    Right (text "~" <> subDoc)
  PParP subPattern -> do
    subDoc <- renderPat False subPattern
    Right (parenthesizeDoc subDoc)
  PRecP conName fieldPatterns ->
    renderRecordPattern conName fieldPatterns
  PLossyP tagValue _ ->
    Left (RenderPatOpaque tagValue)

renderConName :: RdrName -> Doc
renderConName conName =
  if isSymOcc (rdrNameOcc conName)
    then text ("(" <> renderRdrName conName <> ")")
    else text (renderRdrName conName)

renderField :: RenderMode -> (NormalizedFieldLabel, Pattern HsExprF) -> Either RenderRefusal Doc
renderField renderMode (fieldLabelValue, fieldValue) = do
  fieldDoc <- renderExprWith renderMode 0 fieldValue
  Right (text (nflSelector fieldLabelValue) <> text " = " <> fieldDoc)

renderRecordExpression :: RenderMode -> Doc -> [Doc] -> Doc
renderRecordExpression renderMode headDoc fieldDocs =
  case (renderMode, fieldDocs) of
    (CompactRender, _) ->
      headDoc <> text " { " <> intercalateDoc (text ", ") fieldDocs <> text " }"
    (GeneratedRender, []) ->
      headDoc <> text " {}"
    (GeneratedRender, firstField : remainingFields) ->
      vcat
        [ headDoc,
          nest 2
            ( vcat
                ( (text "{ " <> firstField)
                    : fmap (text ", " <>) remainingFields
                    <> [text "}"]
                )
            )
        ]

renderRecordPattern :: RdrName -> [(String, HsPatF)] -> Either RenderRefusal Doc
renderRecordPattern conName fieldPatterns =
  case fieldPatterns of
    [] ->
      Right (renderConName conName <> text " {}")
    _ -> do
      fieldDocs <- traverse renderPatternField fieldPatterns
      Right (renderConName conName <> text " {" <> intercalateDoc (text ", ") fieldDocs <> text "}")

renderPatternField :: (String, HsPatF) -> Either RenderRefusal Doc
renderPatternField (fieldName, fieldPattern) = do
  fieldDoc <- renderPat False fieldPattern
  Right (text fieldName <> text " = " <> fieldDoc)

renderArithSeq :: RenderMode -> NormalizedArithSeq (Pattern HsExprF) -> Either RenderRefusal Doc
renderArithSeq renderMode = \case
  ArithSeqFrom fromValue -> do
    fromDoc <- renderExprWith renderMode 0 fromValue
    Right (text "[" <> fromDoc <> text " ..]")
  ArithSeqFromThen fromValue thenValue -> do
    fromDoc <- renderExprWith renderMode 0 fromValue
    thenDoc <- renderExprWith renderMode 0 thenValue
    Right (text "[" <> fromDoc <> text ", " <> thenDoc <> text " ..]")
  ArithSeqFromTo fromValue toValue -> do
    fromDoc <- renderExprWith renderMode 0 fromValue
    toDoc <- renderExprWith renderMode 0 toValue
    Right (text "[" <> fromDoc <> text " .. " <> toDoc <> text "]")
  ArithSeqFromThenTo fromValue thenValue toValue -> do
    fromDoc <- renderExprWith renderMode 0 fromValue
    thenDoc <- renderExprWith renderMode 0 thenValue
    toDoc <- renderExprWith renderMode 0 toValue
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

renderTypeText :: NormalizedTypeText -> Doc
renderTypeText =
  text . nttText

wrapParen :: Bool -> Doc -> Doc
wrapParen shouldWrap innerDoc =
  if shouldWrap then parenthesizeDoc innerDoc else innerDoc

parenthesizeDoc :: Doc -> Doc
parenthesizeDoc innerDoc =
  case docLines innerDoc of
    _ : _ : _ ->
      vcat [text "(", nest 2 innerDoc, text ")"]
    _ ->
      text "(" <> innerDoc <> text ")"

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
