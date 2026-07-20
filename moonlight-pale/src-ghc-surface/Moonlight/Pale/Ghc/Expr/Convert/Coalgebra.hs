{-# LANGUAGE LambdaCase #-}

module Moonlight.Pale.Ghc.Expr.Convert.Coalgebra
  ( TopLevelBinding (..),
    ConvertedModule (..),
    ConvertObstruction (..),
    convertHsExpr,
    convertModule,
    convertHaskellSource,
  )
where

import Control.Monad.State.Strict (StateT, evalStateT, gets, modify', runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Kind (Type)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Vector qualified as V
import GHC.Hs
  ( ArithSeqInfo (..),
    ExprLStmt,
    FieldOcc (..),
    GRHS (..),
    GRHSs (..),
    GuardLStmt,
    GhcPs,
    HsBind,
    HsBindLR (..),
    HsConDetails (..),
    HsDecl (..),
    HsExpr (..),
    HsFieldBind (..),
    HsLocalBinds,
    HsLocalBindsLR (..),
    HsModule (..),
    HsRecField,
    HsRecFields (..),
    HsTupArg (..),
    HsValBindsLR (..),
    LGRHS,
    LHsDecl,
    LHsExpr,
    LHsRecUpdFields (..),
    LMatch,
    LPat,
    Match (..),
    MatchGroup (..),
    Pat (..),
    StmtLR (..),
  )
import GHC.Hs.Utils (CollectFlag (CollNoDictBinders), collectPatBinders)
import GHC.Parser.Annotation (getLocA)
import GHC.Types.Basic (Boxity (..))
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Types.SrcLoc
  ( SrcSpan (..),
    srcSpanEndCol,
    srcSpanEndLine,
    srcSpanStartCol,
    srcSpanStartLine,
    unLoc,
  )
import GHC.Utils.Outputable (Outputable, ppr, showSDocUnsafe)
import Moonlight.Core (BinderId (..), Pattern, binderIdKey)
import Moonlight.Pale.Ghc.Expr.Convert.FreeScopes (ScopeAlgebra (..))
import Moonlight.Pale.Ghc.Expr.Convert.FreeScopes qualified as FreeScopes
import Moonlight.Pale.Ghc.Expr.Convert.Obstruction
import Moonlight.Pale.Ghc.Expr.Opaque
import Moonlight.Pale.Ghc.Expr.Scope
import Moonlight.Pale.Ghc.Expr.Syntax
import Moonlight.Pale.Ghc.ModuleSurface (parseHsModule)

type Env :: Type
type Env = Map RdrName BinderAnn

type TopLevelBinding :: Type
data TopLevelBinding = TopLevelBinding
  { tlbNames :: [RdrName],
    tlbScopedTerm :: !ScopedExpr,
    tlbSpannedTerm :: !SpannedExpr,
    tlbTerm :: !(Pattern HsExprF),
    tlbRegion :: !(Maybe SourceRegion)
  }

type ConvertedModule :: Type
data ConvertedModule = ConvertedModule
  { cmBindings :: ![TopLevelBinding],
    cmScopeIndex :: !ScopeIndex,
    cmLambdaSites :: ![BinderAnn],
    cmLetSites :: ![(BinderAnn, LetProvenance)]
  }

type ConvertedLocalBinds :: Type
data ConvertedLocalBinds = ConvertedLocalBinds
  { clbMode :: !LetMode,
    clbScope :: !ScopeId,
    clbBindings :: ![(HsPatF, ConvExpr)],
    clbBinders :: ![BinderAnn],
    clbEnv :: !Env
  }

type ConvExpr :: Type
data ConvExpr = ConvExpr
  { cxScoped :: !ScopedExpr,
    cxSpanned :: !SpannedExpr
  }

type ConvState :: Type
data ConvState = ConvState
  { csNextBinderId :: !Int,
    csNextScopeId :: !Int,
    csCurrentScope :: !ScopeId,
    csScopeParentsRev :: ![Int],
    csScopeDepths :: !(IntMap Int),
    csBinderIntroRev :: ![Int],
    csBinderIntroMap :: !(IntMap ScopeId),
    csLambdaSites :: ![BinderAnn],
    csLetSites :: ![(BinderAnn, LetProvenance)]
  }

type ConvM :: Type -> Type
type ConvM = StateT ConvState (Either ConvertObstruction)

throwConvert :: ConvertObstruction -> ConvM value
throwConvert =
  lift . Left

liftConvertEither :: Either ConvertObstruction value -> ConvM value
liftConvertEither =
  either throwConvert pure

convertHsExpr :: HsExpr GhcPs -> Either ConvertObstruction (Pattern HsExprF)
convertHsExpr exprValue =
  eraseScopedExpr . cxScoped <$> evalStateT (convertExpr Map.empty Nothing exprValue) initialConvState

convertModule :: HsModule GhcPs -> Either ConvertObstruction ConvertedModule
convertModule moduleValue = do
  (bindings, finalState) <-
    runStateT
      ( fmap concat
          (traverse convertLocatedDecl (hsmodDecls moduleValue))
      )
      initialConvState
  let scopeParents = V.fromList (reverse (csScopeParentsRev finalState))
      binderIntro = V.fromList (reverse (csBinderIntroRev finalState))
  scopeIndex <-
    either
      (Left . ConvertScopeIndexFailure)
      Right
      (mkScopeIndex scopeParents binderIntro)
  pure
    ConvertedModule
      { cmBindings = bindings,
        cmScopeIndex = scopeIndex,
        cmLambdaSites = reverse (csLambdaSites finalState),
        cmLetSites = reverse (csLetSites finalState)
      }

convertHaskellSource :: FilePath -> String -> Either ConvertObstruction ConvertedModule
convertHaskellSource sourcePath moduleContents =
  either (Left . ConvertParseFailure) convertModule (parseHsModule sourcePath moduleContents)

initialConvState :: ConvState
initialConvState =
  ConvState
    { csNextBinderId = 0,
      csNextScopeId = 1,
      csCurrentScope = rootScopeId,
      csScopeParentsRev = [0],
      csScopeDepths = IntMap.singleton 0 0,
      csBinderIntroRev = [],
      csBinderIntroMap = IntMap.empty,
      csLambdaSites = [],
      csLetSites = []
    }

convertLocatedDecl :: LHsDecl GhcPs -> ConvM [TopLevelBinding]
convertLocatedDecl locatedDecl =
  convertDecl (sourceRegionFromSrcSpan (getLocA locatedDecl)) (unLoc locatedDecl)

sourceRegionFromSrcSpan :: SrcSpan -> Maybe SourceRegion
sourceRegionFromSrcSpan = \case
  RealSrcSpan realSrcSpan _ ->
    Just
      SourceRegion
        { srStartLine = srcSpanStartLine realSrcSpan,
          srStartCol = srcSpanStartCol realSrcSpan,
          srEndLine = srcSpanEndLine realSrcSpan,
          srEndCol = srcSpanEndCol realSrcSpan
        }
  UnhelpfulSpan _ ->
    Nothing

convertDecl :: Maybe SourceRegion -> HsDecl GhcPs -> ConvM [TopLevelBinding]
convertDecl declRegion = \case
  ValD _ bindValue
    | convertibleTopLevelBind bindValue ->
        (: []) <$> convertTopLevelBind declRegion bindValue
  _ -> pure []

convertibleTopLevelBind :: HsBind GhcPs -> Bool
convertibleTopLevelBind = \case
  PatBind {pat_lhs = patternValue} ->
    maybe False (const True) (simpleLambdaBinderName (unLoc patternValue))
  _ ->
    True

convertTopLevelBind :: Maybe SourceRegion -> HsBind GhcPs -> ConvM TopLevelBinding
convertTopLevelBind declRegion bindValue = do
  bindingScope <- freshChildScope
  convertedTerm <- withTopLevelRegion declRegion <$> withScope bindingScope (convertLocalBind Map.empty bindValue)
  pure
    TopLevelBinding
      { tlbNames = localBindNames bindValue,
        tlbScopedTerm = cxScoped convertedTerm,
        tlbSpannedTerm = cxSpanned convertedTerm,
        tlbTerm = eraseScopedExpr (cxScoped convertedTerm),
        tlbRegion = declRegion
      }

withTopLevelRegion :: Maybe SourceRegion -> ConvExpr -> ConvExpr
withTopLevelRegion Nothing convertedExpr =
  convertedExpr
withTopLevelRegion (Just region) convertedExpr =
  convertedExpr
    { cxSpanned =
        (cxSpanned convertedExpr)
          { sxRegion = Just region
          }
    }

convertExpr :: Env -> Maybe SourceRegion -> HsExpr GhcPs -> ConvM ConvExpr
convertExpr env region = \case
  HsVar _ nameValue ->
    mkConvExpr region (VarF (resolveVarRef env (unLoc nameValue)))
  HsOverLabel {} ->
    opaqueConvExpr region OpaqueOverLabel
  HsIPVar {} ->
    opaqueConvExpr region OpaqueIPVar
  HsOverLit _ overLitValue ->
    mkConvExpr region (OverLitF (normalizeHsOverLit overLitValue))
  HsLit _ literalValue ->
    mkConvExpr region (LitF (normalizeHsLit literalValue))
  HsLam _ _ matchGroupValue ->
    convertLambdaLikeMatchGroup env region matchGroupValue
  HsApp _ functionValue argumentValue -> do
    functionExpr <- convertLocatedExpr env functionValue
    argumentExpr <- convertLocatedExpr env argumentValue
    mkConvExpr region (AppF functionExpr argumentExpr)
  HsAppType _ exprValue typeValue -> do
    innerExpr <- convertLocatedExpr env exprValue
    mkConvExpr region (AppTypeF innerExpr (normalizedTypeText typeValue))
  OpApp _ leftValue operatorValue rightValue -> do
    leftExpr <- convertLocatedExpr env leftValue
    operatorExpr <- convertLocatedExpr env operatorValue
    rightExpr <- convertLocatedExpr env rightValue
    mkConvExpr region (OpAppF leftExpr operatorExpr rightExpr)
  NegApp _ exprValue _ -> do
    innerExpr <- convertLocatedExpr env exprValue
    mkConvExpr region (NegF innerExpr)
  HsPar _ exprValue -> do
    innerExpr <- convertLocatedExpr env exprValue
    mkConvExpr region (ParF innerExpr)
  SectionL _ exprValue operatorValue -> do
    leftExpr <- convertLocatedExpr env exprValue
    operatorExpr <- convertLocatedExpr env operatorValue
    mkConvExpr region (SectionLF leftExpr operatorExpr)
  SectionR _ operatorValue exprValue -> do
    operatorExpr <- convertLocatedExpr env operatorValue
    rightExpr <- convertLocatedExpr env exprValue
    mkConvExpr region (SectionRF operatorExpr rightExpr)
  ExplicitTuple _ tupleArgs _ -> do
    tupleExprs <- traverse (convertTupleArg env) tupleArgs
    mkConvExpr region (ExplicitTupleF (mapMaybe id tupleExprs))
  ExplicitSum {} ->
    opaqueConvExpr region OpaqueExplicitSum
  HsCase _ scrutineeValue matchGroupValue -> do
    scrutineeExpr <- convertLocatedExpr env scrutineeValue
    alternatives <- convertCaseAlternatives env matchGroupValue
    mkConvExpr region (CaseF scrutineeExpr alternatives)
  HsIf _ conditionValue thenValue elseValue -> do
    conditionExpr <- convertLocatedExpr env conditionValue
    thenExpr <- convertLocatedExpr env thenValue
    elseExpr <- convertLocatedExpr env elseValue
    mkConvExpr region (IfF conditionExpr thenExpr elseExpr)
  HsMultiIf _ grhsValues -> do
    guardedAlts <- traverse (convertGuardedAlt env) (NonEmpty.toList grhsValues)
    mkConvExpr region (MultiIfF guardedAlts)
  HsLet _ localBindsValue bodyValue ->
    convertLocalBinds env LetSyntax localBindsValue >>= \case
      Nothing ->
        opaqueConvExpr region OpaqueXLocalBinds
      Just convertedBinds -> do
        bodyExpr <-
          withScope
            (clbScope convertedBinds)
            (convertLocatedExpr (clbEnv convertedBinds) bodyValue)
        mkConvExpr region (LetF (clbMode convertedBinds) (clbBindings convertedBinds) bodyExpr)
  HsDo _ _ statementValues ->
    convertStatements env (unLoc statementValues) >>= \case
      Nothing ->
        opaqueConvExpr region OpaqueUnsupportedStmt
      Just statements ->
        mkConvExpr region (DoF statements)
  ExplicitList _ exprValues -> do
    listExprs <- traverse (convertLocatedExpr env) exprValues
    mkConvExpr region (ExplicitListF listExprs)
  RecordCon {rcon_con = constructorValue, rcon_flds = recordFieldsValue} -> do
    constructorExpr <- mkConvExpr Nothing (VarF (GlobalName (unLoc constructorValue)))
    fieldValues <- convertRecordFields env recordFieldsValue
    mkConvExpr region (RecordConF constructorExpr fieldValues)
  RecordUpd {rupd_expr = recordValue, rupd_flds = recordFieldsValue} ->
    convertRecordUpdFields env recordFieldsValue >>= \case
      Nothing ->
        opaqueConvExpr region OpaqueRecordUpd
      Just fieldValues -> do
        recordExpr <- convertLocatedExpr env recordValue
        mkConvExpr region (RecordUpdF recordExpr fieldValues)
  HsGetField {} ->
    opaqueConvExpr region OpaqueGetField
  HsProjection {} ->
    opaqueConvExpr region OpaqueProjection
  ExprWithTySig _ exprValue sigValue -> do
    innerExpr <- convertLocatedExpr env exprValue
    mkConvExpr region (ExprWithTySigF innerExpr (normalizedTypeText sigValue))
  ArithSeq _ _ arithSeqValue -> do
    convertedSeq <- convertArithSeq env arithSeqValue
    mkConvExpr region (ArithSeqF convertedSeq)
  HsTypedBracket {} ->
    opaqueConvExpr region OpaqueTypedBracket
  HsUntypedBracket {} ->
    opaqueConvExpr region OpaqueUntypedBracket
  HsTypedSplice {} ->
    opaqueConvExpr region OpaqueTypedSplice
  HsUntypedSplice {} ->
    opaqueConvExpr region OpaqueUntypedSplice
  HsProc {} ->
    opaqueConvExpr region OpaqueProc
  HsStatic {} ->
    opaqueConvExpr region OpaqueStatic
  HsPragE {} ->
    opaqueConvExpr region OpaquePragE
  HsEmbTy {} ->
    opaqueConvExpr region OpaqueEmbTy
  HsHole {} ->
    opaqueConvExpr region OpaqueHole
  HsForAll {} ->
    opaqueConvExpr region OpaqueForAll
  HsQual {} ->
    opaqueConvExpr region OpaqueQual
  HsFunArr {} ->
    opaqueConvExpr region OpaqueFunArr

convertLocatedExpr :: Env -> LHsExpr GhcPs -> ConvM ConvExpr
convertLocatedExpr env locatedExpr =
  convertExpr env (sourceRegionFromSrcSpan (getLocA locatedExpr)) (unLoc locatedExpr)

convertLambdaLikeMatchGroup :: Env -> Maybe SourceRegion -> MatchGroup GhcPs (LHsExpr GhcPs) -> ConvM ConvExpr
convertLambdaLikeMatchGroup env region = \case
  MG {mg_alts = alternativesValue} ->
    case unLoc alternativesValue of
      [matchValue]
        | Just binderNames <- simpleLambdaBinderNames (unLoc matchValue) ->
            convertLambdaBinders env region binderNames (m_grhss (unLoc matchValue))
      matchValues ->
        convertClauses env region matchValues

simpleLambdaBinderNames :: Match GhcPs (LHsExpr GhcPs) -> Maybe [RdrName]
simpleLambdaBinderNames matchValue =
  traverse (simpleLambdaBinderName . unLoc) (unLoc (m_pats matchValue))

convertClauses :: Env -> Maybe SourceRegion -> [LMatch GhcPs (LHsExpr GhcPs)] -> ConvM ConvExpr
convertClauses env region matchValues = do
  clauseValues <- traverse (convertClause env . unLoc) matchValues
  mkConvExpr region (ClausesF clauseValues)

convertClause :: Env -> Match GhcPs (LHsExpr GhcPs) -> ConvM ([HsPatF], ConvExpr)
convertClause env matchValue = do
  let patternValues = unLoc (m_pats matchValue)
      binderNames = concatMap collectPatternNames patternValues
  if null binderNames
    then do
      clausePatterns <- traverse convertPat patternValues
      bodyExpr <- convertGRHSs env (m_grhss matchValue)
      pure (clausePatterns, bodyExpr)
    else do
      childScope <- freshChildScope
      withScope childScope $ do
        clausePatterns <- traverse convertPat patternValues
        let extendedEnv = extendEnv env (concatMap patBinders clausePatterns)
        bodyExpr <- convertGRHSs extendedEnv (m_grhss matchValue)
        pure (clausePatterns, bodyExpr)

convertLambdaBinders :: Env -> Maybe SourceRegion -> [RdrName] -> GRHSs GhcPs (LHsExpr GhcPs) -> ConvM ConvExpr
convertLambdaBinders env region binderNames grhssValue =
  case binderNames of
    [] ->
      convertGRHSs env grhssValue
    binderName : remainingNames -> do
      childScope <- freshChildScope
      (binderAnn, bodyExpr) <-
        withScope childScope $ do
          binderAnn <- freshBinderAnn binderName
          recordLambdaSite binderAnn
          bodyExpr <-
            convertLambdaBinders
              (extendEnv env [binderAnn])
              Nothing
              remainingNames
              grhssValue
          pure (binderAnn, bodyExpr)
      mkConvExpr region (LamF binderAnn bodyExpr)

convertCaseAlternatives :: Env -> MatchGroup GhcPs (LHsExpr GhcPs) -> ConvM [(HsPatF, ConvExpr)]
convertCaseAlternatives env = \case
  MG {mg_alts = alternativesValue} ->
    traverse (convertCaseAlternative env . unLoc) (unLoc alternativesValue)

convertCaseAlternative :: Env -> Match GhcPs (LHsExpr GhcPs) -> ConvM (HsPatF, ConvExpr)
convertCaseAlternative env matchValue =
  case unLoc (m_pats matchValue) of
    [patternValue] -> do
      let binderNames = collectPatternNames patternValue
      if null binderNames
        then do
          casePattern <- convertPat patternValue
          rhsExpr <- convertGRHSs env (m_grhss matchValue)
          pure (casePattern, rhsExpr)
        else do
          childScope <- freshChildScope
          withScope childScope $ do
            casePattern <- convertPat patternValue
            let extendedEnv = extendEnv env (patBinders casePattern)
            rhsExpr <- convertGRHSs extendedEnv (m_grhss matchValue)
            pure (casePattern, rhsExpr)
    _ -> do
      fallback <- opaqueConvExpr Nothing OpaqueCaseAlternative
      pure (PWildP, fallback)

convertGRHSs :: Env -> GRHSs GhcPs (LHsExpr GhcPs) -> ConvM ConvExpr
convertGRHSs env grhssValue =
  convertLocalBindsMaybe env WhereSyntax (grhssLocalBinds grhssValue) >>= \case
    Nothing ->
      opaqueConvExpr Nothing OpaqueXLocalBinds
    Just Nothing ->
      convertGuardedRHSs env (NonEmpty.toList (grhssGRHSs grhssValue))
    Just (Just convertedBinds) -> do
      bodyExpr <-
        withScope
          (clbScope convertedBinds)
          ( convertGuardedRHSs
              (clbEnv convertedBinds)
              (NonEmpty.toList (grhssGRHSs grhssValue))
          )
      mkConvExpr Nothing (LetF (clbMode convertedBinds) (clbBindings convertedBinds) bodyExpr)

convertGuardedRHSs :: Env -> [LGRHS GhcPs (LHsExpr GhcPs)] -> ConvM ConvExpr
convertGuardedRHSs env = \case
  [] ->
    mkConvExpr Nothing (GuardedF [])
  [grhsValue] ->
    case unLoc grhsValue of
      GRHS _ [] bodyValue ->
        convertLocatedExpr env bodyValue
      _ -> do
        guardedAlt <- convertGuardedAlt env grhsValue
        mkConvExpr Nothing (GuardedF [guardedAlt])
  grhsValues -> do
    guardedAlts <- traverse (convertGuardedAlt env) grhsValues
    mkConvExpr Nothing (GuardedF guardedAlts)

convertGuardedAlt :: Env -> LGRHS GhcPs (LHsExpr GhcPs) -> ConvM (GuardedAltF ConvExpr)
convertGuardedAlt env grhsValue =
  case unLoc grhsValue of
    GRHS _ guardValues bodyValue ->
      convertGuardedAltBody env guardValues bodyValue >>= \case
        Nothing ->
          unsupportedGuardedAlt
        Just (guardStatements, bodyExpr) ->
          pure
            GuardedAltF
              { gaGuards = guardStatements,
                gaBody = bodyExpr
              }

unsupportedGuardedAlt :: ConvM (GuardedAltF ConvExpr)
unsupportedGuardedAlt = do
  fallbackExpr <- opaqueConvExpr Nothing OpaqueUnsupportedGuard
  pure
    GuardedAltF
      { gaGuards = [],
        gaBody = fallbackExpr
      }

convertGuardedAltBody ::
  Env ->
  [GuardLStmt GhcPs] ->
  LHsExpr GhcPs ->
  ConvM (Maybe ([HsGuardStmtF ConvExpr], ConvExpr))
convertGuardedAltBody env guardValues bodyValue =
  case guardValues of
    [] -> do
      bodyExpr <- convertLocatedExpr env bodyValue
      pure (Just ([], bodyExpr))
    guardValue : remainingValues ->
      case unLoc guardValue of
        BodyStmt _ exprValue _ _ -> do
          guardExpr <- convertLocatedExpr env exprValue
          fmap
            (fmap (prependGuardStatement (GuardBoolF guardExpr)))
            (convertGuardedAltBody env remainingValues bodyValue)
        LastStmt _ exprValue _ _ -> do
          guardExpr <- convertLocatedExpr env exprValue
          fmap
            (fmap (prependGuardStatement (GuardBoolF guardExpr)))
            (convertGuardedAltBody env remainingValues bodyValue)
        BindStmt _ patternValue rhsValue -> do
          rhsExpr <- convertLocatedExpr env rhsValue
          let binderNames = collectPatternNames patternValue
          if null binderNames
            then do
              bindPattern <- convertPat patternValue
              fmap
                (fmap (prependGuardStatement (GuardPatF bindPattern rhsExpr)))
                (convertGuardedAltBody env remainingValues bodyValue)
            else do
              childScope <- freshChildScope
              withScope childScope $ do
                bindPattern <- convertPat patternValue
                let extendedEnv = extendEnv env (patBinders bindPattern)
                fmap
                  (fmap (prependGuardStatement (GuardPatF bindPattern rhsExpr)))
                  (convertGuardedAltBody extendedEnv remainingValues bodyValue)
        LetStmt _ localBindsValue ->
          convertLocalBinds env LetSyntax localBindsValue >>= \case
            Nothing ->
              pure Nothing
            Just convertedBinds ->
              fmap
                (fmap (prependGuardStatement (GuardLetF (clbMode convertedBinds) (clbBindings convertedBinds))))
                ( withScope
                    (clbScope convertedBinds)
                    (convertGuardedAltBody (clbEnv convertedBinds) remainingValues bodyValue)
                )
        ParStmt {} ->
          pure Nothing
        TransStmt {} ->
          pure Nothing
        RecStmt {} ->
          pure Nothing

prependGuardStatement ::
  HsGuardStmtF ConvExpr ->
  ([HsGuardStmtF ConvExpr], ConvExpr) ->
  ([HsGuardStmtF ConvExpr], ConvExpr)
prependGuardStatement guardStatement (guardStatements, bodyExpr) =
  (guardStatement : guardStatements, bodyExpr)

convertStatements :: Env -> [ExprLStmt GhcPs] -> ConvM (Maybe [HsStmtF ConvExpr])
convertStatements env = \case
  [] ->
    pure (Just [])
  statementValue : remainingValues ->
    case unLoc statementValue of
      BindStmt _ patternValue rhsValue -> do
        rhsExpr <- convertLocatedExpr env rhsValue
        let binderNames = collectPatternNames patternValue
        if null binderNames
          then do
            bindPattern <- convertPat patternValue
            fmap
              (fmap (BindStmtF bindPattern rhsExpr :))
              (convertStatements env remainingValues)
          else do
            childScope <- freshChildScope
            withScope childScope $ do
              bindPattern <- convertPat patternValue
              let extendedEnv = extendEnv env (patBinders bindPattern)
              convertStatements extendedEnv remainingValues >>= \case
                Nothing ->
                  pure Nothing
                Just remainingStatements ->
                  pure (Just (BindStmtF bindPattern rhsExpr : remainingStatements))
      BodyStmt _ exprValue _ _ -> do
        bodyExpr <- convertLocatedExpr env exprValue
        fmap (fmap (BodyStmtF bodyExpr :)) (convertStatements env remainingValues)
      LastStmt _ exprValue _ _ -> do
        bodyExpr <- convertLocatedExpr env exprValue
        pure (Just [BodyStmtF bodyExpr])
      LetStmt _ localBindsValue ->
        convertLocalBinds env LetSyntax localBindsValue >>= \case
          Nothing ->
            pure Nothing
          Just convertedBinds ->
            withScope
              (clbScope convertedBinds)
              (convertStatements (clbEnv convertedBinds) remainingValues)
              >>= \case
                Nothing ->
                  pure Nothing
                Just remainingStatements ->
                  pure
                    (Just (LetStmtF (clbMode convertedBinds) (clbBindings convertedBinds) : remainingStatements))
      ParStmt {} ->
        pure Nothing
      TransStmt {} ->
        pure Nothing
      RecStmt {} ->
        pure Nothing

convertLocalBindsMaybe :: Env -> LetProvenance -> HsLocalBinds GhcPs -> ConvM (Maybe (Maybe ConvertedLocalBinds))
convertLocalBindsMaybe env provenanceValue = \case
  EmptyLocalBinds _ ->
    pure (Just Nothing)
  HsValBinds _ valBindsValue ->
    fmap Just <$> convertValBinds env provenanceValue valBindsValue
  HsIPBinds {} ->
    pure Nothing

convertLocalBinds :: Env -> LetProvenance -> HsLocalBinds GhcPs -> ConvM (Maybe ConvertedLocalBinds)
convertLocalBinds env provenanceValue localBindsValue =
  convertLocalBindsMaybe env provenanceValue localBindsValue >>= \case
    Nothing -> pure Nothing
    Just maybeBinds -> pure maybeBinds

convertValBinds :: Env -> LetProvenance -> HsValBindsLR GhcPs GhcPs -> ConvM (Maybe ConvertedLocalBinds)
convertValBinds env provenanceValue = \case
  ValBinds _ bindsValue _ -> do
    let bindValues = fmap unLoc bindsValue
    if any unsupportedLocalBind bindValues
      then pure Nothing
      else do
        childScope <- freshChildScope
        withScope childScope $ do
          bindPatterns <- traverse localBindPattern bindValues
          let binders = concatMap patBinders bindPatterns
              extendedEnv = extendEnv env binders
          rhsValues <- traverse (convertLocalBind extendedEnv) bindValues
          let bindingRows = zip bindPatterns rhsValues
          scopeAlgebra <- currentScopeAlgebra
          letRecursionValue <-
            liftConvertEither $
              FreeScopes.inferLetRecursion
                scopeAlgebra
                (fmap (\(rowPattern, rhsExpr) -> (rowPattern, cxScoped rhsExpr)) bindingRows)
          case bindingRows of
            [(PVarP binderAnn, _)]
              | letRecursionValue == NonRecursiveBinds ->
                  recordLetSite binderAnn provenanceValue
            _ ->
              pure ()
          pure
            ( Just
                ConvertedLocalBinds
                  { clbMode = LetMode letRecursionValue provenanceValue,
                    clbScope = childScope,
                    clbBindings = bindingRows,
                    clbBinders = binders,
                    clbEnv = extendedEnv
                  }
            )
  XValBindsLR _ ->
    pure Nothing

unsupportedLocalBind :: HsBind GhcPs -> Bool
unsupportedLocalBind = \case
  PatSynBind {} -> True
  _ -> False

localBindPattern :: HsBind GhcPs -> ConvM HsPatF
localBindPattern = \case
  FunBind {fun_id = nameValue} ->
    PVarP <$> freshBinderAnn (unLoc nameValue)
  PatBind {pat_lhs = patternValue} ->
    convertPat patternValue
  VarBind {var_id = nameValue} ->
    PVarP <$> freshBinderAnn nameValue
  PatSynBind {} ->
    pure (PLossyP PatOpaqueExtension [])

convertLocalBind :: Env -> HsBind GhcPs -> ConvM ConvExpr
convertLocalBind env = \case
  FunBind {fun_matches = matchGroupValue} ->
    convertLambdaLikeMatchGroup env Nothing matchGroupValue
  PatBind {pat_rhs = grhssValue} ->
    convertGRHSs env grhssValue
  VarBind {var_rhs = rhsValue} ->
    convertLocatedExpr env rhsValue
  PatSynBind {} ->
    opaqueConvExpr Nothing OpaqueUnsupportedBind

localBindNames :: HsBind GhcPs -> [RdrName]
localBindNames = \case
  FunBind {fun_id = nameValue} -> [unLoc nameValue]
  PatBind {pat_lhs = patternValue} -> maybe [] (: []) (simpleLambdaBinderName (unLoc patternValue))
  VarBind {var_id = nameValue} -> [nameValue]
  PatSynBind {} -> []

convertTupleArg :: Env -> HsTupArg GhcPs -> ConvM (Maybe ConvExpr)
convertTupleArg env = \case
  Present _ exprValue ->
    Just <$> convertLocatedExpr env exprValue
  Missing _ ->
    pure Nothing

convertRecordFields :: Env -> HsRecFields GhcPs (LHsExpr GhcPs) -> ConvM [(NormalizedFieldLabel, ConvExpr)]
convertRecordFields env recordFieldsValue =
  mapMaybe id <$>
    traverse
      (convertRecordField env . unLoc)
      (rec_flds recordFieldsValue)

convertRecordUpdFields :: Env -> LHsRecUpdFields GhcPs -> ConvM (Maybe [(NormalizedFieldLabel, ConvExpr)])
convertRecordUpdFields env = \case
  RegularRecUpdFields {recUpdFields = recordFieldsValue} ->
    Just . mapMaybe id <$>
      traverse
        (convertRecordField env . unLoc)
        recordFieldsValue
  OverloadedRecUpdFields {} ->
    pure Nothing

convertRecordField :: Env -> HsRecField GhcPs (LHsExpr GhcPs) -> ConvM (Maybe (NormalizedFieldLabel, ConvExpr))
convertRecordField env fieldBindValue =
  case unLoc (hfbLHS fieldBindValue) of
    FieldOcc {foLabel = labelValue} -> do
      fieldExpr <-
        if hfbPun fieldBindValue
          then mkConvExpr Nothing (VarF (resolveVarRef env (unLoc labelValue)))
          else convertLocatedExpr env (hfbRHS fieldBindValue)
      pure
        ( Just
            ( normalizeFieldOcc (unLoc labelValue),
              fieldExpr
            )
        )

normalizeFieldOcc :: RdrName -> NormalizedFieldLabel
normalizeFieldOcc rdrName =
  NormalizedFieldLabel
    { nflSelector = occNameString (rdrNameOcc rdrName),
      nflAllowsDuplicateRecordFields = False,
      nflHasSelector = True
    }

convertArithSeq :: Env -> ArithSeqInfo GhcPs -> ConvM (NormalizedArithSeq ConvExpr)
convertArithSeq env = \case
  From fromValue ->
    ArithSeqFrom <$> convertLocatedExpr env fromValue
  FromThen fromValue thenValue ->
    ArithSeqFromThen
      <$> convertLocatedExpr env fromValue
      <*> convertLocatedExpr env thenValue
  FromTo fromValue toValue ->
    ArithSeqFromTo
      <$> convertLocatedExpr env fromValue
      <*> convertLocatedExpr env toValue
  FromThenTo fromValue thenValue toValue ->
    ArithSeqFromThenTo
      <$> convertLocatedExpr env fromValue
      <*> convertLocatedExpr env thenValue
      <*> convertLocatedExpr env toValue

simpleLambdaBinderName :: Pat GhcPs -> Maybe RdrName
simpleLambdaBinderName = \case
  VarPat _ nameValue -> Just (unLoc nameValue)
  ParPat _ patternValue -> simpleLambdaBinderName (unLoc patternValue)
  BangPat _ patternValue -> simpleLambdaBinderName (unLoc patternValue)
  LazyPat _ patternValue -> simpleLambdaBinderName (unLoc patternValue)
  SigPat _ patternValue _ -> simpleLambdaBinderName (unLoc patternValue)
  _ -> Nothing

collectPatternNames :: LPat GhcPs -> [RdrName]
collectPatternNames = collectPatBinders CollNoDictBinders

convertPat :: LPat GhcPs -> ConvM HsPatF
convertPat patternValue =
  case unLoc patternValue of
    VarPat _ nameValue ->
      PVarP <$> freshBinderAnn (unLoc nameValue)
    WildPat {} ->
      pure PWildP
    ParPat _ innerValue ->
      PParP <$> convertPat innerValue
    BangPat _ innerValue ->
      PBangP <$> convertPat innerValue
    LazyPat _ innerValue ->
      PLazyP <$> convertPat innerValue
    AsPat _ nameValue innerValue ->
      PAsP <$> freshBinderAnn (unLoc nameValue) <*> convertPat innerValue
    TuplePat _ componentValues Boxed ->
      PTupleP <$> traverse convertPat componentValues
    TuplePat {} ->
      lossyPat PatOpaqueUnboxedTuple patternValue
    ListPat _ componentValues ->
      PListP <$> traverse convertPat componentValues
    LitPat _ literalValue ->
      pure (PLitP (normalizeHsLit literalValue))
    NPat _ overLitValue Nothing _ ->
      pure (POverLitP (normalizeHsOverLit (unLoc overLitValue)))
    NPat {} ->
      lossyPat PatOpaqueNegativeLit patternValue
    ConPat {pat_con = conValue, pat_args = argsValue} ->
      case argsValue of
        PrefixCon argValues ->
          PConP (unLoc conValue) <$> traverse convertPat argValues
        InfixCon leftValue rightValue ->
          PConP (unLoc conValue) <$> traverse convertPat [leftValue, rightValue]
        RecCon recordFieldsValue ->
          case rec_dotdot recordFieldsValue of
            Just {} ->
              lossyPat PatOpaqueRecCon patternValue
            Nothing ->
              PRecP (unLoc conValue)
                <$> traverse (convertRecPatField . unLoc) (rec_flds recordFieldsValue)
    OrPat {} ->
      lossyPat PatOpaqueOr patternValue
    SumPat {} ->
      lossyPat PatOpaqueSum patternValue
    ViewPat {} ->
      lossyPat PatOpaqueView patternValue
    SplicePat {} ->
      lossyPat PatOpaqueSplice patternValue
    NPlusKPat {} ->
      lossyPat PatOpaqueNPlusK patternValue
    SigPat {} ->
      lossyPat PatOpaqueSig patternValue
    EmbTyPat {} ->
      lossyPat PatOpaqueEmbTy patternValue
    InvisPat {} ->
      lossyPat PatOpaqueInvis patternValue

convertRecPatField :: HsRecField GhcPs (LPat GhcPs) -> ConvM (String, HsPatF)
convertRecPatField fieldBindValue =
  case unLoc (hfbLHS fieldBindValue) of
    FieldOcc {foLabel = labelValue} -> do
      fieldPattern <-
        if hfbPun fieldBindValue
          then PVarP <$> freshBinderAnn (unLoc labelValue)
          else convertPat (hfbRHS fieldBindValue)
      pure (occNameString (rdrNameOcc (unLoc labelValue)), fieldPattern)

lossyPat :: HsPatOpaqueTag -> LPat GhcPs -> ConvM HsPatF
lossyPat tagValue patternValue =
  PLossyP tagValue <$> traverse freshBinderAnn (collectPatternNames patternValue)

normalizedTypeText :: Outputable a => a -> NormalizedTypeText
normalizedTypeText =
  NormalizedTypeText . unwords . words . showSDocUnsafe . ppr

resolveVarRef :: Env -> RdrName -> HsVarRef
resolveVarRef env nameValue =
  maybe (GlobalName nameValue) LocalName (Map.lookup nameValue env)

extendEnv :: Env -> [BinderAnn] -> Env
extendEnv env binderAnns =
  foldr (\binderAnn -> Map.insert (baName binderAnn) binderAnn) env binderAnns

currentScopeId :: ConvM ScopeId
currentScopeId =
  gets csCurrentScope

withScope :: ScopeId -> ConvM value -> ConvM value
withScope scopeId action = do
  previousScope <- gets csCurrentScope
  modify' (\stateValue -> stateValue {csCurrentScope = scopeId})
  resultValue <- action
  modify' (\stateValue -> stateValue {csCurrentScope = previousScope})
  pure resultValue

freshChildScope :: ConvM ScopeId
freshChildScope = do
  parentScope <- gets csCurrentScope
  parentDepth <- scopeDepthInState parentScope
  nextScopeId <- gets csNextScopeId
  childScope <-
    either
      (throwConvert . ConvertFreshScopeIdFailure nextScopeId)
      pure
      (mkScopeId nextScopeId)
  let parentKey = scopeIdKey parentScope
  modify'
    ( \stateValue ->
        stateValue
          { csNextScopeId = nextScopeId + 1,
            csScopeParentsRev = parentKey : csScopeParentsRev stateValue,
            csScopeDepths = IntMap.insert nextScopeId (parentDepth + 1) (csScopeDepths stateValue)
          }
    )
  pure childScope

scopeDepthInState :: ScopeId -> ConvM Int
scopeDepthInState scopeId = do
  depthMap <- gets csScopeDepths
  maybe
    (throwConvert (ConvertMissingScopeDepth scopeId))
    pure
    (IntMap.lookup (scopeIdKey scopeId) depthMap)

mkScopedExpr :: HsExprF ScopedExpr -> ConvM ScopedExpr
mkScopedExpr nodeValue = do
  occurrenceScope <- currentScopeId
  scopeAlgebra <- currentScopeAlgebra
  freeScopes <- liftConvertEither (FreeScopes.freeScopesExpr scopeAlgebra nodeValue)
  pure
    ScopedExpr
      { seOccScope = occurrenceScope,
        seFreeScopes = freeScopes,
        seNode = nodeValue
      }

mkConvExpr :: Maybe SourceRegion -> HsExprF ConvExpr -> ConvM ConvExpr
mkConvExpr region nodeValue = do
  scopedExpr <- mkScopedExpr (fmap cxScoped nodeValue)
  pure
    ConvExpr
      { cxScoped = scopedExpr,
        cxSpanned =
          SpannedExpr
            { sxRegion = region,
              sxNode = fmap cxSpanned nodeValue
            }
      }

opaqueConvExpr :: Maybe SourceRegion -> HsOpaqueTag -> ConvM ConvExpr
opaqueConvExpr region =
  mkConvExpr region . OpaqueF

freshBinderAnn :: RdrName -> ConvM BinderAnn
freshBinderAnn binderName = do
  nextBinderId <- gets csNextBinderId
  introScope <- gets csCurrentScope
  let introKey = scopeIdKey introScope
  modify'
    ( \stateValue ->
        stateValue
          { csNextBinderId = nextBinderId + 1,
            csBinderIntroRev = introKey : csBinderIntroRev stateValue,
            csBinderIntroMap = IntMap.insert nextBinderId introScope (csBinderIntroMap stateValue)
          }
    )
  pure
    BinderAnn
      { baId = BinderId nextBinderId,
        baName = binderName
      }

recordLambdaSite :: BinderAnn -> ConvM ()
recordLambdaSite binderAnn =
  modify' (\stateValue -> stateValue {csLambdaSites = binderAnn : csLambdaSites stateValue})

recordLetSite :: BinderAnn -> LetProvenance -> ConvM ()
recordLetSite binderAnn provenanceValue =
  modify' (\stateValue -> stateValue {csLetSites = (binderAnn, provenanceValue) : csLetSites stateValue})

currentScopeAlgebra :: ConvM (ScopeAlgebra ConvertObstruction)
currentScopeAlgebra = do
  depthMap <- gets csScopeDepths
  introMap <- gets csBinderIntroMap
  pure
    ScopeAlgebra
      { saScopeDepth =
          \scopeId ->
            maybe
              (Left (ConvertMissingScopeSummaryDepth scopeId))
              Right
              (IntMap.lookup (scopeIdKey scopeId) depthMap),
        saBinderIntro =
          \binderAnn ->
            maybe
              (Left (ConvertMissingBinderIntro (baId binderAnn)))
              Right
              (IntMap.lookup (binderIdKey (baId binderAnn)) introMap)
      }
