{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Moonlight.EGraph.Introspection.Core.HsExpr.Front
  ( HsExprTerm,
    HsExprLawEmitError (..),
    HsExprFoldError (..),
    global,
    local,
    app,
    app2,
    op,
    par,
    nilList,
    lam,
    zeroLit,
    oneLit,
    mkPatternVarMap,
    emitExpr,
    foldNodeToHsExprF,
    foldGuardTermToHsExprF,
    foldRewriteConditionToHsExprF,
    foldApplicationConditionToHsExprF,
  )
where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Types.Name.Reader (RdrName)
import Moonlight.Core (Pattern (..))
import Moonlight.Core qualified as EGraph
import Moonlight.EGraph.Introspection.Core.HsExpr.Sig (HsExprSig (..))
import Moonlight.Rewrite.Algebra (ApplicationCondition (..), PatternExtension (..))
import Moonlight.Rewrite.Algebra (PatternQuery (..))
import Moonlight.Rewrite.DSL (K (..), Node (..), htraverse, nodeResultSort, sortWitnessSortName)
import Moonlight.Rewrite.DSL (SortName, Term (..), sortName, symbolToken, typedVarName, typedVarSort)
import Moonlight.Rewrite.System (GuardAtom (..), GuardTerm (..), RewriteCondition (..))
import Moonlight.Pale.Ghc.Expr
  ( BinderAnn,
    ConvertObstruction,
    GuardedAltF (..),
    HsExprF (..),
    HsGuardStmtF (..),
    HsStmtF (..),
    HsVarRef (..),
    NormalizedArithSeq (..),
    NormalizedOverLit (..),
  )

type HsExprTerm :: Type
type HsExprTerm = Term HsExprSig "expr"

type HsExprLawEmitError :: Type
data HsExprLawEmitError
  = DuplicateEmitterVariable !String
  | UnknownEmitterVariable !String !SortName
  | EmitterFoldError !HsExprFoldError
  | InvalidRuleInstantiationIndex !Int
  | EquationMissingEquals !String
  | EquationAmbiguousEquals !String !Int
  | EquationConvertFailure !ConvertObstruction
  deriving stock (Eq, Show)

type HsExprFoldError :: Type
data HsExprFoldError
  = NonExprHoleError !EGraph.PatternVar !SortName
  | NonExprGuardTermError !SortName
  | UnexpectedNodeSort !SortName !SortName
  deriving stock (Eq, Show)

global :: RdrName -> HsExprTerm
global =
  TNode . SVarF . GlobalName

local :: BinderAnn -> HsExprTerm
local =
  TNode . SVarF . LocalName

app :: HsExprTerm -> HsExprTerm -> HsExprTerm
app function argument =
  TNode (SAppF function argument)

app2 :: RdrName -> HsExprTerm -> HsExprTerm -> HsExprTerm
app2 functionName firstArgument secondArgument =
  app (app (global functionName) firstArgument) secondArgument

op :: HsExprTerm -> RdrName -> HsExprTerm -> HsExprTerm
op left operatorName right =
  TNode (SOpAppF left (global operatorName) right)

par :: HsExprTerm -> HsExprTerm
par =
  TNode . SParF

nilList :: HsExprTerm
nilList =
  TNode (SExplicitListF [])

lam :: BinderAnn -> HsExprTerm -> HsExprTerm
lam binder =
  TNode . SLamF binder

zeroLit :: HsExprTerm
zeroLit =
  TNode (SOverLitF (NormalizedIntegralOverLit 0))

oneLit :: HsExprTerm
oneLit =
  TNode (SOverLitF (NormalizedIntegralOverLit 1))

emitExpr :: [String] -> HsExprTerm -> Either HsExprLawEmitError (Pattern HsExprF)
emitExpr names term =
  internInOrder names term >>= first EmitterFoldError . foldNodeToHsExprF

foldNodeToHsExprF :: Pattern (Node HsExprSig) -> Either HsExprFoldError (Pattern HsExprF)
foldNodeToHsExprF =
  foldExprPattern

foldRewriteConditionToHsExprF ::
  RewriteCondition capability (Node HsExprSig) ->
  Either HsExprFoldError (RewriteCondition capability HsExprF)
foldRewriteConditionToHsExprF (RewriteCondition conditionExpr) =
  RewriteCondition <$> traverse foldGuardAtomToHsExprF conditionExpr

foldApplicationConditionToHsExprF ::
  ApplicationCondition (RewriteCondition capability (Node HsExprSig)) (Node HsExprSig) ->
  Either HsExprFoldError (ApplicationCondition (RewriteCondition capability HsExprF) HsExprF)
foldApplicationConditionToHsExprF (ApplicationCondition conditionExpr) =
  ApplicationCondition <$> traverse foldPatternExtensionToHsExprF conditionExpr

foldGuardAtomToHsExprF ::
  GuardAtom capability (Node HsExprSig) ->
  Either HsExprFoldError (GuardAtom capability HsExprF)
foldGuardAtomToHsExprF =
  \case
    ClassesEquivalent leftTerm rightTerm ->
      ClassesEquivalent <$> foldGuardTermToHsExprF leftTerm <*> foldGuardTermToHsExprF rightTerm
    HasFact factId terms ->
      HasFact factId <$> traverse foldGuardTermToHsExprF terms
    HasCapability capability terms ->
      HasCapability capability <$> traverse foldGuardTermToHsExprF terms

foldPatternExtensionToHsExprF ::
  PatternExtension (RewriteCondition capability (Node HsExprSig)) (Node HsExprSig) ->
  Either HsExprFoldError (PatternExtension (RewriteCondition capability HsExprF) HsExprF)
foldPatternExtensionToHsExprF extension =
  PatternExtension
    <$> foldPatternQueryToHsExprF (peQuery extension)
    <*> pure (peExplicitAnchorVars extension)
    <*> pure (peScope extension)

foldPatternQueryToHsExprF ::
  PatternQuery (RewriteCondition capability (Node HsExprSig)) (Node HsExprSig) ->
  Either HsExprFoldError (PatternQuery (RewriteCondition capability HsExprF) HsExprF)
foldPatternQueryToHsExprF =
  \case
    SinglePatternQuery patternValue ->
      SinglePatternQuery <$> foldNodeToHsExprF patternValue
    ConjunctivePatternQuery queries ->
      ConjunctivePatternQuery <$> traverse foldPatternQueryToHsExprF queries
    GuardedPatternQuery queryValue condition ->
      GuardedPatternQuery <$> foldPatternQueryToHsExprF queryValue <*> foldRewriteConditionToHsExprF condition

foldGuardTermToHsExprF ::
  GuardTerm (Node HsExprSig) ->
  Either HsExprFoldError (GuardTerm HsExprF)
foldGuardTermToHsExprF =
  \case
    GuardRefTerm guardRef ->
      Right (GuardRefTerm guardRef)
    GuardProjectTerm baseTerm childIndex ->
      GuardProjectTerm <$> foldGuardTermToHsExprF baseTerm <*> pure childIndex
    GuardNodeTerm nodeValue ->
      GuardNodeTerm <$> foldGuardExprNodeToHsExprF nodeValue

type PatternVarMap :: Type
newtype PatternVarMap = PatternVarMap
  { pvmVariables :: Map String EGraph.PatternVar
  }

internInOrder :: [String] -> Term HsExprSig sort -> Either HsExprLawEmitError (Pattern (Node HsExprSig))
internInOrder names term =
  patternVarMapFromNames names >>= \variableMap ->
    lowerWithVariables (pvmVariables variableMap) term

mkPatternVarMap :: [String] -> Either HsExprLawEmitError (Map String EGraph.PatternVar)
mkPatternVarMap names =
  pvmVariables <$> patternVarMapFromNames names

patternVarMapFromNames :: [String] -> Either HsExprLawEmitError PatternVarMap
patternVarMapFromNames names =
  foldM observeName (PatternVarMap Map.empty) (zip names (EGraph.mkPatternVar <$> [0 ..]))
  where
    observeName accumulator (name, patternVariable)
      | Map.member name (pvmVariables accumulator) =
          Left (DuplicateEmitterVariable name)
      | otherwise =
          Right
            accumulator
              { pvmVariables =
                  Map.insert name patternVariable (pvmVariables accumulator)
              }

lowerWithVariables :: Map String EGraph.PatternVar -> Term HsExprSig sort -> Either HsExprLawEmitError (Pattern (Node HsExprSig))
lowerWithVariables variables =
  \case
    TVar typedVariable ->
      maybe
        (Left (UnknownEmitterVariable (typedVarName typedVariable) (typedVarSort typedVariable)))
        (Right . PatternVar)
        (Map.lookup (typedVarName typedVariable) variables)
    TNode sigNode ->
      PatternNode . Node
        <$> htraverse
          (fmap K . lowerWithVariables variables)
          sigNode

foldExprPattern :: Pattern (Node HsExprSig) -> Either HsExprFoldError (Pattern HsExprF)
foldExprPattern =
  \case
    PatternVar patternVariable ->
      Right (PatternVar patternVariable)
    PatternNode (Node sigNode) ->
      case sigNode of
        SVarF name ->
          Right (PatternNode (VarF name))
        SAppF function argument ->
          PatternNode <$> (AppF <$> expr function <*> expr argument)
        SLamF binder body ->
          PatternNode . LamF binder <$> expr body
        SLetF mode bindings body ->
          PatternNode <$> (LetF mode <$> traverse (traverse expr) bindings <*> expr body)
        SOpAppF left operator right ->
          PatternNode <$> (OpAppF <$> expr left <*> expr operator <*> expr right)
        SSectionLF left operator ->
          PatternNode <$> (SectionLF <$> expr left <*> expr operator)
        SSectionRF operator right ->
          PatternNode <$> (SectionRF <$> expr operator <*> expr right)
        SParF body ->
          PatternNode . ParF <$> expr body
        SLitF literalValue ->
          Right (PatternNode (LitF literalValue))
        SOverLitF literalValue ->
          Right (PatternNode (OverLitF literalValue))
        SIfF condition thenBranch elseBranch ->
          PatternNode <$> (IfF <$> expr condition <*> expr thenBranch <*> expr elseBranch)
        SCaseF scrutinee alternatives ->
          PatternNode <$> (CaseF <$> expr scrutinee <*> traverse (traverse expr) alternatives)
        SDoF statements ->
          PatternNode . DoF <$> traverse stmt statements
        SNegF body ->
          PatternNode . NegF <$> expr body
        SExplicitListF elements ->
          PatternNode . ExplicitListF <$> traverse expr elements
        SExplicitTupleF elements ->
          PatternNode . ExplicitTupleF <$> traverse expr elements
        SRecordConF constructor fields ->
          PatternNode <$> (RecordConF <$> expr constructor <*> traverse (traverse expr) fields)
        SRecordUpdF record fields ->
          PatternNode <$> (RecordUpdF <$> expr record <*> traverse (traverse expr) fields)
        SArithFromF fromValue ->
          PatternNode . ArithSeqF . ArithSeqFrom <$> expr fromValue
        SArithFromThenF fromValue thenValue ->
          PatternNode . ArithSeqF <$> (ArithSeqFromThen <$> expr fromValue <*> expr thenValue)
        SArithFromToF fromValue toValue ->
          PatternNode . ArithSeqF <$> (ArithSeqFromTo <$> expr fromValue <*> expr toValue)
        SArithFromThenToF fromValue thenValue toValue ->
          PatternNode . ArithSeqF <$> (ArithSeqFromThenTo <$> expr fromValue <*> expr thenValue <*> expr toValue)
        SGuardedF alternatives ->
          PatternNode . GuardedF <$> traverse guardedAlt alternatives
        SClausesF clauses ->
          PatternNode . ClausesF <$> traverse (traverse expr) clauses
        SMultiIfF alternatives ->
          PatternNode . MultiIfF <$> traverse guardedAlt alternatives
        SExprWithTySigF body typeText ->
          PatternNode . (`ExprWithTySigF` typeText) <$> expr body
        SAppTypeF body typeText ->
          PatternNode . (`AppTypeF` typeText) <$> expr body
        SOpaqueF tag ->
          Right (PatternNode (OpaqueF tag))
        SBindStmtF {} ->
          Left (unexpectedNodeSort exprSortName sigNode)
        SBodyStmtF {} ->
          Left (unexpectedNodeSort exprSortName sigNode)
        SLetStmtF {} ->
          Left (unexpectedNodeSort exprSortName sigNode)
        SGuardedAltF {} ->
          Left (unexpectedNodeSort exprSortName sigNode)
        SGuardBoolF {} ->
          Left (unexpectedNodeSort exprSortName sigNode)
        SGuardPatF {} ->
          Left (unexpectedNodeSort exprSortName sigNode)
        SGuardLetF {} ->
          Left (unexpectedNodeSort exprSortName sigNode)

foldStmtPattern :: Pattern (Node HsExprSig) -> Either HsExprFoldError (HsStmtF (Pattern HsExprF))
foldStmtPattern =
  \case
    PatternVar patternVariable ->
      Left (NonExprHoleError patternVariable stmtSortName)
    PatternNode (Node sigNode) ->
      case sigNode of
        SBindStmtF patternValue body ->
          BindStmtF patternValue <$> expr body
        SBodyStmtF body ->
          BodyStmtF <$> expr body
        SLetStmtF mode bindings ->
          LetStmtF mode <$> traverse (traverse expr) bindings
        _ ->
          Left (unexpectedNodeSort stmtSortName sigNode)

foldGuardedAltPattern :: Pattern (Node HsExprSig) -> Either HsExprFoldError (GuardedAltF (Pattern HsExprF))
foldGuardedAltPattern =
  \case
    PatternVar patternVariable ->
      Left (NonExprHoleError patternVariable guardedAltSortName)
    PatternNode (Node sigNode) ->
      case sigNode of
        SGuardedAltF guards body ->
          GuardedAltF <$> traverse guardStmt guards <*> expr body
        _ ->
          Left (unexpectedNodeSort guardedAltSortName sigNode)

foldGuardStmtPattern :: Pattern (Node HsExprSig) -> Either HsExprFoldError (HsGuardStmtF (Pattern HsExprF))
foldGuardStmtPattern =
  \case
    PatternVar patternVariable ->
      Left (NonExprHoleError patternVariable guardStmtSortName)
    PatternNode (Node sigNode) ->
      case sigNode of
        SGuardBoolF condition ->
          GuardBoolF <$> expr condition
        SGuardPatF patternValue body ->
          GuardPatF patternValue <$> expr body
        SGuardLetF mode bindings ->
          GuardLetF mode <$> traverse (traverse expr) bindings
        _ ->
          Left (unexpectedNodeSort guardStmtSortName sigNode)

foldGuardExprNodeToHsExprF :: Node HsExprSig (GuardTerm (Node HsExprSig)) -> Either HsExprFoldError (HsExprF (GuardTerm HsExprF))
foldGuardExprNodeToHsExprF (Node sigNode) =
  case sigNode of
    SVarF name ->
      Right (VarF name)
    SAppF function argument ->
      AppF <$> guardExpr function <*> guardExpr argument
    SLamF binder body ->
      LamF binder <$> guardExpr body
    SLetF mode bindings body ->
      LetF mode <$> traverse (traverse guardExpr) bindings <*> guardExpr body
    SOpAppF left operator right ->
      OpAppF <$> guardExpr left <*> guardExpr operator <*> guardExpr right
    SSectionLF left operator ->
      SectionLF <$> guardExpr left <*> guardExpr operator
    SSectionRF operator right ->
      SectionRF <$> guardExpr operator <*> guardExpr right
    SParF body ->
      ParF <$> guardExpr body
    SLitF literalValue ->
      Right (LitF literalValue)
    SOverLitF literalValue ->
      Right (OverLitF literalValue)
    SIfF condition thenBranch elseBranch ->
      IfF <$> guardExpr condition <*> guardExpr thenBranch <*> guardExpr elseBranch
    SCaseF scrutinee alternatives ->
      CaseF <$> guardExpr scrutinee <*> traverse (traverse guardExpr) alternatives
    SDoF statements ->
      DoF <$> traverse guardTermStmt statements
    SNegF body ->
      NegF <$> guardExpr body
    SExplicitListF elements ->
      ExplicitListF <$> traverse guardExpr elements
    SExplicitTupleF elements ->
      ExplicitTupleF <$> traverse guardExpr elements
    SRecordConF constructor fields ->
      RecordConF <$> guardExpr constructor <*> traverse (traverse guardExpr) fields
    SRecordUpdF record fields ->
      RecordUpdF <$> guardExpr record <*> traverse (traverse guardExpr) fields
    SArithFromF fromValue ->
      ArithSeqF . ArithSeqFrom <$> guardExpr fromValue
    SArithFromThenF fromValue thenValue ->
      ArithSeqF <$> (ArithSeqFromThen <$> guardExpr fromValue <*> guardExpr thenValue)
    SArithFromToF fromValue toValue ->
      ArithSeqF <$> (ArithSeqFromTo <$> guardExpr fromValue <*> guardExpr toValue)
    SArithFromThenToF fromValue thenValue toValue ->
      ArithSeqF <$> (ArithSeqFromThenTo <$> guardExpr fromValue <*> guardExpr thenValue <*> guardExpr toValue)
    SGuardedF alternatives ->
      GuardedF <$> traverse guardTermAlt alternatives
    SClausesF clauses ->
      ClausesF <$> traverse (traverse guardExpr) clauses
    SMultiIfF alternatives ->
      MultiIfF <$> traverse guardTermAlt alternatives
    SExprWithTySigF body typeText ->
      (`ExprWithTySigF` typeText) <$> guardExpr body
    SAppTypeF body typeText ->
      (`AppTypeF` typeText) <$> guardExpr body
    SOpaqueF tag ->
      Right (OpaqueF tag)
    SBindStmtF {} ->
      Left (unexpectedNodeSort exprSortName sigNode)
    SBodyStmtF {} ->
      Left (unexpectedNodeSort exprSortName sigNode)
    SLetStmtF {} ->
      Left (unexpectedNodeSort exprSortName sigNode)
    SGuardedAltF {} ->
      Left (unexpectedNodeSort exprSortName sigNode)
    SGuardBoolF {} ->
      Left (unexpectedNodeSort exprSortName sigNode)
    SGuardPatF {} ->
      Left (unexpectedNodeSort exprSortName sigNode)
    SGuardLetF {} ->
      Left (unexpectedNodeSort exprSortName sigNode)

foldGuardStmtNodeToHsExprF :: Node HsExprSig (GuardTerm (Node HsExprSig)) -> Either HsExprFoldError (HsStmtF (GuardTerm HsExprF))
foldGuardStmtNodeToHsExprF (Node sigNode) =
  case sigNode of
    SBindStmtF patternValue body ->
      BindStmtF patternValue <$> guardExpr body
    SBodyStmtF body ->
      BodyStmtF <$> guardExpr body
    SLetStmtF mode bindings ->
      LetStmtF mode <$> traverse (traverse guardExpr) bindings
    _ ->
      Left (unexpectedNodeSort stmtSortName sigNode)

foldGuardAltNodeToHsExprF :: Node HsExprSig (GuardTerm (Node HsExprSig)) -> Either HsExprFoldError (GuardedAltF (GuardTerm HsExprF))
foldGuardAltNodeToHsExprF (Node sigNode) =
  case sigNode of
    SGuardedAltF guards body ->
      GuardedAltF <$> traverse guardTermGuardStmt guards <*> guardExpr body
    _ ->
      Left (unexpectedNodeSort guardedAltSortName sigNode)

foldGuardGuardStmtNodeToHsExprF :: Node HsExprSig (GuardTerm (Node HsExprSig)) -> Either HsExprFoldError (HsGuardStmtF (GuardTerm HsExprF))
foldGuardGuardStmtNodeToHsExprF (Node sigNode) =
  case sigNode of
    SGuardBoolF condition ->
      GuardBoolF <$> guardExpr condition
    SGuardPatF patternValue body ->
      GuardPatF patternValue <$> guardExpr body
    SGuardLetF mode bindings ->
      GuardLetF mode <$> traverse (traverse guardExpr) bindings
    _ ->
      Left (unexpectedNodeSort guardStmtSortName sigNode)

expr :: K (Pattern (Node HsExprSig)) "expr" -> Either HsExprFoldError (Pattern HsExprF)
expr (K patternValue) =
  foldExprPattern patternValue

stmt :: K (Pattern (Node HsExprSig)) "stmt" -> Either HsExprFoldError (HsStmtF (Pattern HsExprF))
stmt (K patternValue) =
  foldStmtPattern patternValue

guardedAlt :: K (Pattern (Node HsExprSig)) "galt" -> Either HsExprFoldError (GuardedAltF (Pattern HsExprF))
guardedAlt (K patternValue) =
  foldGuardedAltPattern patternValue

guardStmt :: K (Pattern (Node HsExprSig)) "gstmt" -> Either HsExprFoldError (HsGuardStmtF (Pattern HsExprF))
guardStmt (K patternValue) =
  foldGuardStmtPattern patternValue

guardExpr :: K (GuardTerm (Node HsExprSig)) "expr" -> Either HsExprFoldError (GuardTerm HsExprF)
guardExpr (K guardTerm) =
  foldGuardTermToHsExprF guardTerm

guardTermStmt :: K (GuardTerm (Node HsExprSig)) "stmt" -> Either HsExprFoldError (HsStmtF (GuardTerm HsExprF))
guardTermStmt (K guardTerm) =
  case guardTerm of
    GuardNodeTerm nodeValue ->
      foldGuardStmtNodeToHsExprF nodeValue
    GuardRefTerm {} ->
      Left (NonExprGuardTermError stmtSortName)
    GuardProjectTerm {} ->
      Left (NonExprGuardTermError stmtSortName)

guardTermAlt :: K (GuardTerm (Node HsExprSig)) "galt" -> Either HsExprFoldError (GuardedAltF (GuardTerm HsExprF))
guardTermAlt (K guardTerm) =
  case guardTerm of
    GuardNodeTerm nodeValue ->
      foldGuardAltNodeToHsExprF nodeValue
    GuardRefTerm {} ->
      Left (NonExprGuardTermError guardedAltSortName)
    GuardProjectTerm {} ->
      Left (NonExprGuardTermError guardedAltSortName)

guardTermGuardStmt :: K (GuardTerm (Node HsExprSig)) "gstmt" -> Either HsExprFoldError (HsGuardStmtF (GuardTerm HsExprF))
guardTermGuardStmt (K guardTerm) =
  case guardTerm of
    GuardNodeTerm nodeValue ->
      foldGuardGuardStmtNodeToHsExprF nodeValue
    GuardRefTerm {} ->
      Left (NonExprGuardTermError guardStmtSortName)
    GuardProjectTerm {} ->
      Left (NonExprGuardTermError guardStmtSortName)

unexpectedNodeSort :: SortName -> HsExprSig actualSort child -> HsExprFoldError
unexpectedNodeSort expected sigNode =
  UnexpectedNodeSort expected (sortWitnessSortName (nodeResultSort sigNode))

exprSortName :: SortName
exprSortName =
  sortName (symbolToken @"expr")

stmtSortName :: SortName
stmtSortName =
  sortName (symbolToken @"stmt")

guardedAltSortName :: SortName
guardedAltSortName =
  sortName (symbolToken @"galt")

guardStmtSortName :: SortName
guardStmtSortName =
  sortName (symbolToken @"gstmt")
