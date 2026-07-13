{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Introspection.Core.HsExpr.Sig
  ( HsExprSig (..),
    HsExprSigTag (..),
  )
where

import Control.Monad.Trans.State.Strict (StateT (..), runStateT)
import Data.Foldable qualified as Foldable
import Data.Kind (Type)
import Data.Word (Word64)
import GHC.TypeLits (Symbol)
import Moonlight.Core (ZipMatch (..))
import Moonlight.Rewrite.DSL (HTraversable (..), K (..), Node (..), RewriteSignature (..), SortWitness (..), htraverse, nodeChildren)
import Moonlight.Pale.Ghc.Expr
  ( BinderAnn,
    HsPatF,
    HsOpaqueTag,
    HsVarRef,
    LetMode,
    NormalizedFieldLabel,
    NormalizedLit,
    NormalizedOverLit,
    NormalizedTypeText,
  )

type HsExprSig :: Symbol -> (Symbol -> Type) -> Type
data HsExprSig result r where
  SVarF :: !HsVarRef -> HsExprSig "expr" r
  SAppF :: r "expr" -> r "expr" -> HsExprSig "expr" r
  SLamF :: !BinderAnn -> r "expr" -> HsExprSig "expr" r
  SLetF :: !LetMode -> ![(HsPatF, r "expr")] -> r "expr" -> HsExprSig "expr" r
  SOpAppF :: r "expr" -> r "expr" -> r "expr" -> HsExprSig "expr" r
  SSectionLF :: r "expr" -> r "expr" -> HsExprSig "expr" r
  SSectionRF :: r "expr" -> r "expr" -> HsExprSig "expr" r
  SParF :: r "expr" -> HsExprSig "expr" r
  SLitF :: !NormalizedLit -> HsExprSig "expr" r
  SOverLitF :: !NormalizedOverLit -> HsExprSig "expr" r
  SIfF :: r "expr" -> r "expr" -> r "expr" -> HsExprSig "expr" r
  SCaseF :: r "expr" -> ![(HsPatF, r "expr")] -> HsExprSig "expr" r
  SDoF :: ![r "stmt"] -> HsExprSig "expr" r
  SNegF :: r "expr" -> HsExprSig "expr" r
  SExplicitListF :: ![r "expr"] -> HsExprSig "expr" r
  SExplicitTupleF :: ![r "expr"] -> HsExprSig "expr" r
  SRecordConF :: r "expr" -> ![(NormalizedFieldLabel, r "expr")] -> HsExprSig "expr" r
  SRecordUpdF :: r "expr" -> ![(NormalizedFieldLabel, r "expr")] -> HsExprSig "expr" r
  SArithFromF :: r "expr" -> HsExprSig "expr" r
  SArithFromThenF :: r "expr" -> r "expr" -> HsExprSig "expr" r
  SArithFromToF :: r "expr" -> r "expr" -> HsExprSig "expr" r
  SArithFromThenToF :: r "expr" -> r "expr" -> r "expr" -> HsExprSig "expr" r
  SGuardedF :: ![r "galt"] -> HsExprSig "expr" r
  SClausesF :: ![([HsPatF], r "expr")] -> HsExprSig "expr" r
  SMultiIfF :: ![r "galt"] -> HsExprSig "expr" r
  SExprWithTySigF :: r "expr" -> !NormalizedTypeText -> HsExprSig "expr" r
  SAppTypeF :: r "expr" -> !NormalizedTypeText -> HsExprSig "expr" r
  SOpaqueF :: !HsOpaqueTag -> HsExprSig "expr" r
  SBindStmtF :: !HsPatF -> r "expr" -> HsExprSig "stmt" r
  SBodyStmtF :: r "expr" -> HsExprSig "stmt" r
  SLetStmtF :: !LetMode -> ![(HsPatF, r "expr")] -> HsExprSig "stmt" r
  SGuardedAltF :: ![r "gstmt"] -> r "expr" -> HsExprSig "galt" r
  SGuardBoolF :: r "expr" -> HsExprSig "gstmt" r
  SGuardPatF :: !HsPatF -> r "expr" -> HsExprSig "gstmt" r
  SGuardLetF :: !LetMode -> ![(HsPatF, r "expr")] -> HsExprSig "gstmt" r

type HsExprSigTag :: Type
data HsExprSigTag
  = HsExprSigVarTag !HsVarRef
  | HsExprSigAppTag
  | HsExprSigLamTag !BinderAnn
  | HsExprSigLetTag !LetMode ![HsPatF]
  | HsExprSigOpAppTag
  | HsExprSigSectionLTag
  | HsExprSigSectionRTag
  | HsExprSigParTag
  | HsExprSigLitTag !NormalizedLit
  | HsExprSigOverLitTag !NormalizedOverLit
  | HsExprSigIfTag
  | HsExprSigCaseTag ![HsPatF]
  | HsExprSigDoTag
  | HsExprSigNegTag
  | HsExprSigExplicitListTag
  | HsExprSigExplicitTupleTag
  | HsExprSigRecordConTag ![NormalizedFieldLabel]
  | HsExprSigRecordUpdTag ![NormalizedFieldLabel]
  | HsExprSigArithFromTag
  | HsExprSigArithFromThenTag
  | HsExprSigArithFromToTag
  | HsExprSigArithFromThenToTag
  | HsExprSigGuardedTag
  | HsExprSigClausesTag ![[HsPatF]]
  | HsExprSigMultiIfTag
  | HsExprSigExprWithTySigTag !NormalizedTypeText
  | HsExprSigAppTypeTag !NormalizedTypeText
  | HsExprSigOpaqueTag !HsOpaqueTag
  | HsExprSigBindStmtTag !HsPatF
  | HsExprSigBodyStmtTag
  | HsExprSigLetStmtTag !LetMode ![HsPatF]
  | HsExprSigGuardedAltTag
  | HsExprSigGuardBoolTag
  | HsExprSigGuardPatTag !HsPatF
  | HsExprSigGuardLetTag !LetMode ![HsPatF]
  deriving stock (Eq, Ord, Show)

instance HTraversable HsExprSig where
  htraverseWithSort transform =
    \case
      SVarF name ->
        pure (SVarF name)
      SAppF function argument ->
        SAppF <$> expr function <*> expr argument
      SLamF binder body ->
        SLamF binder <$> expr body
      SLetF mode bindings body ->
        SLetF mode <$> traverse (traverse expr) bindings <*> expr body
      SOpAppF left operator right ->
        SOpAppF <$> expr left <*> expr operator <*> expr right
      SSectionLF left operator ->
        SSectionLF <$> expr left <*> expr operator
      SSectionRF operator right ->
        SSectionRF <$> expr operator <*> expr right
      SParF body ->
        SParF <$> expr body
      SLitF literalValue ->
        pure (SLitF literalValue)
      SOverLitF literalValue ->
        pure (SOverLitF literalValue)
      SIfF condition thenBranch elseBranch ->
        SIfF <$> expr condition <*> expr thenBranch <*> expr elseBranch
      SCaseF scrutinee alternatives ->
        SCaseF <$> expr scrutinee <*> traverse (traverse expr) alternatives
      SDoF statements ->
        SDoF <$> traverse stmt statements
      SNegF body ->
        SNegF <$> expr body
      SExplicitListF elements ->
        SExplicitListF <$> traverse expr elements
      SExplicitTupleF elements ->
        SExplicitTupleF <$> traverse expr elements
      SRecordConF constructor fields ->
        SRecordConF <$> expr constructor <*> traverse (traverse expr) fields
      SRecordUpdF record fields ->
        SRecordUpdF <$> expr record <*> traverse (traverse expr) fields
      SArithFromF fromValue ->
        SArithFromF <$> expr fromValue
      SArithFromThenF fromValue thenValue ->
        SArithFromThenF <$> expr fromValue <*> expr thenValue
      SArithFromToF fromValue toValue ->
        SArithFromToF <$> expr fromValue <*> expr toValue
      SArithFromThenToF fromValue thenValue toValue ->
        SArithFromThenToF <$> expr fromValue <*> expr thenValue <*> expr toValue
      SGuardedF alternatives ->
        SGuardedF <$> traverse guardedAlt alternatives
      SClausesF clauses ->
        SClausesF <$> traverse (traverse expr) clauses
      SMultiIfF alternatives ->
        SMultiIfF <$> traverse guardedAlt alternatives
      SExprWithTySigF body typeText ->
        (`SExprWithTySigF` typeText) <$> expr body
      SAppTypeF body typeText ->
        (`SAppTypeF` typeText) <$> expr body
      SOpaqueF tag ->
        pure (SOpaqueF tag)
      SBindStmtF patternValue body ->
        SBindStmtF patternValue <$> expr body
      SBodyStmtF body ->
        SBodyStmtF <$> expr body
      SLetStmtF mode bindings ->
        SLetStmtF mode <$> traverse (traverse expr) bindings
      SGuardedAltF guards body ->
        SGuardedAltF <$> traverse guardStmt guards <*> expr body
      SGuardBoolF condition ->
        SGuardBoolF <$> expr condition
      SGuardPatF patternValue body ->
        SGuardPatF patternValue <$> expr body
      SGuardLetF mode bindings ->
        SGuardLetF mode <$> traverse (traverse expr) bindings
    where
      expr = transform (SortWitness :: SortWitness "expr")
      stmt = transform (SortWitness :: SortWitness "stmt")
      guardedAlt = transform (SortWitness :: SortWitness "galt")
      guardStmt = transform (SortWitness :: SortWitness "gstmt")

instance RewriteSignature HsExprSig where
  type NodeTag HsExprSig = HsExprSigTag

  nodeTag =
    \case
      SVarF name -> HsExprSigVarTag name
      SAppF {} -> HsExprSigAppTag
      SLamF binder _ -> HsExprSigLamTag binder
      SLetF mode bindings _ -> HsExprSigLetTag mode (fmap fst bindings)
      SOpAppF {} -> HsExprSigOpAppTag
      SSectionLF {} -> HsExprSigSectionLTag
      SSectionRF {} -> HsExprSigSectionRTag
      SParF {} -> HsExprSigParTag
      SLitF literalValue -> HsExprSigLitTag literalValue
      SOverLitF literalValue -> HsExprSigOverLitTag literalValue
      SIfF {} -> HsExprSigIfTag
      SCaseF _ alternatives -> HsExprSigCaseTag (fmap fst alternatives)
      SDoF {} -> HsExprSigDoTag
      SNegF {} -> HsExprSigNegTag
      SExplicitListF {} -> HsExprSigExplicitListTag
      SExplicitTupleF {} -> HsExprSigExplicitTupleTag
      SRecordConF _ fields -> HsExprSigRecordConTag (fmap fst fields)
      SRecordUpdF _ fields -> HsExprSigRecordUpdTag (fmap fst fields)
      SArithFromF {} -> HsExprSigArithFromTag
      SArithFromThenF {} -> HsExprSigArithFromThenTag
      SArithFromToF {} -> HsExprSigArithFromToTag
      SArithFromThenToF {} -> HsExprSigArithFromThenToTag
      SGuardedF {} -> HsExprSigGuardedTag
      SClausesF clauses -> HsExprSigClausesTag (fmap fst clauses)
      SMultiIfF {} -> HsExprSigMultiIfTag
      SExprWithTySigF _ typeText -> HsExprSigExprWithTySigTag typeText
      SAppTypeF _ typeText -> HsExprSigAppTypeTag typeText
      SOpaqueF tag -> HsExprSigOpaqueTag tag
      SBindStmtF patternValue _ -> HsExprSigBindStmtTag patternValue
      SBodyStmtF {} -> HsExprSigBodyStmtTag
      SLetStmtF mode bindings -> HsExprSigLetStmtTag mode (fmap fst bindings)
      SGuardedAltF {} -> HsExprSigGuardedAltTag
      SGuardBoolF {} -> HsExprSigGuardBoolTag
      SGuardPatF patternValue _ -> HsExprSigGuardPatTag patternValue
      SGuardLetF mode bindings -> HsExprSigGuardLetTag mode (fmap fst bindings)

  nodeTagDigest _ =
    stableShowDigest

  nodeResultSort =
    \case
      SVarF {} -> SortWitness
      SAppF {} -> SortWitness
      SLamF {} -> SortWitness
      SLetF {} -> SortWitness
      SOpAppF {} -> SortWitness
      SSectionLF {} -> SortWitness
      SSectionRF {} -> SortWitness
      SParF {} -> SortWitness
      SLitF {} -> SortWitness
      SOverLitF {} -> SortWitness
      SIfF {} -> SortWitness
      SCaseF {} -> SortWitness
      SDoF {} -> SortWitness
      SNegF {} -> SortWitness
      SExplicitListF {} -> SortWitness
      SExplicitTupleF {} -> SortWitness
      SRecordConF {} -> SortWitness
      SRecordUpdF {} -> SortWitness
      SArithFromF {} -> SortWitness
      SArithFromThenF {} -> SortWitness
      SArithFromToF {} -> SortWitness
      SArithFromThenToF {} -> SortWitness
      SGuardedF {} -> SortWitness
      SClausesF {} -> SortWitness
      SMultiIfF {} -> SortWitness
      SExprWithTySigF {} -> SortWitness
      SAppTypeF {} -> SortWitness
      SOpaqueF {} -> SortWitness
      SBindStmtF {} -> SortWitness
      SBodyStmtF {} -> SortWitness
      SLetStmtF {} -> SortWitness
      SGuardedAltF {} -> SortWitness
      SGuardBoolF {} -> SortWitness
      SGuardPatF {} -> SortWitness
      SGuardLetF {} -> SortWitness

instance ZipMatch (Node HsExprSig) where
  zipMatch (Node leftSig) (Node rightSig)
    | nodeTag leftSig == nodeTag rightSig =
        Node <$> zipChildren leftSig (nodeChildren rightSig)
    | otherwise =
        Nothing

stableShowDigest :: Show a => a -> Word64
stableShowDigest value =
  Foldable.foldl'
    (\acc character -> acc * 16777619 + fromIntegral (fromEnum character))
    (2166136261 :: Word64)
    (show value)

zipChildren ::
  HTraversable sig =>
  sig sort (K left) ->
  [right] ->
  Maybe (sig sort (K (left, right)))
zipChildren leftSig rightChildren =
  case runStateT (htraverse takeRightChild leftSig) rightChildren of
    Just (zippedSig, []) ->
      Just zippedSig
    _ ->
      Nothing

takeRightChild :: K left sort -> StateT [right] Maybe (K (left, right) sort)
takeRightChild (K leftChild) =
  StateT $
    \case
      rightChild : remainingChildren ->
        Just (K (leftChild, rightChild), remainingChildren)
      [] ->
        Nothing
