{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.Nebula.Discovery.AlphaUnify
  ( alphaUnifyTerms,
  )
where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Traversable (mapAccumM)
import Moonlight.Core (Pattern (..), binderIdKey, patternVarKey)
import Moonlight.Core qualified as EGraph
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( BinderAnn (..),
    GuardedAltF (..),
    HsExprF (..),
    HsGuardStmtF (..),
    HsPatF (..),
    HsStmtF (..),
    HsVarRef (..),
    LetMode (..),
    LetRecursion (..),
    NormalizedArithSeq (..),
  )
import Moonlight.EGraph.Pure.AntiUnify (BinaryLGGResult (..))
import Moonlight.EGraph.Pure.Types (ClassId, classIdKey)
import Data.Fix (Fix (..))

type PatternClassResolver :: Type
type PatternClassResolver = Pattern HsExprF -> Maybe ClassId

type AlphaResult :: Type -> Type
data AlphaResult a
  = AlphaMatched !a
  | AlphaMismatch
  | AlphaObstructed

instance Functor AlphaResult where
  fmap mapValue = \case
    AlphaMatched value -> AlphaMatched (mapValue value)
    AlphaMismatch -> AlphaMismatch
    AlphaObstructed -> AlphaObstructed

instance Applicative AlphaResult where
  pure =
    AlphaMatched

  AlphaMatched mapValue <*> AlphaMatched value =
    AlphaMatched (mapValue value)
  AlphaObstructed <*> _ =
    AlphaObstructed
  _ <*> AlphaObstructed =
    AlphaObstructed
  AlphaMismatch <*> _ =
    AlphaMismatch
  _ <*> AlphaMismatch =
    AlphaMismatch

instance Monad AlphaResult where
  AlphaMatched value >>= next =
    next value
  AlphaMismatch >>= _ =
    AlphaMismatch
  AlphaObstructed >>= _ =
    AlphaObstructed

type BinderBijection :: Type
data BinderBijection = BinderBijection
  { bbLeftToRight :: !(IntMap Int),
    bbRightToLeft :: !(IntMap Int)
  }

type AlphaUnifyState :: Type
data AlphaUnifyState = AlphaUnifyState
  { ausNextVar :: !EGraph.PatternVar,
    ausLeftSubst :: !(IntMap ClassId),
    ausRightSubst :: !(IntMap ClassId),
    ausHoleMemo :: !(Map (Int, Int) EGraph.PatternVar),
    ausSharedStructure :: !Int
  }

alphaUnifyTerms ::
  PatternClassResolver ->
  Fix HsExprF ->
  Fix HsExprF ->
  Maybe (BinaryLGGResult HsExprF ClassId)
alphaUnifyTerms resolveClass leftTerm rightTerm =
  case alphaUnifyTerm resolveClass emptyBijection initialAlphaUnifyState leftTerm rightTerm of
    AlphaMatched (patternValue, stateValue) ->
      Just (lggFromState patternValue stateValue)
    AlphaMismatch ->
      Nothing
    AlphaObstructed ->
      Nothing

initialAlphaUnifyState :: AlphaUnifyState
initialAlphaUnifyState =
  AlphaUnifyState
    { ausNextVar = EGraph.mkPatternVar 0,
      ausLeftSubst = IntMap.empty,
      ausRightSubst = IntMap.empty,
      ausHoleMemo = Map.empty,
      ausSharedStructure = 0
    }

emptyBijection :: BinderBijection
emptyBijection =
  BinderBijection
    { bbLeftToRight = IntMap.empty,
      bbRightToLeft = IntMap.empty
    }

alphaUnifyTerm ::
  PatternClassResolver ->
  BinderBijection ->
  AlphaUnifyState ->
  Fix HsExprF ->
  Fix HsExprF ->
  AlphaResult (Pattern HsExprF, AlphaUnifyState)
alphaUnifyTerm resolveClass binders stateValue leftTerm@(Fix leftNode) rightTerm@(Fix rightNode)
  | sameClosedResolvedClass resolveClass leftTerm rightTerm =
      AlphaMatched
        ( eraseFixTerm leftTerm,
          stateValue {ausSharedStructure = ausSharedStructure stateValue + fixNodeCount leftTerm}
        )
  | otherwise =
      case alphaUnifyNode resolveClass binders stateValue leftNode rightNode of
        AlphaMatched (nodePattern, nextState) ->
          AlphaMatched (PatternNode nodePattern, nextState {ausSharedStructure = ausSharedStructure nextState + 1})
        AlphaMismatch ->
          freshHole resolveClass leftTerm rightTerm stateValue
        AlphaObstructed ->
          AlphaObstructed

sameClosedResolvedClass :: PatternClassResolver -> Fix HsExprF -> Fix HsExprF -> Bool
sameClosedResolvedClass resolveClass leftTerm rightTerm =
  not (fixMentionsLocal leftTerm)
    && not (fixMentionsLocal rightTerm)
    && case (resolveClass (eraseFixTerm leftTerm), resolveClass (eraseFixTerm rightTerm)) of
      (Just leftClass, Just rightClass) -> leftClass == rightClass
      _ -> False

fixMentionsLocal :: Fix HsExprF -> Bool
fixMentionsLocal (Fix nodeValue) =
  case nodeValue of
    VarF (LocalName _) ->
      True
    _ ->
      any fixMentionsLocal nodeValue

fixNodeCount :: Fix HsExprF -> Int
fixNodeCount (Fix nodeValue) =
  1 + sum (fmap fixNodeCount nodeValue)

alphaUnifyNode ::
  PatternClassResolver ->
  BinderBijection ->
  AlphaUnifyState ->
  HsExprF (Fix HsExprF) ->
  HsExprF (Fix HsExprF) ->
  AlphaResult (HsExprF (Pattern HsExprF), AlphaUnifyState)
alphaUnifyNode resolveClass binders stateValue leftNode rightNode =
  case (leftNode, rightNode) of
    (VarF (GlobalName leftName), VarF (GlobalName rightName))
      | leftName == rightName ->
          AlphaMatched (VarF (GlobalName leftName), stateValue)
    (VarF (LocalName leftBinder), VarF (LocalName rightBinder))
      | boundBinderPair binders leftBinder rightBinder ->
          AlphaMatched (VarF (LocalName leftBinder), stateValue)
    (AppF leftFn leftArg, AppF rightFn rightArg) ->
      unifyBinary AppF binders stateValue leftFn leftArg rightFn rightArg
    (LamF leftBinder leftBody, LamF rightBinder rightBody) -> do
      bodyBinders <- extendBinderPair leftBinder rightBinder binders
      (bodyPattern, nextState) <- alphaUnifyTerm resolveClass bodyBinders stateValue leftBody rightBody
      AlphaMatched (LamF leftBinder bodyPattern, nextState)
    (LetF leftMode leftRows leftBody, LetF rightMode rightRows rightBody)
      | leftMode == rightMode ->
          alphaUnifyLet resolveClass binders stateValue leftMode leftRows rightRows leftBody rightBody
    (OpAppF leftA leftOp leftB, OpAppF rightA rightOp rightB) ->
      unifyTernary OpAppF binders stateValue leftA leftOp leftB rightA rightOp rightB
    (SectionLF leftOp leftArg, SectionLF rightOp rightArg) ->
      unifyBinary SectionLF binders stateValue leftOp leftArg rightOp rightArg
    (SectionRF leftArg leftOp, SectionRF rightArg rightOp) ->
      unifyBinary SectionRF binders stateValue leftArg leftOp rightArg rightOp
    (ParF leftExpr, ParF rightExpr) ->
      unifyUnary ParF binders stateValue leftExpr rightExpr
    (LitF leftLit, LitF rightLit)
      | leftLit == rightLit ->
          AlphaMatched (LitF leftLit, stateValue)
    (OverLitF leftLit, OverLitF rightLit)
      | leftLit == rightLit ->
          AlphaMatched (OverLitF leftLit, stateValue)
    (IfF leftCond leftThen leftElse, IfF rightCond rightThen rightElse) ->
      unifyTernary IfF binders stateValue leftCond leftThen leftElse rightCond rightThen rightElse
    (CaseF leftScrutinee leftBranches, CaseF rightScrutinee rightBranches) ->
      alphaUnifyCase resolveClass binders stateValue leftScrutinee rightScrutinee leftBranches rightBranches
    (DoF leftStatements, DoF rightStatements) ->
      first DoF <$> alphaUnifyStatements resolveClass binders stateValue leftStatements rightStatements
    (NegF leftExpr, NegF rightExpr) ->
      unifyUnary NegF binders stateValue leftExpr rightExpr
    (ExplicitListF leftExprs, ExplicitListF rightExprs) ->
      first ExplicitListF <$> alphaUnifyTermList resolveClass binders stateValue leftExprs rightExprs
    (ExplicitTupleF leftExprs, ExplicitTupleF rightExprs) ->
      first ExplicitTupleF <$> alphaUnifyTermList resolveClass binders stateValue leftExprs rightExprs
    (RecordConF leftHead leftFields, RecordConF rightHead rightFields) ->
      alphaUnifyRecord RecordConF resolveClass binders stateValue leftHead rightHead leftFields rightFields
    (RecordUpdF leftHead leftFields, RecordUpdF rightHead rightFields) ->
      alphaUnifyRecord RecordUpdF resolveClass binders stateValue leftHead rightHead leftFields rightFields
    (ArithSeqF leftSeq, ArithSeqF rightSeq) ->
      alphaUnifyArithSeq resolveClass binders stateValue leftSeq rightSeq
    (GuardedF leftAlts, GuardedF rightAlts) ->
      first GuardedF <$> alphaUnifyGuardedAlts resolveClass binders stateValue leftAlts rightAlts
    (ClausesF leftClauses, ClausesF rightClauses) ->
      first ClausesF <$> alphaUnifyClauses resolveClass binders stateValue leftClauses rightClauses
    (MultiIfF leftAlts, MultiIfF rightAlts) ->
      first MultiIfF <$> alphaUnifyGuardedAlts resolveClass binders stateValue leftAlts rightAlts
    (ExprWithTySigF leftExpr leftTy, ExprWithTySigF rightExpr rightTy)
      | leftTy == rightTy ->
          unifyUnary (`ExprWithTySigF` leftTy) binders stateValue leftExpr rightExpr
    (AppTypeF leftExpr leftTy, AppTypeF rightExpr rightTy)
      | leftTy == rightTy ->
          unifyUnary (`AppTypeF` leftTy) binders stateValue leftExpr rightExpr
    (OpaqueF leftTag, OpaqueF rightTag)
      | leftTag == rightTag ->
          AlphaMatched (OpaqueF leftTag, stateValue)
    _ ->
      AlphaMismatch
  where
    unifyUnary makeNode currentBinders currentState leftChild rightChild = do
      (childPattern, nextState) <- alphaUnifyTerm resolveClass currentBinders currentState leftChild rightChild
      AlphaMatched (makeNode childPattern, nextState)
    unifyBinary makeNode currentBinders currentState leftA leftB rightA rightB = do
      (leftPattern, stateAfterLeft) <- alphaUnifyTerm resolveClass currentBinders currentState leftA rightA
      (rightPattern, stateAfterRight) <- alphaUnifyTerm resolveClass currentBinders stateAfterLeft leftB rightB
      AlphaMatched (makeNode leftPattern rightPattern, stateAfterRight)
    unifyTernary makeNode currentBinders currentState leftA leftB leftC rightA rightB rightC = do
      (firstPattern, stateAfterFirst) <- alphaUnifyTerm resolveClass currentBinders currentState leftA rightA
      (secondPattern, stateAfterSecond) <- alphaUnifyTerm resolveClass currentBinders stateAfterFirst leftB rightB
      (thirdPattern, stateAfterThird) <- alphaUnifyTerm resolveClass currentBinders stateAfterSecond leftC rightC
      AlphaMatched (makeNode firstPattern secondPattern thirdPattern, stateAfterThird)

alphaUnifyLet ::
  PatternClassResolver ->
  BinderBijection ->
  AlphaUnifyState ->
  LetMode ->
  [(HsPatF, Fix HsExprF)] ->
  [(HsPatF, Fix HsExprF)] ->
  Fix HsExprF ->
  Fix HsExprF ->
  AlphaResult (HsExprF (Pattern HsExprF), AlphaUnifyState)
alphaUnifyLet resolveClass binders stateValue letModeValue leftRows rightRows leftBody rightBody = do
  (matchedRows, bodyBinders) <- matchBindingRowPatterns binders leftRows rightRows
  let rhsBinders =
        case lmRecursion letModeValue of
          NonRecursiveBinds -> binders
          RecursiveOpaqueBinds -> bodyBinders
  (rhsRows, stateAfterRows) <- alphaUnifyBindingRhsRows resolveClass rhsBinders stateValue matchedRows
  (bodyPattern, stateAfterBody) <- alphaUnifyTerm resolveClass bodyBinders stateAfterRows leftBody rightBody
  AlphaMatched (LetF letModeValue rhsRows bodyPattern, stateAfterBody)

alphaUnifyCase ::
  PatternClassResolver ->
  BinderBijection ->
  AlphaUnifyState ->
  Fix HsExprF ->
  Fix HsExprF ->
  [(HsPatF, Fix HsExprF)] ->
  [(HsPatF, Fix HsExprF)] ->
  AlphaResult (HsExprF (Pattern HsExprF), AlphaUnifyState)
alphaUnifyCase resolveClass binders stateValue leftScrutinee rightScrutinee leftBranches rightBranches = do
  (scrutineePattern, stateAfterScrutinee) <- alphaUnifyTerm resolveClass binders stateValue leftScrutinee rightScrutinee
  (branchPatterns, stateAfterBranches) <- alphaUnifyBranches resolveClass binders stateAfterScrutinee leftBranches rightBranches
  AlphaMatched (CaseF scrutineePattern branchPatterns, stateAfterBranches)

alphaUnifyRecord ::
  Ord field =>
  (Pattern HsExprF -> [(field, Pattern HsExprF)] -> HsExprF (Pattern HsExprF)) ->
  PatternClassResolver ->
  BinderBijection ->
  AlphaUnifyState ->
  Fix HsExprF ->
  Fix HsExprF ->
  [(field, Fix HsExprF)] ->
  [(field, Fix HsExprF)] ->
  AlphaResult (HsExprF (Pattern HsExprF), AlphaUnifyState)
alphaUnifyRecord makeNode resolveClass binders stateValue leftHead rightHead leftFields rightFields = do
  (headPattern, stateAfterHead) <- alphaUnifyTerm resolveClass binders stateValue leftHead rightHead
  (fieldPatterns, stateAfterFields) <- alphaUnifyFieldRows resolveClass binders stateAfterHead leftFields rightFields
  AlphaMatched (makeNode headPattern fieldPatterns, stateAfterFields)

alphaUnifyArithSeq ::
  PatternClassResolver ->
  BinderBijection ->
  AlphaUnifyState ->
  NormalizedArithSeq (Fix HsExprF) ->
  NormalizedArithSeq (Fix HsExprF) ->
  AlphaResult (HsExprF (Pattern HsExprF), AlphaUnifyState)
alphaUnifyArithSeq resolveClass binders stateValue leftNode rightNode =
  case (leftNode, rightNode) of
    (ArithSeqFrom leftA, ArithSeqFrom rightA) ->
      wrap ArithSeqFrom stateValue leftA rightA
    (ArithSeqFromThen leftA leftB, ArithSeqFromThen rightA rightB) ->
      wrap2 ArithSeqFromThen stateValue leftA leftB rightA rightB
    (ArithSeqFromTo leftA leftB, ArithSeqFromTo rightA rightB) ->
      wrap2 ArithSeqFromTo stateValue leftA leftB rightA rightB
    (ArithSeqFromThenTo leftA leftB leftC, ArithSeqFromThenTo rightA rightB rightC) ->
      wrap3 ArithSeqFromThenTo stateValue leftA leftB leftC rightA rightB rightC
    _ ->
      AlphaMismatch
  where
    wrap makeSeq currentState leftA rightA = do
      (patternA, stateAfterA) <- alphaUnifyTerm resolveClass binders currentState leftA rightA
      AlphaMatched (ArithSeqF (makeSeq patternA), stateAfterA)
    wrap2 makeSeq currentState leftA leftB rightA rightB = do
      (patternA, stateAfterA) <- alphaUnifyTerm resolveClass binders currentState leftA rightA
      (patternB, stateAfterB) <- alphaUnifyTerm resolveClass binders stateAfterA leftB rightB
      AlphaMatched (ArithSeqF (makeSeq patternA patternB), stateAfterB)
    wrap3 makeSeq currentState leftA leftB leftC rightA rightB rightC = do
      (patternA, stateAfterA) <- alphaUnifyTerm resolveClass binders currentState leftA rightA
      (patternB, stateAfterB) <- alphaUnifyTerm resolveClass binders stateAfterA leftB rightB
      (patternC, stateAfterC) <- alphaUnifyTerm resolveClass binders stateAfterB leftC rightC
      AlphaMatched (ArithSeqF (makeSeq patternA patternB patternC), stateAfterC)

alphaUnifyTermList ::
  PatternClassResolver ->
  BinderBijection ->
  AlphaUnifyState ->
  [Fix HsExprF] ->
  [Fix HsExprF] ->
  AlphaResult ([Pattern HsExprF], AlphaUnifyState)
alphaUnifyTermList resolveClass binders stateValue leftTerms rightTerms = do
  termPairs <- zipEqual leftTerms rightTerms
  swapAccumResult
    <$>
    mapAccumM
      ( \currentState (leftTerm, rightTerm) ->
          fmap
            (\(patternValue, nextState) -> (nextState, patternValue))
            (alphaUnifyTerm resolveClass binders currentState leftTerm rightTerm)
      )
      stateValue
      termPairs

alphaUnifyFieldRows ::
  Ord field =>
  PatternClassResolver ->
  BinderBijection ->
  AlphaUnifyState ->
  [(field, Fix HsExprF)] ->
  [(field, Fix HsExprF)] ->
  AlphaResult ([(field, Pattern HsExprF)], AlphaUnifyState)
alphaUnifyFieldRows resolveClass binders stateValue leftFields rightFields = do
  fieldPairs <- zipEqual (sortOn fst leftFields) (sortOn fst rightFields)
  swapAccumResult
    <$>
    mapAccumM
      ( \currentState ((leftField, leftTerm), (rightField, rightTerm)) ->
          if leftField == rightField
            then
              fmap
                (\(fieldPattern, nextState) -> (nextState, (leftField, fieldPattern)))
                (alphaUnifyTerm resolveClass binders currentState leftTerm rightTerm)
            else AlphaMismatch
      )
      stateValue
      fieldPairs

alphaUnifyBranches ::
  PatternClassResolver ->
  BinderBijection ->
  AlphaUnifyState ->
  [(HsPatF, Fix HsExprF)] ->
  [(HsPatF, Fix HsExprF)] ->
  AlphaResult ([(HsPatF, Pattern HsExprF)], AlphaUnifyState)
alphaUnifyBranches resolveClass binders stateValue leftBranches rightBranches = do
  branchPairs <- maybe (zipEqual leftBranches rightBranches) AlphaMatched (alignRowsBy (branchPatternKey . fst) leftBranches rightBranches)
  swapAccumResult
    <$>
    mapAccumM
      ( \currentState ((leftPattern, leftBody), (rightPattern, rightBody)) -> do
          (branchPattern, branchBinders) <- matchPattern binders leftPattern rightPattern
          (bodyPattern, nextState) <- alphaUnifyTerm resolveClass branchBinders currentState leftBody rightBody
          AlphaMatched (nextState, (branchPattern, bodyPattern))
      )
      stateValue
      branchPairs

alphaUnifyClauses ::
  PatternClassResolver ->
  BinderBijection ->
  AlphaUnifyState ->
  [([HsPatF], Fix HsExprF)] ->
  [([HsPatF], Fix HsExprF)] ->
  AlphaResult ([([HsPatF], Pattern HsExprF)], AlphaUnifyState)
alphaUnifyClauses resolveClass binders stateValue leftClauses rightClauses = do
  clausePairs <- zipEqual leftClauses rightClauses
  swapAccumResult
    <$>
    mapAccumM
      ( \currentState ((leftPatterns, leftBody), (rightPatterns, rightBody)) -> do
          (clausePatterns, clauseBinders) <- matchPatternList binders leftPatterns rightPatterns
          (bodyPattern, nextState) <- alphaUnifyTerm resolveClass clauseBinders currentState leftBody rightBody
          AlphaMatched (nextState, (clausePatterns, bodyPattern))
      )
      stateValue
      clausePairs

alphaUnifyStatements ::
  PatternClassResolver ->
  BinderBijection ->
  AlphaUnifyState ->
  [HsStmtF (Fix HsExprF)] ->
  [HsStmtF (Fix HsExprF)] ->
  AlphaResult ([HsStmtF (Pattern HsExprF)], AlphaUnifyState)
alphaUnifyStatements resolveClass binders stateValue leftStatements rightStatements = do
  statementPairs <- zipEqual leftStatements rightStatements
  (\(nextState, _, reversedStatements) -> (reverse reversedStatements, nextState))
    <$> foldM
      ( \(currentState, currentBinders, builtStatements) (leftStatement, rightStatement) -> do
          (statementPattern, nextBinders, nextState) <-
            alphaUnifyStatement resolveClass currentBinders currentState leftStatement rightStatement
          AlphaMatched (nextState, nextBinders, statementPattern : builtStatements)
      )
      (stateValue, binders, [])
      statementPairs

alphaUnifyStatement ::
  PatternClassResolver ->
  BinderBijection ->
  AlphaUnifyState ->
  HsStmtF (Fix HsExprF) ->
  HsStmtF (Fix HsExprF) ->
  AlphaResult (HsStmtF (Pattern HsExprF), BinderBijection, AlphaUnifyState)
alphaUnifyStatement resolveClass binders stateValue leftStatement rightStatement =
  case (leftStatement, rightStatement) of
    (BindStmtF leftPattern leftRhs, BindStmtF rightPattern rightRhs) -> do
      (statementPattern, nextBinders) <- matchPattern binders leftPattern rightPattern
      (rhsPattern, nextState) <- alphaUnifyTerm resolveClass binders stateValue leftRhs rightRhs
      AlphaMatched (BindStmtF statementPattern rhsPattern, nextBinders, nextState)
    (BodyStmtF leftExpr, BodyStmtF rightExpr) -> do
      (exprPattern, nextState) <- alphaUnifyTerm resolveClass binders stateValue leftExpr rightExpr
      AlphaMatched (BodyStmtF exprPattern, binders, nextState)
    (LetStmtF leftMode leftRows, LetStmtF rightMode rightRows)
      | leftMode == rightMode ->
          alphaUnifyLetStatement resolveClass binders stateValue leftMode leftRows rightRows
    _ ->
      AlphaMismatch

alphaUnifyLetStatement ::
  PatternClassResolver ->
  BinderBijection ->
  AlphaUnifyState ->
  LetMode ->
  [(HsPatF, Fix HsExprF)] ->
  [(HsPatF, Fix HsExprF)] ->
  AlphaResult (HsStmtF (Pattern HsExprF), BinderBijection, AlphaUnifyState)
alphaUnifyLetStatement resolveClass binders stateValue letModeValue leftRows rightRows = do
  (matchedRows, nextBinders) <- matchBindingRowPatterns binders leftRows rightRows
  let rhsBinders =
        case lmRecursion letModeValue of
          NonRecursiveBinds -> binders
          RecursiveOpaqueBinds -> nextBinders
  (rhsRows, nextState) <- alphaUnifyBindingRhsRows resolveClass rhsBinders stateValue matchedRows
  AlphaMatched (LetStmtF letModeValue rhsRows, nextBinders, nextState)

alphaUnifyGuardedAlts ::
  PatternClassResolver ->
  BinderBijection ->
  AlphaUnifyState ->
  [GuardedAltF (Fix HsExprF)] ->
  [GuardedAltF (Fix HsExprF)] ->
  AlphaResult ([GuardedAltF (Pattern HsExprF)], AlphaUnifyState)
alphaUnifyGuardedAlts resolveClass binders stateValue leftAlts rightAlts = do
  altPairs <- zipEqual leftAlts rightAlts
  swapAccumResult
    <$>
    mapAccumM
      ( \currentState (leftAlt, rightAlt) ->
          fmap
            (\(altPattern, nextState) -> (nextState, altPattern))
            (alphaUnifyGuardedAlt resolveClass binders currentState leftAlt rightAlt)
      )
      stateValue
      altPairs

alphaUnifyGuardedAlt ::
  PatternClassResolver ->
  BinderBijection ->
  AlphaUnifyState ->
  GuardedAltF (Fix HsExprF) ->
  GuardedAltF (Fix HsExprF) ->
  AlphaResult (GuardedAltF (Pattern HsExprF), AlphaUnifyState)
alphaUnifyGuardedAlt resolveClass binders stateValue leftAlt rightAlt = do
  (guardPatterns, guardBinders, stateAfterGuards) <-
    alphaUnifyGuardStatements resolveClass binders stateValue (gaGuards leftAlt) (gaGuards rightAlt)
  (bodyPattern, stateAfterBody) <- alphaUnifyTerm resolveClass guardBinders stateAfterGuards (gaBody leftAlt) (gaBody rightAlt)
  AlphaMatched (GuardedAltF guardPatterns bodyPattern, stateAfterBody)

alphaUnifyGuardStatements ::
  PatternClassResolver ->
  BinderBijection ->
  AlphaUnifyState ->
  [HsGuardStmtF (Fix HsExprF)] ->
  [HsGuardStmtF (Fix HsExprF)] ->
  AlphaResult ([HsGuardStmtF (Pattern HsExprF)], BinderBijection, AlphaUnifyState)
alphaUnifyGuardStatements resolveClass binders stateValue leftGuards rightGuards = do
  guardPairs <- zipEqual leftGuards rightGuards
  (\(nextState, nextBinders, reversedGuards) -> (reverse reversedGuards, nextBinders, nextState))
    <$> foldM
      ( \(currentState, currentBinders, builtGuards) (leftGuard, rightGuard) -> do
          (guardPattern, nextBinders, nextState) <-
            alphaUnifyGuardStatement resolveClass currentBinders currentState leftGuard rightGuard
          AlphaMatched (nextState, nextBinders, guardPattern : builtGuards)
      )
      (stateValue, binders, [])
      guardPairs

alphaUnifyGuardStatement ::
  PatternClassResolver ->
  BinderBijection ->
  AlphaUnifyState ->
  HsGuardStmtF (Fix HsExprF) ->
  HsGuardStmtF (Fix HsExprF) ->
  AlphaResult (HsGuardStmtF (Pattern HsExprF), BinderBijection, AlphaUnifyState)
alphaUnifyGuardStatement resolveClass binders stateValue leftGuard rightGuard =
  case (leftGuard, rightGuard) of
    (GuardBoolF leftExpr, GuardBoolF rightExpr) -> do
      (exprPattern, nextState) <- alphaUnifyTerm resolveClass binders stateValue leftExpr rightExpr
      AlphaMatched (GuardBoolF exprPattern, binders, nextState)
    (GuardPatF leftPattern leftRhs, GuardPatF rightPattern rightRhs) -> do
      (guardPattern, nextBinders) <- matchPattern binders leftPattern rightPattern
      (rhsPattern, nextState) <- alphaUnifyTerm resolveClass binders stateValue leftRhs rightRhs
      AlphaMatched (GuardPatF guardPattern rhsPattern, nextBinders, nextState)
    (GuardLetF leftMode leftRows, GuardLetF rightMode rightRows)
      | leftMode == rightMode -> do
          (matchedRows, nextBinders) <- matchBindingRowPatterns binders leftRows rightRows
          let rhsBinders =
                case lmRecursion leftMode of
                  NonRecursiveBinds -> binders
                  RecursiveOpaqueBinds -> nextBinders
          (rhsRows, nextState) <- alphaUnifyBindingRhsRows resolveClass rhsBinders stateValue matchedRows
          AlphaMatched (GuardLetF leftMode rhsRows, nextBinders, nextState)
    _ ->
      AlphaMismatch

type MatchedBindingRow :: Type
type MatchedBindingRow = (HsPatF, Fix HsExprF, Fix HsExprF)

matchBindingRowPatterns ::
  BinderBijection ->
  [(HsPatF, Fix HsExprF)] ->
  [(HsPatF, Fix HsExprF)] ->
  AlphaResult ([MatchedBindingRow], BinderBijection)
matchBindingRowPatterns binders leftRows rightRows = do
  rowPairs <- zipEqual leftRows rightRows
  swapAccumResult
    <$>
    mapAccumM
      ( \currentBinders ((leftPattern, leftRhs), (rightPattern, rightRhs)) -> do
          (matchedPattern, nextBinders) <- matchPattern currentBinders leftPattern rightPattern
          AlphaMatched (nextBinders, (matchedPattern, leftRhs, rightRhs))
      )
      binders
      rowPairs

alphaUnifyBindingRhsRows ::
  PatternClassResolver ->
  BinderBijection ->
  AlphaUnifyState ->
  [MatchedBindingRow] ->
  AlphaResult ([(HsPatF, Pattern HsExprF)], AlphaUnifyState)
alphaUnifyBindingRhsRows resolveClass binders stateValue matchedRows =
  swapAccumResult
    <$>
    mapAccumM
      ( \currentState (rowPattern, leftRhs, rightRhs) -> do
          (rhsPattern, nextState) <- alphaUnifyTerm resolveClass binders currentState leftRhs rightRhs
          AlphaMatched (nextState, (rowPattern, rhsPattern))
      )
      stateValue
      matchedRows

matchPatternList ::
  BinderBijection ->
  [HsPatF] ->
  [HsPatF] ->
  AlphaResult ([HsPatF], BinderBijection)
matchPatternList binders leftPatterns rightPatterns = do
  patternPairs <- zipEqual leftPatterns rightPatterns
  swapAccumResult
    <$>
    mapAccumM
      ( \currentBinders (leftPattern, rightPattern) -> do
          (patternValue, nextBinders) <- matchPattern currentBinders leftPattern rightPattern
          AlphaMatched (nextBinders, patternValue)
      )
      binders
      patternPairs

matchPattern :: BinderBijection -> HsPatF -> HsPatF -> AlphaResult (HsPatF, BinderBijection)
matchPattern binders leftPattern rightPattern =
  case (leftPattern, rightPattern) of
    (PVarP leftBinder, PVarP rightBinder) ->
      (,) (PVarP leftBinder) <$> extendBinderPair leftBinder rightBinder binders
    (PWildP, PWildP) ->
      AlphaMatched (PWildP, binders)
    (PConP leftName leftPatterns, PConP rightName rightPatterns)
      | leftName == rightName ->
          first (PConP leftName) <$> matchPatternList binders leftPatterns rightPatterns
    (PTupleP leftPatterns, PTupleP rightPatterns) ->
      first PTupleP <$> matchPatternList binders leftPatterns rightPatterns
    (PListP leftPatterns, PListP rightPatterns) ->
      first PListP <$> matchPatternList binders leftPatterns rightPatterns
    (PLitP leftLit, PLitP rightLit)
      | leftLit == rightLit ->
          AlphaMatched (PLitP leftLit, binders)
    (POverLitP leftLit, POverLitP rightLit)
      | leftLit == rightLit ->
          AlphaMatched (POverLitP leftLit, binders)
    (PAsP leftBinder leftSubPattern, PAsP rightBinder rightSubPattern) -> do
      asBinders <- extendBinderPair leftBinder rightBinder binders
      (subPattern, nextBinders) <- matchPattern asBinders leftSubPattern rightSubPattern
      AlphaMatched (PAsP leftBinder subPattern, nextBinders)
    (PBangP leftSubPattern, PBangP rightSubPattern) ->
      first PBangP <$> matchPattern binders leftSubPattern rightSubPattern
    (PLazyP leftSubPattern, PLazyP rightSubPattern) ->
      first PLazyP <$> matchPattern binders leftSubPattern rightSubPattern
    (PParP leftSubPattern, PParP rightSubPattern) ->
      first PParP <$> matchPattern binders leftSubPattern rightSubPattern
    (PRecP leftName leftFields, PRecP rightName rightFields)
      | leftName == rightName ->
          first (PRecP leftName) <$> matchPatternFields binders leftFields rightFields
    (PLossyP leftTag leftBinders, PLossyP rightTag rightBinders)
      | leftTag == rightTag -> do
          nextBinders <- extendBinderPairs binders leftBinders rightBinders
          AlphaMatched (PLossyP leftTag leftBinders, nextBinders)
    _ ->
      AlphaMismatch

matchPatternFields ::
  BinderBijection ->
  [(String, HsPatF)] ->
  [(String, HsPatF)] ->
  AlphaResult ([(String, HsPatF)], BinderBijection)
matchPatternFields binders leftFields rightFields = do
  fieldPairs <- zipEqual (sortOn fst leftFields) (sortOn fst rightFields)
  swapAccumResult
    <$>
    mapAccumM
      ( \currentBinders ((leftField, leftPattern), (rightField, rightPattern)) ->
          if leftField == rightField
            then do
              (fieldPattern, nextBinders) <- matchPattern currentBinders leftPattern rightPattern
              AlphaMatched (nextBinders, (leftField, fieldPattern))
            else AlphaMismatch
      )
      binders
      fieldPairs

alignRowsBy :: Ord key => (row -> Maybe key) -> [row] -> [row] -> Maybe [(row, row)]
alignRowsBy rowKey leftRows rightRows = do
  leftTable <- uniqueKeyTable rowKey leftRows
  rightTable <- uniqueKeyTable rowKey rightRows
  if Map.keysSet leftTable == Map.keysSet rightTable
    then Just (Map.elems (Map.intersectionWith (,) leftTable rightTable))
    else Nothing

uniqueKeyTable :: Ord key => (row -> Maybe key) -> [row] -> Maybe (Map key row)
uniqueKeyTable rowKey rows =
  let keyedRows =
        [ (key, row)
        | row <- rows,
          key <- maybe [] pure (rowKey row)
        ]
      table = Map.fromList keyedRows
   in if length keyedRows == length rows && Map.size table == length rows
        then Just table
        else Nothing

branchPatternKey :: HsPatF -> Maybe HsPatF
branchPatternKey = \case
  PConP constructorName _ ->
    Just (PConP constructorName [])
  PLitP literalValue ->
    Just (PLitP literalValue)
  POverLitP literalValue ->
    Just (POverLitP literalValue)
  _ ->
    Nothing

extendBinderPairs :: BinderBijection -> [BinderAnn] -> [BinderAnn] -> AlphaResult BinderBijection
extendBinderPairs binders leftBinders rightBinders = do
  binderPairs <- zipEqual leftBinders rightBinders
  foldM
    (\currentBinders (leftBinder, rightBinder) -> extendBinderPair leftBinder rightBinder currentBinders)
    binders
    binderPairs

extendBinderPair :: BinderAnn -> BinderAnn -> BinderBijection -> AlphaResult BinderBijection
extendBinderPair leftBinder rightBinder binders =
  let leftKey = binderIdKey (baId leftBinder)
      rightKey = binderIdKey (baId rightBinder)
      leftCompatible =
        maybe True (== rightKey) (IntMap.lookup leftKey (bbLeftToRight binders))
      rightCompatible =
        maybe True (== leftKey) (IntMap.lookup rightKey (bbRightToLeft binders))
   in if leftCompatible && rightCompatible
        then
          AlphaMatched
            ( BinderBijection
              { bbLeftToRight = IntMap.insert leftKey rightKey (bbLeftToRight binders),
                bbRightToLeft = IntMap.insert rightKey leftKey (bbRightToLeft binders)
              }
            )
        else AlphaMismatch

boundBinderPair :: BinderBijection -> BinderAnn -> BinderAnn -> Bool
boundBinderPair binders leftBinder rightBinder =
  IntMap.lookup (binderIdKey (baId leftBinder)) (bbLeftToRight binders)
    == Just (binderIdKey (baId rightBinder))

freshHole ::
  PatternClassResolver ->
  Fix HsExprF ->
  Fix HsExprF ->
  AlphaUnifyState ->
  AlphaResult (Pattern HsExprF, AlphaUnifyState)
freshHole resolveClass leftTerm rightTerm stateValue =
  case (resolveClass (eraseFixTerm leftTerm), resolveClass (eraseFixTerm rightTerm)) of
    (Just leftClass, Just rightClass) ->
      let memoKey = (classIdKey leftClass, classIdKey rightClass)
       in case Map.lookup memoKey (ausHoleMemo stateValue) of
            Just patternVar ->
              AlphaMatched (PatternVar patternVar, stateValue)
            Nothing ->
              let patternVar = ausNextVar stateValue
                  patternKey = patternVarKey patternVar
               in AlphaMatched
                    ( PatternVar patternVar,
                      stateValue
                        { ausNextVar = succ patternVar,
                          ausLeftSubst = IntMap.insert patternKey leftClass (ausLeftSubst stateValue),
                          ausRightSubst = IntMap.insert patternKey rightClass (ausRightSubst stateValue),
                          ausHoleMemo = Map.insert memoKey patternVar (ausHoleMemo stateValue)
                        }
                    )
    _ ->
      AlphaObstructed

eraseFixTerm :: Fix HsExprF -> Pattern HsExprF
eraseFixTerm (Fix nodeValue) =
  PatternNode (fmap eraseFixTerm nodeValue)

lggFromState :: Pattern HsExprF -> AlphaUnifyState -> BinaryLGGResult HsExprF ClassId
lggFromState patternValue stateValue =
  BinaryLGGResult
    { binaryLggPattern = patternValue,
      binaryLggLeftBindings = ausLeftSubst stateValue,
      binaryLggRightBindings = ausRightSubst stateValue,
      binaryLggSharedStructure = ausSharedStructure stateValue
    }

zipEqual :: [left] -> [right] -> AlphaResult [(left, right)]
zipEqual leftValues rightValues =
  if length leftValues == length rightValues
    then AlphaMatched (zip leftValues rightValues)
    else AlphaMismatch

swapAccumResult :: (state, value) -> (value, state)
swapAccumResult (stateValue, value) =
  (value, stateValue)
