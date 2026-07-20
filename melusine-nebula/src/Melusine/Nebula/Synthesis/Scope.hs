{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.Nebula.Synthesis.Scope
  ( closePairDefinitionBoundaryLocals,
    wellScopedDefinitionPattern,
    wellScopedDefinitionTerm,
  )
where

import Control.Monad (foldM)
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust)
import Data.Traversable (mapAccumL)
import Moonlight.Core (ClassId, Pattern (..), binderIdKey, patternVarKey)
import Moonlight.Core qualified as EGraph
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( BinderAnn (..),
    GuardedAltF (..),
    HsExprF (..),
    HsGuardStmtF (..),
    HsPatF,
    HsStmtF (..),
    HsVarRef (..),
    LetMode (..),
    LetRecursion (..),
    patBinders,
  )
import Moonlight.EGraph.Pure.AntiUnify (BinaryLGGResult (..))
import Data.Fix (Fix (..))

type BoundaryClosureState :: Type
data BoundaryClosureState = BoundaryClosureStateRow
  { bcsNextVar :: !EGraph.PatternVar,
    bcsSlots :: !(IntMap BoundarySlot),
    bcsLeftSubst :: !(IntMap ClassId),
    bcsRightSubst :: !(IntMap ClassId),
    bcsReplacedNodes :: !Int
  }

type BoundarySlot :: Type
data BoundarySlot = BoundarySlotRow
  { bsVar :: !EGraph.PatternVar,
    bsClass :: !ClassId
  }

type ScopedBoundaryState :: Type
data ScopedBoundaryState = ScopedBoundaryStateRow
  { sbsScope :: !IntSet.IntSet,
    sbsClosure :: !BoundaryClosureState
  }

closePairDefinitionBoundaryLocals ::
  (Pattern HsExprF -> Maybe ClassId) ->
  BinaryLGGResult HsExprF ClassId ->
  BinaryLGGResult HsExprF ClassId
closePairDefinitionBoundaryLocals resolveClass lggResult =
  let initialState =
        BoundaryClosureStateRow
          { bcsNextVar = nextPatternVar (binaryLggPattern lggResult) (binaryLggLeftBindings lggResult) (binaryLggRightBindings lggResult),
            bcsSlots = IntMap.empty,
            bcsLeftSubst = binaryLggLeftBindings lggResult,
            bcsRightSubst = binaryLggRightBindings lggResult,
            bcsReplacedNodes = 0
          }
      (closedTerm, finalState) =
        closeBoundaryPattern resolveClass IntSet.empty (binaryLggPattern lggResult) initialState
   in lggResult
        { binaryLggPattern = closedTerm,
          binaryLggLeftBindings = bcsLeftSubst finalState,
          binaryLggRightBindings = bcsRightSubst finalState,
          binaryLggSharedStructure = max 0 (binaryLggSharedStructure lggResult - bcsReplacedNodes finalState)
        }

nextPatternVar :: Pattern HsExprF -> IntMap ClassId -> IntMap ClassId -> EGraph.PatternVar
nextPatternVar patternValue leftSubst rightSubst =
  EGraph.mkPatternVar (1 + maximumVarKey)
  where
    maximumVarKey =
      foldr max (-1) (patternVarKeys patternValue <> substitutionKeys leftSubst <> substitutionKeys rightSubst)

substitutionKeys :: IntMap ClassId -> [Int]
substitutionKeys =
  IntMap.keys

patternVarKeys :: Pattern HsExprF -> [Int]
patternVarKeys = \case
  PatternVar patternVar ->
    [patternVarKey patternVar]
  PatternNode nodeValue ->
    foldMap patternVarKeys nodeValue

closeBoundaryPattern ::
  (Pattern HsExprF -> Maybe ClassId) ->
  IntSet.IntSet ->
  Pattern HsExprF ->
  BoundaryClosureState ->
  (Pattern HsExprF, BoundaryClosureState)
closeBoundaryPattern resolveClass scopeValue = \case
  PatternVar patternVar ->
    \stateValue -> (PatternVar patternVar, stateValue)
  PatternNode nodeValue ->
    closeBoundaryNode resolveClass scopeValue nodeValue

closeBoundaryNode ::
  (Pattern HsExprF -> Maybe ClassId) ->
  IntSet.IntSet ->
  HsExprF (Pattern HsExprF) ->
  BoundaryClosureState ->
  (Pattern HsExprF, BoundaryClosureState)
closeBoundaryNode resolveClass scopeValue nodeValue stateValue =
  case nodeValue of
    VarF (GlobalName _) ->
      (PatternNode nodeValue, stateValue)
    VarF (LocalName binderAnn)
      | IntSet.member (binderIdKey (baId binderAnn)) scopeValue ->
          (PatternNode nodeValue, stateValue)
      | otherwise ->
          closeBoundaryLocal resolveClass binderAnn stateValue
    LamF binderAnn bodyPattern ->
      let (closedBody, nextState) =
            closeBoundaryPattern resolveClass (insertBinderAnn binderAnn scopeValue) bodyPattern stateValue
       in (PatternNode (LamF binderAnn closedBody), nextState)
    LetF letModeValue localBinds bodyPattern ->
      closeBoundaryLet resolveClass scopeValue letModeValue localBinds bodyPattern stateValue
    CaseF scrutineePattern alternatives ->
      let (closedScrutinee, stateAfterScrutinee) =
            closeBoundaryPattern resolveClass scopeValue scrutineePattern stateValue
          (closedAlternatives, stateAfterAlternatives) =
            closeBoundaryCaseAlternatives resolveClass scopeValue alternatives stateAfterScrutinee
       in (PatternNode (CaseF closedScrutinee closedAlternatives), stateAfterAlternatives)
    DoF statements ->
      let (closedStatements, _statementScope, nextState) =
            closeBoundaryStatements resolveClass scopeValue statements stateValue
       in (PatternNode (DoF closedStatements), nextState)
    GuardedF alternatives ->
      let (closedAlternatives, nextState) =
            closeBoundaryGuardedAlternatives resolveClass scopeValue alternatives stateValue
       in (PatternNode (GuardedF closedAlternatives), nextState)
    MultiIfF alternatives ->
      let (closedAlternatives, nextState) =
            closeBoundaryGuardedAlternatives resolveClass scopeValue alternatives stateValue
       in (PatternNode (MultiIfF closedAlternatives), nextState)
    ClausesF clauses ->
      let (closedClauses, nextState) =
            closeBoundaryClauses resolveClass scopeValue clauses stateValue
       in (PatternNode (ClausesF closedClauses), nextState)
    _ ->
      let (closedNode, nextState) =
            closeBoundaryTraversableNode resolveClass scopeValue nodeValue stateValue
       in (PatternNode closedNode, nextState)

closeBoundaryLocal ::
  (Pattern HsExprF -> Maybe ClassId) ->
  BinderAnn ->
  BoundaryClosureState ->
  (Pattern HsExprF, BoundaryClosureState)
closeBoundaryLocal resolveClass binderAnn stateValue =
  let binderKey =
        binderIdKey (baId binderAnn)
   in case IntMap.lookup binderKey (bcsSlots stateValue) of
        Just slotValue ->
          (PatternVar (bsVar slotValue), stateValue {bcsReplacedNodes = bcsReplacedNodes stateValue + 1})
        Nothing ->
          case resolveClass (PatternNode (VarF (LocalName binderAnn))) of
            Just classId ->
              let patternVar =
                    bcsNextVar stateValue
                  patternKey =
                    patternVarKey patternVar
                  slotValue =
                    BoundarySlotRow
                      { bsVar = patternVar,
                        bsClass = classId
                      }
               in ( PatternVar patternVar,
                    stateValue
                      { bcsNextVar = succ patternVar,
                        bcsSlots = IntMap.insert binderKey slotValue (bcsSlots stateValue),
                        bcsLeftSubst = IntMap.insert patternKey (bsClass slotValue) (bcsLeftSubst stateValue),
                        bcsRightSubst = IntMap.insert patternKey (bsClass slotValue) (bcsRightSubst stateValue),
                        bcsReplacedNodes = bcsReplacedNodes stateValue + 1
                      }
                  )
            Nothing ->
              (PatternNode (VarF (LocalName binderAnn)), stateValue)

closeBoundaryLet ::
  (Pattern HsExprF -> Maybe ClassId) ->
  IntSet.IntSet ->
  LetMode ->
  [(HsPatF, Pattern HsExprF)] ->
  Pattern HsExprF ->
  BoundaryClosureState ->
  (Pattern HsExprF, BoundaryClosureState)
closeBoundaryLet resolveClass scopeValue letModeValue localBinds bodyPattern stateValue =
  let bodyScope =
        extendScopeWithBindingRows localBinds scopeValue
      rhsScope =
        case lmRecursion letModeValue of
          NonRecursiveBinds ->
            scopeValue
          RecursiveOpaqueBinds ->
            bodyScope
      (closedRows, stateAfterRows) =
        closeBoundaryRows resolveClass rhsScope localBinds stateValue
      (closedBody, stateAfterBody) =
        closeBoundaryPattern resolveClass bodyScope bodyPattern stateAfterRows
   in (PatternNode (LetF letModeValue closedRows closedBody), stateAfterBody)

closeBoundaryRows ::
  (Pattern HsExprF -> Maybe ClassId) ->
  IntSet.IntSet ->
  [(HsPatF, Pattern HsExprF)] ->
  BoundaryClosureState ->
  ([(HsPatF, Pattern HsExprF)], BoundaryClosureState)
closeBoundaryRows resolveClass scopeValue rows stateValue =
  let (nextState, closedRows) =
        mapAccumL closeRow stateValue rows
   in (closedRows, nextState)
  where
    closeRow currentState (rowPattern, rowBody) =
      let (closedBody, nextState) =
            closeBoundaryPattern resolveClass scopeValue rowBody currentState
       in (nextState, (rowPattern, closedBody))

closeBoundaryCaseAlternatives ::
  (Pattern HsExprF -> Maybe ClassId) ->
  IntSet.IntSet ->
  [(HsPatF, Pattern HsExprF)] ->
  BoundaryClosureState ->
  ([(HsPatF, Pattern HsExprF)], BoundaryClosureState)
closeBoundaryCaseAlternatives resolveClass scopeValue alternatives stateValue =
  let (nextState, closedAlternatives) =
        mapAccumL closeAlternative stateValue alternatives
   in (closedAlternatives, nextState)
  where
    closeAlternative currentState (casePattern, bodyPattern) =
      let (closedBody, nextState) =
            closeBoundaryPattern resolveClass (extendScopeWithPat casePattern scopeValue) bodyPattern currentState
       in (nextState, (casePattern, closedBody))

closeBoundaryClauses ::
  (Pattern HsExprF -> Maybe ClassId) ->
  IntSet.IntSet ->
  [([HsPatF], Pattern HsExprF)] ->
  BoundaryClosureState ->
  ([([HsPatF], Pattern HsExprF)], BoundaryClosureState)
closeBoundaryClauses resolveClass scopeValue clauses stateValue =
  let (nextState, closedClauses) =
        mapAccumL closeClause stateValue clauses
   in (closedClauses, nextState)
  where
    closeClause currentState (clausePatterns, bodyPattern) =
      let clauseScope =
            foldr extendScopeWithPat scopeValue clausePatterns
          (closedBody, nextState) =
            closeBoundaryPattern resolveClass clauseScope bodyPattern currentState
       in (nextState, (clausePatterns, closedBody))

closeBoundaryStatements ::
  (Pattern HsExprF -> Maybe ClassId) ->
  IntSet.IntSet ->
  [HsStmtF (Pattern HsExprF)] ->
  BoundaryClosureState ->
  ([HsStmtF (Pattern HsExprF)], IntSet.IntSet, BoundaryClosureState)
closeBoundaryStatements resolveClass scopeValue statements stateValue =
  let initialState =
        ScopedBoundaryStateRow scopeValue stateValue
      (finalState, closedStatements) =
        mapAccumL (closeBoundaryStatement resolveClass) initialState statements
   in (closedStatements, sbsScope finalState, sbsClosure finalState)

closeBoundaryStatement ::
  (Pattern HsExprF -> Maybe ClassId) ->
  ScopedBoundaryState ->
  HsStmtF (Pattern HsExprF) ->
  (ScopedBoundaryState, HsStmtF (Pattern HsExprF))
closeBoundaryStatement resolveClass scopedState statement =
  case statement of
    BindStmtF bindPattern rhsPattern ->
      let (closedRhs, nextState) =
            closeBoundaryPattern resolveClass (sbsScope scopedState) rhsPattern (sbsClosure scopedState)
       in ( ScopedBoundaryStateRow (extendScopeWithPat bindPattern (sbsScope scopedState)) nextState,
            BindStmtF bindPattern closedRhs
          )
    BodyStmtF bodyPattern ->
      let (closedBody, nextState) =
            closeBoundaryPattern resolveClass (sbsScope scopedState) bodyPattern (sbsClosure scopedState)
       in (scopedState {sbsClosure = nextState}, BodyStmtF closedBody)
    LetStmtF letModeValue localBinds ->
      let bodyScope =
            extendScopeWithBindingRows localBinds (sbsScope scopedState)
          rhsScope =
            case lmRecursion letModeValue of
              NonRecursiveBinds ->
                sbsScope scopedState
              RecursiveOpaqueBinds ->
                bodyScope
          (closedRows, nextState) =
            closeBoundaryRows resolveClass rhsScope localBinds (sbsClosure scopedState)
       in ( ScopedBoundaryStateRow bodyScope nextState,
            LetStmtF letModeValue closedRows
          )

closeBoundaryGuardedAlternatives ::
  (Pattern HsExprF -> Maybe ClassId) ->
  IntSet.IntSet ->
  [GuardedAltF (Pattern HsExprF)] ->
  BoundaryClosureState ->
  ([GuardedAltF (Pattern HsExprF)], BoundaryClosureState)
closeBoundaryGuardedAlternatives resolveClass scopeValue alternatives stateValue =
  let (nextState, closedAlternatives) =
        mapAccumL closeAlternative stateValue alternatives
   in (closedAlternatives, nextState)
  where
    closeAlternative currentState guardedAlt =
      let (closedGuards, guardScope, stateAfterGuards) =
            closeBoundaryGuardStatements resolveClass scopeValue (gaGuards guardedAlt) currentState
          (closedBody, stateAfterBody) =
            closeBoundaryPattern resolveClass guardScope (gaBody guardedAlt) stateAfterGuards
       in (stateAfterBody, GuardedAltF closedGuards closedBody)

closeBoundaryGuardStatements ::
  (Pattern HsExprF -> Maybe ClassId) ->
  IntSet.IntSet ->
  [HsGuardStmtF (Pattern HsExprF)] ->
  BoundaryClosureState ->
  ([HsGuardStmtF (Pattern HsExprF)], IntSet.IntSet, BoundaryClosureState)
closeBoundaryGuardStatements resolveClass scopeValue guards stateValue =
  let initialState =
        ScopedBoundaryStateRow scopeValue stateValue
      (finalState, closedGuards) =
        mapAccumL (closeBoundaryGuardStatement resolveClass) initialState guards
   in (closedGuards, sbsScope finalState, sbsClosure finalState)

closeBoundaryGuardStatement ::
  (Pattern HsExprF -> Maybe ClassId) ->
  ScopedBoundaryState ->
  HsGuardStmtF (Pattern HsExprF) ->
  (ScopedBoundaryState, HsGuardStmtF (Pattern HsExprF))
closeBoundaryGuardStatement resolveClass scopedState guardStatement =
  case guardStatement of
    GuardBoolF guardPattern ->
      let (closedGuard, nextState) =
            closeBoundaryPattern resolveClass (sbsScope scopedState) guardPattern (sbsClosure scopedState)
       in (scopedState {sbsClosure = nextState}, GuardBoolF closedGuard)
    GuardPatF guardPattern rhsPattern ->
      let (closedRhs, nextState) =
            closeBoundaryPattern resolveClass (sbsScope scopedState) rhsPattern (sbsClosure scopedState)
       in ( ScopedBoundaryStateRow (extendScopeWithPat guardPattern (sbsScope scopedState)) nextState,
            GuardPatF guardPattern closedRhs
          )
    GuardLetF letModeValue localBinds ->
      let bodyScope =
            extendScopeWithBindingRows localBinds (sbsScope scopedState)
          rhsScope =
            case lmRecursion letModeValue of
              NonRecursiveBinds ->
                sbsScope scopedState
              RecursiveOpaqueBinds ->
                bodyScope
          (closedRows, nextState) =
            closeBoundaryRows resolveClass rhsScope localBinds (sbsClosure scopedState)
       in ( ScopedBoundaryStateRow bodyScope nextState,
            GuardLetF letModeValue closedRows
          )

closeBoundaryTraversableNode ::
  (Pattern HsExprF -> Maybe ClassId) ->
  IntSet.IntSet ->
  HsExprF (Pattern HsExprF) ->
  BoundaryClosureState ->
  (HsExprF (Pattern HsExprF), BoundaryClosureState)
closeBoundaryTraversableNode resolveClass scopeValue nodeValue stateValue =
  let (nextState, closedNode) =
        mapAccumL closeChild stateValue nodeValue
   in (closedNode, nextState)
  where
    closeChild currentState childPattern =
      let (closedChild, nextState) =
            closeBoundaryPattern resolveClass scopeValue childPattern currentState
       in (nextState, closedChild)

wellScopedDefinitionPattern :: Pattern HsExprF -> Bool
wellScopedDefinitionPattern =
  wellScopedPattern IntSet.empty

wellScopedDefinitionTerm :: Fix HsExprF -> Bool
wellScopedDefinitionTerm =
  wellScopedTerm IntSet.empty

wellScopedPattern :: IntSet.IntSet -> Pattern HsExprF -> Bool
wellScopedPattern scopeValue = \case
  PatternVar {} ->
    True
  PatternNode nodeValue ->
    case nodeValue of
      VarF (GlobalName _) ->
        True
      VarF (LocalName binderAnn) ->
        IntSet.member (binderIdKey (baId binderAnn)) scopeValue
      LamF binderAnn bodyPattern ->
        wellScopedPattern (insertBinderAnn binderAnn scopeValue) bodyPattern
      LetF letModeValue localBinds bodyPattern ->
        wellScopedPatternLetRows scopeValue letModeValue localBinds
          && wellScopedPattern (extendScopeWithBindingRows localBinds scopeValue) bodyPattern
      CaseF scrutineePattern alternatives ->
        wellScopedPattern scopeValue scrutineePattern
          && all (wellScopedPatternCaseAlternative scopeValue) alternatives
      DoF statements ->
        wellScopedPatternStatements scopeValue statements
      GuardedF alternatives ->
        all (wellScopedPatternGuardedAlt scopeValue) alternatives
      MultiIfF alternatives ->
        all (wellScopedPatternGuardedAlt scopeValue) alternatives
      ClausesF clauses ->
        all (wellScopedPatternClause scopeValue) clauses
      node ->
        all (wellScopedPattern scopeValue) (toList node)

wellScopedPatternLetRows :: IntSet.IntSet -> LetMode -> [(HsPatF, Pattern HsExprF)] -> Bool
wellScopedPatternLetRows scopeValue letModeValue localBinds =
  all (wellScopedPattern rhsScope . snd) localBinds
  where
    rhsScope =
      case lmRecursion letModeValue of
        NonRecursiveBinds ->
          scopeValue
        RecursiveOpaqueBinds ->
          extendScopeWithBindingRows localBinds scopeValue

wellScopedPatternCaseAlternative :: IntSet.IntSet -> (HsPatF, Pattern HsExprF) -> Bool
wellScopedPatternCaseAlternative scopeValue (casePattern, bodyPattern) =
  wellScopedPattern (extendScopeWithPat casePattern scopeValue) bodyPattern

wellScopedPatternClause :: IntSet.IntSet -> ([HsPatF], Pattern HsExprF) -> Bool
wellScopedPatternClause scopeValue (clausePatterns, bodyPattern) =
  wellScopedPattern (foldr extendScopeWithPat scopeValue clausePatterns) bodyPattern

wellScopedPatternStatements :: IntSet.IntSet -> [HsStmtF (Pattern HsExprF)] -> Bool
wellScopedPatternStatements scopeValue statements =
  isJust (foldM wellScopedPatternStatement scopeValue statements)

wellScopedPatternStatement :: IntSet.IntSet -> HsStmtF (Pattern HsExprF) -> Maybe IntSet.IntSet
wellScopedPatternStatement scopeValue = \case
  BindStmtF bindPattern rhsPattern
    | wellScopedPattern scopeValue rhsPattern ->
        Just (extendScopeWithPat bindPattern scopeValue)
  BodyStmtF bodyPattern
    | wellScopedPattern scopeValue bodyPattern ->
        Just scopeValue
  LetStmtF letModeValue localBinds
    | wellScopedPatternLetRows scopeValue letModeValue localBinds ->
        Just (extendScopeWithBindingRows localBinds scopeValue)
  _ ->
    Nothing

wellScopedPatternGuardedAlt :: IntSet.IntSet -> GuardedAltF (Pattern HsExprF) -> Bool
wellScopedPatternGuardedAlt scopeValue guardedAlt =
  case foldM wellScopedPatternGuardStatement scopeValue (gaGuards guardedAlt) of
    Just guardScope ->
      wellScopedPattern guardScope (gaBody guardedAlt)
    Nothing ->
      False

wellScopedPatternGuardStatement :: IntSet.IntSet -> HsGuardStmtF (Pattern HsExprF) -> Maybe IntSet.IntSet
wellScopedPatternGuardStatement scopeValue = \case
  GuardBoolF guardPattern
    | wellScopedPattern scopeValue guardPattern ->
        Just scopeValue
  GuardPatF guardPattern rhsPattern
    | wellScopedPattern scopeValue rhsPattern ->
        Just (extendScopeWithPat guardPattern scopeValue)
  GuardLetF letModeValue localBinds
    | wellScopedPatternLetRows scopeValue letModeValue localBinds ->
        Just (extendScopeWithBindingRows localBinds scopeValue)
  _ ->
    Nothing

wellScopedTerm :: IntSet.IntSet -> Fix HsExprF -> Bool
wellScopedTerm scopeValue (Fix nodeValue) =
  case nodeValue of
    VarF (GlobalName _) ->
      True
    VarF (LocalName binderAnn) ->
      IntSet.member (binderIdKey (baId binderAnn)) scopeValue
    LamF binderAnn bodyTerm ->
      wellScopedTerm (insertBinderAnn binderAnn scopeValue) bodyTerm
    LetF letModeValue localBinds bodyTerm ->
      wellScopedLetRows scopeValue letModeValue localBinds
        && wellScopedTerm (extendScopeWithBindingRows localBinds scopeValue) bodyTerm
    CaseF scrutineeTerm alternatives ->
      wellScopedTerm scopeValue scrutineeTerm
        && all (wellScopedCaseAlternative scopeValue) alternatives
    DoF statements ->
      wellScopedStatements scopeValue statements
    GuardedF alternatives ->
      all (wellScopedGuardedAlt scopeValue) alternatives
    MultiIfF alternatives ->
      all (wellScopedGuardedAlt scopeValue) alternatives
    ClausesF clauses ->
      all (wellScopedClause scopeValue) clauses
    node ->
      all (wellScopedTerm scopeValue) (toList node)

wellScopedLetRows :: IntSet.IntSet -> LetMode -> [(HsPatF, Fix HsExprF)] -> Bool
wellScopedLetRows scopeValue letModeValue localBinds =
  all (wellScopedTerm rhsScope . snd) localBinds
  where
    rhsScope =
      case lmRecursion letModeValue of
        NonRecursiveBinds ->
          scopeValue
        RecursiveOpaqueBinds ->
          extendScopeWithBindingRows localBinds scopeValue

wellScopedCaseAlternative :: IntSet.IntSet -> (HsPatF, Fix HsExprF) -> Bool
wellScopedCaseAlternative scopeValue (casePattern, bodyTerm) =
  wellScopedTerm (extendScopeWithPat casePattern scopeValue) bodyTerm

wellScopedClause :: IntSet.IntSet -> ([HsPatF], Fix HsExprF) -> Bool
wellScopedClause scopeValue (clausePatterns, bodyTerm) =
  wellScopedTerm (foldr extendScopeWithPat scopeValue clausePatterns) bodyTerm

wellScopedStatements :: IntSet.IntSet -> [HsStmtF (Fix HsExprF)] -> Bool
wellScopedStatements scopeValue statements =
  isJust (foldM wellScopedStatement scopeValue statements)

wellScopedStatement :: IntSet.IntSet -> HsStmtF (Fix HsExprF) -> Maybe IntSet.IntSet
wellScopedStatement scopeValue = \case
  BindStmtF bindPattern rhsTerm
    | wellScopedTerm scopeValue rhsTerm ->
        Just (extendScopeWithPat bindPattern scopeValue)
  BodyStmtF bodyTerm
    | wellScopedTerm scopeValue bodyTerm ->
        Just scopeValue
  LetStmtF letModeValue localBinds
    | wellScopedLetRows scopeValue letModeValue localBinds ->
        Just (extendScopeWithBindingRows localBinds scopeValue)
  _ ->
    Nothing

wellScopedGuardedAlt :: IntSet.IntSet -> GuardedAltF (Fix HsExprF) -> Bool
wellScopedGuardedAlt scopeValue guardedAlt =
  case foldM wellScopedGuardStatement scopeValue (gaGuards guardedAlt) of
    Just guardScope ->
      wellScopedTerm guardScope (gaBody guardedAlt)
    Nothing ->
      False

wellScopedGuardStatement :: IntSet.IntSet -> HsGuardStmtF (Fix HsExprF) -> Maybe IntSet.IntSet
wellScopedGuardStatement scopeValue = \case
  GuardBoolF guardTerm
    | wellScopedTerm scopeValue guardTerm ->
        Just scopeValue
  GuardPatF guardPattern rhsTerm
    | wellScopedTerm scopeValue rhsTerm ->
        Just (extendScopeWithPat guardPattern scopeValue)
  GuardLetF letModeValue localBinds
    | wellScopedLetRows scopeValue letModeValue localBinds ->
        Just (extendScopeWithBindingRows localBinds scopeValue)
  _ ->
    Nothing

extendScopeWithBindingRows :: [(HsPatF, r)] -> IntSet.IntSet -> IntSet.IntSet
extendScopeWithBindingRows localBinds scopeValue =
  foldr (extendScopeWithPat . fst) scopeValue localBinds

extendScopeWithPat :: HsPatF -> IntSet.IntSet -> IntSet.IntSet
extendScopeWithPat patternValue scopeValue =
  foldr insertBinderAnn scopeValue (patBinders patternValue)

insertBinderAnn :: BinderAnn -> IntSet.IntSet -> IntSet.IntSet
insertBinderAnn binderAnn =
  IntSet.insert (binderIdKey (baId binderAnn))
