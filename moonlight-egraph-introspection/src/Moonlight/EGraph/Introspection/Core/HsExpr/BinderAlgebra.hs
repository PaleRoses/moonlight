{-# LANGUAGE LambdaCase #-}

module Moonlight.EGraph.Introspection.Core.HsExpr.BinderAlgebra
  ( hsExprBinderSubstAlgebra,
    substituteBinderHsExpr,
  )
where

import Moonlight.Core (BinderId, Pattern (..))
import Moonlight.Rewrite.Runtime (BinderSubstAlgebra (..))
import Moonlight.Pale.Ghc.Expr

hsExprBinderSubstAlgebra :: BinderSubstAlgebra HsExprF
hsExprBinderSubstAlgebra =
  BinderSubstAlgebra
    { bsaSubstituteBinder = substituteBinderHsExpr
    }

substituteBinderHsExpr ::
  BinderId ->
  Pattern HsExprF ->
  Pattern HsExprF ->
  Pattern HsExprF
substituteBinderHsExpr targetBinderId argumentPattern =
  go
  where
    replacementPattern =
      case argumentPattern of
        PatternNode (LamF _ _) ->
          PatternNode (ParF argumentPattern)
        _ ->
          argumentPattern

    go patternValue =
      case patternValue of
        PatternVar patternVar ->
          PatternVar patternVar
        PatternNode patternNode ->
          case patternNode of
            VarF (LocalName binderAnn)
              | baId binderAnn == targetBinderId ->
                  replacementPattern
            LamF binderAnn bodyExpr
              | baId binderAnn == targetBinderId ->
                  PatternNode (LamF binderAnn bodyExpr)
              | otherwise ->
                  PatternNode (LamF binderAnn (go bodyExpr))
            LetF letModeValue bindingValues bodyExpr ->
              PatternNode (LetF letModeValue (substituteBindings letModeValue bindingValues) (substituteLetBody letModeValue bindingValues bodyExpr))
            CaseF scrutineeExpr branchValues ->
              PatternNode (CaseF (go scrutineeExpr) (fmap substituteCaseAlternative branchValues))
            ClausesF clauseValues ->
              PatternNode (ClausesF (fmap substituteClause clauseValues))
            DoF statementValues ->
              PatternNode (DoF (substituteStatements statementValues))
            GuardedF alternativeValues ->
              PatternNode (GuardedF (fmap substituteGuardedAlternative alternativeValues))
            MultiIfF alternativeValues ->
              PatternNode (MultiIfF (fmap substituteGuardedAlternative alternativeValues))
            _ ->
              PatternNode (fmap go patternNode)

    substituteBindings letModeValue bindingValues =
      if bindsTarget bindingValues && lmRecursion letModeValue == RecursiveOpaqueBinds
        then bindingValues
        else fmap substituteBinding bindingValues

    substituteBinding (bindingPattern, rhsExpr) =
      (bindingPattern, go rhsExpr)

    substituteLetBody letModeValue bindingValues bodyExpr
      | bindsTarget bindingValues =
          case lmRecursion letModeValue of
            NonRecursiveBinds -> bodyExpr
            RecursiveOpaqueBinds -> bodyExpr
      | otherwise =
          go bodyExpr

    substituteCaseAlternative (casePattern, branchExpr)
      | targetBinderId `elem` fmap baId (patBinders casePattern) =
          (casePattern, branchExpr)
      | otherwise =
          (casePattern, go branchExpr)

    substituteClause (clausePatterns, bodyExpr)
      | targetBinderId `elem` fmap baId (foldMap patBinders clausePatterns) =
          (clausePatterns, bodyExpr)
      | otherwise =
          (clausePatterns, go bodyExpr)

    substituteStatements statementValues =
      case statementValues of
        [] ->
          []
        statementValue : remainingValues ->
          case statementValue of
            BindStmtF bindPattern rhsExpr ->
              let remainingStatements =
                    if targetBinderId `elem` fmap baId (patBinders bindPattern)
                      then remainingValues
                      else substituteStatements remainingValues
               in BindStmtF bindPattern (go rhsExpr) : remainingStatements
            BodyStmtF exprValue ->
              BodyStmtF (go exprValue) : substituteStatements remainingValues
            LetStmtF letModeValue bindingValues ->
              let bindingTargets = bindingTargetIds bindingValues
                  updatedBindings =
                    if targetBinderId `elem` bindingTargets && lmRecursion letModeValue == RecursiveOpaqueBinds
                      then bindingValues
                      else fmap substituteBinding bindingValues
                  remainingStatements =
                    if targetBinderId `elem` bindingTargets
                      then remainingValues
                      else substituteStatements remainingValues
               in LetStmtF letModeValue updatedBindings : remainingStatements

    substituteGuardedAlternative (GuardedAltF guardValues bodyExpr) =
      let (updatedGuards, shadowed) = substituteGuardValues guardValues
       in GuardedAltF updatedGuards (if shadowed then bodyExpr else go bodyExpr)

    substituteGuardValues = \case
      [] ->
        ([], False)
      guardValue : remainingValues ->
        case guardValue of
          GuardBoolF guardExpr ->
            let (remainingGuards, shadowed) = substituteGuardValues remainingValues
             in (GuardBoolF (go guardExpr) : remainingGuards, shadowed)
          GuardPatF guardPattern guardExpr ->
            let guard' = GuardPatF guardPattern (go guardExpr)
             in if targetBinderId `elem` fmap baId (patBinders guardPattern)
                  then (guard' : remainingValues, True)
                  else
                    let (remainingGuards, shadowed) = substituteGuardValues remainingValues
                     in (guard' : remainingGuards, shadowed)
          GuardLetF letModeValue bindingValues ->
            let bindingTargets = bindingTargetIds bindingValues
                updatedBindings =
                  if targetBinderId `elem` bindingTargets && lmRecursion letModeValue == RecursiveOpaqueBinds
                    then bindingValues
                    else fmap substituteBinding bindingValues
             in if targetBinderId `elem` bindingTargets
                  then (GuardLetF letModeValue updatedBindings : remainingValues, True)
                  else
                    let (remainingGuards, shadowed) = substituteGuardValues remainingValues
                     in (GuardLetF letModeValue updatedBindings : remainingGuards, shadowed)

    bindsTarget =
      any (== targetBinderId) . bindingTargetIds

    bindingTargetIds :: [(HsPatF, r)] -> [BinderId]
    bindingTargetIds =
      fmap baId . foldMap (patBinders . fst)
