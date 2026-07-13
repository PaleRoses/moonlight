{-# LANGUAGE GHC2024 #-}

-- | Canonical checked-system store for the system stratum.
-- Owns the rule-name map, declaration order, rewrite ids, next synthetic id,
-- and projections out of 'LogicalDecoration'.
-- Contracts: names and ids are globally unique, allocation skips used ids,
-- and appended rewrites preserve declaration order.
module Moonlight.Rewrite.System.Checked
  ( CheckedRewrite (..),
    checkedRewriteLhs,
    checkedRewriteRhs,
    checkedRewriteInterface,
    checkedRewriteOrigin,
    checkedRewriteCondition,
    checkedRewriteApplicationCondition,
    checkedRewritePostSubst,
    CheckedSystem,
    CheckedSystemError (..),
    checkedSystemFromRewrites,
    appendCheckedRewrite,
    checkedRuleNames,
    checkedRewrites,
    lookupCheckedRewrite,
    allocateSystemRuleId,
    firstFreeRuleId,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core
  ( Pattern,
    RewriteRuleId (..),
    firstDuplicate,
  )
import Moonlight.Rewrite.Algebra
  ( CompiledApplicationCondition,
  )
import Moonlight.Rewrite.Algebra
  ( PatternInterface,
    PatternRewrite,
    RewriteOrigin,
    prDecoration,
    prInterface,
    prLeft,
    prOrigin,
    prRight,
  )
import Moonlight.Rewrite.Runtime
  ( PostMatchSubst,
  )
import Moonlight.Rewrite.System.Logic.Decoration
  ( LogicalDecoration,
    ldApplicationCondition,
    ldCondition,
    ldPostSubst,
  )
import Moonlight.Rewrite.System.Logic.Guard
  ( CompiledGuard,
  )
import Moonlight.Rewrite.System.Origin
  ( RuleOrigin,
  )
import Moonlight.Rewrite.System.RuleName
  ( RuleName,
  )

type CheckedRewrite :: Type -> (Type -> Type) -> Type
data CheckedRewrite capability f = CheckedRewrite
  { checkedRewriteId :: !RewriteRuleId,
    checkedRewriteName :: !RuleName,
    checkedRewriteAlgebra :: !(PatternRewrite RuleOrigin (LogicalDecoration capability) f)
  }

deriving stock instance
  Eq (PatternRewrite RuleOrigin (LogicalDecoration capability) f) =>
  Eq (CheckedRewrite capability f)

deriving stock instance
  Show (PatternRewrite RuleOrigin (LogicalDecoration capability) f) =>
  Show (CheckedRewrite capability f)

checkedRewriteLhs :: CheckedRewrite capability f -> Pattern f
checkedRewriteLhs =
  prLeft . checkedRewriteAlgebra

checkedRewriteRhs :: CheckedRewrite capability f -> Pattern f
checkedRewriteRhs =
  prRight . checkedRewriteAlgebra

checkedRewriteInterface :: CheckedRewrite capability f -> PatternInterface
checkedRewriteInterface =
  prInterface . checkedRewriteAlgebra

checkedRewriteOrigin :: CheckedRewrite capability f -> RewriteOrigin RuleOrigin
checkedRewriteOrigin =
  prOrigin . checkedRewriteAlgebra

checkedRewriteCondition :: CheckedRewrite capability f -> Maybe (CompiledGuard capability f)
checkedRewriteCondition =
  ldCondition . prDecoration . checkedRewriteAlgebra

checkedRewriteApplicationCondition ::
  CheckedRewrite capability f ->
  Maybe (CompiledApplicationCondition (CompiledGuard capability f) f)
checkedRewriteApplicationCondition =
  ldApplicationCondition . prDecoration . checkedRewriteAlgebra

checkedRewritePostSubst :: CheckedRewrite capability f -> Maybe (PostMatchSubst f)
checkedRewritePostSubst =
  ldPostSubst . prDecoration . checkedRewriteAlgebra

type CheckedSystem :: Type -> (Type -> Type) -> Type
data CheckedSystem capability f = CheckedSystem
  { checkedRewriteMap :: !(Map RuleName (CheckedRewrite capability f)),
    checkedRewriteOrder :: ![CheckedRewrite capability f],
    checkedNextId :: !Int
  }

type CheckedSystemError :: Type
data CheckedSystemError
  = CheckedSystemDuplicateRuleName !RuleName
  | CheckedSystemDuplicateRuleId !RewriteRuleId
  deriving stock (Eq, Ord, Show)

checkedSystemFromRewrites ::
  Int ->
  [CheckedRewrite capability f] ->
  Either CheckedSystemError (CheckedSystem capability f)
checkedSystemFromRewrites nextId rewriteValues = do
  validateCheckedRewriteNames rewriteValues
  validateCheckedRewriteIds rewriteValues
  Right
    CheckedSystem
      { checkedRewriteMap =
          Map.fromList
            [ (checkedRewriteName rewriteValue, rewriteValue)
              | rewriteValue <- rewriteValues
            ],
        checkedRewriteOrder = rewriteValues,
        checkedNextId = nextId
      }

appendCheckedRewrite ::
  Int ->
  CheckedRewrite capability f ->
  CheckedSystem capability f ->
  Either CheckedSystemError (CheckedSystem capability f)
appendCheckedRewrite nextId rewriteValue checkedSystem =
  case lookupCheckedRewrite (checkedRewriteName rewriteValue) checkedSystem of
    Just _ ->
      Left (CheckedSystemDuplicateRuleName (checkedRewriteName rewriteValue))
    Nothing
      | checkedRewriteId rewriteValue `Set.member` usedIds ->
          Left (CheckedSystemDuplicateRuleId (checkedRewriteId rewriteValue))
      | otherwise ->
          Right
            checkedSystem
              { checkedRewriteMap =
                  Map.insert (checkedRewriteName rewriteValue) rewriteValue (checkedRewriteMap checkedSystem),
                checkedRewriteOrder =
                  checkedRewriteOrder checkedSystem <> [rewriteValue],
                checkedNextId =
                  nextId
              }
  where
    usedIds =
      Set.fromList (fmap checkedRewriteId (checkedRewriteOrder checkedSystem))

checkedRuleNames :: CheckedSystem capability f -> [RuleName]
checkedRuleNames =
  fmap checkedRewriteName . checkedRewriteOrder

checkedRewrites :: CheckedSystem capability f -> [CheckedRewrite capability f]
checkedRewrites =
  checkedRewriteOrder

lookupCheckedRewrite :: RuleName -> CheckedSystem capability f -> Maybe (CheckedRewrite capability f)
lookupCheckedRewrite name checkedSystem =
  Map.lookup name (checkedRewriteMap checkedSystem)

allocateSystemRuleId :: CheckedSystem capability f -> (RewriteRuleId, Int)
allocateSystemRuleId checkedSystem =
  firstFreeRuleId usedIds (checkedNextId checkedSystem)
  where
    usedIds =
      Set.fromList (fmap checkedRewriteId (checkedRewrites checkedSystem))

validateCheckedRewriteNames ::
  [CheckedRewrite capability f] ->
  Either CheckedSystemError ()
validateCheckedRewriteNames rewriteValues =
  case firstDuplicate (fmap checkedRewriteName rewriteValues) of
    Just duplicateName ->
      Left (CheckedSystemDuplicateRuleName duplicateName)
    Nothing ->
      Right ()

validateCheckedRewriteIds ::
  [CheckedRewrite capability f] ->
  Either CheckedSystemError ()
validateCheckedRewriteIds rewriteValues =
  case firstDuplicate (fmap checkedRewriteId rewriteValues) of
    Just duplicateId ->
      Left (CheckedSystemDuplicateRuleId duplicateId)
    Nothing ->
      Right ()

firstFreeRuleId :: Set.Set RewriteRuleId -> Int -> (RewriteRuleId, Int)
firstFreeRuleId usedIds candidate =
  let freeCandidate =
        until
          (\candidateKey -> Set.notMember (RewriteRuleId candidateKey) usedIds)
          (+ 1)
          candidate
      candidateId =
        RewriteRuleId freeCandidate
   in (candidateId, freeCandidate + 1)
