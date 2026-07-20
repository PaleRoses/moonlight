{-# LANGUAGE GHC2024 #-}

module Moonlight.Rewrite.System.Checked.Internal
  ( CheckedRewrite,
    checkedRewriteId,
    checkedRewriteName,
    checkedRewriteAlgebra,
    checkedRewriteVariables,
    checkedRewriteFromAlgebra,
    CheckedSystem,
    CheckedSystemError (..),
    checkedSystemFromRewrites,
    insertDerivedRewriteInternal,
    checkedRewrites,
    lookupCheckedRewrite,
    assignRewriteRuleIds,
  )
where

import Control.Monad (foldM)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core
  ( Language,
    RewriteRuleId (..),
    firstDuplicate,
    rewriteRuleIdKey,
  )
import Moonlight.Rewrite.Algebra
  ( PatternRewrite,
    RewriteOrigin (..),
    allPatternRewriteVariables,
    prOrigin,
  )
import Moonlight.Rewrite.System.Logic.Decoration (LogicalDecoration)
import Moonlight.Rewrite.System.Origin
  ( RuleOrigin,
    ruleOriginId,
    ruleOriginName,
  )
import Moonlight.Rewrite.System.RuleName (RuleName)
import Moonlight.Rewrite.System.Variable
  ( RuleVariables,
    allRuleVariablesUntyped,
    untypedRuleVariables,
  )

type CheckedRewrite :: Type -> (Type -> Type) -> Type
data CheckedRewrite capability f
  = CheckedAtomicUntypedRewrite
      !RuleOrigin
      !(PatternRewrite RuleOrigin (LogicalDecoration capability) f)
  | CheckedAtomicTypedRewrite
      !RuleOrigin
      !(PatternRewrite RuleOrigin (LogicalDecoration capability) f)
      !RuleVariables
  | CheckedDerivedUntypedRewrite
      !RewriteRuleId
      !RuleName
      !(PatternRewrite RuleOrigin (LogicalDecoration capability) f)
  | CheckedDerivedTypedRewrite
      !RewriteRuleId
      !RuleName
      !(PatternRewrite RuleOrigin (LogicalDecoration capability) f)
      !RuleVariables

deriving stock instance
  Eq (PatternRewrite RuleOrigin (LogicalDecoration capability) f) =>
  Eq (CheckedRewrite capability f)

deriving stock instance
  Show (PatternRewrite RuleOrigin (LogicalDecoration capability) f) =>
  Show (CheckedRewrite capability f)

checkedRewriteFromAlgebra ::
  RewriteRuleId ->
  RuleName ->
  PatternRewrite RuleOrigin (LogicalDecoration capability) f ->
  RuleVariables ->
  CheckedRewrite capability f
checkedRewriteFromAlgebra rewriteRuleId ruleName algebra variables =
  case prOrigin algebra of
    RewriteAtomic origin
      | ruleOriginId origin == rewriteRuleId,
        ruleOriginName origin == ruleName ->
          if allRuleVariablesUntyped variables
            then CheckedAtomicUntypedRewrite origin algebra
            else CheckedAtomicTypedRewrite origin algebra variables

    _ ->
      if allRuleVariablesUntyped variables
        then CheckedDerivedUntypedRewrite rewriteRuleId ruleName algebra
        else CheckedDerivedTypedRewrite rewriteRuleId ruleName algebra variables

checkedRewriteId :: CheckedRewrite capability f -> RewriteRuleId
checkedRewriteId rewriteValue =
  case rewriteValue of
    CheckedAtomicUntypedRewrite origin _algebra ->
      ruleOriginId origin
    CheckedAtomicTypedRewrite origin _algebra _variables ->
      ruleOriginId origin
    CheckedDerivedUntypedRewrite rewriteRuleId _ruleName _algebra ->
      rewriteRuleId
    CheckedDerivedTypedRewrite rewriteRuleId _ruleName _algebra _variables ->
      rewriteRuleId

checkedRewriteName :: CheckedRewrite capability f -> RuleName
checkedRewriteName rewriteValue =
  case rewriteValue of
    CheckedAtomicUntypedRewrite origin _algebra ->
      ruleOriginName origin
    CheckedAtomicTypedRewrite origin _algebra _variables ->
      ruleOriginName origin
    CheckedDerivedUntypedRewrite _rewriteRuleId ruleName _algebra ->
      ruleName
    CheckedDerivedTypedRewrite _rewriteRuleId ruleName _algebra _variables ->
      ruleName

checkedRewriteAlgebra ::
  CheckedRewrite capability f ->
  PatternRewrite RuleOrigin (LogicalDecoration capability) f
checkedRewriteAlgebra rewriteValue =
  case rewriteValue of
    CheckedAtomicUntypedRewrite _origin algebra -> algebra
    CheckedAtomicTypedRewrite _origin algebra _variables -> algebra
    CheckedDerivedUntypedRewrite _rewriteRuleId _ruleName algebra -> algebra
    CheckedDerivedTypedRewrite _rewriteRuleId _ruleName algebra _variables -> algebra

checkedRewriteVariables ::
  (Language f, Ord capability) =>
  CheckedRewrite capability f ->
  RuleVariables
checkedRewriteVariables rewriteValue =
  case rewriteValue of
    CheckedAtomicUntypedRewrite _origin algebra ->
      untypedRuleVariables (allPatternRewriteVariables algebra)
    CheckedAtomicTypedRewrite _origin _algebra variables ->
      variables
    CheckedDerivedUntypedRewrite _rewriteRuleId _ruleName algebra ->
      untypedRuleVariables (allPatternRewriteVariables algebra)
    CheckedDerivedTypedRewrite _rewriteRuleId _ruleName _algebra variables ->
      variables


type CheckedSystem :: Type -> (Type -> Type) -> Type
data CheckedSystem capability f
  = CheckedSystemWithAvailableId
      !(Map RuleName (CheckedRewrite capability f))
      ![CheckedRewrite capability f]
      {-# UNPACK #-} !Int
  | CheckedSystemWithExhaustedIds
      !(Map RuleName (CheckedRewrite capability f))
      ![CheckedRewrite capability f]

checkedSystemRuleMap :: CheckedSystem capability f -> Map RuleName (CheckedRewrite capability f)
checkedSystemRuleMap checkedSystem =
  case checkedSystem of
    CheckedSystemWithAvailableId ruleMap _rewriteOrder _nextId -> ruleMap
    CheckedSystemWithExhaustedIds ruleMap _rewriteOrder -> ruleMap

lookupCheckedRewrite :: RuleName -> CheckedSystem capability f -> Maybe (CheckedRewrite capability f)
lookupCheckedRewrite name =
  Map.lookup name . checkedSystemRuleMap

checkedRewrites :: CheckedSystem capability f -> [CheckedRewrite capability f]
checkedRewrites checkedSystem =
  case checkedSystem of
    CheckedSystemWithAvailableId _ruleMap rewriteOrder _nextId -> rewriteOrder
    CheckedSystemWithExhaustedIds _ruleMap rewriteOrder -> rewriteOrder

checkedSystemNextId :: CheckedSystem capability f -> Maybe Int
checkedSystemNextId checkedSystem =
  case checkedSystem of
    CheckedSystemWithAvailableId _ruleMap _rewriteOrder nextId -> Just nextId
    CheckedSystemWithExhaustedIds {} -> Nothing

checkedSystemFromParts ::
  Map RuleName (CheckedRewrite capability f) ->
  [CheckedRewrite capability f] ->
  Maybe Int ->
  CheckedSystem capability f
checkedSystemFromParts ruleMap rewriteOrder nextId =
  case nextId of
    Just availableId ->
      CheckedSystemWithAvailableId ruleMap rewriteOrder availableId

    Nothing ->
      CheckedSystemWithExhaustedIds ruleMap rewriteOrder

type CheckedSystemError :: Type
data CheckedSystemError
  = CheckedSystemDuplicateRuleName !RuleName
  | CheckedSystemDuplicateRuleId !RewriteRuleId
  | CheckedSystemInvalidRuleId !RewriteRuleId
  | CheckedSystemRuleIdExhausted
  deriving stock (Eq, Ord, Show)

checkedSystemFromRewrites ::
  [CheckedRewrite capability f] ->
  Either CheckedSystemError (CheckedSystem capability f)
checkedSystemFromRewrites rewriteValues = do
  validateCheckedRewriteNames rewriteValues
  validateCheckedRewriteIds rewriteValues
  validateRuleIds (fmap checkedRewriteId rewriteValues)
  nextCandidate <-
    deriveNextCandidate (usedRewriteIds rewriteValues)
  Right
    ( checkedSystemFromParts
        ( Map.fromList
            [ (checkedRewriteName rewriteValue, rewriteValue)
              | rewriteValue <- rewriteValues
            ]
        )
        rewriteValues
        nextCandidate
    )

insertDerivedRewriteInternal ::
  RuleName ->
  PatternRewrite RuleOrigin (LogicalDecoration capability) f ->
  RuleVariables ->
  CheckedSystem capability f ->
  Either CheckedSystemError (CheckedSystem capability f)
insertDerivedRewriteInternal name algebra variables checkedSystem =
  case Map.lookup name (checkedSystemRuleMap checkedSystem) of
    Just _ ->
      Left (CheckedSystemDuplicateRuleName name)

    Nothing -> do
      let usedIds =
            usedRewriteIds (checkedRewrites checkedSystem)
      (rewriteRuleId, nextCandidate) <-
        allocateRuleId usedIds (checkedSystemNextId checkedSystem)
      let rewriteValue =
            checkedRewriteFromAlgebra rewriteRuleId name algebra variables
      Right
        ( checkedSystemFromParts
            (Map.insert name rewriteValue (checkedSystemRuleMap checkedSystem))
            (checkedRewrites checkedSystem <> [rewriteValue])
            nextCandidate
        )

assignRewriteRuleIds ::
  [Maybe RewriteRuleId] ->
  Either CheckedSystemError [RewriteRuleId]
assignRewriteRuleIds requestedIds = do
  let explicitIds =
        catMaybes requestedIds
  validateRuleIds explicitIds
  case firstDuplicate explicitIds of
    Just duplicateId ->
      Left (CheckedSystemDuplicateRuleId duplicateId)

    Nothing ->
      fmap (reverse . snd)
        ( foldM
            assignRequestedId
            ((Set.fromList explicitIds, Just 0), [])
            requestedIds
        )
  where
    assignRequestedId ((usedIds, nextCandidate), assignedIds) requestedId =
      case requestedId of
        Just explicitId ->
          Right ((usedIds, nextCandidate), explicitId : assignedIds)

        Nothing -> do
          (allocatedId, nextCandidate') <-
            allocateRuleId usedIds nextCandidate
          Right
            ( (Set.insert allocatedId usedIds, nextCandidate'),
              allocatedId : assignedIds
            )

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

validateRuleIds :: [RewriteRuleId] -> Either CheckedSystemError ()
validateRuleIds rewriteRuleIds =
  case filter ((< 0) . rewriteRuleIdKey) rewriteRuleIds of
    invalidId : _ ->
      Left (CheckedSystemInvalidRuleId invalidId)

    [] ->
      Right ()

usedRewriteIds :: [CheckedRewrite capability f] -> Set RewriteRuleId
usedRewriteIds =
  Set.fromList . fmap checkedRewriteId

deriveNextCandidate :: Set RewriteRuleId -> Either CheckedSystemError (Maybe Int)
deriveNextCandidate usedIds =
  fmap Just (findFreeCandidate usedIds 0)

allocateRuleId ::
  Set RewriteRuleId ->
  Maybe Int ->
  Either CheckedSystemError (RewriteRuleId, Maybe Int)
allocateRuleId _usedIds Nothing =
  Left CheckedSystemRuleIdExhausted
allocateRuleId usedIds (Just candidate) = do
  freeCandidate <-
    findFreeCandidate usedIds candidate
  let nextCandidate =
        if freeCandidate == maxBound
          then Nothing
          else Just (freeCandidate + 1)
  Right (RewriteRuleId freeCandidate, nextCandidate)

findFreeCandidate ::
  Set RewriteRuleId ->
  Int ->
  Either CheckedSystemError Int
findFreeCandidate usedIds initialCandidate =
  foldM advanceCandidate initialCandidate (Set.toAscList usedIds)
  where
    advanceCandidate currentCandidate rewriteRuleId
      | rewriteRuleIdKey rewriteRuleId < currentCandidate =
          Right currentCandidate
      | rewriteRuleIdKey rewriteRuleId > currentCandidate =
          Right currentCandidate
      | currentCandidate == maxBound =
          Left CheckedSystemRuleIdExhausted
      | otherwise =
          Right (currentCandidate + 1)
