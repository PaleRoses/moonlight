{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE LambdaCase #-}

-- | Typed metadata indexed by the pattern variables of a checked rewrite.
-- The metadata follows the same renaming and projection evidence as rewrite
-- composition; incompatible local sort assignments are a typed obstruction.
module Moonlight.Rewrite.System.Variable
  ( SortName,
    sortNameFromString,
    sortNameString,
    RuleVariable,
    untypedRuleVariable,
    typedRuleVariable,
    ruleVariableName,
    ruleVariableSort,
    RuleVariables,
    emptyRuleVariables,
    untypedRuleVariables,
    typedRuleVariables,
    ruleVariableMap,
    ruleVariableKeys,
    allRuleVariablesUntyped,
    renameRuleVariables,
    projectRuleVariables,
    mergeRuleVariables,
    restrictRuleVariables,
    RuleVariableMetadataError (..),
  )
where

import Control.Monad (foldM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core
  ( Pattern (..),
    PatternVar,
  )
import Moonlight.Rewrite.Algebra
  ( PatternProjection,
    PatternRenaming,
    applyPatternRenamingVar,
    projectPattern,
  )

newtype SortName = SortName
  { sortNameString :: String
  }
  deriving stock (Eq, Ord, Show, Read)

sortNameFromString :: String -> SortName
sortNameFromString =
  SortName

data RuleVariable
  = UntypedRuleVariable
  | TypedRuleVariable !String !SortName
  deriving stock (Eq, Ord, Show)

untypedRuleVariable :: RuleVariable
untypedRuleVariable =
  UntypedRuleVariable

typedRuleVariable :: String -> SortName -> RuleVariable
typedRuleVariable =
  TypedRuleVariable

ruleVariableName :: RuleVariable -> Maybe String
ruleVariableName =
  \case
    UntypedRuleVariable ->
      Nothing

    TypedRuleVariable name _sortName ->
      Just name

ruleVariableSort :: RuleVariable -> Maybe SortName
ruleVariableSort =
  \case
    UntypedRuleVariable ->
      Nothing

    TypedRuleVariable _name sortNameValue ->
      Just sortNameValue

data RuleVariables
  = EmptyRuleVariables
  | SingletonRuleVariable !PatternVar !RuleVariable
  | MultipleRuleVariables !(Map PatternVar RuleVariable)
  deriving stock (Eq, Show)

emptyRuleVariables :: RuleVariables
emptyRuleVariables =
  EmptyRuleVariables

untypedRuleVariables :: Set PatternVar -> RuleVariables
untypedRuleVariables patternVariables =
  case Set.lookupMin patternVariables of
    Nothing ->
      EmptyRuleVariables

    Just patternVariable
      | Set.size patternVariables == 1 ->
          SingletonRuleVariable patternVariable untypedRuleVariable

    Just _ ->
      MultipleRuleVariables (Map.fromSet (const untypedRuleVariable) patternVariables)

typedRuleVariables :: Map PatternVar (String, SortName) -> RuleVariables
typedRuleVariables =
  ruleVariablesFromMap . fmap (uncurry typedRuleVariable)

ruleVariableMap :: RuleVariables -> Map PatternVar RuleVariable
ruleVariableMap variables =
  case variables of
    EmptyRuleVariables ->
      Map.empty

    SingletonRuleVariable patternVariable metadata ->
      Map.singleton patternVariable metadata

    MultipleRuleVariables variableMap ->
      variableMap

ruleVariableKeys :: RuleVariables -> Set PatternVar
ruleVariableKeys variables =
  case variables of
    EmptyRuleVariables ->
      Set.empty

    SingletonRuleVariable patternVariable _metadata ->
      Set.singleton patternVariable

    MultipleRuleVariables variableMap ->
      Map.keysSet variableMap

allRuleVariablesUntyped :: RuleVariables -> Bool
allRuleVariablesUntyped =
  all ((== Nothing) . ruleVariableSort . snd) . ruleVariableEntries

data RuleVariableMetadataError
  = RuleVariableSortConflict !PatternVar !SortName !SortName
  | RuleVariableMetadataMissing !(Set PatternVar)
  deriving stock (Eq, Ord, Show)

renameRuleVariables ::
  PatternRenaming ->
  RuleVariables ->
  Either RuleVariableMetadataError RuleVariables
renameRuleVariables renaming =
  foldRuleVariables
    (\patternVariable -> Just (applyPatternRenamingVar renaming patternVariable))

projectRuleVariables ::
  Functor f =>
  PatternProjection f ->
  RuleVariables ->
  Either RuleVariableMetadataError RuleVariables
projectRuleVariables projection =
  foldRuleVariables
    ( \patternVariable ->
        case projectPattern projection (PatternVar patternVariable) of
          PatternVar projectedVariable ->
            Just projectedVariable

          PatternNode _ ->
            Nothing
    )

mergeRuleVariables ::
  RuleVariables ->
  RuleVariables ->
  Either RuleVariableMetadataError RuleVariables
mergeRuleVariables leftVariables rightVariables =
  ruleVariablesFromMap
    <$> foldM
      (\variables (patternVariable, metadata) -> insertRuleVariable patternVariable metadata variables)
      (ruleVariableMap leftVariables)
      (ruleVariableEntries rightVariables)

restrictRuleVariables ::
  Set PatternVar ->
  RuleVariables ->
  Either RuleVariableMetadataError RuleVariables
restrictRuleVariables retainedVariables variables =
  let missingVariables =
        Set.difference retainedVariables (ruleVariableKeys variables)
    in if Set.null missingVariables
        then Right (ruleVariablesFromMap (Map.restrictKeys (ruleVariableMap variables) retainedVariables))
        else Left (RuleVariableMetadataMissing missingVariables)

foldRuleVariables ::
  (PatternVar -> Maybe PatternVar) ->
  RuleVariables ->
  Either RuleVariableMetadataError RuleVariables
foldRuleVariables transport variables =
  ruleVariablesFromMap
    <$> foldM
      transportEntry
      Map.empty
      (ruleVariableEntries variables)
  where
    transportEntry accumulator (sourceVariable, metadata) =
      case transport sourceVariable of
        Nothing ->
          Right accumulator

        Just targetVariable ->
          insertRuleVariable targetVariable metadata accumulator

ruleVariablesFromMap :: Map PatternVar RuleVariable -> RuleVariables
ruleVariablesFromMap variableMap =
  case Map.lookupMin variableMap of
    Nothing ->
      EmptyRuleVariables

    Just (patternVariable, metadata)
      | Map.size variableMap == 1 ->
          SingletonRuleVariable patternVariable metadata

    Just _ ->
      MultipleRuleVariables variableMap

ruleVariableEntries :: RuleVariables -> [(PatternVar, RuleVariable)]
ruleVariableEntries variables =
  case variables of
    EmptyRuleVariables ->
      []

    SingletonRuleVariable patternVariable metadata ->
      [(patternVariable, metadata)]

    MultipleRuleVariables variableMap ->
      Map.toAscList variableMap

insertRuleVariable ::
  PatternVar ->
  RuleVariable ->
  Map PatternVar RuleVariable ->
  Either RuleVariableMetadataError (Map PatternVar RuleVariable)
insertRuleVariable patternVariable metadata variables =
  case Map.lookup patternVariable variables of
    Nothing ->
      Right (Map.insert patternVariable metadata variables)

    Just existingMetadata ->
      fmap
        (\mergedMetadata -> Map.insert patternVariable mergedMetadata variables)
        (mergeRuleVariable patternVariable existingMetadata metadata)

mergeRuleVariable ::
  PatternVar ->
  RuleVariable ->
  RuleVariable ->
  Either RuleVariableMetadataError RuleVariable
mergeRuleVariable patternVariable leftVariable rightVariable =
  case (leftVariable, rightVariable) of
    (UntypedRuleVariable, _) ->
      Right rightVariable

    (_, UntypedRuleVariable) ->
      Right leftVariable

    (TypedRuleVariable leftName leftSort, TypedRuleVariable _rightName rightSort)
      | leftSort == rightSort ->
          Right (TypedRuleVariable leftName leftSort)
      | otherwise ->
          Left (RuleVariableSortConflict patternVariable leftSort rightSort)
