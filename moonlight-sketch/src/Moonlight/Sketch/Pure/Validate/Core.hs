module Moonlight.Sketch.Pure.Validate.Core
  ( ValidationContext (..),
    IssueRule (..),
    ValidationRule (..),
    liftIssueRule,
    Validator,
    RefValidatorLookup,
    ValidatorRuntime (..),
    lookupRuntimeValidator,
  )
where

import Data.Aeson (Value)
import Data.Kind (Type)
import qualified Data.Set as Set
import Moonlight.Sketch.Pure.Env (SchemaEnv)
import Moonlight.Sketch.Pure.Types (PathSegment, RefId, SchemaIssue, SchemaNode)

type ValidationContext :: Type
data ValidationContext = ValidationContext
  { vcEnv :: SchemaEnv,
    vcVisited :: Set.Set RefId,
    vcPath :: [PathSegment]
  }

type IssueRule :: Type
newtype IssueRule = IssueRule
  { runIssueRule :: [SchemaIssue]
  }
  deriving newtype (Semigroup, Monoid)

type ValidationRule :: Type
newtype ValidationRule = ValidationRule
  { applyValidationRule :: ValidationContext -> Value -> [SchemaIssue]
  }
  deriving newtype (Semigroup, Monoid)

liftIssueRule :: IssueRule -> ValidationRule
liftIssueRule issueRule =
  ValidationRule
    (\_ _ -> runIssueRule issueRule)

type Validator :: Type
type Validator = ValidationRule

type RefValidatorLookup :: Type
type RefValidatorLookup = RefId -> Maybe Validator

type ValidatorRuntime :: Type
data ValidatorRuntime = ValidatorRuntime
  { vrSchemaLookup :: SchemaNode -> Validator
  }

lookupRuntimeValidator :: ValidatorRuntime -> SchemaNode -> Validator
lookupRuntimeValidator validatorRuntime = vrSchemaLookup validatorRuntime
