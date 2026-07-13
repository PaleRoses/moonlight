{-# LANGUAGE DerivingStrategies #-}

-- | Refined identifier for system rule names.
-- Owns trimming, dot-path identifier validation, and rendering.
-- Contract: empty and invalid names become typed 'RuleNameError' values
-- before they can become lookup failures.
module Moonlight.Rewrite.System.RuleName
  ( RuleName,
    RuleNameError (..),
    mkRuleName,
    ruleNameString,
  )
where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import Moonlight.Core
  ( IdentifierToken,
    isValidIdentifier,
    mkIdentifierTokenWith,
    renderIdentifierToken,
  )

type RuleNameNamespace :: Type
data RuleNameNamespace

type RuleName :: Type
newtype RuleName = RuleName
  { unRuleName :: IdentifierToken RuleNameNamespace
  }
  deriving stock (Eq, Ord, Show)

type RuleNameError :: Type
data RuleNameError
  = EmptyRuleName
  | InvalidRuleName
  deriving stock (Eq, Ord, Show, Read)

mkRuleName :: String -> Either RuleNameError RuleName
mkRuleName raw =
  case Text.strip (Text.pack raw) of
    normalized
      | Text.null normalized ->
        Left EmptyRuleName

    normalized ->
      maybe
        (Left InvalidRuleName)
        (Right . RuleName)
        (mkIdentifierTokenWith isValidRuleNamePath normalized)

isValidRuleNamePath :: Text -> Bool
isValidRuleNamePath =
  all isValidIdentifier . Text.splitOn (Text.pack ".")

ruleNameString :: RuleName -> String
ruleNameString =
  Text.unpack . renderIdentifierToken . unRuleName
