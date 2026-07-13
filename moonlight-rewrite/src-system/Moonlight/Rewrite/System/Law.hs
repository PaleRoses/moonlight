{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Law metadata for rewrite rules.
-- Owns stable law ids, trust and fidelity tiers, oracle requirements, and a
-- monoidal law book over arbitrary rule payloads.
-- Contract: oracle satisfaction is pure set containment; this surface records
-- metadata and does not itself check proofs.
module Moonlight.Rewrite.System.Law
  ( LawId,
    mkLawId,
    lawIdKey,
    TrustTier (..),
    SemanticFidelity (..),
    OracleKey,
    mkOracleKey,
    oracleKeyString,
    OracleRequirement (..),
    oracleRequirementKeys,
    oracleRequirementSatisfiedBy,
    LawSpec (..),
    LawBook (..),
    singletonLawBook,
  )
where

import Data.Kind (Type)
import Data.Set (Set)
import Data.Set qualified as Set

type LawId :: Type
newtype LawId = LawId Int
  deriving stock (Eq, Ord, Show)

mkLawId :: Int -> LawId
mkLawId =
  LawId

lawIdKey :: LawId -> Int
lawIdKey (LawId keyValue) =
  keyValue

type TrustTier :: Type
data TrustTier
  = ParserVerified
  | GhcVerified
  | RegistryTrusted
  | MachineProved
  | ModuleDerived
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type SemanticFidelity :: Type
data SemanticFidelity
  = Observational
  | UpToBottom
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type OracleKey :: Type
newtype OracleKey = OracleKey String
  deriving stock (Eq, Ord, Show)

mkOracleKey :: String -> OracleKey
mkOracleKey =
  OracleKey

oracleKeyString :: OracleKey -> String
oracleKeyString (OracleKey keyValue) =
  keyValue

type OracleRequirement :: Type
data OracleRequirement
  = NoOracleRequired
  | RequiresOracle !(Set OracleKey)
  deriving stock (Eq, Ord, Show)

oracleRequirementKeys :: OracleRequirement -> Set OracleKey
oracleRequirementKeys = \case
  NoOracleRequired ->
    Set.empty
  RequiresOracle oracleKeys ->
    oracleKeys

oracleRequirementSatisfiedBy :: Set OracleKey -> OracleRequirement -> Bool
oracleRequirementSatisfiedBy satisfiedKeys requirement =
  oracleRequirementKeys requirement `Set.isSubsetOf` satisfiedKeys

type LawSpec :: Type -> Type
data LawSpec rule = LawSpec
  { lawId :: !LawId,
    lawTier :: !TrustTier,
    lawFidelity :: !SemanticFidelity,
    lawOracle :: !OracleRequirement,
    lawRule :: !rule
  }
  deriving stock (Eq, Ord, Show, Functor)

type LawBook :: Type -> Type
newtype LawBook rule = LawBook
  { lawBookEntries :: [LawSpec rule]
  }
  deriving stock (Eq, Ord, Show, Functor)

instance Semigroup (LawBook rule) where
  LawBook leftEntries <> LawBook rightEntries =
    LawBook (leftEntries <> rightEntries)

instance Monoid (LawBook rule) where
  mempty =
    LawBook []

singletonLawBook :: LawSpec rule -> LawBook rule
singletonLawBook lawSpec =
  LawBook [lawSpec]
