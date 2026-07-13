module Moonlight.Constraint.Pure.WFC.Types
  ( SlotId (..),
    WFCTopology (..),
    DomainPolicy (..),
    AdjacencyPolicy (..),
    PresencePolicy (..),
    WFCRule (..),
    WFCPolicyProblem (..),
    CompiledPolicySlot (..),
    CompiledPolicyValue (..),
    AdjacencyRule (..),
    WFCProblem (..),
    BacktrackLimit (..),
    WFCOptions (..),
    defaultWFCOptions,
    WFCSearchResult (..),
    WFCError (..),
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Word (Word32)
import Moonlight.Constraint.Pure.CSP (CSPError, Domain)

type SlotId :: Type -> Type
newtype SlotId slot = SlotId
  { unSlotId :: slot
  }
  deriving stock (Eq, Ord, Show, Read)

type WFCTopology :: Type -> Type
newtype WFCTopology slot = WFCTopology
  { wfcTopologyAdjacency :: Map (SlotId slot) [SlotId slot]
  }

type DomainPolicy :: Type -> Type -> Type
newtype DomainPolicy slot value = DomainPolicy
  { applyDomainPolicy :: SlotId slot -> value -> Bool
  }

type AdjacencyPolicy :: Type -> Type -> Type
newtype AdjacencyPolicy slot value = AdjacencyPolicy
  { applyAdjacencyPolicy :: SlotId slot -> SlotId slot -> value -> value -> Bool
  }

type PresencePolicy :: Type -> Type -> Type
data PresencePolicy slot value = PresencePolicy
  { presencePolicyRegion :: [SlotId slot],
    presencePolicyRequired :: value -> Bool
  }

type WFCRule :: Type -> Type -> Type
data WFCRule slot value
  = DomainPolicyRule (DomainPolicy slot value)
  | AdjacencyPolicyRule (AdjacencyPolicy slot value)
  | PresencePolicyRule (PresencePolicy slot value)
  | ExplicitAdjacencyRule (AdjacencyRule slot value)

type WFCPolicyProblem :: Type -> Type -> Type
data WFCPolicyProblem slot value = WFCPolicyProblem
  { wfcPolicyDomains :: Map (SlotId slot) (Domain value),
    wfcPolicyTopology :: WFCTopology slot,
    wfcPolicyRules :: [WFCRule slot value]
  }

type CompiledPolicySlot :: Type -> Type
data CompiledPolicySlot slot
  = CompiledBaseSlot slot
  | CompiledPresenceWitnessSlot Int
  deriving stock (Eq, Ord, Show, Read)

type CompiledPolicyValue :: Type -> Type -> Type
data CompiledPolicyValue slot value
  = CompiledBaseValue value
  | CompiledPresenceWitnessValue (Map (SlotId slot) value)
  deriving stock (Eq, Ord, Show, Read)

type AdjacencyRule :: Type -> Type -> Type
data AdjacencyRule slot value = AdjacencyRule
  { adjacencyRuleSource :: SlotId slot,
    adjacencyRuleTarget :: SlotId slot,
    adjacencyRuleCompatible :: value -> value -> Bool
  }

type WFCProblem :: Type -> Type -> Type
data WFCProblem slot value = WFCProblem
  { wfcProblemDomains :: Map (SlotId slot) (Domain value),
    wfcProblemAdjacencyRules :: [AdjacencyRule slot value]
  }

type BacktrackLimit :: Type
newtype BacktrackLimit = BacktrackLimit
  { unBacktrackLimit :: Word32
  }
  deriving stock (Eq, Ord, Show, Read)

type WFCOptions :: Type
data WFCOptions = WFCOptions
  { wfcBacktrackLimit :: BacktrackLimit
  }
  deriving stock (Eq, Ord, Show, Read)

defaultWFCOptions :: WFCOptions
defaultWFCOptions =
  WFCOptions
    { wfcBacktrackLimit = BacktrackLimit 256
    }

type WFCSearchResult :: Type -> Type -> Type
data WFCSearchResult slot value
  = WFCSolved (Map (SlotId slot) value)
  | WFCUnsatisfiable
  | WFCBacktrackLimitReached
  deriving stock (Eq, Show)

type WFCError :: Type -> Type
data WFCError slot
  = WFCCSPError (CSPError (SlotId slot))
  | WFCProjectionInvariantViolation
  deriving stock (Eq, Ord, Show, Read)
