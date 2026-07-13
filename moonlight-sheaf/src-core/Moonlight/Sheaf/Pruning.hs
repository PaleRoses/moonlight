{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Pruning
  ( PruningDecision (..),
    PruningGate (..),
    PruningReport (..),
    PruningCertificate (..),
    rejectedPruningDecision,
    pruningDecisionAllowed,
    pruningDecisionFootprint,
    pruningDecisionRejectedList,
    pruningDecisionFromVerdict,
    purePruningGate,
    pruneWithGate,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.Sheaf.Verdict
  ( ObstructionVerdict,
    Verdict (..),
  )

type PruningDecision :: Type -> Type -> Type -> Type
data PruningDecision footprint diagnostic obstruction
  = PruningAccepted !footprint
  | PruningRejected !(PruningCertificate footprint diagnostic obstruction)
  deriving stock (Eq, Show, Read)

type PruningGate :: (Type -> Type) -> Type -> Type -> Type -> Type -> Type
newtype PruningGate m candidate footprint diagnostic obstruction = PruningGate
  { runPruningGate :: candidate -> m (PruningDecision footprint diagnostic obstruction)
  }

type PruningReport :: Type -> Type -> Type -> Type -> Type
data PruningReport candidate footprint diagnostic obstruction = PruningReport
  { prLive :: ![(candidate, footprint)],
    prPruned :: ![(candidate, PruningCertificate footprint diagnostic obstruction)]
  }
  deriving stock (Eq, Show, Read)

instance Semigroup (PruningReport candidate footprint diagnostic obstruction) where
  left <> right =
    PruningReport
      { prLive = prLive left <> prLive right,
        prPruned = prPruned left <> prPruned right
      }

instance Monoid (PruningReport candidate footprint diagnostic obstruction) where
  mempty =
    emptyPruningReport

type PruningCertificate :: Type -> Type -> Type -> Type
data PruningCertificate footprint diagnostic obstruction = PruningCertificate
  { pcObstructions :: !(NonEmpty obstruction),
    pcFootprint :: !footprint,
    pcDiagnostic :: !(Maybe diagnostic)
  }
  deriving stock (Eq, Show, Read)

emptyPruningReport :: PruningReport candidate footprint diagnostic obstruction
emptyPruningReport =
  PruningReport
    { prLive = [],
      prPruned = []
    }

rejectedPruningDecision ::
  footprint ->
  Maybe diagnostic ->
  NonEmpty obstruction ->
  PruningDecision footprint diagnostic obstruction
rejectedPruningDecision footprint diagnostic obstructions =
  PruningRejected
    PruningCertificate
      { pcObstructions = obstructions,
        pcFootprint = footprint,
        pcDiagnostic = diagnostic
      }

pruningDecisionAllowed :: PruningDecision footprint diagnostic obstruction -> Bool
pruningDecisionAllowed decision =
  case decision of
    PruningAccepted _ ->
      True
    PruningRejected _ ->
      False

pruningDecisionFootprint :: PruningDecision footprint diagnostic obstruction -> footprint
pruningDecisionFootprint decision =
  case decision of
    PruningAccepted footprint ->
      footprint
    PruningRejected certificate ->
      pcFootprint certificate

pruningDecisionRejectedList :: PruningDecision footprint diagnostic obstruction -> [obstruction]
pruningDecisionRejectedList decision =
  case decision of
    PruningAccepted _ ->
      []
    PruningRejected certificate ->
      NonEmpty.toList (pcObstructions certificate)

pruningDecisionFromVerdict ::
  footprint ->
  Maybe diagnostic ->
  ObstructionVerdict obstruction ->
  PruningDecision footprint diagnostic obstruction
pruningDecisionFromVerdict footprint diagnostic verdict =
  case verdict of
    Accepted () ->
      PruningAccepted footprint
    Rejected obstructions ->
      rejectedPruningDecision footprint diagnostic obstructions

purePruningGate ::
  (candidate -> PruningDecision footprint diagnostic obstruction) ->
  PruningGate Identity candidate footprint diagnostic obstruction
purePruningGate runGate =
  PruningGate (Identity . runGate)

pruneWithGate ::
  Monad m =>
  PruningGate m candidate footprint diagnostic obstruction ->
  [candidate] ->
  m (PruningReport candidate footprint diagnostic obstruction)
pruneWithGate gate candidates =
  foldMap reportFromDecision
    <$> traverse candidateDecision candidates
  where
    candidateDecision candidate = do
      decision <- runPruningGate gate candidate
      pure (candidate, decision)

    reportFromDecision ::
      (candidate, PruningDecision footprint diagnostic obstruction) ->
      PruningReport candidate footprint diagnostic obstruction
    reportFromDecision (candidate, decision) =
      case decision of
        PruningAccepted footprint ->
          PruningReport [(candidate, footprint)] []
        PruningRejected certificate ->
          PruningReport [] [(candidate, certificate)]
