-- | PBPO execution stratum over adhesive categorical structure.
-- It owns pullback, complement, mediator, and pushout failures plus the resulting
-- step witnesses; rule shape is assumed certified by "Moonlight.Rewrite.Algebra.PBPO.Rule".
module Moonlight.Rewrite.Algebra.PBPO.Step
  ( PBPOStepFailure (..),
    PBPOStep (..),
    PBPOUntypedStep (..),
    applyPBPOPlus,
    applyPBPO,
  )
where

import Data.Kind (Type)
import Data.Bifunctor (first)
import Moonlight.Category
  ( Category (..),
    HasPullbacks (..),
    HasPushouts (..),
    MonicMatchWitness,
    PBPOAdhesiveCategory,
    PBPOComplementWitness,
    composeMor,
    monicMatchArrow,
    pbpoComplement,
    pbpoComplementResidualLeg,
  )
import Moonlight.Core (note)
import Moonlight.Rewrite.Algebra.PBPO.Rule
  ( PBPOMatch (..),
    PBPORule,
    pbpoRuleContextLeg,
    pbpoRuleInterfaceTyping,
    pbpoRuleLeftLeg,
    pbpoRuleLeftTyping,
    pbpoRuleMeta,
    pbpoRuleRightLeg,
  )

type PBPOStepFailure :: Type -> Type -> Type
data PBPOStepFailure c meta
  = PBPOAdherenceCompositionFailed !meta !(Mor c) !(Mor c) !(CategoryError c)
  | PBPOConeCompositionFailed !meta !(Mor c) !(Mor c) !(CategoryError c)
  | PBPOAdherenceMismatch !meta !(Mor c) !(Mor c)
  | PBPONoPullback !meta !(Mor c) !(Mor c)
  | PBPONoMediator !meta !(Mor c) !(Mor c)
  | PBPONoPushout !meta !(Mor c) !(Mor c)
  | PBPONoComplement !meta !(Mor c)

deriving stock instance
  (Eq meta, Eq (Mor c), Eq (CategoryError c)) =>
  Eq (PBPOStepFailure c meta)

deriving stock instance
  (Show meta, Show (Mor c), Show (CategoryError c)) =>
  Show (PBPOStepFailure c meta)

type PBPOStep :: Type -> Type -> Type
data PBPOStep c meta = PBPOStep
  { pbpoStepHost :: !(Ob c),
    pbpoStepContext :: !(Ob c),
    pbpoStepContextToPrior :: !(Mor c),
    pbpoStepContextTyping :: !(Mor c),
    pbpoStepMediator :: !(Mor c),
    pbpoStepContextToHost :: !(Mor c),
    pbpoStepReplacementToHost :: !(Mor c),
    pbpoStepRule :: !(PBPORule c meta)
  }

type PBPOUntypedStep :: Type -> Type -> Type
data PBPOUntypedStep c meta = PBPOUntypedStep
  { pbpoUntypedHost :: !(Ob c),
    pbpoUntypedComplement :: !(PBPOComplementWitness c),
    pbpoUntypedContextToHost :: !(Mor c),
    pbpoUntypedReplacementToHost :: !(Mor c),
    pbpoUntypedRule :: !(PBPORule c meta)
  }

applyPBPOPlus ::
  (PBPOAdhesiveCategory c, Eq (Mor c)) =>
  c ->
  PBPORule c meta ->
  PBPOMatch c ->
  Either (PBPOStepFailure c meta) (PBPOStep c meta)
applyPBPOPlus categoryValue rule match = do
  let matchArrow =
        monicMatchArrow (pbpoMatchMonic match)

      adherence =
        pbpoMatchAdherence match

      metaValue =
        pbpoRuleMeta rule

  adherenceTyping <-
    first
      (PBPOAdherenceCompositionFailed metaValue adherence matchArrow)
      (composeMor categoryValue adherence matchArrow)

  if adherenceTyping /= pbpoRuleLeftTyping rule
    then Left (PBPOAdherenceMismatch metaValue adherenceTyping (pbpoRuleLeftTyping rule))
    else do
      (contextObject, contextToPrior, contextTyping) <-
        note
          (PBPONoPullback metaValue adherence (pbpoRuleContextLeg rule))
          (pullback categoryValue adherence (pbpoRuleContextLeg rule))

      interfaceToPrior <-
        first
          (PBPOConeCompositionFailed metaValue matchArrow (pbpoRuleLeftLeg rule))
          (composeMor categoryValue matchArrow (pbpoRuleLeftLeg rule))

      mediator <-
        note
          (PBPONoMediator metaValue interfaceToPrior (pbpoRuleInterfaceTyping rule))
          ( pullbackMediator
              categoryValue
              adherence
              (pbpoRuleContextLeg rule)
              interfaceToPrior
              (pbpoRuleInterfaceTyping rule)
          )

      (hostObject, contextToHost, replacementToHost) <-
        note
          (PBPONoPushout metaValue mediator (pbpoRuleRightLeg rule))
          (pushout categoryValue mediator (pbpoRuleRightLeg rule))

      pure
        PBPOStep
          { pbpoStepHost = hostObject,
            pbpoStepContext = contextObject,
            pbpoStepContextToPrior = contextToPrior,
            pbpoStepContextTyping = contextTyping,
            pbpoStepMediator = mediator,
            pbpoStepContextToHost = contextToHost,
            pbpoStepReplacementToHost = replacementToHost,
            pbpoStepRule = rule
          }

applyPBPO ::
  PBPOAdhesiveCategory c =>
  c ->
  PBPORule c meta ->
  MonicMatchWitness c ->
  Either (PBPOStepFailure c meta) (PBPOUntypedStep c meta)
applyPBPO categoryValue rule monicMatch = do
  let metaValue =
        pbpoRuleMeta rule

  complementWitness <-
    note
      (PBPONoComplement metaValue (pbpoRuleLeftLeg rule))
      (pbpoComplement categoryValue (pbpoRuleLeftLeg rule) monicMatch)

  (hostObject, contextToHost, replacementToHost) <-
    note
      ( PBPONoPushout
          metaValue
          (pbpoComplementResidualLeg complementWitness)
          (pbpoRuleRightLeg rule)
      )
      (pushout categoryValue (pbpoComplementResidualLeg complementWitness) (pbpoRuleRightLeg rule))

  pure
    PBPOUntypedStep
      { pbpoUntypedHost = hostObject,
        pbpoUntypedComplement = complementWitness,
        pbpoUntypedContextToHost = contextToHost,
        pbpoUntypedReplacementToHost = replacementToHost,
        pbpoUntypedRule = rule
      }
