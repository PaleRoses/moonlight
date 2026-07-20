-- | Derived PBPO conveniences for identity-shaped and unit-category admissions.
-- It owns helpers that pass through the real rule and step validators, collapsing
-- structural failure only at the caller-supplied boundary.
module Moonlight.Rewrite.Algebra.PBPO.Derive
  ( identityTypedRule,
  )
where

import Data.Bifunctor (first)
import Moonlight.Category
  ( Category (..),
  )
import Moonlight.Rewrite.Algebra.PBPO.Rule
  ( PBPOEndpointPosition (..),
    PBPOEndpointRef (..),
    PBPOLegName (..),
    PBPOLegs (..),
    PBPORule,
    PBPORuleObstruction (..),
    PBPOTypingSquareSide (..),
    mkPBPORule,
  )

identityTypedRule ::
  (Category c, Eq (Ob c), Eq (Mor c)) =>
  c ->
  meta ->
  Mor c ->
  Mor c ->
  Either (PBPORuleObstruction c meta) (PBPORule c meta)
identityTypedRule categoryValue metaValue leftLeg rightLeg = do
  leftTarget <- first (PBPOEndpointComputationFailed metaValue (PBPOEndpointRef PBPOLeftLeg PBPOTargetEndpoint)) (target categoryValue leftLeg)
  interfaceSource <- first (PBPOEndpointComputationFailed metaValue (PBPOEndpointRef PBPOLeftLeg PBPOSourceEndpoint)) (source categoryValue leftLeg)
  leftTyping <- first (PBPOTypingSquareCompositionFailed metaValue PBPOTypingSquareViaLeft) (identity categoryValue leftTarget)
  interfaceTyping <- first (PBPOTypingSquareCompositionFailed metaValue PBPOTypingSquareViaContext) (identity categoryValue interfaceSource)
  mkPBPORule
    categoryValue
    metaValue
    PBPOLegs
      { plLeftLeg = leftLeg,
        plRightLeg = rightLeg,
        plLeftTyping = leftTyping,
        plInterfaceTyping = interfaceTyping,
        plContextLeg = leftLeg
      }
