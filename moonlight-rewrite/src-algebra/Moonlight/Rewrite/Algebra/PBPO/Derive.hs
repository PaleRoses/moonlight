-- | Derived PBPO conveniences for identity-shaped and unit-category admissions.
-- It owns helpers that pass through the real rule and step validators, collapsing
-- structural failure only at the caller-supplied boundary.
module Moonlight.Rewrite.Algebra.PBPO.Derive
  ( identityTypedRule,
    admitPBPOUnit,
  )
where

import Data.Bifunctor (first)
import Moonlight.Category
  ( Category (..),
    UnitCat (..),
    UnitMor (..),
    witnessMonic,
  )
import Moonlight.Core (note)
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
import Moonlight.Rewrite.Algebra.PBPO.Step
  ( applyPBPO,
  )

identityTypedRule ::
  (Category c, Eq (Ob c), Eq (Mor c)) =>
  c ->
  meta ->
  Mor c ->
  Mor c ->
  Either (PBPORuleObstruction c meta) (PBPORule c meta)
identityTypedRule categoryValue metaValue leftLeg rightLeg = do
  leftTarget <- mapLeft (PBPOEndpointComputationFailed metaValue (PBPOEndpointRef PBPOLeftLeg PBPOTargetEndpoint)) (target categoryValue leftLeg)
  interfaceSource <- mapLeft (PBPOEndpointComputationFailed metaValue (PBPOEndpointRef PBPOLeftLeg PBPOSourceEndpoint)) (source categoryValue leftLeg)
  leftTyping <- mapLeft (PBPOTypingSquareCompositionFailed metaValue PBPOTypingSquareViaLeft) (identity categoryValue leftTarget)
  interfaceTyping <- mapLeft (PBPOTypingSquareCompositionFailed metaValue PBPOTypingSquareViaContext) (identity categoryValue interfaceSource)
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

admitPBPOUnit ::
  hostFailure ->
  source ->
  host ->
  (host -> Either hostFailure host) ->
  Either hostFailure host
admitPBPOUnit structuralFailure sourceValue hostValue admitHost = do
  admittedHost <-
    admitHost hostValue

  unitRule <-
    first
      (const structuralFailure)
      (identityTypedRule UnitCat sourceValue (UnitMor :: Mor UnitCat) UnitMor)

  monicWitness <-
    note
      structuralFailure
      (witnessMonic UnitCat (UnitMor :: Mor UnitCat))

  _unitStep <-
    first
      (const structuralFailure)
      (applyPBPO UnitCat unitRule monicWitness)

  pure admittedHost

mapLeft :: (left -> right) -> Either left value -> Either right value
mapLeft mapper eitherValue =
  case eitherValue of
    Left leftValue -> Left (mapper leftValue)
    Right value -> Right value
