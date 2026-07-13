{-# LANGUAGE LambdaCase #-}

-- | PBPO rule well-formedness stratum.
-- It owns the five-leg typed rule, endpoint equalities, and commuting typing
-- square; failures are explicit obstructions before any adhesive step is built.
module Moonlight.Rewrite.Algebra.PBPO.Rule
  ( PBPORule,
    pbpoRuleMeta,
    pbpoRuleLeftLeg,
    pbpoRuleRightLeg,
    pbpoRuleLeftTyping,
    pbpoRuleInterfaceTyping,
    pbpoRuleContextLeg,
    pbpoRuleInterface,
    pbpoRuleLeft,
    pbpoRuleRight,
    pbpoRuleContextInterface,
    pbpoRuleContextType,
    PBPOLegName (..),
    PBPOEndpointPosition (..),
    PBPOEndpointRef (..),
    PBPOEndpointConstraint (..),
    PBPOTypingSquareSide (..),
    PBPORuleObstruction (..),
    PBPOLegs (..),
    mkPBPORule,
    PBPOMatch (..),
  )
where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Moonlight.Category
  ( Category (..),
    MonicMatchWitness,
    composeMor,
  )

type PBPORule :: Type -> Type -> Type
data PBPORule c meta = PBPORule
  { pbpoRuleMeta :: !meta,
    pbpoRuleLeftLeg :: !(Mor c),
    pbpoRuleRightLeg :: !(Mor c),
    pbpoRuleLeftTyping :: !(Mor c),
    pbpoRuleInterfaceTyping :: !(Mor c),
    pbpoRuleContextLeg :: !(Mor c)
  }

deriving stock instance (Eq meta, Eq (Mor c)) => Eq (PBPORule c meta)
deriving stock instance (Show meta, Show (Mor c)) => Show (PBPORule c meta)

type PBPOLegName :: Type
data PBPOLegName
  = PBPOLeftLeg
  | PBPORightLeg
  | PBPOLeftTyping
  | PBPOInterfaceTyping
  | PBPOContextLeg
  deriving stock (Eq, Ord, Show)

type PBPOEndpointPosition :: Type
data PBPOEndpointPosition
  = PBPOSourceEndpoint
  | PBPOTargetEndpoint
  deriving stock (Eq, Ord, Show)

type PBPOEndpointRef :: Type
data PBPOEndpointRef = PBPOEndpointRef
  { pbpoEndpointLeg :: !PBPOLegName,
    pbpoEndpointPosition :: !PBPOEndpointPosition
  }
  deriving stock (Eq, Ord, Show)

type PBPOEndpointConstraint :: Type
data PBPOEndpointConstraint = PBPOEndpointConstraint
  { pbpoEndpointConstraintLeft :: !PBPOEndpointRef,
    pbpoEndpointConstraintRight :: !PBPOEndpointRef
  }
  deriving stock (Eq, Ord, Show)

type PBPOTypingSquareSide :: Type
data PBPOTypingSquareSide
  = PBPOTypingSquareViaContext
  | PBPOTypingSquareViaLeft
  deriving stock (Eq, Ord, Show)

type PBPORuleObstruction :: Type -> Type -> Type
data PBPORuleObstruction c meta
  = PBPOEndpointComputationFailed !meta !PBPOEndpointRef !(CategoryError c)
  | PBPOEndpointConstraintMismatch !meta !PBPOEndpointConstraint !(Ob c) !(Ob c)
  | PBPOTypingSquareCompositionFailed !meta !PBPOTypingSquareSide !(CategoryError c)
  | PBPOTypingSquareDisagrees !meta !(Mor c) !(Mor c)

deriving stock instance
  (Eq meta, Eq (Ob c), Eq (Mor c), Eq (CategoryError c)) =>
  Eq (PBPORuleObstruction c meta)

deriving stock instance
  (Show meta, Show (Ob c), Show (Mor c), Show (CategoryError c)) =>
  Show (PBPORuleObstruction c meta)

type PBPOMatch :: Type -> Type
data PBPOMatch c = PBPOMatch
  { pbpoMatchMonic :: !(MonicMatchWitness c),
    pbpoMatchAdherence :: !(Mor c)
  }

type PBPOLegs :: Type -> Type
data PBPOLegs c = PBPOLegs
  { plLeftLeg :: !(Mor c),
    plRightLeg :: !(Mor c),
    plLeftTyping :: !(Mor c),
    plInterfaceTyping :: !(Mor c),
    plContextLeg :: !(Mor c)
  }

deriving stock instance Eq (Mor c) => Eq (PBPOLegs c)
deriving stock instance Show (Mor c) => Show (PBPOLegs c)

mkPBPORule ::
  (Category c, Eq (Ob c), Eq (Mor c)) =>
  c ->
  meta ->
  PBPOLegs c ->
  Either (PBPORuleObstruction c meta) (PBPORule c meta)
mkPBPORule categoryValue metaValue legs = do
  traverse_
    (validateEndpointConstraint categoryValue metaValue legs)
    pbpoEndpointConstraints

  contextComposite <-
    first
      (PBPOTypingSquareCompositionFailed metaValue PBPOTypingSquareViaContext)
      (composeMor categoryValue (plContextLeg legs) (plInterfaceTyping legs))

  leftComposite <-
    first
      (PBPOTypingSquareCompositionFailed metaValue PBPOTypingSquareViaLeft)
      (composeMor categoryValue (plLeftTyping legs) (plLeftLeg legs))

  if contextComposite == leftComposite
    then
      Right
        PBPORule
          { pbpoRuleMeta = metaValue,
            pbpoRuleLeftLeg = plLeftLeg legs,
            pbpoRuleRightLeg = plRightLeg legs,
            pbpoRuleLeftTyping = plLeftTyping legs,
            pbpoRuleInterfaceTyping = plInterfaceTyping legs,
            pbpoRuleContextLeg = plContextLeg legs
          }
    else
      Left (PBPOTypingSquareDisagrees metaValue contextComposite leftComposite)

pbpoEndpointConstraints :: [PBPOEndpointConstraint]
pbpoEndpointConstraints =
  [ PBPOEndpointConstraint
      (PBPOEndpointRef PBPOLeftLeg PBPOSourceEndpoint)
      (PBPOEndpointRef PBPORightLeg PBPOSourceEndpoint),
    PBPOEndpointConstraint
      (PBPOEndpointRef PBPOLeftLeg PBPOSourceEndpoint)
      (PBPOEndpointRef PBPOInterfaceTyping PBPOSourceEndpoint),
    PBPOEndpointConstraint
      (PBPOEndpointRef PBPOLeftLeg PBPOTargetEndpoint)
      (PBPOEndpointRef PBPOLeftTyping PBPOSourceEndpoint),
    PBPOEndpointConstraint
      (PBPOEndpointRef PBPOInterfaceTyping PBPOTargetEndpoint)
      (PBPOEndpointRef PBPOContextLeg PBPOSourceEndpoint),
    PBPOEndpointConstraint
      (PBPOEndpointRef PBPOLeftTyping PBPOTargetEndpoint)
      (PBPOEndpointRef PBPOContextLeg PBPOTargetEndpoint)
  ]

validateEndpointConstraint ::
  (Category c, Eq (Ob c)) =>
  c ->
  meta ->
  PBPOLegs c ->
  PBPOEndpointConstraint ->
  Either (PBPORuleObstruction c meta) ()
validateEndpointConstraint categoryValue metaValue legs constraint = do
  leftEndpoint <-
    endpointValue categoryValue metaValue legs (pbpoEndpointConstraintLeft constraint)
  rightEndpoint <-
    endpointValue categoryValue metaValue legs (pbpoEndpointConstraintRight constraint)
  if leftEndpoint == rightEndpoint
    then Right ()
    else Left (PBPOEndpointConstraintMismatch metaValue constraint leftEndpoint rightEndpoint)

endpointValue ::
  Category c =>
  c ->
  meta ->
  PBPOLegs c ->
  PBPOEndpointRef ->
  Either (PBPORuleObstruction c meta) (Ob c)
endpointValue categoryValue metaValue legs endpointRef =
  first
    (PBPOEndpointComputationFailed metaValue endpointRef)
    (endpointPositionFunction endpointRef categoryValue (endpointLegMorphism legs (pbpoEndpointLeg endpointRef)))

endpointPositionFunction ::
  Category c =>
  PBPOEndpointRef ->
  c ->
  Mor c ->
  Either (CategoryError c) (Ob c)
endpointPositionFunction endpointRef =
  case pbpoEndpointPosition endpointRef of
    PBPOSourceEndpoint ->
      source
    PBPOTargetEndpoint ->
      target

endpointLegMorphism :: PBPOLegs c -> PBPOLegName -> Mor c
endpointLegMorphism legs =
  \case
    PBPOLeftLeg ->
      plLeftLeg legs
    PBPORightLeg ->
      plRightLeg legs
    PBPOLeftTyping ->
      plLeftTyping legs
    PBPOInterfaceTyping ->
      plInterfaceTyping legs
    PBPOContextLeg ->
      plContextLeg legs

pbpoRuleInterface :: Category c => c -> PBPORule c meta -> Either (CategoryError c) (Ob c)
pbpoRuleInterface categoryValue =
  source categoryValue . pbpoRuleLeftLeg

pbpoRuleLeft :: Category c => c -> PBPORule c meta -> Either (CategoryError c) (Ob c)
pbpoRuleLeft categoryValue =
  target categoryValue . pbpoRuleLeftLeg

pbpoRuleRight :: Category c => c -> PBPORule c meta -> Either (CategoryError c) (Ob c)
pbpoRuleRight categoryValue =
  target categoryValue . pbpoRuleRightLeg

pbpoRuleContextInterface :: Category c => c -> PBPORule c meta -> Either (CategoryError c) (Ob c)
pbpoRuleContextInterface categoryValue =
  source categoryValue . pbpoRuleContextLeg

pbpoRuleContextType :: Category c => c -> PBPORule c meta -> Either (CategoryError c) (Ob c)
pbpoRuleContextType categoryValue =
  target categoryValue . pbpoRuleContextLeg
