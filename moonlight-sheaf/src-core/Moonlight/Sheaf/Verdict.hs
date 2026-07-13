{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Verdict
  ( Verdict (..),
    ObstructionVerdict,
    acceptedUnit,
    rejectedOne,
    rejectedFromList,
    verdictAllowed,
    verdictRejectedList,
    acceptIfAnyAccepted,
    SearchVerdict (..),
    decidedSearchVerdict,
    completeSearchVerdict,
    searchUndecidedOne,
    searchVerdictObstructions,
    searchVerdictRefusals,
    searchVerdictDecided,
  )
where

import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (fold)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty

type Verdict :: Type -> Type -> Type
data Verdict accepted rejected
  = Accepted !accepted
  | Rejected !rejected
  deriving stock (Eq, Ord, Show, Read, Functor)

type ObstructionVerdict :: Type -> Type
type ObstructionVerdict obstruction = Verdict () (NonEmpty obstruction)

instance Semigroup rejected => Semigroup (Verdict () rejected) where
  Accepted () <> right = right
  left <> Accepted () = left
  Rejected left <> Rejected right = Rejected (left <> right)

instance Semigroup rejected => Monoid (Verdict () rejected) where
  mempty = Accepted ()

acceptedUnit :: ObstructionVerdict obstruction
acceptedUnit = Accepted ()

rejectedOne :: obstruction -> ObstructionVerdict obstruction
rejectedOne obstruction = Rejected (obstruction :| [])

rejectedFromList :: [obstruction] -> ObstructionVerdict obstruction
rejectedFromList = maybe (Accepted ()) Rejected . NonEmpty.nonEmpty

verdictAllowed :: Verdict accepted rejected -> Bool
verdictAllowed (Accepted _) = True
verdictAllowed (Rejected _) = False

verdictRejectedList :: ObstructionVerdict obstruction -> [obstruction]
verdictRejectedList (Accepted ()) = []
verdictRejectedList (Rejected obstructions) = NonEmpty.toList obstructions

acceptIfAnyAccepted :: [ObstructionVerdict obstruction] -> ObstructionVerdict obstruction
acceptIfAnyAccepted verdicts
  | any verdictAllowed verdicts = Accepted ()
  | otherwise = fold verdicts

type SearchVerdict :: Type -> Type -> Type
data SearchVerdict refusal obstruction
  = SearchAccepted
  | SearchRejected !(NonEmpty obstruction)
  | SearchUndecided !(NonEmpty refusal) ![obstruction]
  deriving stock (Eq, Ord, Show, Read, Functor)

instance Semigroup (SearchVerdict refusal obstruction) where
  SearchAccepted <> right = right
  left <> SearchAccepted = left
  SearchRejected left <> SearchRejected right =
    SearchRejected (left <> right)
  SearchRejected obstructions <> SearchUndecided refusals partials =
    SearchUndecided refusals (NonEmpty.toList obstructions <> partials)
  SearchUndecided refusals partials <> SearchRejected obstructions =
    SearchUndecided refusals (partials <> NonEmpty.toList obstructions)
  SearchUndecided leftRefusals leftPartials <> SearchUndecided rightRefusals rightPartials =
    SearchUndecided (leftRefusals <> rightRefusals) (leftPartials <> rightPartials)

instance Monoid (SearchVerdict refusal obstruction) where
  mempty = SearchAccepted

instance Bifunctor SearchVerdict where
  bimap refusalMap obstructionMap searchVerdict =
    case searchVerdict of
      SearchAccepted ->
        SearchAccepted
      SearchRejected obstructions ->
        SearchRejected (fmap obstructionMap obstructions)
      SearchUndecided refusals partials ->
        SearchUndecided (fmap refusalMap refusals) (fmap obstructionMap partials)

decidedSearchVerdict :: ObstructionVerdict obstruction -> SearchVerdict refusal obstruction
decidedSearchVerdict (Accepted ()) = SearchAccepted
decidedSearchVerdict (Rejected obstructions) = SearchRejected obstructions

completeSearchVerdict :: SearchVerdict refusal obstruction -> Maybe (ObstructionVerdict obstruction)
completeSearchVerdict searchVerdict =
  case searchVerdict of
    SearchAccepted -> Just (Accepted ())
    SearchRejected obstructions -> Just (Rejected obstructions)
    SearchUndecided _ _ -> Nothing

searchUndecidedOne :: refusal -> [obstruction] -> SearchVerdict refusal obstruction
searchUndecidedOne refusal partials =
  SearchUndecided (refusal :| []) partials

searchVerdictObstructions :: SearchVerdict refusal obstruction -> [obstruction]
searchVerdictObstructions searchVerdict =
  case searchVerdict of
    SearchAccepted -> []
    SearchRejected obstructions -> NonEmpty.toList obstructions
    SearchUndecided _ partials -> partials

searchVerdictRefusals :: SearchVerdict refusal obstruction -> [refusal]
searchVerdictRefusals searchVerdict =
  case searchVerdict of
    SearchUndecided refusals _ -> NonEmpty.toList refusals
    _ -> []

searchVerdictDecided :: SearchVerdict refusal obstruction -> Bool
searchVerdictDecided searchVerdict =
  case searchVerdict of
    SearchUndecided _ _ -> False
    _ -> True
