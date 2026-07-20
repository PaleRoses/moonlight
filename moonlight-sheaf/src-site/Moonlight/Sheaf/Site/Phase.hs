
module Moonlight.Sheaf.Site.Phase
  ( SheafPhase (..),
    RequireStalked,
    RequireConsistent,
    RequireResolved,
  )
where

import Data.Kind (Constraint, Type)
import GHC.TypeLits (ErrorMessage (..), TypeError)

type SheafPhase :: Type
data SheafPhase
  = Empty
  | Stalked
  | Consistent
  | Resolved

type RequireStalked :: SheafPhase -> Constraint
type RequireStalked phase =
  RequirePhase 'Stalked phase

type RequireConsistent :: SheafPhase -> Constraint
type RequireConsistent phase =
  RequirePhase 'Consistent phase

type RequireResolved :: SheafPhase -> Constraint
type RequireResolved phase =
  RequirePhase 'Resolved phase

type RequirePhase :: SheafPhase -> SheafPhase -> Constraint
type family RequirePhase required actual where
  RequirePhase 'Stalked 'Empty =
    PhaseGateError
      ('Text "Stalk access requires Stalked phase or later.")
      ('Text "Call assignStep before accessing stalk data.")
  RequirePhase 'Stalked _ =
    ()
  RequirePhase 'Consistent 'Consistent =
    ()
  RequirePhase 'Consistent 'Resolved =
    ()
  RequirePhase 'Consistent _ =
    PhaseGateError
      ('Text "Restriction and coboundary access require Consistent phase.")
      ('Text "Call consistencyStep after assignStep.")
  RequirePhase 'Resolved 'Resolved =
    ()
  RequirePhase 'Resolved _ =
    PhaseGateError
      ('Text "Resolved access requires Resolved phase.")
      ('Text "Call resolveStep after consistencyStep.")

type PhaseGateError :: ErrorMessage -> ErrorMessage -> Constraint
type PhaseGateError requirement hint =
  TypeError
    ( 'Text "Moonlight.Sheaf phase gate violation."
        ':$$: requirement
        ':$$: hint
    )
