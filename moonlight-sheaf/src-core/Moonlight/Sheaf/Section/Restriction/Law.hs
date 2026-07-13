{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Section.Restriction.Law
  ( RestrictionSurfaceError (..),
    RestrictionLawFailure (..),
    PreparedRestrictionLawSurface,
    prepareRestrictionLawSurface,
    checkRestrictionIdentityLaw,
    checkPreparedRestrictionIdentityLaw,
    checkRestrictionCompositionLaw,
    checkPreparedRestrictionCompositionLaw,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Sheaf.Section.Morphism
  ( Restriction,
    RestrictionArrow (..),
    composeRestrictionArrow,
    restrictApply,
    restrictionArrow,
  )
import Moonlight.Sheaf.Section.Restriction
  ( RestrictionIndex,
    restrictionEntries,
  )
import Moonlight.Sheaf.Section.Stalk
  ( StalkAlgebra,
    stalkMismatches,
  )

type RestrictionSurfaceError :: Type -> Type
data RestrictionSurfaceError cell
  = MissingRestriction !(RestrictionArrow cell)
  | AmbiguousRestriction !(RestrictionArrow cell)
  | MissingCompositeRestriction
      !(RestrictionArrow cell)
      !(RestrictionArrow cell)
      !(RestrictionArrow cell)
  | AmbiguousCompositeRestriction
      !(RestrictionArrow cell)
      !(RestrictionArrow cell)
      !(RestrictionArrow cell)
  deriving stock (Eq, Ord, Show)

type RestrictionLawFailure :: Type -> Type -> Type
data RestrictionLawFailure cell mismatch
  = IdentitySurfaceFailure !(RestrictionSurfaceError cell)
  | CompositionSurfaceFailure !(RestrictionSurfaceError cell)
  | IdentityLawMismatch !(RestrictionArrow cell) ![mismatch]
  | CompositionLawMismatch
      !(RestrictionArrow cell)
      !(RestrictionArrow cell)
      !(RestrictionArrow cell)
      ![mismatch]
  deriving stock (Eq, Show)

type PreparedRestrictionLawSurface :: Type -> Type -> Type
newtype PreparedRestrictionLawSurface cell witness = PreparedRestrictionLawSurface
  { prlsRestrictionByArrow :: Map (RestrictionArrow cell) [Restriction cell witness]
  }

prepareRestrictionLawSurface ::
  Ord cell =>
  RestrictionIndex cell witness ->
  PreparedRestrictionLawSurface cell witness
prepareRestrictionLawSurface =
  PreparedRestrictionLawSurface . restrictionByArrow

checkRestrictionIdentityLaw ::
  Ord cell =>
  StalkAlgebra witness stalk mismatch repairObstruction ->
  RestrictionIndex cell witness ->
  cell ->
  stalk ->
  Maybe (RestrictionLawFailure cell mismatch)
checkRestrictionIdentityLaw stalkAlgebra restrictions cell stalkValue =
  checkPreparedRestrictionIdentityLaw
    stalkAlgebra
    (prepareRestrictionLawSurface restrictions)
    cell
    stalkValue

checkPreparedRestrictionIdentityLaw ::
  Ord cell =>
  StalkAlgebra witness stalk mismatch repairObstruction ->
  PreparedRestrictionLawSurface cell witness ->
  cell ->
  stalk ->
  Maybe (RestrictionLawFailure cell mismatch)
checkPreparedRestrictionIdentityLaw stalkAlgebra lawSurface cell stalkValue =
  let identityArrow = RestrictionArrow cell cell
      restrictionsByArrow = prlsRestrictionByArrow lawSurface
   in case uniqueRestrictionIn restrictionsByArrow identityArrow of
        Left surfaceError ->
          Just (IdentitySurfaceFailure surfaceError)
        Right identityRestriction ->
          let mismatches =
                stalkMismatches
                  stalkAlgebra
                  (restrictApply stalkAlgebra identityRestriction stalkValue)
                  stalkValue
           in if null mismatches
                then Nothing
                else Just (IdentityLawMismatch identityArrow mismatches)

checkRestrictionCompositionLaw ::
  Ord cell =>
  StalkAlgebra witness stalk mismatch repairObstruction ->
  RestrictionIndex cell witness ->
  RestrictionArrow cell ->
  RestrictionArrow cell ->
  stalk ->
  Maybe (RestrictionLawFailure cell mismatch)
checkRestrictionCompositionLaw stalkAlgebra restrictions firstArrow secondArrow stalkValue =
  checkPreparedRestrictionCompositionLaw
    stalkAlgebra
    (prepareRestrictionLawSurface restrictions)
    firstArrow
    secondArrow
    stalkValue

checkPreparedRestrictionCompositionLaw ::
  Ord cell =>
  StalkAlgebra witness stalk mismatch repairObstruction ->
  PreparedRestrictionLawSurface cell witness ->
  RestrictionArrow cell ->
  RestrictionArrow cell ->
  stalk ->
  Maybe (RestrictionLawFailure cell mismatch)
checkPreparedRestrictionCompositionLaw stalkAlgebra lawSurface firstArrow secondArrow stalkValue =
  let restrictionsByArrow =
        prlsRestrictionByArrow lawSurface
   in case composeRestrictionArrow firstArrow secondArrow of
    Nothing ->
      Nothing
    Just compositeArrow ->
      case
        ( uniqueRestrictionIn restrictionsByArrow firstArrow,
          uniqueRestrictionIn restrictionsByArrow secondArrow,
          uniqueRestrictionIn restrictionsByArrow compositeArrow
        ) of
        (Left surfaceError, _, _) ->
          Just (CompositionSurfaceFailure surfaceError)
        (_, Left surfaceError, _) ->
          Just (CompositionSurfaceFailure surfaceError)
        (_, _, Left (MissingRestriction _)) ->
          Just
            ( CompositionSurfaceFailure
                (MissingCompositeRestriction firstArrow secondArrow compositeArrow)
            )
        (_, _, Left (AmbiguousRestriction _)) ->
          Just
            ( CompositionSurfaceFailure
                (AmbiguousCompositeRestriction firstArrow secondArrow compositeArrow)
            )
        (_, _, Left surfaceError) ->
          Just (CompositionSurfaceFailure surfaceError)
        (Right firstRestriction, Right secondRestriction, Right compositeRestriction) ->
          let sequentialRestriction =
                restrictApply stalkAlgebra secondRestriction
                  (restrictApply stalkAlgebra firstRestriction stalkValue)
              directRestriction =
                restrictApply stalkAlgebra compositeRestriction stalkValue
              mismatches =
                stalkMismatches stalkAlgebra sequentialRestriction directRestriction
           in if null mismatches
                then Nothing
                else
                  Just
                    ( CompositionLawMismatch
                        firstArrow
                        secondArrow
                        compositeArrow
                        mismatches
                    )

restrictionByArrow ::
  Ord cell =>
  RestrictionIndex cell witness ->
  Map (RestrictionArrow cell) [Restriction cell witness]
restrictionByArrow restrictions =
  Map.fromListWith
    (<>)
    [ (restrictionArrow restriction, [restriction])
      | restriction <- restrictionEntries restrictions
    ]

uniqueRestrictionIn ::
  Ord cell =>
  Map (RestrictionArrow cell) [Restriction cell witness] ->
  RestrictionArrow cell ->
  Either (RestrictionSurfaceError cell) (Restriction cell witness)
uniqueRestrictionIn restrictionsByArrow arrow =
  case Map.findWithDefault [] arrow restrictionsByArrow of
    [] ->
      Left (MissingRestriction arrow)
    [restriction] ->
      Right restriction
    _ ->
      Left (AmbiguousRestriction arrow)
