module Moonlight.Stochastic.Sheaf.Algebra
  ( MarkovKernel (..),
    identityKernel,
    pushforward,
    supportPushforward,
    StochasticKernelWitness (..),
    PossibilisticKernelWitness (..),
    stochasticKernelRestriction,
    possibilisticKernelRestriction,
  )
where

import Moonlight.Sheaf.Section.Morphism
  ( Restriction (..),
    RestrictionId,
    RestrictionKind,
  )
import Moonlight.Stochastic.Sheaf.Core
  ( MarkovKernel (..),
    PossibilisticKernelWitness (..),
    StochasticKernelWitness (..),
    identityKernel,
    pushforward,
    supportPushforward,
  )

stochasticKernelRestriction ::
  RestrictionId ->
  cell ->
  cell ->
  RestrictionKind ->
  MarkovKernel a ->
  Restriction cell (StochasticKernelWitness a)
stochasticKernelRestriction restrictionId sourceCell targetCell restrictionKind kernel =
  Restriction
    { rId = restrictionId,
      rKind = restrictionKind,
      rSource = sourceCell,
      rTarget = targetCell,
      rWitness = StochasticKernelWitness kernel
    }

possibilisticKernelRestriction ::
  RestrictionId ->
  cell ->
  cell ->
  RestrictionKind ->
  MarkovKernel a ->
  Restriction cell (PossibilisticKernelWitness a)
possibilisticKernelRestriction restrictionId sourceCell targetCell restrictionKind kernel =
  Restriction
    { rId = restrictionId,
      rKind = restrictionKind,
      rSource = sourceCell,
      rTarget = targetCell,
      rWitness = PossibilisticKernelWitness kernel
    }
