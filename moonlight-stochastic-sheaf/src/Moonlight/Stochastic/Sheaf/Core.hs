module Moonlight.Stochastic.Sheaf.Core
  ( StochasticStalk,
    stochasticStalk,
    unstochasticStalk,
    StochasticSection,
    StochasticMismatch (..),
    stochasticStalkOps,
    PossibilisticStalk,
    possibilisticStalk,
    unpossibilisticStalk,
    possibilisticStalkFromCategorical,
    possibilisticStalkFromStochastic,
    PossibilisticSection,
    PossibilisticMismatch (..),
    possibilisticStalkOps,
    stochasticDivergenceTolerance,
    MarkovKernel (..),
    identityKernel,
    pushforward,
    supportPushforward,
    StochasticKernelWitness (..),
    PossibilisticKernelWitness (..),
  )
where

import Data.Function ((&))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Probability
  ( Categorical,
    Divergence,
    blendCategorical,
    categoricalFoldMap1,
    categoricalSupport,
    certainCategorical,
    divergenceValue,
    jsDivergence,
    positiveProbOne,
  )
import Moonlight.Sheaf.Section.Stalk (StalkAlgebra (..), StalkRestrictionKernel (..))
import Moonlight.Sheaf.Section.Store.Types (TotalSectionStore)
import Prelude

type StochasticStalk :: Type -> Type
newtype StochasticStalk a = StochasticStalk
  { unstochasticStalk :: Categorical a
  }
  deriving stock (Eq, Show)

stochasticStalk :: Categorical a -> StochasticStalk a
stochasticStalk = StochasticStalk

type StochasticSection :: Type -> Type -> Type -> Type
type StochasticSection owner cell a = TotalSectionStore owner cell (StochasticStalk a)

type PossibilisticStalk :: Type -> Type
newtype PossibilisticStalk a = PossibilisticStalk
  { unpossibilisticStalk :: Set a
  }
  deriving stock (Eq, Show)

possibilisticStalk :: Set a -> PossibilisticStalk a
possibilisticStalk = PossibilisticStalk

possibilisticStalkFromCategorical :: Categorical a -> PossibilisticStalk a
possibilisticStalkFromCategorical =
  possibilisticStalk . categoricalSupport

possibilisticStalkFromStochastic :: StochasticStalk a -> PossibilisticStalk a
possibilisticStalkFromStochastic =
  possibilisticStalkFromCategorical . unstochasticStalk

type MarkovKernel :: Type -> Type
newtype MarkovKernel a = MarkovKernel
  { applyKernel :: a -> StochasticStalk a
  }

newtype StochasticKernelWitness a = StochasticKernelWitness
  { stochasticKernelWitnessKernel :: MarkovKernel a
  }

newtype PossibilisticKernelWitness a = PossibilisticKernelWitness
  { possibilisticKernelWitnessKernel :: MarkovKernel a
  }

identityKernel :: MarkovKernel a
identityKernel =
  MarkovKernel (stochasticStalk . certainCategorical)

pushforward :: Ord a => MarkovKernel a -> StochasticStalk a -> StochasticStalk a
pushforward kernel source =
  stochasticStalk
    ( blendCategorical
        ( categoricalFoldMap1
            ( \(outcome, probability) ->
                (probability, unstochasticStalk (applyKernel kernel outcome))
                  :| []
            )
            (unstochasticStalk source)
        )
    )

supportPushforward :: Ord a => MarkovKernel a -> PossibilisticStalk a -> PossibilisticStalk a
supportPushforward kernel source =
  source
    & unpossibilisticStalk
    & Set.toAscList
    & foldMap
      ( unpossibilisticStalk
          . possibilisticStalkFromStochastic
          . applyKernel kernel
      )
    & possibilisticStalk

type PossibilisticSection :: Type -> Type -> Type -> Type
type PossibilisticSection owner cell a = TotalSectionStore owner cell (PossibilisticStalk a)

stochasticDivergenceTolerance :: Double
stochasticDivergenceTolerance = 1.0e-6

type StochasticMismatch :: Type -> Type
data StochasticMismatch a
  = DivergenceExceedsThreshold Divergence
  | SupportMismatch (Set a) (Set a)
  deriving stock (Eq, Show)

type PossibilisticMismatch :: Type -> Type
newtype PossibilisticMismatch a = PossibilisticSupportMismatch (Set a, Set a)
  deriving stock (Eq, Show)

stochasticStalkOps :: Ord a => StalkAlgebra (StochasticKernelWitness a) (StochasticStalk a) (StochasticMismatch a) ()
stochasticStalkOps =
  StalkAlgebra
    { saRestrictionKernel = StalkRestrictionMap . pushforward . stochasticKernelWitnessKernel,
      saMismatches =
        \left right -> supportMismatch left right <> divergenceMismatch left right,
      saMerge =
        \left right ->
          Right
            ( StochasticStalk
                ( blendCategorical
                    ( (positiveProbOne, unstochasticStalk left)
                        :| [(positiveProbOne, unstochasticStalk right)]
                    )
                )
            ),
      saRepair = const (Left ()),
      saNormalize = id
    }

supportMismatch :: Ord a => StochasticStalk a -> StochasticStalk a -> [StochasticMismatch a]
supportMismatch left right =
  let leftSupport = categoricalSupport (unstochasticStalk left)
      rightSupport = categoricalSupport (unstochasticStalk right)
   in if leftSupport == rightSupport
        then []
        else [SupportMismatch leftSupport rightSupport]

divergenceMismatch :: Ord a => StochasticStalk a -> StochasticStalk a -> [StochasticMismatch a]
divergenceMismatch left right =
  let divergence = jsDivergence (unstochasticStalk left) (unstochasticStalk right)
   in if divergenceValue divergence < stochasticDivergenceTolerance
        then []
        else [DivergenceExceedsThreshold divergence]

possibilisticStalkOps :: Ord a => StalkAlgebra (PossibilisticKernelWitness a) (PossibilisticStalk a) (PossibilisticMismatch a) ()
possibilisticStalkOps =
  StalkAlgebra
    { saRestrictionKernel = StalkRestrictionMap . supportPushforward . possibilisticKernelWitnessKernel,
      saMismatches =
        \left right ->
          let leftSupport = unpossibilisticStalk left
              rightSupport = unpossibilisticStalk right
           in if leftSupport == rightSupport
                then []
                else [PossibilisticSupportMismatch (leftSupport, rightSupport)],
      saMerge =
        \left right ->
          Right
            ( possibilisticStalk
                ( Set.union
                    (unpossibilisticStalk left)
                    (unpossibilisticStalk right)
                )
            ),
      saRepair = const (Left ()),
      saNormalize = id
    }
