module Moonlight.Saturation.Obstruction.Cohomological.Effect
  ( OptimizationEffect (..),
    EffectLabel (..),
    emptyEffectLabel,
    immediateEffectLabel,
    latentEffectLabel,
    mkEffectLabel,
    finiteEffectLabelAlgebra,
    optimizationEffectLabelAlgebra,
  )
where

import Data.Kind (Type)
import Data.Either (partitionEithers)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Sheaf.Obstruction
  ( CapabilityLabelAlgebra,
    CapabilityRow,
    capabilityRowFromList,
    capabilityRowMembers,
    finiteCapabilityRowAlgebra,
    mapCapabilityLabelAlgebra,
  )

type OptimizationEffect :: Type
data OptimizationEffect
  = ReadEffect
  | WriteEffect
  | AllocateEffect
  | ControlEffect
  | ExceptionEffect
  | ForeignEffect
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

type EffectLabel :: Type -> Type
data EffectLabel effect = EffectLabel
  { elImmediate :: !(Set effect),
    elLatent :: !(Set effect)
  }
  deriving stock (Eq, Ord, Show, Read)

type EffectCode :: Type -> Type
data EffectCode effect
  = ImmediateCode !effect
  | LatentCode !effect
  deriving stock (Eq, Ord, Show, Read)

emptyEffectLabel :: EffectLabel effect
emptyEffectLabel =
  EffectLabel Set.empty Set.empty

immediateEffectLabel :: Ord effect => [effect] -> EffectLabel effect
immediateEffectLabel immediateEffects =
  EffectLabel (Set.fromList immediateEffects) Set.empty

latentEffectLabel :: Ord effect => [effect] -> EffectLabel effect
latentEffectLabel latentEffects =
  EffectLabel Set.empty (Set.fromList latentEffects)

mkEffectLabel :: Ord effect => [effect] -> [effect] -> EffectLabel effect
mkEffectLabel immediateEffects latentEffects =
  EffectLabel
    (Set.fromList immediateEffects)
    (Set.fromList latentEffects)

finiteEffectLabelAlgebra ::
  Ord effect =>
  [effect] ->
  CapabilityLabelAlgebra (EffectLabel effect)
finiteEffectLabelAlgebra effectUniverse =
  mapCapabilityLabelAlgebra
    effectLabelToRow
    effectLabelFromRow
    (finiteCapabilityRowAlgebra (effectCodeUniverse effectUniverse))

optimizationEffectLabelAlgebra :: CapabilityLabelAlgebra (EffectLabel OptimizationEffect)
optimizationEffectLabelAlgebra =
  finiteEffectLabelAlgebra
    [minBound .. maxBound]

effectCodeUniverse :: [effect] -> [EffectCode effect]
effectCodeUniverse =
  foldMap
    (\effectValue -> [ImmediateCode effectValue, LatentCode effectValue])

effectLabelToRow :: Ord effect => EffectLabel effect -> CapabilityRow (EffectCode effect)
effectLabelToRow effectLabel =
  capabilityRowFromList
    ( immediateCodes (elImmediate effectLabel)
        <> latentCodes (elLatent effectLabel)
    )

effectLabelFromRow ::
  Ord effect =>
  CapabilityRow (EffectCode effect) ->
  EffectLabel effect
effectLabelFromRow capabilityRow =
  let (immediateEffects, latentEffects) =
        partitionEithers
          [ case effectCode of
              ImmediateCode effectValue ->
                Left effectValue
              LatentCode effectValue ->
                Right effectValue
          | effectCode <- capabilityRowMembers capabilityRow
          ]
   in EffectLabel
        (Set.fromList immediateEffects)
        (Set.fromList latentEffects)

immediateCodes :: Set effect -> [EffectCode effect]
immediateCodes =
  fmap ImmediateCode . Set.toAscList

latentCodes :: Set effect -> [EffectCode effect]
latentCodes =
  fmap LatentCode . Set.toAscList
