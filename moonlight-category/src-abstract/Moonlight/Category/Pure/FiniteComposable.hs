{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | Composable chains of morphisms (and the 'FiniteComposableCategory' class) for
-- enumerating a finite category by chain dimension.
module Moonlight.Category.Pure.FiniteComposable
  ( ComposableChain,
    chainStartObject,
    chainMorphisms,
    ComposableChainError (..),
    SizedComposableChain,
    sizedChainDimension,
    sizedChainValue,
    chainDimension,
    chainTerminalObject,
    singletonComposableChain,
    mkComposableChain,
    sizedComposableChain,
    appendComposableMorphism,
    chainsOfDimension,
    FiniteComposableCategory (..),
  )
where

import Data.Bifunctor (first)
import Control.Monad (foldM)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Kind (Constraint, Type)
import Data.List (genericTake)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Numeric.Natural (Natural)
import Moonlight.Category.Pure.Category (Category (..))

type ComposableChain :: Type -> Type
-- | A path whose adjacent morphism endpoints have been validated.
data ComposableChain c = ComposableChain
  { -- | The path's source object.
    chainStartObject :: Ob c,
    -- | The path's current target object.
    chainTerminalObject :: Ob c,
    chainMorphismSequence :: Seq (Mor c)
  }

type SizedComposableChain :: Type -> Type
-- | A validated path paired with its morphism count.
data SizedComposableChain c = SizedComposableChain
  { -- | The number of morphisms in the path.
    sizedChainDimension :: Natural,
    -- | The underlying validated path.
    sizedChainValue :: ComposableChain c
  }

type ComposableChainError :: Type -> Type
-- | A category failure or an endpoint incompatibility encountered while
-- extending a path.
data ComposableChainError c
  = ComposableChainCategoryError (CategoryError c)
  | ComposableChainEndpointMismatch (Ob c) (Ob c)

-- | Count the morphisms in a validated path.
chainDimension :: ComposableChain c -> Natural
chainDimension = fromIntegral . Seq.length . chainMorphismSequence

-- | Project the morphisms in composition order.
chainMorphisms :: ComposableChain c -> [Mor c]
chainMorphisms = toList . chainMorphismSequence

-- | The dimension-zero path at an object.
singletonComposableChain :: Ob c -> ComposableChain c
singletonComposableChain objectValue =
  ComposableChain objectValue objectValue Seq.empty

-- | Validate a morphism sequence from a declared start object.
mkComposableChain :: (Category c, Eq (Ob c)) => c -> Ob c -> [Mor c] -> Either (ComposableChainError c) (ComposableChain c)
mkComposableChain categoryValue startObject =
  foldM (appendComposableMorphism categoryValue) (singletonComposableChain startObject)

-- | Attach the derived dimension to a validated path.
sizedComposableChain :: ComposableChain c -> SizedComposableChain c
sizedComposableChain chainValue =
  SizedComposableChain
    { sizedChainDimension = chainDimension chainValue,
      sizedChainValue = chainValue
    }

-- | Extend a path when the new morphism starts at its terminal object.
appendComposableMorphism :: (Category c, Eq (Ob c)) => c -> ComposableChain c -> Mor c -> Either (ComposableChainError c) (ComposableChain c)
appendComposableMorphism categoryValue chainValue morphism = do
  morphismSource <- first ComposableChainCategoryError (source categoryValue morphism)
  morphismTarget <- first ComposableChainCategoryError (target categoryValue morphism)
  if morphismSource == chainTerminalObject chainValue
    then
      Right
        ComposableChain
          { chainStartObject = chainStartObject chainValue,
            chainTerminalObject = morphismTarget,
            chainMorphismSequence = chainMorphismSequence chainValue |> morphism
          }
    else Left (ComposableChainEndpointMismatch (chainTerminalObject chainValue) morphismSource)

-- | Enumerate all composable paths at one exact dimension.
chainsOfDimension :: FiniteComposableCategory c => c -> Natural -> [ComposableChain c]
chainsOfDimension categoryValue dimensionBound =
  fromMaybe [] (indexByNatural dimensionBound (chainsByDimension categoryValue))

indexByNatural :: Natural -> [a] -> Maybe a
indexByNatural indexValue values =
  case (indexValue, values) of
    (_, []) -> Nothing
    (0, value : _) -> Just value
    (_, _ : rest) -> indexByNatural (indexValue - 1) rest

chainsByDimension :: FiniteComposableCategory c => c -> [[ComposableChain c]]
chainsByDimension categoryValue =
  iterate (extendChains categoryValue) (map singletonComposableChain (enumerateObjects categoryValue))

extendChains :: FiniteComposableCategory c => c -> [ComposableChain c] -> [ComposableChain c]
extendChains categoryValue chains =
  chains
    >>= ( \chainValue ->
            mapMaybe
              (either (const Nothing) Just . appendComposableMorphism categoryValue chainValue)
              (enumerateMorphismsFrom categoryValue (chainTerminalObject chainValue))
        )

extendGrowingChainsNonIdentity ::
  (FiniteComposableCategory c, Eq (Mor c)) =>
  c ->
  [ComposableChain c] ->
  [ComposableChain c]
extendGrowingChainsNonIdentity categoryValue chains =
  chains
    >>= ( \chainValue ->
            let terminalObject = chainTerminalObject chainValue
                terminalIdentity =
                  either (const Nothing) Just (identity categoryValue terminalObject)
             in enumerateMorphismsFrom categoryValue terminalObject
                  & mapMaybe
                    ( \morphism -> do
                        if Just morphism == terminalIdentity
                          then Nothing
                          else either (const Nothing) Just (appendComposableMorphism categoryValue chainValue morphism)
                    )
        )

type FiniteComposableCategory :: Type -> Constraint
-- | A finite category whose objects, morphisms, and composable paths can be
-- enumerated.
class (Category c, Eq (Ob c)) => FiniteComposableCategory c where
  enumerateObjects :: c -> [Ob c]
  enumerateMorphisms :: c -> [Mor c]
  enumerateMorphismsFrom :: c -> Ob c -> [Mor c]
  enumerateMorphismsFrom categoryValue sourceObject =
    enumerateMorphisms categoryValue
      & filter
        ( \morphism ->
            case source categoryValue morphism of
              Right morphismSource -> morphismSource == sourceObject
              Left _ -> False
        )

  enumerateComposableChains :: c -> Natural -> [SizedComposableChain c]
  default enumerateComposableChains :: c -> Natural -> [SizedComposableChain c]
  enumerateComposableChains categoryValue dimensionBound =
    genericTake (dimensionBound + 1) (chainsByDimension categoryValue)
      & foldMap (fmap sizedComposableChain)

  enumerateNonDegenerateChainsByDimension :: Eq (Mor c) => c -> Natural -> [[ComposableChain c]]
  enumerateNonDegenerateChainsByDimension categoryValue dimensionBound =
    genericTake
      (dimensionBound + 1)
      ( iterate
          (extendGrowingChainsNonIdentity categoryValue)
          (fmap singletonComposableChain (enumerateObjects categoryValue))
      )
