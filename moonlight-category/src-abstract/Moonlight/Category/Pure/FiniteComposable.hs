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
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Numeric.Natural (Natural)
import Moonlight.Category.Pure.Category (Category (..))

type ComposableChain :: Type -> Type
data ComposableChain c = ComposableChain
  { chainStartObject :: Ob c,
    chainTerminalObject :: Ob c,
    chainMorphismSequence :: Seq (Mor c)
  }

type SizedComposableChain :: Type -> Type
data SizedComposableChain c = SizedComposableChain
  { sizedChainDimension :: Natural,
    sizedChainValue :: ComposableChain c
  }

type ComposableChainError :: Type -> Type
data ComposableChainError c
  = ComposableChainCategoryError (CategoryError c)
  | ComposableChainEndpointMismatch (Ob c) (Ob c)

chainDimension :: ComposableChain c -> Natural
chainDimension = fromIntegral . Seq.length . chainMorphismSequence

chainMorphisms :: ComposableChain c -> [Mor c]
chainMorphisms = toList . chainMorphismSequence

singletonComposableChain :: Ob c -> ComposableChain c
singletonComposableChain objectValue =
  ComposableChain objectValue objectValue Seq.empty

mkComposableChain :: (Category c, Eq (Ob c)) => c -> Ob c -> [Mor c] -> Either (ComposableChainError c) (ComposableChain c)
mkComposableChain categoryValue startObject morphisms =
  foldM (appendComposableMorphism categoryValue) (singletonComposableChain startObject) morphisms

sizedComposableChain :: ComposableChain c -> SizedComposableChain c
sizedComposableChain chainValue =
  SizedComposableChain
    { sizedChainDimension = chainDimension chainValue,
      sizedChainValue = chainValue
    }

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

chainsOfDimension :: FiniteComposableCategory c => c -> Natural -> [ComposableChain c]
chainsOfDimension categoryValue dimensionBound =
  maybe
    []
    id
    (indexByNatural dimensionBound (chainsByDimension categoryValue))

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
