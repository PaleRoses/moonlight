{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Moonlight.Derived.Pure.Functor.Pullback
  ( pullback
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Kind (Type)
import Data.List (foldl')
import Data.Vector (Vector)
import qualified Data.Vector as V
import Moonlight.Core (AdditiveGroup (neg), Field, MoonlightError (..))
import Moonlight.Algebra (IntegralDomain)
import Moonlight.Derived.Pure.Site.Poset
import Moonlight.Derived.Pure.Site.LabeledMatrix
import Moonlight.Derived.Pure.Site.InjectiveComplex
import Moonlight.Derived.Pure.Failure
  ( DerivedFailure (..)
  , derivedFailureToMoonlightError
  )
import Moonlight.Derived.Pure.Gluing.Resolution (resolveLoop)
import Moonlight.Derived.Pure.Gluing.Peeling (minimizeComplex)

type MappingCylinder :: Type
data MappingCylinder = MappingCylinder
  { mcPoset    :: !DerivedPoset
  , mcRightMap :: !(IntMap FinObjectId)
  , mcLeftSet  :: !IntSet
  , mcLeftDesc :: !(Vector FinObjectId)
  } deriving stock (Eq, Show)

buildMappingCylinder :: DerivedPoset -> DerivedPoset -> (FinObjectId -> Either MoonlightError FinObjectId) -> Either MoonlightError MappingCylinder
buildMappingCylinder src tgt f = do
  let srcList = V.toList (derivedPosetNodes src)
      tgtList = V.toList (derivedPosetNodes tgt)
      leftMap = IM.fromList [ (unFinObjectId old, FinObjectId i) | (i, old) <- zip [0 ..] srcList ]
      rightOffset = length srcList
      rightMap = IM.fromList [ (unFinObjectId old, FinObjectId (rightOffset + i)) | (i, old) <- zip [0 ..] tgtList ]
      liftLeft = lookupNode "left" leftMap
      liftRight = lookupNode "right" rightMap
  leftNodes <- traverse liftLeft srcList
  rightNodes <- traverse liftRight tgtList
  sourceCovers <- liftCoverPairs (derivedPosetCoversUp src) liftLeft liftLeft srcList
  targetCovers <- liftCoverPairs (derivedPosetCoversUp tgt) liftRight liftRight tgtList
  crossCovers <- traverse (\sourceNode -> (,) <$> liftLeft sourceNode <*> (f sourceNode >>= liftRight)) srcList
  leftDesc <- V.fromList <$> traverse liftLeft (V.toList (derivedPosetTopoDesc src))
  cylinderPoset <- mkDerivedPosetFromCovers (leftNodes <> rightNodes) (sourceCovers <> targetCovers <> crossCovers)
  pure
    MappingCylinder
      { mcPoset = cylinderPoset
      , mcRightMap = rightMap
      , mcLeftSet = IS.fromList (map unFinObjectId leftNodes)
      , mcLeftDesc = leftDesc
      }
  where
    lookupNode :: String -> IntMap FinObjectId -> FinObjectId -> Either MoonlightError FinObjectId
    lookupNode sideLabel nodeMap oldNode =
      maybe
        (Left (InvariantViolation ("buildMappingCylinder: missing " <> sideLabel <> " node for " <> show oldNode)))
        Right
        (IM.lookup (unFinObjectId oldNode) nodeMap)
    liftCoverPairs :: IntMap IntSet -> (FinObjectId -> Either MoonlightError FinObjectId) -> (FinObjectId -> Either MoonlightError FinObjectId) -> [FinObjectId] -> Either MoonlightError [(FinObjectId, FinObjectId)]
    liftCoverPairs coverMap liftSource liftTarget nodes =
      fmap concat
        ( traverse
            ( \sourceNode ->
                traverse
                  (\targetOrdinal -> (,) <$> liftSource sourceNode <*> liftTarget (FinObjectId targetOrdinal))
                  (IS.toList (IM.findWithDefault IS.empty (unFinObjectId sourceNode) coverMap))
            )
            nodes
        )

negateBlocked :: AdditiveGroup a => BlockedMat a -> BlockedMat a
negateBlocked blockedMat = blockedMat { bmBlocks = IM.map (IM.map negateDenseMat) (bmBlocks blockedMat) }
  where
    negateDenseMat :: AdditiveGroup a => DenseMat a -> DenseMat a
    negateDenseMat denseMat = denseMat { dmData = V.map (V.map neg) (dmData denseMat) }

validatedLift :: String -> IntMap FinObjectId -> [FinObjectId] -> Either MoonlightError (FinObjectId -> Either MoonlightError FinObjectId)
validatedLift context nodeMap nodes =
  if all (\nodeValue -> IM.member (unFinObjectId nodeValue) nodeMap) nodes
    then Right (lookupNode context nodeMap)
    else Left (InvariantViolation (context <> ": encountered a node outside the mapping cylinder codomain"))
  where
    lookupNode label nodeValues nodeValue =
      maybe
        (Left (InvariantViolation (label <> ": missing validated mapping-cylinder node")))
        Right
        (IM.lookup (unFinObjectId nodeValue) nodeValues)

blockedNodes :: BlockedMat a -> [FinObjectId]
blockedNodes blockedMat =
  V.toList (gaOrder (bmRows blockedMat)) <> V.toList (gaOrder (bmCols blockedMat))

pullbackWithMappingCylinder ::
  (Field a, IntegralDomain a, Num a) =>
  DerivedPoset -> MappingCylinder -> Derived a -> Either MoonlightError (Derived a)
pullbackWithMappingCylinder
  src
  mappingCylinder
  derivedValue@Derived {getDerived = injectiveComplex@InjectiveComplex{icStart, icDiffs}} =
    maybe
      (Right derivedValue)
      ( \initialAxis -> do
          liftRightNode <-
            validatedLift
              "pullback"
              (mcRightMap mappingCylinder)
              (V.toList (gaOrder initialAxis) <> concatMap blockedNodes (V.toList icDiffs))
          initialObject <- relabelAxisChecked liftRightNode initialAxis
          let
              leftDesc = V.toList (mcLeftDesc mappingCylinder)
              seed cols maybeInputDifferential =
                fmap
                  negateBlocked
                  ( traverse
                      (relabelBlocked liftRightNode)
                      maybeInputDifferential
                      >>= copyRowsInto cols
                  )
          outputDiffs <-
            resolveLoop
              (mcPoset mappingCylinder)
              leftDesc
              seed
              (restrictBlocked (mcLeftSet mappingCylinder))
              initialObject
              icDiffs
          minimizedComplex <- minimizeComplex (InjectiveComplex (icStart - 1) outputDiffs)
          pure
            ( mkNormalizedDerivedTrusted
                src
                (trustLawfulInjectiveComplex minimizedComplex)
            )
      )
      (initialObjectAxis injectiveComplex)

pullback ::
  (Field a, IntegralDomain a, Num a) =>
  DerivedPosetFunctor -> Derived a -> Either MoonlightError (Derived a)
pullback functorValue derivedValue@Derived {getDerived = injectiveComplex} = do
  let src = derivedPosetFunctorSource functorValue
      tgt = derivedPosetFunctorTarget functorValue
  if derivedPoset derivedValue /= tgt
    then Left (derivedFailureToMoonlightError DerivedFunctorSiteMismatch)
    else
      maybe
        (Right derivedValue)
        ( \_ -> do
            mappingCylinder <- buildMappingCylinder src tgt (firstDerived . applyDerivedPosetFunctor functorValue)
            pullbackWithMappingCylinder src mappingCylinder derivedValue
        )
        (initialObjectAxis injectiveComplex)
  where
    firstDerived = either (Left . derivedFailureToMoonlightError) Right

relabelAxisChecked :: (FinObjectId -> Either MoonlightError FinObjectId) -> GroupedAxis -> Either MoonlightError GroupedAxis
relabelAxisChecked mapNode axisValue = do
  mappedNodes <- traverse mapNode (V.toList (gaOrder axisValue))
  Right
    ( foldl'
        (\mappedAxis (oldNode, newNode) -> appendAxisLabel newNode (axisMultiplicity axisValue oldNode) mappedAxis)
        emptyAxis
        (zip (V.toList (gaOrder axisValue)) mappedNodes)
    )
