{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Derived.Pure.Functor.QuillenA
  ( QuillenACertificate (..)
  , quillenAMaximumCertificate
  , fiberOver
  , fiberHasMaximum
  ) where

import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import Data.Kind (Type)
import qualified Data.Vector as V
import Moonlight.Derived.Pure.Failure (DerivedFailure)
import Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset (..)
  , DerivedPosetFunctor
  , FinObjectId (..)
  , applyDerivedPosetFunctor
  , derivedPosetFunctorObjectPairs
  , derivedPosetFunctorSource
  , derivedPosetFunctorTarget
  , leq
  , star
  )

type QuillenACertificate :: Type
data QuillenACertificate
  = QuillenACertifiedByMaximum
  | QuillenARefutedByEmptyFiber !FinObjectId
  | QuillenAInconclusive !FinObjectId
  deriving stock (Eq, Show)

quillenAMaximumCertificate ::
  DerivedPosetFunctor ->
  Either DerivedFailure QuillenACertificate
quillenAMaximumCertificate functorValue = do
  projectedNodes <- derivedPosetFunctorObjectPairs functorValue
  go projectedNodes (V.toList (derivedPosetNodes targetPoset))
  where
    sourcePoset = derivedPosetFunctorSource functorValue
    targetPoset = derivedPosetFunctorTarget functorValue

    go _ [] = Right QuillenACertifiedByMaximum
    go projectedNodes (targetNode : rest) =
      let fiberValue = fiberOverProjected targetPoset projectedNodes targetNode
      in
      if IS.null fiberValue
            then Right (QuillenARefutedByEmptyFiber targetNode)
            else
              if fiberHasMaximum sourcePoset fiberValue
                then go projectedNodes rest
                else Right (QuillenAInconclusive targetNode)

fiberOver ::
  DerivedPosetFunctor ->
  FinObjectId ->
  Either DerivedFailure IntSet
fiberOver functorValue targetNode = do
  projectedNodes <- traverse (applyDerivedPosetFunctor functorValue) (V.toList (derivedPosetNodes sourcePoset))
  Right (fiberOverProjected targetPoset (zip (V.toList (derivedPosetNodes sourcePoset)) projectedNodes) targetNode)
  where
    sourcePoset = derivedPosetFunctorSource functorValue
    targetPoset = derivedPosetFunctorTarget functorValue

fiberOverProjected :: DerivedPoset -> [(FinObjectId, FinObjectId)] -> FinObjectId -> IntSet
fiberOverProjected targetPoset projectedNodes targetNode =
  IS.fromList
    [ unFinObjectId sourceNode
    | (sourceNode, projectedNode) <- projectedNodes
    , IS.member (unFinObjectId projectedNode) (star targetPoset targetNode)
    ]

fiberHasMaximum ::
  DerivedPoset ->
  IntSet ->
  Bool
fiberHasMaximum sourcePoset fiberNodes =
  case IS.toList fiberNodes of
    [] -> False
    [_] -> True
    nodes ->
      any
        ( \candidate ->
            all
              (\other -> candidate == other || leq sourcePoset (FinObjectId other) (FinObjectId candidate))
              nodes
        )
        nodes
