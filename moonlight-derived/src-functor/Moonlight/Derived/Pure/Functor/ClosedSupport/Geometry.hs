{-# LANGUAGE NamedFieldPuns #-}

module Moonlight.Derived.Pure.Functor.ClosedSupport.Geometry
  ( ClosedSupport
  , mkClosedSupport
  , closedSupportPoset
  , closedSupportNodes
  , validateClosedSupport
  , maximalSupportNodes
  , restrictedClosedPoset
  , supportIntersection
  , supportNodesDescending
  ) where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Vector qualified as V
import Moonlight.Core (MoonlightError (..))
import Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset (..)
  , FinObjectId (..)
  , categoryFromOrderClosure
  , closureOfValidated
  )

data ClosedSupport = ClosedSupport
  { closedSupportPoset :: !DerivedPoset
  , closedSupportNodes :: !IntSet
  }
  deriving stock (Eq, Show)

mkClosedSupport :: DerivedPoset -> IntSet -> Either MoonlightError ClosedSupport
mkClosedSupport posetValue supportNodeSet =
  fmap (ClosedSupport posetValue) (validateClosedSupport "closed support" posetValue supportNodeSet)

validateClosedSupport ::
  String ->
  DerivedPoset ->
  IntSet ->
  Either MoonlightError IntSet
validateClosedSupport context posetValue supportNodeSet = do
  closureValue <-
    closureOfValidated posetValue supportNodeSet
  if closureValue == supportNodeSet
    then
      Right supportNodeSet
    else
      Left
        ( InvariantViolation
            ( context
                <> ": node set is not closed under specialization: expected "
                <> show (IntSet.toList closureValue)
                <> " but received "
                <> show (IntSet.toList supportNodeSet)
            )
        )

maximalSupportNodes :: DerivedPoset -> IntSet -> [FinObjectId]
maximalSupportNodes DerivedPoset {derivedPosetCoversUp, derivedPosetTopoAsc} supportNodeSet =
  [ nodeValue
  | nodeValue@(FinObjectId nodeKey) <- V.toList derivedPosetTopoAsc
  , IntSet.member nodeKey supportNodeSet
  , IntSet.null
      ( IntSet.intersection
          supportNodeSet
          (IntMap.findWithDefault IntSet.empty nodeKey derivedPosetCoversUp)
      )
  ]

restrictedClosedPoset :: DerivedPoset -> IntSet -> DerivedPoset
restrictedClosedPoset
  DerivedPoset
    { derivedPosetNodes
    , derivedPosetUpper
    , derivedPosetLower
    , derivedPosetCoversUp
    , derivedPosetTopoDesc
    , derivedPosetTopoAsc
    }
  supportNodeSet =
    let restrictedNodes = V.filter objectInSupport derivedPosetNodes
        restrictedUpper = restrictRelation derivedPosetUpper
     in DerivedPoset
      { derivedPosetCategory = categoryFromOrderClosure (V.toList restrictedNodes) restrictedUpper
      , derivedPosetNodes = restrictedNodes
      , derivedPosetUpper = restrictedUpper
      , derivedPosetLower = restrictRelation derivedPosetLower
      , derivedPosetCoversUp = restrictRelation derivedPosetCoversUp
      , derivedPosetTopoDesc = V.filter objectInSupport derivedPosetTopoDesc
      , derivedPosetTopoAsc = V.filter objectInSupport derivedPosetTopoAsc
      }
  where
    objectInSupport (FinObjectId nodeKey) =
      IntSet.member nodeKey supportNodeSet

    restrictRelation =
      IntMap.map (IntSet.intersection supportNodeSet)
        . IntMap.filterWithKey
          (\nodeKey _ -> IntSet.member nodeKey supportNodeSet)

supportIntersection ::
  DerivedPoset ->
  FinObjectId ->
  FinObjectId ->
  Either MoonlightError IntSet
supportIntersection posetValue leftNode rightNode = do
  leftClosure <-
    closureOfValidated
      posetValue
      (IntSet.singleton (unFinObjectId leftNode))
  rightClosure <-
    closureOfValidated
      posetValue
      (IntSet.singleton (unFinObjectId rightNode))
  Right (IntSet.intersection leftClosure rightClosure)

supportNodesDescending :: DerivedPoset -> IntSet -> [FinObjectId]
supportNodesDescending DerivedPoset {derivedPosetTopoDesc} supportNodeSet =
  [ nodeValue
  | nodeValue@(FinObjectId nodeKey) <- V.toList derivedPosetTopoDesc
  , IntSet.member nodeKey supportNodeSet
  ]
