{-# LANGUAGE NamedFieldPuns #-}

module Moonlight.Derived.Pure.Functor.Presentation.Internal
  ( PreparedVerdierSite (..)
  , prepareVerdierSite
  , verdierDualPresentationPrepared
  , pushforwardPresentation
  ) where

import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Vector qualified as V
import Moonlight.Core (MoonlightError (..))
import Moonlight.Derived.Pure.Site.InjectiveComplex
  ( InjectiveComplex (..)
  , complexObjectAxes
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix
  ( gaOrder
  , relabelBlocked
  , transposeBlockedMat
  )
import Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset (..)
  , FinObjectId (..)
  , memberOfDerivedPoset
  )

data PreparedVerdierSite = PreparedVerdierSite
  { pvsTopologicalDimension :: !Int
  }

prepareVerdierSite :: DerivedPoset -> PreparedVerdierSite
prepareVerdierSite posetValue =
  PreparedVerdierSite
    { pvsTopologicalDimension = topologicalDimension posetValue
    }

verdierDualPresentationPrepared ::
  PreparedVerdierSite ->
  InjectiveComplex a ->
  InjectiveComplex a
verdierDualPresentationPrepared PreparedVerdierSite {pvsTopologicalDimension} injectiveComplex@InjectiveComplex {icDiffs} =
  InjectiveComplex
    { icStart = pvsTopologicalDimension - cohomologicalEnd injectiveComplex
    , icDiffs = V.reverse (V.map transposeBlockedMat icDiffs)
    }

pushforwardPresentation ::
  (Eq a, Num a) =>
  DerivedPoset ->
  (FinObjectId -> Either MoonlightError FinObjectId) ->
  InjectiveComplex a ->
  Either MoonlightError (InjectiveComplex a)
pushforwardPresentation targetPoset mapToTarget injectiveComplex@InjectiveComplex {icDiffs} = do
  mappedNodes <- traverse mapToTarget (complexNodes injectiveComplex)
  if all (memberOfDerivedPoset targetPoset) mappedNodes
    then do
      relabeledDiffs <- traverse (relabelBlocked mapToTarget) icDiffs
      pure injectiveComplex {icDiffs = relabeledDiffs}
    else
      Left (InvariantViolation "pushforward: map sends nodes outside target poset")

cohomologicalEnd :: InjectiveComplex a -> Int
cohomologicalEnd InjectiveComplex {icStart, icDiffs} =
  icStart + V.length icDiffs

complexNodes :: InjectiveComplex a -> [FinObjectId]
complexNodes injectiveComplex =
  reverse uniqueNodesReversed
  where
    (uniqueNodesReversed, _) =
      foldl'
        collectNode
        ([], IS.empty)
        (concatMap (V.toList . gaOrder) (complexObjectAxes injectiveComplex))

    collectNode (nodesReversed, seenNodes) nodeValue@(FinObjectId nodeKey)
      | IS.member nodeKey seenNodes =
          (nodesReversed, seenNodes)
      | otherwise =
          (nodeValue : nodesReversed, IS.insert nodeKey seenNodes)

topologicalDimension :: DerivedPoset -> Int
topologicalDimension posetValue
  | V.null (derivedPosetNodes posetValue) = -1
  | otherwise =
      foldl' max 0 (IM.elems memo) - 1
  where
    memo =
      foldl'
        assignHeight
        IM.empty
        (V.toList (derivedPosetTopoDesc posetValue))

    assignHeight heights (FinObjectId nodeKey) =
      let successorKeys = IS.toList (IM.findWithDefault IS.empty nodeKey (derivedPosetCoversUp posetValue))
          maxSuccessorHeight =
            foldl'
              (\currentHeight successorKey -> max currentHeight (IM.findWithDefault 0 successorKey heights))
              0
              successorKeys
       in IM.insert nodeKey (1 + maxSuccessorHeight) heights
