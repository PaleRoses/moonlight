module Moonlight.Derived.Pure.Pruning.SpectralGate
  ( SpectralPruningOracle
  , SpectralPruningFailure (..)
  , mkSpectralPruningOracle
  , spectralPruningGate
  , iterativeSpectralPrune
  ) where

import Data.Kind (Type)
import Data.List (find, mapAccumL, sortOn)
import Moonlight.Homology
  ( BasisCellRef
  , Bidegree
  , SpectralPage (..)
  , bidegreeCoordinates
  , freeRank
  )

type SpectralPruningOracle :: Type -> Type
data SpectralPruningOracle r = SpectralPruningOracle
  { spoPages :: [SpectralPage r]
  , spoBidegreeOfCell :: BasisCellRef -> Bidegree
  }

data SpectralPruningFailure
  = SpectralPageIndexNonPositive !Int
  | SpectralPageUnavailable !Int
  deriving stock (Eq, Show)

mkSpectralPruningOracle :: [SpectralPage r] -> (BasisCellRef -> Bidegree) -> SpectralPruningOracle r
mkSpectralPruningOracle pagesValue toBidegree =
  SpectralPruningOracle
    { spoPages = pagesValue
    , spoBidegreeOfCell = toBidegree
    }

spectralPruningGate ::
  SpectralPruningOracle r ->
  Int ->
  (seed -> BasisCellRef) ->
  seed ->
  Either SpectralPruningFailure Bool
spectralPruningGate _ pageNumber _ _
  | pageNumber <= 0 = Left (SpectralPageIndexNonPositive pageNumber)
spectralPruningGate oracle pageNumber projectCell seedValue =
  case pageAt oracle pageNumber of
    Nothing -> Left (SpectralPageUnavailable pageNumber)
    Just pageValue -> Right (spectralPruningGateOnPage oracle pageValue projectCell seedValue)

iterativeSpectralPrune ::
  SpectralPruningOracle r ->
  (seed -> BasisCellRef) ->
  [seed] ->
  [(Int, [seed])]
iterativeSpectralPrune oracle projectCell seeds =
  case orderedPages oracle of
    [] -> [(0, seeds)]
    pagesValue ->
      snd
        ( mapAccumL
            (\keptSeeds pageValue ->
               let keptSeeds' =
                     filter (spectralPruningGateOnPage oracle pageValue projectCell) keptSeeds
                in (keptSeeds', (pageIndex pageValue, keptSeeds'))
            )
            seeds
            pagesValue
        )

pageAt :: SpectralPruningOracle r -> Int -> Maybe (SpectralPage r)
pageAt oracle pageNumber =
  find ((== pageNumber) . pageIndex) (spoPages oracle)

orderedPages :: SpectralPruningOracle r -> [SpectralPage r]
orderedPages =
  sortOn pageIndex . spoPages

spectralPruningGateOnPage ::
  SpectralPruningOracle r ->
  SpectralPage r ->
  (seed -> BasisCellRef) ->
  seed ->
  Bool
spectralPruningGateOnPage oracle pageValue projectCell seedValue =
  let bidegreeValue = spoBidegreeOfCell oracle (projectCell seedValue)
      (filtrationDegreeValue, complementaryDegreeValue) =
        bidegreeCoordinates bidegreeValue
   in freeRank (groupAt pageValue filtrationDegreeValue complementaryDegreeValue) > 0
