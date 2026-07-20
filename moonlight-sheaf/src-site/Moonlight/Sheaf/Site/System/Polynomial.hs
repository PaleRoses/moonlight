module Moonlight.Sheaf.Site.System.Polynomial
  ( PolynomialPresentation (..),
    SystemMorphismPosition (..),
    SystemObjectPosition (..),
    systemMorphismPolynomialPresentation,
    systemObjectPolynomialPresentation,
  )
where

import Moonlight.Sheaf.Site.Interface.Types (MorphismInterface)
import Moonlight.Sheaf.Site.System
  ( AnalyzableSystem (..),
    SystemTag,
  )

data PolynomialPresentation position direction = PolynomialPresentation
  { ppPositions :: [position],
    ppDirections :: position -> direction
  }

data SystemMorphismPosition system =
  SystemMorphismPosition !(SystemCtx system) !(SystemMor system)

data SystemObjectPosition system =
  SystemObjectPosition !(SystemCtx system) !(SystemOb system)

systemMorphismPolynomialPresentation ::
  AnalyzableSystem system =>
  system ->
  PolynomialPresentation
    (SystemMorphismPosition system)
    (MorphismInterface (SystemTag system))
systemMorphismPolynomialPresentation systemValue =
  PolynomialPresentation
    { ppPositions = positionsInContexts systemMorphismsInContext SystemMorphismPosition systemValue,
      ppDirections =
        \(SystemMorphismPosition _contextValue morphismValue) ->
          morphismInterface systemValue morphismValue
    }

systemObjectPolynomialPresentation ::
  AnalyzableSystem system =>
  system ->
  PolynomialPresentation
    (SystemObjectPosition system)
    [SystemMor system]
systemObjectPolynomialPresentation systemValue =
  PolynomialPresentation
    { ppPositions = positionsInContexts systemObjectsInContext SystemObjectPosition systemValue,
      ppDirections =
        \(SystemObjectPosition contextValue objectValue) ->
          filter
            ((== objectValue) . morphismSource systemValue)
            (systemMorphismsInContext systemValue contextValue)
    }

positionsInContexts ::
  AnalyzableSystem system =>
  (system -> SystemCtx system -> [value]) ->
  (SystemCtx system -> value -> position) ->
  system ->
  [position]
positionsInContexts valuesInContext mkPosition systemValue =
  [ mkPosition contextValue value
  | contextValue <- allContexts systemValue,
    value <- valuesInContext systemValue contextValue
  ]
