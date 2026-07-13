{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Runtime.Facts
  ( deriveContextFactViews,
  )
where

import Data.Map.Strict qualified as Map
import qualified Data.Sequence as Seq
import Data.Set qualified as Set
import Numeric.Natural (Natural)
import Moonlight.Core
  ( SiteIndex (..),
  )
import Moonlight.Saturation.Context.Runtime.State
  ( FactDerivationResult (..),
    RuntimeCore (..),
    runtimeCoreFactInputsWithBase,
    runtimeCoreFactViewKeyAt,
  )
import Moonlight.Saturation.Substrate

deriveContextFactViews ::
  forall u schedulerGroup.
  ( FactSystem u,
    Ord (SatContext u)
  ) =>
  SatCapabilityResolver u ->
  Natural ->
  SiteIndex (SatContext u) (SatFactRule u) ->
  SatGraph u ->
  RuntimeCore u schedulerGroup ->
  Either (SatObstruction u) (FactDerivationResult u)
deriveContextFactViews capabilityResolver capabilityGeneration factProgram graph coreState =
  let baseContext =
        graphBaseContext @u graph
      baseGraph =
        graphBase @u graph
      factInputsByContext =
        runtimeCoreFactInputsWithBase @u baseContext coreState
      cachedFactsByContext =
        rcContextFacts coreState
      cachedDerivationsByContext =
        rcContextFactDerivations coreState
      gateAt contextValue factRules =
        let currentKey =
              runtimeCoreFactViewKeyAt @u
                contextValue
                (fmap (factRuleId @u) factRules)
                capabilityGeneration
                coreState
            factInputs =
              Map.findWithDefault
                (emptyFactStore @u)
                contextValue
                factInputsByContext
         in case
              ( Map.lookup contextValue (rcFactViewKeys coreState),
                Map.lookup contextValue cachedFactsByContext,
                Map.lookup contextValue cachedDerivationsByContext
              )
              of
                (Just cachedKey, Just cachedFacts, Just cachedDerivations)
                  | cachedKey == currentKey ->
                      Left
                        ( cachedFacts,
                          cachedDerivations,
                          Seq.empty,
                          currentKey
                        )
                _ ->
                  Right (factInputs, currentKey)
      contextValues =
        Set.delete
          baseContext
          ( Set.unions
              [ Map.keysSet (siContexts factProgram),
                Map.keysSet factInputsByContext,
                Map.keysSet cachedFactsByContext,
                Map.keysSet cachedDerivationsByContext,
                Map.keysSet (rcFactViewKeys coreState)
              ]
          )
      contextFactRulesByContext =
        Map.fromSet
          ( \contextValue ->
              Map.findWithDefault
                []
                contextValue
                (siContexts factProgram)
          )
          contextValues
   in do
        (baseFacts, baseDerivations, baseRoundSeq, baseFactViewKey) <-
          case gateAt baseContext (siBase factProgram) of
            Left cached ->
              Right cached
            Right (baseFactInputs, baseCurrentKey) -> do
              (derivedFacts, derivedFactIndex, derivedRounds) <-
                deriveFactClosure @u
                  capabilityResolver
                  baseFactInputs
                  (siBase factProgram)
                  baseGraph
                  baseFactInputs
                  (emptyFactIndex @u)
              Right
                ( derivedFacts,
                  derivedFactIndex,
                  Seq.fromList derivedRounds,
                  baseCurrentKey
                )

        let baseRoundCount =
              Seq.length baseRoundSeq

        let (cleanResults, dirtyInputs) =
              Map.mapEitherWithKey
                ( \contextValue factRules ->
                    fmap
                      ( \(factInputs, currentKey) ->
                          (factInputs, factRules, currentKey)
                      )
                      (gateAt contextValue factRules)
                )
                contextFactRulesByContext

        dirtyResults <-
          deriveFactClosuresAtContexts @u
            capabilityResolver
            graph
            ( fmap
                ( \(factInputs, factRules, _currentKey) ->
                    (factInputs, factRules)
                )
                dirtyInputs
            )

        let contextResults =
              Map.union
                cleanResults
                ( Map.intersectionWith
                    ( \(_factInputs, _factRules, currentKey) (derivedFacts, derivedFactIndex, derivedRounds) ->
                        ( derivedFacts,
                          derivedFactIndex,
                          Seq.fromList derivedRounds,
                          currentKey
                        )
                    )
                    dirtyInputs
                    dirtyResults
                )

        let contextFactsByContext =
              fmap
                ( \(contextFacts, _contextDerivations, _contextRounds, _contextKey) ->
                    contextFacts
                )
                contextResults
            contextDerivationsByContext =
              fmap
                ( \(_contextFacts, contextDerivations, _contextRounds, _contextKey) ->
                    contextDerivations
                )
                contextResults
            contextRoundsByContext =
              fmap
                ( \(_contextFacts, _contextDerivations, contextRounds, _contextKey) ->
                    contextRounds
                )
                contextResults
            contextFactViewKeysByContext =
              fmap
                ( \(_contextFacts, _contextDerivations, _contextRounds, contextKey) ->
                    contextKey
                )
                contextResults
            contextRoundCount =
              sum (fmap Seq.length contextRoundsByContext)
            factsByContext =
              Map.union
                contextFactsByContext
                (Map.singleton baseContext baseFacts)
            derivationsByContext =
              Map.union
                contextDerivationsByContext
                (Map.singleton baseContext baseDerivations)
            factViewKeysByContext =
              Map.union
                contextFactViewKeysByContext
                (Map.singleton baseContext baseFactViewKey)
            roundsByContext =
              Map.union
                contextRoundsByContext
                (Map.singleton baseContext baseRoundSeq)
            factRoundCount =
              baseRoundCount + contextRoundCount

        pure
          FactDerivationResult
            { fdrFactsByContext = factsByContext,
              fdrFactDerivationsByContext = derivationsByContext,
              fdrFactViewKeysByContext = factViewKeysByContext,
              fdrFactRoundsByContext = roundsByContext,
              fdrFactRoundCount = factRoundCount
            }
