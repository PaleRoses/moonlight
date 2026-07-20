module Moonlight.Flow.Runtime.ReplaySelectionSpec
  ( tests,
  )
where

import Data.Set qualified as Set
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (PhaseProject),
  )
import Moonlight.Flow.Patch qualified as Patch
import Moonlight.Flow.Query qualified as Query
import Moonlight.Core qualified as Rel
import Moonlight.Differential.Proposition qualified as Prop
import Moonlight.Differential.Row.Tuple qualified as Tuple
import Moonlight.Flow.Runtime.Create qualified as RuntimeCreate
import Moonlight.Flow.Runtime.Core.Patch.Internal
  ( patchToQuotientPatch,
  )
import Moonlight.Flow.Runtime.Core.Replay.Policy
  ( runtimeReplaySelectionCarriers,
    runtimeReplaySelectionContexts,
    runtimeReplaySelectionPhases,
  )
import Moonlight.Flow.Runtime.Core.State qualified as Core
import Moonlight.Flow.Runtime.Engine.Patch.Apply qualified as Engine
import Moonlight.Flow.Runtime.Kernel
  ( RuntimeEnvelope (..),
  )
import Moonlight.Flow.Runtime.Spec.Schema qualified as Spec
import Moonlight.Flow.Runtime.Spec.Schema qualified as Schema
import Moonlight.Flow.Runtime.Types qualified as RuntimeTypes
import Test.Moonlight.Flow.Runtime.Diagnostics.Validate.BatchRecompute
  ( validateRuntimeQuotientPatchReplay,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "runtime replay selection"
    [ testCase "quotient patch replay validates only affected typed domains" quotientPatchReplaySelectsAffectedDomains
    ]

quotientPatchReplaySelectsAffectedDomains :: Assertion
quotientPatchReplaySelectsAffectedDomains = do
  queryValue <-
    shouldRight
      ( Query.query
          [ matchAtom edge,
            matchAtom label
          ]
          (Query.select [Rel.mkSlotId 0, Rel.mkSlotId 2])
      )
  planValue <-
    shouldRight (Spec.runtimePlanQuery mainContext reachableProp queryValue)
  runtime0 <-
    shouldRight
      ( RuntimeCreate.createRuntime
          ( Spec.runtimeSpec
              ( Spec.runtimeSchemaWithContextOrder
                  ( Spec.contextOrderDecl
                      topContext
                      bottomContext
                      [ (mainContext, topContext),
                        (otherContext, topContext),
                        (bottomContext, mainContext),
                        (bottomContext, otherContext)
                      ]
                  )
                  [ (topContext, Spec.runtimeContextSchema [otherAtom] [otherProp]),
                    (mainContext, Spec.runtimeContextSchema [edge, label] [reachableProp]),
                    (otherContext, Spec.runtimeContextSchema [otherAtom] [otherProp]),
                    (bottomContext, Spec.runtimeContextSchema [otherAtom] [otherProp])
                  ]
              )
              [planValue]
          )
      )
  patchValue <-
    shouldRight
      ( Patch.patch
          <$> sequence
            [ Patch.insert edge (rows [[1, 10]]),
              Patch.insert label (rows [[10, 7]])
            ]
      )

  case runtime0 of
    RuntimeTypes.Runtime kernel0 -> do
      let quotientPatch =
            patchToQuotientPatch
              (Core.rsQuotientEpoch (rdrState kernel0))
              patchValue
      actualKernel <-
        shouldRight (Engine.applyQuotientPatch quotientPatch kernel0)
      selection <-
        shouldRight (validateRuntimeQuotientPatchReplay quotientPatch kernel0 actualKernel)

      assertBool
        "quotient patch replay must include the project phase selected from affected ops"
        (Set.member PhaseProject (runtimeReplaySelectionPhases selection))
      assertBool
        "quotient patch replay must select concrete carrier domains, not a global replay token"
        (not (Set.null (runtimeReplaySelectionCarriers selection)))
      assertBool
        "quotient patch replay must include the patched context"
        (Set.member mainContext (runtimeReplaySelectionContexts selection))
      assertBool
        "quotient patch replay must not include unrelated contexts"
        (Set.notMember otherContext (runtimeReplaySelectionContexts selection))

mainContext :: String
mainContext =
  "main"

topContext :: String
topContext =
  "top"

otherContext :: String
otherContext =
  "other"

bottomContext :: String
bottomContext =
  "bottom"

reachableProp :: Prop.PropositionKey String
reachableProp =
  Prop.PropositionKey "reachable"

otherProp :: Prop.PropositionKey String
otherProp =
  Prop.PropositionKey "other"

edge :: Spec.RuntimeAtom String String
edge =
  Spec.runtimeAtom (Rel.mkAtomId 0) [Rel.mkSlotId 0, Rel.mkSlotId 1]

label :: Spec.RuntimeAtom String String
label =
  Spec.runtimeAtom (Rel.mkAtomId 1) [Rel.mkSlotId 1, Rel.mkSlotId 2]

otherAtom :: Spec.RuntimeAtom String String
otherAtom =
  Spec.runtimeAtom (Rel.mkAtomId 2) [Rel.mkSlotId 0]

rows :: [[Int]] -> [Tuple.RowTupleKey]
rows =
  fmap Tuple.tupleKeyFromInts
{-# INLINE rows #-}

matchAtom :: Spec.RuntimeAtom ctx prop -> Query.Match
matchAtom atomValue =
  Query.match
    ( Query.atomRef
        (Schema.runtimeAtomId atomValue)
        (Schema.rasColumns (Schema.runtimeAtomSchemaDefinition atomValue))
    )
{-# INLINE matchAtom #-}

shouldRight ::
  Show errorValue =>
  Either errorValue value ->
  IO value
shouldRight =
  either
    (assertFailure . show)
    pure
{-# INLINE shouldRight #-}
