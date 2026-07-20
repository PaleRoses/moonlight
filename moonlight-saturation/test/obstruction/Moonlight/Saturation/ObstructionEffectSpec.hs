module Moonlight.Saturation.ObstructionEffectSpec
  ( obstructionEffectTests,
  )
where

import Moonlight.Saturation.Obstruction.Cohomological.Effect
  ( OptimizationEffect (..),
    immediateEffectLabel,
    latentEffectLabel,
    optimizationEffectLabelAlgebra,
  )
import Moonlight.Sheaf.Obstruction
  ( Anchor (..),
    CapabilityEnvironment (..),
    CapabilitySupport (..),
    ExactLabelCode (..),
    OccurrenceId,
    TypedCapabilityEnvironment (..),
    TypedCapabilitySupport (..),
    lowerCapabilityEnvironment,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

obstructionEffectTests :: TestTree
obstructionEffectTests =
  testGroup
    "obstruction effect"
    [ testCase "optimizationEffectLabelAlgebra lowers immediate and latent rows distinctly" $
        let loweredEnvironment =
              lowerCapabilityEnvironment
                ( TypedCapabilityEnvironment
                    optimizationEffectLabelAlgebra
                    [ TypedCapabilitySupport
                        { tcsAnchors =
                            [ RootAnchor,
                              RootAnchor
                            ],
                          tcsSupportedCapabilities =
                            [ [ immediateEffectLabel [ReadEffect],
                                latentEffectLabel [ControlEffect]
                              ],
                              [ immediateEffectLabel [WriteEffect],
                                latentEffectLabel [ControlEffect]
                              ]
                            ]
                        }
                    ]
                )
                :: CapabilityEnvironment (Anchor OccurrenceId)
         in assertEqual
              "effect rows combine by stage-sensitive union before exact lowering"
              ( CapabilityEnvironment
                  [ CapabilitySupport
                      { csAnchors = [RootAnchor],
                        csSupportedCapabilities =
                          [ [FiniteLabelCode 513],
                            [FiniteLabelCode 514]
                          ]
                      }
                  ]
              )
              loweredEnvironment
    ]
