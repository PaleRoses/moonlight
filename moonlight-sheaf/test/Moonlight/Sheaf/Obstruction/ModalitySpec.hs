{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Obstruction.ModalitySpec
  ( modalityTests,
  )
where

import Data.Dependent.Map qualified as DMap
import Data.Dependent.Sum (DSum ((:=>)))
import Data.EqP (EqP (..))
import Data.Functor.Identity (Identity (..))
import Data.GADT.Compare
  ( GEq (..),
    GCompare (..),
    GOrdering (..),
  )
import Data.Kind (Type)
import Data.IntSet qualified as IntSet
import Data.OrdP (OrdP (..))
import Data.Type.Equality ((:~:) (Refl))
import Moonlight.Sheaf.Obstruction
  ( IndexedEnvironment,
    indexedEnvironmentFromList,
  )
import Moonlight.Sheaf.Obstruction
  ( SectionProjection (..),
    SectionCoordinate (..),
    RelationProjectionConflict (..),
    RelationProjectionMode (RelationalProjection, StructuralProjection),
    emptyRelationProjectionPolicy,
    relationProjectionPolicyFor,
  )
import Moonlight.Sheaf.Obstruction
  ( CapabilityEnvironment (..),
    CapabilityLabelAlgebra,
    CapabilityRow,
    capabilityRowFromList,
    CapabilitySupport (..),
    finiteCapabilityRowAlgebra,
    data MissingReferenceGap,
    ModalityContribution (..),
    ModalityRegistry,
    ObstructionModality,
    obstructionModality,
    obstructionModalityWithReification,
    evaluateModalities,
    evaluateModalityRegistry,
    lowerCapabilityEnvironment,
    modalityRegistryFromList,
    modalityRegistryKeys,
    modalityRegistryProjection,
    modalityRegistryProjectionConflicts,
    modalityRegistryReification,
    typedCapabilityModality,
    TypedCapabilityEnvironment (..),
    TypedCapabilitySupport (..),
  )
import Moonlight.Sheaf.Obstruction
  ( SectionAssignment (..),
    reifySectionAssignment,
    sectionReification,
  )
import Moonlight.Sheaf.Obstruction
  ( Anchor (..),
    ConstraintId (..),
    ExactConstraint (..),
    data CapabilityConstraint,
    ExactLabelCode (..),
    OccurrenceId (..),
    RelationFlavor (..),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertEqual, assertFailure, testCase)

type TestModalityKey :: Type -> Type
data TestModalityKey value where
  AlphaModality :: TestModalityKey Int
  BetaModality :: TestModalityKey Bool

eqTestModalityKey ::
  TestModalityKey left ->
  TestModalityKey right ->
  Bool
eqTestModalityKey leftKey rightKey =
  case (leftKey, rightKey) of
    (AlphaModality, AlphaModality) -> True
    (BetaModality, BetaModality) -> True
    _ -> False

compareTestModalityKey ::
  TestModalityKey left ->
  TestModalityKey right ->
  Ordering
compareTestModalityKey leftKey rightKey =
  case (leftKey, rightKey) of
    (AlphaModality, AlphaModality) -> EQ
    (AlphaModality, BetaModality) -> LT
    (BetaModality, AlphaModality) -> GT
    (BetaModality, BetaModality) -> EQ

type TestCapabilityAtom :: Type
newtype TestCapabilityAtom = TestCapabilityAtom
  { unTestCapabilityAtom :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

testCapabilityAlgebra :: [Int] -> CapabilityLabelAlgebra (CapabilityRow TestCapabilityAtom)
testCapabilityAlgebra capabilityUniverse =
  finiteCapabilityRowAlgebra
    (fmap TestCapabilityAtom capabilityUniverse)

instance Eq (TestModalityKey value) where
  (==) =
    eqTestModalityKey

instance Ord (TestModalityKey value) where
  compare =
    compareTestModalityKey

instance EqP TestModalityKey where
  eqp =
    eqTestModalityKey

instance OrdP TestModalityKey where
  comparep =
    compareTestModalityKey

instance GEq TestModalityKey where
  geq AlphaModality AlphaModality = Just Refl
  geq BetaModality BetaModality = Just Refl
  geq _ _ = Nothing

instance GCompare TestModalityKey where
  gcompare AlphaModality AlphaModality = GEQ
  gcompare AlphaModality BetaModality = GLT
  gcompare BetaModality AlphaModality = GGT
  gcompare BetaModality BetaModality = GEQ

modalityTests :: TestTree
modalityTests =
  testGroup
    "modality"
    [ testCase "evaluateModalities threads constraint identities and accumulates contributions" $
        let firstModality :: ObstructionModality (Anchor OccurrenceId) () () ()
            firstModality =
              obstructionModality emptyRelationProjectionPolicy
                (\startingId _ ->
                   ( ConstraintId (unConstraintId startingId + 1),
                     ModalityContribution
                       { mcExactConstraints =
                           [ EqualityConstraint
                               startingId
                               RootAnchor
                               (OccurrenceAnchor (OccurrenceId 7))
                               (IntSet.singleton 11)
                           ],
                         mcLoweringGaps = []
                       }
                   )
                )
            secondModality :: ObstructionModality (Anchor OccurrenceId) () () ()
            secondModality =
              obstructionModality emptyRelationProjectionPolicy
                (\startingId _ ->
                   ( ConstraintId (unConstraintId startingId + 2),
                     ModalityContribution
                       { mcExactConstraints =
                           [ GuardConstraint
                               (ConstraintId (unConstraintId startingId + 1))
                               RootAnchor
                               RootAnchor
                               IntSet.empty
                           ],
                         mcLoweringGaps =
                           [MissingReferenceGap FactFlavor startingId []]
                       }
                   )
                )
            contribution :: ModalityContribution (Anchor OccurrenceId) ()
            contribution =
              evaluateModalities (ConstraintId 3) () [firstModality, secondModality]
         in assertEqual
              "modalities accumulate with threaded identifiers"
              ( ModalityContribution
                  { mcExactConstraints =
                      [ EqualityConstraint
                          (ConstraintId 3)
                          RootAnchor
                          (OccurrenceAnchor (OccurrenceId 7))
                          (IntSet.singleton 11),
                        GuardConstraint
                          (ConstraintId 5)
                          RootAnchor
                          RootAnchor
                          IntSet.empty
                      ],
                    mcLoweringGaps =
                      [MissingReferenceGap FactFlavor (ConstraintId 4) []]
                  }
              )
              contribution
    , testCase "typedCapabilityModality lowers typed supports through the capability algebra" $
        let contribution :: ModalityContribution (Anchor OccurrenceId) ()
            contribution =
              evaluateModalities
                (ConstraintId 9)
                ( TypedCapabilityEnvironment
                    (testCapabilityAlgebra [11, 13])
                    [ TypedCapabilitySupport
                        { tcsAnchors = [RootAnchor, OccurrenceAnchor (OccurrenceId 4)],
                          tcsSupportedCapabilities =
                              [ [ capabilityRowFromList [TestCapabilityAtom 11],
                                  capabilityRowFromList [TestCapabilityAtom 13]
                                ]
                              ]
                        }
                    ]
                )
                [typedCapabilityModality]
         in assertEqual
              "typed capability supports become exact capability constraints"
              ( ModalityContribution
                  { mcExactConstraints =
                      [ CapabilityConstraint
                          (ConstraintId 9)
                          [RootAnchor, OccurrenceAnchor (OccurrenceId 4)]
                          [[FiniteLabelCode 1, FiniteLabelCode 2]]
                      ],
                    mcLoweringGaps = []
                  }
              )
              contribution
    , testCase "lowerCapabilityEnvironment unions repeated-anchor effect rows" $
        let loweredEnvironment =
              lowerCapabilityEnvironment
                ( TypedCapabilityEnvironment
                    (testCapabilityAlgebra [3, 4, 5])
                    [ TypedCapabilitySupport
                        { tcsAnchors =
                            [ RootAnchor,
                              RootAnchor,
                              OccurrenceAnchor (OccurrenceId 4)
                            ],
                          tcsSupportedCapabilities =
                            [ [ capabilityRowFromList [TestCapabilityAtom 3],
                                capabilityRowFromList [TestCapabilityAtom 3],
                                capabilityRowFromList [TestCapabilityAtom 5]
                              ],
                              [ capabilityRowFromList [TestCapabilityAtom 3],
                                capabilityRowFromList [TestCapabilityAtom 4],
                                capabilityRowFromList [TestCapabilityAtom 5]
                              ]
                            ]
                        }
                    ]
                )
         in assertEqual
              "repeated anchors combine by row union before encoding"
              ( CapabilityEnvironment
                  [ CapabilitySupport
                      { csAnchors =
                          [ RootAnchor,
                            OccurrenceAnchor (OccurrenceId 4)
                          ],
                        csSupportedCapabilities =
                          [ [FiniteLabelCode 1, FiniteLabelCode 4],
                            [FiniteLabelCode 3, FiniteLabelCode 4]
                          ]
                      }
                  ]
              )
              loweredEnvironment
    , testCase "evaluateModalityRegistry evaluates heterogeneous payloads in key order" $
        let registry :: ModalityRegistry TestModalityKey (Anchor OccurrenceId) () ()
            registry =
              modalityRegistryFromList
                [ BetaModality :=>
                    obstructionModality emptyRelationProjectionPolicy
                      (\startingId betaEnabled ->
                         ( ConstraintId (unConstraintId startingId + 1),
                           ModalityContribution
                             { mcExactConstraints =
                                 if betaEnabled
                                   then
                                     [ GuardConstraint
                                         startingId
                                         RootAnchor
                                         RootAnchor
                                         IntSet.empty
                                     ]
                                   else [],
                               mcLoweringGaps = []
                             }
                         )
                      ),
                  AlphaModality :=>
                    obstructionModality emptyRelationProjectionPolicy
                      (\startingId alphaSupport ->
                         ( ConstraintId (unConstraintId startingId + 1),
                           ModalityContribution
                             { mcExactConstraints =
                                 [ EqualityConstraint
                                     startingId
                                     RootAnchor
                                     (OccurrenceAnchor (OccurrenceId 9))
                                     (IntSet.singleton alphaSupport)
                                 ],
                               mcLoweringGaps = []
                             }
                         )
                      )
                ]
            environment :: IndexedEnvironment TestModalityKey
            environment =
              indexedEnvironmentFromList
                [ AlphaModality :=> Identity (17 :: Int),
                  BetaModality :=> Identity True
                ]
            contribution :: ModalityContribution (Anchor OccurrenceId) ()
            contribution =
              evaluateModalityRegistry (ConstraintId 4) environment registry
         in assertEqual
              "registry order follows key order while preserving payload types"
              ( ModalityContribution
                  { mcExactConstraints =
                      [ EqualityConstraint
                          (ConstraintId 4)
                          RootAnchor
                          (OccurrenceAnchor (OccurrenceId 9))
                          (IntSet.singleton 17),
                        GuardConstraint
                          (ConstraintId 5)
                          RootAnchor
                          RootAnchor
                          IntSet.empty
                      ],
                    mcLoweringGaps = []
                  }
              )
              contribution
    , testCase "modalityRegistryReification composes hooks in key order" $
        let registry :: ModalityRegistry TestModalityKey (Anchor OccurrenceId) Int ()
            registry =
              modalityRegistryFromList
                [ BetaModality :=>
                    obstructionModalityWithReification
                      emptyRelationProjectionPolicy
                      (\betaEnabled ->
                         sectionReification
                           (\_ seedValue ->
                              if betaEnabled
                                then Just (seedValue * 2)
                                else Just seedValue
                           )
                      )
                      (\startingId _ -> (startingId, mempty)),
                  AlphaModality :=>
                    obstructionModalityWithReification
                      emptyRelationProjectionPolicy
                      (\alphaOffset ->
                         sectionReification (\_ -> Just . (+ alphaOffset))
                      )
                      (\startingId _ -> (startingId, mempty))
                ]
            environment :: IndexedEnvironment TestModalityKey
            environment =
              indexedEnvironmentFromList
                [ AlphaModality :=> Identity (3 :: Int),
                  BetaModality :=> Identity True
                ]
         in reifySectionAssignment
              (modalityRegistryReification environment registry)
              4
              (SectionAssignment mempty)
              @?= Just 14
    , testCase "modalityRegistryKeys preserves the closed modality universe order" $
        let registry :: ModalityRegistry TestModalityKey (Anchor OccurrenceId) () ()
            registry =
              modalityRegistryFromList
                [ BetaModality :=>
                    obstructionModality emptyRelationProjectionPolicy
                      (\startingId _ -> (startingId, mempty)),
                  AlphaModality :=>
                    obstructionModality emptyRelationProjectionPolicy
                      (\startingId _ -> (startingId, mempty))
                ]
         in case DMap.toAscList (modalityRegistryKeys registry) of
              [AlphaModality :=> _, BetaModality :=> _] -> pure ()
              otherKeys ->
                assertFailure
                  ("expected Alpha/Beta key order, got " <> show (length otherKeys) <> " keys")
    , testCase "modalityRegistryProjection keeps capability relational and other relations structural" $
        case
          modalityRegistryProjection
            ( modalityRegistryFromList
                [ AlphaModality :=>
                    obstructionModality emptyRelationProjectionPolicy (\startingId _ -> (startingId, mempty)),
                  BetaModality :=>
                    obstructionModality
                      (relationProjectionPolicyFor CapabilityFlavor RelationalProjection)
                      (\startingId _ -> (startingId, mempty))
                ]
            ) of
          Left conflicts ->
            assertFailure ("expected projection policy to validate, got conflicts: " <> show conflicts)
          Right capabilityProjection ->
            let capabilityCoordinates =
                  projectConstraintCoordinates capabilityProjection
                    ( RelationConstraint
                        CapabilityFlavor
                        (ConstraintId 3)
                        [RootAnchor, OccurrenceAnchor (OccurrenceId 0)]
                        [[FiniteLabelCode 1, FiniteLabelCode 2]]
                    )
                factCoordinates =
                  projectConstraintCoordinates capabilityProjection
                    ( RelationConstraint
                        FactFlavor
                        (ConstraintId 4)
                        [RootAnchor, OccurrenceAnchor (OccurrenceId 0)]
                        [[ClassLabelCode 7, ClassLabelCode 11]]
                    )
             in assertEqual
                  "capability is separated while fact remains structural"
                  ( [RelationCoordinate CapabilityFlavor RootAnchor, RelationCoordinate CapabilityFlavor (OccurrenceAnchor (OccurrenceId 0))],
                    [StructuralCoordinate RootAnchor, StructuralCoordinate (OccurrenceAnchor (OccurrenceId 0))]
                  )
                  (capabilityCoordinates, factCoordinates)
    , testCase "modalityRegistryProjectionConflicts detects conflicting relation projections" $
        modalityRegistryProjectionConflicts
          ( modalityRegistryFromList
              [ AlphaModality :=>
                  obstructionModality
                    (relationProjectionPolicyFor FactFlavor StructuralProjection)
                    (\startingId _ -> (startingId, mempty)),
                BetaModality :=>
                  obstructionModality
                    (relationProjectionPolicyFor FactFlavor RelationalProjection)
                    (\startingId _ -> (startingId, mempty))
              ]
          )
          @?= [RelationProjectionConflict FactFlavor StructuralProjection RelationalProjection]
    ]
