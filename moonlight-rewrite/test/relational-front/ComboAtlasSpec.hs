{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

module ComboAtlasSpec
  ( comboAtlasTests,
  )
where

import Data.Maybe
  ( mapMaybe,
  )
import Data.List qualified as List
import Moonlight.Control.Schedule
  ( ScheduleOrder (BackoffByGroup),
    SchedulerConfig (..),
    TracePolicy (TraceAll),
    backoffConfig,
    defaultSchedulerConfig,
  )
import Data.Fix
  ( Fix (..),
  )
import Moonlight.Core.Pattern.AntiUnify
  ( BinaryLGGResult (..),
    antiUnifyTerms,
  )
import Moonlight.Pale.Test.Site.Assertion (expectRight)
import Moonlight.Rewrite
  ( ApplyRejection (..),
    ApplyResult (..),
    ApplyStatus (..),
    ClassId,
    ContextName,
    HTraversable (..),
    Host,
    HostBuildError,
    HostTerm (..),
    K (..),
    Match,
    MatchQuery (..),
    NoGuardAtom,
    Program,
    RelationalProgramError,
    RewriteTarget (..),
    RuleName,
    RuleNameError,
    Rules,
    SaturationConfig (..),
    SaturationRound (..),
    SaturationResult (..),
    Term,
    apply,
    at,
    bind,
    compile,
    context,
    defaultApplyConfig,
    defaultSaturationConfig,
    Engine,
    engineHost,
    extension,
    forbids_,
    forall_,
    hostCanonicalClass,
    hostClassWitness,
    hostFromTerms,
    hostLookupTermClass,
    hostNodeClasses,
    match,
    matchGeneration,
    nodeChildren,
    prepare,
    prettyHostBuildError,
    prettyRelationalProgramError,
    program,
    rule,
    mkRuleName,
    saturate,
    setContext,
    replaceHost,
    symbolToken,
    var,
    contextNameString,
    (==>),
  )
import Moonlight.Rewrite.DSL
  ( Node (..),
  )
import Moonlight.Rewrite.ProofContext
  ( ProofRetention (..),
    pcsTotalSteps,
    proofClassWitnesses,
    proofRelated,
    summarizeProofLog,
  )
import Vocabulary
import Moonlight.Rewrite.Relational
  ( Limit (..),
    RewriteRunConfig (..),
    RewriteRunMetrics (..),
    defaultRewriteRunLimits,
    defaultRewriteRunConfig,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertBool,
    assertFailure,
    testCase,
    (@?=),
  )

comboAtlasTests :: TestTree
comboAtlasTests =
  testGroup
    "ComboAtlas saturation"
    [ testCase "exposes a closed primitive magic vocabulary" closedPrimitiveVocabulary,
      testCase "DSL elaborates anchored NACs with extension-local existentials" applicationConditionDslCompiles,
      testGroup
        "The Ward Beats the Fireball"
        [ testCase "scorches an unwarded target" unwardedFireballScorchesTarget,
          testCase "blocks a target with an existing anchored ward" anchoredWardBlocksFireball,
          testCase "does not let a ward on another target block globally" wardOnAnotherTargetDoesNotBlock,
          testCase "applies a same-round fireball against the round-start snapshot" sameRoundFireballAppliesAgainstSnapshot,
          testCase "rejects a stale match after same-revision host replacement" staleMatchRejectedAfterHostReplacement,
          testCase "revalidates a stale match that still exists after in-engine host growth" staleMatchRevalidatedAfterHostGrowth,
          testCase "treats a second pass over a warded result as a fixed point" wardedResultSecondPassIsFixedPoint,
          testCase "continues after a scheduler-suppressed empty round" backoffSchedulerSuppressionIsNotFixedPoint
        ],
      testCase "derives local physics combos, retains witnesses, and mines a motif" localPhysicsSaturation,
      testCase "keeps rain-local physics scoped" rainContextSaturation,
      testCase "renders a discovered combo atlas with mined archetypes and context legality" discoveredComboAtlas
    ]

closedPrimitiveVocabulary :: IO ()
closedPrimitiveVocabulary = do
  allAtomKinds
    @?= [ AtomFire,
          AtomOil,
          AtomIce,
          AtomCold,
          AtomLightning,
          AtomMoonlight,
          AtomMirror,
          AtomBlood,
          AtomBone,
          AtomSalt,
          AtomAsh
        ]

  allOutcomeKinds
    @?= [ OutcomeSteam,
          OutcomeFog,
          OutcomeExplosion,
          OutcomePrism,
          OutcomeSanguineGate,
          OutcomeWardingCircle,
          OutcomeWraith,
          OutcomeVoid,
          OutcomeBlackMirror
        ]

  allStatusKinds
    @?= [ StatusWet,
          StatusHaunted,
          StatusShocked,
          StatusWarded,
          StatusScorched
        ]

  allEntityKinds
    @?= [ EntityPlayer,
          EntityBonePile,
          EntityGoblin,
          EntityDragon
        ]

  allComboKinds
    @?= [ ComboGraveyardWraith
        ]

  fmap atlasContextRawName allAtlasContextKinds
    @?= [ "rain",
          "underwater",
          "graveyard",
          "eclipse"
        ]

  fmap (vocabularyLabelText . atomKindVocabularyLabel) allAtomKinds
    @?= [ "Fire",
          "Oil",
          "Ice",
          "Cold",
          "Lightning",
          "Moonlight",
          "Mirror",
          "Blood",
          "Bone",
          "Salt",
          "Ash"
        ]

  traverse (decodeAtomKind . atomKindVocabularyId) allAtomKinds
    @?= Right allAtomKinds

  traverse (decodeOutcomeKind . outcomeKindVocabularyId) allOutcomeKinds
    @?= Right allOutcomeKinds

  traverse (decodeEntityKind . entityKindVocabularyId) allEntityKinds
    @?= Right allEntityKinds

  traverse (decodeStatusKind . statusKindVocabularyId) allStatusKinds
    @?= Right allStatusKinds

  traverse (decodeComboKind . comboKindVocabularyId) allComboKinds
    @?= Right allComboKinds

  traverse (decodeAtlasContextKind . atlasContextKindVocabularyId) allAtlasContextKinds
    @?= Right allAtlasContextKinds

  decodeAtomKind (VocabularyId "soup")
    @?= Left (UnknownVocabularyId AtomVocabulary (VocabularyId "soup"))

  assertUniqueVocabularyIds "atom ids" (fmap atomKindVocabularyId allAtomKinds)
  assertUniqueVocabularyIds "outcome ids" (fmap outcomeKindVocabularyId allOutcomeKinds)
  assertUniqueVocabularyIds "entity ids" (fmap entityKindVocabularyId allEntityKinds)
  assertUniqueVocabularyIds "status ids" (fmap statusKindVocabularyId allStatusKinds)
  assertUniqueVocabularyIds "combo ids" (fmap comboKindVocabularyId allComboKinds)
  assertUniqueVocabularyIds "context ids" (fmap atlasContextKindVocabularyId allAtlasContextKinds)

applicationConditionDslCompiles :: IO ()
applicationConditionDslCompiles =
  case compile applicationConditionProgram of
    Left programError ->
      assertFailure
        ( "application-condition DSL program should compile, but produced: "
            <> prettyRelationalProgramError programError
        )

    Right _ ->
      pure ()

unwardedFireballScorchesTarget :: IO ()
unwardedFireballScorchesTarget = do
  result <-
    runWardFireballSaturation
      [fireballAt goblin]

  assertHostContains
    "scorched goblin"
    (saturationHost result)
    (scorchedTarget goblin)

  totalExecutedRewrites result @?= 1

anchoredWardBlocksFireball :: IO ()
anchoredWardBlocksFireball = do
  result <-
    runWardFireballSaturation
      [ fireballAt goblin,
        wardedTarget goblin
      ]

  assertHostOmits
    "scorched goblin"
    (saturationHost result)
    (scorchedTarget goblin)

  totalExecutedRewrites result @?= 0

wardOnAnotherTargetDoesNotBlock :: IO ()
wardOnAnotherTargetDoesNotBlock = do
  result <-
    runWardFireballSaturation
      [ fireballAt goblin,
        wardedTarget dragon
      ]

  assertHostContains
    "scorched goblin"
    (saturationHost result)
    (scorchedTarget goblin)

  totalExecutedRewrites result @?= 1

sameRoundFireballAppliesAgainstSnapshot :: IO ()
sameRoundFireballAppliesAgainstSnapshot = do
  result <-
    runWardFireballSaturation
      [ raiseWardAt goblin,
        fireballAt goblin
      ]

  assertHostContains
    "warded goblin"
    (saturationHost result)
    (wardedTarget goblin)

  assertHostContains
    "scorched goblin admitted by the round-start snapshot"
    (saturationHost result)
    (scorchedTarget goblin)

  totalExecutedRewrites result @?= 2
  pcsTotalSteps (summarizeProofLog (saturationProofs result)) @?= 2

staleMatchRejectedAfterHostReplacement :: IO ()
staleMatchRejectedAfterHostReplacement = do
  staleCase <-
    prepareStaleFireballCase
      [fireballAt goblin]

  (replacementHost, _roots) <-
    expectRightHost (hostFromTerms [])

  let staleMatch =
        sfcMatch staleCase

      engineWithReplacement =
        replaceHost replacementHost (sfcEngine staleCase)

  (_engineAfterApply, applyResult) <-
    expectRightProgram
      (apply defaultApplyConfig staleMatch engineWithReplacement)

  case applyResultStatus applyResult of
    ApplyRejected (RejectedTargetGeneration observedGeneration replacementGeneration) -> do
      observedGeneration @?= matchGeneration staleMatch
      assertBool
        "host replacement must rotate the engine-owned generation"
        (replacementGeneration /= observedGeneration)

    otherStatus ->
      assertFailure ("expected replaced-target rejection, got " <> show otherStatus)

staleMatchRevalidatedAfterHostGrowth :: IO ()
staleMatchRevalidatedAfterHostGrowth = do
  staleCase <-
    prepareStaleFireballCase
      [fireballAt goblin, raiseWardAt dragon]

  raiseWardRuleName <-
    expectRightRuleName (mkRuleName "raise-ward")

  (engineAfterWardMatch, wardMatches) <-
    expectRightProgram
      ( match
          MatchQuery
            { matchQueryTarget = RewriteBase,
              matchQueryRule = raiseWardRuleName,
              matchQueryRoot = Nothing
            }
          (sfcEngine staleCase)
      )

  wardMatch <-
    case wardMatches of
      matchValue : _ ->
        pure matchValue

      [] ->
        assertFailure "expected a raise-ward match"

  (engineWithGrowth, wardApplyResult) <-
    expectRightProgram (apply defaultApplyConfig wardMatch engineAfterWardMatch)

  case applyResultStatus wardApplyResult of
    ApplyExecuted _ changed ->
      assertBool "raise-ward should grow the live host" changed

    otherStatus ->
      assertFailure ("expected raise-ward to execute, got " <> show otherStatus)

  (engineAfterApply, applyResult) <-
    expectRightProgram
      (apply defaultApplyConfig (sfcMatch staleCase) engineWithGrowth)

  case applyResultStatus applyResult of
    ApplyExecuted _ changed ->
      assertBool "revalidated stale fireball should change the grown host" changed

    otherStatus ->
      assertFailure ("expected stale match to revalidate and execute, but got " <> show otherStatus)

  assertHostContains
    "scorched goblin after stale revalidation"
    (engineHost engineAfterApply)
    (scorchedTarget goblin)

wardedResultSecondPassIsFixedPoint :: IO ()
wardedResultSecondPassIsFixedPoint = do
  firstResult <-
    runWardFireballSaturation
      [ raiseWardAt goblin,
        fireballAt goblin
      ]

  compiledProgram <-
    expectRightProgram (compile wardFireballProgram)

  secondResult <-
    expectRightProgram
      (runBaseSaturation wardFireballSaturationConfig compiledProgram (saturationHost firstResult))

  totalExecutedRewrites secondResult @?= 0

backoffSchedulerSuppressionIsNotFixedPoint :: IO ()
backoffSchedulerSuppressionIsNotFixedPoint = do
  compiledProgram <-
    expectRightProgram (compile wardFireballProgram)

  (host0, _roots) <-
    expectRightHost
      ( hostFromTerms
          (fmap HostTerm [fireballAt goblin, fireballAt dragon])
      )

  result <-
    expectRightProgram
      (runBaseSaturation backoffWardFireballSaturationConfig compiledProgram host0)

  assertHostContains
    "scorched goblin"
    (saturationHost result)
    (scorchedTarget goblin)

  assertHostContains
    "scorched dragon"
    (saturationHost result)
    (scorchedTarget dragon)

  totalExecutedRewrites result @?= 2
  assertBool
    "expected scheduler trace evidence for backoff suppression"
    (not (null (saturationSchedulerTrace result)))

assertUniqueVocabularyIds :: String -> [VocabularyId] -> IO ()
assertUniqueVocabularyIds label vocabularyIds =
  assertBool
    ("expected unique " <> label)
    (List.nub vocabularyIds == vocabularyIds)

localPhysicsSaturation :: IO ()
localPhysicsSaturation = do
  (host0, _roots) <-
    expectRightHost comboHost

  igniteRoot <-
    expectClass host0 (fuse fire oil)

  nestedRoot <-
    expectClass host0 (fuse (fuse fire ice) cold)

  compiledProgram <-
    expectRightProgram (compile comboProgram)

  result <-
    expectRightProgram
      (runBaseSaturation comboSaturationConfig compiledProgram host0)

  explosionClass <-
    expectClass (saturationHost result) explosion

  fogClass <-
    expectClass (saturationHost result) fog

  hostCanonicalClass (saturationHost result) igniteRoot
    @?= Just explosionClass

  hostCanonicalClass (saturationHost result) nestedRoot
    @?= Just fogClass

  proofRelated igniteRoot explosionClass (saturationProofs result)
    @?= Right True

  proofRelated nestedRoot fogClass (saturationProofs result)
    @?= Right True

  assertBool
    "expected at least three retained proof steps"
    (pcsTotalSteps (summarizeProofLog (saturationProofs result)) >= 3)

  proofClassWitnesses fogClass (saturationProofs result)
    `shouldSatisfyNonEmpty` "expected fog class to retain a proof witness"

  sourceWitnessA <-
    expectWitness (saturationHost result) igniteRoot

  sourceWitnessB <-
    expectWitness (saturationHost result) nestedRoot

  assertBool
    "expected anti-unification to recover shared combo structure"
    (binaryLggSharedStructure (antiUnifyTerms sourceWitnessA sourceWitnessB) > 0)

rainContextSaturation :: IO ()
rainContextSaturation = do
  rainContext <-
    expectAtlasContextName RainContext

  (host0, _roots) <-
    expectRightHost comboHost

  lightningRoot <-
    expectClass host0 (fuse lightning (target player))

  boneLightningRoot <-
    expectClass host0 (fuse lightning (target bonePile))

  compiledProgram <-
    expectRightProgram (compile comboProgram)

  baseResult <-
    expectRightProgram
      (runBaseSaturation comboSaturationConfig compiledProgram host0)

  hostLookupTermClass (shocked (target player)) (saturationHost baseResult)
    @?= Right Nothing

  hostLookupTermClass (shocked (target bonePile)) (saturationHost baseResult)
    @?= Right Nothing

  rainResult <-
    expectRightProgram
      ( runContextSaturation
          comboSaturationConfig
          rainContext
          compiledProgram
          host0
          host0
      )

  shockedClass <-
    expectClass (saturationHost rainResult) (shocked (target player))

  shockedBoneClass <-
    expectClass (saturationHost rainResult) (shocked (target bonePile))

  hostCanonicalClass (saturationHost rainResult) lightningRoot
    @?= Just shockedClass

  hostCanonicalClass (saturationHost rainResult) boneLightningRoot
    @?= Just shockedBoneClass

  proofRelated lightningRoot shockedClass (saturationProofs rainResult)
    @?= Right True

  proofRelated boneLightningRoot shockedBoneClass (saturationProofs rainResult)
    @?= Right True

discoveredComboAtlas :: IO ()
discoveredComboAtlas = do
  rainContext <-
    expectAtlasContextName RainContext
  graveyardContext <-
    expectAtlasContextName GraveyardContext
  eclipseContext <-
    expectAtlasContextName EclipseContext

  (host0, _roots) <-
    expectRightHost comboHost

  compiledProgram <-
    expectRightProgram (compile comboProgram)

  baseResult <-
    expectRightProgram
      (runBaseSaturation comboSaturationConfig compiledProgram host0)

  rainResult <-
    expectRightProgram
      ( runContextSaturation
          comboSaturationConfig
          rainContext
          compiledProgram
          host0
          host0
      )

  graveyardResult <-
    expectRightProgram
      ( runContextSaturation
          comboSaturationConfig
          graveyardContext
          compiledProgram
          host0
          host0
      )

  eclipseResult <-
    expectRightProgram
      ( runContextSaturation
          comboSaturationConfig
          eclipseContext
          compiledProgram
          host0
          host0
      )

  atlas <-
    expectRight
      ( discoverAtlas
          (comboAtlasMiningConfig rainContext graveyardContext eclipseContext)
          [ SaturatedAtlasWorld AtlasBase baseResult,
            SaturatedAtlasWorld (AtlasContext rainContext) rainResult,
            SaturatedAtlasWorld (AtlasContext graveyardContext) graveyardResult,
            SaturatedAtlasWorld (AtlasContext eclipseContext) eclipseResult
          ]
      )

  fmap (atlasDiscoveryStableId . atlasDiscovery) (atlasCards atlas)
    @?= [ AtlasDiscoveryId "combustion-burst",
          AtlasDiscoveryId "phase-chain",
          AtlasDiscoveryId "conductive-shock",
          AtlasDiscoveryId "astral-blood-gate",
          AtlasDiscoveryId "ossuary-ward",
          AtlasDiscoveryId "graveyard-wraith",
          AtlasDiscoveryId "eclipse-black-mirror"
        ]

  atlasCards atlas
    @?= [ AtlasCard
            { atlasDiscovery = CombustionBurst,
              atlasArchetype = FuseSourceMotif 1,
              atlasLegalWorlds = [AtlasBase],
              atlasBlockedWorlds = []
            },
          AtlasCard
            { atlasDiscovery = PhaseChain,
              atlasArchetype = FuseSourceMotif 2,
              atlasLegalWorlds = [AtlasBase],
              atlasBlockedWorlds = []
            },
          AtlasCard
            { atlasDiscovery = ConductiveShock,
              atlasArchetype = ContextPreparedCatalyst,
              atlasLegalWorlds = [AtlasContext rainContext],
              atlasBlockedWorlds = [AtlasBase]
            },
          AtlasCard
            { atlasDiscovery = AstralBloodGate,
              atlasArchetype = FuseSourceMotif 2,
              atlasLegalWorlds = [AtlasBase],
              atlasBlockedWorlds = []
            },
          AtlasCard
            { atlasDiscovery = OssuaryWard,
              atlasArchetype = FuseSourceMotif 2,
              atlasLegalWorlds = [AtlasBase],
              atlasBlockedWorlds = []
            },
          AtlasCard
            { atlasDiscovery = GraveyardWraith,
              atlasArchetype = ContextPreparedCatalyst,
              atlasLegalWorlds = [AtlasContext graveyardContext],
              atlasBlockedWorlds = [AtlasBase]
            },
          AtlasCard
            { atlasDiscovery = EclipseBlackMirror,
              atlasArchetype = ContextPreparedCatalyst,
              atlasLegalWorlds = [AtlasContext eclipseContext],
              atlasBlockedWorlds = [AtlasBase]
            }
        ]

  renderAtlas atlas
    @?= [ "Combustion Burst | archetype: shared Fuse(_, _) source (1 shared node) | legal: base",
          "Phase Chain | archetype: shared Fuse(_, _) source (2 shared nodes) | legal: base",
          "Conductive Shock | archetype: context prepares target, catalyst triggers status | legal: rain | blocked: base",
          "Astral Blood Gate | archetype: shared Fuse(_, _) source (2 shared nodes) | legal: base",
          "Ossuary Ward | archetype: shared Fuse(_, _) source (2 shared nodes) | legal: base",
          "Graveyard Wraith | archetype: context prepares target, catalyst triggers status | legal: graveyard | blocked: base",
          "Eclipse Black Mirror | archetype: context prepares target, catalyst triggers status | legal: eclipse | blocked: base"
        ]

comboProgram :: Program ComboSig NoGuardAtom
comboProgram =
  program $ do
    context (atlasContextRawName RainContext)
    context (atlasContextRawName UnderwaterContext)
    context (atlasContextRawName GraveyardContext)
    context (atlasContextRawName EclipseContext)

    rule "ignite-oil" $
      fuse fire oil ==> explosion

    rule "melt-ice" $
      fuse fire ice ==> steam

    rule "cold-steam" $
      fuse steam cold ==> fog

    rule "refract-moon" $
      fuse moonlight mirror ==> prism

    rule "open-sanguine-gate" $
      fuse prism blood ==> sanguineGate

    rule "raise-ashbone" $
      fuse ash bone ==> wraith

    rule "salt-wraith" $
      fuse wraith salt ==> wardingCircle

    rule "rain-wets-target" $
      forall_
        (bind (symbolToken @"x") (symbolToken @"world"))
        ( at (atlasContextRawName RainContext) $
            target (var (symbolToken @"x") (symbolToken @"world")) ==> wet (target (var (symbolToken @"x") (symbolToken @"world")))
        )

    rule "shock-wet-target" $
      forall_
        (bind (symbolToken @"x") (symbolToken @"world"))
        ( fuse lightning (wet (target (var (symbolToken @"x") (symbolToken @"world")))) ==> shocked (target (var (symbolToken @"x") (symbolToken @"world")))
        )

    rule "graveyard-haunts-bone" $
      at (atlasContextRawName GraveyardContext) $
        bone ==> haunted bone

    rule "grave-moon-wraith" $
      fuse moonlight (haunted bone) ==> combo ComboGraveyardWraith wraith

    rule "eclipse-drinks-moon" $
      at (atlasContextRawName EclipseContext) $
        moonlight ==> void

    rule "eclipse-black-mirror" $
      fuse void mirror ==> blackMirror

applicationConditionProgram :: Program ComboSig NoGuardAtom
applicationConditionProgram =
  program $
    rule "shock-clean-target" $
      forall_
        (bind (symbolToken @"x") (symbolToken @"world") <> bind (symbolToken @"witness") (symbolToken @"world"))
        ( (target (var (symbolToken @"x") (symbolToken @"world")) ==> shocked (target (var (symbolToken @"x") (symbolToken @"world"))))
            `forbids_` extension (fuse (target (var (symbolToken @"x") (symbolToken @"world"))) (var (symbolToken @"witness") (symbolToken @"world")))
        )

wardFireballProgram :: Program ComboSig NoGuardAtom
wardFireballProgram =
  program $ do
    rule "raise-ward" $
      forall_
        (bind (symbolToken @"prey") (symbolToken @"world"))
        (raiseWardAt (var (symbolToken @"prey") (symbolToken @"world")) ==> wardedTarget (var (symbolToken @"prey") (symbolToken @"world")))

    rule "fireball-hits-unwarded" $
      forall_
        (bind (symbolToken @"prey") (symbolToken @"world"))
        ( (fireballAt (var (symbolToken @"prey") (symbolToken @"world")) ==> scorchedTarget (var (symbolToken @"prey") (symbolToken @"world")))
            `forbids_` extension (wardedTarget (var (symbolToken @"prey") (symbolToken @"world")))
        )

fireballAt :: Term ComboSig "world" -> Term ComboSig "world"
fireballAt prey =
  fuse fire (target prey)

raiseWardAt :: Term ComboSig "world" -> Term ComboSig "world"
raiseWardAt prey =
  fuse moonlight (target prey)

wardedTarget :: Term ComboSig "world" -> Term ComboSig "world"
wardedTarget =
  warded . target

scorchedTarget :: Term ComboSig "world" -> Term ComboSig "world"
scorchedTarget =
  scorched . target

expectAtlasContextName :: AtlasContextKind -> IO ContextName
expectAtlasContextName contextKind =
  case atlasContextName contextKind of
    Left errorValue ->
      assertFailure ("invalid atlas context name: " <> show errorValue)

    Right contextNameValue ->
      pure contextNameValue

comboHost :: Either HostBuildError (Host ComboSig, [ClassId])
comboHost =
  hostFromTerms
    (fmap HostTerm comboSeedTerms)

comboSeedTerms :: [Term ComboSig "world"]
comboSeedTerms =
  foldMap fuseSeedSpaceTerms comboSeedSpaces

data FuseSeedSpace = FuseSeedSpace
  { fuseSeedSpaceMaxDepth :: !Int,
    fuseSeedSpaceIngredients :: ![Term ComboSig "world"]
  }

comboSeedSpaces :: [FuseSeedSpace]
comboSeedSpaces =
  [ FuseSeedSpace
      { fuseSeedSpaceMaxDepth = 2,
        fuseSeedSpaceIngredients =
          [ fire,
            oil,
            ice,
            cold
          ]
      },
    FuseSeedSpace
      { fuseSeedSpaceMaxDepth = 2,
        fuseSeedSpaceIngredients =
          [ lightning,
            target player,
            target bonePile
          ]
      },
    FuseSeedSpace
      { fuseSeedSpaceMaxDepth = 2,
        fuseSeedSpaceIngredients =
          [ moonlight,
            mirror,
            blood
          ]
      },
    FuseSeedSpace
      { fuseSeedSpaceMaxDepth = 2,
        fuseSeedSpaceIngredients =
          [ ash,
            bone,
            salt
          ]
      },
    FuseSeedSpace
      { fuseSeedSpaceMaxDepth = 1,
        fuseSeedSpaceIngredients =
          [ moonlight,
            bone
          ]
      }
  ]

fuseSeedSpaceTerms :: FuseSeedSpace -> [Term ComboSig "world"]
fuseSeedSpaceTerms seedSpace =
  boundedFuseSeeds
    (fuseSeedSpaceMaxDepth seedSpace)
    (fuseSeedSpaceIngredients seedSpace)

boundedFuseSeeds :: Int -> [Term ComboSig "world"] -> [Term ComboSig "world"]
boundedFuseSeeds maxDepth ingredients =
  concat (take (max 0 maxDepth) (drop 1 (iterate (fuseLayer ingredients) ingredients)))

fuseLayer :: [Term ComboSig "world"] -> [Term ComboSig "world"] -> [Term ComboSig "world"]
fuseLayer ingredients previousTerms =
  [ fuse leftTerm rightTerm
    | leftTerm <- previousTerms,
      rightTerm <- ingredients
  ]

comboSaturationConfig :: SaturationConfig ComboSig
comboSaturationConfig =
  defaultSaturationConfig
    { scRunConfig =
        defaultRewriteRunConfig
          { rrcLimits =
              defaultRewriteRunLimits
                { rrmRounds = Limit (Just 8),
                  rrmRewriteApplications = Limit (Just 1024)
                }
          },
      scProofRetention = KeepFullProof
    }

wardFireballSaturationConfig :: SaturationConfig ComboSig
wardFireballSaturationConfig =
  comboSaturationConfig

backoffWardFireballSaturationConfig :: SaturationConfig ComboSig
backoffWardFireballSaturationConfig =
  wardFireballSaturationConfig
    { scSchedulerConfig =
        defaultSchedulerConfig
          { scOrder =
              BackoffByGroup
                (backoffConfig 1 1),
            scTracePolicy = TraceAll
          }
    }

data StaleFireballCase = StaleFireballCase
  { sfcEngine :: !(Engine ComboSig NoGuardAtom),
    sfcMatch :: !Match
  }

prepareStaleFireballCase ::
  [Term ComboSig "world"] ->
  IO StaleFireballCase
prepareStaleFireballCase initialTerms = do
  compiledProgram <-
    expectRightProgram (compile wardFireballProgram)

  (host0, _roots) <-
    expectRightHost (hostFromTerms (fmap HostTerm initialTerms))

  fireballRuleName <-
    expectRightRuleName (mkRuleName "fireball-hits-unwarded")

  (engineAfterMatch, matches) <-
    expectRightProgram
      ( match
          MatchQuery
            { matchQueryTarget = RewriteBase,
              matchQueryRule = fireballRuleName,
              matchQueryRoot = Nothing
            }
          (prepare compiledProgram host0)
      )

  case matches of
    fireballMatch : _ ->
      pure
        StaleFireballCase
          { sfcEngine = engineAfterMatch,
            sfcMatch = fireballMatch
          }

    [] ->
      assertFailure "expected at least one fireball match"

runWardFireballSaturation ::
  [Term ComboSig "world"] ->
  IO (SaturationResult ComboSig)
runWardFireballSaturation initialTerms = do
  compiledProgram <-
    expectRightProgram (compile wardFireballProgram)

  (host0, _roots) <-
    expectRightHost (hostFromTerms (fmap HostTerm initialTerms))

  expectRightProgram
    (runBaseSaturation wardFireballSaturationConfig compiledProgram host0)

runBaseSaturation ::
  SaturationConfig ComboSig ->
  Rules ComboSig NoGuardAtom ->
  Host ComboSig ->
  Either (RelationalProgramError ComboSig) (SaturationResult ComboSig)
runBaseSaturation config rulesValue host =
  snd <$> saturate RewriteBase config (prepare rulesValue host)

runContextSaturation ::
  SaturationConfig ComboSig ->
  ContextName ->
  Rules ComboSig NoGuardAtom ->
  Host ComboSig ->
  Host ComboSig ->
  Either (RelationalProgramError ComboSig) (SaturationResult ComboSig)
runContextSaturation config contextNameValue rulesValue baseHost liveHost =
  snd
    <$> saturate
      (RewriteContext contextNameValue)
      config
      (setContext contextNameValue liveHost (prepare rulesValue baseHost))

expectRightHost :: Either HostBuildError a -> IO a
expectRightHost =
  \case
    Left errorValue ->
      assertFailure (prettyHostBuildError errorValue)

    Right value ->
      pure value

expectRightProgram :: Either (RelationalProgramError ComboSig) a -> IO a
expectRightProgram =
  \case
    Left errorValue ->
      assertFailure (prettyRelationalProgramError errorValue)

    Right value ->
      pure value

expectRightRuleName :: Either RuleNameError RuleName -> IO RuleName
expectRightRuleName =
  \case
    Left errorValue ->
      assertFailure ("invalid rewrite rule name: " <> show errorValue)

    Right value ->
      pure value

expectClass :: Host ComboSig -> Term ComboSig "world" -> IO ClassId
expectClass host termValue =
  case hostLookupTermClass termValue host of
    Left errorValue ->
      assertFailure (prettyHostBuildError errorValue)

    Right Nothing ->
      assertFailure "expected term to be present in saturated host"

    Right (Just classId) ->
      pure classId

expectWitness :: Host ComboSig -> ClassId -> IO (Fix (Node ComboSig))
expectWitness host classId =
  case hostClassWitness 32 classId host of
    Nothing ->
      assertFailure "expected saturated class to have a finite witness"

    Just witness ->
      pure witness

assertHostContains :: String -> Host ComboSig -> Term ComboSig "world" -> IO ()
assertHostContains label host termValue =
  case hostLookupTermClass termValue host of
    Left errorValue ->
      assertFailure (prettyHostBuildError errorValue)

    Right Nothing ->
      assertFailure ("expected host to contain " <> label)

    Right (Just _) ->
      pure ()

assertHostOmits :: String -> Host ComboSig -> Term ComboSig "world" -> IO ()
assertHostOmits label host termValue =
  case hostLookupTermClass termValue host of
    Left errorValue ->
      assertFailure (prettyHostBuildError errorValue)

    Right Nothing ->
      pure ()

    Right (Just _) ->
      assertFailure ("expected host not to contain " <> label)

totalExecutedRewrites :: SaturationResult sig -> Int
totalExecutedRewrites =
  sum . fmap (length . saturationRoundExecuted) . saturationRounds

shouldSatisfyNonEmpty :: Either errorValue [value] -> String -> IO ()
shouldSatisfyNonEmpty result message =
  case result of
    Right (_ : _) ->
      pure ()

    _ ->
      assertFailure message

data AtlasWorld
  = AtlasBase
  | AtlasContext !ContextName
  deriving stock (Eq, Show)

newtype AtlasDiscoveryId = AtlasDiscoveryId String
  deriving stock (Eq, Show)

newtype AtlasDiscoveryLabel = AtlasDiscoveryLabel
  { atlasDiscoveryLabelText :: String
  }

data AtlasDiscovery
  = CombustionBurst
  | PhaseChain
  | ConductiveShock
  | AstralBloodGate
  | OssuaryWard
  | GraveyardWraith
  | EclipseBlackMirror
  deriving stock (Eq, Show)

allAtlasDiscoveries :: [AtlasDiscovery]
allAtlasDiscoveries =
  [ CombustionBurst,
    PhaseChain,
    ConductiveShock,
    AstralBloodGate,
    OssuaryWard,
    GraveyardWraith,
    EclipseBlackMirror
  ]

atlasDiscoveryStableId :: AtlasDiscovery -> AtlasDiscoveryId
atlasDiscoveryStableId =
  \case
    CombustionBurst ->
      AtlasDiscoveryId "combustion-burst"

    PhaseChain ->
      AtlasDiscoveryId "phase-chain"

    ConductiveShock ->
      AtlasDiscoveryId "conductive-shock"

    AstralBloodGate ->
      AtlasDiscoveryId "astral-blood-gate"

    OssuaryWard ->
      AtlasDiscoveryId "ossuary-ward"

    GraveyardWraith ->
      AtlasDiscoveryId "graveyard-wraith"

    EclipseBlackMirror ->
      AtlasDiscoveryId "eclipse-black-mirror"

atlasDiscoveryDisplayLabel :: AtlasDiscovery -> AtlasDiscoveryLabel
atlasDiscoveryDisplayLabel =
  \case
    CombustionBurst ->
      AtlasDiscoveryLabel "Combustion Burst"

    PhaseChain ->
      AtlasDiscoveryLabel "Phase Chain"

    ConductiveShock ->
      AtlasDiscoveryLabel "Conductive Shock"

    AstralBloodGate ->
      AtlasDiscoveryLabel "Astral Blood Gate"

    OssuaryWard ->
      AtlasDiscoveryLabel "Ossuary Ward"

    GraveyardWraith ->
      AtlasDiscoveryLabel "Graveyard Wraith"

    EclipseBlackMirror ->
      AtlasDiscoveryLabel "Eclipse Black Mirror"

data AtlasArchetype
  = FuseSourceMotif !Int
  | ContextPreparedCatalyst
  deriving stock (Eq, Show)

newtype ArchetypeMinerId = ArchetypeMinerId String
  deriving stock (Show)

data ArchetypeMiner = ArchetypeMiner
  { archetypeMinerId :: !ArchetypeMinerId,
    runArchetypeMiner ::
      AtlasMiningConfig ->
      [AtlasOutcome] ->
      AtlasDiscovery ->
      [AtlasWorld] ->
      Either AtlasBuildError (Maybe AtlasArchetype)
  }

data AtlasMiningConfig = AtlasMiningConfig
  { amcBaseWorld :: !AtlasWorld,
    amcWorldOrder :: ![AtlasWorld],
    amcClassifyNode :: !(Node ComboSig ClassId -> Maybe AtlasDiscovery),
    amcArchetypeMiners :: ![ArchetypeMiner]
  }

data AtlasCard = AtlasCard
  { atlasDiscovery :: !AtlasDiscovery,
    atlasArchetype :: !AtlasArchetype,
    atlasLegalWorlds :: ![AtlasWorld],
    atlasBlockedWorlds :: ![AtlasWorld]
  }
  deriving stock (Eq, Show)

newtype ComboAtlas = ComboAtlas
  { atlasCards :: [AtlasCard]
  }

data SaturatedAtlasWorld = SaturatedAtlasWorld
  { saturatedAtlasWorld :: !AtlasWorld,
    saturatedAtlasResult :: !(SaturationResult ComboSig)
  }

data AtlasOutcome = AtlasOutcome
  { atlasOutcomeWorld :: !AtlasWorld,
    atlasOutcomeDiscovery :: !AtlasDiscovery,
    atlasOutcomeClass :: !ClassId,
    atlasOutcomeSourceWitness :: !(Maybe (Fix (Node ComboSig)))
  }

data AtlasBuildError
  = AtlasNoArchetypeMined !AtlasDiscovery ![ArchetypeMinerId]
  deriving stock (Show)

comboAtlasMiningConfig :: ContextName -> ContextName -> ContextName -> AtlasMiningConfig
comboAtlasMiningConfig rainContext graveyardContext eclipseContext =
  AtlasMiningConfig
    { amcBaseWorld = AtlasBase,
      amcWorldOrder =
        [ AtlasBase,
          AtlasContext rainContext,
          AtlasContext graveyardContext,
          AtlasContext eclipseContext
        ],
      amcClassifyNode = classifyComboDiscoveryNode,
      amcArchetypeMiners = comboArchetypeMiners
    }

classifyComboDiscoveryNode :: Node ComboSig ClassId -> Maybe AtlasDiscovery
classifyComboDiscoveryNode =
  \case
    Node (Outcome OutcomeExplosion) ->
      Just CombustionBurst

    Node (Outcome OutcomeFog) ->
      Just PhaseChain

    Node (Status StatusShocked _) ->
      Just ConductiveShock

    Node (Outcome OutcomeSanguineGate) ->
      Just AstralBloodGate

    Node (Outcome OutcomeWardingCircle) ->
      Just OssuaryWard

    Node (Combo ComboGraveyardWraith _) ->
      Just GraveyardWraith

    Node (Outcome OutcomeBlackMirror) ->
      Just EclipseBlackMirror

    _ ->
      Nothing

sharedFuseMotifMinerId :: ArchetypeMinerId
sharedFuseMotifMinerId =
  ArchetypeMinerId "shared-fuse-motif"

contextPreparedCatalystMinerId :: ArchetypeMinerId
contextPreparedCatalystMinerId =
  ArchetypeMinerId "context-prepared-catalyst"

comboArchetypeMiners :: [ArchetypeMiner]
comboArchetypeMiners =
  [ contextPreparedCatalystMiner,
    sharedFuseMotifMiner
  ]

contextPreparedCatalystMiner :: ArchetypeMiner
contextPreparedCatalystMiner =
  ArchetypeMiner
    { archetypeMinerId = contextPreparedCatalystMinerId,
      runArchetypeMiner =
        \config _outcomes _discovery legalWorlds ->
          if amcBaseWorld config `notElem` legalWorlds
            && any isContextWorld legalWorlds
            then Right (Just ContextPreparedCatalyst)
            else Right Nothing
    }

sharedFuseMotifMiner :: ArchetypeMiner
sharedFuseMotifMiner =
  ArchetypeMiner
    { archetypeMinerId = sharedFuseMotifMinerId,
      runArchetypeMiner =
        \_config outcomes discovery _legalWorlds -> do
          let ownSources =
                outcomeSourceWitnesses
                  (\atlasOutcome -> atlasOutcomeDiscovery atlasOutcome == discovery)
                  outcomes
              otherSources =
                outcomeSourceWitnesses
                  (\atlasOutcome -> atlasOutcomeDiscovery atlasOutcome /= discovery)
                  outcomes
              sharedCounts =
                [ binaryLggSharedStructure (antiUnifyTerms ownSource otherSource)
                  | ownSource <- ownSources,
                    otherSource <- otherSources,
                    fuseRootedWitness ownSource,
                    fuseRootedWitness otherSource
                ]
          case bestPositive sharedCounts of
            Nothing ->
              Right Nothing

            Just sharedStructure ->
              Right (Just (FuseSourceMotif sharedStructure))
    }

outcomeSourceWitnesses ::
  (AtlasOutcome -> Bool) ->
  [AtlasOutcome] ->
  [Fix (Node ComboSig)]
outcomeSourceWitnesses acceptOutcome =
  mapMaybe
    (\atlasOutcome -> if acceptOutcome atlasOutcome then atlasOutcomeSourceWitness atlasOutcome else Nothing)

bestPositive :: [Int] -> Maybe Int
bestPositive =
  foldr step Nothing
  where
    step :: Int -> Maybe Int -> Maybe Int
    step value bestValue
      | value <= 0 =
          bestValue
      | otherwise =
          case bestValue of
            Nothing ->
              Just value

            Just currentBest ->
              Just (max value currentBest)

isContextWorld :: AtlasWorld -> Bool
isContextWorld =
  \case
    AtlasBase ->
      False

    AtlasContext _ ->
      True

discoverAtlas ::
  AtlasMiningConfig ->
  [SaturatedAtlasWorld] ->
  Either AtlasBuildError ComboAtlas
discoverAtlas config worlds = do
  outcomes <-
    fmap concat (traverse (scanAtlasWorld config) worlds)
  ComboAtlas <$> traverse (mineAtlasCard config outcomes) (discoveredAtlasOrder config outcomes)

scanAtlasWorld ::
  AtlasMiningConfig ->
  SaturatedAtlasWorld ->
  Either AtlasBuildError [AtlasOutcome]
scanAtlasWorld config saturatedWorld =
  pure
    [ AtlasOutcome
        { atlasOutcomeWorld = saturatedAtlasWorld saturatedWorld,
          atlasOutcomeDiscovery = discovery,
          atlasOutcomeClass = classId,
          atlasOutcomeSourceWitness = bestFuseSourceWitness host nodes
        }
      | (classId, nodes) <- hostNodeClasses host,
        nodeValue <- nodes,
        Just discovery <- [amcClassifyNode config nodeValue]
    ]
  where
    host =
      saturationHost (saturatedAtlasResult saturatedWorld)

discoveredAtlasOrder ::
  AtlasMiningConfig ->
  [AtlasOutcome] ->
  [AtlasDiscovery]
discoveredAtlasOrder _config outcomes =
  filter
    (\discovery -> any ((== discovery) . atlasOutcomeDiscovery) outcomes)
    allAtlasDiscoveries

mineAtlasCard ::
  AtlasMiningConfig ->
  [AtlasOutcome] ->
  AtlasDiscovery ->
  Either AtlasBuildError AtlasCard
mineAtlasCard config outcomes discovery = do
  archetype <-
    mineAtlasArchetype config outcomes discovery legalWorlds
  pure
    AtlasCard
      { atlasDiscovery = discovery,
        atlasArchetype = archetype,
        atlasLegalWorlds = legalWorlds,
        atlasBlockedWorlds = blockedWorlds
      }
  where
    presentWorlds =
      atlasOutcomeWorld <$> filter ((== discovery) . atlasOutcomeDiscovery) outcomes
    legalWorlds =
      summarizeLegalWorlds config presentWorlds
    blockedWorlds =
      summarizeBlockedWorlds config presentWorlds

mineAtlasArchetype ::
  AtlasMiningConfig ->
  [AtlasOutcome] ->
  AtlasDiscovery ->
  [AtlasWorld] ->
  Either AtlasBuildError AtlasArchetype
mineAtlasArchetype config outcomes discovery legalWorlds = do
  minedArchetypes <-
    traverse
      (\miner -> runArchetypeMiner miner config outcomes discovery legalWorlds)
      (amcArchetypeMiners config)
  case firstJust minedArchetypes of
    Nothing ->
      Left
        ( AtlasNoArchetypeMined
            discovery
            (fmap archetypeMinerId (amcArchetypeMiners config))
        )

    Just archetype ->
      Right archetype

summarizeLegalWorlds :: AtlasMiningConfig -> [AtlasWorld] -> [AtlasWorld]
summarizeLegalWorlds config presentWorlds
  | amcBaseWorld config `elem` presentWorlds =
      [amcBaseWorld config]
  | otherwise =
      filter (`elem` presentWorlds) (amcWorldOrder config)

summarizeBlockedWorlds :: AtlasMiningConfig -> [AtlasWorld] -> [AtlasWorld]
summarizeBlockedWorlds config presentWorlds
  | amcBaseWorld config `elem` presentWorlds =
      []
  | otherwise =
      [amcBaseWorld config]

firstJust :: [Maybe value] -> Maybe value
firstJust =
  \case
    [] ->
      Nothing

    Nothing : values ->
      firstJust values

    Just value : _ ->
      Just value

bestFuseSourceWitness ::
  Host ComboSig ->
  [Node ComboSig ClassId] ->
  Maybe (Fix (Node ComboSig))
bestFuseSourceWitness host nodes =
  bestBy fuseDepth
    ( mapMaybe
        (nodeWitness host)
        (filter fuseRootedNode nodes)
    )

nodeWitness ::
  Host ComboSig ->
  Node ComboSig ClassId ->
  Maybe (Fix (Node ComboSig))
nodeWitness host (Node sigNode) =
  Fix . Node
    <$> htraverse
      (\(K classId) -> K <$> hostClassWitness 32 classId host)
      sigNode

bestBy :: Ord score => (value -> score) -> [value] -> Maybe value
bestBy score =
  foldr step Nothing
  where
    step value bestValue =
      case bestValue of
        Nothing ->
          Just value

        Just currentBest
          | score value > score currentBest ->
              Just value
          | otherwise ->
              bestValue

fuseRootedNode :: Node ComboSig classId -> Bool
fuseRootedNode =
  \case
    Node (Fuse _ _) ->
      True

    _ ->
      False

fuseRootedWitness :: Fix (Node ComboSig) -> Bool
fuseRootedWitness =
  \case
    Fix (Node (Fuse _ _)) ->
      True

    _ ->
      False

fuseDepth :: Fix (Node ComboSig) -> Int
fuseDepth (Fix (Node sigNode)) =
  case sigNode of
    Fuse _ _ ->
      childFuseDepth + 1

    _ ->
      childFuseDepth
  where
    childFuseDepth =
      foldr (max . fuseDepth) 0 (nodeChildren sigNode)

renderAtlas :: ComboAtlas -> [String]
renderAtlas =
  fmap renderAtlasCard . atlasCards

renderAtlasCard :: AtlasCard -> String
renderAtlasCard card =
  renderDiscovery (atlasDiscovery card)
    <> " | archetype: "
    <> renderArchetype (atlasArchetype card)
    <> " | legal: "
    <> renderWorlds (atlasLegalWorlds card)
    <> renderBlockedWorlds (atlasBlockedWorlds card)

renderBlockedWorlds :: [AtlasWorld] -> String
renderBlockedWorlds worlds =
  case worlds of
    [] ->
      ""

    _ ->
      " | blocked: " <> renderWorlds worlds

renderDiscovery :: AtlasDiscovery -> String
renderDiscovery =
  atlasDiscoveryLabelText . atlasDiscoveryDisplayLabel

renderArchetype :: AtlasArchetype -> String
renderArchetype =
  \case
    FuseSourceMotif sharedStructure ->
      "shared Fuse(_, _) source (" <> show sharedStructure <> " shared " <> sharedNodeWord sharedStructure <> ")"

    ContextPreparedCatalyst ->
      "context prepares target, catalyst triggers status"

sharedNodeWord :: Int -> String
sharedNodeWord sharedStructure
  | sharedStructure == 1 =
      "node"
  | otherwise =
      "nodes"

renderWorlds :: [AtlasWorld] -> String
renderWorlds =
  \case
    [] ->
      "none"

    worlds ->
      List.intercalate ", " (fmap renderWorld worlds)

renderWorld :: AtlasWorld -> String
renderWorld =
  \case
    AtlasBase ->
      "base"

    AtlasContext contextName ->
      contextNameString contextName
