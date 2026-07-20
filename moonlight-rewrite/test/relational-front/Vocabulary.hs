{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

module Vocabulary
  ( AtomKind (..),
    OutcomeKind (..),
    EntityKind (..),
    StatusKind (..),
    ComboKind (..),
    AtlasContextKind (..),
    VocabularyFamily (..),
    VocabularyId (..),
    VocabularyLabel (..),
    VocabularyDecodeError (..),
    ComboSig (..),
    allAtomKinds,
    allOutcomeKinds,
    allEntityKinds,
    allStatusKinds,
    allComboKinds,
    allAtlasContextKinds,
    atomKindVocabularyId,
    atomKindVocabularyLabel,
    outcomeKindVocabularyId,
    entityKindVocabularyId,
    statusKindVocabularyId,
    comboKindVocabularyId,
    atlasContextKindVocabularyId,
    decodeAtomKind,
    decodeOutcomeKind,
    decodeEntityKind,
    decodeStatusKind,
    decodeComboKind,
    decodeAtlasContextKind,
    atlasContextName,
    atlasContextRawName,
    atom,
    outcome,
    entity,
    target,
    status,
    fuse,
    combo,
    fire,
    oil,
    ice,
    cold,
    lightning,
    moonlight,
    mirror,
    blood,
    bone,
    salt,
    ash,
    steam,
    fog,
    explosion,
    prism,
    sanguineGate,
    wardingCircle,
    wraith,
    void,
    blackMirror,
    player,
    bonePile,
    goblin,
    dragon,
    wet,
    haunted,
    shocked,
    warded,
    scorched,
  )
where

import Data.List qualified as List
import GHC.TypeLits
  ( Symbol,
  )
import Moonlight.Rewrite
  ( ContextName,
    ContextNameError,
    HTraversable (..),
    RewriteSignature (..),
    Term,
    contextName,
    deriveRewriteSignature,
    node,
  )

newtype VocabularyId = VocabularyId
  { vocabularyIdText :: String
  }
  deriving stock (Eq, Show)

newtype VocabularyLabel = VocabularyLabel
  { vocabularyLabelText :: String
  }

data VocabularyFamily
  = AtomVocabulary
  | OutcomeVocabulary
  | EntityVocabulary
  | StatusVocabulary
  | ComboVocabulary
  | ContextVocabulary
  deriving stock (Eq, Show)

data VocabularyDecodeError
  = UnknownVocabularyId !VocabularyFamily !VocabularyId
  deriving stock (Eq, Show)

data AtomKind
  = AtomFire
  | AtomOil
  | AtomIce
  | AtomCold
  | AtomLightning
  | AtomMoonlight
  | AtomMirror
  | AtomBlood
  | AtomBone
  | AtomSalt
  | AtomAsh
  deriving stock (Eq, Ord, Show)

data OutcomeKind
  = OutcomeSteam
  | OutcomeFog
  | OutcomeExplosion
  | OutcomePrism
  | OutcomeSanguineGate
  | OutcomeWardingCircle
  | OutcomeWraith
  | OutcomeVoid
  | OutcomeBlackMirror
  deriving stock (Eq, Ord, Show)

data EntityKind
  = EntityPlayer
  | EntityBonePile
  | EntityGoblin
  | EntityDragon
  deriving stock (Eq, Ord, Show)

data StatusKind
  = StatusWet
  | StatusHaunted
  | StatusShocked
  | StatusWarded
  | StatusScorched
  deriving stock (Eq, Ord, Show)

data ComboKind
  = ComboGraveyardWraith
  deriving stock (Eq, Ord, Show)

data AtlasContextKind
  = RainContext
  | UnderwaterContext
  | GraveyardContext
  | EclipseContext
  deriving stock (Eq, Show)

data ComboSig (result :: Symbol) r where
  Atom :: AtomKind -> ComboSig "world" r
  Outcome :: OutcomeKind -> ComboSig "world" r
  Entity :: EntityKind -> ComboSig "world" r
  Target :: r "world" -> ComboSig "world" r
  Status :: StatusKind -> r "world" -> ComboSig "world" r
  Fuse :: r "world" -> r "world" -> ComboSig "world" r
  Combo :: ComboKind -> r "world" -> ComboSig "world" r

$(deriveRewriteSignature ''ComboSig)

allAtomKinds :: [AtomKind]
allAtomKinds =
  [ AtomFire,
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

allOutcomeKinds :: [OutcomeKind]
allOutcomeKinds =
  [ OutcomeSteam,
    OutcomeFog,
    OutcomeExplosion,
    OutcomePrism,
    OutcomeSanguineGate,
    OutcomeWardingCircle,
    OutcomeWraith,
    OutcomeVoid,
    OutcomeBlackMirror
  ]

allEntityKinds :: [EntityKind]
allEntityKinds =
  [ EntityPlayer,
    EntityBonePile,
    EntityGoblin,
    EntityDragon
  ]

allStatusKinds :: [StatusKind]
allStatusKinds =
  [ StatusWet,
    StatusHaunted,
    StatusShocked,
    StatusWarded,
    StatusScorched
  ]

allComboKinds :: [ComboKind]
allComboKinds =
  [ ComboGraveyardWraith
  ]

allAtlasContextKinds :: [AtlasContextKind]
allAtlasContextKinds =
  [ RainContext,
    UnderwaterContext,
    GraveyardContext,
    EclipseContext
  ]

atomKindVocabularyId :: AtomKind -> VocabularyId
atomKindVocabularyId =
  \case
    AtomFire ->
      VocabularyId "fire"

    AtomOil ->
      VocabularyId "oil"

    AtomIce ->
      VocabularyId "ice"

    AtomCold ->
      VocabularyId "cold"

    AtomLightning ->
      VocabularyId "lightning"

    AtomMoonlight ->
      VocabularyId "moonlight"

    AtomMirror ->
      VocabularyId "mirror"

    AtomBlood ->
      VocabularyId "blood"

    AtomBone ->
      VocabularyId "bone"

    AtomSalt ->
      VocabularyId "salt"

    AtomAsh ->
      VocabularyId "ash"

atomKindVocabularyLabel :: AtomKind -> VocabularyLabel
atomKindVocabularyLabel =
  \case
    AtomFire ->
      VocabularyLabel "Fire"

    AtomOil ->
      VocabularyLabel "Oil"

    AtomIce ->
      VocabularyLabel "Ice"

    AtomCold ->
      VocabularyLabel "Cold"

    AtomLightning ->
      VocabularyLabel "Lightning"

    AtomMoonlight ->
      VocabularyLabel "Moonlight"

    AtomMirror ->
      VocabularyLabel "Mirror"

    AtomBlood ->
      VocabularyLabel "Blood"

    AtomBone ->
      VocabularyLabel "Bone"

    AtomSalt ->
      VocabularyLabel "Salt"

    AtomAsh ->
      VocabularyLabel "Ash"

outcomeKindVocabularyId :: OutcomeKind -> VocabularyId
outcomeKindVocabularyId =
  \case
    OutcomeSteam ->
      VocabularyId "steam"

    OutcomeFog ->
      VocabularyId "fog"

    OutcomeExplosion ->
      VocabularyId "explosion"

    OutcomePrism ->
      VocabularyId "prism"

    OutcomeSanguineGate ->
      VocabularyId "sanguine-gate"

    OutcomeWardingCircle ->
      VocabularyId "warding-circle"

    OutcomeWraith ->
      VocabularyId "wraith"

    OutcomeVoid ->
      VocabularyId "void"

    OutcomeBlackMirror ->
      VocabularyId "black-mirror"

entityKindVocabularyId :: EntityKind -> VocabularyId
entityKindVocabularyId =
  \case
    EntityPlayer ->
      VocabularyId "player"

    EntityBonePile ->
      VocabularyId "bone-pile"

    EntityGoblin ->
      VocabularyId "goblin"

    EntityDragon ->
      VocabularyId "dragon"

statusKindVocabularyId :: StatusKind -> VocabularyId
statusKindVocabularyId =
  \case
    StatusWet ->
      VocabularyId "wet"

    StatusHaunted ->
      VocabularyId "haunted"

    StatusShocked ->
      VocabularyId "shocked"

    StatusWarded ->
      VocabularyId "warded"

    StatusScorched ->
      VocabularyId "scorched"

comboKindVocabularyId :: ComboKind -> VocabularyId
comboKindVocabularyId =
  \case
    ComboGraveyardWraith ->
      VocabularyId "graveyard-wraith"

atlasContextKindVocabularyId :: AtlasContextKind -> VocabularyId
atlasContextKindVocabularyId =
  \case
    RainContext ->
      VocabularyId "rain"

    UnderwaterContext ->
      VocabularyId "underwater"

    GraveyardContext ->
      VocabularyId "graveyard"

    EclipseContext ->
      VocabularyId "eclipse"

decodeAtomKind :: VocabularyId -> Either VocabularyDecodeError AtomKind
decodeAtomKind =
  decodeVocabularyId AtomVocabulary atomKindVocabularyId allAtomKinds

decodeOutcomeKind :: VocabularyId -> Either VocabularyDecodeError OutcomeKind
decodeOutcomeKind =
  decodeVocabularyId OutcomeVocabulary outcomeKindVocabularyId allOutcomeKinds

decodeEntityKind :: VocabularyId -> Either VocabularyDecodeError EntityKind
decodeEntityKind =
  decodeVocabularyId EntityVocabulary entityKindVocabularyId allEntityKinds

decodeStatusKind :: VocabularyId -> Either VocabularyDecodeError StatusKind
decodeStatusKind =
  decodeVocabularyId StatusVocabulary statusKindVocabularyId allStatusKinds

decodeComboKind :: VocabularyId -> Either VocabularyDecodeError ComboKind
decodeComboKind =
  decodeVocabularyId ComboVocabulary comboKindVocabularyId allComboKinds

decodeAtlasContextKind :: VocabularyId -> Either VocabularyDecodeError AtlasContextKind
decodeAtlasContextKind =
  decodeVocabularyId ContextVocabulary atlasContextKindVocabularyId allAtlasContextKinds

atlasContextName :: AtlasContextKind -> Either ContextNameError ContextName
atlasContextName =
  contextName . atlasContextRawName

atlasContextRawName :: AtlasContextKind -> String
atlasContextRawName =
  vocabularyIdText . atlasContextKindVocabularyId

fire, oil, ice, cold, lightning, moonlight, mirror, blood, bone, salt, ash :: Term ComboSig "world"
fire =
  atom AtomFire

oil =
  atom AtomOil

ice =
  atom AtomIce

cold =
  atom AtomCold

lightning =
  atom AtomLightning

moonlight =
  atom AtomMoonlight

mirror =
  atom AtomMirror

blood =
  atom AtomBlood

bone =
  atom AtomBone

salt =
  atom AtomSalt

ash =
  atom AtomAsh

steam, fog, explosion, prism, sanguineGate, wardingCircle, wraith, void, blackMirror :: Term ComboSig "world"
steam =
  outcome OutcomeSteam

fog =
  outcome OutcomeFog

explosion =
  outcome OutcomeExplosion

prism =
  outcome OutcomePrism

sanguineGate =
  outcome OutcomeSanguineGate

wardingCircle =
  outcome OutcomeWardingCircle

wraith =
  outcome OutcomeWraith

void =
  outcome OutcomeVoid

blackMirror =
  outcome OutcomeBlackMirror

player, bonePile, goblin, dragon :: Term ComboSig "world"
player =
  entity EntityPlayer

bonePile =
  entity EntityBonePile

goblin =
  entity EntityGoblin

dragon =
  entity EntityDragon

wet, haunted, shocked, warded, scorched :: Term ComboSig "world" -> Term ComboSig "world"
wet =
  status StatusWet

haunted =
  status StatusHaunted

shocked =
  status StatusShocked

warded =
  status StatusWarded

scorched =
  status StatusScorched

decodeVocabularyId ::
  VocabularyFamily ->
  (value -> VocabularyId) ->
  [value] ->
  VocabularyId ->
  Either VocabularyDecodeError value
decodeVocabularyId vocabularyFamily encodeValue values valueId =
  case List.find ((== valueId) . encodeValue) values of
    Nothing ->
      Left (UnknownVocabularyId vocabularyFamily valueId)

    Just value ->
      Right value
