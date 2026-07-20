{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.TestFixture.Site
  ( SampleContext (..),
    SampleSystem (..),
    SampleSiteTag,
    constantRestrictionModel,
    featureRestrictionModel,
    nodeCellKey,
    sampleSystem,
    sampleGrothendieckSite,
    sampleNerveSite,
    sourceInterfaceStalk,
    targetInterfaceStalk,
  )
where

import Control.Monad (foldM)
import Data.Kind (Type)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Monoid (Any (..))
import Data.Set qualified as Set
import Moonlight.Algebra (JoinSemilattice (..), Lattice, MeetSemilattice (..))
import Moonlight.Category
  ( FinCat,
    FinGeneratorId (..),
    FinMor,
    FinMorphismId (..),
    FinObj,
    FinObjectId (..),
    allMorphisms,
    allObjects,
    chainMorphisms,
    chainStartObject,
    composeMor,
    finMorId,
    finMorSourceId,
    finMorTargetId,
    finObjId,
    identity,
    sampleFinCat,
    source,
    target,
  )
import Moonlight.Sheaf.Section.Linearize
  ( constantRestrictionLinearization,
  )
import Moonlight.Sheaf.Site.Class
  ( Site (..),
    coveringFamilyFromTargetedWitnesses,
  )
import Moonlight.Sheaf.Site.Context
  ( ContextArrow (..),
    ContextCover,
    ContextCoverBasis (..),
    allContextMorphisms,
    composeContextArrows,
    identityContextArrow,
    pullbackContextSquare,
  )
import Moonlight.Sheaf.Site.Context.Pairs (ContextPairStrategy (ExhaustivePairs))
import Moonlight.Sheaf.Site.Context.Presentation
  ( ContextPresentationSystem (..),
    contextPresentationWith,
  )
import Moonlight.Sheaf.Site.Grothendieck
  ( GrothendieckSite,
    mkGrothendieckSite,
  )
import Moonlight.Sheaf.Site.Construction.Nerve
  ( CellKey (..),
    nerveCellKey,
    NerveSite,
    NerveSiteAlgebra (..),
    mkNerveSite,
    nerveSiteCells,
  )
import Moonlight.Sheaf.Site.Interface.Types
  ( InterfaceDirectionEstimate (..),
    InterfaceMeasure (..),
    InterfaceName,
    MorphismInterface (..),
    interfaceNameFromString,
  )
import Moonlight.Sheaf.Site.Stalk.Interface
  ( CompositionWitness (..),
    InterfaceDomain (..),
    InterfaceStalk (..),
  )
import Moonlight.Sheaf.Site.Stalk.Interface.Linearization
  ( LinearizedRestrictionModel,
    buildLinearizedRestrictionModel,
    interfaceStalkBasisLinearization,
  )
import Moonlight.Sheaf.Site.System
  ( AnalyzableSystem (..),
  )
import Moonlight.Category.Simplicial (NerveSimplex, nerve, nerveSimplexChain)
import Moonlight.Category.Simplicial (TruncatedNormalizedSSet)
import Numeric.Natural (Natural)

type SampleSiteTag :: Type
data SampleSiteTag

type SampleContext :: Type
data SampleContext
  = RootCtx
  | LeftCtx
  | RightCtx
  | MeetCtx
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type SampleSystem :: Type
data SampleSystem = SampleSystem
  { ssCategory :: FinCat
  }

sampleSystem :: SampleSystem
sampleSystem =
  SampleSystem sampleFinCat

sampleValidObject :: FinObj
sampleValidObject =
  case allObjects sampleFinCat of
    [] -> error "sampleFinCat fixture: no objects"
    objectValue : _ -> objectValue

sampleValidMorphism :: FinMor
sampleValidMorphism =
  case allMorphisms sampleFinCat of
    [] -> error "sampleFinCat fixture: no morphisms"
    morphismValue : _ -> morphismValue

instance JoinSemilattice SampleContext where
  join leftContext rightContext =
    case (leftContext, rightContext) of
      (MeetCtx, otherContext) -> otherContext
      (otherContext, MeetCtx) -> otherContext
      (RootCtx, _) -> RootCtx
      (_, RootCtx) -> RootCtx
      (LeftCtx, LeftCtx) -> LeftCtx
      (RightCtx, RightCtx) -> RightCtx
      (LeftCtx, RightCtx) -> RootCtx
      (RightCtx, LeftCtx) -> RootCtx

instance MeetSemilattice SampleContext where
  meet leftContext rightContext =
    case (leftContext, rightContext) of
      (RootCtx, otherContext) -> otherContext
      (otherContext, RootCtx) -> otherContext
      (MeetCtx, _) -> MeetCtx
      (_, MeetCtx) -> MeetCtx
      (LeftCtx, LeftCtx) -> LeftCtx
      (RightCtx, RightCtx) -> RightCtx
      (LeftCtx, RightCtx) -> MeetCtx
      (RightCtx, LeftCtx) -> MeetCtx

instance Lattice SampleContext

instance NerveSiteAlgebra SampleSiteTag where
  type NerveCategory SampleSiteTag = FinCat
  type NerveSource SampleSiteTag = FinObj
  type NerveMorphism SampleSiteTag = FinMor

  buildSiteNerve :: FinCat -> Natural -> TruncatedNormalizedSSet (NerveSimplex FinCat)
  buildSiteNerve = nerve

  simplexSourceValue =
    chainStartObject . nerveSimplexChain

  simplexMorphismChain =
    chainMorphisms . nerveSimplexChain

instance InterfaceDomain SampleSiteTag where
  type InterfaceObject SampleSiteTag = FinObj
  type InterfaceMorphism SampleSiteTag = FinMor
  type InterfaceComposeError SampleSiteTag = ()

  measureObject objectValue =
    InterfaceMeasure
      { imBoundNames = Set.singleton (sampleInterfaceName ("obj-" <> show (finObjId objectValue))),
        imDeletedNames = Set.empty,
        imCreatedNames = Set.empty,
        imGuarded = Any False
      }

  measureMorphism =
    interfaceMeasureFromMorphism

  composeMorphismChain morphismValues =
    composeMorphismChainInCategory @SampleSiteTag (ssCategory sampleSystem) morphismValues

  composeMorphismChainInCategory categoryValue morphismValues =
    case morphismValues of
      [] ->
        Left ()
      firstMorphism : restMorphisms ->
        foldM composeStep firstMorphism restMorphisms
    where
      composeStep :: FinMor -> FinMor -> Either () FinMor
      composeStep accumulatedMorphism nextMorphism =
        either
          (const (Left ()))
          Right
          (composeMor categoryValue nextMorphism accumulatedMorphism)

instance AnalyzableSystem SampleSystem where
  type SystemTag SampleSystem = SampleSiteTag
  type SystemOb SampleSystem = FinObj
  type SystemMor SampleSystem = FinMor
  type SystemCtx SampleSystem = SampleContext
  type SystemMismatch SampleSystem = ()

  allContexts _ =
    [RootCtx, LeftCtx, RightCtx, MeetCtx]

  contextLeq _ leftContext rightContext =
    case (leftContext, rightContext) of
      (MeetCtx, _) -> True
      (LeftCtx, LeftCtx) -> True
      (LeftCtx, RootCtx) -> True
      (RightCtx, RightCtx) -> True
      (RightCtx, RootCtx) -> True
      (RootCtx, RootCtx) -> True
      _ -> False

  systemObjectsInContext currentSystem _ =
    allObjects (ssCategory currentSystem)

  systemMorphismsInContext currentSystem _ =
    allMorphisms (ssCategory currentSystem)

  restrictObject _ _ _ =
    Just

  restrictMorphism _ _ _ =
    Just

  identityMorphism currentSystem _ objectValue =
    case identity (ssCategory currentSystem) objectValue of
      Right morphismValue -> morphismValue
      Left _ -> sampleValidMorphism

  morphismSource currentSystem morphismValue =
    case source (ssCategory currentSystem) morphismValue of
      Right objectValue -> objectValue
      Left _ -> sampleValidObject

  morphismTarget currentSystem morphismValue =
    case target (ssCategory currentSystem) morphismValue of
      Right objectValue -> objectValue
      Left _ -> sampleValidObject

  composeMorphisms currentSystem _ leftMorphism rightMorphism =
    either
      (const (Left ()))
      Right
      (composeMor (ssCategory currentSystem) leftMorphism rightMorphism)

  morphismInterface _ =
    morphismInterfaceFromMorphism

  normalizeMorphism _ _ =
    id

instance ContextPresentationSystem SampleSystem where
  systemContextPresentation currentSystem =
    contextPresentationWith currentSystem [RootCtx] ExhaustivePairs

instance ContextCoverBasis SampleSystem where
  contextCoversAt _currentSystem contextValue =
    case contextValue of
      RootCtx ->
        [staticContextCover RootCtx (LeftCtx :| [RightCtx])]
      LeftCtx ->
        [staticContextCover LeftCtx (MeetCtx :| [])]
      RightCtx ->
        [staticContextCover RightCtx (MeetCtx :| [])]
      MeetCtx ->
        []

instance Site SampleSystem where
  type SiteObject SampleSystem = SampleContext
  type SiteMorphism SampleSystem = ContextArrow SampleContext

  siteObjects =
    allContexts

  siteMorphisms =
    allContextMorphisms

  identityAt _system =
    identityContextArrow

  coversAt =
    contextCoversAt

  composeChecked _ =
    composeContextArrows

  pullbackPair =
    pullbackContextSquare

staticContextCover ::
  SampleContext ->
  NonEmpty SampleContext ->
  ContextCover SampleSystem
staticContextCover targetContext sourceContexts =
  coveringFamilyFromTargetedWitnesses
    targetContext
    (fmap (, ContextArrow) sourceContexts)

sampleNerveSite :: NerveSite SampleSiteTag
sampleNerveSite =
  mkNerveSite @SampleSiteTag (ssCategory sampleSystem) 2

sampleGrothendieckSite :: GrothendieckSite SampleSystem
sampleGrothendieckSite =
  mkGrothendieckSite sampleSystem 2

nodeCellKey :: NerveSite tag -> FinObjectId -> Maybe CellKey
nodeCellKey siteValue (FinObjectId ordinalValue) =
  fmap nerveCellKey
    (find ((== ordinalValue) . ckOrdinal . nerveCellKey) (nerveSiteCells siteValue))

sampleInterfaceName :: String -> InterfaceName SampleSiteTag
sampleInterfaceName =
  interfaceNameFromString

sourceInterfaceStalk :: InterfaceStalk SampleSiteTag
sourceInterfaceStalk =
  InterfaceStalk
    { rsBoundNames = Set.fromList (fmap sampleInterfaceName ["x", "y"]),
      rsDeletedNames = Set.singleton (sampleInterfaceName "z"),
      rsCreatedNames = Set.empty,
      rsGuarded = True,
      rsWitness = TerminalWitness,
      rsCellDimension = 2
    }

targetInterfaceStalk :: InterfaceStalk SampleSiteTag
targetInterfaceStalk =
  InterfaceStalk
    { rsBoundNames = Set.singleton (sampleInterfaceName "y"),
      rsDeletedNames = Set.empty,
      rsCreatedNames = Set.singleton (sampleInterfaceName "w"),
      rsGuarded = True,
      rsWitness = TerminalWitness,
      rsCellDimension = 1
    }

featureRestrictionModel :: LinearizedRestrictionModel String Int
featureRestrictionModel =
  buildLinearizedRestrictionModel
    (Map.fromList [("source", sourceInterfaceStalk), ("target", targetInterfaceStalk)])
    (\upperNode lowerNode -> upperNode == lowerNode || (upperNode, lowerNode) == ("source", "target"))
    interfaceStalkBasisLinearization

constantRestrictionModel :: LinearizedRestrictionModel String Int
constantRestrictionModel =
  buildLinearizedRestrictionModel
    (Map.fromList [("upper", sourceInterfaceStalk), ("lower", targetInterfaceStalk)])
    (\upperNode lowerNode -> upperNode == lowerNode || (upperNode, lowerNode) == ("upper", "lower"))
    (constantRestrictionLinearization 1)

interfaceMeasureFromMorphism :: FinMor -> InterfaceMeasure SampleSiteTag
interfaceMeasureFromMorphism morphismValue =
  let morphismInterfaceValue = morphismInterfaceFromMorphism morphismValue
   in InterfaceMeasure
        { imBoundNames = Set.singleton (sampleInterfaceName ("mor-" <> show (finMorId morphismValue))),
          imDeletedNames = miDeletedNames morphismInterfaceValue,
          imCreatedNames = miCreatedNames morphismInterfaceValue,
          imGuarded = Any (miGuarded morphismInterfaceValue)
        }

morphismInterfaceFromMorphism :: FinMor -> MorphismInterface SampleSiteTag
morphismInterfaceFromMorphism morphismValue =
  MorphismInterface
    { miBoundNames = Set.singleton (sampleInterfaceName ("mor-" <> show (finMorId morphismValue))),
      miDeletedNames = Set.singleton (sampleInterfaceName ("src-" <> show (finMorSourceId morphismValue))),
      miCreatedNames = Set.singleton (sampleInterfaceName ("dst-" <> show (finMorTargetId morphismValue))),
      miGuarded =
        case finMorId morphismValue of
          FinGeneratorMorphismId (FinGeneratorId 12) -> True
          _ -> False,
      miDirectionEstimate =
        InterfaceDirectionEstimate
          (unFinObjectId (finMorTargetId morphismValue) - unFinObjectId (finMorSourceId morphismValue))
    }
