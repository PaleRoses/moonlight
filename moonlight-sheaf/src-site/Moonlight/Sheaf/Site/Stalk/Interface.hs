{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.Sheaf.Site.Stalk.Interface
  ( CompositionWitness (..),
    FaceStalkProjectionError (..),
    InterfaceDomain (..),
    InterfaceMismatch (..),
    InterfaceStalk (..),
    InterfaceStalkSignature (..),
    WitnessClass (..),
    interfaceStalkAlgebra,
    interfaceStalkExactEq,
    interfaceStalkSignature,
    grothendieckStalkFromCell,
    reducedFaceMorphisms,
    stalkFromCell,
    stalkFromSourceAndMorphisms,
    targetStalkForFace,
    projectInterfaceFaceMorphisms,
    witnessClass,
  )
where

import Data.Kind (Constraint, Type)
import Data.List (unsnoc)
import Data.Maybe (mapMaybe)
import Data.Monoid (Any (..))
import Data.Set qualified as Set
import Moonlight.Category (chainMorphisms, chainStartObject)
import Moonlight.Sheaf.Site.Grothendieck
  ( GrothendieckCell,
    grothendieckCellSimplex,
    gmTargetMorphism,
    goValue,
    grothendieckCellDimension,
  )
import Moonlight.Sheaf.Site.Construction.Nerve
  ( CellKey (..),
    FaceKind (..),
    FaceMorphism,
    faceMorphismKind,
    faceMorphismSource,
    faceMorphismTarget,
    NerveCell,
    nerveCellKey,
    nerveCellSimplex,
    NerveMorphism,
    NerveSiteAlgebra (..),
    NerveSource,
  )
import Moonlight.Sheaf.Section.Stalk
  ( StalkAlgebra (..),
    StalkRestrictionKernel (..),
    mismatchObstruction,
  )
import Moonlight.Sheaf.Site.Interface.Types
  ( InterfaceMeasure (..),
    InterfaceName,
  )
import Moonlight.Sheaf.Site.System (AnalyzableSystem (..), SystemTag)
import Moonlight.Category.Simplicial (nerveSimplexChain)
import Numeric.Natural (Natural)

type CompositionWitness :: Type -> Type
data CompositionWitness tag
  = TerminalWitness
  | ComposedWitness (InterfaceMorphism tag)
  | ObstructedWitness (InterfaceComposeError tag)

deriving stock instance
  (Eq (InterfaceMorphism tag), Eq (InterfaceComposeError tag)) =>
  Eq (CompositionWitness tag)

type WitnessClass :: Type
data WitnessClass
  = WitnessTerminal
  | WitnessComposed
  | WitnessObstructed
  deriving stock (Eq, Ord, Show)

witnessClass :: CompositionWitness tag -> WitnessClass
witnessClass witnessValue =
  case witnessValue of
    TerminalWitness -> WitnessTerminal
    ComposedWitness {} -> WitnessComposed
    ObstructedWitness {} -> WitnessObstructed

deriving stock instance
  (Show (InterfaceMorphism tag), Show (InterfaceComposeError tag)) =>
  Show (CompositionWitness tag)

type InterfaceMismatch :: Type
data InterfaceMismatch
  = BoundNamesMismatch
  | DeletedNamesMismatch
  | CreatedNamesMismatch
  | GuardedMismatch
  | CellDimensionMismatch
  | WitnessClassMismatch
  | WitnessValueMismatch
  deriving stock (Eq, Ord, Show)

type InterfaceStalk :: Type -> Type
data InterfaceStalk tag = InterfaceStalk
  { rsBoundNames :: Set.Set (InterfaceName tag),
    rsDeletedNames :: Set.Set (InterfaceName tag),
    rsCreatedNames :: Set.Set (InterfaceName tag),
    rsGuarded :: Bool,
    rsWitness :: CompositionWitness tag,
    rsCellDimension :: Int
  }

type InterfaceStalkSignature :: Type -> Type
data InterfaceStalkSignature tag = InterfaceStalkSignature
  { issBoundNames :: Set.Set (InterfaceName tag),
    issDeletedNames :: Set.Set (InterfaceName tag),
    issCreatedNames :: Set.Set (InterfaceName tag),
    issGuarded :: Bool,
    issWitnessClass :: WitnessClass,
    issCellDimension :: Int
  }
  deriving stock (Eq, Ord, Show)

interfaceStalkSignature :: InterfaceStalk tag -> InterfaceStalkSignature tag
interfaceStalkSignature stalkValue =
  InterfaceStalkSignature
    { issBoundNames = rsBoundNames stalkValue,
      issDeletedNames = rsDeletedNames stalkValue,
      issCreatedNames = rsCreatedNames stalkValue,
      issGuarded = rsGuarded stalkValue,
      issWitnessClass = witnessClass (rsWitness stalkValue),
      issCellDimension = rsCellDimension stalkValue
    }

interfaceStalkExactEq ::
  (Eq (InterfaceMorphism tag), Eq (InterfaceComposeError tag)) =>
  InterfaceStalk tag ->
  InterfaceStalk tag ->
  Bool
interfaceStalkExactEq leftStalk rightStalk =
  ( rsBoundNames leftStalk,
    rsDeletedNames leftStalk,
    rsCreatedNames leftStalk,
    rsGuarded leftStalk,
    rsWitness leftStalk,
    rsCellDimension leftStalk
  )
    ==
  ( rsBoundNames rightStalk,
    rsDeletedNames rightStalk,
    rsCreatedNames rightStalk,
    rsGuarded rightStalk,
    rsWitness rightStalk,
    rsCellDimension rightStalk
  )

instance Show (InterfaceStalk tag) where
  show =
    show . interfaceStalkSignature

type InterfaceDomain :: Type -> Constraint
class InterfaceDomain tag where
  type InterfaceObject tag
  type InterfaceMorphism tag
  type InterfaceComposeError tag

  measureObject :: InterfaceObject tag -> InterfaceMeasure tag
  measureMorphism :: InterfaceMorphism tag -> InterfaceMeasure tag
  composeMorphismChain :: [InterfaceMorphism tag] -> Either (InterfaceComposeError tag) (InterfaceMorphism tag)
  composeMorphismChainInCategory :: NerveCategory tag -> [InterfaceMorphism tag] -> Either (InterfaceComposeError tag) (InterfaceMorphism tag)
  composeMorphismChainInCategory _ morphismValues = composeMorphismChain @tag morphismValues

interfaceStalkAlgebra ::
  (Eq (InterfaceMorphism tag), Eq (InterfaceComposeError tag)) =>
  StalkAlgebra witness (InterfaceStalk tag) InterfaceMismatch ()
interfaceStalkAlgebra =
  StalkAlgebra
    { saRestrictionKernel = const StalkRestrictionIdentity,
      saMismatches = interfaceStalkMismatches,
      saMerge =
        \leftStalk rightStalk ->
          case mismatchObstruction (interfaceStalkMismatches leftStalk rightStalk) of
            Just obstruction -> Left obstruction
            Nothing ->
              Right
                InterfaceStalk
                  { rsBoundNames = rsBoundNames leftStalk <> rsBoundNames rightStalk,
                    rsDeletedNames = rsDeletedNames leftStalk <> rsDeletedNames rightStalk,
                    rsCreatedNames = rsCreatedNames leftStalk <> rsCreatedNames rightStalk,
                    rsGuarded = rsGuarded leftStalk || rsGuarded rightStalk,
                    rsWitness = mergeWitness (rsWitness leftStalk) (rsWitness rightStalk),
                    rsCellDimension = max (rsCellDimension leftStalk) (rsCellDimension rightStalk)
                  },
      saRepair = const (Left ()),
      saNormalize = id
    }

interfaceStalkMismatches ::
  (Eq (InterfaceMorphism tag), Eq (InterfaceComposeError tag)) =>
  InterfaceStalk tag ->
  InterfaceStalk tag ->
  [InterfaceMismatch]
interfaceStalkMismatches leftStalk rightStalk =
  let leftSignature = interfaceStalkSignature leftStalk
      rightSignature = interfaceStalkSignature rightStalk
   in [BoundNamesMismatch | issBoundNames leftSignature /= issBoundNames rightSignature]
        <> [DeletedNamesMismatch | issDeletedNames leftSignature /= issDeletedNames rightSignature]
        <> [CreatedNamesMismatch | issCreatedNames leftSignature /= issCreatedNames rightSignature]
        <> [GuardedMismatch | issGuarded leftSignature /= issGuarded rightSignature]
        <> [CellDimensionMismatch | issCellDimension leftSignature /= issCellDimension rightSignature]
        <> witnessMismatches (rsWitness leftStalk) (rsWitness rightStalk)

witnessMismatches ::
  (Eq (InterfaceMorphism tag), Eq (InterfaceComposeError tag)) =>
  CompositionWitness tag ->
  CompositionWitness tag ->
  [InterfaceMismatch]
witnessMismatches leftWitness rightWitness
  | witnessClass leftWitness /= witnessClass rightWitness =
      [WitnessClassMismatch]
  | leftWitness /= rightWitness =
      [WitnessValueMismatch]
  | otherwise =
      []

stalkFromCell ::
  forall tag.
  ( NerveSiteAlgebra tag,
    InterfaceDomain tag,
    NerveSource tag ~ InterfaceObject tag,
    NerveMorphism tag ~ InterfaceMorphism tag
  ) =>
  NerveCategory tag ->
  NerveCell tag ->
  InterfaceStalk tag
stalkFromCell categoryValue cellValue =
  stalkFromSourceAndMorphisms
    categoryValue
    (simplexSourceValue @tag (nerveCellSimplex cellValue))
    (simplexMorphismChain @tag (nerveCellSimplex cellValue))
    (nerveCellDimensionInt cellValue)

type FaceStalkProjectionError :: Type -> Type
data FaceStalkProjectionError tag
  = FaceStalkProjectionInnerFaceIndexOutOfRange Natural
  | FaceStalkProjectionAdjacentCompositionFailed Natural (InterfaceComposeError tag)

deriving stock instance Eq (InterfaceComposeError tag) => Eq (FaceStalkProjectionError tag)

deriving stock instance
  Show (InterfaceComposeError tag) =>
  Show (FaceStalkProjectionError tag)

targetStalkForFace ::
  forall tag.
  ( NerveSiteAlgebra tag,
    InterfaceDomain tag,
    NerveSource tag ~ InterfaceObject tag,
    NerveMorphism tag ~ InterfaceMorphism tag
  ) =>
  NerveCategory tag ->
  FaceMorphism tag ->
  Either (FaceStalkProjectionError tag) (InterfaceStalk tag)
targetStalkForFace categoryValue faceMorphism =
  let targetCell = faceMorphismTarget faceMorphism
   in fmap
        ( \restrictedMorphisms ->
            stalkFromSourceAndMorphisms
              categoryValue
              (simplexSourceValue @tag (nerveCellSimplex targetCell))
              restrictedMorphisms
              (nerveCellDimensionInt targetCell)
        )
        (reducedFaceMorphisms @tag categoryValue faceMorphism)

reducedFaceMorphisms ::
  forall tag.
  ( NerveSiteAlgebra tag,
    InterfaceDomain tag,
    NerveMorphism tag ~ InterfaceMorphism tag
  ) =>
  NerveCategory tag ->
  FaceMorphism tag ->
  Either (FaceStalkProjectionError tag) [NerveMorphism tag]
reducedFaceMorphisms categoryValue faceMorphism =
  let morphismValues = simplexMorphismChain @tag (nerveCellSimplex (faceMorphismSource faceMorphism))
   in case faceMorphismKind faceMorphism of
        LeadingFace -> projectInterfaceFaceMorphisms @tag categoryValue 0 morphismValues
        TrailingFace -> projectInterfaceFaceMorphisms @tag categoryValue (fromIntegral (length morphismValues)) morphismValues
        InnerFace innerIndexValue -> composeAdjacentMorphisms @tag categoryValue innerIndexValue morphismValues

projectInterfaceFaceMorphisms ::
  forall tag.
  InterfaceDomain tag =>
  NerveCategory tag ->
  Natural ->
  [InterfaceMorphism tag] ->
  Either (FaceStalkProjectionError tag) [InterfaceMorphism tag]
projectInterfaceFaceMorphisms categoryValue faceIndexValue morphismValues =
  maybe
    (Left (FaceStalkProjectionInnerFaceIndexOutOfRange faceIndexValue))
    projectAtIndex
    (naturalToBoundedInt faceIndexValue)
  where
    sourceDimensionValue =
      length morphismValues

    projectAtIndex faceIndexInt
      | faceIndexInt == 0 =
          Right (dropLeadingMorphism morphismValues)
      | faceIndexInt == sourceDimensionValue =
          Right (dropTrailingMorphism morphismValues)
      | otherwise =
          composeAdjacentMorphisms @tag categoryValue (faceIndexValue - 1) morphismValues

composeAdjacentMorphisms ::
  forall tag.
  InterfaceDomain tag =>
  NerveCategory tag ->
  Natural ->
  [InterfaceMorphism tag] ->
  Either (FaceStalkProjectionError tag) [InterfaceMorphism tag]
composeAdjacentMorphisms categoryValue innerIndexValue morphismValues =
  maybe
    (Left (FaceStalkProjectionInnerFaceIndexOutOfRange innerIndexValue))
    composeAtIndex
    (naturalToBoundedInt innerIndexValue)
  where
    composeAtIndex indexValue =
      case splitAt indexValue morphismValues of
        (prefixMorphisms, leftMorphism : rightMorphism : suffixMorphisms) ->
          either
            (Left . FaceStalkProjectionAdjacentCompositionFailed innerIndexValue)
            (\combinedMorphism -> Right (prefixMorphisms <> [combinedMorphism] <> suffixMorphisms))
            (composeMorphismChainInCategory @tag categoryValue [leftMorphism, rightMorphism])
        _ -> Left (FaceStalkProjectionInnerFaceIndexOutOfRange innerIndexValue)

naturalToBoundedInt :: Natural -> Maybe Int
naturalToBoundedInt value
  | value <= fromIntegral (maxBound :: Int) =
      Just (fromIntegral value)
  | otherwise =
      Nothing

nerveCellDimensionInt :: NerveCell tag -> Int
nerveCellDimensionInt =
  fromIntegral . ckDimension . nerveCellKey

dropLeadingMorphism :: [a] -> [a]
dropLeadingMorphism =
  drop 1

dropTrailingMorphism :: [a] -> [a]
dropTrailingMorphism =
  maybe [] fst . unsnoc

grothendieckStalkFromCell ::
  forall system.
  ( InterfaceDomain (SystemTag system),
    SystemOb system ~ InterfaceObject (SystemTag system),
    SystemMor system ~ InterfaceMorphism (SystemTag system)
  ) =>
  GrothendieckCell system ->
  InterfaceStalk (SystemTag system)
grothendieckStalkFromCell cellValue =
  let chainValue = nerveSimplexChain (grothendieckCellSimplex cellValue)
   in stalkFromSourceAndMorphismsWith
        (composeMorphismChain @(SystemTag system))
        (goValue (chainStartObject chainValue))
        (mapMaybe gmTargetMorphism (chainMorphisms chainValue))
        (grothendieckCellDimension cellValue)

stalkFromSourceAndMorphisms ::
  forall tag.
  InterfaceDomain tag =>
  NerveCategory tag ->
  InterfaceObject tag ->
  [InterfaceMorphism tag] ->
  Int ->
  InterfaceStalk tag
stalkFromSourceAndMorphisms categoryValue =
  stalkFromSourceAndMorphismsWith (composeMorphismChainInCategory @tag categoryValue)

stalkFromSourceAndMorphismsWith ::
  forall tag.
  InterfaceDomain tag =>
  ([InterfaceMorphism tag] -> Either (InterfaceComposeError tag) (InterfaceMorphism tag)) ->
  InterfaceObject tag ->
  [InterfaceMorphism tag] ->
  Int ->
  InterfaceStalk tag
stalkFromSourceAndMorphismsWith composeChain sourceObject morphismValues dimensionValue =
  case morphismValues of
    [] ->
      objectStalk sourceObject dimensionValue
    _ ->
      let chainMeasure =
            foldMap (measureMorphism @tag) morphismValues
       in
      either
        (\compositionError ->
            fromMeasure
              chainMeasure
              (ObstructedWitness compositionError)
              dimensionValue
        )
        (\composedMorphism ->
            fromMeasure
              (chainMeasure <> measureMorphism @tag composedMorphism)
              (ComposedWitness composedMorphism)
              dimensionValue
        )
        (composeChain morphismValues)

mergeWitness :: CompositionWitness tag -> CompositionWitness tag -> CompositionWitness tag
mergeWitness leftWitness rightWitness =
  case (leftWitness, rightWitness) of
    (ObstructedWitness compositionError, _) -> ObstructedWitness compositionError
    (_, ObstructedWitness compositionError) -> ObstructedWitness compositionError
    (ComposedWitness morphismValue, _) -> ComposedWitness morphismValue
    (_, ComposedWitness morphismValue) -> ComposedWitness morphismValue
    _ -> TerminalWitness

objectStalk :: forall tag. InterfaceDomain tag => InterfaceObject tag -> Int -> InterfaceStalk tag
objectStalk objectValue = fromMeasure
    (measureObject @tag objectValue)
    TerminalWitness

fromMeasure :: InterfaceMeasure tag -> CompositionWitness tag -> Int -> InterfaceStalk tag
fromMeasure measure witness dimensionValue =
  InterfaceStalk
    { rsBoundNames = imBoundNames measure,
      rsDeletedNames = imDeletedNames measure,
      rsCreatedNames = imCreatedNames measure,
      rsGuarded = getAny (imGuarded measure),
      rsWitness = witness,
      rsCellDimension = dimensionValue
    }
