{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Differential.Row.Patch
  ( EpochTransition (..),

    PlainRowPatch,
    emptyPlainRowPatch,
    plainRowPatchFromList,
    plainRowPatchFromChangeMap,
    plainRowPatchFromMultiplicityMap,
    plainRowPatchChangeMap,
    positivePlainRowPatchRows,
    normalizePlainRowPatch,
    composePlainRowPatch,
    negatePlainRowPatch,
    subtractPlainRowPatch,
    mapPlainRowPatchRows,
    traversePlainRowPatchRowsWith,
    plainRowPatchNull,
    plainRowPatchRows,
    applyPlainRowPatchWith,

    AnnotatedRowPatch,
    emptyAnnotatedRowPatch,
    singletonAnnotatedRowPatch,
    annotatedRowPatchFromList,
    annotatedRowPatchFromRows,
    annotatedRowPatchRows,
    positiveAnnotatedRowPatchRows,
    annotatedRowPatchRowReadoutWith,
    annotatedRowPatchReadoutWith,
    annotatePlainRowPatch,
    applyAnnotatedRowPatchWith,
    normalizeAnnotatedRowPatch,
    composeAnnotatedRowPatch,
    negateAnnotatedRowPatch,
    subtractAnnotatedRowPatch,
    mapAnnotatedRowPatchRows,
    traverseAnnotatedRowPatchRowsWith,
    mapAnnotatedRowPatchAnnotations,
    forgetAnnotatedRowPatch,
    annotatedRowPatchNull,

    ShapedPatch (..),
    ShapedPatchComposeError (..),
    emptyShapedPatch,
    normalizeShapedPatch,
    shapedPatchNull,
    composeShapedPatch,
    shapedPatchSupport,
    oldCellsOfShapedPatch,
    newCellsOfShapedPatch,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Delta.Normalize
  ( DeltaNormalize (..),
  )
import Moonlight.Delta.Patch qualified as CorePatch
import Moonlight.Delta.Signed
  ( Multiplicity (..),
    MultiplicityChange (..),
    SignedApplyError (..),
    addMultiplicityChange,
    multiplicityAsChange,
    multiplicityValue,
    negateMultiplicityChange,
    positiveMultiplicityChange,
    zeroMultiplicityChange,
  )
import Moonlight.Delta.Signed qualified as Signed
import Moonlight.Delta.Support
  ( DeltaSupport (..),
  )

type PlainRows row =
  Map row MultiplicityChange

normalizePlainRows ::
  PlainRows row ->
  PlainRows row
normalizePlainRows =
  Map.filter (/= zeroMultiplicityChange)
{-# INLINE normalizePlainRows #-}

plainRowsFromList ::
  Ord row =>
  [(row, MultiplicityChange)] ->
  PlainRows row
plainRowsFromList =
  normalizePlainRows . Map.fromListWith addMultiplicityChange
{-# INLINE plainRowsFromList #-}

unionPlainRows ::
  Ord row =>
  PlainRows row ->
  PlainRows row ->
  PlainRows row
unionPlainRows left right =
  normalizePlainRows (Map.unionWith addMultiplicityChange left right)
{-# INLINE unionPlainRows #-}

mapPlainRowKeys ::
  Ord target =>
  (source -> target) ->
  PlainRows source ->
  PlainRows target
mapPlainRowKeys project =
  normalizePlainRows . Map.mapKeysWith addMultiplicityChange project
{-# INLINE mapPlainRowKeys #-}

applyPlainRowsDeltaWith ::
  Ord row =>
  (row -> Multiplicity -> MultiplicityChange -> err) ->
  PlainRows row ->
  Map row Multiplicity ->
  Either err (Map row Multiplicity)
applyPlainRowsDeltaWith mkUnderflow deltaRows currentRows =
  case Signed.applySignedToMap (Signed.signedFromChangeMap deltaRows) currentRows of
    Left (SignedMultiplicityUnderflow rowValue oldMultiplicity deltaMultiplicity) ->
      Left (mkUnderflow rowValue oldMultiplicity deltaMultiplicity)
    Right nextRows ->
      Right nextRows
{-# INLINE applyPlainRowsDeltaWith #-}

type EpochTransition :: Type -> Type
data EpochTransition epoch = EpochTransition
  { etBefore :: !epoch,
    etAfter :: !epoch
  }
  deriving stock (Eq, Ord, Show)

type PlainRowPatch :: Type -> Type
newtype PlainRowPatch row =
  PlainRowPatch (Signed.Signed row)
  deriving stock (Eq, Show)

type AnnotatedRowPatch :: Type -> Type -> Type
newtype AnnotatedRowPatch row ann =
  AnnotatedRowPatch (Map row (Map ann MultiplicityChange))
  deriving stock (Eq, Show)

emptyPlainRowPatch :: PlainRowPatch row
emptyPlainRowPatch =
  PlainRowPatch Signed.emptySigned
{-# INLINE emptyPlainRowPatch #-}

plainRowPatchFromList ::
  Ord row =>
  [(row, MultiplicityChange)] ->
  PlainRowPatch row
plainRowPatchFromList =
  PlainRowPatch . Signed.signedFromChangeMap . plainRowsFromList
{-# INLINE plainRowPatchFromList #-}

plainRowPatchFromChangeMap ::
  Map row MultiplicityChange ->
  PlainRowPatch row
plainRowPatchFromChangeMap =
  PlainRowPatch . Signed.signedFromChangeMap
{-# INLINE plainRowPatchFromChangeMap #-}

plainRowPatchFromMultiplicityMap ::
  Map row Multiplicity ->
  PlainRowPatch row
plainRowPatchFromMultiplicityMap =
  plainRowPatchFromChangeMap . Map.map multiplicityAsChange
{-# INLINE plainRowPatchFromMultiplicityMap #-}

plainRowPatchChangeMap ::
  PlainRowPatch row ->
  Map row MultiplicityChange
plainRowPatchChangeMap (PlainRowPatch rowsDelta) =
  Signed.signedToChangeMap rowsDelta
{-# INLINE plainRowPatchChangeMap #-}

positivePlainRowPatchRows ::
  PlainRowPatch row ->
  Map row Multiplicity
positivePlainRowPatchRows =
  Map.mapMaybe positiveMultiplicityChange . plainRowPatchChangeMap . normalizePlainRowPatch
{-# INLINE positivePlainRowPatchRows #-}

normalizePlainRowPatch ::
  PlainRowPatch row ->
  PlainRowPatch row
normalizePlainRowPatch (PlainRowPatch rowsDelta) =
  PlainRowPatch (Signed.normalizeSigned rowsDelta)
{-# INLINE normalizePlainRowPatch #-}

composePlainRowPatch ::
  Ord row =>
  PlainRowPatch row ->
  PlainRowPatch row ->
  PlainRowPatch row
composePlainRowPatch (PlainRowPatch newerRows) (PlainRowPatch olderRows) =
  PlainRowPatch (Signed.combineSigned newerRows olderRows)
{-# INLINE composePlainRowPatch #-}

negatePlainRowPatch ::
  PlainRowPatch row ->
  PlainRowPatch row
negatePlainRowPatch (PlainRowPatch rowsDelta) =
  PlainRowPatch (Signed.negateSigned rowsDelta)
{-# INLINE negatePlainRowPatch #-}

subtractPlainRowPatch ::
  Ord row =>
  PlainRowPatch row ->
  PlainRowPatch row ->
  PlainRowPatch row
subtractPlainRowPatch newer older =
  composePlainRowPatch newer (negatePlainRowPatch older)
{-# INLINE subtractPlainRowPatch #-}

mapPlainRowPatchRows ::
  Ord target =>
  (source -> target) ->
  PlainRowPatch source ->
  PlainRowPatch target
mapPlainRowPatchRows project (PlainRowPatch rowsDelta) =
  PlainRowPatch (Signed.mapSignedKeys project rowsDelta)
{-# INLINE mapPlainRowPatchRows #-}

traversePlainRowPatchRowsWith ::
  Ord target =>
  (source -> Either err target) ->
  PlainRowPatch source ->
  Either err (PlainRowPatch target)
traversePlainRowPatchRowsWith project (PlainRowPatch rowsDelta) =
  PlainRowPatch <$> Signed.traverseSignedKeysWith project rowsDelta
{-# INLINE traversePlainRowPatchRowsWith #-}

plainRowPatchNull ::
  PlainRowPatch row ->
  Bool
plainRowPatchNull (PlainRowPatch rowsDelta) =
  Signed.signedNull rowsDelta
{-# INLINE plainRowPatchNull #-}

plainRowPatchRows ::
  PlainRowPatch row ->
  Set row
plainRowPatchRows (PlainRowPatch rowsDelta) =
  Signed.support rowsDelta
{-# INLINE plainRowPatchRows #-}

applyPlainRowPatchWith ::
  Ord row =>
  (row -> Multiplicity -> MultiplicityChange -> err) ->
  PlainRowPatch row ->
  Map row Multiplicity ->
  Either err (Map row Multiplicity)
applyPlainRowPatchWith mkUnderflow (PlainRowPatch deltaRows) currentRows =
  case Signed.applySignedToMap deltaRows currentRows of
    Left (SignedMultiplicityUnderflow rowValue oldMultiplicity deltaMultiplicity) ->
      Left (mkUnderflow rowValue oldMultiplicity deltaMultiplicity)
    Right nextRows ->
      Right nextRows
{-# INLINE applyPlainRowPatchWith #-}

instance Ord row => Semigroup (PlainRowPatch row) where
  PlainRowPatch left <> PlainRowPatch right =
    PlainRowPatch (Signed.combineSigned left right)
  {-# INLINE (<>) #-}

instance Ord row => Monoid (PlainRowPatch row) where
  mempty =
    emptyPlainRowPatch
  {-# INLINE mempty #-}

instance DeltaNormalize (PlainRowPatch row) where
  normalizeDelta =
    normalizePlainRowPatch
  {-# INLINE normalizeDelta #-}

  deltaNull =
    plainRowPatchNull
  {-# INLINE deltaNull #-}

instance DeltaSupport (PlainRowPatch row) where
  type DeltaSupportSet (PlainRowPatch row) = Set row

  emptySupport =
    Set.empty
  {-# INLINE emptySupport #-}

  deltaSupport =
    plainRowPatchRows
  {-# INLINE deltaSupport #-}

emptyAnnotatedRowPatch :: AnnotatedRowPatch row ann
emptyAnnotatedRowPatch =
  AnnotatedRowPatch Map.empty
{-# INLINE emptyAnnotatedRowPatch #-}

singletonAnnotatedRowPatch ::
  row ->
  ann ->
  MultiplicityChange ->
  AnnotatedRowPatch row ann
singletonAnnotatedRowPatch rowValue annotation multiplicity =
  normalizeAnnotatedRowPatch
    (AnnotatedRowPatch (Map.singleton rowValue (Map.singleton annotation multiplicity)))
{-# INLINE singletonAnnotatedRowPatch #-}

annotatedRowPatchFromList ::
  (Ord row, Ord ann) =>
  [(row, ann, MultiplicityChange)] ->
  AnnotatedRowPatch row ann
annotatedRowPatchFromList =
  annotatedRowPatchFromRows
    . Map.fromListWith unionPlainRows
    . fmap (\(rowValue, annotation, multiplicity) -> (rowValue, Map.singleton annotation multiplicity))
{-# INLINE annotatedRowPatchFromList #-}

annotatedRowPatchFromRows ::
  Map row (Map ann MultiplicityChange) ->
  AnnotatedRowPatch row ann
annotatedRowPatchFromRows =
  normalizeAnnotatedRowPatch . AnnotatedRowPatch
{-# INLINE annotatedRowPatchFromRows #-}

annotatedRowPatchRows ::
  AnnotatedRowPatch row ann ->
  Map row (Map ann MultiplicityChange)
annotatedRowPatchRows (AnnotatedRowPatch rows) =
  normalizeAnnotatedRows rows
{-# INLINE annotatedRowPatchRows #-}

positiveAnnotatedRowPatchRows ::
  AnnotatedRowPatch row ann ->
  Map row (Map ann Multiplicity)
positiveAnnotatedRowPatchRows =
  Map.mapMaybe positiveAnnotationBag . annotatedRowPatchRows
{-# INLINE positiveAnnotatedRowPatchRows #-}

annotatedRowPatchRowReadoutWith ::
  (ann -> ann -> ann) ->
  AnnotatedRowPatch row ann ->
  Map row ann
annotatedRowPatchRowReadoutWith joinAnnotation =
  Map.mapMaybe
    (annotationBagReadoutWith joinAnnotation)
    . positiveAnnotatedRowPatchRows
{-# INLINE annotatedRowPatchRowReadoutWith #-}

annotatedRowPatchReadoutWith ::
  (ann -> ann -> ann) ->
  AnnotatedRowPatch row ann ->
  Maybe ann
annotatedRowPatchReadoutWith joinAnnotation =
  Map.foldl'
    (mergeMaybeAnnotation joinAnnotation)
    Nothing
    . annotatedRowPatchRowReadoutWith joinAnnotation
{-# INLINE annotatedRowPatchReadoutWith #-}

annotationBagReadoutWith ::
  (ann -> ann -> ann) ->
  Map ann Multiplicity ->
  Maybe ann
annotationBagReadoutWith joinAnnotation =
  Map.foldlWithKey'
    ( \acc annotation multiplicity ->
        if multiplicityValue multiplicity > 0
          then mergeMaybeAnnotation joinAnnotation acc annotation
          else acc
    )
    Nothing
{-# INLINE annotationBagReadoutWith #-}

mergeMaybeAnnotation ::
  (ann -> ann -> ann) ->
  Maybe ann ->
  ann ->
  Maybe ann
mergeMaybeAnnotation _ Nothing annotation =
  Just annotation
mergeMaybeAnnotation joinAnnotation (Just oldAnnotation) newAnnotation =
  Just (joinAnnotation oldAnnotation newAnnotation)
{-# INLINE mergeMaybeAnnotation #-}

annotatePlainRowPatch ::
  ann ->
  PlainRowPatch row ->
  AnnotatedRowPatch row ann
annotatePlainRowPatch annotation rows =
  annotatedRowPatchFromRows
    (Map.map (Map.singleton annotation) (plainRowPatchChangeMap rows))
{-# INLINE annotatePlainRowPatch #-}

applyAnnotatedRowPatchWith ::
  (Ord row, Ord ann) =>
  (row -> ann -> Multiplicity -> MultiplicityChange -> err) ->
  AnnotatedRowPatch row ann ->
  Map row (Map ann Multiplicity) ->
  Either err (Map row (Map ann Multiplicity))
applyAnnotatedRowPatchWith mkUnderflow deltaRows currentRows =
  Map.foldlWithKey'
    applyRow
    (Right currentRows)
    (annotatedRowPatchRows deltaRows)
  where
    applyRow eitherRows rowValue annotationDeltas = do
      rows <- eitherRows
      let currentAnnotations =
            Map.findWithDefault Map.empty rowValue rows
          mkAnnotationUnderflow annotation =
            mkUnderflow rowValue annotation
      nextAnnotations <-
        applyPlainRowsDeltaWith
          mkAnnotationUnderflow
          annotationDeltas
          currentAnnotations
      pure (alterEmptyMap rowValue nextAnnotations rows)
{-# INLINE applyAnnotatedRowPatchWith #-}

normalizeAnnotatedRowPatch ::
  AnnotatedRowPatch row ann ->
  AnnotatedRowPatch row ann
normalizeAnnotatedRowPatch (AnnotatedRowPatch rows) =
  AnnotatedRowPatch (normalizeAnnotatedRows rows)
{-# INLINE normalizeAnnotatedRowPatch #-}

composeAnnotatedRowPatch ::
  (Ord row, Ord ann) =>
  AnnotatedRowPatch row ann ->
  AnnotatedRowPatch row ann ->
  AnnotatedRowPatch row ann
composeAnnotatedRowPatch (AnnotatedRowPatch newerRows) (AnnotatedRowPatch olderRows) =
  annotatedRowPatchFromRows
    (Map.unionWith unionPlainRows newerRows olderRows)
{-# INLINE composeAnnotatedRowPatch #-}

negateAnnotatedRowPatch ::
  AnnotatedRowPatch row ann ->
  AnnotatedRowPatch row ann
negateAnnotatedRowPatch (AnnotatedRowPatch rows) =
  AnnotatedRowPatch (Map.map (Map.map negateMultiplicityChange) rows)
{-# INLINE negateAnnotatedRowPatch #-}

subtractAnnotatedRowPatch ::
  (Ord row, Ord ann) =>
  AnnotatedRowPatch row ann ->
  AnnotatedRowPatch row ann ->
  AnnotatedRowPatch row ann
subtractAnnotatedRowPatch newer older =
  composeAnnotatedRowPatch newer (negateAnnotatedRowPatch older)
{-# INLINE subtractAnnotatedRowPatch #-}

mapAnnotatedRowPatchRows ::
  (Ord target, Ord ann) =>
  (source -> target) ->
  AnnotatedRowPatch source ann ->
  AnnotatedRowPatch target ann
mapAnnotatedRowPatchRows project (AnnotatedRowPatch rows) =
  annotatedRowPatchFromRows
    (Map.mapKeysWith unionPlainRows project rows)
{-# INLINE mapAnnotatedRowPatchRows #-}

traverseAnnotatedRowPatchRowsWith ::
  (Ord target, Ord ann) =>
  (source -> Either err target) ->
  AnnotatedRowPatch source ann ->
  Either err (AnnotatedRowPatch target ann)
traverseAnnotatedRowPatchRowsWith project (AnnotatedRowPatch rows) =
  annotatedRowPatchFromRows
    <$> Map.foldlWithKey'
      step
      (Right Map.empty)
      rows
  where
    step eitherAcc sourceRow annotations = do
      acc <- eitherAcc
      targetRow <- project sourceRow
      pure (Map.insertWith unionPlainRows targetRow annotations acc)
{-# INLINE traverseAnnotatedRowPatchRowsWith #-}

mapAnnotatedRowPatchAnnotations ::
  Ord targetAnn =>
  (sourceAnn -> targetAnn) ->
  AnnotatedRowPatch row sourceAnn ->
  AnnotatedRowPatch row targetAnn
mapAnnotatedRowPatchAnnotations project (AnnotatedRowPatch rows) =
  annotatedRowPatchFromRows
    (Map.map (mapPlainRowKeys project) rows)
{-# INLINE mapAnnotatedRowPatchAnnotations #-}

forgetAnnotatedRowPatch ::
  AnnotatedRowPatch row ann ->
  PlainRowPatch row
forgetAnnotatedRowPatch =
  plainRowPatchFromChangeMap
    . Map.map annotationBagChange
    . annotatedRowPatchRows
{-# INLINE forgetAnnotatedRowPatch #-}

annotatedRowPatchNull ::
  AnnotatedRowPatch row ann ->
  Bool
annotatedRowPatchNull =
  Map.null . annotatedRowPatchRows
{-# INLINE annotatedRowPatchNull #-}

instance DeltaNormalize (AnnotatedRowPatch row ann) where
  normalizeDelta =
    normalizeAnnotatedRowPatch
  {-# INLINE normalizeDelta #-}

  deltaNull =
    annotatedRowPatchNull
  {-# INLINE deltaNull #-}

instance DeltaSupport (AnnotatedRowPatch row ann) where
  type DeltaSupportSet (AnnotatedRowPatch row ann) = Set row

  emptySupport =
    Set.empty
  {-# INLINE emptySupport #-}

  deltaSupport =
    Map.keysSet . annotatedRowPatchRows
  {-# INLINE deltaSupport #-}

normalizeAnnotatedRows ::
  Map row (Map ann MultiplicityChange) ->
  Map row (Map ann MultiplicityChange)
normalizeAnnotatedRows =
  Map.mapMaybe normalizeAnnotationBag
{-# INLINE normalizeAnnotatedRows #-}

normalizeAnnotationBag ::
  Map ann MultiplicityChange ->
  Maybe (Map ann MultiplicityChange)
normalizeAnnotationBag annotations =
  let !normalizedAnnotations =
        normalizePlainRows annotations
   in if Map.null normalizedAnnotations
        then Nothing
        else Just normalizedAnnotations
{-# INLINE normalizeAnnotationBag #-}

positiveAnnotationBag ::
  Map ann MultiplicityChange ->
  Maybe (Map ann Multiplicity)
positiveAnnotationBag annotations =
  let !positiveAnnotations =
        Map.mapMaybe positiveMultiplicityChange annotations
   in if Map.null positiveAnnotations
        then Nothing
        else Just positiveAnnotations
{-# INLINE positiveAnnotationBag #-}

annotationBagChange ::
  Map ann MultiplicityChange ->
  MultiplicityChange
annotationBagChange =
  Map.foldl' addMultiplicityChange zeroMultiplicityChange
{-# INLINE annotationBagChange #-}

alterEmptyMap ::
  Ord key =>
  key ->
  Map inner value ->
  Map key (Map inner value) ->
  Map key (Map inner value)
alterEmptyMap key rows =
  if Map.null rows
    then Map.delete key
    else Map.insert key rows
{-# INLINE alterEmptyMap #-}

type ShapedPatch :: Type -> Type -> Type -> Type
data ShapedPatch shape key value = ShapedPatch
  { spdShape :: !shape,
    spdDelta :: !(CorePatch.Patch key value)
  }
  deriving stock (Eq, Show)

type ShapedPatchComposeError :: Type -> Type -> Type -> Type
data ShapedPatchComposeError shape key value
  = ShapedPatchShapeMismatch !shape !shape
  | ShapedCellMismatch !(CorePatch.ComposeError key value)
  deriving stock (Eq, Show)

emptyShapedPatch ::
  shape ->
  ShapedPatch shape key value
emptyShapedPatch shape =
  ShapedPatch
    { spdShape = shape,
      spdDelta = CorePatch.empty
    }
{-# INLINE emptyShapedPatch #-}

normalizeShapedPatch ::
  ShapedPatch shape key value ->
  ShapedPatch shape key value
normalizeShapedPatch patch =
  patch
    { spdDelta =
        CorePatch.normalize (spdDelta patch)
    }
{-# INLINE normalizeShapedPatch #-}

shapedPatchNull ::
  ShapedPatch shape key value ->
  Bool
shapedPatchNull =
  CorePatch.null . spdDelta
{-# INLINE shapedPatchNull #-}

composeShapedPatch ::
  (Eq shape, CorePatch.PatchKey key, CorePatch.PatchValue value) =>
  ShapedPatch shape key value ->
  ShapedPatch shape key value ->
  Either (ShapedPatchComposeError shape key value) (ShapedPatch shape key value)
composeShapedPatch newer older
  | spdShape newer /= spdShape older =
      Left
        ( ShapedPatchShapeMismatch
            (spdShape newer)
            (spdShape older)
        )
  | otherwise =
      case CorePatch.compose (spdDelta newer) (spdDelta older) of
        Left err ->
          Left (ShapedCellMismatch err)
        Right delta ->
          Right
            ( normalizeShapedPatch
                oldShapePatch
                  { spdDelta = delta
                  }
            )
  where
    oldShapePatch =
      older
{-# INLINE composeShapedPatch #-}

shapedPatchSupport ::
  ShapedPatch shape key value ->
  Set key
shapedPatchSupport =
  CorePatch.support . CorePatch.normalize . spdDelta
{-# INLINE shapedPatchSupport #-}

oldCellsOfShapedPatch ::
  ShapedPatch shape key value ->
  Map key value
oldCellsOfShapedPatch =
  CorePatch.mapMaybeWithKey (const CorePatch.cellBefore) . spdDelta
{-# INLINE oldCellsOfShapedPatch #-}

newCellsOfShapedPatch ::
  ShapedPatch shape key value ->
  Map key value
newCellsOfShapedPatch =
  CorePatch.mapMaybeWithKey (const CorePatch.cellAfter) . spdDelta
{-# INLINE newCellsOfShapedPatch #-}

instance DeltaNormalize (ShapedPatch shape key value) where
  normalizeDelta =
    normalizeShapedPatch

  deltaNull =
    shapedPatchNull

instance DeltaSupport (ShapedPatch shape key value) where
  type DeltaSupportSet (ShapedPatch shape key value) = Set key

  emptySupport =
    Set.empty

  deltaSupport =
    shapedPatchSupport
