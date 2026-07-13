-- | The sole public authoring route for derived objects. This name-binding
-- builder lowers through the package-private specification compiler and returns
-- the sealed, site-owned carrier.
--
-- 'object' declares the graded object at a degree exactly once; both
-- differentials adjacent to that degree read their axis from the same
-- declaration, so the shape agreement a raw specification authors twice
-- holds here by construction. Summand references address coefficients by
-- name instead of flattened offsets:
--
-- > derivedObject poset $ do
-- >   (x0, [ia, ib]) <- object 0 [a, b]
-- >   (x1, [jb]) <- object 1 [b]
-- >   differential x0 x1 [component ia jb 1, component ib jb 1]
--
-- A complex concentrated in its top degree ends, exactly as in the raw
-- dialect, with an explicit empty successor object and a componentless
-- differential:
--
-- > (top, [ia]) <- object 0 [a]
-- > (end, _) <- object 1 []
-- > differential top end []
--
-- Shape faults — duplicate or gapped degrees, non-adjacent or missing or
-- repeated differentials, summand references used against the wrong endpoint,
-- duplicate components — are reported as typed 'DerivedBuildError's, first
-- fault wins. Everything semantic (@d∘d = 0@, minimality, normalization)
-- remains the compiler's judgment, exactly as for a hand-written
-- specification. Site membership and the chosen order variance are checked by
-- 'derivedObject'; no public formal-complex alternative exists.
module Moonlight.Derived.Presentation.Builder
  ( DerivedBuildError (..)
  , DerivedBuilder
  , ObjectRef
  , SummandRef
  , Component
  , object
  , objectsFrom
  , component
  , differential
  , differentialDense
  , derivedObject
  ) where

import Control.Monad (foldM)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import Moonlight.Algebra (IntegralDomain)
import Moonlight.Core (Field)
import Moonlight.Derived.Presentation
  ( DerivedBuildError (..)
  , DerivedSpec (..)
  , DifferentialSpec (..)
  , compileDerived
  )
import Moonlight.Derived.Pure.Site.InjectiveComplex (Derived)
import Moonlight.Derived.Pure.Site.LabeledMatrix (DenseMat (..))
import Moonlight.Derived.Pure.Site.Poset (DerivedPoset, FinObjectId)

-- | An opaque reference to the graded object declared at a degree.
data ObjectRef = ObjectRef !Int
  deriving stock (Eq, Show)

-- | An opaque reference to one injective summand of a declared object,
-- minted by 'object' in declaration order.
data SummandRef = SummandRef !Int !Int !FinObjectId
  deriving stock (Eq, Show)

-- | One named coefficient of a differential: source summand, target summand,
-- value. Construct with 'component'.
data Component c = Component !SummandRef !SummandRef !c

-- | Address a coefficient by its summand names instead of flattened offsets.
component :: SummandRef -> SummandRef -> c -> Component c
component = Component
{-# INLINE component #-}

data DifferentialDraft c
  = DraftComponents !(Map (Int, Int) c)
  | DraftDense !(DenseMat c)

data BuilderState c = BuilderState
  { bsObjects :: !(IntMap (Vector FinObjectId))
  , bsDifferentials :: !(IntMap (DifferentialDraft c))
  , bsFirstError :: !(Maybe DerivedBuildError)
  }

initialState :: BuilderState c
initialState =
  BuilderState
    { bsObjects = IntMap.empty
    , bsDifferentials = IntMap.empty
    , bsFirstError = Nothing
    }

-- | A pure presentation builder. The 'Maybe' lets refutable @do@-patterns
-- (e.g. @(x0, [ia, ib]) <- object 0 [a, b]@) short-circuit via 'fail' while
-- preserving the declarations accumulated so far for error reporting.
newtype DerivedBuilder c a = DerivedBuilder
  { unDerivedBuilder :: BuilderState c -> (Maybe a, BuilderState c) }

instance Functor (DerivedBuilder c) where
  fmap f (DerivedBuilder run) =
    DerivedBuilder (\state -> let (result, state') = run state in (fmap f result, state'))

instance Applicative (DerivedBuilder c) where
  pure value = DerivedBuilder (\state -> (Just value, state))
  DerivedBuilder runF <*> DerivedBuilder runA =
    DerivedBuilder
      ( \state ->
          case runF state of
            (Nothing, state') -> (Nothing, state')
            (Just f, state') ->
              let (result, state'') = runA state'
               in (fmap f result, state'')
      )

instance Monad (DerivedBuilder c) where
  DerivedBuilder run >>= k =
    DerivedBuilder
      ( \state ->
          case run state of
            (Nothing, state') -> (Nothing, state')
            (Just value, state') -> unDerivedBuilder (k value) state'
      )

instance MonadFail (DerivedBuilder c) where
  fail message =
    DerivedBuilder (\state -> (Nothing, recordError (DerivedBuildPatternFailure message) state))

recordError :: DerivedBuildError -> BuilderState c -> BuilderState c
recordError buildError state =
  case bsFirstError state of
    Nothing -> state { bsFirstError = Just buildError }
    Just _ -> state
{-# INLINE recordError #-}

-- | Declare the graded object at a degree by listing its injective summands;
-- multiplicity is repetition and an empty list is the zero object. Returns
-- the object reference together with one summand reference per listed node,
-- in declaration order.
object :: Int -> [FinObjectId] -> DerivedBuilder c (ObjectRef, [SummandRef])
object degreeValue summandNodes =
  DerivedBuilder
    ( \state ->
        let refs =
              ( ObjectRef degreeValue
              , zipWith (SummandRef degreeValue) [0 ..] summandNodes
              )
         in case IntMap.lookup degreeValue (bsObjects state) of
              Just _ ->
                (Just refs, recordError (DerivedBuildDuplicateDegree degreeValue) state)
              Nothing ->
                ( Just refs
                , state
                    { bsObjects =
                        IntMap.insert degreeValue (V.fromList summandNodes) (bsObjects state)
                    }
                )
    )

-- | Declare consecutive graded objects starting at a degree, one summand list
-- per successive degree — the contiguous window in one motion, with degree
-- gaps and typos unrepresentable by construction. Returns the declarations in
-- degree order, exactly as the corresponding 'object' calls would.
objectsFrom :: Int -> [[FinObjectId]] -> DerivedBuilder c [(ObjectRef, [SummandRef])]
objectsFrom startDegree =
  traverse (uncurry object) . zip [startDegree ..]

-- | Declare the differential between two adjacent declared objects by its
-- named components; omitted cells are zero, so an empty list is the zero map.
differential :: ObjectRef -> ObjectRef -> [Component c] -> DerivedBuilder c ()
differential sourceRef@(ObjectRef sourceDegree) targetRef@(ObjectRef targetDegree) components =
  recordDifferential sourceRef targetRef $ \sourceLabels targetLabels ->
    DraftComponents
      <$> foldM (insertComponent sourceLabels targetLabels) Map.empty components
  where
    insertComponent sourceLabels targetLabels cells (Component sourceSummand targetSummand coefficient) = do
      sourceIndex <- resolveSummand sourceDegree sourceLabels sourceSummand
      targetIndex <- resolveSummand targetDegree targetLabels targetSummand
      let cellKey = (targetIndex, sourceIndex)
      case Map.lookup cellKey cells of
        Just _ ->
          Left (DerivedBuildDuplicateComponent sourceDegree targetIndex sourceIndex)
        Nothing ->
          Right (Map.insert cellKey coefficient cells)

resolveSummand :: Int -> Vector FinObjectId -> SummandRef -> Either DerivedBuildError Int
resolveSummand expectedDegree labels (SummandRef refDegree refIndex refNode)
  | refDegree /= expectedDegree =
      Left (DerivedBuildComponentDegreeMismatch expectedDegree refDegree)
  | refIndex < 0 || refIndex >= V.length labels || labels V.! refIndex /= refNode =
      Left (DerivedBuildForeignSummand refDegree refIndex)
  | otherwise = Right refIndex

-- | Declare the differential as one dense matrix — rows indexed by the target
-- object, columns by the source object — for when a literal reads better than
-- a component list.
differentialDense :: ObjectRef -> ObjectRef -> DenseMat c -> DerivedBuilder c ()
differentialDense sourceRef@(ObjectRef sourceDegree) targetRef denseValue =
  recordDifferential sourceRef targetRef $ \sourceLabels targetLabels ->
    let expectedShape = (V.length targetLabels, V.length sourceLabels)
        actualShape = (dmRows denseValue, dmCols denseValue)
     in if actualShape == expectedShape
          then Right (DraftDense denseValue)
          else Left (DerivedBuildDenseShapeMismatch sourceDegree expectedShape actualShape)

recordDifferential ::
  ObjectRef ->
  ObjectRef ->
  (Vector FinObjectId -> Vector FinObjectId -> Either DerivedBuildError (DifferentialDraft c)) ->
  DerivedBuilder c ()
recordDifferential (ObjectRef sourceDegree) (ObjectRef targetDegree) mkDraft =
  DerivedBuilder (\state -> (Just (), applyDraft state))
  where
    applyDraft state =
      case draftAgainst state of
        Left buildError -> recordError buildError state
        Right draftValue ->
          state
            { bsDifferentials =
                IntMap.insert sourceDegree draftValue (bsDifferentials state)
            }
    draftAgainst state
      | targetDegree /= sourceDegree + 1 =
          Left (DerivedBuildNonAdjacentDifferential sourceDegree targetDegree)
      | IntMap.member sourceDegree (bsDifferentials state) =
          Left (DerivedBuildDuplicateDifferential sourceDegree)
      | otherwise = do
          sourceLabels <- declaredLabelsOr DerivedBuildUnknownDegree state sourceDegree
          targetLabels <- declaredLabelsOr DerivedBuildUnknownDegree state targetDegree
          mkDraft sourceLabels targetLabels

-- | Run the builder and emit the presentation it declares. The result is an
-- ordinary 'DerivedSpec'; nothing downstream distinguishes it from one
-- written by hand.
buildDerivedSpec :: Num c => DerivedBuilder c a -> Either DerivedBuildError (DerivedSpec c)
buildDerivedSpec builder =
  case unDerivedBuilder builder initialState of
    (_, state) ->
      case bsFirstError state of
        Just buildError -> Left buildError
        Nothing -> assembleSpec state

assembleSpec :: Num c => BuilderState c -> Either DerivedBuildError (DerivedSpec c)
assembleSpec state
  | IntMap.null (bsObjects state) = Left DerivedBuildEmpty
  | lowDegree == highDegree = Left DerivedBuildEmpty
  | otherwise = do
      differentials <- traverse differentialAt [lowDegree .. highDegree - 1]
      Right DerivedSpec { dsStartDegree = lowDegree, dsDifferentials = differentials }
  where
    (lowDegree, _) = IntMap.findMin (bsObjects state)
    (highDegree, _) = IntMap.findMax (bsObjects state)
    differentialAt degreeValue = do
      sourceLabels <- declaredLabels degreeValue
      targetLabels <- declaredLabels (degreeValue + 1)
      draftValue <-
        case IntMap.lookup degreeValue (bsDifferentials state) of
          Nothing -> Left (DerivedBuildMissingDifferential degreeValue)
          Just draftValue -> Right draftValue
      Right
        DifferentialSpec
          { diffRowLabels = targetLabels
          , diffColLabels = sourceLabels
          , diffMatrix = materialize (V.length targetLabels) (V.length sourceLabels) draftValue
          }
    declaredLabels = declaredLabelsOr DerivedBuildDegreeGap state

declaredLabelsOr ::
  (Int -> DerivedBuildError) ->
  BuilderState c ->
  Int ->
  Either DerivedBuildError (Vector FinObjectId)
declaredLabelsOr missingFault state degreeValue =
  case IntMap.lookup degreeValue (bsObjects state) of
    Nothing -> Left (missingFault degreeValue)
    Just labels -> Right labels

materialize :: Num c => Int -> Int -> DifferentialDraft c -> DenseMat c
materialize rowCount colCount draftValue =
  case draftValue of
    DraftDense denseValue -> denseValue
    DraftComponents cells ->
      DenseMat
        rowCount
        colCount
        ( V.generate
            rowCount
            ( \rowIndex ->
                V.generate
                  colCount
                  (\colIndex -> Map.findWithDefault 0 (rowIndex, colIndex) cells)
            )
        )

-- | Build and compile in one motion: the builder's specification handed to
-- 'compileDerived' over the given site.
derivedObject ::
  (Eq c, Field c, IntegralDomain c, Num c) =>
  DerivedPoset -> DerivedBuilder c a -> Either DerivedBuildError (Derived c)
derivedObject posetValue builder =
  buildDerivedSpec builder >>= compileDerived posetValue
