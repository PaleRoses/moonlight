{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Carrier.Morphism.Internal.Glue
  ( amalgamateCarrierFamily,
  )
where

import Control.Monad
  ( unless,
  )
import Data.Bifunctor
  ( first,
  )
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.List qualified as List
import Data.List.NonEmpty
  ( NonEmpty (..),
  )
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Semigroup
  ( sconcat,
  )
import Data.Set qualified as Set
import Moonlight.Core
  ( firstDuplicate,
  )
import Moonlight.Core
  ( pairwise,
  )
import Moonlight.Core
  ( SlotId,
    slotIdKey,
  )
import Moonlight.Differential.Carrier.Address
  ( caContext,
    caProp,
    caCarrier,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
    BoundaryShape (..),
    boundaryShape,
  )
import Moonlight.Flow.Carrier.Core.Coverage
  ( CoverageFact (ExactAmalgamated, LowerBound),
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDeltaP (..),
    RelationalCarrierDelta,
  )
import Moonlight.Delta.Signed
  ( Multiplicity,
    addMultiplicity,
    zeroMultiplicity
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchFromMultiplicityMap,
    positivePlainRowPatchRows
  )
import Moonlight.Flow.Carrier.Core.Origin
  ( OriginEvent (OriginAmalgamated), originMerge,
  )
import Moonlight.Flow.Carrier.Core.Obstruction.Types
  ( CarrierObstructionEvidence (..),
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (PhaseAmalgamate),
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
    recontextRelationalCarrierTime,
    retimeRelationalCarrierPhase,
  )
import Moonlight.Differential.Row.Tuple

import Moonlight.Differential.Carrier.Topology
  ( CarrierCover,
    carrierCoverComplete,
    carrierCoverMembers,
    carrierCoverSupport,
    carrierCoverTarget,
  )
import Moonlight.Flow.Carrier.Morphism.Amalgamation
  ( AmalgamationError (..),
    AmalgamationResult (..),
    BoundaryCoherenceResult (..),
    checkCarrierBoundaryCoherence,
    mergeCarrierBoundaries,
  )
data RowFragment ctx boundary = RowFragment
  { rfContext :: !ctx,
    rfBoundary :: !boundary,
    rfSchema :: ![SlotId],
    rfRow :: !RowTupleKey,
    rfMultiplicity :: !Multiplicity,
    rfEnv :: !(IntMap Int)
  }
  deriving stock (Eq, Show)

amalgamateCarrierFamily ::
  (Ord ctx, Ord carrier, Ord prop, Semigroup evidence) =>
  CarrierCover ctx ->
  NonEmpty (RelationalCarrierDelta ctx carrier prop RuntimeBoundary evidence) ->
  Either
    (AmalgamationError ctx carrier prop RuntimeBoundary evidence)
    (AmalgamationResult ctx carrier prop RuntimeBoundary evidence)
amalgamateCarrierFamily cover deltas = do
  validateCarrierFamily deltas
  validateCoverMembership cover deltas
  mergedBoundary <-
    first AmalgamationBoundaryMergeError $
      mergeCarrierBoundaries (fmap deBoundary deltas)
  case boundaryObstructions deltas of
    obstruction : obstructions ->
      Right (ObstructedAmalgamation (obstruction :| obstructions))
    [] -> do
      fragmentGroups <-
        traverse deltaFragments (NonEmpty.toList deltas)
      case glueFragmentGroups fragmentGroups mergedBoundary of
        Left obstructions ->
          Right (ObstructedAmalgamation obstructions)
        Right gluedRows ->
          let coverage =
                if carrierCoverComplete cover
                  && Set.fromList (fmap (caContext . deAddr) (NonEmpty.toList deltas)) == carrierCoverMembers cover
                  then ExactAmalgamated
                  else LowerBound
              outputDelta =
                amalgamatedDelta cover coverage mergedBoundary deltas gluedRows
           in Right $
                case coverage of
                  ExactAmalgamated ->
                    ExactAmalgamatedDelta outputDelta
                  _ ->
                    LowerBoundDelta outputDelta

validateCarrierFamily ::
  (Eq carrier, Eq prop) =>
  NonEmpty (RelationalCarrierDelta ctx carrier prop boundary evidence) ->
  Either
    (AmalgamationError ctx carrier prop boundary evidence)
    ()
validateCarrierFamily (firstDelta :| restDeltas) =
  Foldable.traverse_ validateOne restDeltas
  where
    firstAddr =
      deAddr firstDelta

    validateOne deltaValue =
      let addr = deAddr deltaValue
       in unless
            ( caCarrier addr == caCarrier firstAddr
                && caProp addr == caProp firstAddr
            )
            (Left (AmalgamationCarrierMismatch firstAddr addr))

validateCoverMembership ::
  Ord ctx =>
  CarrierCover ctx ->
  NonEmpty (RelationalCarrierDelta ctx carrier prop boundary evidence) ->
  Either
    (AmalgamationError ctx carrier prop boundary evidence)
    ()
validateCoverMembership cover deltas =
  Foldable.traverse_ validateMember contexts
    *> case firstDuplicate contexts of
      Nothing ->
        Right ()
      Just duplicateContext ->
        Left (AmalgamationDuplicateContext duplicateContext)
  where
    contexts =
      fmap (caContext . deAddr) (NonEmpty.toList deltas)

    validateMember contextValue =
      unless (Set.member contextValue (carrierCoverMembers cover)) $
        Left (AmalgamationContextOutsideCover contextValue)

boundaryObstructions ::
  NonEmpty (RelationalCarrierDelta ctx carrier prop RuntimeBoundary evidence) ->
  [CarrierObstructionEvidence ctx carrier prop RuntimeBoundary evidence]
boundaryObstructions deltas =
  [ StructuralMismatch
      (deAddr leftDelta)
      (deAddr rightDelta)
      conflictBoundary
      (deBoundary leftDelta)
      (deBoundary rightDelta)
  | (leftDelta, rightDelta) <- pairwise (NonEmpty.toList deltas),
    IncompatibleBoundary conflictBoundary <-
      [checkCarrierBoundaryCoherence (deBoundary leftDelta) (deBoundary rightDelta)]
  ]

deltaFragments ::
  RelationalCarrierDelta ctx carrier prop RuntimeBoundary evidence ->
  Either
    (AmalgamationError ctx carrier prop RuntimeBoundary evidence)
    [RowFragment ctx RuntimeBoundary]
deltaFragments deltaValue =
  traverse rowFragment (Map.toAscList (positivePlainRowPatchRows (deRows deltaValue)))
  where
    contextValue =
      caContext (deAddr deltaValue)

    boundaryValue =
      deBoundary deltaValue

    schema =
      bsSchema (boundaryShape boundaryValue)

    rowFragment (rowValue, multiplicity) = do
      env <-
        case tupleKeyFoldlSlotInts'
          (\acc slotKey repKey -> IntMap.insert slotKey repKey acc)
          IntMap.empty
          schema
          rowValue of
          Just envValue ->
            Right envValue
          Nothing ->
            Left (AmalgamationRowWidthMismatch contextValue schema rowValue)
      Right
        RowFragment
          { rfContext = contextValue,
            rfBoundary = boundaryValue,
            rfSchema = schema,
            rfRow = rowValue,
            rfMultiplicity = multiplicity,
            rfEnv = env
          }

glueFragmentGroups ::
  [[RowFragment ctx RuntimeBoundary]] ->
  RuntimeBoundary ->
  Either
    (NonEmpty (CarrierObstructionEvidence ctx carrier prop RuntimeBoundary evidence))
    (Map RowTupleKey Multiplicity)
glueFragmentGroups fragmentGroups boundaryValue =
  let orderedGroups =
        List.sortOn length fragmentGroups
      results =
        glueSearch boundaryValue orderedGroups
      obstructions =
        concatMap fst results
      rows =
        Map.unionsWith addMultiplicity (fmap snd results)
   in case obstructions of
        obstruction : rest ->
          Left (obstruction :| rest)
        [] ->
          Right rows

glueSearch ::
  RuntimeBoundary ->
  [[RowFragment ctx RuntimeBoundary]] ->
  [([CarrierObstructionEvidence ctx carrier prop RuntimeBoundary evidence], Map RowTupleKey Multiplicity)]
glueSearch boundaryValue groups =
  go IntMap.empty Nothing groups
  where
    outputSchema =
      bsSchema (boundaryShape boundaryValue)

    go env maybeMultiplicity remaining =
      case remaining of
        [] ->
          case (envToRow outputSchema env, maybeMultiplicity) of
            (Just rowValue, Just multiplicity) ->
              [([], Map.singleton rowValue multiplicity)]
            (Just rowValue, Nothing) ->
              [([], Map.singleton rowValue zeroMultiplicity)]
            (Nothing, _) ->
              [([], Map.empty)]
        group : rest ->
          [ case mergeFragment env maybeMultiplicity fragment of
              Left obstruction ->
                ([obstruction], Map.empty)
              Right (envNext, multiplicityNext) ->
                combineGlueResults (go envNext (Just multiplicityNext) rest)
          | fragment <- group
          ]

combineGlueResults ::
  [([CarrierObstructionEvidence ctx carrier prop RuntimeBoundary evidence], Map RowTupleKey Multiplicity)] ->
  ([CarrierObstructionEvidence ctx carrier prop RuntimeBoundary evidence], Map RowTupleKey Multiplicity)
combineGlueResults results =
  ( concatMap fst results,
    Map.unionsWith addMultiplicity (fmap snd results)
  )

mergeFragment ::
  IntMap Int ->
  Maybe Multiplicity ->
  RowFragment ctx RuntimeBoundary ->
  Either
    (CarrierObstructionEvidence ctx carrier prop RuntimeBoundary evidence)
    (IntMap Int, Multiplicity)
mergeFragment env maybeMultiplicity fragment = do
  envNext <-
    case mergeEnv env (rfEnv fragment) of
      Just merged ->
        Right merged
      Nothing ->
        Left
          ( CarrierRowProjectionMismatch
              (rfContext fragment)
              (rfBoundary fragment)
              (rfRow fragment)
          )
  multiplicityNext <-
    case maybeMultiplicity of
      Nothing ->
        Right (rfMultiplicity fragment)
      Just existing
        | existing == rfMultiplicity fragment ->
            Right existing
        | otherwise ->
            Left
              ( CarrierMultiplicityMismatch
                  (rfContext fragment)
                  (rfRow fragment)
                  existing
                  (rfMultiplicity fragment)
              )
  Right (envNext, multiplicityNext)

amalgamatedDelta ::
  (Ord ctx, Ord carrier, Ord prop, Semigroup evidence) =>
  CarrierCover ctx ->
  CoverageFact ->
  RuntimeBoundary ->
  NonEmpty (RelationalCarrierDelta ctx carrier prop RuntimeBoundary evidence) ->
  Map RowTupleKey Multiplicity ->
  RelationalCarrierDelta ctx carrier prop RuntimeBoundary evidence
amalgamatedDelta cover _coverage boundaryValue deltas@(firstDelta :| _) gluedRows =
  let firstAddr =
        deAddr firstDelta
      addr =
        firstAddr {caContext = carrierCoverTarget cover}
      rows =
        plainRowPatchFromMultiplicityMap gluedRows
   in RelationalCarrierDelta
        { deAddr = addr,
          deTime = amalgamatedCarrierTime cover deltas,
          deSupport = carrierCoverSupport cover,
          deBoundary = boundaryValue,
          deEvidence = sconcat (fmap deEvidence deltas),
          deRows = rows,
          dePayload = (),
          deOrigin = originMerge OriginAmalgamated (fmap deOrigin deltas),
          deScope =
            foldMap deScope deltas
        }

amalgamatedCarrierTime ::
  Ord ctx =>
  CarrierCover ctx ->
  NonEmpty (RelationalCarrierDelta ctx carrier prop RuntimeBoundary evidence) ->
  RelationalCarrierTime ctx
amalgamatedCarrierTime cover deltas =
  let latestInputTime =
        Foldable.maximum (fmap deTime deltas)
   in retimeRelationalCarrierPhase PhaseAmalgamate
        (recontextRelationalCarrierTime (carrierCoverTarget cover) latestInputTime)
{-# INLINE amalgamatedCarrierTime #-}

mergeEnv ::
  IntMap Int ->
  IntMap Int ->
  Maybe (IntMap Int)
mergeEnv env =
  Foldable.foldlM mergeEnvStep env . IntMap.toAscList

mergeEnvStep ::
  IntMap Int ->
  (Int, Int) ->
  Maybe (IntMap Int)
mergeEnvStep env (slotKey, repKey) =
  case IntMap.lookup slotKey env of
    Nothing ->
      Just (IntMap.insert slotKey repKey env)
    Just existing
      | existing == repKey ->
          Just env
      | otherwise ->
          Nothing

envToRow ::
  [SlotId] ->
  IntMap Int ->
  Maybe RowTupleKey
envToRow schema env =
  tupleKeyFromInts <$> traverse (\slot -> IntMap.lookup (slotIdKey slot) env) schema
